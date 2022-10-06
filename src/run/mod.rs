pub mod builtins;
pub mod value;

use self::{builtins::BuiltinFn, value::DecValue};
use crate::{
    error::{Result, RuntimeError},
    parse::{
        func::DecFn,
        path::Path,
        statement::{Block, Expression, IndexExpr, LiteralExpr, LoopExpr, Statement},
        Item, Module,
    },
    types::DecType,
};
use std::{collections::HashMap, iter};

/// values of identifiers in static and runtime scope.
pub struct Scope {
    /// static scope - all of the items currently in scope
    items: HashMap<String, Item>,

    /// a stack of scopes, innermost last.
    local: Vec<HashMap<String, DecValue>>,
}

impl Scope {
    /// create a scope consisting of a single empty map
    pub fn new() -> Self {
        let mut items = HashMap::new();
        items.insert(String::from("std"), Item::Module(builtins::std()));
        Self {
            items,
            local: vec![HashMap::new()],
        }
    }

    /// get a reference to the item referred to by the given *relative* path in the current static
    /// scope.
    pub fn get_item(&self, path: &Path) -> Result<&Item> {
        let mut components = path.components();
        let mut name_cursor = components.next().expect("empty path");
        let mut item_cursor =
            self.items
                .get(name_cursor)
                .ok_or_else(|| RuntimeError::NoPathRoot {
                    root: name_cursor.clone(),
                })?;
        for component in components {
            let err = || RuntimeError::NoSubPath {
                parent: name_cursor.clone(),
                child: component.clone(),
            };

            name_cursor = component;
            if let Item::Module(module) = item_cursor {
                item_cursor = module.items.get(component).ok_or_else(err)?;
            } else {
                return Err(err());
            }
        }

        Ok(item_cursor)
    }

    /// get a clone of the static value referred to by the given *relative* path in the current
    /// static scope. returns an error if the path terminates at a module or other non-value tiem
    pub fn get_value_item(&self, path: &Path) -> Result<DecValue> {
        let item = self.get_item(path)?;
        match item {
            Item::Fn(f) => Ok(DecValue::Fn(f.clone())),
            Item::BuiltinFn(f) => Ok(DecValue::BuiltinFn(f.clone())),
            Item::Module(_) => Err(RuntimeError::NonValuePath {
                found: item.clone(),
                path: path.clone(),
            }),
        }
    }

    /// get a clone of the vaule assigned to the given identifier in the inner*most* scope.
    /// traverse the current scope outward until the identifier is found. if the identifier
    /// is not found in runtime scope, look for an item in static scope which can be converted
    /// to an immutable value, such as a `fn` or `const` item.
    pub fn get(&self, ident: &str) -> Result<DecValue> {
        for scope in self.local.iter().rev() {
            if let Some(val) = scope.get(ident) {
                return Ok(val.clone());
            }
        }

        // TODO: make this result propogation better
        if let val@Ok(_) = self.get_value_item(&Path::from(ident)) {
            return val;
        }

        bail_ident_error!(ident.to_owned())
    }

    /// get a mutable reference to the value assigned to the given identifier in the inner*most*
    /// scope. does not return `Ok` if the identifier references a function or other item for
    /// which it would not make sense to mutate.
    pub fn get_mut(&mut self, ident: &str) -> Result<&mut DecValue> {
        for scope in &mut self.local {
            if let Some(val) = scope.get_mut(ident) {
                return Ok(val);
            }
        }

        if self.items.contains_key(ident) {
            return Err(RuntimeError::ImmutBuiltin);
        }

        bail_ident_error!(ident.to_owned())
    }

    /// define the given identifier to be the given value in the innermost scope.
    pub fn insert(&mut self, ident: String, value: DecValue) {
        self.local
            .last_mut()
            .expect("missing scope")
            .insert(ident, value);
    }

    /// evaluate a statement and discard the result
    pub fn run_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Expression(e) => {
                self.evaluate_expr(e)?;
            }
            Statement::Definition(d) => {
                let value = self.evaluate_expr(&d.value)?;
                self.insert(d.name.clone(), value);
            }
            Statement::Assignment(a) => {
                let new_value = self.evaluate_expr(&a.value)?;
                let var = self.get_mut(&a.name)?;
                *var = new_value;
            }
        }

        Ok(())
    }

    /// create a new scope, evaluate all the statements of the given block in order, and return
    /// either the tail expression or `void`.
    pub fn evaluate_block(&mut self, block: &Block) -> Result<DecValue> {
        self.local.push(HashMap::new());

        for statement in &block.statements {
            self.run_statement(statement)?;
        }

        let res = block
            .tail
            .as_ref()
            .map_or(Ok(DecValue::Void), |tail| self.evaluate_expr(tail));

        self.local.pop();

        res
    }

    /// find the value of the given expression in the current scope
    pub fn evaluate_expr(&mut self, expr: &Expression) -> Result<DecValue> {
        let expr_type = expr.check_type(self)?;

        match expr {
            Expression::Literal(l) => match l {
                LiteralExpr::String(s) => Ok(DecValue::String(s.to_string())),
                LiteralExpr::Bool(b) => Ok(DecValue::Bool(*b)),
                LiteralExpr::Int(i) => Ok(DecValue::Int(*i)),
                LiteralExpr::Float(f) => Ok(DecValue::Float(*f)),
                LiteralExpr::Void => Ok(DecValue::Void),
            },
            Expression::Ident(i) => Ok(self.get(i)?),
            Expression::Path(p) => Ok(self.get_value_item(p)?),
            Expression::FnCall(call) => {
                let args = call
                    .args
                    .iter()
                    .map(|arg| self.evaluate_expr(arg))
                    .collect::<Result<Vec<_>>>()?;
                match self.evaluate_expr(&call.func)? {
                    DecValue::BuiltinFn(BuiltinFn { func, .. }) => Ok(func(args)),
                    DecValue::Fn(f) => {
                        self.run_func(&f, iter::zip(f.args.iter().map(|a| a.name.clone()), args))
                    }
                    _ => unreachable!("typecheck"),
                }
            }
            Expression::If(if_expr) => {
                for (condition, result) in &if_expr.branches {
                    let value = self.evaluate_expr(condition)?;
                    if let DecValue::Bool(b) = value {
                        if b {
                            return self.evaluate_block(result);
                        }
                    } else {
                        unreachable!("typecheck");
                    }
                }

                if_expr
                    .else_block
                    .as_ref()
                    .map_or(Ok(DecValue::Void), |b| self.evaluate_block(b))
            }
            Expression::List(l) => {
                let mut v = Vec::new();
                for expr in &l.elements {
                    v.push(self.evaluate_expr(expr)?);
                }
                if let DecType::List(t) = expr_type {
                    Ok(DecValue::List(*t, v))
                } else {
                    unreachable!("typecheck")
                }
            }
            Expression::IndexExpr(IndexExpr { list, index }) => {
                if let DecValue::Int(i) = self.evaluate_expr(index)? {
                    if let DecValue::List(.., list) = self.get(list)? {
                        usize::try_from(i)
                            .map(|i| list[i].clone())
                            .map_err(|_| RuntimeError::BadIdx(i))
                    } else {
                        unreachable!("typecheck");
                    }
                } else {
                    unreachable!("typecheck");
                }
            }
            Expression::Loop(LoopExpr { block }) => loop {
                self.evaluate_block(block)?;
            },
            Expression::Block(b) => self.evaluate_block(b),
        }
    }

    /// bring the given values for the given arguments into scope and then evaluate the block of
    /// the given function in a new scope.
    pub fn run_func<Z>(&mut self, f: &DecFn, args: Z) -> Result<DecValue>
    where
        Z: Iterator<Item = (String, DecValue)>,
    {
        self.local.push(HashMap::new());

        for (ident, val) in args {
            self.insert(ident, val);
        }

        let res = self.evaluate_block(&f.block);
        self.local.pop();
        res
    }

    /// create a new scope containing all of the top-level items of `root` in static scope
    pub fn from_root(root: Module) -> Self {
        let mut res = Self::new();
        for (ident, item) in root.items {
            res.items.insert(ident, item);
        }
        res
    }
}

/// find the given program's `main` function and run it
pub fn program(module: Module) -> Result<()> {
    let mut scope = Scope::from_root(module);

    if let DecValue::Fn(main) = scope
        .get("main")
        .map_err(|_| RuntimeError::MissingMain)?
    {
        if main.args.is_empty() && main.return_type == DecType::Void {
            return scope.run_func(&main, iter::empty()).map(|_| ());
        }
    }

    Err(RuntimeError::MainWrongType)
}
