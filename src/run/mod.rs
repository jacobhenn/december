pub mod builtins;
pub mod value;

use self::value::DecValue;
use crate::{
    error::{Result, RuntimeError},
    parse::{
        func::DecFn,
        statement::{Block, Expression, Identifier, IndexExpr, LiteralExpr, LoopExpr, Statement},
        Item, Program,
    },
    types::DecType,
};
use builtins::BUILTINS;
use std::{collections::HashMap, iter};

/// a stack of maps of identifiers to values. each time a new scope is entered, a new map is pushed
/// to the stack.
pub struct Scope {
    /// a stack of scopes, innermost last.
    scopes: Vec<HashMap<Identifier, DecValue>>,
}

impl Scope {
    /// create a scope consisting of a single empty map
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// get a reference to the vaule assigned to the given identifier in the inner*most* scope.
    /// traverse the current scope from most local to global until the identifier is found.
    pub fn get(&self, ident: &Identifier) -> Result<&DecValue> {
        for scope in &self.scopes {
            if let Some(val) = scope.get(ident) {
                return Ok(val);
            }
        }

        if let Some(val) = BUILTINS.get(ident) {
            return Ok(val);
        }

        bail_ident_error!(ident.clone())
    }

    /// get a mutable reference to the value assigned to the given identifier in the inner*most*
    /// scope. does not return `Ok` if the identifier references a function or other item for
    /// which it would not make sense to mutate.
    pub fn get_mut(&mut self, ident: &Identifier) -> Result<&mut DecValue> {
        for scope in &mut self.scopes {
            if let Some(val) = scope.get_mut(ident) {
                return Ok(val);
            }
        }

        if BUILTINS.contains_key(ident) {
            return Err(RuntimeError::ImmutBuiltin);
        }

        bail_ident_error!(ident.clone())
    }

    /// define the given identifier to be the given value in the innermost scope.
    pub fn insert(&mut self, ident: Identifier, value: DecValue) {
        self.scopes
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
        self.scopes.push(HashMap::new());

        for statement in &block.statements {
            self.run_statement(statement)?;
        }

        let res = block
            .tail
            .as_ref()
            .map_or(Ok(DecValue::Void), |tail| self.evaluate_expr(tail));

        self.scopes.pop();

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
            Expression::Identifier(i) => Ok(self.get(i)?.clone()),
            Expression::FnCall(call) => {
                let args = call
                    .args
                    .iter()
                    .map(|arg| self.evaluate_expr(arg))
                    .collect::<Result<Vec<_>>>()?;
                match self.get(&call.func)?.clone() {
                    DecValue::BuiltinFn { func, .. } => Ok(func(args)),
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
        Z: Iterator<Item = (Identifier, DecValue)>,
    {
        self.scopes.push(HashMap::new());

        for (ident, val) in args {
            self.insert(ident, val);
        }

        let res = self.evaluate_block(&f.block);
        self.scopes.pop();
        res
    }

    /// bring the value of the given item into the current scope under the given name
    pub fn setup_item(&mut self, (ident, item): (Identifier, Item)) {
        match item {
            Item::Fn(f) => self.insert(ident, DecValue::Fn(f)),
        }
    }
}

/// find the given program's `main` function and run it
pub fn program(program: Program) -> Result<()> {
    let mut scope = Scope::new();
    for item in program.items {
        scope.setup_item(item);
    }

    if let DecValue::Fn(main) = scope
        .get(&Identifier::new(String::from("main")))
        .map_err(|_| RuntimeError::MissingMain)?
        .clone()
    {
        if main.args.is_empty() && main.return_type == DecType::Void {
            return scope.run_func(&main, iter::empty()).map(|_| ());
        }
    }

    Err(RuntimeError::MainWrongType)
}
