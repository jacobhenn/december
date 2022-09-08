pub mod builtins;
pub mod value;

use self::value::DecValue;
use crate::{
    error::{Result, RuntimeError},
    parse::{
        func::DecFn,
        statement::{Block, Expression, Identifier, LiteralExpr, Statement, IndexExpr, LoopExpr},
        Item, Program,
    },
    types::DecType,
};
use builtins::BUILTINS;
use std::collections::HashMap;

pub struct Scope {
    idents: HashMap<Identifier, DecValue>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            idents: HashMap::new(),
        }
    }

    pub fn run_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Expression(e) => {
                self.evaluate_expr(e)?;
            }
            Statement::Definition(d) => {
                let value = self.evaluate_expr(&d.value)?;
                self.idents.insert(d.name.clone(), value);
            }
            Statement::Assignment(a) => {
                let new_value = self.evaluate_expr(&a.value)?;
                if let Some(var) = self.idents.get_mut(&a.name) {
                    *var = new_value;
                } else {
                    bail_ident_error!(a.name.clone());
                }
            }
        }

        Ok(())
    }

    pub fn get_ident(&self, i: &Identifier) -> Result<DecValue> {
        if let Some(value) = self.idents.get(i) {
            Ok(value.clone())
        } else if let Some(value) = BUILTINS.get(i) {
            Ok(value.clone())
        } else {
            bail_ident_error!(i.clone());
        }
    }

    pub fn evaluate_block(&mut self, block: &Block) -> Result<DecValue> {
        for statement in &block.statements {
            self.run_statement(statement)?;
        }
        Ok(DecValue::Void)
    }

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
            Expression::Identifier(i) => self.get_ident(i),
            Expression::FnCall(call) => match self.get_ident(&call.func)? {
                DecValue::BuiltinFn { func, .. } => {
                    let args = call
                        .args
                        .iter()
                        .map(|arg| self.evaluate_expr(arg))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(func(args))
                }
                DecValue::Fn { .. } => todo!(),
                _ => unreachable!("typecheck"),
            },
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

                if let Some(else_block) = &if_expr.else_block {
                    self.evaluate_block(else_block)
                } else {
                    Ok(DecValue::Void)
                }
            }
            Expression::List(l) => {
                let mut v = Vec::new();
                for expr in &l.elements {
                    v.push(self.evaluate_expr(&expr)?)
                }
                if let DecType::List(t) = expr_type {
                    Ok(DecValue::List(*t, v))
                } else {
                    unreachable!("typecheck")
                }
            }
            Expression::IndexExpr(IndexExpr { list, index }) => {
                if let DecValue::List(.., list) = self.get_ident(list)? {
                    if let DecValue::Int(i) = self.evaluate_expr(index)? {
                        Ok(list[i as usize].clone())
                    } else {
                        unreachable!("typecheck");
                    }
                } else {
                    unreachable!("typecheck");
                }
            }
            Expression::Loop(LoopExpr { block }) => {
                loop {
                    self.evaluate_block(block)?;
                }
            }
        }
    }
}

pub fn func(f: &DecFn) -> Result<DecValue> {
    let mut scope = Scope::new();
    scope.evaluate_block(&f.block)?;
    if f.return_type == DecType::Void {
        Ok(DecValue::Void)
    } else {
        bail_type_error!(DecType::Void, f.return_type.clone())
    }
}

pub fn program(program: &Program) -> Result<()> {
    if let Some(Item::Fn(main)) = program.items.get(&Identifier::new(String::from("main"))) {
        if main.args.is_empty() && main.return_type == DecType::Void {
            func(main)?;
        } else {
            return Err(RuntimeError::MainWrongType);
        }
    } else {
        return Err(RuntimeError::MissingMain);
    }

    Ok(())
}
