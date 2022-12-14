use std::iter;

use super::DecType;
use crate::{
    error::{Result, RuntimeError},
    parse::statement::{Block, Expression, IndexExpr, ListExpr, LiteralExpr},
    run::{builtins::BuiltinFn, value::DecValue, Scope},
};

impl Block {
    pub fn check_type(&self, scope: &mut Scope) -> Result<DecType> {
        self.tail
            .as_ref()
            .map_or(Ok(DecType::Void), |tail| tail.check_type(scope))
    }
}

impl Expression {
    /// trivially infer the type of an expression at runtime. additionally, verify that the
    /// expression is *internally consistent* (i.e., all branches of an if statement return the
    /// same type).
    pub fn check_type(&self, scope: &mut Scope) -> Result<DecType> {
        let ty = match self {
            Self::Literal(l) => match l {
                LiteralExpr::String(_) => DecType::String,
                LiteralExpr::Bool(_) => DecType::Bool,
                LiteralExpr::Int(_) => DecType::Int,
                LiteralExpr::Float(_) => DecType::Float,
                LiteralExpr::Void => DecType::Void,
            },
            Self::Ident(i) => scope.get(i)?.dectype(),
            Self::Path(p) => scope.get_value_item(p)?.dectype(),
            Self::FnCall(fn_call) => {
                let func = scope.evaluate_expr(&fn_call.func)?;
                let call_args = fn_call.args.iter().map(|arg| arg.check_type(scope));
                match func {
                    DecValue::Fn(func) => {
                        check_arg_types(func.args.iter().map(|arg| &arg.dectype), call_args)?;
                        func.return_type
                    }
                    DecValue::BuiltinFn(BuiltinFn { fntype, .. }) => {
                        check_arg_types(fntype.arg_types.iter(), call_args)?;
                        *fntype.return_type
                    }
                    v => return Err(RuntimeError::NonFunctionCall { found: v.dectype() }),
                }
            }
            Self::If(if_expr) => {
                let mut blocks_iter = if_expr
                    .branches
                    .iter()
                    .map(|(_, block)| block)
                    .chain(if_expr.else_block.iter());
                let first_block_type = blocks_iter.next().unwrap().check_type(scope)?;
                for block in blocks_iter {
                    let block_type = block.check_type(scope)?;
                    if block_type != first_block_type {
                        bail_type_error!(first_block_type, block_type);
                    }
                }

                for condition in if_expr.branches.iter().map(|(expr, _)| expr) {
                    let condition_type = condition.check_type(scope)?;
                    if condition_type != DecType::Bool {
                        bail_type_error!(DecType::Bool, condition_type);
                    }
                }

                if if_expr.else_block.is_none() && first_block_type != DecType::Void {
                    bail_type_error!(DecType::Void, first_block_type);
                }

                first_block_type
            }
            Self::List(ListExpr { elements }) => {
                if elements.is_empty() {
                    return Err(RuntimeError::EmptyList);
                }
                let mut elements_iter = elements.iter();
                let first_element_type = elements_iter.next().unwrap().check_type(scope)?;
                for other_element in elements_iter {
                    let other_element_type = other_element.check_type(scope)?;
                    if other_element_type != first_element_type {
                        bail_type_error!(first_element_type, other_element_type);
                    }
                }
                DecType::List(Box::new(first_element_type))
            }
            Self::IndexExpr(IndexExpr { list, index }) => match scope.get(list)?.dectype() {
                DecType::List(element_type) => match index.check_type(scope)? {
                    DecType::Int => *element_type,
                    other => bail_type_error!(DecType::Int, other),
                },
                other => return Err(RuntimeError::NonListIndex { found: other }),
            },
            Self::Loop(_) => DecType::Never,
            Self::Block(b) => {
                if let Some(tail) = &b.tail {
                    tail.check_type(scope)?
                } else {
                    DecType::Void
                }
            }
        };

        Ok(ty)
    }
}

pub fn check_arg_types<'a, FnArgs, CallArgs>(fn_args: FnArgs, call_args: CallArgs) -> Result<()>
where
    FnArgs: Iterator<Item = &'a DecType> + ExactSizeIterator,
    CallArgs: Iterator<Item = Result<DecType>> + ExactSizeIterator,
{
    if fn_args.len() != call_args.len() {
        return Err(RuntimeError::WrongNumArgs {
            expected: fn_args.len(),
            found: call_args.len(),
        });
    }

    for (fn_arg_type, call_arg_type) in iter::zip(fn_args, call_args) {
        let call_arg_type = call_arg_type?;
        if fn_arg_type != &call_arg_type {
            bail_type_error!(fn_arg_type.clone(), call_arg_type);
        }
    }

    Ok(())
}
