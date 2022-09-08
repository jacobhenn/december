use std::iter;

use super::DecType;
use crate::{
    error::{Result, RuntimeError},
    parse::statement::{Block, Expression, LiteralExpr},
    run::{value::DecValue, Scope},
};

impl Block {
    pub fn check_type(&self, _scope: &Scope) -> Result<DecType> {
        // for now, blocks are only capable of returning `void`
        Ok(DecType::Void)
    }
}

impl Expression {
    /// trivially infer the type of an expression at runtime. additionally, verify that the
    /// expression is *internally consistent* (i.e., all branches of an if statement return the
    /// same type).
    pub fn check_type(&self, scope: &Scope) -> Result<DecType> {
        let ty = match self {
            Expression::Literal(l) => match l {
                LiteralExpr::String(_) => DecType::String,
                LiteralExpr::Bool(_) => DecType::Bool,
                LiteralExpr::Int(_) => DecType::Int,
                LiteralExpr::Float(_) => DecType::Float,
                LiteralExpr::Void => DecType::Void,
            },
            Expression::Identifier(i) => scope.get_ident(i)?.dectype(),
            Expression::FnCall(fn_call) => {
                let func = scope.get_ident(&fn_call.func)?;
                let call_args = fn_call.args.iter().map(|arg| arg.check_type(scope));
                match func {
                    DecValue::Fn(func) => {
                        check_arg_types(func.args.iter().map(|arg| &arg.dectype), call_args)?;
                        func.return_type
                    }
                    DecValue::BuiltinFn { fntype, .. } => {
                        check_arg_types(fntype.arg_types.iter(), call_args)?;
                        *fntype.return_type
                    }
                    v => return Err(RuntimeError::NonFunctionCall { found: v.dectype() }),
                }
            }
            Expression::If(if_expr) => {
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

                if first_block_type == DecType::Void && if_expr.else_block.is_none() {
                    bail_type_error!(DecType::Void, first_block_type);
                }

                first_block_type
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
