pub mod parse;

use crate::{parse::statement::Identifier, types::DecType};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("mismatched types: expected `{expected}`, found `{found}`")]
    MismatchedTypes { expected: DecType, found: DecType },
    #[error("could not find `{ident}` in this scope")]
    IdentNotFound { ident: Identifier },
    #[error("main function has wrong type: expected `fn() -> void`")]
    MainWrongType,
    #[error("program needs a `main` function")]
    MissingMain,
    #[error("mismatched types in function call expression: expected function, found `{found}`")]
    NonFunctionCall { found: DecType },
    #[error("wrong number of arguments in function call: expected {expected}, found {found}")]
    WrongNumArgs { expected: usize, found: usize },
}

/// (expr) -> !
///
/// return an error that the first argument could not be found in the current scope.
macro_rules! bail_ident_error {
    ($ident:expr) => {
        return Err(crate::error::RuntimeError::IdentNotFound {
            ident: $ident
        })
    };
}

/// (expr, expr) -> !
///
/// return an error that the first argument was expected, but the second was found.
macro_rules! bail_type_error {
    ($expected:expr, $found:expr) => {
        return Err(crate::error::RuntimeError::MismatchedTypes {
            expected: $expected,
            found: $found,
        })
    };
}
