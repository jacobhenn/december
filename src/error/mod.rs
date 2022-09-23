pub mod parse;

use crate::{parse::statement::Identifier, types::DecType};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[allow(clippy::module_name_repetitions)]
#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("E0: mismatched types: expected `{expected}`, found `{found}`")]
    MismatchedTypes { expected: DecType, found: DecType },
    #[error("E1: could not find `{ident}` in this scope")]
    IdentNotFound { ident: Identifier },
    #[error("E2: main function has wrong type: expected `fn() -> void`")]
    MainWrongType,
    #[error("E3: program needs a `main` function")]
    MissingMain,
    #[error(
        "E4: mismatched types in function call expression: expected function, found `{found}`"
    )]
    NonFunctionCall { found: DecType },
    #[error("E5: wrong number of arguments in function call: expected {expected}, found {found}")]
    WrongNumArgs { expected: usize, found: usize },
    #[error("E6: list declarations may not be empty")]
    EmptyList,
    #[error("E7: mismatched types in list index expression: expected list, found `{found}`")]
    NonListIndex { found: DecType },
    #[error("E8: cannot mutate a builtin")]
    ImmutBuiltin,
    #[error("E9: {} is an invalid list index", _0)]
    BadIdx(i128),
}

/// (expr) -> !
///
/// return an error that the first argument could not be found in the current scope.
macro_rules! bail_ident_error {
    ($ident:expr) => {
        return Err(crate::error::RuntimeError::IdentNotFound { ident: $ident })
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
