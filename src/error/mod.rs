pub mod parse;

use crate::{parse::Item, types::DecType};
use thiserror::Error;

/// an alias for `Result<T, RuntimeError>`
pub type Result<T> = std::result::Result<T, RuntimeError>;

#[allow(clippy::module_name_repetitions)]
#[derive(Error, Debug)]
/// an error that happened after parsing was successful
pub enum RuntimeError {
    #[error("E00: mismatched types: expected `{expected}`, found `{found}`")]
    MismatchedTypes { expected: DecType, found: DecType },

    #[error("E01: could not find `{ident}` in this scope")]
    IdentNotFound { ident: String },

    #[error("E02: main function has wrong type: expected `fn() -> void`")]
    MainWrongType,

    #[error("E03: program needs a `main` function")]
    MissingMain,

    #[error(
        "E04: mismatched types in function call expression: expected function, found `{found}`"
    )]
    NonFunctionCall { found: DecType },

    #[error("E05: wrong number of arguments in function call: expected {expected}, found {found}")]
    WrongNumArgs { expected: usize, found: usize },

    #[error("E06: list declarations may not be empty")]
    EmptyList,

    #[error("E07: mismatched types in list index expression: expected list, found `{found}`")]
    NonListIndex { found: DecType },

    #[error("E08: cannot mutate a builtin")]
    ImmutBuiltin,

    #[error("E09: {_0} is an invalid list index")]
    BadIdx(i128),

    #[error(
        "E10: expected an item which can be a value, found {}",
        match found {
            Item::Module(_) => "module",
            _ => unreachable!()
        }
    )]
    NonValueItem { found: Item },

    #[error("E11: item `{root}` has not been imported or declared")]
    NoPathRoot { root: String },

    #[error("E11: item `{child}` not found in `{parent}`")]
    NoSubPath { child: String, parent: String },
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
