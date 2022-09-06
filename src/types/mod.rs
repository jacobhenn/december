pub mod infer;

use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DecType {
    Fn(FnType),
    String,
    Bool,

    // a type with only one member, `void`.
    Void,
}

impl Display for DecType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecType::Fn(fn_type) => write!(
                f,
                "fn({}) -> {}",
                fn_type
                    .arg_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                fn_type.return_type,
            ),
            DecType::String => f.write_str("String"),
            DecType::Void => f.write_str("void"),
            DecType::Bool => f.write_str("bool"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub arg_types: Box<[DecType]>,
    pub return_type: Box<DecType>,
}
