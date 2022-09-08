pub mod infer;

use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DecType {
    Fn(FnType),
    String,
    Bool,
    Int,
    Float,
    List(Box<DecType>),
    Never,

    // a type with only one member, `void`.
    Void,
}

impl Display for DecType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(fn_type) => write!(
                f,
                "fn({}) -> {}",
                fn_type
                    .arg_types
                    .iter()
                    .map(Self::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
                fn_type.return_type,
            ),
            Self::String => f.write_str("String"),
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Float => f.write_str("float"),
            Self::Int => f.write_str("int"),
            Self::List(t) => write!(f, "[{t}]"),
            Self::Never => f.write_str("!"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub arg_types: Box<[DecType]>,
    pub return_type: Box<DecType>,
}
