use crate::{
    parse::func::DecFn,
    types::DecType,
};

use super::builtins::BuiltinFn;

#[allow(clippy::module_name_repetitions)]
#[derive(Clone)]
pub enum DecValue {
    Fn(DecFn),
    BuiltinFn(BuiltinFn),
    String(String),
    Bool(bool),
    Int(i128),
    Float(f64),
    Void,
    List(DecType, Vec<DecValue>),
    Break,
}

impl DecValue {
    /// get the type of the given value
    pub fn dectype(&self) -> DecType {
        match self {
            Self::Fn(f) => f.dectype(),
            Self::BuiltinFn( BuiltinFn { fntype, .. }) => DecType::Fn(fntype.clone()),
            Self::String(_) => DecType::String,
            Self::Bool(_) => DecType::Bool,
            Self::Void => DecType::Void,
            Self::Int(_) => DecType::Int,
            Self::Float(_) => DecType::Float,
            Self::List(t, ..) => DecType::List(Box::new(t.clone())),
            Self::Break => unreachable!(),
        }
    }

    pub const fn is_break(&self) -> bool {
        matches!(self, Self::Break)
    }
}
