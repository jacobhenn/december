use crate::{
    parse::func::DecFn,
    types::{DecType, FnType},
};

#[derive(Clone)]
pub enum DecValue {
    Fn(DecFn),
    BuiltinFn {
        fntype: FnType,
        func: fn(Vec<DecValue>) -> DecValue,
    },
    String(String),
    Bool(bool),
    Int(i128),
    Float(f64),
    Void,
}

impl DecValue {
    pub fn dectype(&self) -> DecType {
        match self {
            Self::Fn(DecFn {
                args, return_type, ..
            }) => DecType::Fn(FnType {
                arg_types: args.iter().map(|a| a.dectype.clone()).collect(),
                return_type: Box::new(return_type.clone()),
            }),
            Self::BuiltinFn { fntype, .. } => DecType::Fn(fntype.clone()),
            Self::String(_) => DecType::String,
            Self::Bool(_) => DecType::Bool,
            Self::Void => DecType::Void,
            Self::Int(_) => DecType::Int,
            Self::Float(_) => DecType::Float,
        }
    }
}
