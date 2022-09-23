use crate::{
    parse::func::DecFn,
    types::{DecType, FnType},
};

#[allow(clippy::module_name_repetitions)]
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
    List(DecType, Vec<DecValue>),
}

impl DecValue {
    /// get the type of the given value
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
            Self::List(t, ..) => DecType::List(Box::new(t.clone())),
        }
    }
}
