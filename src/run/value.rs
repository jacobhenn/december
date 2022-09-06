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
    Void,
}

impl DecValue {
    pub fn dectype(&self) -> DecType {
        match self {
            DecValue::Fn(DecFn {
                args, return_type, ..
            }) => DecType::Fn(FnType {
                arg_types: args.iter().map(|a| a.dectype.clone()).collect(),
                return_type: Box::new(return_type.clone()),
            }),
            DecValue::BuiltinFn { fntype, .. } => DecType::Fn(fntype.clone()),
            DecValue::String(_) => DecType::String,
            DecValue::Bool(_) => DecType::Bool,
            DecValue::Void => DecType::Void,
        }
    }
}
