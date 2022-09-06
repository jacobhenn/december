use lazy_static::lazy_static;
use std::collections::HashMap;

use super::value::DecValue;
use crate::{
    parse::statement::Identifier,
    types::{DecType, FnType},
};

lazy_static! {
    pub static ref PRINTLN: DecValue = DecValue::BuiltinFn {
        fntype: FnType {
            arg_types: vec![DecType::String].into_boxed_slice(),
            return_type: Box::new(DecType::Void),
        },
        func: |v| {
            if let DecValue::String(s) = &v[0] {
                println!("{s}");
                DecValue::Void
            } else {
                unreachable!();
            }
        },
    };
    pub static ref PRINT: DecValue = DecValue::BuiltinFn {
        fntype: FnType {
            arg_types: vec![DecType::String].into_boxed_slice(),
            return_type: Box::new(DecType::Void),
        },
        func: |v| {
            if let DecValue::String(s) = &v[0] {
                print!("{s}");
                DecValue::Void
            } else {
                unreachable!();
            }
        },
    };
    pub static ref BUILTINS: HashMap<Identifier, DecValue> = {
        let mut m = HashMap::new();
        m.insert(Identifier::from("println"), PRINTLN.clone());
        m.insert(Identifier::from("print"), PRINT.clone());
        m
    };
}
