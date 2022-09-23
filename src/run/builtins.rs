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
    pub static ref INT2STRING: DecValue = DecValue::BuiltinFn {
        fntype: FnType {
            arg_types: vec![DecType::Int].into_boxed_slice(),
            return_type: Box::new(DecType::String),
        },
        func: |v| {
            if let DecValue::Int(i) = &v[0] {
                DecValue::String(format!("{i}"))
            } else {
                unreachable!();
            }
        },
    };
    pub static ref FLOAT2STRING: DecValue = DecValue::BuiltinFn {
        fntype: FnType {
            arg_types: vec![DecType::Float].into_boxed_slice(),
            return_type: Box::new(DecType::String),
        },
        func: |v| {
            if let DecValue::Float(i) = &v[0] {
                DecValue::String(format!("{i}"))
            } else {
                unreachable!();
            }
        },
    };
    pub static ref INT_ADD: DecValue = DecValue::BuiltinFn {
        fntype: FnType {
            arg_types: vec![DecType::Int, DecType::Int].into_boxed_slice(),
            return_type: Box::new(DecType::Int),
        },
        func: |v| {
            if let (DecValue::Int(x), DecValue::Int(y)) = (&v[0], &v[1]) {
                DecValue::Int(x + y)
            } else {
                unreachable!();
            }
        },
    };
    pub static ref INT_MOD: DecValue = DecValue::BuiltinFn {
        fntype: FnType {
            arg_types: vec![DecType::Int, DecType::Int].into_boxed_slice(),
            return_type: Box::new(DecType::Int),
        },
        func: |v| {
            if let (DecValue::Int(x), DecValue::Int(y)) = (&v[0], &v[1]) {
                DecValue::Int(x % y)
            } else {
                unreachable!();
            }
        },
    };

    pub static ref INT_MUL: DecValue = DecValue::BuiltinFn {
        fntype: FnType {
            arg_types: vec![DecType::Int, DecType::Int].into_boxed_slice(),
            return_type: Box::new(DecType::Int),
        },
        func: |v| {
            if let (DecValue::Int(x), DecValue::Int(y)) = (&v[0], &v[1]) {
                DecValue::Int(x * y)
            } else {
                unreachable!();
            }
        },
    };

    pub static ref BUILTINS: HashMap<Identifier, DecValue> = {
        let mut m = HashMap::new();
        m.insert(Identifier::from("println"), PRINTLN.clone());
        m.insert(Identifier::from("print"), PRINT.clone());
        m.insert(Identifier::from("int2string"), INT2STRING.clone());
        m.insert(Identifier::from("float2string"), FLOAT2STRING.clone());
        m.insert(Identifier::from("int_add"), INT_ADD.clone());
        m.insert(Identifier::from("int_mod"), INT_MOD.clone());
        m.insert(Identifier::from("int_mul"), INT_MUL.clone());
        m
    };
}
