use std::{collections::HashMap, io};

use super::value::DecValue;
use crate::{
    parse::{Item, Module},
    types::{DecType, FnType},
};

#[derive(Debug, Clone)]
pub struct BuiltinFn {
    pub fntype: FnType,
    pub func: fn(Vec<DecValue>) -> DecValue,
}

macro_rules! decfn {
    {
        $funcname:ident($($argid:ident: $argty:ident),*) -> $rettype:ident $funcbody:block
    } => {
        #[allow(unused_variables, unused_mut)]
        pub fn $funcname() -> Item {
            let mut arg_types = Vec::new();
            $(
                arg_types.push(DecType::$argty);
            )*

            Item::BuiltinFn(BuiltinFn {
                fntype: FnType {
                    arg_types: arg_types.into_boxed_slice(),
                    return_type: Box::new(DecType::$rettype),
                },
                func: |argv| {
                    let mut argit = argv.into_iter();
                    $(
                        let $argid = if let Some(DecValue::$argty($argid)) = argit.next() {
                            $argid
                        } else {
                            unreachable!("mismatched types in call of builtin function")
                        };
                    )*

                    $funcbody
                },
            })
        }
    }
}

decfn! {
    println(s: String) -> Void {
        println!("{s}");
        DecValue::Void
    }
}

decfn! {
    getln() -> String {
        let stdin = io::stdin();
        let mut res = String::new();
        stdin.read_line(&mut res).unwrap();
        DecValue::String(res.trim().to_string())
    }
}

decfn! {
    print(s: String) -> Void {
        print!("{s}");
        DecValue::Void
    }
}

decfn! {
    float_to_string(f: Float) -> String {
        DecValue::String(f.to_string())
    }
}

decfn! {
    string_to_int(s: String) -> Int {
        DecValue::Int(s.parse().unwrap())
    }
}

decfn! {
    int_to_string(n: Int) -> String {
        DecValue::String(n.to_string())
    }
}

decfn! {
    int_add(lhs: Int, rhs: Int) -> Int {
        DecValue::Int(lhs + rhs)
    }
}

decfn! {
    int_mod(lhs: Int, rhs: Int) -> Int {
        DecValue::Int(lhs % rhs)
    }
}

decfn! {
    int_mul(lhs: Int, rhs: Int) -> Int {
        DecValue::Int(lhs * rhs)
    }
}

decfn! {
    int_less(lhs: Int, rhs: Int) -> Bool {
        DecValue::Bool(lhs < rhs)
    }
}

pub fn int() -> Module {
    let mut items = HashMap::new();
    items.insert(String::from("to_string"), int_to_string());
    items.insert(String::from("add"), int_add());
    items.insert(String::from("mod"), int_mod());
    items.insert(String::from("mul"), int_mul());
    items.insert(String::from("less"), int_less());

    Module { items }
}

pub fn float() -> Module {
    let mut items = HashMap::new();
    items.insert(String::from("to_string"), float_to_string());

    Module { items }
}

pub fn std() -> Module {
    let mut items = HashMap::new();
    items.insert(String::from("int"), Item::Module(int()));
    items.insert(String::from("float"), Item::Module(float()));

    items.insert(String::from("println"), println());
    items.insert(String::from("print"), print());
    items.insert(String::from("getln"), getln());

    Module { items }
}
