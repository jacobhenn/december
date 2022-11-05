pub mod func;
pub mod statement;
pub mod string;
pub mod types;
pub mod path;

use func::decfn;
use nom::{
    bytes::complete::{is_not, tag},
    character::complete::{multispace0, multispace1},
    combinator::{eof, map, value},
    error::{context, ContextError, ParseError},
    multi::many0,
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::collections::HashMap;

use crate::{run::builtins::BuiltinFn, types::DecType};

/// a `T` along with its position in the input stream
// pub struct Located<'a, T, Position>
// where
//     Position: Deref<Target = &'a str>,
// {
//     inner: T,
//     position: Position,
// }

pub fn comment<'a, E>(s: &'a str) -> IResult<&'a str, (), E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    value((), pair(tag("//"), is_not("\n\r")))(s)
}

// parses something, optionally preceded by whitespace or a comment.
fn spaced0<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E> + 'a,
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    preceded(
        tuple((
            multispace0,
            many0(preceded(multispace0, comment)),
            multispace0,
        )),
        inner,
    )
}

// parses something, absolutely preceded by whitespace.
fn spaced1<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E> + 'a,
    E: ParseError<&'a str>,
{
    preceded(multispace1, inner)
}

#[test]
fn test_good_comments() {
    use crate::parse::statement::fn_call_args;
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_matches!(fn_call_args::<VerboseError<&str>>("(x, y) // foo"), Ok(_));
    assert_matches!(
        fn_call_args::<VerboseError<&str>>(
            "(
            x, // foo
            y  // bar
    )"
        ),
        Ok(_)
    );
    assert_matches!(
        func::decfn::<VerboseError<&str>>(
            r#"fn foo() -> void {
        // test of the thing

        // thing of the test //
        println("foo");
        // test of the thing
        // thing of the test
    }"#
        ),
        Ok(_)
    );
}

#[test]
#[should_panic]
fn test_bad_comments() {
    use crate::parse::statement::expression;
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_matches!(expression::<VerboseError<&str>>("f(x, // foo y)"), Ok(("", _)));
}

#[derive(Debug, Clone)]
pub enum Item {
    Fn(func::DecFn),
    BuiltinFn(BuiltinFn),
    Module(Module),
}

impl Item {
    pub fn dectype(&self) -> Option<DecType> {
        match self {
            Self::Fn(f) => Some(f.dectype()),
            _ => None,
        }
    }
}

pub fn item<'a, E>(s: &'a str) -> IResult<&'a str, (String, Item), E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    context("item", map(decfn, |(i, f)| (i, Item::Fn(f))))(s)
}

#[derive(Debug, Clone)]
pub struct Module {
    pub items: HashMap<String, Item>,
}

pub fn program<'a, E>(s: &'a str) -> IResult<&'a str, Module, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    let (s, res) = map(many0(spaced0(item)), |items| Module {
        items: items.into_iter().collect(),
    })(s)?;
    let (s, _) = context("item", spaced0(eof))(s)?;
    Ok((s, res))
}
