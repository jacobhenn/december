pub mod func;
pub mod statement;
pub mod string;
pub mod types;

use std::collections::HashMap;

use func::decfn;
use nom::{
    character::complete::{multispace0, multispace1},
    combinator::{eof, map},
    error::{context, ContextError, ParseError},
    multi::many1,
    sequence::preceded,
    IResult,
};

use self::statement::Identifier;

// parses something, optionally preceded by whitespace.
fn spaced0<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E> + 'a,
    E: ParseError<&'a str>,
{
    preceded(multispace0, inner)
}

// parses something, absolutely preceded by whitespace.
fn spaced1<'a, F, O, E>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E> + 'a,
    E: ParseError<&'a str>,
{
    preceded(multispace1, inner)
}

#[derive(Debug)]
pub enum Item {
    Fn(func::DecFn),
}

pub fn item<'a, E>(s: &'a str) -> IResult<&'a str, (Identifier, Item), E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    context("item", map(decfn, |(i, f)| (i, Item::Fn(f))))(s)
}

#[derive(Debug)]
pub struct Program {
    pub items: HashMap<Identifier, Item>,
}

pub fn program<'a, E>(s: &'a str) -> IResult<&'a str, Program, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    let (s, res) = map(many1(spaced0(item)), |items| Program {
        items: items.into_iter().collect(),
    })(s)?;
    let (s, _) = spaced0(eof)(s)?;
    Ok((s, res))
}
