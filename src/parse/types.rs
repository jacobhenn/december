use crate::types::DecType;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{ContextError, ParseError},
    IResult,
};

pub fn void<'a, E>(s: &'a str) -> IResult<&'a str, DecType, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    map(tag("void"), |_| DecType::Void)(s)
}

pub fn string<'a, E>(s: &'a str) -> IResult<&'a str, DecType, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    map(tag("String"), |_| DecType::String)(s)
}

pub fn dectype<'a, E>(s: &'a str) -> IResult<&'a str, DecType, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    alt((void, string))(s)
}
