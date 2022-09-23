use crate::types::DecType;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::char,
    combinator::map,
    error::{ContextError, ParseError},
    sequence::delimited,
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

pub fn bool<'a, E>(s: &'a str) -> IResult<&'a str, DecType, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    map(tag("bool"), |_| DecType::Bool)(s)
}

pub fn list<'a, E>(s: &'a str) -> IResult<&'a str, DecType, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    map(delimited(char('['), dectype, char(']')), |s| {
        DecType::List(Box::new(s))
    })(s)
}

#[test]
fn test_list() {
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_matches!(list::<VerboseError<&str>>("[String]"), Ok(_));
    assert_matches!(dectype::<VerboseError<&str>>("[[[[[bool]]]]]"), Ok(_));
}

pub fn int<'a, E>(s: &'a str) -> IResult<&'a str, DecType, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    map(tag("int"), |_| DecType::Int)(s)
}

pub fn dectype<'a, E>(s: &'a str) -> IResult<&'a str, DecType, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    alt((void, string, bool, int, list))(s)
}
