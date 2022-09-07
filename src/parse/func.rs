use super::{
    spaced0, spaced1,
    statement::{block, identifier, Block, Identifier},
    types::dectype,
};
use crate::types::DecType;
use nom::{
    bytes::complete::tag,
    combinator::{cut, map},
    error::{context, ContextError, ParseError},
    multi::separated_list0,
    sequence::{delimited, separated_pair},
    IResult,
};

#[derive(Debug, Clone)]
pub struct DecFn {
    pub args: Box<[FnArg]>,
    pub return_type: DecType,
    pub block: Block,
}

pub fn decfn<'a, E>(s: &'a str) -> IResult<&'a str, (Identifier, DecFn), E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    let (s, _) = tag("fn")(s)?;
    let (s, ident) = cut(context("function name", spaced1(identifier)))(s)?;
    let (s, args) = cut(context(
        "function arguments",
        delimited(spaced0(tag("(")), fn_args, spaced0(tag(")"))),
    ))(s)?;
    let (s, _) = cut(context("`->`", spaced0(tag("->"))))(s)?;
    let (s, return_type) = cut(context("return type", spaced0(dectype)))(s)?;
    let (s, block) = cut(context("block of function", spaced0(block)))(s)?;
    Ok((
        s,
        (
            ident,
            DecFn {
                args: args.into_boxed_slice(),
                return_type,
                block,
            },
        ),
    ))
}

#[derive(Debug, Clone)]
pub struct FnArg {
    pub name: Identifier,
    pub dectype: DecType,
}

pub fn fn_arg<'a, E>(s: &'a str) -> IResult<&'a str, FnArg, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    map(
        separated_pair(
            context("argument name", identifier),
            cut(context("`:` separating argument name from type", spaced0(tag(":")))),
            cut(context("argument type", spaced0(dectype))),
        ),
        |(name, dectype)| FnArg { name, dectype },
    )(s)
}

pub fn fn_args<'a, E>(s: &'a str) -> IResult<&'a str, Vec<FnArg>, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    separated_list0(spaced0(tag(",")), spaced0(fn_arg))(s)
}
