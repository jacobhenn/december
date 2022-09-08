use super::{spaced0, spaced1, string};
use crate::error::RuntimeError;
use assert_matches::assert_matches;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace1, one_of},
    combinator::{cut, fail, map, opt, recognize},
    error::{context, ContextError, ParseError, VerboseError},
    multi::{many0, many0_count, separated_list0, many1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

pub fn block<'a, E>(s: &'a str) -> IResult<&'a str, Block, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    map(
        delimited(
            tag("{"),
            many0(spaced0(statement)),
            context("end of block", spaced0(tag("}"))),
        ),
        |statements| Block { statements },
    )(s)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Definition(Definition),
    Expression(Expression),
}

pub fn statement<'a, E>(s: &'a str) -> IResult<&'a str, Statement, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    alt((
        terminated(
            map(expression, Statement::Expression),
            context("semicolon", spaced0(tag(";"))),
        ),
        terminated(
            map(definition, Statement::Definition),
            context("semicolon", spaced0(tag(";"))),
        ),
    ))(s)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpr),
    Identifier(Identifier),
    FnCall(FnCallExpr),
    If(If),
}

pub fn expression<'a, E>(s: &'a str) -> IResult<&'a str, Expression, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    alt((
        map(if_expr, Expression::If),
        map(literal_expr, Expression::Literal),
        map(fn_call_expr, Expression::FnCall),
        map(identifier, Expression::Identifier),
    ))(s)
}

#[derive(Debug, Clone)]
pub enum LiteralExpr {
    String(String),
    Bool(bool),
    Int(i128),
    Float(f64),
    Void,
}

pub fn literal_expr<'a, E>(s: &'a str) -> IResult<&'a str, LiteralExpr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    alt((
        map(tag("void"), |_| LiteralExpr::Void),
        map(float, LiteralExpr::Float),
        map(int, LiteralExpr::Int),
        map(bool, LiteralExpr::Bool),
        map(string::parse, LiteralExpr::String),
    ))(s)
}

macro_rules! digit {
    () => {
        one_of("0123456789")
    };
}

pub fn int<'a, E>(s: &'a str) -> IResult<&'a str, i128, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    let (s, n) = recognize(preceded(alt((digit!(), char('-'))), many0(digit!())))(s)?;
    match str::parse::<i128>(n) {
        Ok(n) => Ok((s, n)),
        Err(_) => cut(context(
            "integer literal to be between −2^127 and 2^127-1",
            fail,
        ))(s),
    }
}

#[test]
fn test_int() {
    assert_eq!(int::<VerboseError<&str>>("123456"), Ok(("", 123456)));
    assert!(int::<VerboseError<&str>>("foo").is_err());
}

pub fn float<'a, E>(s: &'a str) -> IResult<&'a str, f64, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    let (s, n) = recognize(tuple((
        opt(char('-')),
        many1(digit!()),
        char('.'),
        many1(digit!()),
        opt(preceded(char('e'), many1(digit!()))),
    )))(s)?;
    match str::parse::<f64>(n) {
        Ok(n) => Ok((s, n)),
        Err(_) => cut(context(
            "floating point literal to be between -2147483648 and 2147483647",
            fail,
        ))(s),
    }
}

#[test]
fn test_float() {
    assert_eq!(
        float::<VerboseError<&str>>("-123.456e7foo"),
        Ok(("foo", -123.456e7))
    );
    assert_matches!(float::<VerboseError<&str>>(".5"), Err(_));
    assert_matches!(float::<VerboseError<&str>>("5."), Err(_));
}

pub fn bool<'a, E>(s: &'a str) -> IResult<&'a str, bool, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    alt((map(tag("true"), |_| true), map(tag("false"), |_| false)))(s)
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Self {
            name: s.to_string(),
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub fn identifier<'a, E>(s: &'a str) -> IResult<&'a str, Identifier, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    context(
        "identifier",
        map(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0_count(alt((alphanumeric1, tag("_")))),
            )),
            |name: &str| Identifier {
                name: name.to_string(),
            },
        ),
    )(s)
}

#[derive(Debug, Clone)]
pub struct FnCallExpr {
    pub func: Identifier,
    pub args: Vec<Expression>,
}

pub fn fn_call_expr<'a, E>(s: &'a str) -> IResult<&'a str, FnCallExpr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    let (s, func) = context("function of call expression", identifier)(s)?;
    let (s, args) = delimited(
        tag("("),
        // after consuming an identifier followed by `(`, we are now in the correct branch and
        // can `cut` out
        cut(separated_list0(
            spaced0(tag(",")),
            context("argument of call expression", spaced0(expression)),
        )),
        cut(spaced0(tag(")"))),
    )(s)?;

    Ok((s, FnCallExpr { func, args }))
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub name: Identifier,
    pub value: Expression,
}

pub fn definition<'a, E>(s: &'a str) -> IResult<&'a str, Definition, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    map(
        separated_pair(identifier, spaced0(tag(":=")), cut(spaced0(expression))),
        |(name, value)| Definition { name, value },
    )(s)
}

#[derive(Debug, Clone)]
pub struct If {
    pub branches: Vec<(Expression, Block)>,
    pub else_block: Option<Block>,
}

pub fn if_expr<'a, E>(s: &'a str) -> IResult<&'a str, If, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    // TODO: clean up prepending to vector
    let (s, first_condition) = preceded(
        tag("if"),
        preceded(
            multispace1,
            cut(context("condition of `if` statement", if_condition)),
        ),
    )(s)?;
    let (s, mut conditions) = many0(preceded(
        spaced0(tag("else if")),
        cut(spaced1(if_condition)),
    ))(s)?;
    let (s, else_block) = opt(preceded(spaced0(tag("else")), cut(spaced1(block))))(s)?;
    conditions.insert(0, first_condition);
    Ok((
        s,
        If {
            branches: conditions,
            else_block,
        },
    ))
}

pub fn if_condition<'a, E>(s: &'a str) -> IResult<&'a str, (Expression, Block), E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    pair(expression, spaced0(block))(s)
}
