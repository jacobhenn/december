use super::{spaced0, spaced1, string};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1},
    combinator::{cut, map, opt, recognize},
    error::{context, ContextError, ParseError},
    multi::{many0, many0_count, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
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
            map(expression, |e| Statement::Expression(e)),
            context("semicolon", spaced0(tag(";"))),
        ),
        terminated(
            map(definition, |d| Statement::Definition(d)),
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
    StringLiteral(String),
    BoolLiteral(bool),
}

pub fn literal_expr<'a, E>(s: &'a str) -> IResult<&'a str, LiteralExpr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str>,
{
    alt((
        map(string::parse, |t| LiteralExpr::StringLiteral(t)),
        map(bool, |b| LiteralExpr::BoolLiteral(b)),
    ))(s)
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
    let (s, first_condition) = preceded(tag("if"), spaced1(if_condition))(s)?;
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
