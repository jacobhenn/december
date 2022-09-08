use super::{spaced0, spaced1, string};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace1, one_of},
    combinator::{cut, fail, map, opt, recognize},
    error::{context, ContextError, ParseError},
    multi::{many0, many0_count, many1, separated_list0, separated_list1},
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
    Assignment(Assignment),
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
        terminated(
            map(assignment, Statement::Assignment),
            context("semicolon", spaced0(tag(";"))),
        ),
    ))(s)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpr),
    Identifier(Identifier),
    FnCall(FnCallExpr),
    If(IfExpr),
    Loop(LoopExpr),
    List(ListExpr),
    IndexExpr(IndexExpr),
}

pub fn expression<'a, E>(s: &'a str) -> IResult<&'a str, Expression, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    alt((
        map(if_expr, Expression::If),
        map(loop_expr, Expression::Loop),
        map(list_expr, Expression::List),
        map(index_expr, Expression::IndexExpr),
        map(fn_call_expr, Expression::FnCall),
        map(literal_expr, Expression::Literal),
        map(identifier, Expression::Identifier),
        // operator_expr,
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
    use nom::error::VerboseError;

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
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_eq!(
        float::<VerboseError<&str>>("-123.456e7foo"),
        Ok(("foo", -123.456e7))
    );
    assert_matches!(float::<VerboseError<&str>>(".5"), Err(_));
    assert_matches!(float::<VerboseError<&str>>("5."), Err(_));
    assert_matches!(float::<VerboseError<&str>>("foo.78"), Err(_));
    assert_matches!(float::<VerboseError<&str>>("78.foo"), Err(_));
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
        cut(context("closing `)` of call expression", spaced0(tag(")")))),
    )(s)?;

    Ok((s, FnCallExpr { func, args }))
}

#[test]
fn test_fn_call_expr() {
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_matches!(fn_call_expr::<VerboseError<&str>>("foo()"), Ok(_));
    assert_matches!(fn_call_expr::<VerboseError<&str>>("foo(x , y,z )"), Ok(_));
    assert_matches!(fn_call_expr::<VerboseError<&str>>("foo(,)"), Err(_));
    assert_matches!(fn_call_expr::<VerboseError<&str>>("foo(x,)"), Err(_));
    assert_matches!(fn_call_expr::<VerboseError<&str>>("foo(x, y"), Err(_));
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
        separated_pair(
            identifier,
            spaced0(tag(":=")),
            cut(context(
                "expression on right side of definition",
                spaced0(expression),
            )),
        ),
        |(name, value)| Definition { name, value },
    )(s)
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub branches: Vec<(Expression, Block)>,
    pub else_block: Option<Block>,
}

pub fn if_expr<'a, E>(s: &'a str) -> IResult<&'a str, IfExpr, E>
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
        IfExpr {
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

#[derive(Debug, Clone)]
pub struct ListExpr {
    pub elements: Vec<Expression>,
}

pub fn list_expr<'a, E>(s: &'a str) -> IResult<&'a str, ListExpr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    // TODO: allow empty list expressions
    map(
        delimited(
            char('['),
            cut(context(
                "elements of list",
                separated_list1(spaced0(char(',')), spaced0(expression)),
            )),
            cut(context("closing bracket of list", spaced0(char(']')))),
        ),
        |elements| ListExpr { elements },
    )(s)
}

#[test]
fn test_list_expr() {
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_matches!(
        list_expr::<VerboseError<&str>>(r#"[rhombus, 5, "glonk"]"#),
        Ok(_)
    );
    assert_matches!(
        list_expr::<VerboseError<&str>>("[[6], [[void]]], [5]"),
        Ok(_)
    );
    assert_matches!(list_expr::<VerboseError<&str>>("[]"), Err(_));
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub list: Identifier,
    pub index: Box<Expression>,
}

pub fn index_expr<'a, E>(s: &'a str) -> IResult<&'a str, IndexExpr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    map(
        tuple((
            identifier,
            delimited(
                spaced0(char('[')),
                cut(context("index", spaced0(expression))),
                cut(context("closing bracket of index", spaced0(char(']')))),
            ),
        )),
        |(list, index)| IndexExpr {
            list,
            index: Box::new(index),
        },
    )(s)
}

#[test]
fn test_index_expr() {
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_matches!(
        index_expr::<VerboseError<&str>>("x [ int_add(5, 2) ]"),
        Ok(_)
    );
    assert_matches!(
        index_expr::<VerboseError<&str>>("(int_add(5, 2))[x]"),
        Err(_)
    );
}

#[derive(Debug, Clone)]
pub struct LoopExpr {
    pub block: Block,
}

pub fn loop_expr<'a, E>(s: &'a str) -> IResult<&'a str, LoopExpr, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    map(preceded(tag("loop"), spaced0(block)), |block| LoopExpr {
        block,
    })(s)
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: Identifier,
    pub value: Expression,
}

pub fn assignment<'a, E>(s: &'a str) -> IResult<&'a str, Assignment, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    map(
        separated_pair(
            identifier,
            spaced0(tag("=")),
            cut(context(
                "expression on right side of assignment",
                spaced0(expression),
            )),
        ),
        |(name, value)| Assignment { name, value },
    )(s)
}

pub fn operator_expr<'a, E>(s: &'a str) -> IResult<&'a str, Expression, E>
where
    E: ParseError<&'a str> + ContextError<&'a str> + 'a,
{
    alt((
        map(
            separated_pair(
                expression,
                spaced0(char('+')),
                cut(context(
                    "expression on right side of `+` operator",
                    spaced0(expression),
                )),
            ),
            |(x, y)| {
                Expression::FnCall(FnCallExpr {
                    func: Identifier::from("int_add"),
                    args: vec![x, y],
                })
            },
        ),
        map(
            separated_pair(
                expression,
                spaced0(char('%')),
                cut(context(
                    "expression on right side of `%` operator",
                    spaced0(expression),
                )),
            ),
            |(x, y)| {
                Expression::FnCall(FnCallExpr {
                    func: Identifier::from("int_mod"),
                    args: vec![x, y],
                })
            },
        ),
    ))(s)
}

#[test]
fn test_operator_expr() {
    use assert_matches::assert_matches;
    use nom::error::VerboseError;

    assert_matches!(operator_expr::<VerboseError<&str>>("x+y"), Ok(_));
    assert_matches!(expression::<VerboseError<&str>>("x  %3"), Ok(_));
    assert_matches!(operator_expr::<VerboseError<&str>>("x ++ 3"), Err(_));
}
