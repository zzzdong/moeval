use std::sync::OnceLock;

use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser, Span,
};

use crate::ast::*;

#[derive(Debug)]
pub struct ParseError(Box<pest::error::Error<Rule>>);

impl ParseError {
    pub fn with_message(span: Span, message: impl ToString) -> Self {
        Self(Box::new(pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: message.to_string(),
            },
            span,
        )))
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<pest::error::Error<Rule>> for ParseError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        Self(Box::new(e))
    }
}

impl std::error::Error for ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct PestParser;

pub fn parse_file(input: &str) -> Result<Program> {
    let mut pairs = PestParser::parse(Rule::program, input)?;

    let pair = pairs.next().unwrap();
    parse_program(pair)
}

pub fn parse_expression_input(input: &str) -> Result<Expression> {
    let pairs = PestParser::parse(Rule::expression, input)?;
    parse_expression_pairs(pairs)
}

static PRATT_PARSER: OnceLock<PrattParser<Rule>> = OnceLock::new();

fn pratt_parser() -> &'static PrattParser<Rule> {
    // reference: https://doc.rust-lang.org/nightly/reference/expressions.html
    PRATT_PARSER.get_or_init(|| {
        PrattParser::new()
            .op(Op::infix(Rule::assign_operator, Assoc::Right)
                | Op::infix(Rule::add_assign_operator, Assoc::Right)
                | Op::infix(Rule::sub_assign_operator, Assoc::Right)
                | Op::infix(Rule::mul_assign_operator, Assoc::Right)
                | Op::infix(Rule::div_assign_operator, Assoc::Right)
                | Op::infix(Rule::mod_assign_operator, Assoc::Right))
            .op(Op::infix(Rule::range_operator, Assoc::Left))
            .op(Op::infix(Rule::or_operator, Assoc::Left))
            .op(Op::infix(Rule::and_operator, Assoc::Left))
            .op(Op::infix(Rule::equal_operator, Assoc::Left)
                | Op::infix(Rule::not_equal_operator, Assoc::Left)
                | Op::infix(Rule::less_operator, Assoc::Left)
                | Op::infix(Rule::less_equal_operator, Assoc::Left)
                | Op::infix(Rule::greater_operator, Assoc::Left)
                | Op::infix(Rule::greater_equal_operator, Assoc::Left))
            .op(Op::infix(Rule::add_operator, Assoc::Left)
                | Op::infix(Rule::sub_operator, Assoc::Left))
            .op(Op::infix(Rule::mul_operator, Assoc::Left)
                | Op::infix(Rule::div_operator, Assoc::Left)
                | Op::infix(Rule::mod_operator, Assoc::Left))
            .op(Op::infix(Rule::pow_operator, Assoc::Right))
            .op(Op::infix(Rule::as_operator, Assoc::Left))
            .op(Op::prefix(Rule::negative_operator) | Op::prefix(Rule::not_operator))
            .op(Op::postfix(Rule::try_operator))
            .op(Op::infix(Rule::as_operator, Assoc::Left))
            .op(Op::infix(Rule::dot_operator, Assoc::Left))
            .op(Op::postfix(Rule::member_operator)
                | Op::postfix(Rule::call_operator)
                | Op::postfix(Rule::index_operator)
                | Op::postfix(Rule::slice_operator))
            .op(Op::infix(Rule::path_operator, Assoc::Left))
    })
}

fn parse_program(pair: Pair<Rule>) -> Result<Program> {
    let mut program = Program::new();
    for item in pair.into_inner() {
        match item.as_rule() {
            Rule::statement => {
                let statement = parse_statement(item)?;
                program.stmts.push(statement);
            }
            // Rule::expression => {
            //     let expression = parse_expression(item)?;
            //     program.items.push(TopLevelItem::Expression(expression));
            // }
            Rule::EOI => {
                break;
            }
            _ => unreachable!("{item:?}"),
        }
    }

    Ok(program)
}

fn parse_statement(pair: Pair<Rule>) -> Result<Statement> {
    let pair = pair.into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::expression_statement => {
            let expression = parse_expression(pair)?;
            Ok(Statement::Expression(expression))
        }
        Rule::item_statement => {
            let stat = parse_item_statement(pair)?;
            Ok(Statement::Item(stat))
        }
        Rule::empty_statement => Ok(Statement::Empty),
        Rule::let_statement => {
            let stat = parse_let_statement(pair)?;
            Ok(Statement::Let(stat))
        }
        Rule::for_statement => {
            let stat = parse_for_statement(pair)?;
            Ok(Statement::For(stat))
        }
        Rule::loop_statement => {
            let stat = parse_loop_statement(pair)?;
            Ok(Statement::Loop(stat))
        }
        Rule::if_statement => {
            let stat = parse_if_statement(pair)?;
            Ok(Statement::If(stat))
        }
        Rule::return_statement => {
            let stat = parse_return_statement(pair)?;
            Ok(Statement::Return(stat))
        }
        Rule::break_statement => Ok(Statement::Break),
        Rule::continue_statement => Ok(Statement::Continue),
        _ => unreachable!("unknown statement: {pair:?}"),
    }
}

fn parse_item_statement(pair: Pair<Rule>) -> Result<ItemStatement> {
    let stat = pair.into_inner().next().unwrap();

    match stat.as_rule() {
        Rule::enum_item => Ok(ItemStatement::Enum(parse_enum_item(stat))),
        Rule::struct_item => Ok(ItemStatement::Struct(parse_struct_item(stat))),
        Rule::fn_item => parse_function_item(stat).map(ItemStatement::Fn),
        _ => unreachable!("unknown item statement: {stat:?}"),
    }
}

fn parse_enum_item(pair: Pair<Rule>) -> EnumItem {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();

    let variants = pairs.next().unwrap().into_inner();

    let variants = variants.map(parse_enum_field).collect();

    EnumItem { name, variants }
}

fn parse_enum_field(pair: Pair<Rule>) -> EnumVariant {
    let field = pair.into_inner().next().unwrap();
    match field.as_rule() {
        Rule::simple_enum_field => EnumVariant::Simple(field.as_str().to_string()),
        Rule::tuple_enum_field => parse_tuple_enum_field(field),
        _ => unreachable!("{field:?}"),
    }
}

fn parse_tuple_enum_field(pair: Pair<Rule>) -> EnumVariant {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let tuple = pairs.map(|item| parse_type_expression(item)).collect();

    EnumVariant::Tuple(name, tuple)
}

fn parse_struct_item(pair: Pair<Rule>) -> StructItem {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let fields = pairs.next().unwrap();
    let fields = fields
        .into_inner()
        .map(|field| parse_struct_field(field))
        .collect();

    StructItem { name, fields }
}

fn parse_struct_field(pair: Pair<Rule>) -> StructField {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let ty = parse_type_expression(pairs.next().unwrap());

    StructField { name, ty }
}

fn parse_function_item(pair: Pair<Rule>) -> Result<FunctionItem> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();
    let mut params = pairs.next().unwrap().into_inner();
    let params = if let Some(params) = params.next() {
        params
            .into_inner()
            .map(|param| parse_function_param(param))
            .collect()
    } else {
        Vec::new()
    };
    let mut return_type = pairs.next().unwrap().into_inner();
    let return_ty = return_type.next().map(parse_type_expression);

    let body = parse_block(pairs.next().unwrap())?;
    Ok(FunctionItem {
        name,
        params,
        return_ty,
        body,
    })
}

fn parse_function_param(pair: Pair<Rule>) -> FunctionParam {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();

    let ty = pairs.next().map(|pair| parse_type_expression(pair));

    FunctionParam { name, ty }
}

fn parse_let_statement(pair: Pair<Rule>) -> Result<LetStatement> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap().as_str().to_string();

    let type_annotation = pairs.next().unwrap();

    let ty = type_annotation
        .into_inner()
        .next()
        .map(|pair| parse_type_expression(pair));

    let assignment = pairs.next().unwrap();

    let value = if let Some(pair) = assignment.into_inner().next() {
        Some(parse_expression(pair)?)
    } else {
        None
    };

    Ok(LetStatement { name, ty, value })
}

fn parse_loop_statement(pair: Pair<Rule>) -> Result<LoopStatement> {
    let inner = pair.into_inner().next().unwrap();
    let body = parse_block(inner)?;
    Ok(LoopStatement { body })
}

fn parse_for_statement(pair: Pair<Rule>) -> Result<ForStatement> {
    let mut pairs = pair.into_inner();

    let pat = parse_pattern(pairs.next().unwrap())?;
    let iterable = parse_expression(pairs.next().unwrap())?;
    let body = parse_block(pairs.next().unwrap())?;

    Ok(ForStatement {
        pat,
        iterable,
        body,
    })
}

fn parse_if_statement(pair: Pair<Rule>) -> Result<IfStatement> {
    let mut pairs = pair.into_inner();

    let condition = parse_expression(pairs.next().unwrap())?;
    let then_branch = parse_block(pairs.next().unwrap())?;
    let else_branch = pairs
        .next()
        .map(|pair| match pair.as_rule() {
            Rule::block => parse_block(pair),
            Rule::if_statement => parse_if_statement(pair).map(|item| vec![Statement::If(item)]),
            _ => unreachable!("unknown else_branch: {:?}", pair),
        })
        .transpose()?;

    Ok(IfStatement {
        condition,
        then_branch,
        else_branch,
    })
}

fn parse_return_statement(pair: Pair<Rule>) -> Result<ReturnStatement> {
    let mut pairs = pair.into_inner();

    let value = pairs
        .next()
        .map(|pair| parse_expression(pair))
        .transpose()?;

    Ok(ReturnStatement { value })
}

fn parse_pattern(pair: Pair<Rule>) -> Result<Pattern> {
    let pat = pair.into_inner().next().unwrap();

    match pat.as_rule() {
        Rule::wildcard_pattern => Ok(Pattern::Wildcard),
        Rule::identifier => Ok(Pattern::Identifier(pat.as_str().to_string())),
        Rule::literal => Ok(Pattern::Literal(parse_literal(pat)?)),
        Rule::tuple_pattern => {
            let mut tuple = Vec::new();
            for item in pat.into_inner() {
                match item.as_rule() {
                    Rule::pattern => {
                        tuple.push(parse_pattern(item)?);
                    }
                    _ => unreachable!("unknown tuple pattern part: {item:?}"),
                }
            }
            Ok(Pattern::Tuple(tuple))
        }
        _ => unreachable!("unknown pattern: {pat:?}"),
    }
}

fn parse_block(pair: Pair<Rule>) -> Result<Vec<Statement>> {
    let statements = pair
        .into_inner()
        .map(parse_statement)
        .collect::<Result<_>>()?;
    Ok(statements)
}

fn parse_type_expression(pair: Pair<Rule>) -> TypeExpression {
    let pair = pair.into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::type_bool => TypeExpression::Boolean,
        Rule::type_byte => TypeExpression::Byte,
        Rule::type_int => TypeExpression::Integer,
        Rule::type_float => TypeExpression::Float,
        Rule::type_char => TypeExpression::Char,
        Rule::type_string => TypeExpression::String,
        Rule::type_user_defined => TypeExpression::UserDefined(pair.as_str().to_string()),
        Rule::type_array => TypeExpression::Array(Box::new(parse_type_expression(
            pair.into_inner().next().unwrap(),
        ))),
        Rule::type_tuple => {
            let tys = pair.into_inner().map(parse_type_expression).collect();
            TypeExpression::Tuple(tys)
        }
        Rule::type_generic => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let params = inner.map(parse_type_expression).collect();
            TypeExpression::Generic(name, params)
        }
        Rule::type_impl => TypeExpression::Impl(Box::new(parse_type_expression(
            pair.into_inner().next().unwrap(),
        ))),
        _ => unreachable!("unknown type expression: {pair:?}"),
    }
}

fn parse_expression(pair: Pair<Rule>) -> Result<Expression> {
    let pairs = pair.into_inner();

    parse_expression_pairs(pairs)
}

fn parse_expression_pairs(pairs: Pairs<Rule>) -> Result<Expression> {
    pratt_parser()
        .map_primary(parse_primary)
        .map_prefix(|op, rhs| {
            Ok(Expression::Prefix(
                op.as_str().parse::<PrefixOp>().unwrap(),
                Box::new(rhs?),
            ))
        })
        .map_postfix(parse_postfix)
        .map_infix(parse_infix)
        .parse(pairs)
}

fn parse_primary(pair: Pair<Rule>) -> Result<Expression> {
    match pair.as_rule() {
        Rule::identifier | Rule::literal | Rule::atom => parse_atom(pair),
        Rule::expression => parse_expression(pair),
        Rule::grouped_expression => parse_expression(pair.into_inner().next().unwrap()),
        _ => unreachable!("unknown primary: {:?}", pair),
    }
}

fn parse_infix(
    lhs: Result<Expression>,
    op: Pair<Rule>,
    rhs: Result<Expression>,
) -> Result<Expression> {
    match op.as_rule() {
        Rule::assign_operator => {
            let expr = Expression::Assign(AssignExpression {
                object: Box::new(lhs?),
                value: Box::new(rhs?),
                op: None,
            });
            Ok(expr)
        }
        Rule::add_assign_operator => {
            let expr = Expression::Assign(AssignExpression {
                object: Box::new(lhs?),
                value: Box::new(rhs?),
                op: Some(BinOp::Add),
            });
            Ok(expr)
        }
        Rule::sub_assign_operator => {
            let expr = Expression::Assign(AssignExpression {
                object: Box::new(lhs?),
                value: Box::new(rhs?),
                op: Some(BinOp::Sub),
            });
            Ok(expr)
        }
        Rule::mul_assign_operator => {
            let expr = Expression::Assign(AssignExpression {
                object: Box::new(lhs?),
                value: Box::new(rhs?),
                op: Some(BinOp::Mul),
            });
            Ok(expr)
        }
        Rule::div_assign_operator => {
            let expr = Expression::Assign(AssignExpression {
                object: Box::new(lhs?),
                value: Box::new(rhs?),
                op: Some(BinOp::Div),
            });
            Ok(expr)
        }
        Rule::mod_assign_operator => {
            let expr = Expression::Assign(AssignExpression {
                object: Box::new(lhs?),
                value: Box::new(rhs?),
                op: Some(BinOp::Mod),
            });
            Ok(expr)
        }
        _ => Ok(Expression::Binary(
            op.as_str().parse::<BinOp>().unwrap(),
            Box::new(lhs?),
            Box::new(rhs?),
        )),
    }
}

fn parse_postfix(lhs: Result<Expression>, op: Pair<Rule>) -> Result<Expression> {
    let object = Box::new(lhs?);

    match op.as_rule() {
        Rule::try_operator => {
            let expr = Expression::Try(object);
            Ok(expr)
        }
        Rule::member_operator => {
            let property = op.into_inner().next().unwrap().as_str().to_string();
            let expr = MemberExpression { object, property };
            Ok(Expression::Member(expr))
        }
        Rule::call_operator => {
            let args = op.into_inner().next().unwrap();
            let args: Result<Vec<Expression>> = args.into_inner().map(parse_expression).collect();
            let expr = CallExpression {
                func: object,
                args: args?,
            };

            Ok(Expression::Call(expr))
        }
        Rule::index_operator => {
            let index = op.into_inner().next().unwrap();
            let index = parse_expression(index.into_inner().next().unwrap())?;

            let expr = IndexExpression {
                object,
                index: Box::new(index),
            };

            Ok(Expression::Index(expr))
        }
        Rule::slice_operator => {
            let mut begin = None;
            let mut end = None;

            for arg in op.into_inner() {
                match arg.as_rule() {
                    Rule::slice_begin => {
                        begin = Some(Box::new(parse_expression(
                            arg.into_inner().next().unwrap(),
                        )?))
                    }
                    Rule::slice_end => {
                        end = Some(Box::new(parse_expression(
                            arg.into_inner().next().unwrap(),
                        )?))
                    }
                    _ => unreachable!(),
                }
            }

            let expr = SliceExpression { object, begin, end };

            Ok(Expression::Slice(expr))
        }
        _ => unreachable!("unknown postfix: {:?}", op),
    }
}

fn parse_atom(pair: Pair<Rule>) -> Result<Expression> {
    let atom = pair.into_inner().next().unwrap();

    match atom.as_rule() {
        Rule::identifier => parse_identifier(atom).map(Expression::Identifier),
        Rule::literal => parse_literal(atom).map(Expression::Literal),
        Rule::tuple => parse_tuple(atom).map(Expression::Tuple),
        Rule::array => parse_array(atom).map(Expression::Array),
        Rule::closure => parse_closure(atom).map(Expression::Closure),
        Rule::env => parse_env(atom).map(Expression::Environment),
        _ => unreachable!("unknown atom: {:?}", atom),
    }
}

fn parse_closure(pair: Pair<Rule>) -> Result<ClosureExpression> {
    let mut pairs = pair.into_inner();

    let mut params = pairs.next().unwrap().into_inner();
    let params = if let Some(params) = params.next() {
        params
            .into_inner()
            .map(|param| parse_function_param(param))
            .collect()
    } else {
        Vec::new()
    };

    let body = parse_block(pairs.next().unwrap())?;

    Ok(ClosureExpression { params, body })
}

fn parse_identifier(pair: Pair<Rule>) -> Result<IdentifierExpression> {
    let mut pairs = pair.into_inner();
    let name = pairs.next().unwrap();

    Ok(IdentifierExpression(name.as_str().to_string()))
}

fn parse_literal(pair: Pair<Rule>) -> Result<LiteralExpression> {
    let mut pairs = pair.into_inner();
    let value = pairs.next().unwrap();

    match value.as_rule() {
        Rule::boolean => Ok(LiteralExpression::Boolean(
            pairs.next().unwrap().as_str() == "true",
        )),
        Rule::integer => Ok(LiteralExpression::Integer(value.as_str().parse().map_err(
            |err| ParseError::with_message(value.as_span(), format!("parse integer failed, {err}")),
        )?)),
        Rule::float => Ok(LiteralExpression::Float(value.as_str().parse().map_err(
            |err| ParseError::with_message(value.as_span(), format!("parse float failed, {err}")),
        )?)),
        Rule::string => Ok(LiteralExpression::String(
            value.as_str().trim_matches('"').to_string(),
        )),
        Rule::character => Ok(LiteralExpression::Char(
            value.as_str().chars().next().unwrap(),
        )),
        _ => unreachable!("unknown literal: {:?}", value),
    }
}

fn parse_tuple(pair: Pair<Rule>) -> Result<TupleExpression> {
    let pairs = pair.into_inner();

    let elements: Result<Vec<Expression>> = pairs.map(parse_expression).collect();

    Ok(TupleExpression(elements?))
}

fn parse_array(pair: Pair<Rule>) -> Result<ArrayExpression> {
    let pairs = pair.into_inner();

    let elements: Result<Vec<Expression>> = pairs.map(parse_expression).collect();

    Ok(ArrayExpression(elements?))
}

fn parse_env(pair: Pair<Rule>) -> Result<EnvironmentExpression> {
    let mut pairs = pair.into_inner();

    let name = pairs.next().unwrap();

    Ok(EnvironmentExpression(name.as_str().to_string()))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser() {
        let input = r#"
enum A {}

enum B {
    AA,
    BB,
}

enum C {
    AA,
    BB(int, float),
}
struct A {}
struct B {
    a: int,
}
fn A() {}
fn B(a, b: float) -> int {}
fn C(a: int, b: float) -> int {
    let a = 1 + 2 * 3 / 4 % a - A::aa;
    x = y + z;
    for i in iii {}
    for (a, b) in 1..=100 {
        let a = 1 + 2 * 3 / 4 % a - A::aa;
    }

    return a;
}
a();
a(a, b, c);
1.a(1).b(11).c(111);
(a,b).to_string();
1.to_string();
(a(1)).b();
(1).b();
(1,).b().c();
[a,b,c][0];
[a,b,c][..];
[a,b,c][1..];
[a,b,c][1..2];
[a,b,c][..2];
        "#;
        let pairs = PestParser::parse(Rule::program, input).unwrap();
        for pair in pairs {
            println!("{:?}", pair.into_inner());
        }
    }

    #[test]
    fn test_expr() {
        let input = "1 + 2 * 3 + c + d.e.f.g";
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expr = parse_expression_pairs(pairs);
        println!("=> {:?}", expr);
    }

    #[test]
    fn test_parse_program() {
        let input = r#"
enum A {}

enum B {
    AA,
    BB,
}

enum C {
    AA,
    BB(int, float),
}
struct A {}
struct B {
    a: int,
}
fn A() -> Result<float> {}
fn B(a, b: float) -> int {}
fn C(a: int, b: float) -> int {
    let a = 1 + 2 * 3 / 4 % a - A::aa;
    x = y + z;
    for i in iii {}
    for (a, b) in 1..=100 {
        let a = 1 + 2 * 3 / 4 % a - A::aa;
    }

    return a;
}
loop {
    let a = 1;
    break;
}
a();
a(a, b, c);
1.a(1).b(11).c(111);
(a,b).to_string();
1.to_string();
(a(1)).b();
(1).b();
(1,).b().c();
[a,b,c][0];
[a,b,c][..];
[a,b,c][1..];
[a,b,c][1..2];
[a,b,c][..2];
(1,);
(1,2);
(1,2,3);

fn a() -> Result<float> {
}

let a : Result<flaot, Error> = Ok(0);
let a : []int = [0, 1, 2];
let a : (int, float) = (0, 1.0);

let a : impl AAA;

fn add(a: int, b: int) -> int {
    return a + b;
}

let a: Fn<(int, int), int> = add;

let r = $req;
"#;
        let mut pairs = PestParser::parse(Rule::program, input).unwrap();

        let pair = pairs.next().unwrap();

        let program = parse_program(pair).unwrap();

        println!("{program:?}");
    }
}
