use std::sync::OnceLock;

use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser, Span,
};

use super::ast::*;

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
#[grammar = "ast/grammar.pest"]
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

pub fn parse_top_level(input: &str) -> Result<TopLevelItem> {
    let mut pairs = PestParser::parse(Rule::top_level, input)?;
    let pair = pairs.next().unwrap();
    let pair = pair.into_inner().next().unwrap();

    match pair.as_rule() {
        Rule::statement => {
            let statement = parse_statement(pair)?;
            Ok(TopLevelItem::Statement(statement))
        }
        Rule::expression => {
            let expression = parse_expression(pair)?;
            Ok(TopLevelItem::Expression(expression))
        }
        _ => unreachable!("unknown top level: {pair:?}"),
    }
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
            .op(Op::postfix(Rule::try_operator)
                | Op::postfix(Rule::await_operator)
                | Op::postfix(Rule::member_operator)
                | Op::postfix(Rule::call_operator)
                | Op::postfix(Rule::index_operator))
            .op(Op::postfix(Rule::slice_operator))
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
            let inner = parse_expression(index.into_inner().next().unwrap())?;

            match inner {
                Expression::Binary(BinOp::Range, begin, end) => {
                    let expr = SliceExpression {
                        object,
                        range: BinOp::Range,
                        begin: Some(Box::new(*begin)),
                        end: Some(Box::new(*end)),
                    };

                    Ok(Expression::Slice(expr))
                }
                Expression::Binary(BinOp::RangeInclusive, begin, end) => {
                    let expr = SliceExpression {
                        object,
                        range: BinOp::RangeInclusive,
                        begin: Some(Box::new(*begin)),
                        end: Some(Box::new(*end)),
                    };

                    Ok(Expression::Slice(expr))
                }
                _ => {
                    let expr = IndexExpression {
                        object,
                        index: Box::new(inner),
                    };

                    Ok(Expression::Index(expr))
                }
            }
        }
        Rule::slice_operator => {
            let mut begin = None;
            let mut end = None;
            let mut range = BinOp::Range;

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
                    Rule::range_operator => {
                        range = arg.as_str().parse::<BinOp>().unwrap();
                    }
                    r => unreachable!("{r:?}"),
                }
            }

            let expr = SliceExpression {
                object,
                range,
                begin,
                end,
            };

            Ok(Expression::Slice(expr))
        }
        Rule::await_operator => {
            let expr = Expression::Await(object);
            Ok(expr)
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
        Rule::map => parse_map(atom).map(Expression::Map),
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
        Rule::boolean => Ok(LiteralExpression::Boolean(value.as_str() == "true")),
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

fn parse_map_item(pair: Pair<Rule>) -> Result<(Expression, Expression)> {
    let mut pairs = pair.into_inner();

    let key_pair = pairs.next().unwrap();
    let key = parse_expression(key_pair.clone())?;
    if !key.is_literal() {
        return Err(ParseError::with_message(
            key_pair.as_span(),
            "map key must be literal".to_string(),
        ));
    }

    let value = parse_expression(pairs.next().unwrap())?;

    Ok((key, value))
}

fn parse_map(pair: Pair<Rule>) -> Result<MapExpression> {
    let pairs = pair.into_inner();

    let elements: Result<Vec<(Expression, Expression)>> = pairs.map(parse_map_item).collect();

    Ok(MapExpression(elements?))
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
    fn test_let_statement() {
        let input = r#"let a = 1;"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::Let(LetStatement {
                name: "a".to_string(),
                ty: None,
                value: Some(Expression::Literal(LiteralExpression::Integer(1))),
            }))
        );
    }

    #[test]
    fn test_let_statement_with_type() {
        let input = r#"let a: int = 1;"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::Let(LetStatement {
                name: "a".to_string(),
                ty: Some(TypeExpression::Integer),
                value: Some(Expression::Literal(LiteralExpression::Integer(1))),
            }))
        );
    }

    #[test]
    fn test_empty_statement() {
        let input = r#";"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(statement, TopLevelItem::Statement(Statement::Empty));
    }

    #[test]
    fn test_break_statement() {
        let input = r#"break;"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(statement, TopLevelItem::Statement(Statement::Break));
    }

    #[test]
    fn test_continue_statement() {
        let input = r#"continue;"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(statement, TopLevelItem::Statement(Statement::Continue));
    }

    #[test]
    fn test_return_statement() {
        let input = r#"return 1;"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::Return(ReturnStatement {
                value: Some(Expression::Literal(LiteralExpression::Integer(1))),
            }))
        );
    }

    #[test]
    fn test_expression_statement() {
        let input = r#"1 + 2;"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::Expression(Expression::Binary(
                BinOp::Add,
                Box::new(Expression::Literal(LiteralExpression::Integer(1))),
                Box::new(Expression::Literal(LiteralExpression::Integer(2))),
            )))
        );
    }

    #[test]
    fn test_for_statement() {
        let input = r#"for i in 0..10 {}"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::For(ForStatement {
                pat: Pattern::Identifier("i".to_string()),
                iterable: Expression::Binary(
                    BinOp::Range,
                    Box::new(Expression::Literal(LiteralExpression::Integer(0))),
                    Box::new(Expression::Literal(LiteralExpression::Integer(10))),
                ),
                body: vec![],
            }))
        );
    }

    #[test]
    fn test_loop_statement() {
        let input = r#"loop { break; }"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::Loop(LoopStatement {
                body: vec![Statement::Break],
            }))
        );
    }

    #[test]
    fn test_if_statement() {
        let input = r#"if a == 1 { return 1; }"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::If(IfStatement {
                condition: Expression::Binary(
                    BinOp::Equal,
                    Box::new(Expression::Identifier(IdentifierExpression(
                        "a".to_string()
                    ))),
                    Box::new(Expression::Literal(LiteralExpression::Integer(1))),
                ),
                then_branch: vec![Statement::Return(ReturnStatement {
                    value: Some(Expression::Literal(LiteralExpression::Integer(1))),
                })],
                else_branch: None,
            }))
        );

        // 新增测试用例：if语句带else分支
        let input = r#"if a == 2 { return 2; } else { return 3; }"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::If(IfStatement {
                condition: Expression::Binary(
                    BinOp::Equal,
                    Box::new(Expression::Identifier(IdentifierExpression(
                        "a".to_string()
                    ))),
                    Box::new(Expression::Literal(LiteralExpression::Integer(2))),
                ),
                then_branch: vec![Statement::Return(ReturnStatement {
                    value: Some(Expression::Literal(LiteralExpression::Integer(2))),
                })],
                else_branch: Some(vec![Statement::Return(ReturnStatement {
                    value: Some(Expression::Literal(LiteralExpression::Integer(3))),
                })]),
            }))
        );

        // 新增测试用例：更复杂的条件表达式
        let input = r#"if a > 0 && b < 10 { return 4; }"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::If(IfStatement {
                condition: Expression::Binary(
                    BinOp::LogicAnd,
                    Box::new(Expression::Binary(
                        BinOp::Greater,
                        Box::new(Expression::Identifier(IdentifierExpression(
                            "a".to_string()
                        ))),
                        Box::new(Expression::Literal(LiteralExpression::Integer(0))),
                    )),
                    Box::new(Expression::Binary(
                        BinOp::Less,
                        Box::new(Expression::Identifier(IdentifierExpression(
                            "b".to_string()
                        ))),
                        Box::new(Expression::Literal(LiteralExpression::Integer(10))),
                    )),
                ),
                then_branch: vec![Statement::Return(ReturnStatement {
                    value: Some(Expression::Literal(LiteralExpression::Integer(4))),
                })],
                else_branch: None,
            }))
        );
    }

    #[test]
    fn test_enum_item() {
        let input = r#"enum A { AA, BB(int, float) }"#;
        let item_statement = parse_top_level(input).unwrap();
        assert_eq!(
            item_statement,
            TopLevelItem::Statement(Statement::Item(ItemStatement::Enum(EnumItem {
                name: "A".to_string(),
                variants: vec![
                    EnumVariant::Simple("AA".to_string()),
                    EnumVariant::Tuple(
                        "BB".to_string(),
                        vec![TypeExpression::Integer, TypeExpression::Float,]
                    ),
                ],
            })))
        );
    }

    #[test]
    fn test_struct_item() {
        let input = r#"struct A { a: int, b: float }"#;
        let item_statement = parse_top_level(input).unwrap();
        assert_eq!(
            item_statement,
            TopLevelItem::Statement(Statement::Item(ItemStatement::Struct(StructItem {
                name: "A".to_string(),
                fields: vec![
                    StructField {
                        name: "a".to_string(),
                        ty: TypeExpression::Integer,
                    },
                    StructField {
                        name: "b".to_string(),
                        ty: TypeExpression::Float,
                    },
                ],
            })))
        );
    }

    #[test]
    fn test_function_item() {
        let input = r#"fn A(a: int, b: float) -> int { return a + b; }"#;
        let item_statement = parse_top_level(input).unwrap();
        assert_eq!(
            item_statement,
            TopLevelItem::Statement(Statement::Item(ItemStatement::Fn(FunctionItem {
                name: "A".to_string(),
                params: vec![
                    FunctionParam {
                        name: "a".to_string(),
                        ty: Some(TypeExpression::Integer),
                    },
                    FunctionParam {
                        name: "b".to_string(),
                        ty: Some(TypeExpression::Float),
                    },
                ],
                return_ty: Some(TypeExpression::Integer),
                body: vec![Statement::Return(ReturnStatement {
                    value: Some(Expression::Binary(
                        BinOp::Add,
                        Box::new(Expression::Identifier(IdentifierExpression(
                            "a".to_string()
                        ))),
                        Box::new(Expression::Identifier(IdentifierExpression(
                            "b".to_string()
                        ))),
                    )),
                }),],
            })))
        );
    }

    #[test]
    fn test_tuple_expression() {
        let input = r#"(1, 2, 3)"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Tuple(TupleExpression(vec![
                Expression::Literal(LiteralExpression::Integer(1)),
                Expression::Literal(LiteralExpression::Integer(2)),
                Expression::Literal(LiteralExpression::Integer(3)),
            ]))
        );
    }

    #[test]
    fn test_array_expression() {
        let input = r#"[1, 2, 3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Array(ArrayExpression(vec![
                Expression::Literal(LiteralExpression::Integer(1)),
                Expression::Literal(LiteralExpression::Integer(2)),
                Expression::Literal(LiteralExpression::Integer(3)),
            ]))
        );
    }

    #[test]
    fn test_map_expression() {
        let input = r#"{"a": 1, "b": 2}"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Map(MapExpression(vec![
                (
                    Expression::Literal(LiteralExpression::String("a".to_string())),
                    Expression::Literal(LiteralExpression::Integer(1))
                ),
                (
                    Expression::Literal(LiteralExpression::String("b".to_string())),
                    Expression::Literal(LiteralExpression::Integer(2))
                ),
            ]))
        );
    }

    #[test]
    fn test_call_expression() {
        let input = r#"f(1, 2, 3)"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Call(CallExpression {
                func: Box::new(Expression::Identifier(IdentifierExpression(
                    "f".to_string()
                ))),
                args: vec![
                    Expression::Literal(LiteralExpression::Integer(1)),
                    Expression::Literal(LiteralExpression::Integer(2)),
                    Expression::Literal(LiteralExpression::Integer(3)),
                ],
            })
        );
    }

    #[test]
    fn test_member_expression() {
        let input = r#"a.b.c"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Member(MemberExpression {
                object: Box::new(Expression::Member(MemberExpression {
                    object: Box::new(Expression::Identifier(IdentifierExpression(
                        "a".to_string()
                    ))),
                    property: "b".to_string(),
                })),
                property: "c".to_string(),
            })
        );
    }

    #[test]
    fn test_index_expression() {
        let input = r#"a[1]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Index(IndexExpression {
                object: Box::new(Expression::Identifier(IdentifierExpression(
                    "a".to_string()
                ))),
                index: Box::new(Expression::Literal(LiteralExpression::Integer(1))),
            })
        );
    }

    #[test]
    fn test_slice_expression() {
        let input = r#"a[1..3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Slice(SliceExpression {
                object: Box::new(Expression::Identifier(IdentifierExpression(
                    "a".to_string()
                ))),
                range: BinOp::Range,
                begin: Some(Box::new(Expression::Literal(LiteralExpression::Integer(1)))),
                end: Some(Box::new(Expression::Literal(LiteralExpression::Integer(3)))),
            })
        );

        let input = r#"a[1..=3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Slice(SliceExpression {
                object: Box::new(Expression::Identifier(IdentifierExpression(
                    "a".to_string()
                ))),
                range: BinOp::RangeInclusive,
                begin: Some(Box::new(Expression::Literal(LiteralExpression::Integer(1)))),
                end: Some(Box::new(Expression::Literal(LiteralExpression::Integer(3)))),
            })
        );

        let input = r#"a[..]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Slice(SliceExpression {
                object: Box::new(Expression::Identifier(IdentifierExpression(
                    "a".to_string()
                ))),
                range: BinOp::Range,
                begin: None,
                end: None,
            })
        );

        let input = r#"a[1..]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Slice(SliceExpression {
                object: Box::new(Expression::Identifier(IdentifierExpression(
                    "a".to_string()
                ))),
                range: BinOp::Range,
                begin: Some(Box::new(Expression::Literal(LiteralExpression::Integer(1)))),
                end: None,
            })
        );

        let input = r#"a[..=3]"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Slice(SliceExpression {
                object: Box::new(Expression::Identifier(IdentifierExpression(
                    "a".to_string()
                ))),
                range: BinOp::RangeInclusive,
                begin: None,
                end: Some(Box::new(Expression::Literal(LiteralExpression::Integer(3)))),
            })
        );
    }

    #[test]
    fn test_await_expression() {
        let input = r#"f().await"#;
        let pairs = PestParser::parse(Rule::expression, input).unwrap();
        let expression = parse_expression_pairs(pairs).unwrap();
        assert_eq!(
            expression,
            Expression::Await(Box::new(Expression::Call(CallExpression {
                func: Box::new(Expression::Identifier(IdentifierExpression(
                    "f".to_string()
                ))),
                args: vec![],
            })))
        );
    }

    #[test]
    fn test_try_expression() {
        let input = r#"f()?;"#;
        let statement = parse_top_level(input).unwrap();
        assert_eq!(
            statement,
            TopLevelItem::Statement(Statement::Expression(Expression::Try(Box::new(
                Expression::Call(CallExpression {
                    func: Box::new(Expression::Identifier(IdentifierExpression(
                        "f".to_string()
                    ))),
                    args: vec![],
                })
            ))))
        );
    }
}
