use std::fmt;

use crate::tokenizer::{Identifier, Literal};

#[derive(Debug)]
pub enum Expression {
    Identifier(IdentifierExpression),
    Literal(LiteralExpression),
    Grouped(GroupedExpression),
    UnaryOperation(UnaryOperationExpression),
    BinaryOperation(BinaryOperationExpression),
    Array(ArrayExpression),
    Dictionary(DictionaryExpression),
    Index(IndexExpression),
    Call(CallExpression),
    Field(FieldExpression),
    In(InExpression),
    Matches(MatchesExpression),
    Closure(ClosureExpression),
    Block(BlockExpression),
}

#[derive(Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

impl From<Identifier> for IdentifierExpression {
    fn from(identifier: Identifier) -> Self {
        IdentifierExpression {
            name: identifier.name,
        }
    }
}

#[derive(Debug)]
pub enum LiteralExpression {
    Char(char),
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
}

impl fmt::Display for LiteralExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralExpression::Char(c) => write!(f, "{}", c),
            LiteralExpression::String(s) => write!(f, "{}", s),
            LiteralExpression::Integer(i) => write!(f, "{}", i),
            LiteralExpression::Float(ff) => write!(f, "{}", ff),
            LiteralExpression::Boolean(b) => write!(f, "{}", b),
            LiteralExpression::Null => write!(f, "null"),
        }
    }
}

impl From<Literal> for LiteralExpression {
    fn from(lit: Literal) -> Self {
        match lit {
            Literal::Char(c) => LiteralExpression::Char(c),
            Literal::String(s) => LiteralExpression::String(s),
            Literal::Integer(i) => LiteralExpression::Integer(i),
            Literal::Float(ff) => LiteralExpression::Float(ff),
        }
    }
}

#[derive(Debug)]
pub struct GroupedExpression(pub Box<Expression>);

#[derive(Debug)]
pub enum UnaryOperationExpression {
    Negation(Box<Expression>),
    Not(Box<Expression>),
    Try(Box<Expression>),
}

#[derive(Debug)]
pub struct BinaryOperationExpression {
    pub op: BinaryOperation,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub enum BinaryOperation {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    Power,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    And,
    Or,
    Member,
    In,
    Matches,
    As,
    Is,
}

#[derive(Debug)]
pub struct ArrayExpression {
    pub elements: Vec<Expression>,
}

#[derive(Debug)]
pub struct DictionaryExpression {
    pub elements: Vec<KeyValueExpress>,
}

#[derive(Debug)]
pub struct KeyValueExpress {
    pub key: IdentifierExpression,
    pub value: Box<Expression>,
}

#[derive(Debug)]
pub struct IndexExpression {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
pub struct FieldExpression {
    pub object: Box<Expression>,
    pub field: String,
}

#[derive(Debug)]
pub struct InExpression {
    pub element: Box<Expression>,
    pub object: Box<Expression>,
}

#[derive(Debug)]
pub struct MatchesExpression {
    pub element: Box<Expression>,
    pub object: Box<Expression>,
}

#[derive(Debug)]
pub struct ClosureExpression {
    pub captures: Vec<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug)]
pub struct BlockExpression {
    pub exprs: Vec<Expression>,
}
