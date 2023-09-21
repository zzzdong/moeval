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
    Matches(MatchesExpression),
    Closure(ClosureExpression),
    Block(BlockExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(identifier) => write!(f, "{}", identifier),
            Expression::Literal(literal) => write!(f, "{}", literal),
            Expression::Grouped(grouped) => write!(f, "{}", grouped),
            Expression::UnaryOperation(unary) => write!(f, "{}", unary),
            Expression::BinaryOperation(binary) => write!(f, "{}", binary),
            Expression::Array(array) => write!(f, "{}", array),
            Expression::Dictionary(dictionary) => write!(f, "{}", dictionary),
            Expression::Index(index) => write!(f, "{}", index),
            Expression::Call(call) => write!(f, "{}", call),
            Expression::Field(field) => write!(f, "{}", field),
            Expression::Matches(matches) => write!(f, "{}", matches),
            Expression::Closure(closure) => write!(f, "{}", closure),
            Expression::Block(block) => write!(f, "{}", block),
        }
    }
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

impl fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
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

impl fmt::Display for GroupedExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.0)
    }
}

#[derive(Debug)]
pub struct UnaryOperationExpression {
    pub op: UnaryOperation,
    pub expr: Box<Expression>,
}

impl fmt::Display for UnaryOperationExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.op {
            UnaryOperation::Negation => write!(f, "-{}", self.expr),
            UnaryOperation::Not => write!(f, "!{}", self.expr),
            UnaryOperation::Try => write!(f, "{}?", self.expr),
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub enum UnaryOperation {
    Negation,
    Not,
    Try,
}

impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperation::Negation => write!(f, "-"),
            UnaryOperation::Not => write!(f, "!"),
            UnaryOperation::Try => write!(f, "?"),
        }
    }
}

#[derive(Debug)]
pub struct BinaryOperationExpression {
    pub op: BinaryOperation,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl fmt::Display for BinaryOperationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.op, self.right)
    }
}

#[derive(Debug, Clone, Copy)]
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
    Assign,
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOperation::Addition => write!(f, "+"),
            BinaryOperation::Subtraction => write!(f, "-"),
            BinaryOperation::Multiplication => write!(f, "*"),
            BinaryOperation::Division => write!(f, "/"),
            BinaryOperation::Modulus => write!(f, "%"),
            BinaryOperation::Power => write!(f, "^"),
            BinaryOperation::Equal => write!(f, "=="),
            BinaryOperation::NotEqual => write!(f, "!="),
            BinaryOperation::GreaterThan => write!(f, ">"),
            BinaryOperation::GreaterThanOrEqual => write!(f, ">="),
            BinaryOperation::LessThan => write!(f, "<"),
            BinaryOperation::LessThanOrEqual => write!(f, "<="),
            BinaryOperation::And => write!(f, "&&"),
            BinaryOperation::Or => write!(f, "||"),
            BinaryOperation::Member => write!(f, "."),
            BinaryOperation::Matches => write!(f, "=~"),
            BinaryOperation::In => write!(f, "in"),
            BinaryOperation::As => write!(f, "as"),
            BinaryOperation::Is => write!(f, "is"),
            BinaryOperation::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug)]
pub struct ArrayExpression {
    pub elements: Vec<Expression>,
}

impl fmt::Display for ArrayExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        if let Some((last, elements)) = self.elements.split_last() {
            for item in elements {
                write!(f, "{},", item)?;
            }
            write!(f, "{}", last)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug)]
pub struct DictionaryExpression {
    pub elements: Vec<KeyValueExpress>,
}

impl fmt::Display for DictionaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        if let Some((last, elements)) = self.elements.split_last() {
            for item in elements {
                write!(f, "{}: {},", item.key, item.value)?;
            }
            write!(f, "{}: {}", last.key, last.value)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct KeyValueExpress {
    pub key: IdentifierExpression,
    pub value: Box<Expression>,
}

impl fmt::Display for KeyValueExpress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

#[derive(Debug)]
pub struct IndexExpression {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.object, self.index)
    }
}

#[derive(Debug)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.callee)?;
        for arg in &self.arguments {
            write!(f, "{},", arg)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct FieldExpression {
    pub object: Box<Expression>,
    pub field: String,
}

impl fmt::Display for FieldExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.object, self.field)
    }
}

#[derive(Debug)]
pub struct InExpression {
    pub element: Box<Expression>,
    pub object: Box<Expression>,
}

impl fmt::Display for InExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} in {}", self.element, self.object)
    }
}

#[derive(Debug)]
pub struct MatchesExpression {
    pub element: Box<Expression>,
    pub object: Box<Expression>,
}

impl fmt::Display for MatchesExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} =~ {}", self.element, self.object)
    }
}

#[derive(Debug)]
pub struct ClosureExpression {
    pub captures: Vec<Expression>,
    pub body: Box<Expression>,
}

impl fmt::Display for ClosureExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "|")?;
        for cap in &self.captures {
            write!(f, "{}, ", cap)?;
        }
        write!(f, "| {{ {} }}", self.body)
    }
}

#[derive(Debug)]
pub struct BlockExpression {
    pub exprs: Vec<Expression>,
}

impl fmt::Display for BlockExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{ ")?;
        for expr in &self.exprs {
            write!(f, "{};", expr)?;
        }
        write!(f, "}}")
    }
}
