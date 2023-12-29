use std::{
    fmt, io,
    io::{Error, ErrorKind},
    str::FromStr,
};

#[derive(Debug)]
pub struct Program {
    pub items: Vec<TopLevelItem>,
}

impl Program {
    pub fn new() -> Program {
        return Program { items: Vec::new() };
    }
}

#[derive(Debug)]
pub enum TopLevelItem {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    Empty,
    Break,
    Continue,
    Item(ItemStatement),
    Expression(Expression),
    Let(LetStatement),
    For(ForStatement),
    Loop(LoopStatement),
    If(IfStatement),
    Return(ReturnStatement),
    Call(String, Vec<Expression>),
}

#[derive(Debug)]
pub enum ItemStatement {
    Enum(EnumItem),
    Struct(StructItem),
    Fn(FunctionItem),
}

#[derive(Debug)]
pub struct EnumItem {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub enum EnumVariant {
    Simple(String),
    Tuple(String, Vec<TypeExpression>),
}

#[derive(Debug)]
pub struct StructItem {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub name: String,
    pub ty: TypeExpression,
}

#[derive(Debug)]
pub struct FunctionItem {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_ty: Option<TypeExpression>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct FunctionParam {
    pub name: String,
    pub ty: Option<TypeExpression>,
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: String,
    pub ty: Option<TypeExpression>,
    pub value: Option<Expression>,
}

#[derive(Debug)]
pub struct ForStatement {
    pub pat: Pattern,
    pub iterable: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct LoopStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Vec<Statement>,
    pub else_branch: Option<Vec<Statement>>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
}

#[derive(Debug)]
pub enum TypeExpression {
    Boolean,
    Byte,
    Integer,
    Float,
    Char,
    String,
    Array(Box<TypeExpression>),
    Tuple(Vec<TypeExpression>),
    Generic(String, Vec<TypeExpression>),
    UserDefined(String),
    Impl(Box<TypeExpression>),
}

#[derive(Debug)]
pub enum Expression {
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Prefix(PrefixOp, Box<Expression>),
    Literal(LiteralExpression),
    Identifier(IdentifierExpression),
    Tuple(TupleExpression),
    Array(ArrayExpression),
    Call(CallExpression),
    Index(IndexExpression),
    Slice(SliceExpression),
    Try(Box<Expression>),
}

#[derive(Debug)]
pub struct CallExpression {
    pub target: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug)]
pub struct IndexExpression {
    pub target: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug)]
pub struct SliceExpression {
    pub target: Box<Expression>,
    pub begin: Option<Box<Expression>>,
    pub end: Option<Box<Expression>>,
}

#[derive(Debug)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    Literal(LiteralExpression),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogicAnd,
    LogicOr,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    As,
    Range,
    Dot,
    Path,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::LogicAnd => write!(f, "&&"),
            BinOp::LogicOr => write!(f, "||"),
            BinOp::Less => write!(f, "<"),
            BinOp::LessEqual => write!(f, "<="),
            BinOp::Greater => write!(f, ">"),
            BinOp::GreaterEqual => write!(f, ">="),
            BinOp::Equal => write!(f, "=="),
            BinOp::NotEqual => write!(f, "!="),
            BinOp::As => write!(f, "as"),
            BinOp::Range => write!(f, "..="),
            BinOp::Dot => write!(f, "."),
            BinOp::Path => write!(f, "::"),
            BinOp::Assign => write!(f, "="),
            BinOp::AddAssign => write!(f, "+="),
            BinOp::SubAssign => write!(f, "-="),
            BinOp::MulAssign => write!(f, "*="),
            BinOp::DivAssign => write!(f, "/="),
            BinOp::ModAssign => write!(f, "%="),
        }
    }
}

impl FromStr for BinOp {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(BinOp::Add),
            "-" => Ok(BinOp::Sub),
            "*" => Ok(BinOp::Mul),
            "/" => Ok(BinOp::Div),
            "%" => Ok(BinOp::Mod),
            "&&" => Ok(BinOp::LogicAnd),
            "||" => Ok(BinOp::LogicOr),
            "<" => Ok(BinOp::Less),
            "<=" => Ok(BinOp::LessEqual),
            ">" => Ok(BinOp::Greater),
            ">=" => Ok(BinOp::GreaterEqual),
            "==" => Ok(BinOp::Equal),
            "!=" => Ok(BinOp::NotEqual),
            "as" => Ok(BinOp::As),
            "..=" => Ok(BinOp::Range),
            "." => Ok(BinOp::Dot),
            "=" => Ok(BinOp::Assign),
            "+=" => Ok(BinOp::AddAssign),
            "-=" => Ok(BinOp::SubAssign),
            "*=" => Ok(BinOp::MulAssign),
            "/=" => Ok(BinOp::DivAssign),
            "%=" => Ok(BinOp::ModAssign),
            "::" => Ok(BinOp::Path),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Invalid binanry op: {}", s),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl FromStr for PrefixOp {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-" => Ok(PrefixOp::Neg),
            "!" => Ok(PrefixOp::Not),
            _ => Err(Error::new(
                ErrorKind::InvalidInput,
                format!("Invalid unary op: {}", s),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct IdentifierExpression(pub String);

#[derive(Debug, PartialEq)]
pub enum LiteralExpression {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Debug)]
pub struct TupleExpression(pub Vec<Expression>);

#[derive(Debug)]
pub struct ArrayExpression(pub Vec<Expression>);
