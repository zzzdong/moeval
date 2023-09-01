#[derive(Debug)]
pub enum Expression {
    EnvVariable(EnvVariableExpression),
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
pub struct EnvVariableExpression {
    pub name: String,
}

#[derive(Debug)]
pub struct IdentifierExpression {
    pub name: String,
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
    Access,
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
