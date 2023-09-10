use crate::ast::BinaryOperation;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    LoadStatic,
    LoadVar,
    LoadMember,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    Not,
    Push,
    Pop,
    In,
    Matches,
    IfEqual,
    IfNotEqual,
    IfLess,
    IfLessOrEqual,
    IfGreater,
    IfGreaterOrEqual,
    Call,
    Return,
}

impl TryFrom<BinaryOperation> for OpCode {
    type Error = ();

    fn try_from(op: BinaryOperation) -> Result<Self, Self::Error> {
        Ok(match op {
            BinaryOperation::Addition => OpCode::Add,
            BinaryOperation::Subtraction => OpCode::Sub,
            BinaryOperation::Multiplication => OpCode::Mul,
            BinaryOperation::Division => OpCode::Div,
            BinaryOperation::Modulus => OpCode::Mod,
            BinaryOperation::Power => OpCode::Pow,
            BinaryOperation::And => OpCode::And,
            BinaryOperation::Or => OpCode::Or,
            BinaryOperation::Member => OpCode::LoadMember,
            BinaryOperation::In => OpCode::In,
            BinaryOperation::Matches => OpCode::Matches,
            BinaryOperation::Equal => OpCode::IfEqual,
            BinaryOperation::NotEqual => OpCode::IfNotEqual,
            BinaryOperation::GreaterThan => OpCode::IfGreater,
            BinaryOperation::GreaterThanOrEqual => OpCode::IfGreaterOrEqual,
            BinaryOperation::LessThan => OpCode::IfLess,
            BinaryOperation::LessThanOrEqual => OpCode::IfLessOrEqual,
            _ => {
                return Err(());
            }
        })
    }
}
