use std::sync::Arc;

use crate::parser::ParseError;

#[derive(Debug)]
pub enum Error {
    Parse(ParseError),
    OpUnimplemented,
    OpIllegalOperate,
    InvalidArgument,
    UndefinedVariable(Arc<String>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Parse(e) => write!(f, "Parser error: {:?}", e),
            Error::OpUnimplemented => write!(f, "Unimplemented operator"),
            Error::OpIllegalOperate => write!(f, "Illegal operator"),
            Error::InvalidArgument => write!(f, "Invalid argument"),
            Error::UndefinedVariable(v) => write!(f, "Undefined variable: {}", v),
        }
    }
}

impl std::error::Error for Error {}


impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parse(e)
    }
}