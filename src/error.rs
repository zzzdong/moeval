use crate::{opcode::Opcode, parser::ParseError};

#[derive(Debug)]
pub enum Error {
    Parse(ParseError),
    OpUnimplemented(Opcode),
    OpIllegalOperate,
    InvalidArgument,
    UndefinedVariable(String),
    IndexOutOfBounds(usize, usize),
    EntryNotFound(String),
    Message(String),
    RegexError(regex::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Parse(e) => write!(f, "Parser error: {:?}", e),
            Error::OpUnimplemented(opcode) => write!(f, "Unimplemented operator: {:?}", opcode),
            Error::OpIllegalOperate => write!(f, "Illegal operator"),
            Error::InvalidArgument => write!(f, "Invalid argument"),
            Error::UndefinedVariable(v) => write!(f, "Undefined variable: {}", v),
            Error::IndexOutOfBounds(i, l) => write!(f, "Index out of bounds: {} of {}", i, l),
            Error::EntryNotFound(v) => write!(f, "Entry not found: {}", v),
            Error::Message(m) => write!(f, "{}", m),
            Error::RegexError(e) => write!(f, "Regex error: {:?}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parse(e)
    }
}
