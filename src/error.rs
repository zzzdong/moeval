use crate::{object::OperateKind, parser::ParseError};

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Compile(CompileError),
    Runtime(RuntimeError),
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::Io(error)
    }
}

impl From<CompileError> for Error {
    fn from(error: CompileError) -> Self {
        Error::Compile(error)
    }
}

impl From<RuntimeError> for Error {
    fn from(error: RuntimeError) -> Self {
        Error::Runtime(error)
    }
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    Semantics(String),
}

impl From<ParseError> for CompileError {
    fn from(error: ParseError) -> Self {
        CompileError::Parse(error)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Parse(error) => write!(f, "Parse error: {}", error),
            CompileError::Semantics(message) => write!(f, "Semantics error: {}", message),
        }
    }
}

impl std::error::Error for CompileError {}

#[derive(Debug)]
pub enum RuntimeError {
    InvalidOperation { kind: OperateKind, message: String },
    InvalidType { expected: &'static str, got: String },
}

impl RuntimeError {
    pub fn invalid_operation(kind: OperateKind, message: impl Into<String>) -> Self {
        RuntimeError::InvalidOperation {
            kind,
            message: message.into(),
        }
    }

    pub fn invalid_type<T: std::any::Any>(got: impl std::fmt::Debug) -> Self {
        RuntimeError::InvalidType {
            expected: std::any::type_name::<T>(),
            got: format!("{:?}", got),
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::InvalidOperation { kind, message } => {
                write!(f, "Invalid `{}` operation: {}", kind, message)
            }
            RuntimeError::InvalidType { expected, got } => {
                write!(f, "Invalid type: expected type `{}`, got `{}`", expected, got)
            }
        }
    }
}

impl std::error::Error for RuntimeError {}
