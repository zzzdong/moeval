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
    InvalidOperation {
        kind: OperateKind,
        message: String,
    },
    InvalidType {
        expected: &'static str,
        got: String,
    },
    InvalidArgumentCount {
        expected: usize,
        got: usize,
    },
    InvalidArgument {
        index: usize,
        expected: &'static str,
        cause: String,
    },
    SymbolNotFound {
        name: String,
    },
    Overflow,
    IndexOutOfBounds {
        index: usize,
        length: usize,
    },
    KeyNotFound {
        key: String,
    },
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

    pub fn invalid_argument_count(expected: usize, got: usize) -> Self {
        RuntimeError::InvalidArgumentCount { expected, got }
    }

    pub fn invalid_argument<T: std::any::Any>(index: usize, got: impl std::fmt::Debug) -> Self {
        RuntimeError::InvalidArgument {
            index,
            expected: std::any::type_name::<T>(),
            cause: format!("{:?}", got),
        }
    }

    pub fn symbol_not_found(name: impl ToString) -> Self {
        RuntimeError::SymbolNotFound {
            name: name.to_string(),
        }
    }

    pub fn index_out_of_bound(index: usize, length: usize) -> Self {
        RuntimeError::IndexOutOfBounds { index, length }
    }

    pub fn key_not_found(key: impl std::fmt::Debug) -> Self {
        RuntimeError::KeyNotFound {
            key: format!("{key:?}"),
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
                write!(
                    f,
                    "Invalid type: expected type `{}`, got `{}`",
                    expected, got
                )
            }
            RuntimeError::InvalidArgumentCount { expected, got } => {
                write!(
                    f,
                    "Invalid argument count: expected `{}`, got `{}`",
                    expected, got
                )
            }
            RuntimeError::InvalidArgument {
                index,
                expected,
                cause,
            } => {
                write!(
                    f,
                    "Invalid argument[{}] type: expected type `{}`, cause `{}`",
                    index, expected, cause
                )
            }
            RuntimeError::SymbolNotFound { name } => {
                write!(f, "Symbol `{}` not found", name)
            }
            RuntimeError::IndexOutOfBounds { index, length } => {
                write!(
                    f,
                    "Index out of bounds: index `{}`, length `{}`",
                    index, length
                )
            }
            RuntimeError::KeyNotFound { key } => {
                write!(f, "Key `{}` not found", key)
            }
            RuntimeError::Overflow => write!(f, "Overflow"),
        }
    }
}

impl std::error::Error for RuntimeError {}
