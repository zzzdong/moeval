use crate::{parser::ParseError, vm::RuntimeError};


#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Parse(ParseError),
    Runtime(RuntimeError),

}