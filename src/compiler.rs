use crate::parser::ParseError;

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

pub struct Compiler {}

impl Compiler {
    pub fn compile(input: &str) -> Result<(), CompileError> {
        // 解析输入
        let ast = crate::parser::parse_file(input)?;
        println!("{:#?}", ast);
        // // 语义分析
        // let ast = crate::semantics::analyze(ast)?;

        // 生成代码
        let mut builder = crate::irbuilder::IRBuilder::new();
        let retval = builder.build(ast);
        builder.debug_instructions();

        // 输出代码
        // println!("{}", code);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_compiler() {
        Compiler::compile("1 + 2 * 3 - 4 - a * b / c == 0").unwrap();

        Compiler::compile("a.b.c.d(1, 1) + b(c, d)").unwrap();

        Compiler::compile("a.c = b.c(1,1)").unwrap();

        Compiler::compile("all(Tweets, |x|{return x.Len <= 240;})").unwrap();
    }
}
