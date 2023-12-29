use crate::parser::ParseError;



pub enum CompileError {
    Parse(ParseError),
    Semantics(String),
}

impl From<ParseError> for CompileError {
    fn from(error: ParseError) -> Self {
        CompileError::Parse(error)
    }
}

pub struct Compiler {
    
}

impl Compiler {
    pub fn compile(&self, input: &str) -> Result<(), CompileError> {
        // 解析输入
        let ast = crate::parser::parse_expression_input(input)?;
        
        // // 语义分析
        // let ast = crate::semantics::analyze(ast)?;
        
        // 生成代码
        // let code = crate::codegen::generate_code(ast);
        
        // 输出代码
        // println!("{}", code);
        
        Ok(())
    }
}