use crate::{codegen::Codegen, error::CompileError, ir::builder::Module};

pub struct Compiler {}

impl Compiler {
    pub fn compile(input: &str) -> Result<Module, CompileError> {
        // 解析输入
        let ast = crate::parser::parse_file(input)?;
        // println!("{:#?}", ast);
        // // 语义分析
        // let ast = crate::semantics::analyze(ast)?;

        // 生成代码
        let module = Codegen::compile(ast);

        Ok(module)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_compiler() {
        let inputs = [
            "1 + 2 * 3 - 4 - a * b / c == 0;",
            "a.b.c.d(1, 1) + b(c, d);",
            "a.c = b.c(1,1);",
            "all(Tweets, |x|{return x.Len <= 240;});",
            r#"
let cc = 100;
fn fib(n: int) -> int {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

let k = fib(10);
let a = 1;
let b = 2;
if a > 0 {
    a -= 1;
} else {
    a += 1;
}

let c = b + a;
c += cc;

k + cc;
"#,
        ];

        for input in inputs.iter() {
            // println!("{}", input);
            let module = Compiler::compile(input).unwrap();
            println!("============");
            println!("{module}")
        }
    }
}
