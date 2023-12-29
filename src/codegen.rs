use std::collections::BTreeMap;

use crate::ast::*;
use crate::instruction::*;

pub struct FunctionId(usize);

pub struct Module {
    functions: BTreeMap<FunctionId, Function>,
}



pub struct Codegen {
    functions: Vec<Function>,
    instructions: Instructions,
}

impl Codegen {
    // pub fn new() -> Self {
    //     Self {
    //         functions: Vec::new(),
    //     }
    // }

    // pub fn generate(&self, ast: Program) -> String {
    //     let mut code = String::new();

    //     for statement in ast.items {
    //         code += &self.generate_statement(items);
    //     }

    //     code
    // }

    // fn generate_statement(&self, statement: Statement) -> String {
    //     match statement {
    //         Statement::Expression(expression) => self.generate_expression(expression),
    //         _ => unimplemented!("{:?}", statement),
    //     }
    // }

    // pub fn generate_expression(&mut self, expr: Expression) -> String {
    //     match expr {
    //         Expression::Literal(literal) => self.generate_literal(literal),
    //     }
    // }

    // fn generate_literal(&mut self, literal: Literal) {
    //     match literal {
    //         Literal::Integer(value) => format!("{}", value),
    //         Literal::String(value) => format!("\"{}\"", value),
    //     }
    // }
}
