use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use crate::{
    ast::{
        BinaryOperation, BinaryOperationExpression, Expression, IdentifierExpression,
        LiteralExpression,
    },
    value::Value,
};

pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    Ret,
}

// #[derive(Debug, Clone)]
// pub struct IRConst(Value);

// impl fmt::Display for IRConst {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

pub struct Compiler {}

impl Compiler {
    // pub fn compile(expr: Expression) -> Vec<OpCode> {

    // }
}

// pub fn compile(expr: Expression) -> Vec<OpCode> {
//     let mut opcods = vec![];

//     match expr {
//         Expression::Identifier(IdentifierExpression { name }) => opcods.push(OpCode::Push),
//         Expression::BinaryOperation(BinaryOperationExpression { op, left, right }) => {
//             let left_opcods = compile(*left);
//             let right_opcods = compile(*right);

//             opcods.extend(left_opcods);
//             opcods.extend(right_opcods);

//             match op {
//                 // TODO: implement more operators
//                 "+" => {
//                     opcods.push(OpCode::Add);
//                 }
//             }
//         }
//         _ => {
//             unreachable!();
//         }
//     };

//     opcods
// }
