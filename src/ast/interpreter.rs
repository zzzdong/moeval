use std::collections::HashMap;


use crate::{
    vm::{Enumerator, Environment, Primitive, Range},
    Error, Null, Object, RuntimeError, Value, ValueRef,
};

use super::{ast::*, parse_file};

struct Variables {
    scopes: Vec<HashMap<String, ValueRef>>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn define(&mut self, name: impl ToString, value: ValueRef) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), value);
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<ValueRef> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name.as_ref()) {
                return Some(value.clone());
            }
        }

        None
    }

    pub fn set(&mut self, name: impl AsRef<str>, value: ValueRef) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.as_ref().to_string(), value);
    }
}

#[derive(Debug, Clone)]
enum ControlFlow {
    Next,
    Break,
    Continue,
    Return(Option<ValueRef>),
}

pub struct Interpreter {
    variables: Variables,
    return_value: Option<ValueRef>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: Variables::new(),
            return_value: None,
        }
    }

    pub fn eval(&mut self, script: &str) -> Result<(), Error> {
        let program = parse_file(script)?;

        self.eval_program(program)
    }

    pub fn eval_program(&mut self, program: Program) -> Result<(), Error> {
        let mut stmts = Vec::new();
        let mut function_map = HashMap::new();

        // split program into statements and items
        for stmt in program.stmts {
            match stmt {
                Statement::Item(ItemStatement::Fn(func)) => {
                    function_map.insert(func.name.clone(), func);
                }
                Statement::Item(_) => {
                    unimplemented!("unsupported item statement")
                }
                Statement::Empty => {}
                _ => {
                    stmts.push(stmt);
                }
            }
        }

        self.eval_statements(&stmts)?;

        Ok(())
    }

    fn eval_statements(&mut self, stmts: &[Statement]) -> Result<ControlFlow, Error> {
        self.variables.scopes.push(HashMap::new());

        for stmt in stmts {
            match self.eval_statement(stmt)? {
                ControlFlow::Next => {}
                control => return Ok(control),
            }
        }

        Ok(ControlFlow::Next)
    }

    fn eval_statement(&mut self, stmt: &Statement) -> Result<ControlFlow, Error> {
        match stmt {
            Statement::Let(LetStatement { name, ty, value }) => {
                match value {
                    Some(value) => {
                        let value = self.eval_expression(value)?;
                        self.variables.define(name.clone(), value);
                    }
                    None => {
                        self.variables
                            .define(name.clone(), ValueRef::new(Null.into()));
                    }
                }

                Ok(ControlFlow::Next)
            }
            Statement::Return(ReturnStatement { value }) => match value {
                Some(value) => {
                    let value = self.eval_expression(value)?;
                    return Ok(ControlFlow::Return(Some(value)));
                }
                None => return Ok(ControlFlow::Return(None)),
            },
            Statement::If(IfStatement {
                condition,
                then_branch,
                else_branch,
            }) => {
                let condition = self.eval_expression(condition)?;
                if condition == true {
                    return self.eval_statements(then_branch);
                } else if let Some(else_branch) = else_branch {
                    return self.eval_statements(else_branch);
                }

                Ok(ControlFlow::Next)
            }
            Statement::For(ForStatement {
                pat,
                iterable,
                body,
            }) => {
                let iterable = self.eval_expression(iterable)?;
                let mut iterable = Enumerator::new(iterable.get().make_iterator()?);

                loop {
                    if iterable.iterator_has_next()? == false {
                        break;
                    }

                    let value = iterable.iterate_next()?;
                    match pat {
                        Pattern::Identifier(ident) => {
                            self.variables.define(ident.clone(), value);
                        }
                        Pattern::Wildcard => {}
                        _ => unimplemented!("unsupported pattern: {:?}", pat),
                    };

                    match self.eval_statements(body)? {
                        ControlFlow::Break => {
                            break;
                        }
                        ControlFlow::Continue => {
                            continue;
                        }
                        ControlFlow::Next => {}
                        ControlFlow::Return(val) => return Ok(ControlFlow::Return(val)),
                    }
                }

                Ok(ControlFlow::Next)
            }
            Statement::Break => {
                return Ok(ControlFlow::Break);
            }
            Statement::Continue => {
                return Ok(ControlFlow::Continue);
            }
            Statement::Expression(expr) => {
                self.eval_expression(expr)?;
                Ok(ControlFlow::Next)
            }
            _ => unimplemented!("unsupported statement: {:?}", stmt),
        }
    }

    fn eval_expression(&mut self, expr: &Expression) -> Result<ValueRef, Error> {
        match expr {
            Expression::Literal(lit) => {
                return Ok(self.eval_literal(lit));
            }
            Expression::Identifier(ref ident) => self
                .variables
                .get(ident.0.as_str())
                .ok_or_else(|| RuntimeError::symbol_not_found(ident.0.as_str()).into()),

            Expression::Binary(ref op, left, right) => self.eval_binary_expression(op, left, right),
            Expression::Assign(AssignExpression { object, value, op }) => {
                let value = self.eval_expression(value)?;
                match object.as_ref() {
                    Expression::Identifier(ident) => match op {
                        Some(op) => {
                            let old_value = self.variables.get(ident.0.as_str()).unwrap();
                            match op {
                                BinOp::Add => {
                                    let new_value = old_value.get().add(&value.get()).unwrap();
                                    self.variables.set(ident.0.as_str(), new_value.into());
                                    Ok(ValueRef::new(Null.into()))
                                }
                                BinOp::Sub => {
                                    let new_value = old_value.get().sub(&value.get()).unwrap();
                                    self.variables.set(ident.0.as_str(), new_value.into());
                                    Ok(ValueRef::new(Null.into()))
                                }
                                BinOp::Mul => {
                                    let new_value = old_value.get().mul(&value.get()).unwrap();
                                    self.variables.set(ident.0.as_str(), new_value.into());
                                    Ok(ValueRef::new(Null.into()))
                                }
                                BinOp::Div => {
                                    let new_value = old_value.get().div(&value.get()).unwrap();
                                    self.variables.set(ident.0.as_str(), new_value.into());
                                    Ok(ValueRef::new(Null.into()))
                                }
                                BinOp::Mod => {
                                    let new_value = old_value.get().modulo(&value.get()).unwrap();
                                    self.variables.set(ident.0.as_str(), new_value.into());
                                    Ok(ValueRef::new(Null.into()))
                                }
                                _ => unimplemented!("unsupported binary operator: {:?}", op),
                            }
                        }
                        None => {
                            self.variables.set(ident.0.as_str(), value);
                            Ok(ValueRef::new(Null.into()))
                        }
                    },
                    _ => unimplemented!("unsupported assignment expression: {:?}", object),
                }
            }
            _ => unimplemented!("unsupported expression: {:?}", expr),
        }
    }

    fn eval_literal(&self, lit: &LiteralExpression) -> ValueRef {
        match lit {
            LiteralExpression::Boolean(b) => {
                ValueRef::new(Value::from_primitive(Primitive::Boolean(*b)))
            }
            LiteralExpression::Char(c) => ValueRef::new(Value::from_primitive(Primitive::Char(*c))),
            LiteralExpression::Integer(i) => {
                ValueRef::new(Value::from_primitive(Primitive::Integer(*i)))
            }
            LiteralExpression::Float(f) => {
                ValueRef::new(Value::from_primitive(Primitive::Float(*f)))
            }
            LiteralExpression::String(s) => {
                ValueRef::new(Value::from_primitive(Primitive::String(s.clone())))
            }
        }
    }

    fn eval_binary_expression(
        &mut self,
        op: &BinOp,
        left_expr: &Expression,
        right_expr: &Expression,
    ) -> Result<ValueRef, Error> {
        let right = self.eval_expression(right_expr)?;

        match op {
            BinOp::Assign => {
                if let Expression::Identifier(ident) = left_expr {
                    if ident.0 == "_" {
                        return Ok(Value::new(Null).into());
                    }
                    self.variables.set(ident.0.as_str(), right);
                    Ok(Value::new(Null).into())
                } else {
                    unimplemented!("unsupported assign expression")
                }
            }
            BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::DivAssign
            | BinOp::ModAssign => {
                if let Expression::Identifier(ident) = left_expr {
                    let value = self.eval_expression(left_expr)?;

                    let result = match op {
                        BinOp::AddAssign => value.get().add(right.get())?,
                        BinOp::SubAssign => value.get().sub(right.get())?,
                        BinOp::MulAssign => value.get().mul(right.get())?,
                        BinOp::DivAssign => value.get().div(right.get())?,
                        BinOp::ModAssign => value.get().modulo(right.get())?,
                        _ => unimplemented!("unsupported assign expression"),
                    };

                    self.variables.set(ident.0.as_str(), ValueRef::new(result));
                    Ok(Value::new(Null).into())
                } else {
                    unimplemented!("unsupported assign expression")
                }
            }
            _ => {
                let left = self.eval_expression(left_expr)?;

                self.eval_binop(op, &left, &right)
            }
        }
    }

    fn eval_binop(&self, op: &BinOp, left: &ValueRef, right: &ValueRef) -> Result<ValueRef, Error> {
        let value = match op {
            BinOp::Add => left.get().add(right.get())?,
            BinOp::Sub => left.get().sub(right.get())?,
            BinOp::Mul => left.get().mul(right.get())?,
            BinOp::Div => left.get().div(right.get())?,
            BinOp::Mod => left.get().modulo(right.get())?,
            BinOp::LogicAnd => left.get().logic_and(right.get())?,
            BinOp::LogicOr => left.get().logic_or(right.get())?,
            BinOp::Equal => Value::new(left.get().compare(right.get())?.is_eq()),
            BinOp::NotEqual => Value::new(left.get().compare(right.get())?.is_ne()),
            BinOp::Greater => Value::new(left.get().compare(right.get())?.is_gt()),
            BinOp::GreaterEqual => Value::new(left.get().compare(right.get())?.is_ge()),
            BinOp::Less => Value::new(left.get().compare(right.get())?.is_lt()),
            BinOp::LessEqual => Value::new(left.get().compare(right.get())?.is_le()),
            BinOp::Range => {
                let range = Range::new(left.clone(), right.clone(), false)?;
                Value::new(range)
            }
            BinOp::RangeInclusive => {
                let range = Range::new(left.clone(), right.clone(), true)?;
                Value::new(range)
            }
            _ => unimplemented!("unsupported binary operator: {:?}", op),
        };
        Ok(value.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_literal() {
        let interp = Interpreter::new();

        // 测试布尔值字面量
        let lit = LiteralExpression::Boolean(true);
        let value = interp.eval_literal(&lit);
        assert_eq!(value, true);

        // 测试整数字面量
        let lit = LiteralExpression::Integer(42);
        let value = interp.eval_literal(&lit);
        assert_eq!(value, 42);

        // 测试浮点数字面量
        let lit = LiteralExpression::Float(3.14);
        let value = interp.eval_literal(&lit);
        assert_eq!(value, 3.14);

        // 测试字符字面量
        let lit = LiteralExpression::Char('a');
        let value = interp.eval_literal(&lit);
        assert_eq!(value, 'a');

        // 测试字符串字面量
        let lit = LiteralExpression::String("hello".to_string());
        let value = interp.eval_literal(&lit);
        assert_eq!(value, "hello");
    }

    #[test]
    fn test_eval_binary_expression() {
        let mut interp = Interpreter::new();

        // 测试加法运算符
        let expr = Expression::Binary(
            BinOp::Add,
            Box::new(Expression::Literal(LiteralExpression::Integer(1))),
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, 3);

        // 测试减法运算符
        let expr = Expression::Binary(
            BinOp::Sub,
            Box::new(Expression::Literal(LiteralExpression::Integer(5))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, 2);

        // 测试乘法运算符
        let expr = Expression::Binary(
            BinOp::Mul,
            Box::new(Expression::Literal(LiteralExpression::Integer(4))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, 12);

        // 测试除法运算符
        let expr = Expression::Binary(
            BinOp::Div,
            Box::new(Expression::Literal(LiteralExpression::Integer(10))),
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, 5);

        // 测试取模运算符
        let expr = Expression::Binary(
            BinOp::Mod,
            Box::new(Expression::Literal(LiteralExpression::Integer(10))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, 1);

        // 测试逻辑与运算符
        let expr = Expression::Binary(
            BinOp::LogicAnd,
            Box::new(Expression::Literal(LiteralExpression::Boolean(true))),
            Box::new(Expression::Literal(LiteralExpression::Boolean(false))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, false);

        // 测试逻辑或运算符
        let expr = Expression::Binary(
            BinOp::LogicOr,
            Box::new(Expression::Literal(LiteralExpression::Boolean(true))),
            Box::new(Expression::Literal(LiteralExpression::Boolean(false))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, true);

        // 测试等于运算符
        let expr = Expression::Binary(
            BinOp::Equal,
            Box::new(Expression::Literal(LiteralExpression::Integer(1))),
            Box::new(Expression::Literal(LiteralExpression::Integer(1))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, true);

        // 测试不等于运算符
        let expr = Expression::Binary(
            BinOp::NotEqual,
            Box::new(Expression::Literal(LiteralExpression::Integer(1))),
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, true);

        // 测试大于运算符
        let expr = Expression::Binary(
            BinOp::Greater,
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, true);

        // 测试大于等于运算符
        let expr = Expression::Binary(
            BinOp::GreaterEqual,
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, true);

        // 测试小于运算符
        let expr = Expression::Binary(
            BinOp::Less,
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, true);

        // 测试小于等于运算符
        let expr = Expression::Binary(
            BinOp::LessEqual,
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, true);

        // 测试赋值运算符
        let expr = Expression::Binary(
            BinOp::Assign,
            Box::new(Expression::Identifier(IdentifierExpression(
                "a".to_string(),
            ))),
            Box::new(Expression::Literal(LiteralExpression::Integer(5))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, Null);
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 5);

        // 测试加法赋值运算符
        let expr = Expression::Binary(
            BinOp::AddAssign,
            Box::new(Expression::Identifier(IdentifierExpression(
                "a".to_string(),
            ))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, Null);
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 8);

        // 测试减法赋值运算符
        let expr = Expression::Binary(
            BinOp::SubAssign,
            Box::new(Expression::Identifier(IdentifierExpression(
                "a".to_string(),
            ))),
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, Null);
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 6);

        // 测试乘法赋值运算符
        let expr = Expression::Binary(
            BinOp::MulAssign,
            Box::new(Expression::Identifier(IdentifierExpression(
                "a".to_string(),
            ))),
            Box::new(Expression::Literal(LiteralExpression::Integer(2))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, Null);
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 12);

        // 测试除法赋值运算符
        let expr = Expression::Binary(
            BinOp::DivAssign,
            Box::new(Expression::Identifier(IdentifierExpression(
                "a".to_string(),
            ))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, Null);
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 4);

        // 测试取模赋值运算符
        let expr = Expression::Binary(
            BinOp::ModAssign,
            Box::new(Expression::Identifier(IdentifierExpression(
                "a".to_string(),
            ))),
            Box::new(Expression::Literal(LiteralExpression::Integer(3))),
        );
        let value = interp.eval_expression(&expr).unwrap();
        assert_eq!(value, Null);
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 1);
    }

    // 添加对 if 语句的测试用例
    #[test]
    fn test_eval_if_statement() {
        let mut interp = Interpreter::new();

        // 测试简单的 if 语句
        let script = r#"
            let a = 1;
            if a == 1 {
                a = 2;
            }
        "#;
        interp.eval(script).unwrap();
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 2);

        // 测试 if-else 语句
        let script = r#"
            let a = 1;
            if a == 2 {
                a = 2;
            } else {
                a = 3;
            }
        "#;
        interp.eval(script).unwrap();
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 3);
    }

    // 添加对 for 循环的测试用例
    #[test]
    fn test_eval_for_loop() {
        let mut interp = Interpreter::new();

        // 测试简单的 for 循环
        let script = r#"
            let a = 0;
            for i in 0..3 {
                a = a + 1;
            }
        "#;
        interp.eval(script).unwrap();
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 3);
    }

    #[test]
    fn test_eval_for_loop_with_break() {
        let mut interp = Interpreter::new();

        // 测试 for 循环中带有 break 语句
        let script = r#"
            let a = 0;
            for i in 0..5 {
                if i == 3 {
                    break;
                }
                a = a + 1;
            }
        "#;
        interp.eval(script).unwrap();
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 3);
    }

    #[test]
    fn test_eval_for_loop_with_continue() {
        let mut interp = Interpreter::new();

        // 测试 for 循环中带有 continue 语句
        let script = r#"
            let a = 0;
            for i in 0..5 {
                if i % 2 == 0 {
                    continue;
                }
                a = a + 1;
            }
        "#;
        interp.eval(script).unwrap();
        let var_value = interp.variables.get("a").unwrap();
        assert_eq!(var_value, 2);
    }
}
