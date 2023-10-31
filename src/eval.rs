use std::{collections::HashMap, sync::Arc};

use crate::{ast::*, instruction::Opcode, vm::Ops, Environment, Error, Value};

pub struct Eval<'a> {
    env: &'a Environment,
    values: HashMap<String, Value>,
}

impl<'a> Eval<'a> {
    pub fn new(env: &'a Environment) -> Self {
        Self {
            env,
            values: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expr: Expression) -> Result<Value, Error> {
        match expr {
            Expression::Literal(lit) => {
                let v = match lit {
                    LiteralExpression::Null => Value::Null,
                    LiteralExpression::Undefined => Value::Undefined,
                    LiteralExpression::Boolean(b) => Value::Bool(b),
                    LiteralExpression::Integer(n) => Value::Integer(n),
                    LiteralExpression::Float(f) => Value::Float(f),
                    LiteralExpression::Char(c) => Value::Char(c),
                    LiteralExpression::String(s) => Value::String(Arc::new(s)),
                };

                Ok(v)
            }
            Expression::Identifier(IdentifierExpression { name }) => {
                Ok(self.values.get(&name).cloned().unwrap_or(Value::Undefined))
            }
            Expression::Variable(VariableExpression { name }) => {
                Ok(self.env.get(&name).unwrap_or(Value::Undefined))
            }
            Expression::Grouped(GroupedExpression(expr)) => self.eval(*expr),

            Expression::UnaryOperation(UnaryOperationExpression { op, expr }) => {
                let value = self.eval(*expr)?;
                match op {
                    UnaryOperation::Negation => value.neg(),
                    UnaryOperation::Not => value.not(),
                    UnaryOperation::Try => unimplemented!(),
                }
            }
            Expression::BinaryOperation(expr) => self.eval_binary_expr(expr),
            Expression::Index(IndexExpression { object, index }) => {
                let object = self.eval(*object)?;
                let index = self.eval(*index)?;
                object.index(index)
            }
            Expression::Array(ArrayExpression { elements }) => {
                let mut array = Vec::new();
                for e in elements {
                    array.push(self.eval(e)?);
                }
                Ok(Value::Array(array))
            }
            Expression::Dictionary(DictionaryExpression { elements }) => {
                let mut map = HashMap::new();
                for kv in elements {
                    map.insert(Arc::new(kv.key.to_string()), self.eval(*kv.value)?);
                }
                Ok(Value::Dictionary(map))
            }
            Expression::Call(expr) => self.eval_call_expr(expr),
            _ => {
                unimplemented!()
            }
        }
    }

    fn eval_binary_expr(&mut self, expr: BinaryOperationExpression) -> Result<Value, Error> {
        let lhs = self.eval(*expr.left)?;

        match (expr.op, *expr.right) {
            (BinaryOperation::Dot, Expression::Identifier(IdentifierExpression { name })) => {
                self.load_member(lhs, &name)
            }
            (op, rhs) => {
                let rhs = self.eval(rhs)?;
                match op {
                    BinaryOperation::Addition => lhs.add(rhs),
                    BinaryOperation::Subtraction => lhs.sub(rhs),
                    BinaryOperation::Multiplication => lhs.mul(rhs),
                    BinaryOperation::Division => lhs.div(rhs),
                    BinaryOperation::Modulus => lhs.mod_(rhs),
                    BinaryOperation::Power => lhs.pow(rhs),
                    BinaryOperation::Equal => lhs.eq(rhs),
                    BinaryOperation::NotEqual => lhs.ne(rhs),
                    BinaryOperation::GreaterThan => lhs.gt(rhs),
                    BinaryOperation::GreaterThanOrEqual => lhs.gte(rhs),
                    BinaryOperation::LessThan => lhs.lt(rhs),
                    BinaryOperation::LessThanOrEqual => lhs.lte(rhs),
                    BinaryOperation::And => lhs.and(rhs),
                    BinaryOperation::Or => lhs.or(rhs),
                    BinaryOperation::In => lhs.in_(rhs),
                    BinaryOperation::Matches => {
                        match (lhs, rhs) {
                            (Value::String(lhs), Value::String(rhs)) => {
                                // TODO: add regex match
                                unimplemented!()
                            }
                            _ => Err(Error::OpIllegalOperate),
                        }
                    }
                    _ => {
                        unimplemented!()
                    }
                }
            }
        }
    }

    fn eval_call_expr(&mut self, expr: CallExpression) -> Result<Value, Error> {
        let mut args = Vec::new();
        for arg in expr.arguments {
            args.push(self.eval(arg)?);
        }

        match *expr.func {
            Expression::BinaryOperation(BinaryOperationExpression { op, left, right }) => {
                match (op, *right) {
                    (
                        BinaryOperation::Dot,
                        Expression::Identifier(IdentifierExpression { name }),
                    ) => {
                        let func = self.eval(*left)?;
                        self.call_method(func, &name, args)
                    }
                    (op, right) => {
                        let func =
                            self.eval(Expression::BinaryOperation(BinaryOperationExpression {
                                op,
                                left,
                                right: Box::new(right),
                            }))?;
                        self.call_function(func, args)
                            .map(|v| v.unwrap_or(Value::Undefined))
                    }
                }
            }
            _ => {
                let func = self.eval(*expr.func)?;
                self.call_function(func, args)
                    .map(|v| v.unwrap_or(Value::Undefined))
            }
        }
    }

    fn load_member(&mut self, value: Value, name: &str) -> Result<Value, Error> {
        match value {
            Value::Dictionary(dict) => Ok(dict
                .get(&Arc::new(name.to_string()))
                .cloned()
                .unwrap_or(Value::Undefined)),
            Value::Dynamic(obj) => obj.get_field(name).map(|v| v.unwrap_or(Value::Undefined)),
            _ => Ok(Value::Undefined),
        }
    }

    fn call_method(&mut self, value: Value, name: &str, args: Vec<Value>) -> Result<Value, Error> {
        match value {
            Value::Dictionary(dict) => Ok(dict
                .get(&Arc::new(name.to_string()))
                .cloned()
                .unwrap_or(Value::Undefined)),
            Value::Dynamic(obj) => obj
                .call_method(name, &args)
                .map(|v| v.unwrap_or(Value::Undefined)),
            _ => Ok(Value::Undefined),
        }
    }

    fn call_function(&mut self, value: Value, args: Vec<Value>) -> Result<Option<Value>, Error> {
        match value {
            Value::Function(func) => func.call(&args),
            _ => Ok(None),
        }
    }

    // fn call_constructor(&mut self, value: Value, args: Vec<Value>) -> Result<Option<Value>, Error> {
    //     match value {
    //         Value::Function(func) => func.construct(&args),
    //         _ => Ok(Value::Undefined),
    //     }
    // }
}
