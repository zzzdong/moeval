use crate::ast::{BinaryOperation, BinaryOperationExpression, Expression, LiteralExpression};

pub struct ExprRewriter {}

impl ExprRewriter {
    pub fn new() -> Self {
        ExprRewriter {}
    }

    pub fn rewrite(&self, expr: Expression) -> Expression {
        match self.simplify_expr(&expr) {
            Some(expr) => expr,
            None => expr,
        }
    }

    fn simplify_expr(&self, expr: &Expression) -> Option<Expression> {
        match expr {
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                if let (
                    Expression::Literal(LiteralExpression::Integer(lhs)),
                    Expression::Literal(LiteralExpression::Integer(rhs)),
                ) = (left.as_ref(), right.as_ref())
                {
                    match op {
                        BinaryOperation::Addition => {
                            Some(Expression::Literal(LiteralExpression::Integer(lhs + rhs)))
                        }
                        BinaryOperation::Subtraction => {
                            Some(Expression::Literal(LiteralExpression::Integer(lhs - rhs)))
                        }
                        BinaryOperation::Multiplication => {
                            Some(Expression::Literal(LiteralExpression::Integer(lhs * rhs)))
                        }
                        BinaryOperation::Division => {
                            Some(Expression::Literal(LiteralExpression::Integer(lhs / rhs)))
                        }
                        BinaryOperation::Modulus => {
                            Some(Expression::Literal(LiteralExpression::Integer(lhs % rhs)))
                        }
                        _ => None,
                    }
                } else {
                    let lhs = self.simplify_expr(left);
                    let rhs = self.simplify_expr(right);
                    if lhs.is_some() && rhs.is_some() {
                        Some(Expression::BinaryOperation(BinaryOperationExpression {
                            op: *op,
                            left: Box::new(lhs.unwrap()),
                            right: Box::new(rhs.unwrap()),
                        }))
                    } else {
                        None
                    }
                }
            }
            _ => None,
        }
    }
}
