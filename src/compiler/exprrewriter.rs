use crate::ast::{BinaryOperation, BinaryOperationExpression, Expression, LiteralExpression};

pub struct ExprRewriter {}

impl ExprRewriter {
    pub fn new() -> Self {
        ExprRewriter {}
    }

    pub fn rewrite(&self, expr: Expression) -> Expression {
        self.simplify_expr(expr)
    }

    fn simplify_expr(&self, expr: Expression) -> Expression {
        match expr {
            Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
                if let (
                    Expression::Literal(LiteralExpression::Integer(lhs)),
                    Expression::Literal(LiteralExpression::Integer(rhs)),
                ) = (left.as_ref(), right.as_ref())
                {
                    match op {
                        BinaryOperation::Addition => {
                            Expression::Literal(LiteralExpression::Integer(lhs + rhs))
                        }
                        BinaryOperation::Subtraction => {
                            Expression::Literal(LiteralExpression::Integer(lhs - rhs))
                        }
                        BinaryOperation::Multiplication => {
                            Expression::Literal(LiteralExpression::Integer(lhs * rhs))
                        }
                        BinaryOperation::Division => {
                            Expression::Literal(LiteralExpression::Integer(lhs / rhs))
                        }
                        BinaryOperation::Modulus => {
                            Expression::Literal(LiteralExpression::Integer(lhs % rhs))
                        }
                        _ => Expression::BinaryOperation(BinaryOperationExpression {
                            op,
                            left,
                            right,
                        }),
                    }
                } else {
                    let expr = Expression::BinaryOperation(BinaryOperationExpression {
                        op,
                        left: Box::new(self.simplify_expr(*left)),
                        right: Box::new(self.simplify_expr(*right)),
                    });
                    self.simplify_expr(expr)
                }
            }
            _ => expr,
        }

        // match expr {
        //     Expression::BinaryOperation(BinaryOperationExpression { left, op, right }) => {
        //         match (left.as_ref(), right.as_ref()) {
        //             (
        //                 Expression::Literal(LiteralExpression::Integer(lhs)),
        //                 Expression::Literal(LiteralExpression::Integer(rhs)),
        //             ) => match op {
        //                 BinaryOperation::Addition => {
        //                     Expression::Literal(LiteralExpression::Integer(lhs + rhs))
        //                 }
        //                 BinaryOperation::Subtraction => {
        //                     Expression::Literal(LiteralExpression::Integer(lhs - rhs))
        //                 }
        //                 BinaryOperation::Multiplication => {
        //                     Expression::Literal(LiteralExpression::Integer(lhs * rhs))
        //                 }
        //                 BinaryOperation::Division => {
        //                     Expression::Literal(LiteralExpression::Integer(lhs / rhs))
        //                 }
        //                 BinaryOperation::Modulus => {
        //                     Expression::Literal(LiteralExpression::Integer(lhs % rhs))
        //                 }
        //                 BinaryOperation::Equal => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs == rhs))
        //                 }
        //                 BinaryOperation::NotEqual => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs != rhs))
        //                 }
        //                 BinaryOperation::GreaterThan => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs > rhs))
        //                 }
        //                 BinaryOperation::GreaterThanOrEqual => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs >= rhs))
        //                 }
        //                 BinaryOperation::LessThan => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs < rhs))
        //                 }
        //                 BinaryOperation::LessThanOrEqual => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs <= rhs))
        //                 }
        //                 _ => Expression::BinaryOperation(BinaryOperationExpression {
        //                     op,
        //                     left: Box::new(*left),
        //                     right: Box::new(*right),
        //                 }),
        //             },
        //             (
        //                 Expression::Literal(LiteralExpression::Float(lhs)),
        //                 Expression::Literal(LiteralExpression::Float(rhs)),
        //             ) => match op {
        //                 BinaryOperation::Addition => {
        //                     Expression::Literal(LiteralExpression::Float(lhs + rhs))
        //                 }
        //                 BinaryOperation::Subtraction => {
        //                     Expression::Literal(LiteralExpression::Float(lhs - rhs))
        //                 }
        //                 BinaryOperation::Multiplication => {
        //                     Expression::Literal(LiteralExpression::Float(lhs * rhs))
        //                 }
        //                 BinaryOperation::Division => {
        //                     Expression::Literal(LiteralExpression::Float(lhs / rhs))
        //                 }
        //                 BinaryOperation::Modulus => {
        //                     Expression::Literal(LiteralExpression::Float(lhs % rhs))
        //                 }
        //                 BinaryOperation::Equal => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs == rhs))
        //                 }
        //                 BinaryOperation::NotEqual => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs != rhs))
        //                 }
        //                 BinaryOperation::GreaterThan => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs > rhs))
        //                 }
        //                 BinaryOperation::GreaterThanOrEqual => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs >= rhs))
        //                 }
        //                 BinaryOperation::LessThan => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs < rhs))
        //                 }
        //                 BinaryOperation::LessThanOrEqual => {
        //                     Expression::Literal(LiteralExpression::Boolean(lhs <= rhs))
        //                 }
        //                 _ => Expression::BinaryOperation(BinaryOperationExpression {
        //                     op,
        //                     left: Box::new(*left),
        //                     right: Box::new(*right),
        //                 }),
        //             },
        //             _ => {

        //                 Expression::BinaryOperation(BinaryOperationExpression {
        //                 op,
        //                 left: Box::new(self.simplify_expr(*left)),
        //                 right: Box::new(self.simplify_expr(*right)),
        //             })

        //         }
        //     }
        //     _ => {
        //         return expr;
        //     }
        // }
    }
}
