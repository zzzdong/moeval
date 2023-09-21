use std::borrow::Cow;

use log::debug;

use crate::ast::*;
use crate::tokenizer::*;

#[derive(Debug)]
pub enum ParseError {
    /// The parser had an error (recoverable)
    Error(Cow<'static, str>),
    /// The parser had an unrecoverable error
    Failure(Cow<'static, str>),
}

impl ParseError {
    pub(crate) fn failure<D: Into<Cow<'static, str>>>(detail: D) -> Self {
        ParseError::Failure(detail.into())
    }

    pub(crate) fn syntax(expected: impl std::fmt::Display, found: impl std::fmt::Debug) -> Self {
        ParseError::Error(Cow::Owned(format!(
            "expected {expected}, but found `{found:?}`",
        )))
    }

    pub(crate) fn unexpect(expected: impl std::fmt::Display, found: &Pair<'_>) -> Self {
        let span = &found.span;
        let tok = &found.token;

        ParseError::Error(Cow::Owned(format!(
            "expected {expected}, but found `{tok:?}` @ {span:?}",
        )))
    }

    pub(crate) fn unexpect_kind(expected: impl std::fmt::Display, found: &Token) -> Self {
        ParseError::Error(Cow::Owned(format!(
            "expected {expected}, but found `{found:?}`"
        )))
    }

    pub(crate) fn eof() -> Self {
        ParseError::Failure(Cow::Borrowed("unexpect EOF"))
    }
}

impl From<TokenError> for ParseError {
    fn from(e: TokenError) -> Self {
        ParseError::Failure(e.detail.unwrap())
    }
}

pub struct Parser<'i> {
    pairs: Pairs<'i>,
}

impl<'i> Parser<'i> {
    pub fn parse(input: &'i str) -> Result<Expression, ParseError> {
        let pairs = Pairs::new(input);
        let mut parser = Parser { pairs };
        parser.parse_expr()
    }

    /// reference: https://github.com/sqlparser-rs/sqlparser-rs/blob/main/src/parser.rs
    /// reference: https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing
    fn parse_expr(&mut self) -> Result<Expression, ParseError> {
        self.parse_subexpr(Precedence::Lowest)
    }

    fn parse_subexpr(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        debug!("parsing expr");

        let mut expr = self.parse_prefix()?;
        debug!("prefix: {expr:?}");

        loop {
            let next_precedence = self.next_precedence()?;
            debug!("next precedence: {next_precedence:?}");

            if precedence >= next_precedence {
                break;
            }

            expr = self.parse_infix(expr, next_precedence)?;
        }

        Ok(expr)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParseError> {
        match self.peek_token()?.as_deref() {
            Some(Token::Symbol(Symbol::Minus)) => {
                Ok(Expression::UnaryOperation(UnaryOperationExpression {
                    op: UnaryOperation::Negation,
                    expr: Box::new(self.parse_subexpr(Precedence::Prefix)?),
                }))
            }
            Some(Token::Symbol(Symbol::Not)) => {
                Ok(Expression::UnaryOperation(UnaryOperationExpression {
                    op: UnaryOperation::Not,
                    expr: Box::new(self.parse_subexpr(Precedence::Prefix)?),
                }))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_infix(
        &mut self,
        expr: Expression,
        precedence: Precedence,
    ) -> Result<Expression, ParseError> {
        let tok = self.consume_token()?;

        debug!("parse_infix, first token {tok:?}");

        match *tok {
            Token::Symbol(sym) => match sym {
                Symbol::OpenParen => {
                    let args: Vec<Expression> = self.separated_list0(
                        Symbol::Comma,
                        Parser::parse_expr,
                        Symbol::CloseParen,
                    )?;
                    self.expect_token(Token::Symbol(Symbol::CloseParen))?;
                    Ok(Expression::Call(CallExpression {
                        callee: Box::new(expr),
                        arguments: args,
                    }))
                }
                Symbol::OpenBracket => {
                    let index = self.parse_expr()?;
                    self.expect_token(Token::Symbol(Symbol::CloseBracket))?;
                    Ok(Expression::Index(IndexExpression {
                        object: Box::new(expr),
                        index: Box::new(index),
                    }))
                }
                Symbol::Question => Ok(Expression::UnaryOperation(UnaryOperationExpression {
                    op: UnaryOperation::Try,
                    expr: Box::new(expr),
                })),
                _ => {
                    if let Ok(op) = sym.try_into() {
                        Ok(Expression::BinaryOperation(BinaryOperationExpression {
                            op,
                            left: Box::new(expr),
                            right: Box::new(self.parse_subexpr(precedence)?),
                        }))
                    } else {
                        unreachable!("unknown symbol in infix expression: {:?}", sym);
                    }
                }
            },
            Token::Keyword(kw) => {
                if let Ok(op) = kw.try_into() {
                    Ok(Expression::BinaryOperation(BinaryOperationExpression {
                        op,
                        left: Box::new(expr),
                        right: Box::new(self.parse_subexpr(precedence)?),
                    }))
                } else {
                    unreachable!("unknown keyword in infix expression: {:?}", kw);
                }
            }
            _ => {
                unreachable!()
            }
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        let tok = self.peek_token()?.ok_or(ParseError::eof())?;

        match tok.token {
            Token::Identifier(ident) => {
                self.consume_token()?;
                Ok(Expression::Identifier(ident.into()))
            }
            Token::Literal(lit) => {
                self.consume_token()?;
                Ok(Expression::Literal(lit.into()))
            }
            Token::Symbol(Symbol::OpenParen) => {
                self.consume_token()?;
                let expr = self.parse_subexpr(Precedence::Lowest)?;
                self.expect_token(Token::Symbol(Symbol::CloseParen))?;
                Ok(Expression::Grouped(GroupedExpression(Box::new(expr))))
            }
            Token::Symbol(Symbol::OpenBracket) => {
                self.consume_token()?;
                let items =
                    self.separated_list0(Symbol::Comma, Parser::parse_expr, Symbol::CloseBracket)?;
                self.expect_token(Token::Symbol(Symbol::CloseBracket))?;
                Ok(Expression::Array(ArrayExpression { elements: items }))
            }
            Token::Symbol(Symbol::OpenBrace) => {
                self.consume_token()?;
                let items = self.separated_list0(
                    Symbol::Comma,
                    Parser::parse_key_value,
                    Symbol::CloseBrace,
                )?;
                self.expect_token(Token::Symbol(Symbol::CloseBrace))?;
                Ok(Expression::Dictionary(DictionaryExpression {
                    elements: items,
                }))
            }
            Token::Symbol(Symbol::Or) => self.parse_closure().map(Expression::Closure),

            _ => Err(ParseError::unexpect("primary", &tok)),
        }
    }

    // eg |a,b|a+b
    fn parse_closure(&mut self) -> Result<ClosureExpression, ParseError> {
        self.consume_token()?;
        let captures = self.separated_list0(Symbol::Comma, Parser::parse_expr, Symbol::Or)?;
        self.expect_token(Token::Symbol(Symbol::Or))?;

        let body = if self.test_next(&Token::Symbol(Symbol::OpenBrace)) {
            self.parse_block().map(Expression::Block)?
        } else {
            self.parse_expr()?
        };

        Ok(ClosureExpression {
            captures,
            body: Box::new(body),
        })
    }

    fn parse_block(&mut self) -> Result<BlockExpression, ParseError> {
        self.consume_token()?;
        let exprs = self.separated_list0(Symbol::Semicolon, Parser::parse_expr, Symbol::Or)?;
        self.expect_token(Token::Symbol(Symbol::CloseBrace))?;

        Ok(BlockExpression { exprs })
    }

    fn parse_key_value(&mut self) -> Result<KeyValueExpress, ParseError> {
        let key = self.parse_identifier()?;
        self.expect_token(Token::Symbol(Symbol::Colon))?;
        let value = self.parse_expr()?;

        Ok(KeyValueExpress {
            key,
            value: Box::new(value),
        })
    }

    fn parse_identifier(&mut self) -> Result<IdentifierExpression, ParseError> {
        let token = self.consume_token()?;

        if let Token::Identifier(ident) = token.token {
            Ok(ident.into())
        } else {
            Err(ParseError::unexpect("Identifier", &token))
        }
    }

    fn next_precedence(&mut self) -> Result<Precedence, ParseError> {
        let tok = self.peek_token()?;

        debug!("next_precedence() {tok:?}");

        let p = match tok.as_deref() {
            Some(Token::Symbol(sym)) => match sym {
                Symbol::Plus | Symbol::Minus => Precedence::Term,
                Symbol::Star | Symbol::Slash | Symbol::Percent => Precedence::Factor,
                Symbol::OpenParen | Symbol::OpenBracket => Precedence::Call,
                Symbol::Dot => Precedence::Call,
                Symbol::Eq => Precedence::Equal,
                Symbol::Gt | Symbol::GtEq | Symbol::Lt | Symbol::LtEq | Symbol::EqEq => {
                    Precedence::Compare
                }
                Symbol::Question => Precedence::Postfix,
                Symbol::PathSep => Precedence::Path,
                Symbol::OrOr => Precedence::LogicOr,
                Symbol::AndAnd => Precedence::LogicAnd,
                Symbol::And => Precedence::Assign,
                Symbol::EqTidle => Precedence::Judge,
                _ => {
                    debug!("Precedence::Lowest for sym {:?}", sym);
                    Precedence::Lowest
                }
            },
            Some(Token::Keyword(Keyword::In)) => Precedence::Judge,
            _ => Precedence::Lowest,
        };

        Ok(p)
    }

    fn separated_list<T, F>(&mut self, sep: Symbol, f: F) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser) -> Result<T, ParseError>,
    {
        let mut values = Vec::new();
        loop {
            values.push(f(self)?);

            if !self.next_token(&Token::Symbol(sep)) {
                break;
            }
        }
        Ok(values)
    }

    fn separated_list0<T, F>(
        &mut self,
        sep: Symbol,
        f: F,
        terminated: Symbol,
    ) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser<'i>) -> Result<T, ParseError>,
    {
        let mut values = Vec::new();
        loop {
            let peek = self.peek_token()?.map(|t| t.token);
            if peek == Some(Token::Symbol(terminated)) {
                break;
            }
            values.push(f(self)?);
            if !self.next_token(&Token::Symbol(sep)) {
                break;
            }
        }
        Ok(values)
    }

    // /// Peek and test next token
    // fn test_next(&mut self, expected: &Token) -> bool {
    //     match self.peek_token() {
    //         Ok(tok) => &tok. == expected,
    //         _ => false,
    //     }
    // }

    // /// Check next token, consume it if ok
    // fn try_next<T, F>(&mut self, f: F) -> Option<T>
    // where
    //     F: Fn(Token) -> Option<T>,
    // {
    //     match self.peek_token() {
    //         Ok(tok) => f(tok.kind).map(|t| {
    //             self.consume_token().unwrap();
    //             t
    //         }),
    //         _ => None,
    //     }
    // }

    /// Consume and return the next token
    fn consume_token(&mut self) -> Result<Pair<'_>, ParseError> {
        match self.pairs.next() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(err)) => Err(err.into()),
            None => Err(ParseError::eof()),
        }
    }

    /// Consume next token, and check it with pattern
    fn expect_token(&mut self, kind: Token) -> Result<Token, ParseError> {
        let tok = self.consume_token()?;
        if tok.token == kind {
            Ok(tok.token)
        } else {
            Err(ParseError::unexpect(kind, &tok))
        }
    }

    /// Peek next token without cunsume it
    fn peek_token(&self) -> Result<Option<Pair>, ParseError> {
        match self.pairs.clone().next() {
            Some(Ok(pair)) => Ok(Some(pair)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(None),
        }
    }

    /// Consume the next token if it matches the expected token, otherwise return false
    #[must_use]
    fn next_token(&mut self, expected: &Token) -> bool {
        match self.peek_token() {
            Ok(Some(tok)) if &tok.token == expected => {
                self.consume_token().unwrap();
                true
            }
            _ => false,
        }
    }

    /// Check the next token if it matches the expected token, otherwise return false
    #[must_use]
    fn test_next(&mut self, expected: &Token) -> bool {
        matches!(self.peek_token(), Ok(Some(tok)) if &tok.token == expected)
    }

    fn is_eof(&self) -> bool {
        self.pairs.clone().next().is_none()
    }
}

/// https://doc.rust-lang.org/reference/expressions.html#expression-precedence
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest = 0,
    Assign,
    Range,
    LogicOr,
    LogicAnd,
    Equal,
    Compare,
    BitOr,
    BitXor,
    BitAnd,
    BitShift,
    Term,
    Factor,
    Judge,
    As,
    Prefix,
    Postfix,
    Call,
    Path,
}

#[cfg(test)]
mod test {
    use log::info;

    use super::*;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_parse_expr() {
        init();

        info!("test_parse_expr");

        let expr = Parser::parse("a + b * c - d").unwrap();
        println!("{:?}", expr);

        let expr =
            Parser::parse(r#"user.Group in ["admin", "moderator"] || user.Id == comment.UserId"#)
                .unwrap();
        println!("{:?}", expr);

        let expr = Parser::parse(r#"all(Tweets, |t|{len(t) <= 240})"#).unwrap();
        println!("{:?}", expr);

        let expr = Parser::parse(r#"hello in {hello: "world"}"#).unwrap();
        println!("{:?}", expr);

        let expr = Parser::parse(r#"hello in req.headers"#).unwrap();
        println!("{:?}", expr);
    }
}
