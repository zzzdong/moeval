use core::fmt;
use std::{borrow::Cow, ops::Deref, str::Chars};

use crate::ast::{BinaryOperation, IdentifierExpression, LiteralExpression};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(Identifier),
    Literal(Literal),
    Symbol(Symbol),
    Whitespace,
    EOF,
}

impl Token {
    pub(crate) fn ident(ident: impl ToString) -> Token {
        Token::Identifier(Identifier {
            name: ident.to_string(),
        })
    }
    pub(crate) fn int(i: i64) -> Token {
        Token::Literal(Literal::Integer(i))
    }
    pub(crate) fn float(f: f64) -> Token {
        Token::Literal(Literal::Float(f))
    }
    pub(crate) fn string(s: impl ToString) -> Token {
        Token::Literal(Literal::String(s.to_string()))
    }
    pub(crate) fn symbol(s: &str) -> Token {
        Token::Symbol(Symbol::from_str(s).unwrap())
    }
    pub(crate) fn whitespace(s: &str) -> Token {
        Token::Whitespace
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Keyword(kw) => write!(f, "Keyword({:?})", kw),
            Token::Identifier(ident) => write!(f, "Identifier({})", ident.name),
            Token::Literal(lit) => write!(f, "Literal({:?})", lit),
            Token::Symbol(sym) => write!(f, "Symbol{:?}", sym.as_str()),
            Token::Whitespace => write!(f, "Whitespace"),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    name: String,
}

impl Into<IdentifierExpression> for Identifier {
    fn into(self) -> IdentifierExpression {
        IdentifierExpression { name: self.name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
}

impl Into<LiteralExpression> for Literal {
    fn into(self) -> LiteralExpression {
        match self {
            Literal::Char(c) => LiteralExpression::Char(c),
            Literal::String(s) => LiteralExpression::String(s),
            Literal::Integer(i) => LiteralExpression::Integer(i),
            Literal::Float(f) => LiteralExpression::Float(f),
        }
    }
}

macro_rules! define_symbols {
    (
        $(
            $name:ident => $str:expr,
        )*
    ) => {
        #[derive(Copy, Clone, Debug,  PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Symbol {
            $(#[doc=$str]$name,)*
        }

        impl Symbol {
            pub const ALL: &'static [Symbol] = &[
                $(Symbol::$name,)*
            ];

            pub const STRS: &'static [&'static str] = &[
                $($str,)*
            ];

            pub fn all() -> impl Iterator<Item=Symbol> {
                Self::ALL.iter().copied()
            }

            pub fn from_str(s: &str) -> Result<Self, &'static str> {
                match s {
                    $($str => Ok(Symbol::$name),)*
                    _ => {
                        Err("unkown symbol")
                    }
                }
            }

            pub fn as_str(&self) -> &'static str {
                match self {
                    $(Symbol::$name => $str,)*
                }
            }
        }
    };
}

define_symbols! {
    // Operators
    Plus       => "+",
    Minus      => "-",
    Star       => "*",
    Slash      => "/",
    Percent    => "%",
    Caret      => "^",
    Not        => "!",
    And        => "&",
    Or         => "|",
    AndAnd     => "&&",
    OrOr       => "||",
    LShift     => "<<",
    RShift     => ">>",
    PlusEq     => "+=",
    MinusEq    => "-=",
    StarEq     => "*=",
    SlashEq    => "/=",
    PercentEq  => "%=",
    CaretEq    => "^=",
    AndEq      => "&=",
    OrEq       => "|=",
    ShlEq      => "<<=",
    ShrEq      => ">>=",
    Eq         => "=",
    EqEq       => "==",
    NotEq      => "!=",
    Lt         => "<",
    LtEq        => "<=",
    Gt         => ">",
    GtEq        => ">=",
    At         => "@",
    Dollar     => "$",
    Dot        => ".",
    DotDot     => "..",
    DotDotEq   => "..=",
    // Delimiters
    Comma     => ",",
    Semicolon  => ";",
    Colon      => ":",
    PathSep    => "::",
    RArrow     => "->",
    FatArrow   => "=>",
    Pound      => "#",
    Question   => "?",
    OpenParen     => "(",
    CloseParen     => ")",
    OpenBracket    => "[",
    CloseBracket    => "]",
    OpenBrace   => "{",
    CloseBrace   => "}",
    // SQuotes    => "'",
    // DQuotes    => "\"",
}

impl TryInto<BinaryOperation> for Symbol {
    type Error = TokenError;

    fn try_into(self) -> Result<BinaryOperation, Self::Error> {
        match self {
            Symbol::Plus => Ok(BinaryOperation::Addition),
            Symbol::Minus => Ok(BinaryOperation::Subtraction),
            Symbol::Star => Ok(BinaryOperation::Multiplication),
            Symbol::Slash => Ok(BinaryOperation::Division),
            Symbol::Percent => Ok(BinaryOperation::Modulus),
            Symbol::Caret => Ok(BinaryOperation::Power),
            Symbol::AndAnd => Ok(BinaryOperation::And),
            Symbol::OrOr => Ok(BinaryOperation::Or),
            Symbol::EqEq => Ok(BinaryOperation::Equal),
            Symbol::NotEq => Ok(BinaryOperation::NotEqual),
            Symbol::Lt => Ok(BinaryOperation::LessThan),
            Symbol::Gt => Ok(BinaryOperation::GreaterThan),
            Symbol::LtEq => Ok(BinaryOperation::LessThanOrEqual),
            Symbol::GtEq => Ok(BinaryOperation::GreaterThanOrEqual),
            Symbol::Dot => Ok(BinaryOperation::Access),

            _ => Err(TokenError::new(format!("{:?} not a binary operator", self))),
        }
    }
}

macro_rules! define_keywords {
    (
        $(
            $name:ident => $str:expr,
        )*
    ) => {
        #[derive(Copy, Clone, Debug,  PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Keyword {
            $(#[doc=$str]$name,)*
        }

        impl Keyword {
            pub const ALL: &'static [Keyword] = &[
                $(Keyword::$name,)*
            ];

            pub const STRS: &'static [&'static str] = &[
                $($str,)*
            ];

            pub fn all() -> impl Iterator<Item=Keyword> {
                Self::ALL.iter().copied()
            }

            pub fn from_str(s: &str) -> Result<Self, &'static str> {
                match s {
                    $($str => Ok(Keyword::$name),)*
                    _ => {
                        Err("unkown keyword")
                    }
                }
            }

            pub fn as_str(&self) -> &'static str {
                match self {
                    $(Keyword::$name => $str,)*
                }
            }
        }
    };
}

define_keywords! {
    Int => "int",
    Float => "float",
    String => "string",
    Bool => "bool",
    Null => "null",
    True => "true",
    False => "false",
    In => "in",
    Matches => "matches",
    As => "as",
    Is => "is",
}

impl TryInto<BinaryOperation> for Keyword {
    type Error = TokenError;

    fn try_into(self) -> Result<BinaryOperation, Self::Error> {
        match self {
            Keyword::As => Ok(BinaryOperation::As),
            Keyword::In => Ok(BinaryOperation::In),
            Keyword::Matches => Ok(BinaryOperation::Matches),
            Keyword::Is => Ok(BinaryOperation::Is),
            _ => Err(TokenError::new(format!("{:?} not a binary operator", self))),
        }
    }
}

#[derive(Debug, Clone)]
struct Tokenizer<'i> {
    input: &'i str,
    chars: Chars<'i>,
    pos: usize,
}

impl<'i> Tokenizer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            input,
            chars: input.chars(),
            pos: 0,
        }
    }

    fn next(&mut self) -> Result<Pair<'i>, TokenError> {
        while let Some(c) = self.peek_char() {
            match c {
                '"' => return self.eat_string(),
                c if c.is_whitespace() => return self.eat_whitespace(),
                c if c.is_ascii_digit() => return self.eat_number(),
                c if c.is_ascii_alphabetic() || c == '_' => return self.eat_identifier(),
                c => {
                    if Symbol::STRS.contains(&self.peek_nchar(3)) {
                        let symbol = Symbol::from_str(self.take_nchar(3)).expect("Invalid symbol");
                        return Ok(Pair::new(Token::Symbol(symbol), self.new_span(self.pos)));
                    }
                    if Symbol::STRS.contains(&self.peek_nchar(2)) {
                        let symbol = Symbol::from_str(self.take_nchar(2)).expect("Invalid symbol");
                        return Ok(Pair::new(Token::Symbol(symbol), self.new_span(self.pos)));
                    }
                    if Symbol::STRS.contains(&self.peek_nchar(1)) {
                        let symbol = Symbol::from_str(self.take_nchar(1)).expect("Invalid symbol");
                        return Ok(Pair::new(Token::Symbol(symbol), self.new_span(self.pos)));
                    }

                    return Err(TokenError::new("invalid char"));
                }
            }
        }

        Ok(Pair::new(Token::EOF, self.new_span(self.pos)))
    }

    fn eat_whitespace(&mut self) -> Result<Pair<'i>, TokenError> {
        let start = self.pos;
        let ws = self.eat_while(|c| c.is_whitespace());
        Ok(Pair::new(
            Token::Whitespace,
            Span::new(self.input, start, self.pos),
        ))
    }

    fn eat_number(&mut self) -> Result<Pair<'i>, TokenError> {
        let start = self.pos;

        let _i = self.eat_integer();

        if self.peek_char() == Some('.') {
            self.next_char();
            let f = self.eat_integer();
            if f.is_empty() {
                return Err(TokenError::new("invalid float"));
            }
            let f = self
                .sub_str(start)
                .parse::<f64>()
                .map_err(|err| TokenError::new("invalid float").with_source(err))?;
            return Ok(Pair::new(
                Token::Literal(Literal::Float(f)),
                self.new_span(start),
            ));
        }

        let i = self
            .sub_str(start)
            .parse::<i64>()
            .map_err(|err| TokenError::new("invalid integer").with_source(err))?;

        return Ok(Pair::new(
            Token::Literal(Literal::Integer(i)),
            self.new_span(start),
        ));
    }

    fn eat_integer(&mut self) -> &str {
        self.eat_while(|c| c.is_ascii_digit())
    }

    fn eat_string(&mut self) -> Result<Pair<'i>, TokenError> {
        let start = self.pos;
        self.next_char();
        let s = self.eat_qouted('"')?;
        Ok(Pair::new(
            Token::Literal(Literal::String(s)),
            self.new_span(start),
        ))
    }

    fn eat_qouted(&mut self, qoute: char) -> Result<String, TokenError> {
        let mut ret = String::new();

        while let Some(c) = self.next_char() {
            match c {
                c if c == qoute => {
                    return Ok(ret);
                }
                '\\' => {
                    // unescape
                    match self.next_char() {
                        Some(c) => match c {
                            '"' => ret.push('"'),
                            '\'' => ret.push('\''),
                            '\\' => ret.push('\\'),
                            'r' => ret.push('\r'),
                            'n' => ret.push('\n'),
                            't' => ret.push('\t'),
                            '0' => ret.push('\0'),
                            c if c == qoute => {
                                ret.push(c);
                            }
                            c => {
                                ret.push('\\');
                                ret.push(c);
                            }
                        },
                        None => {
                            return Err(TokenError::new("unexpected end of input"));
                        }
                    }
                }

                c => {
                    ret.push(c);
                }
            }
        }

        Ok(ret)
    }

    fn eat_identifier(&mut self) -> Result<Pair<'i>, TokenError> {
        let start = self.pos;
        let s = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        if let Ok(kw) = Keyword::from_str(s) {
            return Ok(Pair::new(Token::Keyword(kw), self.new_span(start)));
        }

        Ok(Pair::new(
            Token::Identifier(Identifier {
                name: s.to_string(),
            }),
            self.new_span(start),
        ))
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|c| {
            self.pos += c.len_utf8();
            c
        })
    }

    fn peek_nchar(&mut self, n: usize) -> &'i str {
        let start = self.pos;
        let sub = self.chars.clone().take(n).collect::<String>();
        &self.input[start..start + sub.len()]
    }

    fn take_nchar(&mut self, n: usize) -> &'i str {
        let s = self.peek_nchar(n);
        for _ in 0..n {
            self.next_char();
        }
        s
    }

    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.next_char();
        }
    }

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> &'i str {
        let start = self.pos;
        while let Some(c) = self.peek_char() {
            if !predicate(c) {
                break;
            }
            self.next_char();
        }

        &self.input[start..self.pos]
    }

    fn sub_str(&self, start: usize) -> &str {
        &self.input[start..self.pos]
    }

    fn new_span(&self, start: usize) -> Span<'i> {
        Span::new(self.input, start, self.pos)
    }
}

#[derive(Debug)]
pub struct TokenError {
    pub(crate) detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl TokenError {
    pub fn new<D: Into<Cow<'static, str>>>(detail: D) -> TokenError {
        TokenError {
            detail: Some(detail.into()),
            source: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}

#[derive(Debug, Clone)]
pub struct Pair<'i> {
    pub(crate) token: Token,
    pub(crate) span: Span<'i>,
}

impl<'i> Pair<'i> {
    pub fn new(token: Token, span: Span<'i>) -> Self {
        Self { token, span }
    }
}

impl<'i> Deref for Pair<'i> {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

#[derive(Clone)]
pub struct Span<'i> {
    input: &'i str,
    start: usize,
    end: usize,
}

impl<'i> Span<'i> {
    pub fn new(input: &'i str, start: usize, end: usize) -> Self {
        Self { input, start, end }
    }
}

impl fmt::Debug for Span<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Span")
            .field(&&self.input[self.start..self.end])
            .finish()
    }
}

impl<'i> IntoIterator for Tokenizer<'i> {
    type Item = Result<Pair<'i>, TokenError>;
    type IntoIter = Pairs<'i>;

    fn into_iter(self) -> Self::IntoIter {
        Pairs { inner: self }
    }
}

#[derive(Debug, Clone)]
pub struct Pairs<'i> {
    inner: Tokenizer<'i>,
}

impl<'i> Pairs<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            inner: Tokenizer::new(input),
        }
    }

    pub fn as_tokens(&self) -> Tokens {
        Tokens {
            inner: self.inner.clone(),
        }
    }
}

impl<'i> Iterator for Pairs<'i> {
    type Item = Result<Pair<'i>, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Ok(pair) => {
                if pair.token == Token::EOF {
                    None
                } else if pair.token == Token::Whitespace {
                    self.next()
                } else {
                    Some(Ok(pair))
                }
            }
            Err(err) => Some(Err(err)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tokens<'i> {
    inner: Tokenizer<'i>,
}

impl<'i> Tokens<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            inner: Tokenizer::new(input),
        }
    }
}

impl<'i> Iterator for Tokens<'i> {
    type Item = Result<Token, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Ok(pair) => {
                if pair.token == Token::EOF {
                    None
                } else if pair.token == Token::Whitespace {
                    self.next()
                } else {
                    Some(Ok(pair.token))
                }
            }
            Err(err) => Some(Err(err)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let inputs: Vec<(&str, Vec<Token>)> = vec![
            (
                "a b\tc",
                vec![
                    Token::ident("a"),
                    Token::whitespace(" "),
                    Token::ident("b"),
                    Token::whitespace(" "),
                    Token::ident("c"),
                ],
            ),
            (
                "[hello,123,123.4]",
                vec![
                    Token::symbol("["),
                    Token::ident("hello"),
                    Token::symbol(","),
                    Token::int(123),
                    Token::symbol(","),
                    Token::float(123.4),
                    Token::symbol("]"),
                ],
            ),
            (
                r#"a=b+c*123;"#,
                vec![
                    Token::ident("a"),
                    Token::symbol("="),
                    Token::ident("b"),
                    Token::symbol("+"),
                    Token::ident("c"),
                    Token::symbol("*"),
                    Token::int(123),
                    Token::symbol(";"),
                ],
            ),
        ];

        for i in &inputs {
            let mut tokenizer = Tokenizer::new(i.0);

            let mut ret = Vec::new();

            println!("-> {}", i.0);
            loop {
                let pair = tokenizer.next().unwrap();
                println!("pair: {pair:?}");
                if pair.token == Token::EOF {
                    break;
                }

                ret.push(pair.token);
            }

            assert_eq!(ret, i.1);
        }
    }
}
