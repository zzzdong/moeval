use core::fmt;
use std::{
    borrow::Cow,
    ops::Deref,
    str::{CharIndices, Chars},
};

use crate::ast::{BinaryOperation, LiteralExpression};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(Identifier),
    Variable(Variable),
    Literal(Literal),
    Symbol(Symbol),
    Whitespace,
    Eof,
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
    pub(crate) fn whitespace(_s: &str) -> Token {
        Token::Whitespace
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Keyword(kw) => write!(f, "Keyword({:?})", kw),
            Token::Identifier(ident) => write!(f, "Identifier({})", ident.name),
            Token::Variable(var) => write!(f, "Variable({:?})", var),
            Token::Literal(lit) => write!(f, "Literal({:?})", lit),
            Token::Symbol(sym) => write!(f, "Symbol{:?}", sym.as_str()),
            Token::Whitespace => write!(f, "Whitespace"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub(crate) name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    pub(crate) name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
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
    LtEq       => "<=",
    Gt         => ">",
    GtEq       => ">=",
    EqTidle    => "=~",
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
    Undefined => "undefined",
    True => "true",
    False => "false",
    In => "in",
    As => "as",
    Is => "is",
}

impl TryInto<BinaryOperation> for Keyword {
    type Error = TokenError;

    fn try_into(self) -> Result<BinaryOperation, Self::Error> {
        match self {
            Keyword::As => Ok(BinaryOperation::As),
            Keyword::In => Ok(BinaryOperation::In),
            Keyword::Is => Ok(BinaryOperation::Is),
            _ => Err(TokenError::new(format!("{:?} not a binary operator", self))),
        }
    }
}

impl TryInto<LiteralExpression> for Keyword {
    type Error = TokenError;

    fn try_into(self) -> Result<LiteralExpression, Self::Error> {
        match self {
            Keyword::True => Ok(LiteralExpression::Boolean(true)),
            Keyword::False => Ok(LiteralExpression::Boolean(false)),
            Keyword::Null => Ok(LiteralExpression::Null),
            Keyword::Undefined => Ok(LiteralExpression::Undefined),
            _ => Err(TokenError::new(format!(
                "{:?} not a literal expression",
                self
            ))),
        }
    }
}

#[derive(Debug, Clone)]
struct Lexer<'i> {
    input: &'i str,
    chars: Chars<'i>,
    loc: LineColumn,
}

// reference:
impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            input,
            chars: input.chars(),
            loc: LineColumn { line: 1, column: 1 },
        }
    }

    fn next(&mut self) -> Result<LocatedToken, TokenError> {
        if let Some(c) = self.peek_char() {
            match c {
                '"' => return self.eat_string(),
                '$' => return self.eat_variable(),
                c if c.is_whitespace() => return self.eat_whitespace(),
                c if c.is_ascii_digit() => return self.eat_number(),
                c if c.is_ascii_alphabetic() || c == '_' => return self.eat_identifier(),
                c => {
                    let start = self.loc;
                    if self.has_at_least(3) && Symbol::STRS.contains(&self.peek_nchar(3).as_str()) {
                        let symbol =
                            Symbol::from_str(self.take_nchar(3).as_str()).expect("Invalid symbol");
                        return Ok(self.new_token(Token::Symbol(symbol), start));
                    }
                    if self.has_at_least(2) && Symbol::STRS.contains(&self.peek_nchar(2).as_str()) {
                        let symbol =
                            Symbol::from_str(self.take_nchar(2).as_str()).expect("Invalid symbol");
                        return Ok(self.new_token(Token::Symbol(symbol), start));
                    }
                    if self.has_at_least(1) && Symbol::STRS.contains(&self.peek_nchar(1).as_str()) {
                        let symbol =
                            Symbol::from_str(self.take_nchar(1).as_str()).expect("Invalid symbol");
                        return Ok(self.new_token(Token::Symbol(symbol), start));
                    }

                    return Err(TokenError::new(format!("invalid char({})", c)));
                }
            }
        }

        Ok(self.new_token(Token::Eof, self.loc))
    }

    fn eat_whitespace(&mut self) -> Result<LocatedToken, TokenError> {
        let start = self.loc;
        let _ws = self.eat_while(|c| c.is_whitespace());

        Ok(self.new_token(Token::Whitespace, start))
    }

    fn eat_number(&mut self) -> Result<LocatedToken, TokenError> {
        let start = self.loc;

        let mut s = self.eat_integer();
        if let Some('.') = self.peek_char() {
            let c = self.consume_char('.')?;

            s.push(c);

            let f = self.eat_integer();
            if f.is_empty() {
                return Err(TokenError::new("invalid float"));
            }

            s.push_str(&f);

            let f = s
                .parse::<f64>()
                .map_err(|err| TokenError::new("invalid float").with_source(err))?;
            return Ok(self.new_token(Token::Literal(Literal::Float(f)), start));
        }

        let i = s
            .parse::<i64>()
            .map_err(|err| TokenError::new("invalid integer").with_source(err))?;

        Ok(self.new_token(Token::Literal(Literal::Integer(i)), start))
    }

    fn eat_integer(&mut self) -> String {
        self.eat_while(|c| c.is_ascii_digit())
    }

    fn eat_string(&mut self) -> Result<LocatedToken, TokenError> {
        let start = self.loc;
        self.next_char();
        let s = self.eat_qouted('"')?;

        Ok(self.new_token(Token::Literal(Literal::String(s)), start))
    }

    fn eat_qouted(&mut self, qoute: char) -> Result<String, TokenError> {
        let mut ret = String::new();

        loop {
            match self.peek_char() {
                Some(c) if c == qoute => {
                    self.next_char();
                    return Ok(ret);
                }
                Some('\\') => {
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
                Some(c) => {
                    ret.push(c);
                }
                None => {
                    return Err(TokenError::new("unexpected end of input"));
                }
            }

            self.next_char();
        }
    }

    fn eat_identifier(&mut self) -> Result<LocatedToken, TokenError> {
        let start = self.loc;
        let s = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        if let Ok(kw) = Keyword::from_str(s.as_str()) {
            return Ok(self.new_token(Token::Keyword(kw), start));
        }

        Ok(self.new_token(
            Token::Identifier(Identifier {
                name: s.to_string(),
            }),
            start,
        ))
    }

    fn eat_variable(&mut self) -> Result<LocatedToken, TokenError> {
        self.next_char();
        let start = self.loc;
        let s = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');
        Ok(self.new_token(
            Token::Variable(Variable {
                name: s.to_string(),
            }),
            start,
        ))
    }

    /// Advance to the next chararter.
    fn next_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    self.loc.line += 1;
                    self.loc.column = 1;
                } else {
                    self.loc.column += 1;
                }
                Some(c)
            }
            None => None,
        }
    }

    fn consume_char(&mut self, ch: char) -> Result<char, TokenError> {
        match self.next_char() {
            Some(c) if c != ch => Err(TokenError::new(format!("expected '{}', got '{}'", ch, c))),
            Some(c) => Ok(c),
            None => Err(TokenError::new(format!("expected '{}', got EOF", ch))),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn has_at_least(&self, n: usize) -> bool {
        self.chars.clone().count() >= n
    }

    fn peek_nchar(&self, n: usize) -> String {
        self.chars.clone().take(n).collect::<String>()
    }

    fn take_nchar(&mut self, n: usize) -> String {
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

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek_char() {
            if !predicate(c) {
                break;
            }
            self.next_char();
            s.push(c);
        }

        s
    }

    fn new_token(&self, token: Token, start: LineColumn) -> LocatedToken {
        LocatedToken::new(
            token,
            Location {
                start,
                end: self.loc,
            },
        )
    }
}

#[derive(Debug)]
pub struct TokenError {
    pub(crate) detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
    position: usize,
}

impl TokenError {
    pub fn new<D: Into<Cow<'static, str>>>(detail: D) -> TokenError {
        TokenError {
            detail: Some(detail.into()),
            source: None,
            position: 0,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }

    pub fn with_position(mut self, position: usize) -> Self {
        self.position = position;
        self
    }

    pub fn unexpected_eof(position: usize) -> TokenError {
        TokenError::new("unexcepted eof").with_position(position)
    }
}

#[derive(Debug, Clone)]
pub struct LocatedToken {
    pub token: Token,
    pub location: Location,
}

impl LocatedToken {
    pub fn new(token: Token, location: Location) -> LocatedToken {
        LocatedToken { token, location }
    }
}

impl Deref for LocatedToken {
    type Target = Token;

    fn deref(&self) -> &Token {
        &self.token
    }
}

impl AsRef<Token> for LocatedToken {
    fn as_ref(&self) -> &Token {
        &self.token
    }
}

#[derive(Debug, Copy, Clone)]
pub struct LineColumn {
    line: usize,
    column: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct Location {
    start: LineColumn,
    end: LineColumn,
}

impl Location {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
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

impl<'i> IntoIterator for Lexer<'i> {
    type Item = Result<LocatedToken, TokenError>;
    type IntoIter = Tokens<'i>;

    fn into_iter(self) -> Self::IntoIter {
        Tokens { inner: self }
    }
}

#[derive(Debug, Clone)]
pub struct Tokens<'i> {
    inner: Lexer<'i>,
}

impl<'i> Tokens<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            inner: Lexer::new(input),
        }
    }
}

impl<'i> Iterator for Tokens<'i> {
    type Item = Result<LocatedToken, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Ok(pair) => {
                if pair.token == Token::Eof {
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
pub struct TokenStream<'i> {
    inner: Lexer<'i>,
}

impl<'i> TokenStream<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            inner: Lexer::new(input),
        }
    }
}

impl<'i> Iterator for TokenStream<'i> {
    type Item = Result<Token, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Ok(pair) => {
                if pair.token == Token::Eof {
                    None
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
            let mut tokenizer = Lexer::new(i.0);

            let mut ret = Vec::new();

            println!("-> {}", i.0);
            loop {
                let pair = tokenizer.next().unwrap();
                println!("pair: {pair:?}");
                if pair.token == Token::Eof {
                    break;
                }

                ret.push(pair.token);
            }

            assert_eq!(ret, i.1);
        }
    }
}
