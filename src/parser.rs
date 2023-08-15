use anyhow::{anyhow, Context, Result};
use std::{
    fmt::{self, Display, Formatter},
    str::Chars,
};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

// TODO: add span
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Space,
    Newline,
    Ident(String),
    Number(i64),
    Symbol(String),
}

impl Token {
    pub fn space(range: Range) -> Token {
        Token {
            kind: TokenKind::Space,
            range,
        }
    }

    pub fn newline(range: Range) -> Token {
        Token {
            kind: TokenKind::Newline,
            range,
        }
    }

    pub fn ident(name: &str, range: Range) -> Token {
        Token {
            kind: TokenKind::Ident(name.to_string()),
            range,
        }
    }

    pub fn number(n: i64, range: Range) -> Token {
        Token {
            kind: TokenKind::Number(n),
            range,
        }
    }

    pub fn symbol(name: &str, range: Range) -> Token {
        Token {
            kind: TokenKind::Symbol(name.to_string()),
            range,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.kind {
            TokenKind::Space => write!(f, " "),
            TokenKind::Newline => write!(f, "\\n"),
            TokenKind::Ident(name) => write!(f, "{}", name),
            TokenKind::Number(n) => write!(f, "{}", n),
            TokenKind::Symbol(name) => write!(f, "{}", name),
        }
    }
}

pub struct Lexer<'a> {
    input: Chars<'a>,
    pos: usize,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input.chars(),
            pos: 0,
            line: 0,
            column: 0,
        }
    }

    fn consume(&mut self) -> Result<char> {
        let c = self.input.next().context("unexpected EOF")?;
        self.pos += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += c.len_utf8();
        }
        Ok(c)
    }

    fn peek(&self, i: usize) -> Option<char> {
        self.input.clone().nth(i)
    }

    fn current_position(&self) -> Position {
        Position {
            offset: self.pos,
            line: self.line,
            column: self.column,
        }
    }

    fn lex(&mut self) -> Result<Token> {
        let start = self.current_position();

        let c = self.consume()?;
        match c {
            // _ if c.is_whitespace() => self.lex(),
            ' ' | '\t' => {
                let end = self.current_position();
                Ok(Token::space(Range { start, end }))
            }
            '\r' if self.peek(0) == Some('\n') => {
                self.consume()?; // skip '\n'
                let end = self.current_position();
                Ok(Token::newline(Range { start, end }))
            }
            '\r' | '\n' => {
                let end = self.current_position();
                Ok(Token::newline(Range { start, end }))
            }
            '0' if self.peek(0) == Some('b') => {
                self.consume()?; // skip 'b'
                let mut n = 0;
                while let Some(c) = self.peek(0) {
                    if let Some(d) = c.to_digit(2) {
                        n = n * 2 + d;
                        self.consume()?;
                    } else {
                        break;
                    }
                }
                let end = self.current_position();
                Ok(Token::number(n as i64, Range { start, end }))
            }
            '0' if self.peek(0) == Some('o') => {
                self.consume()?; // skip 'o'
                let mut n = 0;
                while let Some(c) = self.peek(0) {
                    if let Some(d) = c.to_digit(8) {
                        n = n * 8 + d;
                        self.consume()?;
                    } else {
                        break;
                    }
                }
                let end = self.current_position();
                Ok(Token::number(n as i64, Range { start, end }))
            }
            '0' if self.peek(0) == Some('x') => {
                self.consume()?; // skip 'x'
                let mut n = 0;
                while let Some(c) = self.peek(0) {
                    if let Some(d) = c.to_digit(16) {
                        n = n * 16 + d;
                        self.consume()?;
                    } else {
                        break;
                    }
                }
                let end = self.current_position();
                Ok(Token::number(n as i64, Range { start, end }))
            }
            '0'..='9' => {
                let mut n = c.to_digit(10).unwrap();
                while let Some(c) = self.peek(0) {
                    if let Some(d) = c.to_digit(10) {
                        n = n * 10 + d;
                        self.consume()?;
                    } else {
                        break;
                    }
                }
                let end = self.current_position();
                Ok(Token::number(n as i64, Range { start, end }))
            }
            // Identifiers starts with XID_Start
            // To check if a character is XID_Start, use regex crate.
            _ if is_ident_start(c) => {
                let mut name = c.to_string();
                while let Some(c) = self.peek(0) {
                    if is_ident_continue(c) {
                        name.push(c);
                        self.consume()?;
                    } else {
                        break;
                    }
                }
                let end = self.current_position();
                Ok(Token::ident(&name, Range { start, end }))
            }
            // Symbols are a sequence of Symbol or Punctuation except for Open and Close
            _ if is_symbol_punctuation(c) => {
                let mut name = c.to_string();
                while let Some(c) = self.peek(0) {
                    if is_symbol_punctuation(c) {
                        name.push(c);
                        self.consume()?;
                    } else {
                        break;
                    }
                }
                let end = self.current_position();
                Ok(Token::symbol(&name, Range { start, end }))
            }
            // Open and Close Puncutuation are tokens by themselves
            _ if is_open_close(c) => {
                let end = self.current_position();
                Ok(Token::symbol(&c.to_string(), Range { start, end }))
            }
            _ => Err(anyhow!("Unexpected character: {}", c)),
        }
    }
}

fn is_ident_start(c: char) -> bool {
    regex::Regex::new(r"\p{XID_Start}")
        .unwrap()
        .is_match(&c.to_string())
}

fn is_ident_continue(c: char) -> bool {
    regex::Regex::new(r"\p{XID_Continue}")
        .unwrap()
        .is_match(&c.to_string())
}

fn is_symbol_punctuation(c: char) -> bool {
    regex::Regex::new(r"\p{Symbol}|\p{Punctuation}")
        .unwrap()
        .is_match(&c.to_string())
        && !is_open_close(c)
}

fn is_open_close(c: char) -> bool {
    regex::Regex::new(r"\p{Open_Punctuation}|\p{Close_Punctuation}")
        .unwrap()
        .is_match(&c.to_string())
}

pub fn tokenize(source: &str) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    while let Ok(token) = lexer.lex() {
        tokens.push(token);
    }
    Ok(tokens)
}
