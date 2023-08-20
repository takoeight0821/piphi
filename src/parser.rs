use self::{
    lexer::{Token, TokenKind},
    range::{Position, Range},
};
use crate::syntax::{Clause, Expr, Pat};
use anyhow::Result;
use std::rc::Rc;

pub mod lexer;
pub mod range;

/// Parses the given tokens into an AST.
/// Tokens must not include spaces and newlines.
pub fn parse(tokens: Vec<lexer::Token>) -> Result<Rc<Expr>> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Creates a new parser from the given tokens.
    /// Tokens must not include spaces and newlines.
    pub fn new(tokens: Vec<lexer::Token>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Returns the AST of the expression from parsing the tokens.
    pub fn parse(&mut self) -> Result<Rc<Expr>> {
        self.expr()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn consume(&mut self) {
        self.current += 1
    }

    fn expected_error(&self, expected: &str) -> anyhow::Error {
        match self.peek() {
            Some(token) => anyhow::anyhow!("expected {}, found '{}'", expected, token),
            None => anyhow::anyhow!("expected {}, found EOF", expected),
        }
    }

    fn match_token(&mut self, token: &TokenKind) -> bool {
        match self.peek() {
            Some(Token { kind, .. }) if kind == token => {
                self.consume();
                true
            }
            _ => false,
        }
    }

    /// expr ::= apply
    fn expr(&mut self) -> Result<Rc<Expr>> {
        self.apply()
    }

    /// apply ::= term (term)*
    fn apply(&mut self) -> Result<Rc<Expr>> {
        fn build_apply(terms: Vec<Rc<Expr>>) -> Rc<Expr> {
            let mut iter = terms.into_iter();
            let mut expr = iter.next().unwrap();
            for term in iter {
                expr = Expr::apply(&expr, &term);
            }
            expr
        }

        let mut terms = vec![];
        while let Ok(term) = self.term() {
            terms.push(term);
        }
        if terms.is_empty() {
            Err(self.expected_error("term"))
        } else {
            Ok(build_apply(terms))
        }
    }

    /// term ::= atom | codata
    fn term(&mut self) -> Result<Rc<Expr>> {
        if let Ok(atom) = self.atom() {
            Ok(atom)
        } else if let Ok(codata) = self.codata() {
            Ok(codata)
        } else {
            Err(self.expected_error("term"))
        }
    }

    /// atom ::= identifier | number | '(' expr ')'
    fn atom(&mut self) -> Result<Rc<Expr>> {
        if let Ok(ident) = self.identifier() {
            if ident.starts_with('.') {
                Ok(Expr::label(ident.strip_prefix('.').unwrap()))
            } else {
                Ok(Expr::variable(&ident))
            }
        } else if let Ok(number) = self.number() {
            Ok(Expr::number(number))
        } else if self.match_token(&TokenKind::Symbol("(".to_string())) {
            let expr = self.expr()?;
            if !self.match_token(&TokenKind::Symbol(")".to_string())) {
                return Err(self.expected_error("')'"));
            }
            Ok(expr)
        } else {
            Err(self.expected_error("atom"))
        }
    }

    /// codata ::= '{' clause (',' clause)* '}'
    fn codata(&mut self) -> Result<Rc<Expr>> {
        if !self.match_token(&TokenKind::Symbol("{".to_string())) {
            return Err(self.expected_error("'{{'"));
        }
        let mut clauses = vec![];
        loop {
            if let Ok((pat, expr)) = self.clause() {
                clauses.push(Clause::new(&pat, &expr));
            } else {
                break;
            }
            if !self.match_token(&TokenKind::Symbol(",".to_string())) {
                break;
            }
        }
        if !self.match_token(&TokenKind::Symbol("}".to_string())) {
            return Err(self.expected_error("'}}'"));
        }
        Ok(Expr::codata(clauses))
    }

    /// clause ::= pattern '->' expr
    fn clause(&mut self) -> Result<(Pat, Rc<Expr>)> {
        let pat = self.pattern()?;
        if !self.match_token(&TokenKind::Symbol("->".to_string())) {
            return Err(self.expected_error("'->'"));
        }
        let expr = self.expr()?;
        Ok((pat, expr))
    }

    /// pattern ::= pat_sequence
    fn pattern(&mut self) -> Result<Pat> {
        self.pat_sequence()
    }

    /// pat_sequence ::= pat_term (pat_term)*
    fn pat_sequence(&mut self) -> Result<Pat> {
        let mut pats = vec![];
        while let Ok(pat) = self.pat_term() {
            pats.push(pat);
        }
        if pats.is_empty() {
            Err(self.expected_error("pattern sequence"))
        } else if pats.len() == 1 {
            Ok(pats.pop().unwrap())
        } else {
            let range = Range {
                start: pats.first().unwrap().range.start,
                end: pats.last().unwrap().range.end,
            };
            Ok(Pat::sequence(pats, range))
        }
    }

    /// pat_term ::= identifier | number | '#' | '(' pat_sequence ')'
    fn pat_term(&mut self) -> Result<Pat> {
        let start = self
            .peek()
            .map_or(Position::default(), |token| token.range.start);
        let end = self
            .peek()
            .map_or(Position::default(), |token| token.range.end);

        if let Ok(ident) = self.identifier() {
            if ident.starts_with('.') {
                Ok(Pat::label(
                    ident.strip_prefix('.').unwrap(),
                    Range { start, end },
                ))
            } else {
                Ok(Pat::variable(&ident, Range { start, end }))
            }
        } else if let Ok(number) = self.number() {
            Ok(Pat::number(number, Range { start, end }))
        } else if self.match_token(&TokenKind::Symbol("#".to_string())) {
            Ok(Pat::this(Range { start, end }))
        } else if self.match_token(&TokenKind::Symbol("(".to_string())) {
            let pat = self.pattern()?;
            if !self.match_token(&TokenKind::Symbol(")".to_string())) {
                return Err(self.expected_error("')'"));
            }
            Ok(pat)
        } else {
            Err(self.expected_error("pattern term"))
        }
    }

    fn identifier(&mut self) -> Result<String> {
        let ident = match self.peek() {
            Some(Token {
                kind: TokenKind::Ident(ident),
                ..
            }) => Ok(ident.clone()),
            _ => Err(self.expected_error("identifier")),
        }?;
        self.consume();
        Ok(ident)
    }

    fn number(&mut self) -> Result<i64> {
        let number = match self.peek() {
            Some(Token {
                kind: TokenKind::Number(number),
                ..
            }) => Ok(*number),
            _ => Err(self.expected_error("number")),
        }?;
        self.consume();
        Ok(number)
    }
}
