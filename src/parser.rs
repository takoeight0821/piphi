use self::{
    lexer::{Token, TokenKind},
    range::Range,
};
use crate::syntax::{Clause, Expr, Pat};
use anyhow::Result;

pub mod lexer;
pub mod range;
#[cfg(test)]
mod tests;

/// Parses the given tokens into an AST.
/// Tokens must not include spaces and newlines.
pub fn parse(tokens: Vec<Token>) -> Result<Expr> {
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
    pub fn parse(&mut self) -> Result<Expr> {
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

    /// expr ::= let
    fn expr(&mut self) -> Result<Expr> {
        if let Ok(e) = self.let_() {
            Ok(e)
        } else {
            self.fix()
        }
    }

    fn fix(&mut self) -> Result<Expr> {
        let range = self.peek().map_or(Default::default(), |token| token.range);

        if !self.match_token(&TokenKind::Ident("fix".to_string())) {
            return self.atom();
        }
        let name = self.identifier().unwrap();
        if !self.match_token(&TokenKind::Ident("in".to_string())) {
            return Err(self.expected_error("'in'"));
        }
        let body = self.expr().unwrap();
        Ok(Expr::fix(&name, body, range))
    }

    /// let ::= 'fix' identifier 'in' expr | 'let' identifier '=' expr 'in' expr | apply
    fn let_(&mut self) -> Result<Expr> {
        let range = self.peek().map_or(Default::default(), |token| token.range);

        if self.match_token(&TokenKind::Ident("fix".to_string())) {
            let name = self.identifier().unwrap();
            if !self.match_token(&TokenKind::Ident("in".to_string())) {
                return Err(self.expected_error("'in'"));
            }
            let body = self.expr().unwrap();
            return Ok(Expr::fix(&name, body, range));
        }

        if !self.match_token(&TokenKind::Ident("let".to_string())) {
            return self.apply();
        }
        let name = self.identifier()?;
        if !self.match_token(&TokenKind::Symbol("=".to_string())) {
            return Err(self.expected_error("'='"));
        }
        let value = self.expr()?;
        if !self.match_token(&TokenKind::Ident("in".to_string())) {
            return Err(self.expected_error("'in'"));
        }
        let body = self.expr()?;
        Ok(Expr::let_(&name, value, body, range))
    }

    /// apply ::= term (term)*
    fn apply(&mut self) -> Result<Expr> {
        fn build_apply(terms: Vec<Expr>) -> Expr {
            let mut iter = terms.into_iter();
            let mut expr: Expr = iter.next().unwrap();
            for term in iter {
                let range = expr.range + term.range;
                expr = Expr::apply(expr, term, range);
            }
            expr
        }

        let mut terms: Vec<Expr> = vec![];
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
    fn term(&mut self) -> Result<Expr> {
        if let Ok(atom) = self.atom() {
            Ok(atom)
        } else if let Ok(codata) = self.codata() {
            Ok(codata)
        } else {
            Err(self.expected_error("term"))
        }
    }

    /// atom ::= identifier | number | '(' expr ')'
    fn atom(&mut self) -> Result<Expr> {
        let range = self.peek().map_or(Default::default(), |token| token.range);

        if let Ok(ident) = self.identifier() {
            if ident.starts_with('.') {
                Ok(Expr::label(ident.strip_prefix('.').unwrap(), range))
            } else {
                Ok(Expr::variable(&ident, range))
            }
        } else if let Ok(number) = self.number() {
            Ok(Expr::number(number, range))
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
    fn codata(&mut self) -> Result<Expr> {
        let start = self
            .peek()
            .map_or(Default::default(), |token| token.range.start);

        if !self.match_token(&TokenKind::Symbol("{".to_string())) {
            return Err(self.expected_error("'{{'"));
        }
        let mut clauses = vec![];
        loop {
            if let Ok((pattern, body)) = self.clause() {
                clauses.push(Clause::new(pattern, body));
            } else {
                break;
            }
            if !self.match_token(&TokenKind::Symbol(",".to_string())) {
                break;
            }
        }

        let end = self
            .peek()
            .map_or(Default::default(), |token| token.range.end);
        if !self.match_token(&TokenKind::Symbol("}".to_string())) {
            return Err(self.expected_error("'}}'"));
        }
        Ok(Expr::codata(clauses, Range { start, end }))
    }

    /// clause ::= pattern '->' expr
    fn clause(&mut self) -> Result<(Pat, Expr)> {
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
            Ok(Pat::sequence(&pats, range))
        }
    }

    /// pat_term ::= identifier | number | '#' | '(' pat_sequence ')'
    fn pat_term(&mut self) -> Result<Pat> {
        let range = self.peek().map_or(Default::default(), |token| token.range);

        if let Ok(ident) = self.identifier() {
            if ident.starts_with('.') {
                Ok(Pat::label(ident.strip_prefix('.').unwrap(), range))
            } else {
                Ok(Pat::variable(&ident, range))
            }
        } else if let Ok(number) = self.number() {
            Ok(Pat::number(number, range))
        } else if self.match_token(&TokenKind::Symbol("#".to_string())) {
            Ok(Pat::this(range))
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

    fn is_keyword(name: &str) -> bool {
        const KEYWORDS: [&str; 2] = ["let", "in"];
        KEYWORDS.contains(&name)
    }

    fn identifier(&mut self) -> Result<String> {
        let ident = match self.peek() {
            Some(Token {
                kind: TokenKind::Ident(ident),
                ..
            }) => Ok(ident.clone()),
            _ => Err(self.expected_error("identifier")),
        }?;
        if Self::is_keyword(&ident) {
            Err(self.expected_error("identifier"))
        } else {
            self.consume();
            Ok(ident)
        }
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
