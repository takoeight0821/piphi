use self::{
    lexer::{Token, TokenKind},
    range::Range,
};
use crate::syntax::{Clause, Expr, Pat};
use anyhow::Result;

pub mod lexer;
pub mod range;

/// Parses the given tokens into an AST.
/// Tokens must not include spaces and newlines.
pub fn parse(tokens: Vec<Token>) -> Result<Box<Expr>> {
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
    pub fn parse(&mut self) -> Result<Box<Expr>> {
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
    fn expr(&mut self) -> Result<Box<Expr>> {
        self.apply()
    }

    /// apply ::= term (term)*
    fn apply(&mut self) -> Result<Box<Expr>> {
        fn build_apply(terms: Vec<Expr>) -> Box<Expr> {
            let mut iter = terms.into_iter();
            let mut expr: Box<Expr> = Box::new(iter.next().unwrap());
            for term in iter {
                expr = Expr::apply(&expr, &term, expr.range + term.range);
            }
            expr
        }

        let mut terms: Vec<Expr> = vec![];
        while let Ok(term) = self.term() {
            terms.push(*term);
        }
        if terms.is_empty() {
            Err(self.expected_error("term"))
        } else {
            Ok(build_apply(terms))
        }
    }

    /// term ::= atom | codata
    fn term(&mut self) -> Result<Box<Expr>> {
        if let Ok(atom) = self.atom() {
            Ok(atom)
        } else if let Ok(codata) = self.codata() {
            Ok(codata)
        } else {
            Err(self.expected_error("term"))
        }
    }

    /// atom ::= identifier | number | '(' expr ')'
    fn atom(&mut self) -> Result<Box<Expr>> {
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
    fn codata(&mut self) -> Result<Box<Expr>> {
        let start = self
            .peek()
            .map_or(Default::default(), |token| token.range.start);

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

        let end = self
            .peek()
            .map_or(Default::default(), |token| token.range.end);
        if !self.match_token(&TokenKind::Symbol("}".to_string())) {
            return Err(self.expected_error("'}}'"));
        }
        Ok(Expr::codata(clauses, Range { start, end }))
    }

    /// clause ::= pattern '->' expr
    fn clause(&mut self) -> Result<(Pat, Box<Expr>)> {
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

#[cfg(test)]
mod tests {
    use super::{
        lexer::{remove_whitespace, tokenize},
        parse,
        range::{Position, Range},
    };
    use crate::syntax::{Clause, Expr, ExprKind, Pat, PatKind};
    use pretty_assertions::assert_eq;

    trait ResetPosition {
        fn reset(&self) -> Self;
    }

    impl ResetPosition for Position {
        fn reset(&self) -> Self {
            Position {
                offset: Default::default(),
                line: Default::default(),
                column: Default::default(),
            }
        }
    }

    impl ResetPosition for Range {
        fn reset(&self) -> Self {
            Range {
                start: self.start.reset(),
                end: self.end.reset(),
            }
        }
    }

    impl ResetPosition for Expr {
        fn reset(&self) -> Self {
            Expr {
                kind: self.kind.reset(),
                range: self.range.reset(),
            }
        }
    }

    impl ResetPosition for ExprKind {
        fn reset(&self) -> Self {
            match self {
                ExprKind::Apply(f, x) => ExprKind::Apply(Box::new(f.reset()), Box::new(x.reset())),
                ExprKind::Codata(cs) => ExprKind::Codata(cs.iter().map(|c| c.reset()).collect()),
                _ => self.clone(),
            }
        }
    }

    impl ResetPosition for Clause {
        fn reset(&self) -> Self {
            Clause {
                pattern: self.pattern.reset(),
                body: self.body.reset(),
            }
        }
    }

    impl ResetPosition for Pat {
        fn reset(&self) -> Self {
            Pat {
                kind: self.kind.reset(),
                range: self.range.reset(),
            }
        }
    }

    impl ResetPosition for PatKind {
        fn reset(&self) -> Self {
            match self {
                PatKind::Sequence(ps) => PatKind::Sequence(ps.iter().map(|p| p.reset()).collect()),
                _ => self.clone(),
            }
        }
    }

    #[test]
    fn parse_test() {
        let fun = Expr::codata(
            vec![
                Clause::new(
                    &Pat::sequence(
                        vec![
                            Pat::label("get", Default::default()),
                            Pat::sequence(
                                vec![
                                    Pat::this(Default::default()),
                                    Pat::variable("x", Default::default()),
                                ],
                                Default::default(),
                            ),
                        ],
                        Default::default(),
                    ),
                    &Expr::variable("x", Default::default()),
                ),
                Clause::new(
                    &Pat::sequence(
                        vec![
                            Pat::label("set", Default::default()),
                            Pat::sequence(
                                vec![
                                    Pat::this(Default::default()),
                                    Pat::variable("p", Default::default()),
                                ],
                                Default::default(),
                            ),
                            Pat::variable("y", Default::default()),
                        ],
                        Default::default(),
                    ),
                    &Expr::apply(
                        &Expr::variable("fun", Default::default()),
                        &Expr::variable("y", Default::default()),
                        Default::default(),
                    ),
                ),
            ],
            Default::default(),
        );
        let right = Expr::apply(
            &fun,
            &Expr::number(1, Default::default()),
            Default::default(),
        );
        let left = Expr::label("get", Default::default());
        let expr = Expr::apply(&left, &right, Default::default());

        let src = format!("{}", expr);

        let tokens = tokenize(&src).unwrap();

        let parsed = parse(remove_whitespace(&tokens)).map(|x| Box::new(x.reset()));

        assert_eq!(parsed.ok(), Some(expr))
    }
}
