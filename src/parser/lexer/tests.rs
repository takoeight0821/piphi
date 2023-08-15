use super::*;
use pretty_assertions::assert_eq;

///
/// Builds a vector of tokens from a vector of string-token pairs.
///
/// # Arguments
///
/// * `tokens` - A vector of string-token pairs.
///
/// # Returns
///
/// A vector of tokens with positions.
fn build_testcase(tokens: Vec<(&str, TokenKind)>) -> Vec<Token> {
    let mut result = Vec::new();
    let mut offset = 0;
    let mut line = 0;
    let mut column = 0;
    for (s, kind) in tokens {
        let start = Position {
            offset,
            line,
            column,
        };
        offset += s.len();
        if matches!(kind, TokenKind::Newline) {
            line += 1;
            column = 0;
        } else {
            column += s.len();
        }
        let end = Position {
            offset,
            line,
            column,
        };
        result.push(Token {
            kind,
            range: Range { start, end },
        });
    }
    result
}

#[test]
fn test_lexer() {
    use TokenKind::*;
    let testcases = vec![
        (
            "1 + 2",
            vec![
                ("1", Number(1)),
                (" ", Space),
                ("+", Symbol("+".to_string())),
                (" ", Space),
                ("2", Number(2)),
            ],
        ),
        (
            "{ x -> x }",
            vec![
                ("{", Symbol("{".to_string())),
                (" ", Space),
                ("x", Ident("x".to_string())),
                (" ", Space),
                ("->", Symbol("->".to_string())),
                (" ", Space),
                ("x", Ident("x".to_string())),
                (" ", Space),
                ("}", Symbol("}".to_string())),
            ],
        ),
        (
            "let x = 1\n in x + 2",
            vec![
                ("let", Ident("let".to_string())),
                (" ", Space),
                ("x", Ident("x".to_string())),
                (" ", Space),
                ("=", Symbol("=".to_string())),
                (" ", Space),
                ("1", Number(1)),
                ("\n", Newline),
                (" ", Space),
                ("in", Ident("in".to_string())),
                (" ", Space),
                ("x", Ident("x".to_string())),
                (" ", Space),
                ("+", Symbol("+".to_string())),
                (" ", Space),
                ("2", Number(2)),
            ],
        ),
        (
            "get (# 0)",
            vec![
                ("get", Ident("get".to_string())),
                (" ", Space),
                ("(", Symbol("(".to_string())),
                ("#", Symbol("#".to_string())),
                (" ", Space),
                ("0", Number(0)),
                (")", Symbol(")".to_string())),
            ],
        ),
        (
            "((~~))",
            vec![
                ("(", Symbol("(".to_string())),
                ("(", Symbol("(".to_string())),
                ("~~", Symbol("~~".to_string())),
                (")", Symbol(")".to_string())),
                (")", Symbol(")".to_string())),
            ],
        ),
        (
            "「こんにちは」",
            vec![
                ("「", Symbol("「".to_string())),
                ("こんにちは", Ident("こんにちは".to_string())),
                ("」", Symbol("」".to_string())),
            ],
        ),
        ("0b1010", vec![("0b1010", Number(0b1010))]),
        ("0o755", vec![("0o755", Number(0o755))]),
        ("0xDEADBEEF", vec![("0xDEADBEEF", Number(0xDEADBEEF))]),
    ];
    for (source, expected) in testcases {
        match tokenize(source) {
            Ok(tokens) => assert_eq!(tokens, build_testcase(expected)),
            Err(e) => panic!("{}", e),
        }
    }
}
