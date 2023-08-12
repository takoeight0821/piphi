use super::*;

#[test]
fn test_lexer() {
    let testcases = vec![
        (
            "1 + 2",
            vec![
                Token::number(1, 0, 0),
                Token::space(0, 1),
                Token::symbol("+", 0, 2),
                Token::space(0, 3),
                Token::number(2, 0, 4),
            ],
        ),
        (
            "{ x -> x }",
            vec![
                Token::symbol("{", 0, 0),
                Token::space(0, 1),
                Token::ident("x", 0, 2),
                Token::space(0, 3),
                Token::symbol("->", 0, 4),
                Token::space(0, 6),
                Token::ident("x", 0, 7),
                Token::space(0, 8),
                Token::symbol("}", 0, 9),
            ],
        ),
        (
            "let x = 1\n in x + 2",
            vec![
                Token::ident("let", 0, 0),
                Token::space(0, 3),
                Token::ident("x", 0, 4),
                Token::space(0, 5),
                Token::symbol("=", 0, 6),
                Token::space(0, 7),
                Token::number(1, 0, 8),
                Token::newline(0, 9),
                Token::space(1, 0),
                Token::ident("in", 1, 1),
                Token::space(1, 3),
                Token::ident("x", 1, 4),
                Token::space(1, 5),
                Token::symbol("+", 1, 6),
                Token::space(1, 7),
                Token::number(2, 1, 8),
            ],
        ),
        (
            "get (# 0)",
            vec![
                Token::ident("get", 0, 0),
                Token::space(0, 3),
                Token::symbol("(", 0, 4),
                Token::symbol("#", 0, 5),
                Token::space(0, 6),
                Token::number(0, 0, 7),
                Token::symbol(")", 0, 8),
            ],
        ),
        (
            "((~~))",
            vec![
                Token::symbol("(", 0, 0),
                Token::symbol("(", 0, 1),
                Token::symbol("~~", 0, 2),
                Token::symbol(")", 0, 4),
                Token::symbol(")", 0, 5),
            ],
        ),
        (
            "「こんにちは」",
            vec![
                Token::symbol("「", 0, 0),
                Token::ident("こんにちは", 0, 1),
                Token::symbol("」", 0, 6),
            ],
        ),
        ("0b1010", vec![Token::number(0b1010, 0, 0)]),
        ("0o755", vec![Token::number(0o755, 0, 0)]),
        ("0xDEADBEAF", vec![Token::number(0xDEADBEAF, 0, 0)]),
    ];
    for (source, expected) in testcases {
        match tokenize(source) {
            Ok(tokens) => assert_eq!(tokens, expected),
            Err(e) => panic!("{}", e),
        }
    }
}
