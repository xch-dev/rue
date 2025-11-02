use std::str::Chars;

use crate::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            pos: 0,
        }
    }

    fn bump(&mut self) -> char {
        match self.chars.next() {
            Some(c) => {
                self.pos += c.len_utf8();
                c
            }
            None => '\0',
        }
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    fn at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn eat_whitespace(&mut self) -> TokenKind {
        while is_whitespace(self.peek()) {
            self.bump();
        }
        TokenKind::Whitespace
    }

    fn eat_ident(&mut self, start: usize) -> TokenKind {
        while matches!(self.peek(), 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
            self.bump();
        }

        match &self.source[start..self.pos] {
            "nil" => TokenKind::Nil,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "export" => TokenKind::Export,
            "extern" => TokenKind::Extern,
            "inline" => TokenKind::Inline,
            "test" => TokenKind::Test,
            "mod" => TokenKind::Mod,
            "fn" => TokenKind::Fn,
            "const" => TokenKind::Const,
            "type" => TokenKind::Type,
            "struct" => TokenKind::Struct,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "assert" => TokenKind::Assert,
            "raise" => TokenKind::Raise,
            "debug" => TokenKind::Debug,
            "is" => TokenKind::Is,
            "as" => TokenKind::As,
            _ => TokenKind::Ident,
        }
    }

    fn eat_line_comment(&mut self) -> TokenKind {
        while self.peek() != '\n' {
            self.bump();
        }
        self.bump();
        TokenKind::LineComment
    }

    fn eat_block_comment(&mut self) -> TokenKind {
        let mut depth = 1;

        // Skip the initial `*`
        self.bump();

        let is_terminated = loop {
            match self.bump() {
                '*' if self.peek() == '/' => {
                    depth -= 1;
                    self.bump();

                    if depth == 0 {
                        break true;
                    }
                }
                '/' if self.peek() == '*' => {
                    depth += 1;
                    self.bump();
                }
                '\0' => break false,
                _ => {}
            }
        };

        TokenKind::BlockComment { is_terminated }
    }

    fn eat_string(&mut self) -> TokenKind {
        let is_terminated = loop {
            match self.peek() {
                '"' => {
                    self.bump();
                    break true;
                }
                '\0' => {
                    break false;
                }
                _ => {
                    self.bump();
                }
            }
        };

        TokenKind::String { is_terminated }
    }

    fn eat_hex(&mut self) -> TokenKind {
        // Skip the initial `x`
        self.bump();

        let mut is_terminated = false;

        while matches!(self.peek(), '0'..='9' | 'a'..='f' | 'A'..='F' | '_') {
            self.bump();
            is_terminated = true;
        }

        TokenKind::Hex { is_terminated }
    }

    fn eat_binary(&mut self) -> TokenKind {
        // Skip the initial `b`
        self.bump();

        let mut is_terminated = false;

        while matches!(self.peek(), '0' | '1' | '_') {
            self.bump();
            is_terminated = true;
        }

        TokenKind::Binary { is_terminated }
    }

    fn eat_octal(&mut self) -> TokenKind {
        // Skip the initial `o`
        self.bump();

        let mut is_terminated = false;

        while matches!(self.peek(), '0'..='7' | '_') {
            self.bump();
            is_terminated = true;
        }

        TokenKind::Octal { is_terminated }
    }

    fn eat_integer(&mut self) -> TokenKind {
        while matches!(self.peek(), '0'..='9' | '_') {
            self.bump();
        }

        TokenKind::Integer
    }
}

fn is_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at_end() {
            return None;
        }

        let start = self.pos;

        let kind = match self.bump() {
            c if is_whitespace(c) => self.eat_whitespace(),
            'a'..='z' | 'A'..='Z' | '_' => self.eat_ident(start),
            '"' => self.eat_string(),
            '0' if self.peek() == 'x' => self.eat_hex(),
            '0' if self.peek() == 'b' => self.eat_binary(),
            '0' if self.peek() == 'o' => self.eat_octal(),
            '0'..='9' => self.eat_integer(),
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => match self.peek() {
                '/' => self.eat_line_comment(),
                '*' => self.eat_block_comment(),
                _ => TokenKind::Slash,
            },
            '%' => TokenKind::Percent,
            '=' => TokenKind::Equals,
            '<' => TokenKind::LessThan,
            '>' => TokenKind::GreaterThan,
            '!' => TokenKind::Not,
            '&' => TokenKind::And,
            '|' => TokenKind::Or,
            '~' => TokenKind::Tilde,
            '^' => TokenKind::Xor,
            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            _ => TokenKind::Unknown,
        };

        Some(Token::new(start..self.pos, kind))
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use super::*;

    #[allow(clippy::needless_pass_by_value)]
    fn check(source: &str, expect: Expect) {
        let mut output = String::new();

        for token in Lexer::new(source) {
            #[allow(clippy::format_push_string)]
            output.push_str(&format!("{:?}\n", token.kind));
        }

        expect.assert_eq(&output);
    }

    #[test]
    fn test_whitespace() {
        check(
            " ",
            expect![[r#"
                Whitespace
            "#]],
        );

        check(
            "    ",
            expect![[r#"
                Whitespace
            "#]],
        );

        check(
            "\t\r\n",
            expect![[r#"
                Whitespace
            "#]],
        );
    }

    #[test]
    fn test_line_comments() {
        // Basic line comment
        check(
            "// hello world\n",
            expect![[r#"
                LineComment
            "#]],
        );

        // Line comment with surrounding whitespace
        check(
            "  // hello world\n  ",
            expect![[r#"
                Whitespace
                LineComment
                Whitespace
            "#]],
        );

        // Multiple line comments
        check(
            "//first\n//second\n",
            expect![[r#"
                LineComment
                LineComment
            "#]],
        );
    }

    #[test]
    fn test_block_comments() {
        // Basic block comment
        check(
            "/* hello world */",
            expect![[r#"
                BlockComment { is_terminated: true }
            "#]],
        );

        // Block comment with surrounding whitespace
        check(
            "  /* hello world */  ",
            expect![[r#"
                Whitespace
                BlockComment { is_terminated: true }
                Whitespace
            "#]],
        );

        // Nested block comments
        check(
            "/* outer /* inner */ comment */",
            expect![[r#"
                BlockComment { is_terminated: true }
            "#]],
        );

        // Unterminated block comment
        check(
            "/* unterminated",
            expect![[r#"
                BlockComment { is_terminated: false }
            "#]],
        );

        // Unterminated nested block comment
        check(
            "/* outer /* inner */ unterminated",
            expect![[r#"
                BlockComment { is_terminated: false }
            "#]],
        );
    }

    #[test]
    fn test_string() {
        check(
            "\"hello world\"",
            expect![[r#"
                String { is_terminated: true }
            "#]],
        );

        check(
            "\"hello world",
            expect![[r#"
                String { is_terminated: false }
            "#]],
        );
    }

    #[test]
    fn test_hex() {
        check(
            "0x",
            expect![[r#"
                Hex { is_terminated: false }
            "#]],
        );

        check(
            "0x 42",
            expect![[r#"
                Hex { is_terminated: false }
                Whitespace
                Integer
            "#]],
        );

        check(
            "0xFACEF00D",
            expect![[r#"
                Hex { is_terminated: true }
            "#]],
        );

        check(
            "0xfacef00d",
            expect![[r#"
                Hex { is_terminated: true }
            "#]],
        );

        check(
            "0xabc_def___",
            expect![[r#"
                Hex { is_terminated: true }
            "#]],
        );
    }

    #[test]
    fn test_binary() {
        check(
            "0b",
            expect![[r#"
                Binary { is_terminated: false }
            "#]],
        );

        check(
            "0b 42",
            expect![[r#"
                Binary { is_terminated: false }
                Whitespace
                Integer
            "#]],
        );

        check(
            "0b01011",
            expect![[r#"
                Binary { is_terminated: true }
            "#]],
        );

        check(
            "0b01011_01100_",
            expect![[r#"
                Binary { is_terminated: true }
            "#]],
        );
    }

    #[test]
    fn test_octal() {
        check(
            "0o",
            expect![[r#"
                Octal { is_terminated: false }
            "#]],
        );

        check(
            "0o 42",
            expect![[r#"
                Octal { is_terminated: false }
                Whitespace
                Integer
            "#]],
        );

        check(
            "0o0471",
            expect![[r#"
                Octal { is_terminated: true }
            "#]],
        );

        check(
            "0o0471_0541_",
            expect![[r#"
                Octal { is_terminated: true }
            "#]],
        );
    }

    #[test]
    fn test_integer() {
        check(
            "0",
            expect![[r#"
                Integer
            "#]],
        );

        check(
            "1234567890",
            expect![[r#"
                Integer
            "#]],
        );

        check(
            "1__23456_7890__",
            expect![[r#"
                Integer
            "#]],
        );
    }

    #[test]
    fn test_keyword() {
        check(
            "nil_",
            expect![[r#"
            Ident
        "#]],
        );

        check(
            "nil",
            expect![[r#"
                Nil
            "#]],
        );

        check(
            "true",
            expect![[r#"
                True
            "#]],
        );

        check(
            "false",
            expect![[r#"
                False
            "#]],
        );

        check(
            "export",
            expect![[r#"
                Export
            "#]],
        );

        check(
            "extern",
            expect![[r#"
                Extern
            "#]],
        );

        check(
            "inline",
            expect![[r#"
                Inline
            "#]],
        );

        check(
            "test",
            expect![[r#"
                Test
            "#]],
        );

        check(
            "mod",
            expect![[r#"
                Mod
            "#]],
        );

        check(
            "fn",
            expect![[r#"
                Fn
            "#]],
        );

        check(
            "const",
            expect![[r#"
                Const
            "#]],
        );

        check(
            "type",
            expect![[r#"
                Type
            "#]],
        );

        check(
            "struct",
            expect![[r#"
                Struct
            "#]],
        );

        check(
            "let",
            expect![[r#"
                Let
            "#]],
        );

        check(
            "if",
            expect![[r#"
                If
            "#]],
        );

        check(
            "else",
            expect![[r#"
                Else
            "#]],
        );

        check(
            "return",
            expect![[r#"
                Return
            "#]],
        );

        check(
            "assert",
            expect![[r#"
                Assert
            "#]],
        );

        check(
            "raise",
            expect![[r#"
                Raise
            "#]],
        );

        check(
            "debug",
            expect![[r#"
                Debug
            "#]],
        );

        check(
            "is",
            expect![[r#"
                Is
            "#]],
        );

        check(
            "as",
            expect![[r#"
                As
            "#]],
        );
    }

    #[test]
    fn test_grouping() {
        check(
            "()",
            expect![[r#"
                OpenParen
                CloseParen
            "#]],
        );

        check(
            "{}",
            expect![[r#"
                OpenBrace
                CloseBrace
            "#]],
        );

        check(
            "[]",
            expect![[r#"
                OpenBracket
                CloseBracket
            "#]],
        );
    }

    #[test]
    fn test_arithmetic() {
        check(
            "+-*/%",
            expect![[r#"
                Plus
                Minus
                Star
                Slash
                Percent
            "#]],
        );
    }

    #[test]
    fn test_comparison() {
        check(
            "=<>",
            expect![[r#"
                Equals
                LessThan
                GreaterThan
            "#]],
        );
    }

    #[test]
    fn test_logical() {
        check(
            "!&|",
            expect![[r#"
                Not
                And
                Or
            "#]],
        );
    }

    #[test]
    fn test_bitwise() {
        check(
            "~^",
            expect![[r#"
                Tilde
                Xor
            "#]],
        );
    }

    #[test]
    fn test_punctuation() {
        check(
            ".,:;",
            expect![[r#"
                Dot
                Comma
                Colon
                Semicolon
            "#]],
        );
    }

    #[test]
    fn test_unknown() {
        check(
            "\\",
            expect![[r#"
                Unknown
            "#]],
        );
    }
}
