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
}

fn is_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at_end() {
            return None;
        }

        let start = self.pos;

        let kind = match self.bump() {
            c if is_whitespace(c) => self.eat_whitespace(),
            '/' => match self.peek() {
                '/' => self.eat_line_comment(),
                '*' => self.eat_block_comment(),
                _ => TokenKind::Slash,
            },
            _ => TokenKind::Unknown,
        };

        Some(Token::new(start..self.pos, kind))
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};

    use super::*;

    fn check(source: &str, expect: Expect) {
        let mut output = String::new();

        for token in Lexer::new(source) {
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
    fn test_unknown() {
        check(
            "\\",
            expect![[r#"
                Unknown
            "#]],
        );
    }
}
