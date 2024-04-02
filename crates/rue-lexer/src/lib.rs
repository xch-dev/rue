mod token;
mod token_kind;

use std::str::Chars;

pub use token::*;
pub use token_kind::*;
use unicode_xid::UnicodeXID;

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

    fn token(&mut self) -> Option<Token> {
        let start = self.pos;

        let kind = match self.bump() {
            '\0' => return None,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            ',' => TokenKind::Comma,
            '+' => TokenKind::Plus,
            '-' => match self.peek() {
                '>' => {
                    self.bump();
                    TokenKind::Arrow
                }
                _ => TokenKind::Minus,
            },
            '*' => TokenKind::Star,
            '<' => TokenKind::LessThan,
            '>' => TokenKind::GreaterThan,
            '=' => match self.peek() {
                '=' => {
                    self.bump();
                    TokenKind::Equals
                }
                _ => TokenKind::Unknown,
            },
            '/' => match self.peek() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => TokenKind::Slash,
            },
            ':' => TokenKind::Colon,
            c if c.is_numeric() => {
                while self.peek().is_numeric() {
                    self.bump();
                }
                TokenKind::Int
            }
            c if UnicodeXID::is_xid_start(c) || c == '_' => {
                while UnicodeXID::is_xid_continue(self.peek()) {
                    self.bump();
                }
                match &self.source[start..self.pos] {
                    "fun" => TokenKind::Fun,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "nil" => TokenKind::Nil,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    _ => TokenKind::Ident,
                }
            }
            c if c.is_whitespace() => {
                while self.peek().is_whitespace() {
                    self.bump();
                }
                TokenKind::Whitespace
            }
            _ => TokenKind::Unknown,
        };

        Some(Token::new(kind, self.pos - start))
    }

    fn line_comment(&mut self) -> TokenKind {
        loop {
            match self.bump() {
                '\n' | '\0' => break,
                _ => {}
            }
        }
        TokenKind::LineComment
    }

    fn block_comment(&mut self) -> TokenKind {
        self.bump();
        loop {
            match self.bump() {
                '*' => {
                    if self.peek() == '/' {
                        self.bump();
                        break;
                    }
                }
                '\0' => break,
                _ => {}
            }
        }
        TokenKind::BlockComment
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
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
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(source: &str, expected: &[TokenKind]) {
        let actual: Vec<TokenKind> = Lexer::new(source).map(|token| token.kind()).collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_ident() {
        check("main", &[TokenKind::Ident]);
        check("LineageProof", &[TokenKind::Ident]);
        check("hello_there_42", &[TokenKind::Ident]);
        check("_hello", &[TokenKind::Ident]);
        check("_", &[TokenKind::Ident]);
    }

    #[test]
    fn test_int() {
        check("0", &[TokenKind::Int]);
        check("42", &[TokenKind::Int]);
    }

    #[test]
    fn test_keyword() {
        check("fun", &[TokenKind::Fun]);
        check("if", &[TokenKind::If]);
        check("else", &[TokenKind::Else]);
    }

    #[test]
    fn test_brackets() {
        check("(", &[TokenKind::OpenParen]);
        check(")", &[TokenKind::CloseParen]);
        check("{", &[TokenKind::OpenBrace]);
        check("}", &[TokenKind::CloseBrace]);
    }

    #[test]
    fn test_punctuation() {
        check(",", &[TokenKind::Comma]);
        check(":", &[TokenKind::Colon]);
        check("->", &[TokenKind::Arrow]);
    }

    #[test]
    fn test_arithmetic() {
        check("+", &[TokenKind::Plus]);
        check("-", &[TokenKind::Minus]);
        check("*", &[TokenKind::Star]);
        check("/", &[TokenKind::Slash]);
    }

    #[test]
    fn test_comparison() {
        check("<", &[TokenKind::LessThan]);
        check(">", &[TokenKind::GreaterThan]);
        check("==", &[TokenKind::Equals]);
    }

    #[test]
    fn test_whitespace() {
        check(" ", &[TokenKind::Whitespace]);
        check("    ", &[TokenKind::Whitespace]);
        check("\t", &[TokenKind::Whitespace]);
        check("\n", &[TokenKind::Whitespace]);
        check("\r", &[TokenKind::Whitespace]);
        check("\r\n", &[TokenKind::Whitespace]);
    }

    #[test]
    fn test_line_comment() {
        check("//", &[TokenKind::LineComment]);
        check("// hello", &[TokenKind::LineComment]);
        check("// hello\n", &[TokenKind::LineComment]);
    }

    #[test]
    fn test_block_comment() {
        check("/*", &[TokenKind::BlockComment]);
        check("/* hello */", &[TokenKind::BlockComment]);
        check("/* hello\nworld */", &[TokenKind::BlockComment]);
        check(
            "/* hello *//* good bye */",
            &[TokenKind::BlockComment, TokenKind::BlockComment],
        );
    }
}
