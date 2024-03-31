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
            '/' => match self.peek() {
                '/' => {
                    self.bump();
                    while self.peek() != '\n' {
                        self.bump();
                    }
                    TokenKind::LineComment
                }
                '*' => {
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
}
