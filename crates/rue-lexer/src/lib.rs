mod token;
mod token_kind;

use std::str::Chars;

pub use token::*;
pub use token_kind::*;

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
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
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
            '%' => TokenKind::Percent,
            '<' => match self.peek() {
                '=' => {
                    self.bump();
                    TokenKind::LessThanEquals
                }
                _ => TokenKind::LessThan,
            },
            '>' => match self.peek() {
                '=' => {
                    self.bump();
                    TokenKind::GreaterThanEquals
                }
                _ => TokenKind::GreaterThan,
            },
            '=' => match self.peek() {
                '=' => {
                    self.bump();
                    TokenKind::Equals
                }
                '>' => {
                    self.bump();
                    TokenKind::FatArrow
                }
                _ => TokenKind::Assign,
            },
            '!' => match self.peek() {
                '=' => {
                    self.bump();
                    TokenKind::NotEquals
                }
                _ => TokenKind::Not,
            },
            '/' => match self.peek() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => TokenKind::Slash,
            },
            '&' => match self.peek() {
                '&' => {
                    self.bump();
                    TokenKind::And
                }
                _ => TokenKind::Unknown,
            },
            '|' => match self.peek() {
                '|' => {
                    self.bump();
                    TokenKind::Or
                }
                _ => TokenKind::Unknown,
            },
            '?' => TokenKind::Question,
            '.' => match self.peek() {
                '.' if self.peek_nth(1) == '.' => {
                    self.bump();
                    self.bump();
                    TokenKind::Spread
                }
                _ => TokenKind::Dot,
            },
            ':' => match self.peek() {
                ':' => {
                    self.bump();
                    TokenKind::PathSeparator
                }
                _ => TokenKind::Colon,
            },
            ';' => TokenKind::Semicolon,
            c @ ('"' | '\'') => self.string(c),
            c @ '0'..='9' => self.numeric(c == '0'),
            'a'..='z' | 'A'..='Z' | '_' => {
                while matches!(self.peek(), 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
                    self.bump();
                }
                match &self.source[start..self.pos] {
                    "fun" => TokenKind::Fun,
                    "inline" => TokenKind::Inline,
                    "import" => TokenKind::Import,
                    "export" => TokenKind::Export,
                    "type" => TokenKind::Type,
                    "struct" => TokenKind::Struct,
                    "enum" => TokenKind::Enum,
                    "let" => TokenKind::Let,
                    "const" => TokenKind::Const,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "return" => TokenKind::Return,
                    "raise" => TokenKind::Raise,
                    "assert" => TokenKind::Assert,
                    "assume" => TokenKind::Assume,
                    "nil" => TokenKind::Nil,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "as" => TokenKind::As,
                    "is" => TokenKind::Is,
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

    fn numeric(&mut self, zero: bool) -> TokenKind {
        if zero && matches!(self.peek(), 'x' | 'X') {
            self.hex_literal()
        } else {
            self.int_literal()
        }
    }

    fn hex_literal(&mut self) -> TokenKind {
        self.bump();
        let mut is_valid = false;
        while self.peek().is_ascii_hexdigit() || self.peek() == '_' {
            if self.bump().is_ascii_hexdigit() {
                is_valid = true;
            }
        }
        TokenKind::Hex { is_valid }
    }

    fn int_literal(&mut self) -> TokenKind {
        while matches!(self.peek(), '0'..='9' | '_') {
            self.bump();
        }
        TokenKind::Int
    }

    fn string(&mut self, quote: char) -> TokenKind {
        let is_terminated = loop {
            match self.bump() {
                c if c == quote => break true,
                '\0' => break false,
                _ => {}
            }
        };
        TokenKind::String { is_terminated }
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
        let mut depth = 1;
        let is_terminated = loop {
            match self.bump() {
                '*' => {
                    if self.peek() == '/' {
                        self.bump();
                        depth -= 1;
                        if depth == 0 {
                            break true;
                        }
                    }
                }
                '/' => {
                    if self.peek() == '*' {
                        self.bump();
                        depth += 1;
                    }
                }
                '\0' => break false,
                _ => {}
            }
        };
        TokenKind::BlockComment { is_terminated }
    }

    fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    fn peek_nth(&self, index: usize) -> char {
        self.chars.clone().nth(index).unwrap_or('\0')
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
    fn test_numeric() {
        check("0", &[TokenKind::Int]);
        check("42", &[TokenKind::Int]);
        check("0xFACE", &[TokenKind::Hex { is_valid: true }]);
        check("0xface", &[TokenKind::Hex { is_valid: true }]);
        check("0xFaCe", &[TokenKind::Hex { is_valid: true }]);
        check("0xF4c3", &[TokenKind::Hex { is_valid: true }]);
        check("0xf", &[TokenKind::Hex { is_valid: true }]);
        check("0x9", &[TokenKind::Hex { is_valid: true }]);
        check("0x0", &[TokenKind::Hex { is_valid: true }]);
        check(
            "0x9249104738faceecabcdef0123456789",
            &[TokenKind::Hex { is_valid: true }],
        );
        check("0x", &[TokenKind::Hex { is_valid: false }]);
        check("0x_", &[TokenKind::Hex { is_valid: false }]);
        check("0x_F_A_C_E", &[TokenKind::Hex { is_valid: true }]);
    }

    #[test]
    fn test_string() {
        check(
            "'abc'",
            &[TokenKind::String {
                is_terminated: true,
            }],
        );
        check(
            "'abc",
            &[TokenKind::String {
                is_terminated: false,
            }],
        );
        check(
            "\"abc\"",
            &[TokenKind::String {
                is_terminated: true,
            }],
        );
        check(
            "\"abc",
            &[TokenKind::String {
                is_terminated: false,
            }],
        );
    }

    #[test]
    fn test_keyword() {
        check("fun", &[TokenKind::Fun]);
        check("inline", &[TokenKind::Inline]);
        check("import", &[TokenKind::Import]);
        check("export", &[TokenKind::Export]);
        check("type", &[TokenKind::Type]);
        check("struct", &[TokenKind::Struct]);
        check("enum", &[TokenKind::Enum]);
        check("let", &[TokenKind::Let]);
        check("const", &[TokenKind::Const]);

        check("if", &[TokenKind::If]);
        check("else", &[TokenKind::Else]);
        check("return", &[TokenKind::Return]);
        check("raise", &[TokenKind::Raise]);
        check("assert", &[TokenKind::Assert]);
        check("assume", &[TokenKind::Assume]);
        check("true", &[TokenKind::True]);
        check("false", &[TokenKind::False]);
        check("nil", &[TokenKind::Nil]);
        check("as", &[TokenKind::As]);
        check("is", &[TokenKind::Is]);
    }

    #[test]
    fn test_brackets() {
        check("(", &[TokenKind::OpenParen]);
        check(")", &[TokenKind::CloseParen]);
        check("[", &[TokenKind::OpenBracket]);
        check("]", &[TokenKind::CloseBracket]);
        check("{", &[TokenKind::OpenBrace]);
        check("}", &[TokenKind::CloseBrace]);
    }

    #[test]
    fn test_punctuation() {
        check(".", &[TokenKind::Dot]);
        check(",", &[TokenKind::Comma]);
        check(":", &[TokenKind::Colon]);
        check("::", &[TokenKind::PathSeparator]);
        check(";", &[TokenKind::Semicolon]);
        check("->", &[TokenKind::Arrow]);
        check("=>", &[TokenKind::FatArrow]);
        check("...", &[TokenKind::Spread]);
        check("?", &[TokenKind::Question]);
    }

    #[test]
    fn test_arithmetic() {
        check("+", &[TokenKind::Plus]);
        check("-", &[TokenKind::Minus]);
        check("*", &[TokenKind::Star]);
        check("/", &[TokenKind::Slash]);
        check("%", &[TokenKind::Percent]);
    }

    #[test]
    fn test_comparison() {
        check("<", &[TokenKind::LessThan]);
        check(">", &[TokenKind::GreaterThan]);
        check("<=", &[TokenKind::LessThanEquals]);
        check(">=", &[TokenKind::GreaterThanEquals]);
        check("==", &[TokenKind::Equals]);
        check("!=", &[TokenKind::NotEquals]);
    }

    #[test]
    fn test_logical_ops() {
        check("!", &[TokenKind::Not]);
        check("&&", &[TokenKind::And]);
        check("||", &[TokenKind::Or]);
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
        check(
            "/*",
            &[TokenKind::BlockComment {
                is_terminated: false,
            }],
        );
        check(
            "/**/",
            &[TokenKind::BlockComment {
                is_terminated: true,
            }],
        );
        check(
            "/* hello */",
            &[TokenKind::BlockComment {
                is_terminated: true,
            }],
        );
        check(
            "/* hello\nworld */",
            &[TokenKind::BlockComment {
                is_terminated: true,
            }],
        );
        check(
            "/* hello *//* good bye */",
            &[
                TokenKind::BlockComment {
                    is_terminated: true,
                },
                TokenKind::BlockComment {
                    is_terminated: true,
                },
            ],
        );
        check(
            "/*/**/*/",
            &[TokenKind::BlockComment {
                is_terminated: true,
            }],
        );
    }
}
