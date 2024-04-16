use indexmap::IndexSet;
use rowan::{Checkpoint, GreenNodeBuilder, Language};
use rue_lexer::{Token, TokenKind};

use crate::{ParserError, ParserErrorKind, RueLang, SyntaxKind, SyntaxNode};

pub struct Parser<'a> {
    items: Vec<(SyntaxKind, &'a str)>,
    cursor: usize,
    char_pos: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<ParserError>,
    expected_kinds: IndexSet<SyntaxKind>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &[Token]) -> Self {
        let mut errors = Vec::new();
        let items = convert_tokens(&mut errors, source, tokens);

        Self {
            items,
            cursor: 0,
            char_pos: 0,
            builder: GreenNodeBuilder::new(),
            errors,
            expected_kinds: IndexSet::new(),
        }
    }

    pub fn build(self) -> (SyntaxNode, Vec<ParserError>) {
        (SyntaxNode::new_root(self.builder.finish()), self.errors)
    }

    pub fn start(&mut self, kind: SyntaxKind) {
        self.builder.start_node(RueLang::kind_to_raw(kind));
    }

    pub fn start_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, RueLang::kind_to_raw(kind));
    }

    pub fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }

    pub fn finish(&mut self) {
        self.eat_whitespace();
        self.builder.finish_node();
    }

    pub fn try_eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            let found = self.nth(0);
            self.push_error(ParserErrorKind::UnexpectedToken {
                expected: vec![kind],
                found,
            });
            self.expected_kinds.clear();
            false
        }
    }

    pub fn bump(&mut self) -> SyntaxKind {
        self.expected_kinds.clear();
        let kind = self.nth(0);
        self.token();
        kind
    }

    pub fn at(&mut self, kind: SyntaxKind) -> bool {
        self.expected_kinds.insert(kind);
        self.nth(0) == kind
    }

    pub fn error(&mut self, set: &[SyntaxKind]) {
        let expected: Vec<SyntaxKind> = self.expected_kinds.drain(..).collect();
        let found = self.nth(0);

        self.push_error(ParserErrorKind::UnexpectedToken { expected, found });

        if self.at_end() || set.contains(&found) {
            return;
        }

        self.start(SyntaxKind::Error);
        self.token();
        self.finish();
    }

    fn push_error(&mut self, error: ParserErrorKind) {
        if self.cursor == self.items.len() {
            self.errors
                .push(ParserError::new(error, self.char_pos..self.char_pos));
            return;
        }
        let range = self.char_pos..self.char_pos + self.items[self.cursor].1.len();
        self.errors.push(ParserError::new(error, range));
    }

    fn nth(&mut self, index: usize) -> SyntaxKind {
        self.eat_whitespace();
        self.items
            .get(self.cursor + index)
            .map(|(kind, _)| *kind)
            .unwrap_or(SyntaxKind::Eof)
    }

    fn eat_whitespace(&mut self) {
        while !self.at_end() {
            if matches!(
                self.items[self.cursor].0,
                SyntaxKind::Whitespace
                    | SyntaxKind::LineComment
                    | SyntaxKind::BlockComment
                    | SyntaxKind::Error
            ) {
                self.token();
            } else {
                break;
            }
        }
    }

    fn token(&mut self) {
        if self.at_end() {
            return;
        }

        let (kind, text) = self.items[self.cursor];

        self.cursor += 1;
        self.char_pos += text.len();
        self.builder.token(RueLang::kind_to_raw(kind), text);
    }

    fn at_end(&mut self) -> bool {
        self.cursor >= self.items.len()
    }
}

fn convert_tokens<'a>(
    errors: &mut Vec<ParserError>,
    source: &'a str,
    tokens: &[Token],
) -> Vec<(SyntaxKind, &'a str)> {
    let mut items = Vec::new();
    let mut pos = 0;

    for token in tokens {
        let text = &source[pos..pos + token.len()];
        let kind = match token.kind() {
            TokenKind::Ident => SyntaxKind::Ident,
            TokenKind::Int => SyntaxKind::Int,
            TokenKind::String { is_terminated } => {
                if !is_terminated {
                    errors.push(ParserError::new(
                        ParserErrorKind::UnterminatedString,
                        pos..pos + token.len(),
                    ));
                }
                SyntaxKind::String
            }

            TokenKind::OpenParen => SyntaxKind::OpenParen,
            TokenKind::CloseParen => SyntaxKind::CloseParen,
            TokenKind::OpenBracket => SyntaxKind::OpenBracket,
            TokenKind::CloseBracket => SyntaxKind::CloseBracket,
            TokenKind::OpenBrace => SyntaxKind::OpenBrace,
            TokenKind::CloseBrace => SyntaxKind::CloseBrace,

            TokenKind::Fun => SyntaxKind::Fun,
            TokenKind::Type => SyntaxKind::Type,
            TokenKind::Struct => SyntaxKind::Struct,
            TokenKind::Enum => SyntaxKind::Enum,
            TokenKind::Let => SyntaxKind::Let,
            TokenKind::Const => SyntaxKind::Const,
            TokenKind::If => SyntaxKind::If,
            TokenKind::Else => SyntaxKind::Else,
            TokenKind::Return => SyntaxKind::Return,
            TokenKind::Raise => SyntaxKind::Raise,
            TokenKind::Assert => SyntaxKind::Assert,
            TokenKind::Nil => SyntaxKind::Nil,
            TokenKind::True => SyntaxKind::True,
            TokenKind::False => SyntaxKind::False,
            TokenKind::As => SyntaxKind::As,
            TokenKind::Is => SyntaxKind::Is,

            TokenKind::Dot => SyntaxKind::Dot,
            TokenKind::Comma => SyntaxKind::Comma,
            TokenKind::Colon => SyntaxKind::Colon,
            TokenKind::PathSeparator => SyntaxKind::PathSeparator,
            TokenKind::Semicolon => SyntaxKind::Semicolon,
            TokenKind::Arrow => SyntaxKind::Arrow,
            TokenKind::FatArrow => SyntaxKind::FatArrow,
            TokenKind::Spread => SyntaxKind::Spread,

            TokenKind::Plus => SyntaxKind::Plus,
            TokenKind::Minus => SyntaxKind::Minus,
            TokenKind::Star => SyntaxKind::Star,
            TokenKind::Slash => SyntaxKind::Slash,
            TokenKind::Percent => SyntaxKind::Percent,
            TokenKind::Not => SyntaxKind::Not,
            TokenKind::LessThan => SyntaxKind::LessThan,
            TokenKind::GreaterThan => SyntaxKind::GreaterThan,
            TokenKind::LessThanEquals => SyntaxKind::LessThanEquals,
            TokenKind::GreaterThanEquals => SyntaxKind::GreaterThanEquals,
            TokenKind::Equals => SyntaxKind::Equals,
            TokenKind::NotEquals => SyntaxKind::NotEquals,
            TokenKind::Assign => SyntaxKind::Assign,

            TokenKind::Whitespace => SyntaxKind::Whitespace,
            TokenKind::LineComment => SyntaxKind::LineComment,
            TokenKind::BlockComment { is_terminated } => {
                if !is_terminated {
                    errors.push(ParserError::new(
                        ParserErrorKind::UnterminatedBlockComment,
                        pos..pos + token.len(),
                    ));
                }
                SyntaxKind::BlockComment
            }
            TokenKind::Unknown => {
                errors.push(ParserError::new(
                    ParserErrorKind::UnknownToken(text.to_string()),
                    pos..pos + token.len(),
                ));
                SyntaxKind::Error
            }
        };

        items.push((kind, text));
        pos += token.len();
    }

    items
}
