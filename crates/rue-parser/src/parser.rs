use rowan::{GreenNodeBuilder, Language};
use rue_lexer::{Token, TokenKind};

use crate::{RueLang, SyntaxKind, SyntaxNode};

pub struct Parser<'a> {
    items: Vec<(SyntaxKind, &'a str)>,
    cursor: usize,
    builder: GreenNodeBuilder<'static>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &[Token]) -> Self {
        let mut items = Vec::new();
        let mut pos = 0;

        for token in tokens {
            let kind = match token.kind() {
                TokenKind::Ident => SyntaxKind::Ident,
                TokenKind::OpenParen => SyntaxKind::OpenParen,
                TokenKind::CloseParen => SyntaxKind::CloseParen,
                TokenKind::OpenBrace => SyntaxKind::OpenBrace,
                TokenKind::CloseBrace => SyntaxKind::CloseBrace,
                TokenKind::Comma => SyntaxKind::Comma,
                TokenKind::Colon => SyntaxKind::Colon,
                TokenKind::Arrow => SyntaxKind::Arrow,
                TokenKind::Int => SyntaxKind::Int,
                TokenKind::Fun => SyntaxKind::Fun,
                TokenKind::Whitespace => SyntaxKind::Whitespace,
                TokenKind::Unknown => SyntaxKind::Error,
            };

            items.push((kind, &source[pos..pos + token.len()]));
            pos += token.len();
        }

        Self {
            items,
            cursor: 0,
            builder: GreenNodeBuilder::new(),
        }
    }

    pub fn build(self) -> SyntaxNode {
        SyntaxNode::new_root(self.builder.finish())
    }

    pub fn start(&mut self, kind: SyntaxKind) {
        self.builder.start_node(RueLang::kind_to_raw(kind));
    }

    pub fn finish(&mut self) {
        self.eat_whitespace();
        self.builder.finish_node();
    }

    pub fn at_end(&self) -> bool {
        self.cursor >= self.items.len()
    }

    pub fn peek(&mut self) -> SyntaxKind {
        self.nth(0)
    }

    pub fn bump(&mut self) -> SyntaxKind {
        self.eat_whitespace();
        let kind = self.peek();
        self.token();
        kind
    }

    pub fn nth(&mut self, index: usize) -> SyntaxKind {
        self.eat_whitespace();
        self.items
            .get(self.cursor + index)
            .map(|(kind, _)| *kind)
            .unwrap_or(SyntaxKind::Eof)
    }

    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.peek() == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_whitespace(&mut self) {
        while !self.at_end() {
            if self.items[self.cursor].0 == SyntaxKind::Whitespace {
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
        self.builder.token(RueLang::kind_to_raw(kind), text);
    }
}
