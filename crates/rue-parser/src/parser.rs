use std::{cmp::Reverse, mem, ops::Range};

use indexmap::IndexSet;
use itertools::Itertools;
use rowan::{Checkpoint, GreenNodeBuilder, Language};
use rue_diagnostic::{Diagnostic, DiagnosticKind, Source, SrcLoc};
use rue_lexer::{Token, TokenKind};

use crate::{RueLang, SyntaxKind, SyntaxNode, T, document};

#[derive(Debug)]
struct ParseToken {
    span: Range<usize>,
    kind: SyntaxKind,
}

#[derive(Debug, Clone)]
pub struct ParseResult {
    pub diagnostics: Vec<Diagnostic>,
    pub node: SyntaxNode,
}

#[derive(Debug)]
pub struct Parser {
    source: Source,
    parse_tokens: Vec<ParseToken>,
    pos: usize,
    expected: IndexSet<SyntaxKind>,
    diagnostics: Vec<Diagnostic>,
    builder: GreenNodeBuilder<'static>,
}

impl Parser {
    pub fn new(source: Source, tokens: Vec<Token>) -> Self {
        let mut parse_tokens = Vec::with_capacity(tokens.len());
        let mut diagnostics = Vec::new();

        for token in tokens {
            let kind = match token.kind {
                TokenKind::Whitespace => SyntaxKind::Whitespace,
                TokenKind::LineComment => SyntaxKind::LineComment,
                TokenKind::BlockComment { is_terminated } => {
                    if !is_terminated {
                        diagnostics.push(Diagnostic::new(
                            SrcLoc::new(source.clone(), token.span.clone()),
                            DiagnosticKind::UnterminatedBlockComment,
                        ));
                    }
                    SyntaxKind::BlockComment
                }
                TokenKind::String { is_terminated } => {
                    if !is_terminated {
                        diagnostics.push(Diagnostic::new(
                            SrcLoc::new(source.clone(), token.span.clone()),
                            DiagnosticKind::UnterminatedString,
                        ));
                    }
                    SyntaxKind::String
                }
                TokenKind::Hex { is_terminated } => {
                    if !is_terminated {
                        diagnostics.push(Diagnostic::new(
                            SrcLoc::new(source.clone(), token.span.clone()),
                            DiagnosticKind::UnterminatedHex,
                        ));
                    }
                    SyntaxKind::Hex
                }
                TokenKind::Integer => SyntaxKind::Integer,
                TokenKind::Ident => SyntaxKind::Ident,
                TokenKind::Nil => T![nil],
                TokenKind::True => T![true],
                TokenKind::False => T![false],
                TokenKind::Export => T![export],
                TokenKind::Extern => T![extern],
                TokenKind::Inline => T![inline],
                TokenKind::Test => T![test],
                TokenKind::Mod => T![mod],
                TokenKind::Fn => T![fn],
                TokenKind::Const => T![const],
                TokenKind::Type => T![type],
                TokenKind::Struct => T![struct],
                TokenKind::Let => T![let],
                TokenKind::If => T![if],
                TokenKind::Else => T![else],
                TokenKind::Return => T![return],
                TokenKind::Assert => T![assert],
                TokenKind::Raise => T![raise],
                TokenKind::Is => T![is],
                TokenKind::As => T![as],
                TokenKind::OpenParen => T!['('],
                TokenKind::CloseParen => T![')'],
                TokenKind::OpenBrace => T!['{'],
                TokenKind::CloseBrace => T!['}'],
                TokenKind::OpenBracket => T!['['],
                TokenKind::CloseBracket => T![']'],
                TokenKind::Plus => T![+],
                TokenKind::Minus => T![-],
                TokenKind::Star => T![*],
                TokenKind::Slash => T![/],
                TokenKind::Percent => T![%],
                TokenKind::Equals => T![=],
                TokenKind::LessThan => T![<],
                TokenKind::GreaterThan => T![>],
                TokenKind::Not => T![!],
                TokenKind::And => T![&],
                TokenKind::Or => T![|],
                TokenKind::Tilde => T![~],
                TokenKind::Xor => T![^],
                TokenKind::Dot => T![.],
                TokenKind::Comma => T![,],
                TokenKind::Colon => T![:],
                TokenKind::Semicolon => T![;],
                TokenKind::Unknown => {
                    diagnostics.push(Diagnostic::new(
                        SrcLoc::new(source.clone(), token.span.clone()),
                        DiagnosticKind::UnknownToken(source.text[token.span.clone()].to_string()),
                    ));
                    SyntaxKind::Error
                }
            };

            parse_tokens.push(ParseToken {
                span: token.span,
                kind,
            });
        }

        Self {
            source,
            parse_tokens,
            pos: 0,
            expected: IndexSet::new(),
            diagnostics,
            builder: GreenNodeBuilder::new(),
        }
    }

    pub fn parse(mut self) -> ParseResult {
        document(&mut self);
        ParseResult {
            diagnostics: self.diagnostics,
            node: SyntaxNode::new_root(self.builder.finish()),
        }
    }

    #[cfg(test)]
    pub(crate) fn parse_raw(self) -> ParseResult {
        ParseResult {
            diagnostics: self.diagnostics,
            node: SyntaxNode::new_root(self.builder.finish()),
        }
    }

    pub(crate) fn checkpoint(&mut self) -> Checkpoint {
        self.eat_trivia();
        self.builder.checkpoint()
    }

    pub(crate) fn start_including_trivia(&mut self, kind: SyntaxKind) {
        self.builder.start_node(RueLang::kind_to_raw(kind));
    }

    pub(crate) fn start(&mut self, kind: SyntaxKind) {
        self.eat_trivia();
        self.builder.start_node(RueLang::kind_to_raw(kind));
    }

    pub(crate) fn start_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.builder
            .start_node_at(checkpoint, RueLang::kind_to_raw(kind));
    }

    pub(crate) fn finish(&mut self) {
        self.builder.finish_node();
    }

    pub(crate) fn at_any(&mut self, kinds: &[SyntaxKind]) -> Option<SyntaxKind> {
        for kind in kinds
            .iter()
            .sorted_by_key(|kind| Reverse(kind.split().len()))
        {
            if self.at(*kind) {
                return Some(*kind);
            }
        }
        None
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.eat_trivia();

        self.expected.insert(kind);

        let split = kind.split();

        for (i, kind) in split.iter().enumerate() {
            if self.nth(i) != *kind {
                return false;
            }
        }

        true
    }

    pub(crate) fn try_eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump(kind);
            true
        } else {
            false
        }
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) {
        if self.at(kind) {
            self.bump(kind);
        } else {
            self.skip();
        }
    }

    fn nth(&self, n: usize) -> SyntaxKind {
        self.parse_tokens
            .get(self.pos + n)
            .map_or(SyntaxKind::Eof, |token| token.kind)
    }

    pub(crate) fn eat_trivia(&mut self) {
        while self.nth(0).is_trivia() {
            self.bump(self.nth(0));
        }
    }

    pub(crate) fn skip(&mut self) {
        let expected = mem::take(&mut self.expected);

        let len = self.source.text.len();

        let span = self
            .parse_tokens
            .get(self.pos)
            .map_or(len..len, |token| token.span.clone());

        self.diagnostics.push(Diagnostic::new(
            SrcLoc::new(self.source.clone(), span),
            DiagnosticKind::UnexpectedToken(
                self.nth(0).to_string(),
                expected.iter().map(ToString::to_string).collect(),
            ),
        ));

        if self.pos < self.parse_tokens.len() {
            self.pos += 1;
        }
    }

    fn bump(&mut self, kind: SyntaxKind) {
        self.expected.clear();

        let len = kind.split().len();

        let span =
            self.parse_tokens[self.pos].span.start..self.parse_tokens[self.pos + len - 1].span.end;

        self.builder
            .token(RueLang::kind_to_raw(kind), &self.source.text[span]);

        self.pos += len;
    }
}
