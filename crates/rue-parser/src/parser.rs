use std::{mem, ops::Range};

use indexmap::IndexSet;
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
    expected: IndexSet<String>,
    last_error_pos: Option<usize>,
    diagnostics: Vec<Diagnostic>,
    builder: GreenNodeBuilder<'static>,
}

impl Parser {
    pub fn new(source: Source, tokens: Vec<Token>) -> Self {
        let mut parse_tokens = Vec::with_capacity(tokens.len());
        let mut diagnostics = Vec::new();

        // Unterminated strings and block comments extend to the end of the file, so any syntax
        // errors at the end of the file are caused by them and aren't worth reporting
        let mut unterminated_at_eof = false;

        for token in tokens {
            let kind = match token.kind {
                TokenKind::Whitespace => SyntaxKind::Whitespace,
                TokenKind::LineComment => SyntaxKind::LineComment,
                TokenKind::BlockComment { is_terminated } => {
                    if !is_terminated {
                        unterminated_at_eof = true;
                        diagnostics.push(Diagnostic::new(
                            SrcLoc::new(source.clone(), token.span.clone()),
                            DiagnosticKind::UnterminatedBlockComment,
                        ));
                    }
                    SyntaxKind::BlockComment
                }
                TokenKind::String { is_terminated } => {
                    if !is_terminated {
                        unterminated_at_eof = true;
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
                TokenKind::Binary { is_terminated } => {
                    if !is_terminated {
                        diagnostics.push(Diagnostic::new(
                            SrcLoc::new(source.clone(), token.span.clone()),
                            DiagnosticKind::UnterminatedBinary,
                        ));
                    }
                    SyntaxKind::Binary
                }
                TokenKind::Octal { is_terminated } => {
                    if !is_terminated {
                        diagnostics.push(Diagnostic::new(
                            SrcLoc::new(source.clone(), token.span.clone()),
                            DiagnosticKind::UnterminatedOctal,
                        ));
                    }
                    SyntaxKind::Octal
                }
                TokenKind::Integer => SyntaxKind::Integer,
                TokenKind::Ident => SyntaxKind::Ident,
                TokenKind::Nil => T![nil],
                TokenKind::True => T![true],
                TokenKind::False => T![false],
                TokenKind::Import => T![import],
                TokenKind::Export => T![export],
                TokenKind::Extern => T![extern],
                TokenKind::From => T![from],
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
                TokenKind::Debug => T![debug],
                TokenKind::Is => T![is],
                TokenKind::As => T![as],
                TokenKind::Super => T![super],
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

        let last_error_pos = unterminated_at_eof.then_some(parse_tokens.len());

        Self {
            source,
            parse_tokens,
            pos: 0,
            expected: IndexSet::new(),
            last_error_pos,
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

    /// The index of the current token, which can be compared to detect whether progress was made.
    pub(crate) fn pos(&mut self) -> usize {
        self.eat_trivia();
        self.pos
    }

    pub(crate) fn at_any(&mut self, kinds: &[SyntaxKind]) -> Option<SyntaxKind> {
        self.at_any_inner(kinds, true)
    }

    /// Like [`Self::at_any`], but doesn't add the kinds to the expected set for diagnostics.
    pub(crate) fn at_any_quiet(&mut self, kinds: &[SyntaxKind]) -> Option<SyntaxKind> {
        self.at_any_inner(kinds, false)
    }

    /// Prefers the longest match, so that for example `>>` is matched rather than `>`.
    fn at_any_inner(&mut self, kinds: &[SyntaxKind], record: bool) -> Option<SyntaxKind> {
        let mut found: Option<SyntaxKind> = None;

        for &kind in kinds {
            if self.at_inner(kind, record)
                && found.is_none_or(|found| kind.split().len() > found.split().len())
            {
                found = Some(kind);
            }
        }

        found
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.at_inner(kind, true)
    }

    /// Like [`Self::at`], but doesn't add the kind to the expected set for diagnostics.
    pub(crate) fn at_quiet(&mut self, kind: SyntaxKind) -> bool {
        self.at_inner(kind, false)
    }

    fn at_inner(&mut self, kind: SyntaxKind, record: bool) -> bool {
        self.eat_trivia();

        if record {
            self.expected.insert(kind.to_string());
        }

        let split = kind.split();

        for (i, kind) in split.iter().enumerate() {
            if self.nth(i) != *kind {
                return false;
            }
        }

        true
    }

    /// Returns the kind of the `n`th non-trivia token after the current position.
    pub(crate) fn lookahead(&self, n: usize) -> SyntaxKind {
        let mut remaining = n;
        let mut i = 0;

        loop {
            let kind = self.nth(i);

            if kind == SyntaxKind::Eof {
                return kind;
            }

            if !kind.is_trivia() {
                if remaining == 0 {
                    return kind;
                }
                remaining -= 1;
            }

            i += 1;
        }
    }

    /// Whether the current token is the first one on its line (ignoring trivia).
    pub(crate) fn at_line_start(&mut self) -> bool {
        self.eat_trivia();

        self.parse_tokens[..self.pos]
            .iter()
            .rev()
            .take_while(|token| token.kind.is_trivia())
            .any(|token| self.source.text[token.span.clone()].contains('\n'))
    }

    /// Returns the kind of the first non-trivia token after the delimited group (such as `{...}`)
    /// that starts at the current token, or the current token's kind if it doesn't start a group.
    pub(crate) fn lookahead_past_group(&self) -> SyntaxKind {
        let mut depth = 0usize;
        let mut n = 0;

        loop {
            let kind = self.lookahead(n);

            match kind {
                T!['('] | T!['['] | T!['{'] => depth += 1,
                T![')'] | T![']'] | T!['}'] => depth = depth.saturating_sub(1),
                SyntaxKind::Eof => return kind,
                _ => {}
            }

            n += 1;

            if depth == 0 {
                return self.lookahead(n);
            }
        }
    }

    pub(crate) fn try_eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump(kind);
            true
        } else {
            false
        }
    }

    /// Consumes the token if it's present, otherwise reports an error without consuming anything.
    /// This allows the caller to continue parsing as though the token was there, which prevents a
    /// single missing token from swallowing the next valid construct.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump(kind);
            true
        } else {
            self.error_unexpected();
            false
        }
    }

    /// Adds a description of what was expected (such as "expression") to the set of expected
    /// items that will be included in the next diagnostic.
    pub(crate) fn expected(&mut self, name: &str) {
        self.expected.insert(name.to_string());
    }

    pub(crate) fn clear_expected(&mut self) {
        self.expected.clear();
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

    /// Reports an unexpected token error at the current token, listing what was expected.
    pub(crate) fn error_unexpected(&mut self) {
        self.eat_trivia();
        let expected = mem::take(&mut self.expected);
        let found = self.nth(0).to_string();
        self.push_error(DiagnosticKind::UnexpectedToken(
            found,
            expected.into_iter().collect(),
        ));
    }

    pub(crate) fn error(&mut self, kind: DiagnosticKind) {
        self.eat_trivia();
        self.expected.clear();
        self.push_error(kind);
    }

    /// Only one syntax error is reported per token, since once there's an error at a given
    /// position any subsequent errors at the same position are just noise.
    fn push_error(&mut self, kind: DiagnosticKind) {
        if self.last_error_pos == Some(self.pos) {
            return;
        }

        self.last_error_pos = Some(self.pos);

        let len = self.source.text.len();

        let span = self
            .parse_tokens
            .get(self.pos)
            .map_or(len..len, |token| token.span.clone());

        self.diagnostics.push(Diagnostic::new(
            SrcLoc::new(self.source.clone(), span),
            kind,
        ));
    }

    /// Consumes the current token as an error token, so that it remains in the syntax tree. Does
    /// nothing at the end of the file.
    pub(crate) fn bump_as_error(&mut self) {
        self.eat_trivia();

        let Some(token) = self.parse_tokens.get(self.pos) else {
            return;
        };

        self.expected.clear();

        self.builder.token(
            RueLang::kind_to_raw(SyntaxKind::Error),
            &self.source.text[token.span.clone()],
        );

        self.pos += 1;
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
