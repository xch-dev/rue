use std::ops::Range;

use crate::{Error, ErrorKind, SyntaxKind, T, Token, TokenKind};

#[derive(Debug, Clone)]
struct ParseToken {
    span: Range<usize>,
    kind: SyntaxKind,
}

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    source: &'a str,
    parse_tokens: Vec<ParseToken>,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: Vec<Token>) -> Self {
        let mut parse_tokens = Vec::with_capacity(tokens.len());
        let mut errors = Vec::new();

        for token in tokens {
            let kind = match token.kind {
                TokenKind::Whitespace => SyntaxKind::Whitespace,
                TokenKind::LineComment => SyntaxKind::LineComment,
                TokenKind::BlockComment { is_terminated } => {
                    if !is_terminated {
                        errors.push(Error::new(
                            token.span.clone(),
                            ErrorKind::UnterminatedBlockComment,
                        ));
                    }
                    SyntaxKind::BlockComment
                }
                TokenKind::String { is_terminated } => {
                    if !is_terminated {
                        errors.push(Error::new(
                            token.span.clone(),
                            ErrorKind::UnterminatedString,
                        ));
                    }
                    SyntaxKind::String
                }
                TokenKind::Bytes { is_terminated } => {
                    if !is_terminated {
                        errors.push(Error::new(token.span.clone(), ErrorKind::UnterminatedBytes));
                    }
                    SyntaxKind::Bytes
                }
                TokenKind::Integer => SyntaxKind::Integer,
                TokenKind::Ident => SyntaxKind::Ident,
                TokenKind::Nil => SyntaxKind::Nil,
                TokenKind::True => SyntaxKind::True,
                TokenKind::False => SyntaxKind::False,
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
                    errors.push(Error::new(
                        token.span.clone(),
                        ErrorKind::UnknownToken(source[token.span.clone()].to_string()),
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
            errors,
        }
    }
}
