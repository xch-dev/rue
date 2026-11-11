//! Canonical source formatter for Rue.
//!
//! The formatter uses Rue's typed AST for syntax structure and its lossless
//! rowan tree for source-ordered tokens and comments. Defaults mirror rustfmt:
//! a 100-column target, four-space indentation, LF line endings, no trailing
//! whitespace, at most one blank line, and exactly one final newline.
//!
//! Range formatting, malformed-tree formatting, and comment reflow are
//! intentionally deferred.

mod analysis;
mod document;
mod emit;
mod equivalence;
mod format;
mod ordering;
mod renderer;
mod token_stream;
mod trivia;

use std::sync::Arc;

use rue_ast::{AstDocument, AstNode};
use rue_diagnostic::{Diagnostic, Source, SourceKind};
use rue_lexer::Lexer;
use rue_parser::{Parser, SyntaxKind, SyntaxNode};
use thiserror::Error;

pub use rue_options::FormatOptions;

use crate::{
    equivalence::comment_signature, format::format_document, renderer::render,
    token_stream::TokenStream,
};

/// A failure that prevents a safe formatting result.
#[derive(Debug, Error)]
pub enum FormatError {
    /// The parser reported malformed source. No replacement text is returned.
    #[error("source contains syntax errors")]
    Parse {
        /// Original parser diagnostics.
        diagnostics: Vec<Diagnostic>,
    },
    /// The lossless tree contains an unsupported trivia token.
    #[error("unsupported trivia kind: {0}")]
    UnsupportedTrivia(SyntaxKind),
    /// Lossless CST tokens were not encountered in source order.
    #[error("token order violation: token at {next_start} follows end offset {previous_end}")]
    TokenOrder {
        /// End offset of the previous token.
        previous_end: usize,
        /// Start offset of the next token.
        next_start: usize,
    },
    /// A formatter invariant failed.
    #[error("formatter invariant failed: {0}")]
    Internal(String),
}

/// Format a complete Rue source file.
///
/// Malformed input is rejected. A successful result has also been reparsed,
/// checked for syntax and comment equivalence, and formatted a second time to
/// prove idempotency.
pub fn format_source(source: &str, options: &FormatOptions) -> Result<String, FormatError> {
    let input = parse(source)?;
    let formatted = format_once(&input, options)?;
    let output = parse(&formatted)?;

    verify_equivalence(&input, &output)?;

    let second = format_once(&output, options)?;
    if second != formatted {
        return Err(FormatError::Internal(format!(
            "formatting was not idempotent\nfirst:\n{formatted}\nsecond:\n{second}"
        )));
    }

    Ok(formatted)
}

fn format_once(parsed: &Parsed, options: &FormatOptions) -> Result<String, FormatError> {
    let stream = TokenStream::from_syntax(parsed.document.syntax())?;
    let doc = format_document(&parsed.document, &stream)?;
    Ok(render(&doc, options))
}

#[derive(Debug)]
struct Parsed {
    document: AstDocument,
}

fn parse(text: &str) -> Result<Parsed, FormatError> {
    // Rue line-comment tokens include their terminating newline. Supplying one
    // here also lets the current lexer represent a line comment at physical EOF.
    let parse_text = if text.ends_with('\n') {
        text.to_string()
    } else {
        format!("{text}\n")
    };
    let source = Source::new(
        Arc::from(parse_text.as_str()),
        SourceKind::File("<formatter>".to_string()),
    );
    let result = Parser::new(source, Lexer::new(&parse_text).collect()).parse();
    if !result.diagnostics.is_empty() {
        return Err(FormatError::Parse {
            diagnostics: result.diagnostics,
        });
    }
    let document = AstDocument::cast(result.node)
        .ok_or_else(|| FormatError::Internal("parser root was not an AstDocument".to_string()))?;
    Ok(Parsed { document })
}

fn verify_equivalence(before: &Parsed, after: &Parsed) -> Result<(), FormatError> {
    if structural_signature(before.document.syntax())
        != structural_signature(after.document.syntax())
    {
        return Err(FormatError::Internal(
            "formatted source changed syntax".to_string(),
        ));
    }

    let before_stream = TokenStream::from_syntax(before.document.syntax())?;
    let after_stream = TokenStream::from_syntax(after.document.syntax())?;
    let before_comments = comment_signature(&before.document, &before_stream)?;
    let after_comments = comment_signature(&after.document, &after_stream)?;
    if before_comments != after_comments {
        return Err(FormatError::Internal(format!(
            "formatted source changed or duplicated comments\nbefore: {before_comments:#?}\nafter: {after_comments:#?}"
        )));
    }
    Ok(())
}

fn structural_signature(root: &SyntaxNode) -> Vec<String> {
    let mut signature = Vec::new();
    append_node_signature(root, &mut signature);
    signature
}

fn append_node_signature(node: &SyntaxNode, signature: &mut Vec<String>) {
    signature.push(format!("n:{:?}", node.kind()));

    if node.kind() == SyntaxKind::Document {
        let mut imports = Vec::new();
        let mut items = Vec::new();
        for child in node.children() {
            let mut child_signature = Vec::new();
            append_node_signature(&child, &mut child_signature);
            if child.kind() == SyntaxKind::ImportItem {
                imports.push(child_signature);
            } else {
                items.push(child_signature);
            }
        }
        imports.sort();
        signature.extend(imports.into_iter().flatten());
        signature.extend(items.into_iter().flatten());
        return;
    }

    let mut sorted_import_paths = if node.kind() == SyntaxKind::ImportPathSegment {
        let mut paths: Vec<_> = node
            .children()
            .filter(|child| child.kind() == SyntaxKind::ImportPath)
            .map(|child| {
                let mut child_signature = Vec::new();
                append_node_signature(&child, &mut child_signature);
                child_signature
            })
            .collect();
        paths.sort();
        paths.into_iter()
    } else {
        Vec::new().into_iter()
    };

    for element in node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Node(child)
                if node.kind() == SyntaxKind::ImportPathSegment
                    && child.kind() == SyntaxKind::ImportPath =>
            {
                signature.extend(
                    sorted_import_paths
                        .next()
                        .expect("each import path has a sorted signature"),
                );
            }
            rowan::NodeOrToken::Node(child) => append_node_signature(&child, signature),
            rowan::NodeOrToken::Token(token)
                if token.kind() == rue_parser::T![,] && is_optional_trailing_comma(&token) => {}
            rowan::NodeOrToken::Token(token) if !token.kind().is_trivia() => {
                signature.push(format!("t:{:?}:{}", token.kind(), token.text()));
            }
            rowan::NodeOrToken::Token(_) => {}
        }
    }
}

fn is_optional_trailing_comma(token: &rue_parser::SyntaxToken) -> bool {
    let mut next = token.next_token();
    while next.as_ref().is_some_and(|token| token.kind().is_trivia()) {
        next = next.and_then(|token| token.next_token());
    }
    if !next.is_some_and(|token| {
        matches!(
            token.kind(),
            rue_parser::T![')'] | rue_parser::T![']'] | rue_parser::T!['}'] | rue_parser::T![>]
        )
    }) {
        return false;
    }

    token.parent_ancestors().any(|node| {
        matches!(
            node.kind(),
            SyntaxKind::Attribute
                | SyntaxKind::FunctionItem
                | SyntaxKind::FunctionCallExpr
                | SyntaxKind::LambdaExpr
                | SyntaxKind::LambdaType
                | SyntaxKind::PairExpr
                | SyntaxKind::PairType
                | SyntaxKind::PairBinding
                | SyntaxKind::ListExpr
                | SyntaxKind::ListType
                | SyntaxKind::ListBinding
                | SyntaxKind::StructItem
                | SyntaxKind::StructInitializerExpr
                | SyntaxKind::StructBinding
                | SyntaxKind::ImportPathSegment
                | SyntaxKind::GenericParameters
                | SyntaxKind::GenericArguments
        )
    })
}

#[cfg(test)]
mod tests;
