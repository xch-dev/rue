mod ast;
mod error;
mod grammar;
mod language;
mod parser;
mod syntax_kind;

pub use ast::*;
pub use error::*;
pub use language::*;
pub use syntax_kind::*;

pub fn parse(source: &str) -> (Root, Vec<ParserError>) {
    let lexer = rue_lexer::Lexer::new(source);
    let tokens: Vec<rue_lexer::Token> = lexer.collect();
    let mut parser = parser::Parser::new(source, &tokens);
    grammar::root(&mut parser);
    let (ast, errors) = parser.build();
    (Root::cast(ast).unwrap(), errors)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    pub line: usize,
    pub col: usize,
}

/// Returns the line and column of the given index in the source.
/// Line and column numbers are from 0.
pub fn line_col(source: &str, index: usize) -> LineCol {
    let mut line = 0;
    let mut col = 0;

    for (i, character) in source.chars().enumerate() {
        if i == index {
            break;
        }

        if character == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    LineCol { line, col }
}
