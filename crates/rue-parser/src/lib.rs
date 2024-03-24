mod ast;
mod grammar;
mod language;
mod parser;
mod syntax_kind;

pub use ast::*;
pub use language::*;
pub use syntax_kind::*;

pub fn parse(source: &str) -> Root {
    let lexer = rue_lexer::Lexer::new(source);
    let tokens: Vec<rue_lexer::Token> = lexer.collect();
    let mut parser = parser::Parser::new(source, &tokens);
    grammar::root(&mut parser);
    Root::cast(parser.build()).unwrap()
}
