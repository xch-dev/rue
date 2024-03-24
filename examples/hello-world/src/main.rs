use rue_lexer::{Lexer, Token};

fn main() {
    let source = include_str!("../hello.rue");
    let lexer = Lexer::new(source);
    let tokens: Vec<Token> = lexer.collect();
    dbg!(tokens);
}
