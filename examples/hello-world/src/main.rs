use rue_parser::{parse, AstNode};

fn main() {
    let source = include_str!("../hello.rue");
    let cst = parse(source);
    println!("{:#?}", cst.syntax());
}
