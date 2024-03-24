use rue_parser::parse;

fn main() {
    let source = include_str!("../hello.rue");
    let cst = parse(source);
    println!("{:#?}", cst);
}
