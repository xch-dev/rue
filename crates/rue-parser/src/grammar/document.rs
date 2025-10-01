use crate::{Parser, SyntaxKind, grammar::item::item};

pub fn document(p: &mut Parser) {
    p.start_including_trivia(SyntaxKind::Document);
    while !p.at(SyntaxKind::Eof) {
        item(p);
    }
    p.eat_trivia();
    p.finish();
}
