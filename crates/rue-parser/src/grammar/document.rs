use crate::{Parser, SyntaxKind, grammar::item::item};

pub fn document(p: &mut Parser) {
    p.start(SyntaxKind::Document);
    while !p.at(SyntaxKind::Eof) {
        item(p);
    }
    p.finish();
}
