use crate::{
    Parser, SyntaxKind, T,
    grammar::stmt::{StatementKind, stmt},
};

pub fn block(p: &mut Parser<'_>) {
    p.start(SyntaxKind::Block);
    p.expect(T!['{']);
    while !p.at(T!['}']) {
        if stmt(p) == StatementKind::End {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish();
}
