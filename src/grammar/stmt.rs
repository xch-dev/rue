use crate::{
    Parser, SyntaxKind, T,
    grammar::{expr::expr, ty::ty},
};

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatementKind {
    Normal,
    End,
}

pub fn stmt(p: &mut Parser<'_>) -> StatementKind {
    if p.at(T![let]) {
        let_stmt(p);
    } else {
        expr(p);
        if !p.try_eat(T![;]) {
            return StatementKind::End;
        }
    }
    StatementKind::Normal
}

fn let_stmt(p: &mut Parser<'_>) {
    p.start(SyntaxKind::LetStmt);
    p.expect(T![let]);
    p.expect(SyntaxKind::Ident);
    p.expect(T![:]);
    ty(p);
    p.expect(T![=]);
    expr(p);
    p.expect(T![;]);
    p.finish();
}
