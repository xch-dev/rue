use crate::{Parser, SyntaxKind, T};

pub fn binding(p: &mut Parser) {
    if p.at(SyntaxKind::Ident) {
        p.start(SyntaxKind::NamedBinding);
        p.expect(SyntaxKind::Ident);
        p.finish();
    } else if p.at(T!['(']) {
        p.start(SyntaxKind::PairBinding);
        p.expect(T!['(']);
        binding(p);
        p.expect(T![,]);
        binding(p);
        p.try_eat(T![,]);
        p.expect(T![')']);
        p.finish();
    } else if p.at(T!['[']) {
        p.start(SyntaxKind::ListBinding);
        p.expect(T!['[']);
        while !p.at(T![']']) {
            p.start(SyntaxKind::ListBindingItem);
            p.try_eat(T![...]);
            binding(p);
            p.finish();
            if !p.try_eat(T![,]) {
                break;
            }
        }
        p.expect(T![']']);
        p.finish();
    } else if p.at(T!['{']) {
        p.start(SyntaxKind::StructBinding);
        p.expect(T!['{']);
        while !p.at(T!['}']) {
            p.start(SyntaxKind::StructFieldBinding);
            let spread = p.try_eat(T![...]);
            p.expect(SyntaxKind::Ident);
            if p.try_eat(T![:]) && !spread {
                binding(p);
            }
            p.finish();
            if !p.try_eat(T![,]) {
                break;
            }
        }
        p.expect(T!['}']);
        p.finish();
    } else {
        p.skip();
    }
}
