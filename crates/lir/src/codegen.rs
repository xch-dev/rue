use clvm_traits::{ToClvm, clvm_list, clvm_quote};
use clvmr::{Allocator, NodePtr};
use id_arena::Arena;

use crate::{Lir, LirId, Result};

const OP_C: u8 = 4;

pub fn codegen(arena: &Arena<Lir>, allocator: &mut Allocator, lir: LirId) -> Result<NodePtr> {
    match &arena[lir] {
        Lir::Atom(atom) => {
            if atom.is_empty() {
                return Ok(NodePtr::NIL);
            }
            let atom = allocator.new_atom(atom)?;
            Ok(clvm_quote!(atom).to_clvm(allocator)?)
        }
        Lir::Cons(first, rest) => {
            let first = codegen(arena, allocator, *first)?;
            let rest = codegen(arena, allocator, *rest)?;
            Ok(clvm_list!(OP_C, first, rest).to_clvm(allocator)?)
        }
    }
}

#[cfg(test)]
mod tests {
    use clvm_tools_rs::classic::clvm_tools::binutils::disassemble;
    use expect_test::{Expect, expect};

    use super::*;

    fn check(arena: &Arena<Lir>, lir: LirId, expect: Expect) {
        let mut allocator = Allocator::new();
        let ptr = codegen(arena, &mut allocator, lir).unwrap();
        let result = disassemble(&allocator, ptr, None);
        expect.assert_eq(&result);
    }

    #[test]
    fn test_atom() {
        let mut arena = Arena::new();
        let lir = arena.alloc(Lir::Atom(b"hello".to_vec()));
        check(&arena, lir, expect![[r#"(q . "hello")"#]]);
    }

    #[test]
    fn test_nil() {
        let mut arena = Arena::new();
        let lir = arena.alloc(Lir::Atom(Vec::new()));
        check(&arena, lir, expect!["()"]);
    }

    #[test]
    fn test_cons() {
        let mut arena = Arena::new();
        let hello = arena.alloc(Lir::Atom(b"hello".to_vec()));
        let world = arena.alloc(Lir::Atom(b"world".to_vec()));
        let lir = arena.alloc(Lir::Cons(hello, world));
        check(
            &arena,
            lir,
            expect![[r#"(c (q . "hello") (q . "world"))"#]],
        );
    }
}
