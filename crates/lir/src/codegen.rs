use clvm_traits::{ToClvm, clvm_list, clvm_quote};
use clvmr::{Allocator, NodePtr};
use id_arena::Arena;

use crate::{Lir, LirId, Result};

const OP_C: u8 = 4;
const OP_ADD: u8 = 16;
const OP_SUB: u8 = 17;
const OP_MUL: u8 = 18;
const OP_DIV: u8 = 19;

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
        Lir::Add(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_ADD, args).to_clvm(allocator)?)
        }
        Lir::Sub(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_SUB, args).to_clvm(allocator)?)
        }
        Lir::Mul(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_MUL, args).to_clvm(allocator)?)
        }
        Lir::Div(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_DIV, first, second).to_clvm(allocator)?)
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
        check(&arena, lir, expect![[r#"(c (q . "hello") (q . "world"))"#]]);
    }

    #[test]
    fn test_add() {
        let mut arena = Arena::new();
        let hello = arena.alloc(Lir::Atom(vec![0x01]));
        let world = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Add(vec![hello, world]));
        check(&arena, lir, expect!["(+ (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_sub() {
        let mut arena = Arena::new();
        let hello = arena.alloc(Lir::Atom(vec![0x01]));
        let world = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Sub(vec![hello, world]));
        check(&arena, lir, expect!["(- (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_mul() {
        let mut arena = Arena::new();
        let hello = arena.alloc(Lir::Atom(vec![0x01]));
        let world = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Mul(vec![hello, world]));
        check(&arena, lir, expect!["(* (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_div() {
        let mut arena = Arena::new();
        let hello = arena.alloc(Lir::Atom(vec![0x01]));
        let world = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Div(hello, world));
        check(&arena, lir, expect!["(/ (q . 1) (q . 2))"]);
    }
}
