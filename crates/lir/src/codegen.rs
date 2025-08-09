use clvm_traits::{ToClvm, clvm_list, clvm_quote};
use clvmr::{Allocator, NodePtr};
use id_arena::Arena;

use crate::{Lir, LirId, Result};

const OP_A: u8 = 2;
const OP_I: u8 = 3;
const OP_C: u8 = 4;
const OP_F: u8 = 5;
const OP_R: u8 = 6;
const OP_L: u8 = 7;
const OP_X: u8 = 8;
const OP_EQ: u8 = 9;
const OP_ADD: u8 = 16;
const OP_SUB: u8 = 17;
const OP_MUL: u8 = 18;
const OP_DIV: u8 = 19;
const OP_DIVMOD: u8 = 20;
const OP_GT: u8 = 21;
const OP_NOT: u8 = 32;
const OP_ANY: u8 = 33;
const OP_ALL: u8 = 34;
const OP_MODPOW: u8 = 60;
const OP_MOD: u8 = 61;

pub fn codegen(arena: &Arena<Lir>, allocator: &mut Allocator, lir: LirId) -> Result<NodePtr> {
    match &arena[lir] {
        Lir::Atom(atom) => {
            if atom.is_empty() {
                return Ok(NodePtr::NIL);
            }
            let atom = allocator.new_atom(atom)?;
            Ok(clvm_quote!(atom).to_clvm(allocator)?)
        }
        Lir::First(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_F, arg).to_clvm(allocator)?)
        }
        Lir::Rest(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_R, arg).to_clvm(allocator)?)
        }
        Lir::Cons(first, rest) => {
            let first = codegen(arena, allocator, *first)?;
            let rest = codegen(arena, allocator, *rest)?;
            Ok(clvm_list!(OP_C, first, rest).to_clvm(allocator)?)
        }
        Lir::Listp(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_L, arg).to_clvm(allocator)?)
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
        Lir::Divmod(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_DIVMOD, first, second).to_clvm(allocator)?)
        }
        Lir::Mod(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_MOD, first, second).to_clvm(allocator)?)
        }
        Lir::Modpow(base, exponent, modulus) => {
            let base = codegen(arena, allocator, *base)?;
            let exponent = codegen(arena, allocator, *exponent)?;
            let modulus = codegen(arena, allocator, *modulus)?;
            Ok(clvm_list!(OP_MODPOW, base, exponent, modulus).to_clvm(allocator)?)
        }
        Lir::Eq(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_EQ, first, second).to_clvm(allocator)?)
        }
        Lir::Gt(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_GT, first, second).to_clvm(allocator)?)
        }
        Lir::Not(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_NOT, arg).to_clvm(allocator)?)
        }
        Lir::All(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_ALL, args).to_clvm(allocator)?)
        }
        Lir::Any(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_ANY, args).to_clvm(allocator)?)
        }
        Lir::If(cond, then_lir, else_lir) => {
            let cond = codegen(arena, allocator, *cond)?;
            let then_ptr = codegen(arena, allocator, *then_lir)?;
            let else_ptr = codegen(arena, allocator, *else_lir)?;
            Ok(clvm_list!(
                OP_A,
                clvm_list!(OP_I, cond, clvm_quote!(then_ptr), clvm_quote!(else_ptr)),
                1
            )
            .to_clvm(allocator)?)
        }
        Lir::Raise(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_X, args).to_clvm(allocator)?)
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
    fn test_first() {
        let mut arena = Arena::new();
        let first = arena.alloc(Lir::Atom(b"first".to_vec()));
        let rest = arena.alloc(Lir::Atom(b"rest".to_vec()));
        let pair = arena.alloc(Lir::Cons(first, rest));
        let lir = arena.alloc(Lir::First(pair));
        check(
            &arena,
            lir,
            expect![[r#"(f (c (q . "first") (q . "rest")))"#]],
        );
    }

    #[test]
    fn test_rest() {
        let mut arena = Arena::new();
        let first = arena.alloc(Lir::Atom(b"first".to_vec()));
        let rest = arena.alloc(Lir::Atom(b"rest".to_vec()));
        let pair = arena.alloc(Lir::Cons(first, rest));
        let lir = arena.alloc(Lir::Rest(pair));
        check(
            &arena,
            lir,
            expect![[r#"(r (c (q . "first") (q . "rest")))"#]],
        );
    }

    #[test]
    fn test_cons() {
        let mut arena = Arena::new();
        let first = arena.alloc(Lir::Atom(b"first".to_vec()));
        let rest = arena.alloc(Lir::Atom(b"rest".to_vec()));
        let lir = arena.alloc(Lir::Cons(first, rest));
        check(&arena, lir, expect![[r#"(c (q . "first") (q . "rest"))"#]]);
    }

    #[test]
    fn test_listp() {
        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Atom(b"value".to_vec()));
        let lir = arena.alloc(Lir::Listp(value));
        check(&arena, lir, expect![[r#"(l (q . "value"))"#]]);
    }

    #[test]
    fn test_add() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Add(vec![a, b]));
        check(&arena, lir, expect!["(+ (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_sub() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Sub(vec![a, b]));
        check(&arena, lir, expect!["(- (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_mul() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Mul(vec![a, b]));
        check(&arena, lir, expect!["(* (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_div() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Div(a, b));
        check(&arena, lir, expect!["(/ (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_divmod() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x34]));
        let b = arena.alloc(Lir::Atom(vec![0x07]));
        let lir = arena.alloc(Lir::Divmod(a, b));
        check(&arena, lir, expect!["(divmod (q . 52) (q . 7))"]);
    }

    #[test]
    fn test_mod() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x34]));
        let b = arena.alloc(Lir::Atom(vec![0x07]));
        let lir = arena.alloc(Lir::Mod(a, b));
        check(&arena, lir, expect!["(% (q . 52) (q . 7))"]);
    }

    #[test]
    fn test_modpow() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x34]));
        let b = arena.alloc(Lir::Atom(vec![0x07]));
        let c = arena.alloc(Lir::Atom(vec![0x03]));
        let lir = arena.alloc(Lir::Modpow(a, b, c));
        check(&arena, lir, expect!["(modpow (q . 52) (q . 7) (q . 3))"]);
    }

    #[test]
    fn test_eq() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Eq(a, b));
        check(&arena, lir, expect!["(= (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_gt() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Gt(a, b));
        check(&arena, lir, expect!["(> (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_not() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let lir = arena.alloc(Lir::Not(a));
        check(&arena, lir, expect!["(not (q . 1))"]);
    }

    #[test]
    fn test_all() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::All(vec![a, b]));
        check(&arena, lir, expect!["(all (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_any() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Any(vec![a, b]));
        check(&arena, lir, expect!["(any (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_if() {
        let mut arena = Arena::new();
        let cond = arena.alloc(Lir::Atom(vec![0x01]));
        let then = arena.alloc(Lir::Atom(vec![0x02]));
        let else_ = arena.alloc(Lir::Atom(vec![0x03]));
        let lir = arena.alloc(Lir::If(cond, then, else_));
        check(&arena, lir, expect!["(a (i (q . 1) (q 1 . 2) (q 1 . 3)) 1)"]);
    }

    #[test]
    fn test_raise() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let lir = arena.alloc(Lir::Raise(vec![a]));
        check(&arena, lir, expect!["(x (q . 1))"]);
    }
}
