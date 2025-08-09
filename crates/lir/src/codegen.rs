use clvm_traits::{ToClvm, clvm_list, clvm_quote, clvm_tuple};
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
const OP_GT_BYTES: u8 = 10;
const OP_SHA256: u8 = 11;
const OP_SUBSTR: u8 = 12;
const OP_STRLEN: u8 = 13;
const OP_CONCAT: u8 = 14;
const OP_ADD: u8 = 16;
const OP_SUB: u8 = 17;
const OP_MUL: u8 = 18;
const OP_DIV: u8 = 19;
const OP_DIVMOD: u8 = 20;
const OP_GT: u8 = 21;
const OP_ASH: u8 = 22;
const OP_LSH: u8 = 23;
const OP_LOGAND: u8 = 24;
const OP_LOGIOR: u8 = 25;
const OP_LOGXOR: u8 = 26;
const OP_LOGNOT: u8 = 27;
const OP_NOT: u8 = 32;
const OP_ANY: u8 = 33;
const OP_ALL: u8 = 34;
const OP_MODPOW: u8 = 60;
const OP_MOD: u8 = 61;
const OP_G1_ADD: u8 = 29;
const OP_PUBKEY_FOR_EXP: u8 = 30;
const OP_G1_SUBTRACT: u8 = 49;
const OP_G1_MULTIPLY: u8 = 50;
const OP_G1_NEGATE: u8 = 51;
const OP_G2_ADD: u8 = 52;
const OP_G2_SUBTRACT: u8 = 53;
const OP_G2_MULTIPLY: u8 = 54;
const OP_G2_NEGATE: u8 = 55;
const OP_G1_MAP: u8 = 56;
const OP_G2_MAP: u8 = 57;
const OP_BLS_PAIRING_IDENTITY: u8 = 58;
const OP_BLS_VERIFY: u8 = 59;
const OP_COIN_ID: u8 = 48;
const OP_K1_VERIFY: &[u8] = &[0x13, 0xd6, 0x1f, 0x00];
const OP_R1_VERIFY: &[u8] = &[0x1c, 0x3a, 0x8f, 0x00];
const OP_KECCAK256: u8 = 62;

pub fn codegen(arena: &Arena<Lir>, allocator: &mut Allocator, lir: LirId) -> Result<NodePtr> {
    match &arena[lir] {
        Lir::Atom(atom) => {
            if atom.is_empty() {
                return Ok(NodePtr::NIL);
            }
            let atom = allocator.new_atom(atom)?;
            Ok(clvm_quote!(atom).to_clvm(allocator)?)
        }
        Lir::Path(path) => Ok(allocator.new_number((*path).into())?),
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
        Lir::GtBytes(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_GT_BYTES, first, second).to_clvm(allocator)?)
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
        Lir::Concat(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_CONCAT, args).to_clvm(allocator)?)
        }
        Lir::Strlen(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_STRLEN, arg).to_clvm(allocator)?)
        }
        Lir::Substr(arg, start, end) => {
            let arg = codegen(arena, allocator, *arg)?;
            let start = codegen(arena, allocator, *start)?;
            let end = end.map(|end| codegen(arena, allocator, end)).transpose()?;
            if let Some(end) = end {
                Ok(clvm_list!(OP_SUBSTR, arg, start, end).to_clvm(allocator)?)
            } else {
                Ok(clvm_list!(OP_SUBSTR, arg, start).to_clvm(allocator)?)
            }
        }
        Lir::Logand(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_LOGAND, args).to_clvm(allocator)?)
        }
        Lir::Logior(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_LOGIOR, args).to_clvm(allocator)?)
        }
        Lir::Logxor(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_LOGXOR, args).to_clvm(allocator)?)
        }
        Lir::Lognot(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_LOGNOT, arg).to_clvm(allocator)?)
        }
        Lir::Ash(arg, shift) => {
            let arg = codegen(arena, allocator, *arg)?;
            let shift = codegen(arena, allocator, *shift)?;
            Ok(clvm_list!(OP_ASH, arg, shift).to_clvm(allocator)?)
        }
        Lir::Lsh(arg, shift) => {
            let arg = codegen(arena, allocator, *arg)?;
            let shift = codegen(arena, allocator, *shift)?;
            Ok(clvm_list!(OP_LSH, arg, shift).to_clvm(allocator)?)
        }
        Lir::PubkeyForExp(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_PUBKEY_FOR_EXP, arg).to_clvm(allocator)?)
        }
        Lir::G1Add(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_G1_ADD, args).to_clvm(allocator)?)
        }
        Lir::G1Subtract(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_G1_SUBTRACT, args).to_clvm(allocator)?)
        }
        Lir::G1Multiply(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_G1_MULTIPLY, first, second).to_clvm(allocator)?)
        }
        Lir::G1Negate(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_G1_NEGATE, arg).to_clvm(allocator)?)
        }
        Lir::G1Map(value, dst) => {
            let value = codegen(arena, allocator, *value)?;
            let dst = dst.map(|dst| codegen(arena, allocator, dst)).transpose()?;
            if let Some(dst) = dst {
                Ok(clvm_list!(OP_G1_MAP, value, dst).to_clvm(allocator)?)
            } else {
                Ok(clvm_list!(OP_G1_MAP, value).to_clvm(allocator)?)
            }
        }
        Lir::G2Add(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_G2_ADD, args).to_clvm(allocator)?)
        }
        Lir::G2Subtract(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_G2_SUBTRACT, args).to_clvm(allocator)?)
        }
        Lir::G2Multiply(first, second) => {
            let first = codegen(arena, allocator, *first)?;
            let second = codegen(arena, allocator, *second)?;
            Ok(clvm_list!(OP_G2_MULTIPLY, first, second).to_clvm(allocator)?)
        }
        Lir::G2Negate(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            Ok(clvm_list!(OP_G2_NEGATE, arg).to_clvm(allocator)?)
        }
        Lir::G2Map(value, dst) => {
            let value = codegen(arena, allocator, *value)?;
            let dst = dst.map(|dst| codegen(arena, allocator, dst)).transpose()?;
            if let Some(dst) = dst {
                Ok(clvm_list!(OP_G2_MAP, value, dst).to_clvm(allocator)?)
            } else {
                Ok(clvm_list!(OP_G2_MAP, value).to_clvm(allocator)?)
            }
        }
        Lir::BlsPairingIdentity(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_BLS_PAIRING_IDENTITY, args).to_clvm(allocator)?)
        }
        Lir::BlsVerify(arg, args) => {
            let arg = codegen(arena, allocator, *arg)?;
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok(clvm_tuple!(OP_BLS_VERIFY, arg, args).to_clvm(allocator)?)
        }
        Lir::Sha256(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_SHA256, args).to_clvm(allocator)?)
        }
        Lir::Keccak256(args) => {
            let args = args
                .iter()
                .map(|arg| codegen(arena, allocator, *arg))
                .collect::<Result<Vec<_>>>()?;
            Ok((OP_KECCAK256, args).to_clvm(allocator)?)
        }
        Lir::CoinId(parent, puzzle, amount) => {
            let parent = codegen(arena, allocator, *parent)?;
            let puzzle = codegen(arena, allocator, *puzzle)?;
            let amount = codegen(arena, allocator, *amount)?;
            Ok(clvm_list!(OP_COIN_ID, parent, puzzle, amount).to_clvm(allocator)?)
        }
        Lir::K1Verify(pubkey, message, signature) => {
            let pubkey = codegen(arena, allocator, *pubkey)?;
            let message = codegen(arena, allocator, *message)?;
            let signature = codegen(arena, allocator, *signature)?;
            let op = allocator.new_atom(OP_K1_VERIFY)?;
            Ok(clvm_list!(op, pubkey, message, signature).to_clvm(allocator)?)
        }
        Lir::R1Verify(pubkey, message, signature) => {
            let pubkey = codegen(arena, allocator, *pubkey)?;
            let message = codegen(arena, allocator, *message)?;
            let signature = codegen(arena, allocator, *signature)?;
            let op = allocator.new_atom(OP_R1_VERIFY)?;
            Ok(clvm_list!(op, pubkey, message, signature).to_clvm(allocator)?)
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
    fn test_path() {
        let mut arena = Arena::new();
        let lir = arena.alloc(Lir::Path(1));
        check(&arena, lir, expect!["1"]);
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
        check(
            &arena,
            lir,
            expect!["(a (i (q . 1) (q 1 . 2) (q 1 . 3)) 1)"],
        );
    }

    #[test]
    fn test_raise() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let lir = arena.alloc(Lir::Raise(vec![a]));
        check(&arena, lir, expect!["(x (q . 1))"]);
    }

    #[test]
    fn test_gt_bytes() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"abc".to_vec()));
        let b = arena.alloc(Lir::Atom(b"def".to_vec()));
        let lir = arena.alloc(Lir::GtBytes(a, b));
        check(&arena, lir, expect![[r#"(>s (q . "abc") (q . "def"))"#]]);
    }

    #[test]
    fn test_concat() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"hello ".to_vec()));
        let b = arena.alloc(Lir::Atom(b"world".to_vec()));
        let lir = arena.alloc(Lir::Concat(vec![a, b]));
        check(
            &arena,
            lir,
            expect![[r#"(concat (q . "hello ") (q . "world"))"#]],
        );
    }

    #[test]
    fn test_strlen() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"hello".to_vec()));
        let lir = arena.alloc(Lir::Strlen(a));
        check(&arena, lir, expect![[r#"(strlen (q . "hello"))"#]]);
    }

    #[test]
    fn test_substr() {
        let mut arena = Arena::new();
        let str = arena.alloc(Lir::Atom(b"hello world".to_vec()));
        let start = arena.alloc(Lir::Atom(vec![]));
        let end = arena.alloc(Lir::Atom(vec![0x05]));
        let lir = arena.alloc(Lir::Substr(str, start, Some(end)));
        check(
            &arena,
            lir,
            expect![[r#"(substr (q . "hello world") () (q . 5))"#]],
        );

        // Test without end parameter
        let lir = arena.alloc(Lir::Substr(str, start, None));
        check(&arena, lir, expect![[r#"(substr (q . "hello world") ())"#]]);
    }

    #[test]
    fn test_bitwise_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x0F]));
        let b = arena.alloc(Lir::Atom(vec![0xF0]));

        let logand = arena.alloc(Lir::Logand(vec![a, b]));
        check(&arena, logand, expect!["(logand (q . 15) (q . -16))"]);

        let logior = arena.alloc(Lir::Logior(vec![a, b]));
        check(&arena, logior, expect!["(logior (q . 15) (q . -16))"]);

        let logxor = arena.alloc(Lir::Logxor(vec![a, b]));
        check(&arena, logxor, expect!["(logxor (q . 15) (q . -16))"]);

        let lognot = arena.alloc(Lir::Lognot(a));
        check(&arena, lognot, expect!["(lognot (q . 15))"]);
    }

    #[test]
    fn test_shifts() {
        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Atom(vec![0x0F]));
        let shift = arena.alloc(Lir::Atom(vec![0x02]));

        let ash = arena.alloc(Lir::Ash(value, shift));
        check(&arena, ash, expect!["(ash (q . 15) (q . 2))"]);

        let lsh = arena.alloc(Lir::Lsh(value, shift));
        check(&arena, lsh, expect!["(lsh (q . 15) (q . 2))"]);
    }

    #[test]
    fn test_pubkey_for_exp() {
        let mut arena = Arena::new();
        let exp = arena.alloc(Lir::Atom(vec![0x01]));
        let lir = arena.alloc(Lir::PubkeyForExp(exp));
        check(&arena, lir, expect!["(pubkey_for_exp (q . 1))"]);
    }

    #[test]
    fn test_g1_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));

        let add = arena.alloc(Lir::G1Add(vec![a, b]));
        check(&arena, add, expect!["(point_add (q . 1) (q . 2))"]);

        let sub = arena.alloc(Lir::G1Subtract(vec![a, b]));
        check(&arena, sub, expect!["(g1_subtract (q . 1) (q . 2))"]);

        let mul = arena.alloc(Lir::G1Multiply(a, b));
        check(&arena, mul, expect!["(g1_multiply (q . 1) (q . 2))"]);

        let neg = arena.alloc(Lir::G1Negate(a));
        check(&arena, neg, expect!["(g1_negate (q . 1))"]);

        let map = arena.alloc(Lir::G1Map(a, Some(b)));
        check(&arena, map, expect!["(g1_map (q . 1) (q . 2))"]);

        let map_no_dst = arena.alloc(Lir::G1Map(a, None));
        check(&arena, map_no_dst, expect!["(g1_map (q . 1))"]);
    }

    #[test]
    fn test_g2_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));

        let add = arena.alloc(Lir::G2Add(vec![a, b]));
        check(&arena, add, expect!["(g2_add (q . 1) (q . 2))"]);

        let sub = arena.alloc(Lir::G2Subtract(vec![a, b]));
        check(&arena, sub, expect!["(g2_subtract (q . 1) (q . 2))"]);

        let mul = arena.alloc(Lir::G2Multiply(a, b));
        check(&arena, mul, expect!["(g2_multiply (q . 1) (q . 2))"]);

        let neg = arena.alloc(Lir::G2Negate(a));
        check(&arena, neg, expect!["(g2_negate (q . 1))"]);

        let map = arena.alloc(Lir::G2Map(a, Some(b)));
        check(&arena, map, expect!["(g2_map (q . 1) (q . 2))"]);

        let map_no_dst = arena.alloc(Lir::G2Map(a, None));
        check(&arena, map_no_dst, expect!["(g2_map (q . 1))"]);
    }

    #[test]
    fn test_bls_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));

        let identity = arena.alloc(Lir::BlsPairingIdentity(vec![a, b]));
        check(
            &arena,
            identity,
            expect!["(bls_pairing_identity (q . 1) (q . 2))"],
        );

        let verify = arena.alloc(Lir::BlsVerify(a, vec![b]));
        check(&arena, verify, expect!["(bls_verify (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_hash_functions() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"hello".to_vec()));
        let b = arena.alloc(Lir::Atom(b"world".to_vec()));

        let sha256 = arena.alloc(Lir::Sha256(vec![a, b]));
        check(
            &arena,
            sha256,
            expect![[r#"(sha256 (q . "hello") (q . "world"))"#]],
        );

        let keccak256 = arena.alloc(Lir::Keccak256(vec![a, b]));
        check(
            &arena,
            keccak256,
            expect![[r#"(keccak256 (q . "hello") (q . "world"))"#]],
        );
    }

    #[test]
    fn test_coin_id() {
        let mut arena = Arena::new();
        let parent = arena.alloc(Lir::Atom(vec![0x01]));
        let puzzle = arena.alloc(Lir::Atom(vec![0x02]));
        let amount = arena.alloc(Lir::Atom(vec![0x03]));
        let lir = arena.alloc(Lir::CoinId(parent, puzzle, amount));
        check(&arena, lir, expect!["(coinid (q . 1) (q . 2) (q . 3))"]);
    }

    #[test]
    fn test_signature_verification() {
        let mut arena = Arena::new();
        let pubkey = arena.alloc(Lir::Atom(vec![0x01]));
        let message = arena.alloc(Lir::Atom(vec![0x02]));
        let signature = arena.alloc(Lir::Atom(vec![0x03]));

        let k1_verify = arena.alloc(Lir::K1Verify(pubkey, message, signature));
        check(
            &arena,
            k1_verify,
            expect!["(0x13d61f00 (q . 1) (q . 2) (q . 3))"],
        );

        let r1_verify = arena.alloc(Lir::R1Verify(pubkey, message, signature));
        check(
            &arena,
            r1_verify,
            expect!["(0x1c3a8f00 (q . 1) (q . 2) (q . 3))"],
        );
    }
}
