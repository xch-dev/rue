use std::rc::Rc;

use clvm_traits::{ToClvm, ToClvmError};
use clvmr::{Allocator, NodePtr};
use id_arena::Id;
use num_bigint::BigUint;

use crate::{ClvmTarget, bigint_atom};

pub type LirId = Id<Lir>;

#[derive(Debug, Clone)]
pub enum Lir {
    Atom(Vec<u8>),
    /// A constant value of any shape, as serialized CLVM, which is generated quoted whatever the
    /// options are, such as the program of a function that's used as a value.
    Value(Rc<[u8]>),
    Path(BigUint),
    Quote(LirId),
    Run(LirId, LirId),
    Closure(LirId, Vec<LirId>, bool),
    First(LirId),
    Rest(LirId),
    Cons(LirId, LirId),
    Listp(LirId),
    Add(Vec<LirId>),
    Sub(Vec<LirId>),
    Mul(Vec<LirId>),
    Div(LirId, LirId),
    Divmod(LirId, LirId),
    Mod(LirId, LirId),
    Modpow(LirId, LirId, LirId),
    Eq(LirId, LirId),
    Gt(LirId, LirId),
    GtBytes(LirId, LirId),
    Not(LirId),
    All(Vec<LirId>),
    Any(Vec<LirId>),
    If(LirId, LirId, LirId, bool),
    Raise(Vec<LirId>),
    Concat(Vec<LirId>),
    Strlen(LirId),
    Substr(LirId, LirId, Option<LirId>),
    Logand(Vec<LirId>),
    Logior(Vec<LirId>),
    Logxor(Vec<LirId>),
    Lognot(LirId),
    Ash(LirId, LirId),
    Lsh(LirId, LirId),
    PubkeyForExp(LirId),
    G1Add(Vec<LirId>),
    G1Subtract(Vec<LirId>),
    G1Multiply(LirId, LirId),
    G1Negate(LirId),
    G1Map(LirId, Option<LirId>),
    G2Add(Vec<LirId>),
    G2Subtract(Vec<LirId>),
    G2Multiply(LirId, LirId),
    G2Negate(LirId),
    G2Map(LirId, Option<LirId>),
    BlsPairingIdentity(Vec<LirId>),
    BlsVerify(LirId, Vec<LirId>),
    Sha256(Vec<LirId>),
    Sha256Inline(Vec<LirId>),
    Keccak256(Vec<LirId>),
    Keccak256Inline(Vec<LirId>),
    CoinId(LirId, LirId, LirId),
    K1Verify(LirId, LirId, LirId),
    R1Verify(LirId, LirId, LirId),
    /// Fails unless the Keccak-256 hash of the value is the hash (see `KECCAK256_VERIFY_COST`).
    Keccak256Verify(LirId, LirId),
    Op(ClvmOp, LirId),
    DebugPrint(String, LirId),
}

impl Lir {
    /// The nodes directly inside of this one, in the order they're evaluated and generated.
    pub fn children(&self) -> impl Iterator<Item = LirId> + '_ {
        let (fixed, list): ([Option<LirId>; 3], &[LirId]) = match self {
            Self::Atom(_) | Self::Value(_) | Self::Path(_) => ([None; 3], &[]),
            Self::Quote(a)
            | Self::First(a)
            | Self::Rest(a)
            | Self::Listp(a)
            | Self::Not(a)
            | Self::Strlen(a)
            | Self::Lognot(a)
            | Self::PubkeyForExp(a)
            | Self::G1Negate(a)
            | Self::G2Negate(a)
            | Self::Op(_, a)
            | Self::DebugPrint(_, a) => ([Some(*a), None, None], &[]),
            Self::Run(a, b)
            | Self::Cons(a, b)
            | Self::Div(a, b)
            | Self::Divmod(a, b)
            | Self::Mod(a, b)
            | Self::Eq(a, b)
            | Self::Gt(a, b)
            | Self::GtBytes(a, b)
            | Self::Ash(a, b)
            | Self::Lsh(a, b)
            | Self::G1Multiply(a, b)
            | Self::G2Multiply(a, b)
            | Self::Keccak256Verify(a, b) => ([Some(*a), Some(*b), None], &[]),
            Self::G1Map(a, b) | Self::G2Map(a, b) => ([Some(*a), *b, None], &[]),
            Self::Substr(a, b, c) => ([Some(*a), Some(*b), *c], &[]),
            Self::Modpow(a, b, c)
            | Self::If(a, b, c, _)
            | Self::CoinId(a, b, c)
            | Self::K1Verify(a, b, c)
            | Self::R1Verify(a, b, c) => ([Some(*a), Some(*b), Some(*c)], &[]),
            Self::Closure(a, args, _) | Self::BlsVerify(a, args) => {
                ([Some(*a), None, None], args.as_slice())
            }
            Self::Add(args)
            | Self::Sub(args)
            | Self::Mul(args)
            | Self::All(args)
            | Self::Any(args)
            | Self::Raise(args)
            | Self::Concat(args)
            | Self::Logand(args)
            | Self::Logior(args)
            | Self::Logxor(args)
            | Self::G1Add(args)
            | Self::G1Subtract(args)
            | Self::G2Add(args)
            | Self::G2Subtract(args)
            | Self::BlsPairingIdentity(args)
            | Self::Sha256(args)
            | Self::Sha256Inline(args)
            | Self::Keccak256(args)
            | Self::Keccak256Inline(args) => ([None; 3], args.as_slice()),
        };

        fixed.into_iter().flatten().chain(list.iter().copied())
    }

    /// The same node with each of its children replaced by what `f` returns for it. `f` is called
    /// on the children in the order of [`Lir::children`].
    #[must_use]
    pub fn map_children(self, mut f: impl FnMut(LirId) -> LirId) -> Self {
        fn list(args: Vec<LirId>, f: &mut impl FnMut(LirId) -> LirId) -> Vec<LirId> {
            args.into_iter().map(f).collect()
        }

        match self {
            Self::Atom(_) | Self::Value(_) | Self::Path(_) => self,
            Self::Quote(a) => Self::Quote(f(a)),
            Self::Run(a, b) => Self::Run(f(a), f(b)),
            Self::Closure(a, args, has_parameters) => {
                let a = f(a);
                Self::Closure(a, list(args, &mut f), has_parameters)
            }
            Self::First(a) => Self::First(f(a)),
            Self::Rest(a) => Self::Rest(f(a)),
            Self::Cons(a, b) => Self::Cons(f(a), f(b)),
            Self::Listp(a) => Self::Listp(f(a)),
            Self::Add(args) => Self::Add(list(args, &mut f)),
            Self::Sub(args) => Self::Sub(list(args, &mut f)),
            Self::Mul(args) => Self::Mul(list(args, &mut f)),
            Self::Div(a, b) => Self::Div(f(a), f(b)),
            Self::Divmod(a, b) => Self::Divmod(f(a), f(b)),
            Self::Mod(a, b) => Self::Mod(f(a), f(b)),
            Self::Modpow(a, b, c) => Self::Modpow(f(a), f(b), f(c)),
            Self::Eq(a, b) => Self::Eq(f(a), f(b)),
            Self::Gt(a, b) => Self::Gt(f(a), f(b)),
            Self::GtBytes(a, b) => Self::GtBytes(f(a), f(b)),
            Self::Not(a) => Self::Not(f(a)),
            Self::All(args) => Self::All(list(args, &mut f)),
            Self::Any(args) => Self::Any(list(args, &mut f)),
            Self::If(a, b, c, inline) => Self::If(f(a), f(b), f(c), inline),
            Self::Raise(args) => Self::Raise(list(args, &mut f)),
            Self::Concat(args) => Self::Concat(list(args, &mut f)),
            Self::Strlen(a) => Self::Strlen(f(a)),
            Self::Substr(a, b, c) => {
                let a = f(a);
                let b = f(b);
                Self::Substr(a, b, c.map(&mut f))
            }
            Self::Logand(args) => Self::Logand(list(args, &mut f)),
            Self::Logior(args) => Self::Logior(list(args, &mut f)),
            Self::Logxor(args) => Self::Logxor(list(args, &mut f)),
            Self::Lognot(a) => Self::Lognot(f(a)),
            Self::Ash(a, b) => Self::Ash(f(a), f(b)),
            Self::Lsh(a, b) => Self::Lsh(f(a), f(b)),
            Self::PubkeyForExp(a) => Self::PubkeyForExp(f(a)),
            Self::G1Add(args) => Self::G1Add(list(args, &mut f)),
            Self::G1Subtract(args) => Self::G1Subtract(list(args, &mut f)),
            Self::G1Multiply(a, b) => Self::G1Multiply(f(a), f(b)),
            Self::G1Negate(a) => Self::G1Negate(f(a)),
            Self::G1Map(a, b) => {
                let a = f(a);
                Self::G1Map(a, b.map(&mut f))
            }
            Self::G2Add(args) => Self::G2Add(list(args, &mut f)),
            Self::G2Subtract(args) => Self::G2Subtract(list(args, &mut f)),
            Self::G2Multiply(a, b) => Self::G2Multiply(f(a), f(b)),
            Self::G2Negate(a) => Self::G2Negate(f(a)),
            Self::G2Map(a, b) => {
                let a = f(a);
                Self::G2Map(a, b.map(&mut f))
            }
            Self::BlsPairingIdentity(args) => Self::BlsPairingIdentity(list(args, &mut f)),
            Self::BlsVerify(a, args) => {
                let a = f(a);
                Self::BlsVerify(a, list(args, &mut f))
            }
            Self::Sha256(args) => Self::Sha256(list(args, &mut f)),
            Self::Sha256Inline(args) => Self::Sha256Inline(list(args, &mut f)),
            Self::Keccak256(args) => Self::Keccak256(list(args, &mut f)),
            Self::Keccak256Inline(args) => Self::Keccak256Inline(list(args, &mut f)),
            Self::CoinId(a, b, c) => Self::CoinId(f(a), f(b), f(c)),
            Self::K1Verify(a, b, c) => Self::K1Verify(f(a), f(b), f(c)),
            Self::R1Verify(a, b, c) => Self::R1Verify(f(a), f(b), f(c)),
            Self::Keccak256Verify(a, b) => Self::Keccak256Verify(f(a), f(b)),
            Self::Op(op, a) => Self::Op(op, f(a)),
            Self::DebugPrint(srcloc, a) => Self::DebugPrint(srcloc, f(a)),
        }
    }

    /// The CLVM operator that this node is generated as, called with its children (see
    /// [`Lir::children`]) as the operands, as in `(OPERATOR CHILD ...)`. This is `None` for nodes
    /// that are generated some other way: atoms, values, paths, quotes, closures, conditions,
    /// calls to an operator at runtime ([`Lir::Op`]), debug prints and Keccak-256 checks.
    ///
    /// The secp operators are generated with the opcode for the target (see
    /// [`ClvmOp::to_target_atom`]).
    pub fn operator(&self) -> Option<ClvmOp> {
        Some(match self {
            Self::Atom(_)
            | Self::Value(_)
            | Self::Path(_)
            | Self::Quote(_)
            | Self::Closure(..)
            | Self::If(..)
            | Self::Keccak256Verify(..)
            | Self::Op(..)
            | Self::DebugPrint(..) => return None,
            Self::Run(..) => ClvmOp::Apply,
            Self::First(_) => ClvmOp::First,
            Self::Rest(_) => ClvmOp::Rest,
            Self::Cons(..) => ClvmOp::Cons,
            Self::Listp(_) => ClvmOp::Listp,
            Self::Add(_) => ClvmOp::Add,
            Self::Sub(_) => ClvmOp::Sub,
            Self::Mul(_) => ClvmOp::Mul,
            Self::Div(..) => ClvmOp::Div,
            Self::Divmod(..) => ClvmOp::Divmod,
            Self::Mod(..) => ClvmOp::Mod,
            Self::Modpow(..) => ClvmOp::Modpow,
            Self::Eq(..) => ClvmOp::Eq,
            Self::Gt(..) => ClvmOp::Gt,
            Self::GtBytes(..) => ClvmOp::GtBytes,
            Self::Not(_) => ClvmOp::Not,
            Self::All(_) => ClvmOp::All,
            Self::Any(_) => ClvmOp::Any,
            Self::Raise(_) => ClvmOp::Raise,
            Self::Concat(_) => ClvmOp::Concat,
            Self::Strlen(_) => ClvmOp::Strlen,
            Self::Substr(..) => ClvmOp::Substr,
            Self::Logand(_) => ClvmOp::Logand,
            Self::Logior(_) => ClvmOp::Logior,
            Self::Logxor(_) => ClvmOp::Logxor,
            Self::Lognot(_) => ClvmOp::Lognot,
            Self::Ash(..) => ClvmOp::Ash,
            Self::Lsh(..) => ClvmOp::Lsh,
            Self::PubkeyForExp(_) => ClvmOp::PubkeyForExp,
            Self::G1Add(_) => ClvmOp::G1Add,
            Self::G1Subtract(_) => ClvmOp::G1Subtract,
            Self::G1Multiply(..) => ClvmOp::G1Multiply,
            Self::G1Negate(_) => ClvmOp::G1Negate,
            Self::G1Map(..) => ClvmOp::G1Map,
            Self::G2Add(_) => ClvmOp::G2Add,
            Self::G2Subtract(_) => ClvmOp::G2Subtract,
            Self::G2Multiply(..) => ClvmOp::G2Multiply,
            Self::G2Negate(_) => ClvmOp::G2Negate,
            Self::G2Map(..) => ClvmOp::G2Map,
            Self::BlsPairingIdentity(_) => ClvmOp::BlsPairingIdentity,
            Self::BlsVerify(..) => ClvmOp::BlsVerify,
            Self::Sha256(_) | Self::Sha256Inline(_) => ClvmOp::Sha256,
            Self::Keccak256(_) | Self::Keccak256Inline(_) => ClvmOp::Keccak256,
            Self::CoinId(..) => ClvmOp::CoinId,
            Self::K1Verify(..) => ClvmOp::Secp256K1Verify,
            Self::R1Verify(..) => ClvmOp::Secp256R1Verify,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ClvmOp {
    Quote,
    Apply,
    If,
    Cons,
    First,
    Rest,
    Listp,
    Raise,
    Eq,
    GtBytes,
    Sha256,
    Substr,
    Strlen,
    Concat,
    Add,
    Sub,
    Mul,
    Div,
    Divmod,
    Gt,
    Ash,
    Lsh,
    Logand,
    Logior,
    Logxor,
    Lognot,
    Not,
    Any,
    All,
    Modpow,
    Mod,
    CoinId,
    PubkeyForExp,
    G1Add,
    G1Subtract,
    G1Multiply,
    G1Negate,
    G2Add,
    G2Subtract,
    G2Multiply,
    G2Negate,
    G1Map,
    G2Map,
    BlsPairingIdentity,
    BlsVerify,
    Secp256K1Verify,
    Secp256R1Verify,
    Keccak256,
    Softfork,

    // Non-standard
    DebugPrint,
}

impl ClvmOp {
    /// The opcode of the operator for programs compiled for the target. Hard fork 2 makes the
    /// secp operators available as the one byte opcodes 64 and 65, which are shorter than the four
    /// byte opcodes that encode their cost, and are the same otherwise.
    pub fn to_target_atom(self, target: ClvmTarget) -> Vec<u8> {
        match self {
            Self::Secp256K1Verify if target.hard_fork_2 => vec![64],
            Self::Secp256R1Verify if target.hard_fork_2 => vec![65],
            _ => self.to_atom(),
        }
    }

    pub fn to_atom(self) -> Vec<u8> {
        let num = match self {
            Self::Quote => 1,
            Self::Apply => 2,
            Self::If => 3,
            Self::Cons => 4,
            Self::First => 5,
            Self::Rest => 6,
            Self::Listp => 7,
            Self::Raise => 8,
            Self::Eq => 9,
            Self::GtBytes => 10,
            Self::Sha256 => 11,
            Self::Substr => 12,
            Self::Strlen => 13,
            Self::Concat => 14,
            Self::Add => 16,
            Self::Sub => 17,
            Self::Mul => 18,
            Self::Div => 19,
            Self::Divmod => 20,
            Self::Gt => 21,
            Self::Ash => 22,
            Self::Lsh => 23,
            Self::Logand => 24,
            Self::Logior => 25,
            Self::Logxor => 26,
            Self::Lognot => 27,
            Self::Not => 32,
            Self::Any => 33,
            Self::All => 34,
            Self::Modpow => 60,
            Self::Mod => 61,
            Self::PubkeyForExp => 30,
            Self::CoinId => 48,
            Self::G1Add => 29,
            Self::G1Subtract => 49,
            Self::G1Multiply => 50,
            Self::G1Negate => 51,
            Self::G2Add => 52,
            Self::G2Subtract => 53,
            Self::G2Multiply => 54,
            Self::G2Negate => 55,
            Self::G1Map => 56,
            Self::G2Map => 57,
            Self::BlsPairingIdentity => 58,
            Self::BlsVerify => 59,
            Self::Secp256K1Verify => return vec![0x13, 0xd6, 0x1f, 0x00],
            Self::Secp256R1Verify => return vec![0x1c, 0x3a, 0x8f, 0x00],
            Self::Keccak256 => 62,
            Self::Softfork => 36,
            Self::DebugPrint => return b"debug_print".into(),
        };

        bigint_atom(num.into())
    }
}

impl ToClvm<Allocator> for ClvmOp {
    fn to_clvm(&self, encoder: &mut Allocator) -> Result<NodePtr, ToClvmError> {
        encoder
            .new_atom(&self.to_atom())
            .map_err(|_| ToClvmError::OutOfMemory)
    }
}

#[cfg(test)]
mod tests {
    use clvmr::Allocator;
    use id_arena::Arena;

    use super::*;
    use crate::{CodegenOptions, codegen, optimize};

    /// One node of each kind, with distinct children, so that the order of each can be checked.
    fn every_node(arena: &mut Arena<Lir>) -> Vec<Lir> {
        let [a, b, c] = [1, 2, 3].map(|value| arena.alloc(Lir::Atom(vec![value])));

        vec![
            Lir::Atom(vec![1]),
            Lir::Path(BigUint::from(2u8)),
            Lir::Quote(a),
            Lir::Run(a, b),
            Lir::Closure(a, vec![b, c], true),
            Lir::First(a),
            Lir::Rest(a),
            Lir::Cons(a, b),
            Lir::Listp(a),
            Lir::Add(vec![a, b, c]),
            Lir::Sub(vec![a, b]),
            Lir::Mul(vec![a, b]),
            Lir::Div(a, b),
            Lir::Divmod(a, b),
            Lir::Mod(a, b),
            Lir::Modpow(a, b, c),
            Lir::Eq(a, b),
            Lir::Gt(a, b),
            Lir::GtBytes(a, b),
            Lir::Not(a),
            Lir::All(vec![a, b]),
            Lir::Any(vec![a, b]),
            Lir::If(a, b, c, false),
            Lir::Raise(vec![a]),
            Lir::Concat(vec![a, b]),
            Lir::Strlen(a),
            Lir::Substr(a, b, Some(c)),
            Lir::Substr(a, b, None),
            Lir::Logand(vec![a, b]),
            Lir::Logior(vec![a, b]),
            Lir::Logxor(vec![a, b]),
            Lir::Lognot(a),
            Lir::Ash(a, b),
            Lir::Lsh(a, b),
            Lir::PubkeyForExp(a),
            Lir::G1Add(vec![a, b]),
            Lir::G1Subtract(vec![a, b]),
            Lir::G1Multiply(a, b),
            Lir::G1Negate(a),
            Lir::G1Map(a, Some(b)),
            Lir::G1Map(a, None),
            Lir::G2Add(vec![a, b]),
            Lir::G2Subtract(vec![a, b]),
            Lir::G2Multiply(a, b),
            Lir::G2Negate(a),
            Lir::G2Map(a, Some(b)),
            Lir::G2Map(a, None),
            Lir::BlsPairingIdentity(vec![a, b]),
            Lir::BlsVerify(a, vec![b, c]),
            Lir::Sha256(vec![a, b]),
            Lir::Sha256Inline(vec![a, b]),
            Lir::Keccak256(vec![a, b]),
            Lir::Keccak256Inline(vec![a, b]),
            Lir::CoinId(a, b, c),
            Lir::K1Verify(a, b, c),
            Lir::R1Verify(a, b, c),
            Lir::Keccak256Verify(a, b),
            Lir::Op(ClvmOp::Add, a),
            Lir::DebugPrint("srcloc".into(), a),
        ]
    }

    // `map_children` visits the children in the order of `children`, and puts each replacement
    // where the child it replaces was
    #[test]
    fn map_children_follows_children() {
        let mut arena = Arena::new();
        let replacements: Vec<LirId> = (0..3).map(|_| arena.alloc(Lir::Atom(vec![]))).collect();

        for node in every_node(&mut arena) {
            let children: Vec<LirId> = node.children().collect();
            let mut visited = Vec::new();
            let mapped = node.clone().map_children(|child| {
                visited.push(child);
                replacements[visited.len() - 1]
            });

            assert_eq!(visited, children, "{node:?}");
            assert_eq!(
                mapped.children().collect::<Vec<_>>(),
                replacements[..children.len()],
                "{node:?}"
            );
            assert_eq!(
                std::mem::discriminant(&mapped),
                std::mem::discriminant(&node)
            );
            assert_eq!(mapped.operator(), node.operator());
        }
    }

    // Each kind of node is generated, either by its own case or as a call to its operator, with
    // and without folding static pairs, before and after it's optimized, for each target
    #[test]
    fn every_node_is_generated() {
        let mut arena = Arena::new();

        for node in every_node(&mut arena) {
            let lir = arena.alloc(node);

            for target in ClvmTarget::ALL {
                let optimized = optimize(&mut arena, lir, target);

                for (lir, optimize_static_pairs) in [(lir, false), (optimized, true)] {
                    let options = CodegenOptions {
                        optimize_static_pairs,
                        target,
                    };
                    codegen(&arena, &mut Allocator::new(), lir, options).unwrap();
                }
            }
        }
    }
}
