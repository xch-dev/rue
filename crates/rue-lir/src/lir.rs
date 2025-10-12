use clvm_traits::{ToClvm, ToClvmError};
use clvmr::{Allocator, NodePtr};
use id_arena::{Arena, Id};

use crate::bigint_atom;

pub type LirId = Id<Lir>;

#[derive(Debug, Clone)]
pub enum Lir {
    Atom(Vec<u8>),
    Path(u32),
    Quote(LirId),
    Run(LirId, LirId),
    Closure(LirId, Vec<LirId>, bool),
    First(LirId),
    Rest(LirId),
    Cons(LirId, LirId),
    Listp(LirId, bool),
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
    Op(ClvmOp, LirId),
}

pub fn stringify_lir(arena: &Arena<Lir>, id: LirId) -> String {
    match arena[id].clone() {
        Lir::Atom(atom) => format!("0x{}", hex::encode(atom)),
        Lir::Path(path) => format!("{path}"),
        Lir::Quote(id) => format!("quote({})", stringify_lir(arena, id)),
        Lir::Run(callee, env) => format!(
            "run({}, {})",
            stringify_lir(arena, callee),
            stringify_lir(arena, env)
        ),
        Lir::Closure(callee, env, has_parameters) => format!(
            "closure({}, [{}], has_parameters={})",
            stringify_lir(arena, callee),
            env.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
            has_parameters
        ),
        Lir::First(id) => format!("first({})", stringify_lir(arena, id)),
        Lir::Rest(id) => format!("rest({})", stringify_lir(arena, id)),
        Lir::Cons(first, rest) => format!(
            "cons({}, {})",
            stringify_lir(arena, first),
            stringify_lir(arena, rest)
        ),
        Lir::Listp(id, can_be_truthy) => format!(
            "listp({}, can_be_truthy={})",
            stringify_lir(arena, id),
            can_be_truthy
        ),
        Lir::Add(args) => format!(
            "add({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Sub(args) => format!(
            "sub({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Mul(args) => format!(
            "mul({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Div(dividend, divisor) => format!(
            "div({}, {})",
            stringify_lir(arena, dividend),
            stringify_lir(arena, divisor)
        ),
        Lir::Divmod(dividend, divisor) => format!(
            "divmod({}, {})",
            stringify_lir(arena, dividend),
            stringify_lir(arena, divisor)
        ),
        Lir::Mod(dividend, divisor) => format!(
            "mod({}, {})",
            stringify_lir(arena, dividend),
            stringify_lir(arena, divisor)
        ),
        Lir::Modpow(base, exponent, modulus) => format!(
            "modpow({}, {}, {})",
            stringify_lir(arena, base),
            stringify_lir(arena, exponent),
            stringify_lir(arena, modulus)
        ),
        Lir::Eq(left, right) => format!(
            "eq({}, {})",
            stringify_lir(arena, left),
            stringify_lir(arena, right)
        ),
        Lir::Gt(left, right) => format!(
            "gt({}, {})",
            stringify_lir(arena, left),
            stringify_lir(arena, right)
        ),
        Lir::GtBytes(left, right) => format!(
            "gt_bytes({}, {})",
            stringify_lir(arena, left),
            stringify_lir(arena, right)
        ),
        Lir::Not(id) => format!("not({})", stringify_lir(arena, id)),
        Lir::All(args) => format!(
            "all({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Any(args) => format!(
            "any({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::If(condition, then, otherwise, inline) => format!(
            "if({}, {}, {}, inline={})",
            stringify_lir(arena, condition),
            stringify_lir(arena, then),
            stringify_lir(arena, otherwise),
            inline
        ),
        Lir::Raise(args) => format!(
            "raise({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Concat(args) => format!(
            "concat({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Strlen(id) => format!("strlen({})", stringify_lir(arena, id)),
        Lir::Substr(string, start, length) => format!(
            "substr({}, {}, {})",
            stringify_lir(arena, string),
            stringify_lir(arena, start),
            length.map_or("None".to_string(), |id| stringify_lir(arena, id))
        ),
        Lir::Logand(args) => format!(
            "logand({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Logior(args) => format!(
            "logior({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Logxor(args) => format!(
            "logxor({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Lognot(id) => format!("lognot({})", stringify_lir(arena, id)),
        Lir::Ash(value, shift) => format!(
            "ash({}, {})",
            stringify_lir(arena, value),
            stringify_lir(arena, shift)
        ),
        Lir::Lsh(value, shift) => format!(
            "lsh({}, {})",
            stringify_lir(arena, value),
            stringify_lir(arena, shift)
        ),
        Lir::PubkeyForExp(id) => format!("pubkey_for_exp({})", stringify_lir(arena, id)),
        Lir::G1Add(args) => format!(
            "g1_add({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::G1Subtract(args) => format!(
            "g1_subtract({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::G1Multiply(left, right) => format!(
            "g1_multiply({}, {})",
            stringify_lir(arena, left),
            stringify_lir(arena, right)
        ),
        Lir::G1Negate(id) => format!("g1_negate({})", stringify_lir(arena, id)),
        Lir::G1Map(data, dst) => format!(
            "g1_map({}, {})",
            stringify_lir(arena, data),
            dst.map_or("None".to_string(), |id| stringify_lir(arena, id))
        ),
        Lir::G2Add(args) => format!(
            "g2_add({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::G2Subtract(args) => format!(
            "g2_subtract({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::G2Multiply(left, right) => format!(
            "g2_multiply({}, {})",
            stringify_lir(arena, left),
            stringify_lir(arena, right)
        ),
        Lir::G2Negate(id) => format!("g2_negate({})", stringify_lir(arena, id)),
        Lir::G2Map(data, dst) => format!(
            "g2_map({}, {})",
            stringify_lir(arena, data),
            dst.map_or("None".to_string(), |id| stringify_lir(arena, id))
        ),
        Lir::BlsPairingIdentity(args) => format!(
            "bls_pairing_identity({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::BlsVerify(sig, args) => format!(
            "bls_verify({}, {})",
            stringify_lir(arena, sig),
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Sha256(args) => format!(
            "sha256({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Sha256Inline(args) => format!(
            "sha256_inline({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Keccak256(args) => format!(
            "keccak256({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::Keccak256Inline(args) => format!(
            "keccak256_inline({})",
            args.iter()
                .map(|id| stringify_lir(arena, *id))
                .collect::<Vec<_>>()
                .join(", "),
        ),
        Lir::CoinId(parent_coin_info, puzzle_hash, amount) => format!(
            "coin_id({}, {}, {})",
            stringify_lir(arena, parent_coin_info),
            stringify_lir(arena, puzzle_hash),
            stringify_lir(arena, amount)
        ),
        Lir::K1Verify(public_key, message, signature) => format!(
            "k1_verify({}, {}, {})",
            stringify_lir(arena, public_key),
            stringify_lir(arena, message),
            stringify_lir(arena, signature)
        ),
        Lir::R1Verify(public_key, message, signature) => format!(
            "r1_verify({}, {}, {})",
            stringify_lir(arena, public_key),
            stringify_lir(arena, message),
            stringify_lir(arena, signature)
        ),
        Lir::Op(op, arg) => format!("{:?}({})", op, stringify_lir(arena, arg)),
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
}

impl ClvmOp {
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
        };

        bigint_atom(num.into())
    }
}

impl ToClvm<Allocator> for ClvmOp {
    fn to_clvm(&self, encoder: &mut Allocator) -> Result<NodePtr, clvm_traits::ToClvmError> {
        encoder
            .new_atom(&self.to_atom())
            .map_err(|_| ToClvmError::OutOfMemory)
    }
}
