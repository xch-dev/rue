use clvm_traits::{ToClvm, ToClvmError};
use clvmr::{Allocator, NodePtr};
use derive_more::Display;
use id_arena::Id;
use strum::EnumIter;

pub type LirId = Id<Lir>;

#[derive(Debug, Clone)]
pub enum Lir {
    Atom(Vec<u8>),
    Path(u32),
    Quote(LirId),
    Run(LirId, LirId),
    Closure(LirId, Vec<LirId>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter, Display)]
pub enum ClvmOp {
    #[display("quote")]
    Quote,

    #[display("apply")]
    Apply,

    #[display("if")]
    If,

    #[display("cons")]
    Cons,

    #[display("first")]
    First,

    #[display("rest")]
    Rest,

    #[display("listp")]
    Listp,

    #[display("raise")]
    Raise,

    #[display("eq")]
    Eq,

    #[display("gt_bytes")]
    GtBytes,

    #[display("sha256")]
    Sha256,

    #[display("substr")]
    Substr,

    #[display("strlen")]
    Strlen,

    #[display("concat")]
    Concat,

    #[display("add")]
    Add,

    #[display("sub")]
    Sub,

    #[display("mul")]
    Mul,

    #[display("div")]
    Div,

    #[display("divmod")]
    Divmod,

    #[display("gt")]
    Gt,

    #[display("ash")]
    Ash,

    #[display("lsh")]
    Lsh,

    #[display("logand")]
    Logand,

    #[display("logior")]
    Logior,

    #[display("logxor")]
    Logxor,

    #[display("lognot")]
    Lognot,

    #[display("not")]
    Not,

    #[display("any")]
    Any,

    #[display("all")]
    All,

    #[display("modpow")]
    Modpow,

    #[display("mod")]
    Mod,

    #[display("coinid")]
    CoinId,

    #[display("pubkey_for_exp")]
    PubkeyForExp,

    #[display("g1_add")]
    G1Add,

    #[display("g1_subtract")]
    G1Subtract,

    #[display("g1_multiply")]
    G1Multiply,

    #[display("g1_negate")]
    G1Negate,

    #[display("g1_map")]
    G2Add,

    #[display("g2_subtract")]
    G2Subtract,

    #[display("g2_multiply")]
    G2Multiply,

    #[display("g2_negate")]
    G2Negate,

    #[display("g1_map")]
    G1Map,

    #[display("g2_map")]
    G2Map,

    #[display("bls_pairing_identity")]
    BlsPairingIdentity,

    #[display("bls_verify")]
    BlsVerify,

    #[display("secp256k1_verify")]
    Secp256K1Verify,

    #[display("secp256r1_verify")]
    Secp256R1Verify,

    #[display("keccak256")]
    Keccak256,
}

impl ClvmOp {
    pub fn to_atom(self) -> &'static [u8] {
        match self {
            Self::Quote => &[1],
            Self::Apply => &[2],
            Self::If => &[3],
            Self::Cons => &[4],
            Self::First => &[5],
            Self::Rest => &[6],
            Self::Listp => &[7],
            Self::Raise => &[8],
            Self::Eq => &[9],
            Self::GtBytes => &[10],
            Self::Sha256 => &[11],
            Self::Substr => &[12],
            Self::Strlen => &[13],
            Self::Concat => &[14],
            Self::Add => &[16],
            Self::Sub => &[17],
            Self::Mul => &[18],
            Self::Div => &[19],
            Self::Divmod => &[20],
            Self::Gt => &[21],
            Self::Ash => &[22],
            Self::Lsh => &[23],
            Self::Logand => &[24],
            Self::Logior => &[25],
            Self::Logxor => &[26],
            Self::Lognot => &[27],
            Self::Not => &[32],
            Self::Any => &[33],
            Self::All => &[34],
            Self::Modpow => &[60],
            Self::Mod => &[61],
            Self::PubkeyForExp => &[30],
            Self::CoinId => &[48],
            Self::G1Add => &[29],
            Self::G1Subtract => &[49],
            Self::G1Multiply => &[50],
            Self::G1Negate => &[51],
            Self::G2Add => &[52],
            Self::G2Subtract => &[53],
            Self::G2Multiply => &[54],
            Self::G2Negate => &[55],
            Self::G1Map => &[56],
            Self::G2Map => &[57],
            Self::BlsPairingIdentity => &[58],
            Self::BlsVerify => &[59],
            Self::Secp256K1Verify => &[13, 0xd6, 0x1f, 0x00],
            Self::Secp256R1Verify => &[0x1c, 0x3a, 0x8f, 0x00],
            Self::Keccak256 => &[62],
        }
    }
}

impl ToClvm<Allocator> for ClvmOp {
    fn to_clvm(&self, encoder: &mut Allocator) -> Result<NodePtr, clvm_traits::ToClvmError> {
        encoder
            .new_atom(self.to_atom())
            .map_err(|_| ToClvmError::OutOfMemory)
    }
}
