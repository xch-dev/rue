use id_arena::Id;
use num_bigint::BigInt;
use rue_diagnostic::SrcLoc;
use rue_lir::ClvmOp;

use crate::{BinaryOp, SymbolId, UnaryOp};

pub type HirId = Id<Hir>;

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Nil,
    String(String),
    Int(BigInt),
    Bytes(Vec<u8>),
    Bool(bool),
    Pair(HirId, HirId),
    Reference(SymbolId),
    Block(Block),
    Lambda(SymbolId),
    If(HirId, HirId, HirId, bool),
    FunctionCall(FunctionCall),
    Unary(UnaryOp, HirId),
    Binary(BinaryOp, HirId, HirId),
    CoinId(HirId, HirId, HirId),
    Substr(HirId, HirId, Option<HirId>),
    G1Map(HirId, Option<HirId>),
    G2Map(HirId, Option<HirId>),
    Modpow(HirId, HirId, HirId),
    BlsPairingIdentity(Vec<HirId>),
    BlsVerify(HirId, Vec<HirId>),
    Secp256K1Verify(HirId, HirId, HirId),
    Secp256R1Verify(HirId, HirId, HirId),
    InfinityG1,
    InfinityG2,
    ClvmOp(ClvmOp, HirId),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub body: Option<HirId>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(ExprStatement),
    Let(SymbolId),
    If(IfStatement),
    Return(HirId),
    Assert(HirId, SrcLoc),
    Raise(Option<HirId>, SrcLoc),
}

#[derive(Debug, Clone, Copy)]
pub struct ExprStatement {
    pub hir: HirId,
    pub always_nil: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct IfStatement {
    pub condition: HirId,
    pub then: HirId,
    pub inline: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: HirId,
    pub args: Vec<HirId>,
    pub nil_terminated: bool,
}
