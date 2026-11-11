use id_arena::Id;
use num_bigint::BigInt;
use rue_diagnostic::SrcLoc;
use rue_lir::ClvmOp;
use rue_types::{Check, TypeId};

use crate::{BinaryOp, SymbolId, TypePath, UnaryOp, check_value_uses};

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
    /// A list literal, which is lowered to nested pairs.
    List(List),
    /// A struct initializer, which is lowered to nested pairs.
    Struct(StructInitializer),
    /// A field access, which is lowered to the operations that extract the field.
    FieldAccess(FieldAccess),
    /// A cast (`value as T`), which is lowered to the value, since it has no effect at runtime.
    Cast(HirId),
    /// A type guard (`value is T`), which is lowered to the check it performs.
    Guard(Guard),
    Const(ConstExpr),
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
pub struct List {
    pub items: Vec<HirId>,
    /// Whether the list ends in nil, rather than the last item being spread into it.
    pub nil_terminated: bool,
}

#[derive(Debug, Clone)]
pub struct StructInitializer {
    pub ty: TypeId,
    /// Every field of the struct, in the order they're declared.
    pub fields: Vec<StructField>,
    pub nil_terminated: bool,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub value: HirId,
    /// Whether the field was left out, so the value is the field's default. The default value is
    /// shared by every struct initializer that uses it.
    pub is_default: bool,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub value: HirId,
    pub name: String,
    pub kind: FieldKind,
}

#[derive(Debug, Clone)]
pub enum FieldKind {
    /// A field of a pair or struct, which is found by following the path from the value. An
    /// empty path is the value itself, such as the only field of a struct that isn't
    /// nil-terminated.
    Path(Vec<TypePath>),
    /// The length of an atom.
    Length,
}

#[derive(Debug, Clone)]
pub struct Guard {
    pub value: HirId,
    /// The type the value is being checked against.
    pub ty: TypeId,
    /// The check to perform at runtime, before it's simplified.
    pub check: Check,
}

impl Guard {
    /// The check that's performed at runtime, which is what the guard is lowered to.
    pub fn runtime_check(&self) -> Check {
        self.check.clone().simplify()
    }

    /// The number of times the value is used by the runtime check, which may be none if there's
    /// nothing to check.
    pub fn value_uses(&self) -> usize {
        check_value_uses(&self.runtime_check())
    }
}

#[derive(Debug, Clone)]
pub struct ConstExpr {
    pub value: HirId,
    pub loc: SrcLoc,
    /// The result of evaluating the expression, once it has been evaluated.
    pub result: Option<HirId>,
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
    Debug(HirId, SrcLoc),
}

#[derive(Debug, Clone)]
pub struct ExprStatement {
    pub hir: HirId,
    pub always_nil: bool,
    pub srcloc: SrcLoc,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: HirId,
    pub then: HirId,
    pub inline: bool,
    pub srcloc: SrcLoc,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: HirId,
    pub args: Vec<HirId>,
    pub nil_terminated: bool,
}
