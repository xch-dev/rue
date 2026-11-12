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

impl Hir {
    /// The nodes directly inside of this one, in the order they're written. A block's statements
    /// are included (see [`Statement::hirs`]), but the values of its bindings and a lambda's body
    /// are stored in their symbols, so they aren't. A struct initializer includes the default
    /// values of the fields that were left out.
    pub fn children(&self) -> impl Iterator<Item = HirId> + '_ {
        let children = match self {
            Self::Unresolved
            | Self::Nil
            | Self::String(_)
            | Self::Int(_)
            | Self::Bytes(_)
            | Self::Bool(_)
            | Self::Reference(_)
            | Self::Lambda(_)
            | Self::InfinityG1
            | Self::InfinityG2 => Children::default(),
            // The result of a const expression is generated, so only the expression is included
            Self::Const(expr) => Children::fixed([Some(expr.value), None, None]),
            Self::List(list) => Children::list(None, &list.items),
            Self::Struct(initializer) => Children {
                fields: &initializer.fields,
                ..Children::default()
            },
            Self::FieldAccess(access) => Children::fixed([Some(access.value), None, None]),
            Self::Guard(guard) => Children::fixed([Some(guard.value), None, None]),
            Self::Cast(hir) | Self::Unary(_, hir) | Self::ClvmOp(_, hir) => {
                Children::fixed([Some(*hir), None, None])
            }
            Self::Block(block) => Children {
                statements: &block.statements,
                body: block.body,
                ..Children::default()
            },
            Self::Pair(a, b) | Self::Binary(_, a, b) => Children::fixed([Some(*a), Some(*b), None]),
            Self::If(a, b, c, _)
            | Self::CoinId(a, b, c)
            | Self::Modpow(a, b, c)
            | Self::Secp256K1Verify(a, b, c)
            | Self::Secp256R1Verify(a, b, c) => Children::fixed([Some(*a), Some(*b), Some(*c)]),
            Self::FunctionCall(call) => Children::list(Some(call.function), &call.args),
            Self::Substr(a, b, c) => Children::fixed([Some(*a), Some(*b), *c]),
            Self::G1Map(a, b) | Self::G2Map(a, b) => Children::fixed([Some(*a), *b, None]),
            Self::BlsPairingIdentity(args) => Children::list(None, args),
            Self::BlsVerify(a, args) => Children::list(Some(*a), args),
        };

        children.into_iter()
    }
}

/// The parts that the children of a node are made of, in the order they're written, so that they
/// can be iterated over without collecting them.
#[derive(Default)]
struct Children<'a> {
    fixed: [Option<HirId>; 3],
    list: &'a [HirId],
    fields: &'a [StructField],
    statements: &'a [Statement],
    body: Option<HirId>,
}

impl<'a> Children<'a> {
    fn fixed(fixed: [Option<HirId>; 3]) -> Self {
        Self {
            fixed,
            ..Self::default()
        }
    }

    fn list(first: Option<HirId>, list: &'a [HirId]) -> Self {
        Self {
            fixed: [first, None, None],
            list,
            ..Self::default()
        }
    }

    fn into_iter(self) -> impl Iterator<Item = HirId> + 'a {
        self.fixed
            .into_iter()
            .flatten()
            .chain(self.list.iter().copied())
            .chain(self.fields.iter().map(|field| field.value))
            .chain(self.statements.iter().flat_map(Statement::hirs))
            .chain(self.body)
    }
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

impl StructInitializer {
    /// The values of the fields that were written in the initializer, without the default values
    /// of the fields that were left out. Those are shared by every initializer that uses them, so
    /// they belong to the struct declaration instead.
    pub fn written_values(&self) -> impl Iterator<Item = HirId> + '_ {
        self.fields
            .iter()
            .filter(|field| !field.is_default)
            .map(|field| field.value)
    }
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

impl Block {
    /// The statements that are evaluated. See [`reachable_statements`].
    pub fn reachable_statements(&self) -> &[Statement] {
        reachable_statements(&self.statements)
    }

    /// The body, if it's evaluated. See [`reachable_body`].
    pub fn reachable_body(&self) -> Option<HirId> {
        reachable_body(&self.statements, self.body)
    }
}

/// The statements that are evaluated, which end at the first `return` or `raise`. Nothing after it
/// is lowered, so analyses that follow lowering (such as the dependency graph) should only look at
/// these statements.
pub fn reachable_statements(statements: &[Statement]) -> &[Statement] {
    match first_exit(statements) {
        Some(index) => &statements[..=index],
        None => statements,
    }
}

/// The body that follows the statements, if it's evaluated, which it isn't if one of the
/// statements is a `return` or `raise`.
pub fn reachable_body(statements: &[Statement], body: Option<HirId>) -> Option<HirId> {
    if first_exit(statements).is_some() {
        None
    } else {
        body
    }
}

fn first_exit(statements: &[Statement]) -> Option<usize> {
    statements
        .iter()
        .position(|statement| matches!(statement, Statement::Return(..) | Statement::Raise(..)))
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(ExprStatement),
    Let(SymbolId, SrcLoc),
    If(IfStatement),
    Return(HirId, SrcLoc),
    Assert(HirId, SrcLoc),
    Raise(Option<HirId>, SrcLoc),
    Debug(HirId, SrcLoc),
}

impl Statement {
    /// The nodes directly inside of the statement, in the order they're written. A binding's
    /// value is stored in its symbol, so a `let` statement doesn't have any.
    pub fn hirs(&self) -> impl Iterator<Item = HirId> + use<> {
        let hirs = match self {
            Self::Let(..) => [None, None],
            Self::Expr(statement) => [Some(statement.hir), None],
            Self::If(statement) => [Some(statement.condition), Some(statement.then)],
            Self::Return(hir, _) | Self::Assert(hir, _) | Self::Debug(hir, _) => [Some(*hir), None],
            Self::Raise(hir, _) => [*hir, None],
        };

        hirs.into_iter().flatten()
    }

    /// The location of the statement in the source code.
    pub fn srcloc(&self) -> &SrcLoc {
        match self {
            Self::Expr(statement) => &statement.srcloc,
            Self::If(statement) => &statement.srcloc,
            Self::Let(_, srcloc)
            | Self::Return(_, srcloc)
            | Self::Assert(_, srcloc)
            | Self::Raise(_, srcloc)
            | Self::Debug(_, srcloc) => srcloc,
        }
    }
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
