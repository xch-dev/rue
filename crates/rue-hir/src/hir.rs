use id_arena::Id;
use num_bigint::BigInt;
use rue_diagnostic::SrcLoc;
use rue_lir::ClvmOp;
use rue_types::{Check, TypeId};

use crate::{
    BinaryOp, BindingSymbol, Database, Symbol, SymbolId, TypePath, UnaryOp, Value,
    analysis::infallible_power_atom, check_value_uses,
};

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
    /// A power (`base ** exponent`), which is lowered to the multiplications it's expanded into.
    Power(Power),
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
    /// Checks that the Keccak-256 hash of the value is the hash.
    Keccak256Verify(HirId, HirId),
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
            // The expansion is generated too, so only the base and the exponent are included
            Self::Power(power) => Children::fixed([Some(power.base), Some(power.exponent), None]),
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
            Self::Pair(a, b) | Self::Binary(_, a, b) | Self::Keccak256Verify(a, b) => {
                Children::fixed([Some(*a), Some(*b), None])
            }
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
    /// Whether the compiler generated the const expression for a value that's made of constants,
    /// rather than it being written as `const { ... }`. If it can't be evaluated, the value is
    /// its result instead, so that it's evaluated at runtime, where it fails the same way.
    pub generated: bool,
}

/// A power (`base ** exponent`). CLVM has no operator for it (`modpow` takes a modulus, and is only
/// allowed after hard fork 2), so the exponent is evaluated at compile time, and the power is
/// expanded into the product of that many copies of the base: `x ** 3` is `x * x * x`.
#[derive(Debug, Clone, Copy)]
pub struct Power {
    pub base: HirId,
    /// The exponent, which is a [`Hir::Const`], since it's evaluated at compile time.
    pub exponent: HirId,
    /// The multiplications that the power is expanded into (see [`expand_power`]), once the
    /// exponent has been evaluated. Like the result of a const expression, this is what's
    /// evaluated at runtime, rather than the base and the exponent.
    pub expansion: Option<HirId>,
}

impl Power {
    /// What's evaluated at runtime: the expansion, or the base and the exponent as they're
    /// written if the exponent hasn't been evaluated, which only happens if there's an error.
    pub fn evaluated(&self) -> impl Iterator<Item = HirId> + use<> {
        let hirs = match self.expansion {
            Some(expansion) => [Some(expansion), None],
            None => [Some(self.base), Some(self.exponent)],
        };

        hirs.into_iter().flatten()
    }
}

/// The most copies of the base that a power is expanded into. A power is compiled to a
/// multiplication with one operand per copy, which is two bytes each for a base that's read from
/// the environment, so this bounds the size of the code it generates, and how deeply the
/// multiplications are nested. It's high enough for `2 ** 256`.
pub const MAX_POWER_EXPONENT: u32 = 256;

/// Expands `base ** exponent` into the multiplications it's compiled to, which are the ones that
/// `base * base * ... * base` with `exponent` copies of the base is compiled to, in the same
/// order. So the power fails with `operand_limits` exactly when that product does, and the
/// failure analysis and the optimizer treat it the same way. A power of a constant that can't
/// fail is the number instead, which is what the product is folded to.
///
/// A base that's a reference, a field of one, or a small enough expression of constants is
/// repeated as it is (see [`is_repeatable`]). Anything else is bound first, so that it's only
/// evaluated once. With an exponent of zero the result is `1` whatever the base is (including
/// zero), and the base is bound, so that it's still evaluated for its effect if it may fail, like
/// the value of a binding that's never referenced.
pub fn expand_power(db: &mut Database, base: Value, srcloc: SrcLoc, exponent: u32) -> HirId {
    // A power of a constant that can't fail on any target is the number it evaluates to, like
    // the product is once it's folded, so `10 ** 18` is that number in a debug build too, and
    // `(2 ** 6) ** 100` is a power of `64`
    if let Some(atom) = infallible_power_atom(db, base.hir, exponent) {
        return db.alloc_hir(Hir::Bytes(atom));
    }

    let bind = exponent == 0 || !is_repeatable(db, base.hir, exponent);

    let body = |db: &mut Database, factor: HirId| match exponent {
        0 => db.alloc_hir(Hir::Int(BigInt::from(1))),
        // The product as it's written by hand: `((x * x) * x) * x`
        _ => (1..exponent).fold(factor, |product, _| {
            db.alloc_hir(Hir::Binary(BinaryOp::Mul, product, factor))
        }),
    };

    if !bind {
        return body(db, base.hir);
    }

    let symbol = db.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: None,
        value: base,
        inline: false,
    }));
    let reference = db.alloc_hir(Hir::Reference(symbol));
    let body = body(db, reference);

    db.alloc_hir(Hir::Block(Block {
        statements: vec![Statement::Let(symbol, srcloc)],
        body: Some(body),
    }))
}

/// The most literals and operators that the copies of a base made of constants can have between
/// them and still be repeated, rather than the base being bound. The copies are folded into one
/// constant, but until then there's one of the base for each, and a base can itself be a power,
/// so without a limit the copies of `((2 ** 50) ** 50) ** 50` would multiply.
const MAX_REPEATED_CONSTANT_SIZE: usize = 1024;

/// Whether evaluating the value again for each of the copies costs no more than binding it: it's
/// a reference or a field that's found by following a path from one, which are read from the
/// environment, or it's an expression of constants that's small enough to repeat that many times,
/// which is folded. There's at least one copy.
fn is_repeatable(db: &Database, hir: HirId, copies: u32) -> bool {
    match db.hir(hir) {
        Hir::Reference(_) => true,
        Hir::Cast(value)
        | Hir::FieldAccess(FieldAccess {
            value,
            kind: FieldKind::Path(_),
            ..
        }) => is_repeatable(db, *value, copies),
        _ => {
            let mut budget = MAX_REPEATED_CONSTANT_SIZE / copies as usize;
            is_small_constant(db, hir, &mut budget)
        }
    }
}

/// Whether the value is made of literals and operators only, so that it doesn't depend on
/// anything that's only known at runtime, and has no more of them than the budget, which is
/// reduced by how many it has. The product of its copies is folded like the product of the number
/// it evaluates to.
fn is_small_constant(db: &Database, hir: HirId, budget: &mut usize) -> bool {
    let mut take = |size: usize| match budget.checked_sub(size) {
        Some(rest) => {
            *budget = rest;
            true
        }
        None => false,
    };

    // A long literal is counted as several, since it's written out for each copy until the
    // copies are folded, which they aren't if the product fails
    let literal = |bytes: usize| 1 + bytes / 8;

    match db.hir(hir) {
        Hir::Nil => take(1),
        Hir::Int(value) => take(literal(value.bits().div_ceil(8) as usize)),
        Hir::Bytes(bytes) => take(literal(bytes.len())),
        // A const expression is replaced with its result
        Hir::Const(expr) => match expr.result {
            Some(result) => is_small_constant(db, result, budget),
            None => take(1),
        },
        Hir::Cast(value) => is_small_constant(db, *value, budget),
        Hir::Unary(_, value) => take(1) && is_small_constant(db, *value, budget),
        Hir::Binary(_, left, right) => {
            take(1) && is_small_constant(db, *left, budget) && is_small_constant(db, *right, budget)
        }
        // A power is as large as what it's expanded into
        Hir::Power(power) => power
            .expansion
            .is_some_and(|expansion| is_small_constant(db, expansion, budget)),
        _ => false,
    }
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

/// The statements that are evaluated, which end at the first `return`, `raise` or statement whose
/// branches all end in one (see [`ControlStatement`]). Nothing after it is lowered, so analyses
/// that follow lowering (such as the dependency graph) should only look at these statements.
pub fn reachable_statements(statements: &[Statement]) -> &[Statement] {
    match first_exit(statements) {
        Some(index) => &statements[..=index],
        None => statements,
    }
}

/// The body that follows the statements, if it's evaluated, which it isn't if one of the
/// statements is a `return`, `raise` or statement whose branches all end in one.
pub fn reachable_body(statements: &[Statement], body: Option<HirId>) -> Option<HirId> {
    if first_exit(statements).is_some() {
        None
    } else {
        body
    }
}

/// Whether the statements never reach their end, since every path through them ends in a `return`
/// or `raise`.
pub fn statements_terminate(statements: &[Statement]) -> bool {
    first_exit(statements).is_some()
}

fn first_exit(statements: &[Statement]) -> Option<usize> {
    statements.iter().position(|statement| match statement {
        Statement::Return(..) | Statement::Raise(..) => true,
        Statement::Control(statement) => statement.fallthroughs == 0,
        _ => false,
    })
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(ExprStatement),
    Let(SymbolId, SrcLoc),
    If(IfStatement),
    Control(ControlStatement),
    Return(HirId, SrcLoc),
    /// The condition, and the message if there is one. Like the value of a `raise`, the message
    /// is only lowered with debug symbols, and only evaluated once the condition is false, so it
    /// never changes whether the program fails.
    Assert(HirId, Option<HirId>, SrcLoc),
    Raise(Option<HirId>, SrcLoc),
    Debug(HirId, SrcLoc),
}

impl Statement {
    /// The nodes directly inside of the statement, in the order they're written. A binding's
    /// value is stored in its symbol, so a `let` statement doesn't have any.
    pub fn hirs(&self) -> impl Iterator<Item = HirId> + use<> {
        self.lowered_hirs(true)
    }

    /// The nodes directly inside of the statement that are lowered, in the order they're written.
    /// A `debug` statement, the value of a `raise` and the message of an `assert` are only lowered
    /// with debug symbols, so a release build doesn't reference anything in them.
    pub fn lowered_hirs(&self, debug_symbols: bool) -> impl Iterator<Item = HirId> + use<> {
        let hirs = match self {
            Self::Let(..) => [None, None],
            Self::Expr(statement) => [Some(statement.hir), None],
            Self::Control(statement) => [Some(statement.hir), None],
            Self::If(statement) => [Some(statement.condition), Some(statement.then)],
            Self::Return(hir, _) => [Some(*hir), None],
            Self::Debug(hir, _) => [debug_symbols.then_some(*hir), None],
            Self::Assert(condition, message, _) => {
                [Some(*condition), message.filter(|_| debug_symbols)]
            }
            Self::Raise(hir, _) => [hir.filter(|_| debug_symbols), None],
        };

        hirs.into_iter().flatten()
    }

    /// The location of the statement in the source code.
    pub fn srcloc(&self) -> &SrcLoc {
        match self {
            Self::Expr(statement) => &statement.srcloc,
            Self::If(statement) => &statement.srcloc,
            Self::Control(statement) => &statement.srcloc,
            Self::Let(_, srcloc)
            | Self::Return(_, srcloc)
            | Self::Assert(_, _, srcloc)
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

/// An `if` and `else` chain or a `match` used as a statement, whose branches are statement blocks
/// rather than values. A branch can end in a `return` or `raise`, or fall through to the rest of
/// the block after the statement, which is its continuation.
///
/// The node is the chain of [`Hir::If`] nodes (for a `match`, inside the [`Hir::Block`] that binds
/// its value), whose leaves are the branches' blocks, which have no body.
#[derive(Debug, Clone)]
pub struct ControlStatement {
    pub hir: HirId,
    /// The number of branches that fall through to the rest of the block (see
    /// [`control_fallthroughs`]). If it's zero, every branch ends in a `return` or `raise`, so
    /// nothing after the statement is evaluated.
    pub fallthroughs: usize,
    /// Whether a branch can return from the function, so the rest of the block isn't always
    /// evaluated after it, even if the branch doesn't raise (see [`control_may_return`]).
    pub may_return: bool,
    /// Whether every branch is evaluated, like the branches of an `inline if`. A branch that falls
    /// through evaluates the rest of the block too, so it's evaluated even if a branch returns.
    pub inline: bool,
    pub srcloc: SrcLoc,
}

/// The number of branches of a [`ControlStatement`]'s node that fall through to the rest of the
/// block, which is the number of places the rest is reached from. It's zero if every branch ends
/// in a `return` or `raise`.
pub fn control_fallthroughs(db: &Database, hir: HirId) -> usize {
    match db.hir(hir) {
        Hir::If(_, then, else_, _) => {
            control_fallthroughs(db, *then) + control_fallthroughs(db, *else_)
        }
        Hir::Block(block) => {
            if statements_terminate(&block.statements) {
                0
            } else {
                block.body.map_or(1, |body| control_fallthroughs(db, body))
            }
        }
        _ => 1,
    }
}

/// Whether a branch of a [`ControlStatement`]'s node can return from the function.
pub fn control_may_return(db: &Database, hir: HirId) -> bool {
    match db.hir(hir) {
        Hir::If(_, then, else_, _) => {
            control_may_return(db, *then) || control_may_return(db, *else_)
        }
        Hir::Block(block) => {
            block.statements.iter().any(|statement| match statement {
                Statement::Return(..) => true,
                Statement::If(statement) => control_may_return(db, statement.then),
                Statement::Control(statement) => statement.may_return,
                Statement::Expr(_)
                | Statement::Let(..)
                | Statement::Assert(..)
                | Statement::Raise(..)
                | Statement::Debug(..) => false,
            }) || block.body.is_some_and(|body| control_may_return(db, body))
        }
        _ => false,
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub function: HirId,
    pub args: Vec<HirId>,
    pub nil_terminated: bool,
}
