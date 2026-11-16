use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use num_bigint::BigInt;
use rue_lir::{ClvmOp, atom_bigint};

use crate::{BinaryOp, Database, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};

/// The largest shift that CLVM allows, in either direction.
const MAX_SHIFT: u32 = 65535;

/// What [`may_fail`] needs to know about how the HIR is lowered.
pub trait FailureContext {
    fn db(&self) -> &Database;

    /// How many times the symbol is referenced. A binding that's never referenced isn't evaluated.
    fn references(&self, symbol: SymbolId) -> usize;

    /// The value that's evaluated where the symbol is referenced, if it's substituted there rather
    /// than bound to the environment. This is the value of an inline binding or constant, or of
    /// one that's inlined automatically, or the argument for a parameter of an inlined function.
    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId>;

    /// Whether creating a closure for the function (or calling it directly) evaluates a
    /// substituted argument that may fail, since the arguments it captures are evaluated then.
    fn captures_may_fail(&self, function: SymbolId) -> bool;

    /// Whether calling each function that has been checked may fail, which only depends on its
    /// body, so it's shared between checks.
    fn function_failures(&self) -> &RefCell<HashMap<SymbolId, bool>>;
}

/// Whether evaluating the HIR may fail for well-typed values, such as a solution that matches the
/// parameter types of `main`.
///
/// This is conservative, so it's `true` for anything that might fail: `raise` and `assert`,
/// operations that fail for some well-typed operands (such as division by zero, `substr` out of
/// range, or an invalid BLS point), and calls to functions whose bodies may fail, or that can't be
/// resolved. Reading a value doesn't fail, including a field of a parameter, since the solution
/// is assumed to match the types.
///
/// A substituted value (see [`FailureContext::substituted_value`]) is checked where it's
/// referenced, since that's where it's evaluated.
pub fn may_fail(ctx: &impl FailureContext, hir: HirId) -> bool {
    Visitor::new(ctx).visit(hir, false)
}

struct Visitor<'a, C> {
    ctx: &'a C,
    /// The substituted values being checked, so that a value that references itself isn't
    /// checked forever.
    visiting: HashSet<SymbolId>,
    /// The functions whose bodies are being checked.
    functions: HashSet<SymbolId>,
    /// Whether a function that's being checked was called again, in which case it was assumed
    /// not to fail, and the result of the functions that called it can't be cached until it's
    /// known.
    recursed: bool,
}

impl<'a, C: FailureContext> Visitor<'a, C> {
    fn new(ctx: &'a C) -> Self {
        Self {
            ctx,
            visiting: HashSet::new(),
            functions: HashSet::new(),
            recursed: false,
        }
    }

    fn any(&mut self, hirs: impl IntoIterator<Item = HirId>, in_body: bool) -> bool {
        hirs.into_iter().any(|hir| self.visit(hir, in_body))
    }

    /// Checks the HIR. Inside of a function body (`in_body`), the parameters and captures are
    /// read from the environment, so they can't fail. Their arguments are checked where the
    /// function is called instead.
    fn visit(&mut self, hir: HirId, in_body: bool) -> bool {
        // This follows chains of values and calls, which can be as deep as the program
        rue_types::grow_stack(|| self.visit_inner(hir, in_body))
    }

    fn visit_inner(&mut self, hir: HirId, in_body: bool) -> bool {
        let db = self.ctx.db();

        match db.hir(hir).clone() {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            // Constant expressions are evaluated at compile time
            | Hir::Const(_) => false,
            Hir::Lambda(lambda) => !in_body && self.ctx.captures_may_fail(lambda),
            Hir::Reference(symbol) => self.visit_reference(symbol, in_body),
            Hir::Block(block) => {
                self.visit_statements(&block.statements, in_body)
                    || self.any(block.reachable_body(), in_body)
            }
            Hir::If(condition, then, else_, _) => self.any([condition, then, else_], in_body),
            Hir::List(list) => self.any(list.items, in_body),
            Hir::Struct(initializer) => self.any(
                initializer.fields.into_iter().map(|field| field.value),
                in_body,
            ),
            Hir::FieldAccess(access) => self.visit(access.value, in_body),
            Hir::Cast(hir) => self.visit(hir, in_body),
            // The value is only evaluated if the check it's lowered to uses it
            Hir::Guard(guard) => guard.value_uses() > 0 && self.visit(guard.value, in_body),
            Hir::FunctionCall(call) => {
                self.visit_callee(call.function, in_body) || self.any(call.args, in_body)
            }
            Hir::Unary(op, hir) => {
                matches!(op, UnaryOp::G1Negate | UnaryOp::G2Negate) || self.visit(hir, in_body)
            }
            Hir::Binary(op, left, right) => {
                binary_may_fail(db, op, right) || self.any([left, right], in_body)
            }
            Hir::ClvmOp(op, args) => clvm_op_may_fail(op) || self.visit(args, in_body),
            Hir::CoinId(..)
            | Hir::Substr(..)
            | Hir::Modpow(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..) => true,
            Hir::Pair(first, rest) => self.any([first, rest], in_body),
        }
    }

    fn visit_statements(&mut self, statements: &[Statement], in_body: bool) -> bool {
        let db = self.ctx.db();

        for statement in crate::reachable_statements(statements) {
            let fails = match *statement {
                Statement::Raise(..) | Statement::Assert(..) => true,
                // Other bindings are evaluated at their statement, or where they're referenced
                // if they can't fail, unless they're never referenced
                Statement::Let(symbol, _) => match db.symbol(symbol) {
                    Symbol::Binding(binding)
                        if !binding.inline && self.ctx.references(symbol) > 0 =>
                    {
                        self.visit(binding.value.hir, in_body)
                    }
                    _ => false,
                },
                Statement::Expr(ref statement) => self.visit(statement.hir, in_body),
                Statement::If(ref statement) => {
                    self.any([statement.condition, statement.then], in_body)
                }
                Statement::Return(hir, _) => self.visit(hir, in_body),
                // Debug statements are only lowered with debug symbols, which are only used for
                // finding errors, so whether they fail doesn't matter
                Statement::Debug(..) => false,
            };

            if fails {
                return true;
            }
        }

        false
    }

    fn visit_reference(&mut self, symbol: SymbolId, in_body: bool) -> bool {
        match self.ctx.db().symbol(symbol) {
            Symbol::Function(_) => !in_body && self.ctx.captures_may_fail(symbol),
            Symbol::Parameter(_) if in_body => false,
            Symbol::Parameter(_) | Symbol::Binding(_) | Symbol::Constant(_) => {
                let Some(value) = self.ctx.substituted_value(symbol) else {
                    return false;
                };

                if !self.visiting.insert(symbol) {
                    return false;
                }

                let fails = self.visit(value, in_body);
                self.visiting.remove(&symbol);
                fails
            }
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => false,
        }
    }

    /// Whether calling the function may fail, other than in evaluating its arguments.
    fn visit_callee(&mut self, callee: HirId, in_body: bool) -> bool {
        let Some(function) = self.resolve_function(callee) else {
            // The function is only known at runtime, so it could do anything
            return true;
        };

        (!in_body && self.ctx.captures_may_fail(function)) || self.function_may_fail(function)
    }

    /// The function that's called, if it's known. This follows bindings, so that calling a
    /// lambda that's bound to a name is checked like calling the lambda itself.
    fn resolve_function(&self, mut callee: HirId) -> Option<SymbolId> {
        let db = self.ctx.db();
        let mut seen = HashSet::new();

        loop {
            match db.hir(db.without_casts(callee)) {
                Hir::Lambda(lambda) => return Some(*lambda),
                Hir::Reference(symbol) => match db.symbol(*symbol) {
                    Symbol::Function(_) => return Some(*symbol),
                    Symbol::Binding(binding) if seen.insert(*symbol) => {
                        callee = binding.value.hir;
                    }
                    _ => return None,
                },
                _ => return None,
            }
        }
    }

    fn function_may_fail(&mut self, function: SymbolId) -> bool {
        if let Some(&fails) = self.ctx.function_failures().borrow().get(&function) {
            return fails;
        }

        let Symbol::Function(symbol) = self.ctx.db().symbol(function) else {
            return true;
        };
        let body = symbol.body;

        // A recursive call is assumed not to fail, since the body fails anyway if anything else
        // in it does
        if !self.functions.insert(function) {
            self.recursed = true;
            return false;
        }

        let recursed = std::mem::replace(&mut self.recursed, false);
        let fails = self.visit(body, true);
        self.functions.remove(&function);

        // If a function that's still being checked was assumed not to fail, this function's
        // result depends on that assumption, unless it fails regardless
        if fails || !self.recursed || self.functions.is_empty() {
            self.ctx
                .function_failures()
                .borrow_mut()
                .insert(function, fails);
        }

        self.recursed |= recursed;
        fails
    }
}

/// Whether the operation may fail for well-typed operands, given the right operand. Division
/// and shifts only fail for some right operands, so they can't fail if it's a suitable constant.
fn binary_may_fail(db: &Database, op: BinaryOp, right: HirId) -> bool {
    match op {
        BinaryOp::Div | BinaryOp::Mod | BinaryOp::Divmod => {
            constant_int(db, right).is_none_or(|divisor| divisor == BigInt::ZERO)
        }
        BinaryOp::LeftArithmeticShift
        | BinaryOp::RightArithmeticShift
        | BinaryOp::RightLogicalShift => {
            constant_int(db, right).is_none_or(|shift| shift.magnitude() > &MAX_SHIFT.into())
        }
        BinaryOp::G1Add
        | BinaryOp::G1Subtract
        | BinaryOp::G1Multiply
        | BinaryOp::G2Add
        | BinaryOp::G2Subtract
        | BinaryOp::G2Multiply => true,
        BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Concat
        | BinaryOp::Gt
        | BinaryOp::Lt
        | BinaryOp::Gte
        | BinaryOp::Lte
        | BinaryOp::GtBytes
        | BinaryOp::LtBytes
        | BinaryOp::GteBytes
        | BinaryOp::LteBytes
        | BinaryOp::Eq
        | BinaryOp::Ne
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::All
        | BinaryOp::Any
        | BinaryOp::BitwiseAnd
        | BinaryOp::BitwiseOr
        | BinaryOp::BitwiseXor => false,
    }
}

/// Whether the builtin operation may fail for well-typed arguments. These are only created for
/// builtins that take a list of arguments.
fn clvm_op_may_fail(op: ClvmOp) -> bool {
    !matches!(
        op,
        ClvmOp::Sha256
            | ClvmOp::Keccak256
            | ClvmOp::Concat
            | ClvmOp::Add
            | ClvmOp::Sub
            | ClvmOp::Mul
            | ClvmOp::Any
            | ClvmOp::All
    )
}

/// The value of the integer, if it's known at compile time.
fn constant_int(db: &Database, hir: HirId) -> Option<BigInt> {
    match db.hir(db.without_casts(hir)) {
        Hir::Int(value) => Some(value.clone()),
        Hir::Bytes(bytes) => Some(atom_bigint(bytes)),
        Hir::Nil => Some(BigInt::ZERO),
        Hir::Const(expr) => expr.result.and_then(|result| constant_int(db, result)),
        _ => None,
    }
}
