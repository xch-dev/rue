//! Estimates of what code costs on the Chia blockchain, which lints use to decide whether a change
//! would make a program cheaper.
//!
//! A program's cost is the cost of running it, plus [`COST_PER_BYTE`] for each byte of it. The
//! estimates follow how the HIR is usually lowered, and the costs of the CLVM operators, but they
//! don't optimize or lower the code, so they're only accurate to within a few bytes. The sizes
//! were checked against the output of the compiler for the cases that the lints compare.

use std::collections::HashMap;

use rue_hir::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};
use rue_lir::ClvmOp;

/// The cost of each byte of a program, which is usually much more than the cost of running it.
pub(crate) const COST_PER_BYTE: u64 = 12_000;

/// The size of a new `let` binding, which wraps the code that uses it in a function that it's
/// passed to. Referencing the binding is about one byte, like referencing a parameter. A binding
/// can be smaller if it's added to an environment that already exists, but that depends on what
/// its value references, so the larger size is used to avoid suggesting bindings that don't help.
pub(crate) const BINDING_SIZE: u64 = 14;

/// The cost of using a value that's stored in a parameter or binding.
pub(crate) const REFERENCE: Cost = Cost::new(1, PATH);

/// The runtime of calling a function that calls itself, which can't be estimated, so it's assumed
/// to be about as much as a few hashes for each of several levels of recursion.
const RECURSIVE_RUNTIME: u64 = 100_000;

/// The runtime of the operators that every expression is made of.
const QUOTE: u64 = 20;
const PATH: u64 = 44;
const APPLY: u64 = 90;
const CONS: u64 = 50;
const FIRST: u64 = 30;
const IF: u64 = 33;

/// The size of a value assumed by the operators whose cost depends on the size of their arguments.
const ASSUMED_BYTES: u64 = 32;

/// The estimated size and runtime of some code.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Cost {
    /// The number of bytes that the code is serialized to.
    pub(crate) size: u64,
    /// The cost of running the code once.
    pub(crate) runtime: u64,
}

impl Cost {
    const fn new(size: u64, runtime: u64) -> Self {
        Self { size, runtime }
    }

    /// The total cost of including the code in a program and running it once.
    pub(crate) fn total(self) -> u64 {
        self.size
            .saturating_mul(COST_PER_BYTE)
            .saturating_add(self.runtime)
    }
}

impl std::ops::Add for Cost {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            size: self.size.saturating_add(other.size),
            runtime: self.runtime.saturating_add(other.runtime),
        }
    }
}

impl std::iter::Sum for Cost {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Self::default(), |a, b| a + b)
    }
}

/// The size and runtime of calling a function that isn't inline with the given number of
/// arguments, besides the arguments themselves and the function's body.
pub(crate) fn call_overhead(args: usize) -> Cost {
    let args = args as u64;

    // `(a f (c a1 (c a2 ... an)))`, where the last argument is the rest of the environment
    Cost::new(
        6 + 5 * args.saturating_sub(1) + u64::from(args == 0),
        APPLY + PATH + CONS * args.saturating_sub(1) + PATH * args,
    )
}

/// Estimates the cost of HIR, and remembers the cost of each function body that it estimates.
pub(crate) struct CostModel<'a> {
    db: &'a Database,
    /// The cost of each function's body, or `None` while it's being estimated, so that a function
    /// that calls itself is found.
    functions: &'a mut HashMap<SymbolId, Option<Cost>>,
}

impl<'a> CostModel<'a> {
    pub(crate) fn new(
        db: &'a Database,
        functions: &'a mut HashMap<SymbolId, Option<Cost>>,
    ) -> Self {
        Self { db, functions }
    }

    /// The cost of a function's body, including the bindings declared in it.
    fn function_body(&mut self, symbol: SymbolId) -> Cost {
        match self.functions.get(&symbol) {
            Some(Some(cost)) => return *cost,
            // The function calls itself, so the body is already being counted
            Some(None) => return Cost::new(0, RECURSIVE_RUNTIME),
            None => {}
        }

        let Symbol::Function(function) = self.db.symbol(symbol) else {
            return Cost::default();
        };

        let body = function.body;

        self.functions.insert(symbol, None);
        let cost = self.estimate(body);
        self.functions.insert(symbol, Some(cost));
        cost
    }

    /// The cost of an expression, which is evaluated once.
    pub(crate) fn estimate(&mut self, hir: HirId) -> Cost {
        let db = self.db;

        match db.hir(hir) {
            Hir::Unresolved => Cost::default(),
            Hir::Nil | Hir::Bool(false) => Cost::new(1, QUOTE),
            Hir::Bool(true) => Cost::new(3, QUOTE),
            Hir::Int(value) => quoted(value.to_signed_bytes_be().len()),
            Hir::Bytes(value) => quoted(value.len()),
            Hir::String(value) => quoted(value.len()),
            Hir::InfinityG1 => quoted(48),
            Hir::InfinityG2 => quoted(96),
            Hir::Pair(first, rest) => op(2, CONS) + self.all([*first, *rest]),
            Hir::List(list) => self.list(list.items.iter().copied(), list.nil_terminated),
            Hir::Struct(initializer) => self.list(
                initializer.fields.iter().map(|field| field.value),
                initializer.nil_terminated,
            ),
            Hir::FieldAccess(access) => match &access.kind {
                rue_hir::FieldKind::Path(path) => {
                    let path = path.len() as u64;

                    if self.is_variable(access.value) {
                        // The path is combined with the variable's path
                        Cost::new(1, PATH)
                    } else {
                        Cost::new(4 * path, FIRST * path) + self.estimate(access.value)
                    }
                }
                rue_hir::FieldKind::Length => op(1, 173) + self.estimate(access.value),
            },
            Hir::Cast(value) => self.estimate(*value),
            Hir::Guard(guard) => op(1, 19) + self.estimate(guard.value),
            Hir::Const(expr) => self.estimate(expr.result.unwrap_or(expr.value)),
            Hir::Reference(symbol) => self.reference(*symbol),
            Hir::Block(block) => {
                let statements = block
                    .statements
                    .iter()
                    .map(|statement| self.statement(statement))
                    .sum::<Cost>();

                statements
                    + block
                        .body
                        .map(|body| self.estimate(body))
                        .unwrap_or_default()
            }
            Hir::Lambda(symbol) => Cost::new(10, APPLY) + self.function_body(*symbol),
            Hir::If(condition, then, else_, inline) => {
                let branches = if *inline {
                    self.estimate(*then) + self.estimate(*else_)
                } else {
                    let then = self.estimate(*then);
                    let else_ = self.estimate(*else_);
                    Cost::new(
                        then.size + else_.size + 11,
                        then.runtime.max(else_.runtime) + APPLY + 2 * QUOTE,
                    )
                };

                op(3, IF) + self.estimate(*condition) + branches
            }
            Hir::FunctionCall(call) => {
                let args = self.all(call.args.iter().copied());

                match self.called_function(call.function) {
                    Some((symbol, FunctionKind::Inline)) => {
                        // The body is inlined, and the arguments replace the parameters in it
                        let body = self.function_body(symbol);
                        Cost::new(
                            body.size + args.size.saturating_sub(call.args.len() as u64),
                            body.runtime + args.runtime,
                        )
                    }
                    Some((symbol, _)) => {
                        call_overhead(call.args.len())
                            + args
                            + Cost::new(0, self.function_body(symbol).runtime)
                    }
                    None => {
                        let function = self.estimate(call.function);
                        call_overhead(call.args.len()) + args + function
                    }
                }
            }
            Hir::Unary(op_, value) => self.unary(*op_, *value),
            Hir::Binary(op_, left, right) => self.binary(hir, *op_, *left, *right),
            Hir::CoinId(a, b, c) => self.op(ClvmOp::CoinId, [*a, *b, *c]),
            Hir::Modpow(a, b, c) => self.op(ClvmOp::Modpow, [*a, *b, *c]),
            Hir::Substr(a, b, c) => self.op(ClvmOp::Substr, [*a, *b].into_iter().chain(*c)),
            Hir::G1Map(a, b) => self.op(ClvmOp::G1Map, [*a].into_iter().chain(*b)),
            Hir::G2Map(a, b) => self.op(ClvmOp::G2Map, [*a].into_iter().chain(*b)),
            Hir::BlsPairingIdentity(args) => {
                self.op(ClvmOp::BlsPairingIdentity, args.iter().copied())
            }
            Hir::BlsVerify(a, args) => self.op(
                ClvmOp::BlsVerify,
                [*a].into_iter().chain(args.iter().copied()),
            ),
            Hir::Secp256K1Verify(a, b, c) => self.op(ClvmOp::Secp256K1Verify, [*a, *b, *c]),
            Hir::Secp256R1Verify(a, b, c) => self.op(ClvmOp::Secp256R1Verify, [*a, *b, *c]),
            // The arguments are a list, which is the rest of the operator's list
            Hir::ClvmOp(clvm_op, args) => {
                let count = match db.hir(*args) {
                    Hir::List(list) => list.items.len(),
                    _ => 1,
                };

                Cost::new(2, runtime(*clvm_op, count as u64)) + self.estimate(*args)
            }
        }
    }

    /// An operator applied to the given arguments.
    fn op(&mut self, clvm_op: ClvmOp, args: impl IntoIterator<Item = HirId>) -> Cost {
        let args = args.into_iter().collect::<Vec<_>>();
        op(args.len(), runtime(clvm_op, args.len() as u64)) + self.all(args)
    }

    fn all(&mut self, hirs: impl IntoIterator<Item = HirId>) -> Cost {
        hirs.into_iter().map(|hir| self.estimate(hir)).sum()
    }

    /// A list is a `cons` for each item, ending in nil or the last item.
    fn list(&mut self, items: impl ExactSizeIterator<Item = HirId>, nil_terminated: bool) -> Cost {
        let conses = items.len().saturating_sub(usize::from(!nil_terminated)) as u64;
        let end = if nil_terminated {
            Cost::new(1, QUOTE)
        } else {
            Cost::default()
        };
        Cost::new(5 * conses, CONS * conses) + end + self.all(items)
    }

    fn statement(&mut self, statement: &Statement) -> Cost {
        match statement {
            Statement::Let(symbol, _) => {
                let Symbol::Binding(binding) = self.db.symbol(*symbol) else {
                    return Cost::default();
                };

                // Bindings are grouped together, so each one after the first is smaller than a new
                // binding would be
                Cost::new(6, CONS) + self.estimate(binding.value.hir)
            }
            Statement::Expr(statement) => self.estimate(statement.hir),
            Statement::Return(hir, _) => self.estimate(*hir),
            // `(a (i condition (q . rest) (q 8)) 1)`
            Statement::Assert(hir, _)
            | Statement::If(rue_hir::IfStatement { condition: hir, .. }) => {
                let then = match statement {
                    Statement::If(statement) => self.estimate(statement.then),
                    _ => Cost::new(3, 0),
                };
                Cost::new(14, IF + APPLY + QUOTE) + self.estimate(*hir) + then
            }
            Statement::Raise(hir, _) => op(usize::from(hir.is_some()), 0) + self.all(*hir),
            // Debug statements aren't included in a release build
            Statement::Debug(..) => Cost::default(),
        }
    }

    fn reference(&mut self, symbol: SymbolId) -> Cost {
        match self.db.symbol(symbol) {
            Symbol::Constant(constant) if constant.inline => {
                let value = constant.value.hir;
                self.estimate(value)
            }
            Symbol::Builtin(_) | Symbol::Module(_) | Symbol::Unresolved => Cost::default(),
            _ => REFERENCE,
        }
    }

    fn unary(&mut self, op_: UnaryOp, value: HirId) -> Cost {
        let clvm_op = match op_ {
            UnaryOp::Plus => return self.estimate(value),
            UnaryOp::First | UnaryOp::Rest if self.is_variable(value) => {
                // The path is combined with the variable's path
                return REFERENCE;
            }
            UnaryOp::First => ClvmOp::First,
            UnaryOp::Rest => ClvmOp::Rest,
            UnaryOp::Listp { .. } => ClvmOp::Listp,
            UnaryOp::Strlen => ClvmOp::Strlen,
            UnaryOp::Not => ClvmOp::Not,
            // `(- () value)`
            UnaryOp::Neg => {
                return Cost::new(1, QUOTE) + self.op(ClvmOp::Sub, [value]);
            }
            UnaryOp::BitwiseNot => ClvmOp::Lognot,
            UnaryOp::G1Negate => ClvmOp::G1Negate,
            UnaryOp::G2Negate => ClvmOp::G2Negate,
            // The arguments of a concatenation are hashed directly
            UnaryOp::Sha256 | UnaryOp::Sha256Inline => {
                let args = self.flattened(value, BinaryOp::Concat);
                return self.op(ClvmOp::Sha256, args);
            }
            UnaryOp::Keccak256 | UnaryOp::Keccak256Inline => {
                let args = self.flattened(value, BinaryOp::Concat);
                return self.op(ClvmOp::Keccak256, args);
            }
            UnaryOp::PubkeyForExp => ClvmOp::PubkeyForExp,
        };

        self.op(clvm_op, [value])
    }

    fn binary(&mut self, hir: HirId, op_: BinaryOp, left: HirId, right: HirId) -> Cost {
        // Nested operators are combined into one with all of their arguments
        let (clvm_op, negated) = match op_ {
            BinaryOp::Add => return self.variadic(hir, op_, ClvmOp::Add),
            BinaryOp::Sub => return self.variadic(hir, op_, ClvmOp::Sub),
            BinaryOp::Mul => return self.variadic(hir, op_, ClvmOp::Mul),
            BinaryOp::Concat => return self.variadic(hir, op_, ClvmOp::Concat),
            BinaryOp::All => return self.variadic(hir, op_, ClvmOp::All),
            BinaryOp::Any => return self.variadic(hir, op_, ClvmOp::Any),
            BinaryOp::BitwiseAnd => return self.variadic(hir, op_, ClvmOp::Logand),
            BinaryOp::BitwiseOr => return self.variadic(hir, op_, ClvmOp::Logior),
            BinaryOp::BitwiseXor => return self.variadic(hir, op_, ClvmOp::Logxor),
            BinaryOp::G1Add => return self.variadic(hir, op_, ClvmOp::G1Add),
            BinaryOp::G1Subtract => return self.variadic(hir, op_, ClvmOp::G1Subtract),
            BinaryOp::G2Add => return self.variadic(hir, op_, ClvmOp::G2Add),
            BinaryOp::G2Subtract => return self.variadic(hir, op_, ClvmOp::G2Subtract),
            // Only one of the operands is evaluated if the other decides the result
            BinaryOp::And | BinaryOp::Or => {
                let left = self.estimate(left);
                let right = self.estimate(right);
                return Cost::new(17, IF + APPLY + 2 * QUOTE) + left + right;
            }
            BinaryOp::Div => (ClvmOp::Div, false),
            BinaryOp::Mod => (ClvmOp::Mod, false),
            BinaryOp::Divmod => (ClvmOp::Divmod, false),
            BinaryOp::Gt | BinaryOp::Lt => (ClvmOp::Gt, false),
            BinaryOp::Gte | BinaryOp::Lte => (ClvmOp::Gt, true),
            BinaryOp::GtBytes | BinaryOp::LtBytes => (ClvmOp::GtBytes, false),
            BinaryOp::GteBytes | BinaryOp::LteBytes => (ClvmOp::GtBytes, true),
            BinaryOp::Eq => (ClvmOp::Eq, false),
            BinaryOp::Ne => (ClvmOp::Eq, true),
            BinaryOp::G1Multiply => (ClvmOp::G1Multiply, false),
            BinaryOp::G2Multiply => (ClvmOp::G2Multiply, false),
            BinaryOp::LeftArithmeticShift | BinaryOp::RightArithmeticShift => (ClvmOp::Ash, false),
            BinaryOp::RightLogicalShift => (ClvmOp::Lsh, false),
        };

        // `(not (> b a))` and the like
        let not = if negated {
            op(1, runtime(ClvmOp::Not, 1))
        } else {
            Cost::default()
        };

        not + self.op(clvm_op, [left, right])
    }

    /// An operator whose nested uses are combined into one, such as `a + (b + c)` being
    /// `(+ a b c)`.
    fn variadic(&mut self, hir: HirId, op_: BinaryOp, clvm_op: ClvmOp) -> Cost {
        let args = self.flattened(hir, op_);
        self.op(clvm_op, args)
    }

    /// The arguments of an operator, where the arguments that use the same operator are replaced
    /// by their own arguments, such as `a + (b + c)` being `(+ a b c)`.
    fn flattened(&self, hir: HirId, op_: BinaryOp) -> Vec<HirId> {
        let mut args = Vec::new();
        let mut stack = vec![hir];

        while let Some(hir) = stack.pop() {
            match self.db.hir(hir) {
                Hir::Binary(other, left, right) if *other == op_ => {
                    // Only the left operand of subtraction can be combined
                    if matches!(
                        op_,
                        BinaryOp::Sub | BinaryOp::G1Subtract | BinaryOp::G2Subtract
                    ) {
                        args.push(*right);
                        stack.push(*left);
                    } else {
                        stack.push(*right);
                        stack.push(*left);
                    }
                }
                _ => args.push(hir),
            }
        }

        if matches!(
            op_,
            BinaryOp::Sub | BinaryOp::G1Subtract | BinaryOp::G2Subtract
        ) {
            args.reverse();
        }

        args
    }

    /// Whether the value is a parameter or binding, whose fields can be found by a path.
    fn is_variable(&self, hir: HirId) -> bool {
        let hir = self.db.without_casts(hir);

        matches!(
            self.db.hir(hir),
            Hir::Reference(symbol)
                if matches!(self.db.symbol(*symbol), Symbol::Parameter(_) | Symbol::Binding(_))
        )
    }

    /// The function that a call calls by name, along with its kind.
    fn called_function(&self, function: HirId) -> Option<(SymbolId, FunctionKind)> {
        let Hir::Reference(symbol) = self.db.hir(self.db.without_casts(function)) else {
            return None;
        };

        match self.db.symbol(*symbol) {
            Symbol::Function(function) => Some((*symbol, function.kind)),
            _ => None,
        }
    }
}

/// A quoted atom of the given length.
fn quoted(len: usize) -> Cost {
    if len == 0 {
        return Cost::new(1, QUOTE);
    }

    let len = len as u64;

    let atom = match len {
        1 => 1,
        2..=0x3f => 1 + len,
        0x40..=0x1fff => 2 + len,
        _ => 3 + len,
    };

    Cost::new(2 + atom, QUOTE)
}

/// An operator applied to the given number of arguments, besides the arguments themselves.
fn op(args: usize, runtime: u64) -> Cost {
    Cost::new(args as u64 + 3, runtime)
}

/// The cost of running an operator with the given number of arguments, from the costs that CLVM
/// charges, assuming arguments of [`ASSUMED_BYTES`] where the cost depends on their size.
fn runtime(op_: ClvmOp, args: u64) -> u64 {
    match op_ {
        ClvmOp::Quote => QUOTE,
        ClvmOp::Apply => APPLY,
        ClvmOp::If => IF,
        ClvmOp::Cons => CONS,
        ClvmOp::First | ClvmOp::Rest => FIRST,
        ClvmOp::Listp => 19,
        ClvmOp::Raise | ClvmOp::DebugPrint => 0,
        ClvmOp::Eq | ClvmOp::GtBytes => 117 + ASSUMED_BYTES,
        ClvmOp::Substr => 1,
        ClvmOp::Strlen => 173 + ASSUMED_BYTES,
        ClvmOp::Sha256 => 87 + (134 + 2 * ASSUMED_BYTES) * args,
        ClvmOp::Keccak256 => 50 + (160 + 2 * ASSUMED_BYTES) * args,
        ClvmOp::Concat => 142 + (135 + 3 * ASSUMED_BYTES) * args,
        // Integers are usually much smaller than other values
        ClvmOp::Add | ClvmOp::Sub => 99 + 344 * args,
        ClvmOp::Mul => 92 + 885 * args.saturating_sub(1),
        ClvmOp::Div => 988,
        ClvmOp::Divmod | ClvmOp::Mod => 1_116,
        ClvmOp::Gt => 498,
        ClvmOp::Ash => 596,
        ClvmOp::Lsh => 277,
        ClvmOp::Logand | ClvmOp::Logior | ClvmOp::Logxor => 100 + 264 * args,
        ClvmOp::Lognot => 331,
        ClvmOp::Not => 200,
        ClvmOp::Any | ClvmOp::All => 200 + 300 * args,
        ClvmOp::Modpow => 17_000 + 62 * ASSUMED_BYTES,
        ClvmOp::CoinId => 800,
        ClvmOp::PubkeyForExp => 1_325_730 + 38 * ASSUMED_BYTES,
        ClvmOp::G1Add | ClvmOp::G1Subtract => 101_094 + 1_343_980 * args,
        ClvmOp::G1Multiply => 705_500,
        ClvmOp::G1Negate => 916,
        ClvmOp::G2Add | ClvmOp::G2Subtract => 80_000 + 1_950_000 * args,
        ClvmOp::G2Multiply => 2_100_000,
        ClvmOp::G2Negate => 2_164,
        ClvmOp::G1Map => 195_000,
        ClvmOp::G2Map => 815_000,
        ClvmOp::BlsPairingIdentity => 3_000_000 + 1_200_000 * args,
        ClvmOp::BlsVerify => 3_000_000 + 1_200_000 * args.saturating_sub(1),
        ClvmOp::Secp256K1Verify => 1_300_000,
        ClvmOp::Secp256R1Verify => 1_850_000,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quoted_atoms() {
        assert_eq!(quoted(0).size, 1);
        assert_eq!(quoted(1).size, 3);
        assert_eq!(quoted(32).size, 35);
        assert_eq!(quoted(100).size, 104);
    }

    #[test]
    fn call_overhead_matches_the_compiler() {
        // `(a 2 (c 5 11))`
        assert_eq!(call_overhead(2).size + 2, 13);
        // `(a 2 5)`
        assert_eq!(call_overhead(1).size + 1, 7);
    }
}
