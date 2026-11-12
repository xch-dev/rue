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
    fn new(size: u64, runtime: u64) -> Self {
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
    pub(crate) fn function_body(&mut self, symbol: SymbolId) -> Cost {
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
            Hir::CoinId(a, b, c) => op(3, 800) + self.all([*a, *b, *c]),
            Hir::Modpow(a, b, c) => op(3, 17_000 + 62 * ASSUMED_BYTES) + self.all([*a, *b, *c]),
            Hir::Substr(a, b, c) => {
                op(if c.is_some() { 3 } else { 2 }, 1) + self.all([*a, *b].into_iter().chain(*c))
            }
            Hir::G1Map(a, b) => {
                op(1 + usize::from(b.is_some()), 195_000) + self.all([*a].into_iter().chain(*b))
            }
            Hir::G2Map(a, b) => {
                op(1 + usize::from(b.is_some()), 815_000) + self.all([*a].into_iter().chain(*b))
            }
            Hir::BlsPairingIdentity(args) => {
                op(args.len(), 3_000_000 + 1_200_000 * args.len() as u64)
                    + self.all(args.iter().copied())
            }
            Hir::BlsVerify(a, args) => {
                op(args.len() + 1, 3_000_000 + 1_200_000 * args.len() as u64)
                    + self.all([*a].into_iter().chain(args.iter().copied()))
            }
            Hir::Secp256K1Verify(a, b, c) => op(3, 1_300_000) + self.all([*a, *b, *c]),
            Hir::Secp256R1Verify(a, b, c) => op(3, 1_850_000) + self.all([*a, *b, *c]),
            Hir::ClvmOp(clvm_op, args) => {
                Cost::new(2, clvm_op_runtime(*clvm_op)) + self.estimate(*args)
            }
        }
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
            _ => Cost::new(1, PATH),
        }
    }

    fn unary(&mut self, op_: UnaryOp, value: HirId) -> Cost {
        let runtime = match op_ {
            UnaryOp::Plus => return self.estimate(value),
            UnaryOp::First | UnaryOp::Rest if self.is_variable(value) => {
                // The path is combined with the variable's path
                return Cost::new(1, PATH);
            }
            UnaryOp::First | UnaryOp::Rest => FIRST,
            UnaryOp::Listp { .. } => 19,
            UnaryOp::Strlen => 173 + ASSUMED_BYTES,
            UnaryOp::Not => 200,
            UnaryOp::Neg => return op(2, 99 + 640) + Cost::new(1, QUOTE) + self.estimate(value),
            UnaryOp::BitwiseNot => 331,
            UnaryOp::G1Negate => 916,
            UnaryOp::G2Negate => 2_164,
            UnaryOp::Sha256 | UnaryOp::Sha256Inline => {
                // The arguments of a concatenation are hashed directly
                let args = self.flattened(value, BinaryOp::Concat);
                return op(
                    args.len(),
                    87 + (134 + 2 * ASSUMED_BYTES) * args.len() as u64,
                ) + self.all(args);
            }
            UnaryOp::Keccak256 | UnaryOp::Keccak256Inline => {
                let args = self.flattened(value, BinaryOp::Concat);
                return op(
                    args.len(),
                    50 + (160 + 2 * ASSUMED_BYTES) * args.len() as u64,
                ) + self.all(args);
            }
            UnaryOp::PubkeyForExp => 1_325_730 + 38 * ASSUMED_BYTES,
        };

        op(1, runtime) + self.estimate(value)
    }

    fn binary(&mut self, hir: HirId, op_: BinaryOp, left: HirId, right: HirId) -> Cost {
        match op_ {
            // Nested operators are combined into one with all of their arguments
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Concat
            | BinaryOp::All
            | BinaryOp::Any
            | BinaryOp::BitwiseAnd
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseXor
            | BinaryOp::G1Add
            | BinaryOp::G1Subtract
            | BinaryOp::G2Add
            | BinaryOp::G2Subtract => {
                let args = self.flattened(hir, op_);
                let n = args.len() as u64;
                let runtime = match op_ {
                    BinaryOp::Add | BinaryOp::Sub => 99 + 344 * n,
                    BinaryOp::Mul => 92 + 885 * (n - 1),
                    BinaryOp::Concat => 142 + (135 + 3 * ASSUMED_BYTES) * n,
                    BinaryOp::All | BinaryOp::Any => 200 + 300 * n,
                    BinaryOp::G1Add | BinaryOp::G1Subtract => 101_094 + 1_343_980 * n,
                    BinaryOp::G2Add | BinaryOp::G2Subtract => 80_000 + 1_950_000 * n,
                    _ => 100 + 264 * n,
                };
                op(args.len(), runtime) + self.all(args)
            }
            // Only one of the operands is evaluated if the other decides the result
            BinaryOp::And | BinaryOp::Or => {
                let left = self.estimate(left);
                let right = self.estimate(right);
                Cost::new(17, IF + APPLY + 2 * QUOTE) + left + right
            }
            _ => {
                let (runtime, negated) = match op_ {
                    BinaryOp::Div => (988, false),
                    BinaryOp::Mod | BinaryOp::Divmod => (1_116, false),
                    BinaryOp::Gt | BinaryOp::Lt => (498, false),
                    BinaryOp::Gte | BinaryOp::Lte => (498, true),
                    BinaryOp::GtBytes | BinaryOp::LtBytes => (117, false),
                    BinaryOp::GteBytes | BinaryOp::LteBytes => (117, true),
                    BinaryOp::Eq => (117, false),
                    BinaryOp::Ne => (117, true),
                    BinaryOp::G1Multiply => (705_500, false),
                    BinaryOp::G2Multiply => (2_100_000, false),
                    _ => (500, false),
                };

                let not = if negated { op(1, 200) } else { Cost::default() };
                not + op(2, runtime) + self.all([left, right])
            }
        }
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

fn clvm_op_runtime(op_: ClvmOp) -> u64 {
    match op_ {
        ClvmOp::Quote => QUOTE,
        ClvmOp::Apply => APPLY,
        ClvmOp::If => IF,
        ClvmOp::Cons => CONS,
        ClvmOp::First | ClvmOp::Rest => FIRST,
        ClvmOp::Sha256 | ClvmOp::Keccak256 => 400,
        ClvmOp::CoinId => 800,
        ClvmOp::PubkeyForExp => 1_325_730,
        ClvmOp::Secp256K1Verify => 1_300_000,
        ClvmOp::Secp256R1Verify => 1_850_000,
        ClvmOp::BlsVerify | ClvmOp::BlsPairingIdentity => 4_200_000,
        _ => 500,
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
