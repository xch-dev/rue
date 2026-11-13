use std::collections::{HashMap, HashSet};

use crate::{
    BinaryOp, Database, FieldKind, FunctionCall, FunctionKind, FunctionSymbol, Hir, HirId,
    Statement, Symbol, SymbolId, UnaryOp, reachable_body, reachable_statements,
};

/// What reading a parameter does, as far as [`may_fail`] is concerned.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParameterRead {
    /// The parameter is read from an environment the compiler built, which can't fail.
    Infallible,

    /// The parameter is read from an environment that's provided at runtime (such as the
    /// solution of the program), which fails if it's too short.
    Fallible,

    /// The parameter stands for an argument that's substituted in its place, since the function
    /// is inlined, so reading it is evaluating the argument.
    Argument(HirId),
}

/// Whether evaluating the given HIR could fail at runtime, for any environment. Unlike
/// [`may_raise`](crate::may_raise), this includes operations that only fail on values that
/// don't match their types (such as arithmetic on a pair, or a field access on an atom), since a
/// solution isn't checked against the declared types at runtime.
///
/// This is conservative, so it can return `true` for a value that can never fail. It's used to
/// decide whether a value can be dropped or moved somewhere it might not be evaluated.
///
/// The `parameter` callback decides how a reference to a parameter is treated.
/// Values of `inline` bindings and constants are evaluated where they are referenced, and the
/// values of other bindings and constants are treated as if they were evaluated at the reference
/// as well, since they may be inlined there.
pub fn may_fail(db: &Database, hir: HirId, parameter: &dyn Fn(SymbolId) -> ParameterRead) -> bool {
    FailureVisitor {
        db,
        parameter,
        values: HashMap::new(),
    }
    .visit(hir)
}

/// The same as [`may_fail`], but every parameter is treated as if reading it could fail. This is
/// used before it's known which functions will be inlined and which parameters are provided at
/// runtime.
pub fn may_fail_conservative(db: &Database, hir: HirId) -> bool {
    may_fail(db, hir, &|_| ParameterRead::Fallible)
}

struct FailureVisitor<'a> {
    db: &'a Database,
    parameter: &'a dyn Fn(SymbolId) -> ParameterRead,
    // Whether the value of each symbol that has been visited may fail
    values: HashMap<SymbolId, bool>,
}

impl FailureVisitor<'_> {
    fn visit_all(&mut self, hirs: impl IntoIterator<Item = HirId>) -> bool {
        hirs.into_iter().any(|hir| self.visit(hir))
    }

    fn visit_symbol_value(&mut self, symbol: SymbolId, value: HirId) -> bool {
        if let Some(&result) = self.values.get(&symbol) {
            return result;
        }

        // A recursive reference is already being checked, so it doesn't add anything
        self.values.insert(symbol, false);
        let result = self.visit(value);
        self.values.insert(symbol, result);
        result
    }

    fn visit_statements(&mut self, statements: &[Statement], body: Option<HirId>) -> bool {
        for stmt in reachable_statements(statements) {
            let fails = match *stmt {
                Statement::Raise(..) | Statement::Assert(..) => true,
                Statement::Let(symbol, _) => match self.db.symbol(symbol) {
                    Symbol::Binding(binding) if !binding.inline => {
                        self.visit_symbol_value(symbol, binding.value.hir)
                    }
                    _ => false,
                },
                Statement::Expr(ref stmt) => self.visit(stmt.hir),
                Statement::If(ref stmt) => self.visit_all([stmt.condition, stmt.then]),
                Statement::Return(hir, _) | Statement::Debug(hir, _) => self.visit(hir),
            };

            if fails {
                return true;
            }
        }

        self.visit_all(reachable_body(statements, body))
    }

    fn visit(&mut self, hir: HirId) -> bool {
        match self.db.hir(hir).clone() {
            Hir::Unresolved => true,
            Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            // A lambda creates a closure, and its body isn't evaluated until it's called
            | Hir::Lambda(_) => false,
            Hir::Const(expr) => expr.result.is_none_or(|result| self.visit(result)),
            Hir::Pair(first, rest) => self.visit_all([first, rest]),
            Hir::List(list) => self.visit_all(list.items),
            Hir::Struct(initializer) => {
                self.visit_all(initializer.fields.into_iter().map(|field| field.value))
            }
            Hir::FieldAccess(access) => match access.kind {
                FieldKind::Path(path) if path.is_empty() => self.visit(access.value),
                FieldKind::Path(_) | FieldKind::Length => true,
            },
            Hir::Cast(value) => self.visit(value),
            // The check itself may fail (for example, checking the length of a pair),
            // so this is only precise if there's nothing to check
            Hir::Guard(guard) => guard.value_uses() > 0 || self.visit(guard.value),
            Hir::Reference(symbol) => match self.db.symbol(symbol).clone() {
                Symbol::Parameter(_) => match (self.parameter)(symbol) {
                    ParameterRead::Infallible => false,
                    ParameterRead::Fallible => true,
                    ParameterRead::Argument(hir) => self.visit_symbol_value(symbol, hir),
                },
                Symbol::Binding(binding) => self.visit_symbol_value(symbol, binding.value.hir),
                Symbol::Constant(constant) => self.visit_symbol_value(symbol, constant.value.hir),
                // Referencing a function creates a closure from its captures
                Symbol::Function(_) | Symbol::Builtin(_) | Symbol::Module(_) => false,
                Symbol::Unresolved => true,
            },
            Hir::Block(block) => self.visit_statements(&block.statements, block.body),
            Hir::If(condition, then, else_, _) => self.visit_all([condition, then, else_]),
            Hir::Unary(op, value) => match op {
                UnaryOp::Listp { .. } | UnaryOp::Not | UnaryOp::Plus => self.visit(value),
                // These accept any atom
                UnaryOp::Strlen
                | UnaryOp::Neg
                | UnaryOp::BitwiseNot
                | UnaryOp::Sha256
                | UnaryOp::Sha256Inline
                | UnaryOp::Keccak256
                | UnaryOp::Keccak256Inline => !self.is_atom_literal(value),
                UnaryOp::First
                | UnaryOp::Rest
                | UnaryOp::G1Negate
                | UnaryOp::G2Negate
                | UnaryOp::PubkeyForExp => true,
            },
            Hir::Binary(op, left, right) => match op {
                // These only check the truthiness of their operands
                BinaryOp::And | BinaryOp::Or | BinaryOp::All | BinaryOp::Any => {
                    self.visit_all([left, right])
                }
                // These accept any atoms
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Concat
                | BinaryOp::BitwiseAnd
                | BinaryOp::BitwiseOr
                | BinaryOp::BitwiseXor
                | BinaryOp::Gt
                | BinaryOp::Lt
                | BinaryOp::Gte
                | BinaryOp::Lte
                | BinaryOp::GtBytes
                | BinaryOp::LtBytes
                | BinaryOp::GteBytes
                | BinaryOp::LteBytes
                | BinaryOp::Eq
                | BinaryOp::Ne => !self.is_atom_literal(left) || !self.is_atom_literal(right),
                BinaryOp::Div
                | BinaryOp::Mod
                | BinaryOp::Divmod
                | BinaryOp::G1Add
                | BinaryOp::G1Subtract
                | BinaryOp::G1Multiply
                | BinaryOp::G2Add
                | BinaryOp::G2Subtract
                | BinaryOp::G2Multiply
                | BinaryOp::LeftArithmeticShift
                | BinaryOp::RightArithmeticShift
                | BinaryOp::RightLogicalShift => true,
            },
            Hir::FunctionCall(_)
            | Hir::CoinId(..)
            | Hir::Substr(..)
            | Hir::G1Map(..)
            | Hir::G2Map(..)
            | Hir::Modpow(..)
            | Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::ClvmOp(..) => true,
        }
    }

    /// Whether the value is an atom that's known at compile time.
    fn is_atom_literal(&self, hir: HirId) -> bool {
        match self.db.hir(hir) {
            Hir::Nil | Hir::String(_) | Hir::Int(_) | Hir::Bytes(_) | Hir::Bool(_) => true,
            Hir::Cast(value) => self.is_atom_literal(*value),
            Hir::Const(expr) => expr
                .result
                .is_some_and(|result| self.is_atom_literal(result)),
            _ => false,
        }
    }
}

/// Whether evaluating the given HIR always evaluates a reference to the symbol, unless it fails
/// before reaching it. This is conservative, so it can return `false` for a reference that's
/// always evaluated.
///
/// A reference isn't always evaluated if it's in a branch of an `if` that isn't `inline`, on the
/// right side of `&&` or `||`, in the body of a lambda, after an `if` statement that may return
/// early, or in an argument of an `inline` function (which is only evaluated if the function
/// uses it). Arguments of other functions are evaluated when the function is called, even if it's
/// inlined. The value of a binding that isn't `inline` is evaluated at its `let` statement.
pub fn always_evaluates(db: &Database, hir: HirId, symbol: SymbolId) -> bool {
    EvaluationVisitor {
        db,
        symbol,
        values: HashMap::new(),
        calls: HashSet::new(),
    }
    .visit(hir)
}

/// The same as [`always_evaluates`], but for a sequence of statements followed by an optional
/// body.
pub fn statements_always_evaluate(
    db: &Database,
    statements: &[Statement],
    body: Option<HirId>,
    symbol: SymbolId,
) -> bool {
    EvaluationVisitor {
        db,
        symbol,
        values: HashMap::new(),
        calls: HashSet::new(),
    }
    .visit_statements(statements, body)
}

struct EvaluationVisitor<'a> {
    db: &'a Database,
    symbol: SymbolId,
    // Whether the value of each inline symbol that has been visited evaluates the symbol
    values: HashMap<SymbolId, bool>,
    // The inline functions whose bodies are being visited
    calls: HashSet<SymbolId>,
}

impl EvaluationVisitor<'_> {
    fn visit_statements(&mut self, statements: &[Statement], body: Option<HirId>) -> bool {
        for (index, stmt) in statements.iter().enumerate() {
            match *stmt {
                // The program fails, so it doesn't matter what's evaluated
                Statement::Raise(..) => return true,
                Statement::Return(hir, _) => return self.visit(hir),
                Statement::Let(symbol, _) => {
                    if let Symbol::Binding(binding) = self.db.symbol(symbol)
                        && !binding.inline
                        && self.visit(binding.value.hir)
                    {
                        return true;
                    }
                }
                Statement::Expr(ref stmt) => {
                    if self.visit(stmt.hir) {
                        return true;
                    }
                }
                Statement::Assert(condition, _) => {
                    if self.visit(condition) {
                        return true;
                    }
                }
                // The then branch replaces the rest of the block, since it always ends early
                Statement::If(ref stmt) => {
                    return self.visit(stmt.condition)
                        || (self.visit(stmt.then)
                            && self.visit_statements(&statements[index + 1..], body));
                }
                // Debug statements are only evaluated with debug symbols
                Statement::Debug(..) => {}
            }
        }

        body.is_some_and(|body| self.visit(body))
    }

    /// The arguments of an inline function are substituted where its parameters are used, so an
    /// argument is only always evaluated if the body always evaluates its parameter.
    fn visit_inline_call(
        &mut self,
        call: &FunctionCall,
        symbol: SymbolId,
        function: FunctionSymbol,
    ) -> bool {
        // An inline function that calls itself can't be inlined, but stop the recursion anyway
        if self.calls.contains(&symbol) {
            return false;
        }

        let mut calls = self.calls.clone();
        calls.insert(symbol);

        // When the function isn't nil-terminated, the last parameter is a list of the remaining
        // arguments, which are all evaluated if it is
        let fixed = if function.nil_terminated {
            function.parameters.len()
        } else {
            function.parameters.len().saturating_sub(1)
        };

        call.args.iter().enumerate().any(|(index, &arg)| {
            let Some(&parameter) = function.parameters.get(index.min(fixed)) else {
                return false;
            };

            self.visit(arg)
                && EvaluationVisitor {
                    db: self.db,
                    symbol: parameter,
                    values: HashMap::new(),
                    calls: calls.clone(),
                }
                .visit(function.body)
        })
    }

    fn visit(&mut self, hir: HirId) -> bool {
        match self.db.hir(hir).clone() {
            Hir::Reference(symbol) => {
                if symbol == self.symbol {
                    return true;
                }

                // The values of inline bindings and constants are evaluated where they're used
                let value = match self.db.symbol(symbol) {
                    Symbol::Binding(binding) if binding.inline => binding.value.hir,
                    Symbol::Constant(constant) if constant.inline => constant.value.hir,
                    _ => return false,
                };

                if let Some(&result) = self.values.get(&symbol) {
                    return result;
                }

                self.values.insert(symbol, false);
                let result = self.visit(value);
                self.values.insert(symbol, result);
                result
            }
            Hir::Block(block) => self.visit_statements(&block.statements, block.body),
            Hir::If(condition, then, else_, inline) => {
                self.visit(condition)
                    || if inline {
                        self.visit(then) || self.visit(else_)
                    } else {
                        self.visit(then) && self.visit(else_)
                    }
            }
            Hir::Binary(BinaryOp::And | BinaryOp::Or, left, _) => self.visit(left),
            Hir::FunctionCall(call) => {
                if let Hir::Reference(symbol) = *self.db.hir(self.db.without_casts(call.function))
                    && let Symbol::Function(function) = self.db.symbol(symbol)
                    && function.kind == FunctionKind::Inline
                {
                    return self.visit_inline_call(&call, symbol, function.clone());
                }

                self.visit(call.function) || call.args.iter().any(|&arg| self.visit(arg))
            }
            Hir::Lambda(_) | Hir::Const(_) => false,
            node => node.children().any(|child| self.visit(child)),
        }
    }
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use rue_types::BuiltinTypes;

    use crate::{BindingSymbol, Block, ExprStatement, FunctionCall, ParameterSymbol, Scope, Value};

    use rue_diagnostic::{Source, SourceKind, SrcLoc};

    use super::*;

    fn srcloc() -> SrcLoc {
        SrcLoc::new(Source::new("".into(), SourceKind::Std(String::new())), 0..0)
    }

    fn binding(db: &mut Database, value: HirId, inline: bool) -> SymbolId {
        let ty = BuiltinTypes::new(db.types_mut()).int;
        db.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: Value::new(value, ty),
            inline,
        }))
    }

    fn int(db: &mut Database, value: i32) -> HirId {
        db.alloc_hir(Hir::Int(BigInt::from(value)))
    }

    #[test]
    fn literals_and_their_operations_cannot_fail() {
        let mut db = Database::new();
        let a = int(&mut db, 1);
        let b = int(&mut db, 2);
        let add = db.alloc_hir(Hir::Binary(BinaryOp::Add, a, b));
        let div = db.alloc_hir(Hir::Binary(BinaryOp::Div, a, b));
        let pair = db.alloc_hir(Hir::Pair(add, a));

        assert!(!may_fail_conservative(&db, add));
        assert!(!may_fail_conservative(&db, pair));
        assert!(may_fail_conservative(&db, div));
    }

    #[test]
    fn parameter_reads() {
        let mut db = Database::new();
        let ty = BuiltinTypes::new(db.types_mut()).int;
        let parameter = db.alloc_symbol(Symbol::Parameter(ParameterSymbol { name: None, ty }));
        let reference = db.alloc_hir(Hir::Reference(parameter));
        let a = int(&mut db, 1);
        let div = db.alloc_hir(Hir::Binary(BinaryOp::Div, a, a));

        assert!(may_fail_conservative(&db, reference));
        assert!(!may_fail(&db, reference, &|_| ParameterRead::Infallible));
        assert!(may_fail(&db, reference, &|_| ParameterRead::Argument(div)));
        assert!(!may_fail(&db, reference, &|_| ParameterRead::Argument(a)));
    }

    #[test]
    fn evaluation_of_branches() {
        let mut db = Database::new();
        let value = int(&mut db, 1);
        let symbol = binding(&mut db, value, false);
        let reference = db.alloc_hir(Hir::Reference(symbol));
        let nil = db.alloc_hir(Hir::Nil);

        let lazy = db.alloc_hir(Hir::If(nil, reference, nil, false));
        let eager = db.alloc_hir(Hir::If(nil, reference, nil, true));
        let both = db.alloc_hir(Hir::If(nil, reference, reference, false));
        let condition = db.alloc_hir(Hir::If(reference, nil, nil, false));
        let and = db.alloc_hir(Hir::Binary(BinaryOp::And, nil, reference));

        assert!(!always_evaluates(&db, lazy, symbol));
        assert!(always_evaluates(&db, eager, symbol));
        assert!(always_evaluates(&db, both, symbol));
        assert!(always_evaluates(&db, condition, symbol));
        assert!(!always_evaluates(&db, and, symbol));
    }

    #[test]
    fn evaluation_through_bindings() {
        let mut db = Database::new();
        let value = int(&mut db, 1);
        let symbol = binding(&mut db, value, false);
        let reference = db.alloc_hir(Hir::Reference(symbol));

        // A binding that isn't inline is evaluated at its let statement
        let other = binding(&mut db, reference, false);
        let block = db.alloc_hir(Hir::Block(Block {
            statements: vec![Statement::Let(other, srcloc())],
            body: None,
        }));
        assert!(always_evaluates(&db, block, symbol));

        // An inline binding is only evaluated where it's referenced
        let inline = binding(&mut db, reference, true);
        let unused = db.alloc_hir(Hir::Block(Block {
            statements: vec![Statement::Let(inline, srcloc())],
            body: None,
        }));
        assert!(!always_evaluates(&db, unused, symbol));

        let inline_reference = db.alloc_hir(Hir::Reference(inline));
        let used = db.alloc_hir(Hir::Block(Block {
            statements: vec![
                Statement::Let(inline, srcloc()),
                Statement::Expr(ExprStatement {
                    hir: inline_reference,
                    always_nil: false,
                    srcloc: srcloc(),
                }),
            ],
            body: None,
        }));
        assert!(always_evaluates(&db, used, symbol));
    }

    // inline fn f(x, y) { if y { x } else { y } }, which always evaluates y but not x
    fn inline_function(db: &mut Database, recursive: bool) -> (SymbolId, SymbolId, SymbolId) {
        let builtins = BuiltinTypes::new(db.types_mut());
        let x = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: None,
            ty: builtins.int,
        }));
        let y = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: None,
            ty: builtins.int,
        }));
        let x_reference = db.alloc_hir(Hir::Reference(x));
        let y_reference = db.alloc_hir(Hir::Reference(y));
        let body = db.alloc_hir(Hir::If(y_reference, x_reference, y_reference, false));
        let scope = db.alloc_scope(Scope::new(None));
        let function = db.alloc_symbol(Symbol::Function(FunctionSymbol {
            name: None,
            ty: builtins.unresolved,
            scope,
            vars: vec![],
            parameters: vec![x, y],
            nil_terminated: true,
            return_type: builtins.unresolved,
            body,
            kind: FunctionKind::Inline,
        }));

        if recursive {
            // inline fn f(x, y) { f(x, y) }
            let reference = db.alloc_hir(Hir::Reference(function));
            *db.hir_mut(body) = Hir::FunctionCall(FunctionCall {
                function: reference,
                args: vec![x_reference, y_reference],
                nil_terminated: true,
            });
        }

        (function, x, y)
    }

    fn call(db: &mut Database, function: SymbolId, args: Vec<HirId>) -> HirId {
        let function = db.alloc_hir(Hir::Reference(function));
        db.alloc_hir(Hir::FunctionCall(FunctionCall {
            function,
            args,
            nil_terminated: true,
        }))
    }

    #[test]
    fn evaluation_of_inline_arguments() {
        let mut db = Database::new();
        let value = int(&mut db, 1);
        let symbol = binding(&mut db, value, false);
        let reference = db.alloc_hir(Hir::Reference(symbol));
        let nil = db.alloc_hir(Hir::Nil);
        let (function, _, _) = inline_function(&mut db, false);

        let as_x = call(&mut db, function, vec![reference, nil]);
        let as_y = call(&mut db, function, vec![nil, reference]);

        assert!(!always_evaluates(&db, as_x, symbol));
        assert!(always_evaluates(&db, as_y, symbol));
    }

    #[test]
    fn evaluation_of_recursive_inline_function() {
        let mut db = Database::new();
        let value = int(&mut db, 1);
        let symbol = binding(&mut db, value, false);
        let reference = db.alloc_hir(Hir::Reference(symbol));
        let (function, _, _) = inline_function(&mut db, true);

        let call = call(&mut db, function, vec![reference, reference]);
        assert!(!always_evaluates(&db, call, symbol));
    }
}
