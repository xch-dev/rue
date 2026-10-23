use std::collections::{HashMap, HashSet};

use indexmap::IndexSet;
use rue_options::CompilerOptions;

use crate::{BinaryOp, Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId};

// The evaluation region of the body of the root function, which is also where constants are
// evaluated, since they are part of its environment
const ROOT_REGION: usize = 0;

#[derive(Debug, Clone)]
pub struct DependencyGraph {
    options: CompilerOptions,

    // Keeps track of the direct dependencies of a symbol
    dependencies: HashMap<SymbolId, IndexSet<SymbolId>>,

    // Keeps track of the locals of a symbol
    locals: HashMap<SymbolId, IndexSet<SymbolId>>,

    // Keeps track of the number of times a symbol is referenced
    references: HashMap<SymbolId, usize>,

    // Keeps track of whether a symbol is ever used as a closure
    closures: HashSet<SymbolId>,

    // Keeps track of the number of times a binding or parameter is referenced in the same
    // evaluation region as its definition, which means it's evaluated exactly once whenever
    // the definition is (rather than conditionally, lazily, or repeatedly)
    unconditional_references: HashMap<SymbolId, usize>,

    // Keeps track of the number of times a binding or parameter is referenced (directly or
    // through nested lambdas) by a lambda created in the same evaluation region as its definition,
    // which means a capture by value would be evaluated exactly once whenever the definition is
    captured_references: HashMap<SymbolId, usize>,

    // Keeps track of the evaluation region each binding or parameter is defined in
    definition_regions: HashMap<SymbolId, usize>,

    // Keeps track of the enclosing lambdas at the definition of each binding
    definition_lambdas: HashMap<SymbolId, Vec<usize>>,

    // The evaluation region currently being visited
    region: usize,

    // The evaluation regions that the lambdas enclosing the current one were created in,
    // up until the nearest named function body
    lambda_regions: Vec<usize>,

    // Whether the next function to be visited is the body of a lambda
    visiting_lambda: bool,

    // The number of evaluation regions that have been created so far
    regions: usize,

    // Keeps track of the const expressions that are reachable
    const_exprs: IndexSet<HirId>,

    // Keeps track of the current stack of symbols being visited
    stack: IndexSet<SymbolId>,

    // Keeps track of the symbols that have been visited to avoid infinite loops
    visited: IndexSet<SymbolId>,
}

impl DependencyGraph {
    pub fn build(db: &Database, main: SymbolId, options: CompilerOptions) -> Self {
        let mut graph = Self {
            options,
            dependencies: HashMap::new(),
            locals: HashMap::new(),
            references: HashMap::new(),
            closures: HashSet::new(),
            unconditional_references: HashMap::new(),
            captured_references: HashMap::new(),
            definition_regions: HashMap::new(),
            definition_lambdas: HashMap::new(),
            region: ROOT_REGION,
            regions: ROOT_REGION + 1,
            lambda_regions: Vec::new(),
            visiting_lambda: false,
            const_exprs: IndexSet::new(),
            stack: IndexSet::new(),
            visited: IndexSet::new(),
        };

        visit_symbol_reference(db, &mut graph, main);

        graph
    }

    pub fn references(&self, symbol: SymbolId) -> usize {
        *self.references.get(&symbol).unwrap_or(&0)
    }

    /// The number of references to a binding or parameter that are evaluated exactly once,
    /// unconditionally, whenever its definition is evaluated. A reference inside of a branch,
    /// a short-circuiting operator, a lambda or nested function, or an argument that an inline
    /// function could drop or duplicate doesn't count.
    pub fn unconditional_references(&self, symbol: SymbolId) -> usize {
        *self.unconditional_references.get(&symbol).unwrap_or(&0)
    }

    /// The number of references to a binding or parameter from inside of a lambda that's created
    /// exactly once, unconditionally, whenever its definition is evaluated. Capturing the value
    /// when the lambda is created evaluates it exactly once.
    pub fn captured_references(&self, symbol: SymbolId) -> usize {
        *self.captured_references.get(&symbol).unwrap_or(&0)
    }

    pub fn is_closure(&self, symbol: SymbolId) -> bool {
        self.closures.contains(&symbol)
    }

    pub fn const_exprs(&self) -> impl Iterator<Item = HirId> + '_ {
        self.const_exprs.iter().copied()
    }

    pub fn dependencies(&self, symbol: SymbolId, captures_only: bool) -> IndexSet<SymbolId> {
        let mut visited = IndexSet::new();
        let mut result = IndexSet::new();
        let mut locals = IndexSet::new();
        let mut stack = vec![(symbol, false)];

        while let Some((current, is_result)) = stack.pop() {
            if !visited.insert((current, is_result)) {
                continue;
            }

            if is_result {
                result.insert(current);
            }

            locals.extend(self.locals.get(&current).cloned().unwrap_or_default());

            for dependency in self.dependencies.get(&current).cloned().unwrap_or_default() {
                stack.push((dependency, !captures_only || !locals.contains(&dependency)));
            }
        }

        result
    }

    /// Visits code that isn't guaranteed to be evaluated exactly once when the surrounding code
    /// is, such as a branch or a function body.
    fn in_new_region(&mut self, f: impl FnOnce(&mut Self)) {
        let region = self.region;
        self.region = self.new_region();
        f(self);
        self.region = region;
    }

    fn new_region(&mut self) -> usize {
        self.regions += 1;
        self.regions - 1
    }

    fn in_region(&mut self, region: usize, f: impl FnOnce(&mut Self)) {
        let previous = std::mem::replace(&mut self.region, region);
        f(self);
        self.region = previous;
    }
}

fn visit_hir(db: &Database, graph: &mut DependencyGraph, hir: HirId, is_call: bool) {
    match db.hir(hir) {
        Hir::Unresolved
        | Hir::Nil
        | Hir::String(_)
        | Hir::Int(_)
        | Hir::Bytes(_)
        | Hir::Bool(_) => {}
        Hir::Pair(first, rest) => {
            visit_hir(db, graph, *first, false);
            visit_hir(db, graph, *rest, false);
        }
        Hir::Const(expr) => {
            graph.const_exprs.insert(hir);
            let lambda_regions = std::mem::take(&mut graph.lambda_regions);
            graph.in_new_region(|graph| visit_hir(db, graph, expr.value, false));
            graph.lambda_regions = lambda_regions;
        }
        Hir::Reference(symbol) => {
            if !is_call {
                graph.closures.insert(*symbol);
            }
            visit_symbol_reference(db, graph, *symbol);
        }
        Hir::Block(block) => {
            let region = graph.region;

            for stmt in &block.statements {
                match stmt {
                    Statement::Let(stmt) => {
                        if let Some(last) = graph.stack.last() {
                            graph.locals.entry(*last).or_default().insert(*stmt);
                        }
                        graph.definition_regions.insert(*stmt, graph.region);
                        graph
                            .definition_lambdas
                            .insert(*stmt, graph.lambda_regions.clone());
                    }
                    Statement::Assert(stmt, _) => {
                        visit_hir(db, graph, *stmt, false);
                    }
                    Statement::Raise(stmt, _) => {
                        if graph.options.debug_symbols
                            && let Some(stmt) = stmt
                        {
                            visit_hir(db, graph, *stmt, false);
                        }
                    }
                    // Both branches of an inline if are always evaluated
                    Statement::If(stmt) if stmt.inline => {
                        visit_hir(db, graph, stmt.condition, false);
                        visit_hir(db, graph, stmt.then, false);
                    }
                    Statement::If(stmt) => {
                        visit_hir(db, graph, stmt.condition, false);
                        graph.in_new_region(|graph| visit_hir(db, graph, stmt.then, false));

                        // The rest of the block is skipped if the condition is true
                        graph.region = graph.new_region();
                    }
                    Statement::Return(expr) => {
                        visit_hir(db, graph, *expr, false);
                    }
                    Statement::Expr(expr) => {
                        visit_hir(db, graph, expr.hir, false);
                    }
                    Statement::Debug(expr, _) => {
                        if graph.options.debug_symbols {
                            visit_hir(db, graph, *expr, false);
                        }
                    }
                }
            }

            if let Some(body) = block.body {
                visit_hir(db, graph, body, false);
            }

            graph.region = region;
        }
        Hir::Lambda(lambda) => {
            if let Some(last) = graph.stack.last() {
                graph.locals.entry(*last).or_default().insert(*lambda);
            }

            graph.closures.insert(*lambda);

            graph.lambda_regions.push(graph.region);
            graph.visiting_lambda = true;
            visit_symbol_reference(db, graph, *lambda);
            graph.lambda_regions.pop().unwrap();
        }
        Hir::If(condition, then, else_, inline) => {
            visit_hir(db, graph, *condition, false);

            // Both branches of an inline if are always evaluated
            if *inline {
                visit_hir(db, graph, *then, false);
                visit_hir(db, graph, *else_, false);
            } else {
                graph.in_new_region(|graph| visit_hir(db, graph, *then, false));
                graph.in_new_region(|graph| visit_hir(db, graph, *else_, false));
            }
        }
        Hir::FunctionCall(call) => {
            visit_hir(db, graph, call.function, true);

            // Inline functions are lowered with their arguments substituted for the parameters,
            // so an argument is only evaluated exactly once if its parameter is
            let inline_function = if let Hir::Reference(symbol) = db.hir(call.function)
                && let Symbol::Function(function) = db.symbol(*symbol)
                && function.kind == FunctionKind::Inline
            {
                Some(function)
            } else {
                None
            };

            for (i, &arg) in call.args.iter().enumerate() {
                let substituted_once = inline_function.is_none_or(|function| {
                    let index = if function.nil_terminated {
                        i
                    } else {
                        i.min(function.parameters.len().saturating_sub(1))
                    };

                    function
                        .parameters
                        .get_index(index)
                        .is_some_and(|(_, &parameter)| {
                            graph.references(parameter) == 1
                                && (graph.unconditional_references(parameter) == 1
                                    || graph.captured_references(parameter) == 1)
                        })
                });

                if substituted_once {
                    visit_hir(db, graph, arg, false);
                } else {
                    graph.in_new_region(|graph| visit_hir(db, graph, arg, false));
                }
            }
        }
        Hir::Unary(_op, arg) => {
            visit_hir(db, graph, *arg, false);
        }
        Hir::Binary(op, lhs, rhs) => {
            visit_hir(db, graph, *lhs, false);

            // The right hand side is skipped by short-circuiting operators
            if matches!(op, BinaryOp::And | BinaryOp::Or) {
                graph.in_new_region(|graph| visit_hir(db, graph, *rhs, false));
            } else {
                visit_hir(db, graph, *rhs, false);
            }
        }
        Hir::CoinId(parent, puzzle, amount) => {
            visit_hir(db, graph, *parent, false);
            visit_hir(db, graph, *puzzle, false);
            visit_hir(db, graph, *amount, false);
        }
        Hir::Substr(hir, start, end) => {
            visit_hir(db, graph, *hir, false);
            visit_hir(db, graph, *start, false);
            if let Some(end) = end {
                visit_hir(db, graph, *end, false);
            }
        }
        Hir::G1Map(data, dst) => {
            visit_hir(db, graph, *data, false);
            if let Some(dst) = dst {
                visit_hir(db, graph, *dst, false);
            }
        }
        Hir::G2Map(data, dst) => {
            visit_hir(db, graph, *data, false);
            if let Some(dst) = dst {
                visit_hir(db, graph, *dst, false);
            }
        }
        Hir::Modpow(base, exponent, modulus) => {
            visit_hir(db, graph, *base, false);
            visit_hir(db, graph, *exponent, false);
            visit_hir(db, graph, *modulus, false);
        }
        Hir::BlsPairingIdentity(args) => {
            for arg in args {
                visit_hir(db, graph, *arg, false);
            }
        }
        Hir::BlsVerify(sig, args) => {
            visit_hir(db, graph, *sig, false);
            for arg in args {
                visit_hir(db, graph, *arg, false);
            }
        }
        Hir::Secp256K1Verify(sig, pk, msg) => {
            visit_hir(db, graph, *sig, false);
            visit_hir(db, graph, *pk, false);
            visit_hir(db, graph, *msg, false);
        }
        Hir::Secp256R1Verify(sig, pk, msg) => {
            visit_hir(db, graph, *sig, false);
            visit_hir(db, graph, *pk, false);
            visit_hir(db, graph, *msg, false);
        }
        Hir::InfinityG1 => {}
        Hir::InfinityG2 => {}
        Hir::ClvmOp(_op, args) => {
            visit_hir(db, graph, *args, false);
        }
    }
}

fn visit_symbol(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    let is_lambda = std::mem::take(&mut graph.visiting_lambda);

    // We've already visited this symbol, so we don't need to track its dependencies again
    if !graph.visited.insert(symbol) {
        return;
    }

    graph.stack.insert(symbol);

    match db.symbol(symbol) {
        Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {}
        Symbol::Function(function) => {
            graph
                .locals
                .entry(symbol)
                .or_default()
                .extend(function.parameters.values().copied());

            // Function bodies are evaluated whenever they are called, rather than where they
            // are referenced, other than the root function which is evaluated exactly once
            let region = if graph.stack.len() == 1 {
                ROOT_REGION
            } else {
                graph.new_region()
            };

            // Lambdas capture their environment when they are created, but named functions
            // receive their captures each time they are called
            let lambda_regions = if is_lambda {
                graph.lambda_regions.clone()
            } else {
                std::mem::take(&mut graph.lambda_regions)
            };

            graph.in_region(region, |graph| {
                for &parameter in function.parameters.values() {
                    graph.definition_regions.insert(parameter, region);
                }

                visit_hir(db, graph, function.body, false);
            });

            graph.lambda_regions = lambda_regions;
        }
        Symbol::Constant(constant) => {
            let lambda_regions = std::mem::take(&mut graph.lambda_regions);
            graph.in_region(ROOT_REGION, |graph| {
                visit_hir(db, graph, constant.value.hir, false);
            });
            graph.lambda_regions = lambda_regions;
        }
        Symbol::Binding(binding) => {
            // Bindings are evaluated where they are defined, unless they are inline, in which
            // case they are evaluated wherever they are referenced
            let (region, lambdas) = match graph.definition_regions.get(&symbol) {
                Some(&region) if !binding.inline => (
                    region,
                    graph
                        .definition_lambdas
                        .get(&symbol)
                        .cloned()
                        .unwrap_or_default(),
                ),
                _ => (graph.new_region(), Vec::new()),
            };

            let lambda_regions = std::mem::replace(&mut graph.lambda_regions, lambdas);
            graph.in_region(region, |graph| {
                visit_hir(db, graph, binding.value.hir, false);
            });
            graph.lambda_regions = lambda_regions;
        }
    }

    graph.stack.pop().unwrap();
}

fn visit_symbol_reference(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    // We should add the symbol to the dependencies of the symbol we're currently visiting
    if let Some(last) = graph.stack.last() {
        graph.dependencies.entry(*last).or_default().insert(symbol);
        *graph.references.entry(symbol).or_insert(0) += 1;

        // Constants are evaluated as part of the environment of the root function
        let definition_region = if matches!(db.symbol(symbol), Symbol::Constant(_)) {
            Some(ROOT_REGION)
        } else {
            graph.definition_regions.get(&symbol).copied()
        };

        if let Some(region) = definition_region {
            if region == graph.region {
                *graph.unconditional_references.entry(symbol).or_insert(0) += 1;
            } else if graph.lambda_regions.contains(&region) {
                *graph.captured_references.entry(symbol).or_insert(0) += 1;
            }
        }
    }

    visit_symbol(db, graph, symbol);
}
