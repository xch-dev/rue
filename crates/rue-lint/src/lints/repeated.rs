use std::{collections::HashMap, mem::Discriminant};

use indexmap::IndexMap;
use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{
    BinaryOp, Database, DependencyGraph, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp,
};
use rue_lir::ClvmOp;
use rue_types::TypeId;

use crate::{
    HirLintPass, LintContext,
    cost::{BINDING_SIZE, COST_PER_BYTE, Cost, CostModel, REFERENCE},
    equality::hir_eq,
    measure::program_graph,
    text::encloses,
};

/// The compiler doesn't reuse the result of an expression that's evaluated more than once, so an
/// expensive call (such as a hash, or a call to a function) that's evaluated again with the same
/// arguments costs as much as the first time. Each byte of a program costs much more than most
/// calls do to run, though, and a new binding adds more bytes than a short call, so a call is only
/// reported if reusing its result would make the program cheaper.
#[derive(Debug)]
pub(crate) struct RepeatedExpensiveCall {
    min_savings: u64,
    /// The calls seen so far in each function being walked, from the outermost to the innermost.
    functions: Vec<Function>,
    /// The estimated cost of each function body.
    costs: HashMap<SymbolId, Option<Cost>>,
    /// The dependencies of the outermost function being walked, which say how many times each
    /// binding in it is referenced in a release build.
    graph: Option<DependencyGraph>,
}

#[derive(Debug, Default)]
struct Function {
    /// The calls seen so far, grouped by their [`CallKey`] so that each call is only compared
    /// with similar ones.
    calls: HashMap<CallKey, Vec<Call>>,
    /// The calls that are evaluated again, by the first call they repeat, in the order they're
    /// first evaluated.
    repeated: IndexMap<HirId, Repeated>,
}

#[derive(Debug)]
struct Call {
    hir: HirId,
    path: Vec<Branch>,
    /// The binding whose value is the call.
    binding: Option<Binding>,
}

/// A binding whose value is a call, which the calls that repeat it could use instead.
#[derive(Debug, Clone)]
struct Binding {
    name: String,
    symbol: SymbolId,
    /// The type of the binding, which may be different from the call's (such as with a cast).
    ty: TypeId,
    /// The block that the binding is declared in.
    block: HirId,
}

#[derive(Debug)]
struct Repeated {
    first: SrcLoc,
    /// The binding whose value is the first call.
    binding: Option<Binding>,
    /// The calls that repeat it, and whether each of them could use the binding instead.
    repeats: Vec<(SrcLoc, bool)>,
    /// What each repeat would save by using the result of the first call, rather than evaluating
    /// it again.
    savings: u64,
}

/// What a call is, which calls must share to be equal: the kind of node, the function it calls
/// (if it's a named function), and the number of arguments.
type CallKey = (Discriminant<Hir>, Option<SymbolId>, usize);

/// One of the branches that a node is inside of, which is only evaluated in some cases. A call is
/// only evaluated whenever another one is if it's inside of all the same branches.
type Branch = (HirId, usize);

impl RepeatedExpensiveCall {
    pub(crate) fn new(min_savings: u64) -> Self {
        Self {
            min_savings: min_savings.max(1),
            functions: Vec::new(),
            costs: HashMap::new(),
            graph: None,
        }
    }
}

impl HirLintPass for RepeatedExpensiveCall {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::RepeatedExpensiveCall]
    }

    fn check_function(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId, _body: HirId) {
        // The compiler inlines a binding that's only referenced once, which this decides from
        if cx.walk.functions.len() == 1 {
            self.graph = Some(program_graph(cx.db(), symbol));
        }

        self.functions.push(Function::default());
    }

    fn check_function_post(&mut self, cx: &mut LintContext<'_>, _symbol: SymbolId, _: HirId) {
        if let Some(function) = self.functions.pop() {
            self.report(cx, function);
        }
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        // `debug` statements and `raise` messages aren't part of a release build
        if !is_expensive(node) || in_debug_only_code(cx, hir) {
            return;
        }

        let key = call_key(cx.db(), node);

        let Some(srcloc) = cx.db().hir_srcloc(hir).cloned() else {
            return;
        };

        let Some(path) = branch_path(cx, hir) else {
            return;
        };

        let Some(function) = self.functions.last_mut() else {
            return;
        };

        let calls = function.calls.entry(key).or_default();

        let earlier = calls
            .iter()
            .position(|call| path.starts_with(&call.path) && hir_eq(cx.db(), call.hir, hir));

        calls.push(Call {
            hir,
            path,
            binding: binding_of(cx, hir),
        });

        let Some(earlier) = earlier else {
            return;
        };

        let earlier = &calls[earlier];
        let db = cx.db();

        let repeated = match function.repeated.entry(earlier.hir) {
            indexmap::map::Entry::Occupied(entry) => entry.into_mut(),
            indexmap::map::Entry::Vacant(entry) => {
                let Some(first) = db.hir_srcloc(earlier.hir).cloned() else {
                    return;
                };

                let cost = CostModel::new(db, &mut self.costs).estimate(earlier.hir);

                entry.insert(Repeated {
                    first,
                    binding: earlier.binding.clone(),
                    repeats: Vec::new(),
                    savings: cost.total().saturating_sub(REFERENCE.total()),
                })
            }
        };

        // The binding can only be used inside of the block it's declared in, where another binding
        // with the same name doesn't shadow it, and its type must be usable where the call is
        let binding = repeated.binding.clone();
        let call_type = cx.db().hir_type(hir);

        let in_scope = binding.is_some_and(|binding| {
            cx.parents().contains(&binding.block)
                && cx.db().hir_scope(hir).is_some_and(|scope| {
                    cx.db()
                        .resolve_symbol_in(scope, &binding.name)
                        .is_some_and(|(symbol, _)| symbol == binding.symbol)
                })
                && call_type.is_some_and(|call_type| cx.is_assignable(binding.ty, call_type))
        });

        repeated.repeats.push((srcloc, in_scope));
    }
}

impl RepeatedExpensiveCall {
    /// The number of times a binding is referenced, which leaves out `debug` statements and code
    /// that isn't reached, as the compiler does.
    fn references(&self, symbol: SymbolId) -> usize {
        self.graph
            .as_ref()
            .map_or(0, |graph| graph.references(symbol))
    }

    fn report(&self, cx: &mut LintContext<'_>, function: Function) {
        let mut reports = Vec::new();

        for repeated in function.repeated.into_values() {
            let count = |bound: bool| {
                repeated
                    .repeats
                    .iter()
                    .filter(|(_, other)| *other == bound)
                    .count() as u64
            };

            // A binding that's never referenced isn't evaluated, so the calls after it aren't
            // evaluated again
            if let Some(binding) = &repeated.binding
                && self.references(binding.symbol) == 0
            {
                continue;
            }

            let new_binding = BINDING_SIZE * COST_PER_BYTE;

            // The repeats that can't use an existing binding share a new one
            let unbound_savings = (repeated.savings * count(false)).saturating_sub(new_binding);

            // A binding that's only referenced once is inlined into where it's used, so using it
            // again makes it a binding, which costs as much as a new one
            let bound_savings = match &repeated.binding {
                Some(binding) if self.references(binding.symbol) < 2 => {
                    (repeated.savings * count(true)).saturating_sub(new_binding)
                }
                _ => repeated.savings,
            };

            for (srcloc, bound) in &repeated.repeats {
                let (savings, help) = match (&repeated.binding, bound) {
                    (Some(binding), true) => {
                        (bound_savings, format!("Use `{}` instead", binding.name))
                    }
                    _ => (
                        unbound_savings,
                        "Store the result in a `let` binding and use that instead".to_string(),
                    ),
                };

                if savings >= self.min_savings {
                    reports.push((srcloc.clone(), repeated.first.clone(), help, savings));
                }
            }
        }

        // A call inside of another one that's reported isn't reported again, since using the
        // result of the outer call removes it
        reports.sort_by_key(|(srcloc, ..)| (srcloc.span.start, std::cmp::Reverse(srcloc.span.end)));

        let mut reported: Vec<SrcLoc> = Vec::new();

        for (srcloc, first, help, savings) in reports {
            if reported.iter().any(|outer| encloses(outer, &srcloc)) {
                continue;
            }

            reported.push(srcloc.clone());

            if let Some(diagnostic) = cx.report(
                Lint::RepeatedExpensiveCall,
                srcloc,
                "This call is evaluated again with the same arguments, which costs as much as the first time",
            ) {
                diagnostic.notes.push(Note::at(first, "It's first evaluated here"));
                diagnostic.notes.push(Note::new(format!(
                    "{help}, which saves an estimated cost of {savings}"
                )));
            }
        }
    }
}

/// Whether the node is inside of a `debug` statement or the message of a `raise`, which release
/// builds leave out.
fn in_debug_only_code(cx: &LintContext<'_>, hir: HirId) -> bool {
    let db = cx.db();
    let parents = cx.parents();

    // Each enclosing node, along with the child of it that the node is in
    let children = parents.iter().skip(1).chain([&hir]);

    parents
        .iter()
        .zip(children)
        .any(|(&parent, &child)| match db.hir(parent) {
            Hir::Block(block) => block.statements.iter().any(|statement| match statement {
                Statement::Debug(value, _) | Statement::Raise(Some(value), _) => *value == child,
                _ => false,
            }),
            _ => false,
        })
}

/// The binding whose value is the call, if the call is the whole value of a binding.
fn binding_of(cx: &LintContext<'_>, call: HirId) -> Option<Binding> {
    let db = cx.db();

    // The value of a binding is visited from the block it's declared in
    let block = cx
        .parents()
        .iter()
        .rev()
        .find(|&&parent| !matches!(db.hir(parent), Hir::Cast(_)))?;

    let Hir::Block(statements) = db.hir(*block) else {
        return None;
    };

    statements.statements.iter().find_map(|statement| {
        let Statement::Let(symbol, _) = statement else {
            return None;
        };

        let Symbol::Binding(binding) = db.symbol(*symbol) else {
            return None;
        };

        // An inline binding is inlined wherever it's used, so using it doesn't save anything
        if binding.inline || db.without_casts(binding.value.hir) != call {
            return None;
        }

        let name = binding.name.as_ref()?.text().to_string();

        // A name starting with `_` is unused on purpose, and destructuring doesn't have a name
        (!name.starts_with('_')).then_some(Binding {
            name,
            symbol: *symbol,
            ty: binding.value.ty,
            block: *block,
        })
    })
}

fn call_key(db: &Database, node: &Hir) -> CallKey {
    let (function, args) = match node {
        Hir::FunctionCall(call) => {
            let function = match db.hir(db.without_casts(call.function)) {
                Hir::Reference(symbol) if matches!(db.symbol(*symbol), Symbol::Function(_)) => {
                    Some(*symbol)
                }
                _ => None,
            };

            (function, call.args.len())
        }
        _ => (None, 0),
    };

    (std::mem::discriminant(node), function, args)
}

fn is_expensive(node: &Hir) -> bool {
    match node {
        Hir::FunctionCall(_)
        | Hir::CoinId(..)
        | Hir::Modpow(..)
        | Hir::G1Map(..)
        | Hir::G2Map(..)
        | Hir::BlsPairingIdentity(_)
        | Hir::BlsVerify(..)
        | Hir::Secp256K1Verify(..)
        | Hir::Secp256R1Verify(..) => true,
        Hir::ClvmOp(op, _) => matches!(op, ClvmOp::Sha256 | ClvmOp::Keccak256),
        Hir::Unary(op, _) => matches!(
            op,
            UnaryOp::Sha256
                | UnaryOp::Sha256Inline
                | UnaryOp::Keccak256
                | UnaryOp::Keccak256Inline
                | UnaryOp::PubkeyForExp
        ),
        _ => false,
    }
}

/// The branches inside of the current function that the node is inside of, or `None` if it's
/// evaluated at compile time.
fn branch_path(cx: &LintContext<'_>, hir: HirId) -> Option<Vec<Branch>> {
    let db = cx.db();
    let parents = cx.parents();

    // Lambdas are walked as functions of their own, so their parents aren't part of this one
    let start = parents
        .iter()
        .rposition(|&parent| matches!(db.hir(parent), Hir::Lambda(_)))
        .map_or(0, |index| index + 1);

    let nodes = parents[start..]
        .iter()
        .copied()
        .chain(std::iter::once(hir))
        .collect::<Vec<_>>();

    let mut path = Vec::new();

    for pair in nodes.windows(2) {
        let (parent, child) = (pair[0], pair[1]);

        match db.hir(parent) {
            Hir::Const(_) => return None,
            Hir::If(_, then, else_, false) => {
                if child == *then {
                    path.push((parent, 1));
                } else if child == *else_ {
                    path.push((parent, 2));
                }
            }
            Hir::Binary(BinaryOp::And | BinaryOp::Or, _, right) if child == *right => {
                path.push((parent, 1));
            }
            Hir::Block(block) => {
                if let Some(index) = block.statements.iter().position(|statement| {
                    matches!(statement, Statement::If(statement) if statement.then == child)
                }) {
                    path.push((parent, index));
                }
            }
            _ => {}
        }
    }

    Some(path)
}
