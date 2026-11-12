use std::{collections::HashMap, mem::Discriminant};

use indexmap::IndexMap;
use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{BinaryOp, Database, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};
use rue_lir::ClvmOp;

use crate::{
    HirLintPass, LintContext,
    cost::{BINDING_SIZE, COST_PER_BYTE, Cost, CostModel},
    equality::hir_eq,
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
    /// The binding whose value is the call, along with the block it's declared in.
    binding: Option<(String, HirId)>,
}

#[derive(Debug)]
struct Repeated {
    first: SrcLoc,
    /// The binding whose value is the first call, along with the block it's declared in.
    binding: Option<(String, HirId)>,
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
            min_savings,
            functions: Vec::new(),
            costs: HashMap::new(),
        }
    }
}

impl HirLintPass for RepeatedExpensiveCall {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::RepeatedExpensiveCall]
    }

    fn check_function(&mut self, _cx: &mut LintContext<'_>, _symbol: SymbolId, _body: HirId) {
        self.functions.push(Function::default());
    }

    fn check_function_post(&mut self, cx: &mut LintContext<'_>, _symbol: SymbolId, _: HirId) {
        if let Some(function) = self.functions.pop() {
            self.report(cx, function);
        }
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        if !is_expensive(node) {
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

                // Using the result costs about as much as using a parameter
                let reference = Cost {
                    size: 1,
                    runtime: 44,
                };

                entry.insert(Repeated {
                    first,
                    binding: earlier.binding.clone(),
                    repeats: Vec::new(),
                    savings: cost.total().saturating_sub(reference.total()),
                })
            }
        };

        // The binding can only be used inside of the block it's declared in
        let in_scope = repeated
            .binding
            .as_ref()
            .is_some_and(|(_, block)| cx.parents().contains(block));

        repeated.repeats.push((srcloc, in_scope));
    }
}

impl RepeatedExpensiveCall {
    fn report(&self, cx: &mut LintContext<'_>, function: Function) {
        let mut reports = Vec::new();

        for repeated in function.repeated.into_values() {
            let unbound = repeated.repeats.iter().filter(|(_, bound)| !bound).count() as u64;

            // The repeats that can't use an existing binding share a new one
            let unbound_savings =
                (repeated.savings * unbound).saturating_sub(BINDING_SIZE * COST_PER_BYTE);

            for (srcloc, bound) in &repeated.repeats {
                let (savings, help) = match (&repeated.binding, bound) {
                    (Some((name, _)), true) => (repeated.savings, format!("Use `{name}` instead")),
                    _ => (
                        unbound_savings,
                        "Store the result in a `let` binding and use that instead".to_string(),
                    ),
                };

                if savings >= self.min_savings.max(1) {
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

/// The name of the binding whose value is the call, and the block it's declared in, if the call
/// is the whole value of a binding.
fn binding_of(cx: &LintContext<'_>, call: HirId) -> Option<(String, HirId)> {
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

        if db.without_casts(binding.value.hir) != call {
            return None;
        }

        let name = binding.name.as_ref()?.text().to_string();

        // A name starting with `_` is unused on purpose, and destructuring doesn't have a name
        (!name.starts_with('_')).then_some((name, *block))
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
