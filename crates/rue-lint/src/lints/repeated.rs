use std::{collections::HashMap, mem::Discriminant};

use rue_diagnostic::{Lint, Note, SrcLoc};
use rue_hir::{BinaryOp, Database, Hir, HirId, Statement, Symbol, SymbolId, UnaryOp};
use rue_lir::ClvmOp;

use crate::{HirLintPass, LintContext, equality::hir_eq, text::encloses};

/// The compiler doesn't reuse the result of an expression that's evaluated more than once, so an
/// expensive call (such as a hash, or a call to a function) that's evaluated again with the same
/// arguments costs as much as the first time.
#[derive(Debug, Default)]
pub(crate) struct RepeatedExpensiveCall {
    /// The calls seen so far in each function being walked, from the outermost to the innermost,
    /// grouped by their [`CallKey`] so that each call is only compared with similar ones.
    functions: Vec<HashMap<CallKey, Vec<Call>>>,
    /// The calls that have been reported, so that the calls inside of them aren't reported again.
    reported: Vec<SrcLoc>,
}

#[derive(Debug)]
struct Call {
    hir: HirId,
    path: Vec<Branch>,
}

/// What a call is, which calls must share to be equal: the kind of node, the function it calls
/// (if it's a named function), and the number of arguments.
type CallKey = (Discriminant<Hir>, Option<SymbolId>, usize);

/// One of the branches that a node is inside of, which is only evaluated in some cases. A call is
/// only evaluated whenever another one is if it's inside of all the same branches.
type Branch = (HirId, usize);

impl HirLintPass for RepeatedExpensiveCall {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::RepeatedExpensiveCall]
    }

    fn check_function(&mut self, _cx: &mut LintContext<'_>, _symbol: SymbolId, _body: HirId) {
        self.functions.push(HashMap::new());
    }

    fn check_function_post(&mut self, _cx: &mut LintContext<'_>, _symbol: SymbolId, _: HirId) {
        self.functions.pop();
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

        let Some(calls) = self
            .functions
            .last_mut()
            .map(|calls| calls.entry(key).or_default())
        else {
            return;
        };

        let earlier = calls
            .iter()
            .find(|call| path.starts_with(&call.path) && hir_eq(cx.db(), call.hir, hir))
            .and_then(|call| cx.db().hir_srcloc(call.hir).cloned());

        calls.push(Call { hir, path });

        let Some(earlier) = earlier else {
            return;
        };

        if self
            .reported
            .iter()
            .any(|reported| encloses(reported, &srcloc))
        {
            return;
        }

        self.reported.push(srcloc.clone());

        if let Some(diagnostic) = cx.report(
            Lint::RepeatedExpensiveCall,
            srcloc,
            "This call is evaluated again with the same arguments, which costs as much as the first time",
        ) {
            diagnostic
                .notes
                .push(Note::at(earlier, "It's first evaluated here"));
            diagnostic.notes.push(Note::new(
                "Store the result in a `let` binding and use that instead",
            ));
        }
    }
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
