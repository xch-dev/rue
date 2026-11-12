use std::collections::{HashMap, HashSet};

use rue_ast::{AstFunctionItem, AstNode};
use rue_diagnostic::{Lint, Note, SourceKind, SrcLoc};
use rue_hir::{BinaryOp, Declaration, Hir, HirId, Symbol, SymbolId};

use crate::{
    HirLintPass, LintContext, LintInput, NameKey,
    flow::{Bindings, Fate, follow, used},
    name_key, token_key,
};

/// Reports parameters of an entrypoint (`main` or an exported function) that are called as
/// puzzles, but whose tree hash is never compared with anything. A parameter that isn't curried
/// into the program comes from the solution, so anyone spending the coin can choose it, and
/// calling it runs whatever code they chose. Checking its tree hash against a trusted hash (such as
/// one that's curried in) is what makes it safe.
///
/// A parameter marked `#[curried]` is part of the program's puzzle hash, so it's trusted, as is one
/// named in `SCREAMING_SNAKE_CASE`, which is the convention for curried parameters. To avoid
/// false positives, a parameter that's passed to another function is assumed to be checked there,
/// and a hash that's part of the output (such as in a condition) is assumed to be checked by it.
#[derive(Debug)]
pub(crate) struct UncheckedPuzzleCall {
    /// The parameters of entrypoints marked `#[curried]`, by the location of their names.
    curried: HashSet<NameKey>,
    /// The entrypoints of the program.
    programs: HashSet<SymbolId>,
    /// The entrypoints that have been walked, and the parameters of each of them.
    entrypoints: HashMap<SymbolId, HashSet<SymbolId>>,
    /// Where each parameter of an entrypoint is called.
    calls: HashMap<SymbolId, Vec<SrcLoc>>,
    /// The fates of the tree hashes of each parameter of an entrypoint.
    hashes: HashMap<SymbolId, Vec<Fate>>,
    /// The parameters of entrypoints that are passed to other functions.
    passed: HashSet<SymbolId>,
    bindings: Bindings,
}

impl UncheckedPuzzleCall {
    pub(crate) fn new(input: &LintInput<'_>) -> Self {
        let mut curried = HashSet::new();

        for (source, document) in &input.documents {
            for function in document
                .syntax()
                .descendants()
                .filter_map(AstFunctionItem::cast)
            {
                for parameter in function.parameters() {
                    let is_curried = parameter.attributes().any(|attribute| {
                        attribute
                            .name()
                            .is_some_and(|name| name.text() == "curried")
                    });

                    let name = parameter.binding().and_then(|binding| match binding {
                        rue_ast::AstBinding::NamedBinding(binding) => binding.name(),
                        _ => None,
                    });

                    if is_curried && let Some(name) = name {
                        curried.insert(token_key(source, &name));
                    }
                }
            }
        }

        let programs = input
            .entrypoints
            .iter()
            .filter_map(|declaration| match *declaration {
                Declaration::Symbol(symbol) => Some(symbol),
                Declaration::Type(_) => None,
            })
            .collect();

        Self {
            curried,
            programs,
            entrypoints: HashMap::new(),
            calls: HashMap::new(),
            hashes: HashMap::new(),
            passed: HashSet::new(),
            bindings: Bindings::default(),
        }
    }
}

impl HirLintPass for UncheckedPuzzleCall {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UncheckedPuzzleCall]
    }

    fn check_function(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId, _body: HirId) {
        let db = cx.db();

        // Only the entrypoint itself, not the lambdas inside of it, is called with the solution
        if cx.walk.functions.len() != 1
            || !self.programs.contains(&symbol)
            || db.tests().any(|test| test.symbol == symbol)
        {
            return;
        }

        let Symbol::Function(function) = db.symbol(symbol) else {
            return;
        };

        self.entrypoints
            .insert(symbol, function.parameters.values().copied().collect());
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let db = cx.db();

        let Some(Declaration::Symbol(entrypoint)) = cx.enclosing_declaration() else {
            return;
        };

        let Some(parameters) = self.entrypoints.get(&entrypoint) else {
            return;
        };

        match node {
            Hir::Reference(symbol) => {
                self.bindings.reference(db, *symbol, || hash_fate(cx, hir));
            }
            Hir::FunctionCall(call) => {
                let parameter = |hir: HirId| match db.hir(db.without_casts(hir)) {
                    Hir::Reference(symbol) if parameters.contains(symbol) => Some(*symbol),
                    _ => None,
                };

                if let Some(parameter) = parameter(call.function)
                    && let Some(srcloc) = db.hir_srcloc(hir)
                {
                    self.calls
                        .entry(parameter)
                        .or_default()
                        .push(srcloc.clone());
                }

                let is_tree_hash = is_tree_hash(cx, call.function);

                for &arg in &call.args {
                    let Some(parameter) = parameter(arg) else {
                        continue;
                    };

                    if is_tree_hash {
                        let fate = hash_fate(cx, hir);
                        self.hashes.entry(parameter).or_default().push(fate);
                    } else {
                        self.passed.insert(parameter);
                    }
                }
            }
            _ => {}
        }
    }

    fn finish(&mut self, cx: &mut LintContext<'_>) {
        let mut parameters = self
            .entrypoints
            .values()
            .flatten()
            .copied()
            .collect::<Vec<_>>();

        parameters.sort_by_key(id_arena::Id::index);

        for parameter in parameters {
            let Some(call) = self.calls.get(&parameter).and_then(|calls| calls.first()) else {
                continue;
            };

            if self.passed.contains(&parameter) {
                continue;
            }

            let Some(name) = cx.db().symbol(parameter).name() else {
                continue;
            };

            if name_key(&name).is_some_and(|key| self.curried.contains(&key))
                || is_screaming_snake_case(name.text())
            {
                continue;
            }

            let checked = self.hashes.get(&parameter).is_some_and(|fates| {
                fates
                    .iter()
                    .any(|fate| self.bindings.reaches(fate, &mut Vec::new()))
            });

            if checked {
                continue;
            }

            let name = name.text();

            if let Some(diagnostic) = cx.report(
                Lint::UncheckedPuzzleCall,
                call.clone(),
                format!(
                    "`{name}` is called, but its tree hash is never checked, so whoever spends the coin can choose what it does"
                ),
            ) {
                diagnostic.notes.push(Note::new(format!(
                    "Compare `tree_hash({name})` with a hash that the puzzle trusts, such as one that's curried in"
                )));
                diagnostic.notes.push(Note::new(format!(
                    "If `{name}` is curried into the puzzle, mark the parameter with `#[curried]`"
                )));
            }
        }
    }
}

/// Whether a name is in `SCREAMING_SNAKE_CASE`, which is the convention for curried parameters
/// in Chialisp, so parameters named that way are treated as curried.
fn is_screaming_snake_case(name: &str) -> bool {
    name.chars().any(|char| char.is_ascii_uppercase())
        && name
            .chars()
            .all(|char| char.is_ascii_uppercase() || char.is_ascii_digit() || char == '_')
}

/// Whether the function is the standard library's `tree_hash`.
fn is_tree_hash(cx: &LintContext<'_>, function: HirId) -> bool {
    let db = cx.db();

    let Hir::Reference(symbol) = db.hir(db.without_casts(function)) else {
        return false;
    };

    let Symbol::Function(function) = db.symbol(*symbol) else {
        return false;
    };

    function.name.as_ref().is_some_and(|name| {
        name.text() == "tree_hash"
            && name
                .srcloc()
                .is_some_and(|srcloc| matches!(srcloc.source.kind, SourceKind::Std(_)))
    })
}

/// Whether a hash (or a value computed from it) reaches a comparison, or the output.
fn hash_fate(cx: &LintContext<'_>, hir: HirId) -> Fate {
    follow(cx, hir, |db, parent, child| match db.hir(parent) {
        Hir::Binary(BinaryOp::Eq | BinaryOp::Ne, ..) => Some(Fate::Reached),
        // The condition of an `if` only decides which branch is taken
        Hir::If(condition, ..) => (*condition == child).then(|| used(db, parent)),
        // Calling a function with the hash computes something from it, such as a curried puzzle
        // hash, which is followed in turn
        Hir::FunctionCall(call) => (call.function == child).then(|| used(db, parent)),
        Hir::Guard(_) => Some(used(db, parent)),
        // Anything else computes a value from it, which is followed in turn
        _ => None,
    })
}
