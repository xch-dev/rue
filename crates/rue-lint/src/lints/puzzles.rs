use std::collections::{HashMap, HashSet};

use rue_ast::{AstBinding, AstFunctionParameter, AstNode};
use rue_diagnostic::{Lint, Note, SourceKind, SrcLoc};
use rue_hir::{BinaryOp, Database, Declaration, Hir, HirId, Symbol, SymbolId};

use crate::{
    CURRIED, HirLintPass, LintContext, LintInput, NameKey,
    flow::{Bindings, Checks, Fate, follow, used},
    name_key,
    text::keyword_locations,
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
pub(crate) struct UncheckedPuzzleCall<'a> {
    input: &'a LintInput<'a>,
    /// The parameters marked `#[curried]`, by the location of their names.
    curried: HashSet<NameKey>,
    /// The entrypoints that have been walked, and the parameters of each of them.
    entrypoints: HashMap<SymbolId, HashSet<SymbolId>>,
    /// Where each parameter of an entrypoint is first called.
    calls: HashMap<SymbolId, SrcLoc>,
    /// The fates of the tree hashes of each parameter of an entrypoint.
    hashes: HashMap<SymbolId, Vec<Fate>>,
    /// The parameters of entrypoints that are passed to other functions.
    passed: HashSet<SymbolId>,
    bindings: Bindings,
}

impl<'a> UncheckedPuzzleCall<'a> {
    pub(crate) fn new(input: &'a LintInput<'a>) -> Self {
        let curried = keyword_locations(input, |node| {
            let parameter = AstFunctionParameter::cast(node.clone())?;

            let attribute = parameter
                .attributes()
                .find_map(|attribute| attribute.name().filter(|name| name.text() == CURRIED))?;

            match parameter.binding()? {
                AstBinding::NamedBinding(binding) => Some((attribute, binding.name()?)),
                _ => None,
            }
        })
        .into_keys()
        .collect();

        Self {
            input,
            curried,
            entrypoints: HashMap::new(),
            calls: HashMap::new(),
            hashes: HashMap::new(),
            passed: HashSet::new(),
            bindings: Bindings::default(),
        }
    }
}

impl HirLintPass for UncheckedPuzzleCall<'_> {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::UncheckedPuzzleCall]
    }

    fn check_function(&mut self, cx: &mut LintContext<'_>, symbol: SymbolId, _body: HirId) {
        let db = cx.db();

        // Only the entrypoint itself, not the lambdas inside of it, is called with the solution
        if cx.walk.functions.len() != 1 || !self.input.programs(db).contains(&symbol) {
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
                        .or_insert_with(|| srcloc.clone());
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

        let parts = self.input.binding_parts(cx.db());

        for parameter in parameters {
            let Some(call) = self.calls.get(&parameter) else {
                continue;
            };

            if self.passed.contains(&parameter) {
                continue;
            }

            let Some(name) = cx.db().symbol(parameter).name() else {
                continue;
            };

            if name_key(&name).is_some_and(|key| self.curried.contains(&key))
                || is_curried_name(name.text())
            {
                continue;
            }

            let checked = self.hashes.get(&parameter).is_some_and(|fates| {
                fates
                    .iter()
                    .any(|fate| self.bindings.reaches(fate, parts, None))
            });

            if checked {
                continue;
            }

            let Some(srcloc) = name.srcloc().cloned() else {
                continue;
            };

            let name = name.text();

            // It's reported at the parameter, so that attributes on it apply
            if let Some(diagnostic) = cx.report(
                Lint::UncheckedPuzzleCall,
                srcloc,
                format!(
                    "`{name}` comes from the solution and is called, but its tree hash is never checked, so whoever spends the coin can choose what it does"
                ),
            ) {
                diagnostic.notes.push(Note::at(call.clone(), "It's called here"));
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
/// in Chialisp.
fn is_curried_name(name: &str) -> bool {
    name.chars().any(char::is_uppercase) && !name.chars().any(char::is_lowercase)
}

/// Whether the function is one that the standard library declares.
fn is_std_function(db: &Database, function: HirId) -> bool {
    let Hir::Reference(symbol) = db.hir(db.without_casts(function)) else {
        return false;
    };

    matches!(
        db.symbol(*symbol),
        Symbol::Function(function)
            if function.name.as_ref().and_then(|name| name.srcloc()).is_some_and(|srcloc| {
                matches!(srcloc.source.kind, SourceKind::Std(_))
            })
    )
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
    // An `assert` or `if` on a value computed from the hash checks it, such as an allow-list lookup
    follow(cx, hir, Checks::Reached, |db, parent, _child| {
        match db.hir(parent) {
            // A comparison checks the hash, and so does verifying a signature of it
            Hir::Binary(BinaryOp::Eq | BinaryOp::Ne, ..)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..) => Some(Fate::Reached),
            // A hash computed in a lambda isn't the entrypoint's output, and isn't compared
            Hir::Guard(_) | Hir::Lambda(_) => Some(used(db, parent)),
            // Passing the hash to a function of the program could check it there, as with passing the
            // parameter itself, but the standard library's functions only compute other values from it
            Hir::FunctionCall(call) if !is_std_function(db, call.function) => Some(Fate::Reached),
            // Anything else computes a value from it, which is followed in turn
            _ => None,
        }
    })
}
