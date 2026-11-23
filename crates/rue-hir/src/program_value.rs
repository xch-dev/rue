//! Finds the functions whose value is the program they're compiled to on their own.
//!
//! A named function that isn't defined inside of another one can be the entrypoint of a program,
//! and its value is a program that's called the same way, with its arguments as the environment.
//! So where it's used as a value, such as `tree_hash(puzzle)`, it's that program: the one that
//! `rue build` compiles it to, with release options, whatever program it's used in, so that its
//! tree hash is the same everywhere. Otherwise its value would be lowered as part of the program
//! it's used in, which decides what to inline and how to lay out the environments for the whole
//! program, and would make it a different program in each one.
//!
//! The program of a function that's used as a value is a constant in each program that uses it,
//! so a function can't be in its own program that way: if `a` uses `b` as a value and `b` uses
//! `a`, neither program could contain the other. Those functions are lowered as closures instead,
//! as is a lambda or a function defined in another one, which capture values that are only known
//! at runtime. A function that uses itself as a value is fine, since it's the entrypoint of its
//! own program, which refers to itself through the environment.

use std::collections::{HashMap, HashSet};

use crate::{Database, FunctionKind, Hir, HirId, Statement, Symbol, SymbolId};

/// Whether each of the candidates (see [`is_candidate`]) can be lowered to its own program where
/// it's used as a value: it isn't in a program that its own program would contain as a value.
pub(crate) fn decide_program_values(
    db: &Database,
    candidates: &[SymbolId],
) -> Vec<(SymbolId, bool)> {
    let mut value_references: HashMap<SymbolId, HashSet<SymbolId>> = HashMap::new();
    let mut references_of = |symbol: SymbolId| -> HashSet<SymbolId> {
        value_references
            .entry(symbol)
            .or_insert_with(|| find_value_references(db, symbol))
            .clone()
    };

    candidates
        .iter()
        .map(|&symbol| {
            // Every program that the function's program would contain as a value, and every one
            // those would contain, is found by following everything that it refers to. If any of
            // them contains this function's program, it would contain itself. Following the
            // functions that end up being closures instead is only more than is needed.
            let references = references_of(symbol);

            let is_program_value = !references.iter().any(|&reference| {
                reference != symbol && references_of(reference).contains(&symbol)
            });

            (symbol, is_program_value)
        })
        .collect()
}

/// Whether the function can be the entrypoint of a program, and so be lowered to one: it's
/// named and not defined inside of another function. Only functions that are declared as items
/// have names, and those only capture other items.
pub(crate) fn is_candidate(db: &Database, symbol: SymbolId) -> bool {
    matches!(
        db.symbol(symbol),
        Symbol::Function(function)
            if function.name.is_some()
                && matches!(
                    function.kind,
                    FunctionKind::Sequential | FunctionKind::BinaryTree | FunctionKind::Inline
                )
    )
}

/// The candidates that are referenced as values (rather than called by name) anywhere in what
/// the symbol refers to, following both calls and values.
fn find_value_references(db: &Database, root: SymbolId) -> HashSet<SymbolId> {
    let mut result = HashSet::new();
    let mut visited = HashSet::new();
    let mut stack = Vec::new();

    let mut visit = |symbol: SymbolId, stack: &mut Vec<HirId>| {
        if !visited.insert(symbol) {
            return;
        }

        match db.symbol(symbol) {
            Symbol::Function(function) => stack.push(function.body),
            Symbol::Constant(constant) => stack.push(constant.value.hir),
            Symbol::Binding(binding) => stack.push(binding.value.hir),
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {}
        }
    };

    visit(root, &mut stack);

    while let Some(hir) = stack.pop() {
        match db.hir(hir) {
            Hir::Reference(symbol) => {
                if is_candidate(db, *symbol) {
                    result.insert(*symbol);
                }

                visit(*symbol, &mut stack);
            }
            Hir::Lambda(lambda) => visit(*lambda, &mut stack),
            Hir::Program(function) => {
                result.insert(*function);
                visit(*function, &mut stack);
            }
            Hir::FunctionCall(call) => {
                if let Hir::Reference(symbol) = db.hir(db.without_casts(call.function)) {
                    visit(*symbol, &mut stack);
                } else {
                    stack.push(call.function);
                }

                stack.extend(call.args.iter().copied());
            }
            Hir::Const(expr) => {
                stack.push(expr.value);
                stack.extend(expr.result);
            }
            // Programs are compiled for release, so only what a release build lowers is
            // followed: nothing after a `return` or `raise`, and no `debug` statements, values of
            // a `raise` or messages of an `assert`
            Hir::Block(block) => {
                for statement in block.reachable_statements() {
                    if let Statement::Let(binding, _) = statement {
                        visit(*binding, &mut stack);
                    }

                    stack.extend(statement.lowered_hirs(false));
                }

                stack.extend(block.reachable_body());
            }
            node => stack.extend(node.children()),
        }
    }

    result
}
