use id_arena::Arena;
use indexmap::IndexSet;
use rue_mir::{Mir, MirId};

use crate::{Database, DependencyGraph, Hir, HirId, Symbol, SymbolId};

#[derive(Debug, Default, Clone)]
struct FunctionContext {
    bindings: IndexSet<SymbolId>,
    captures: IndexSet<SymbolId>,
    parameters: IndexSet<SymbolId>,
}

pub fn lower_main(
    db: &Database,
    arena: &mut Arena<Mir>,
    graph: &DependencyGraph,
    main: SymbolId,
) -> MirId {
    let Symbol::Function(function) = db.symbol(main) else {
        panic!("main is not a function");
    };

    let captures: IndexSet<SymbolId> = graph
        .dependencies(main)
        .into_iter()
        .filter(|symbol| matches!(db.symbol(*symbol), Symbol::Function(_)))
        .collect();

    let mut main_context = FunctionContext {
        bindings: IndexSet::new(),
        captures: captures.clone(),
        parameters: function.parameters.iter().copied().collect(),
    };

    let body = lower_hir(db, arena, graph, &mut main_context, function.body);

    let function = arena.alloc(Mir::Function {
        body,
        captures: captures.len(),
        parameters: function.parameters.len(),
    });

    let mut definitions = Vec::new();

    for symbol in captures {
        match db.symbol(symbol) {
            Symbol::Function(function) => {
                let mut function_context = FunctionContext {
                    bindings: IndexSet::new(),
                    captures: IndexSet::new(),
                    parameters: function.parameters.iter().copied().collect(),
                };

                let body = lower_hir(db, arena, graph, &mut function_context, function.body);

                let function = arena.alloc(Mir::Function {
                    body,
                    captures: function_context.captures.len(),
                    parameters: function.parameters.len(),
                });

                definitions.push(function);
            }
            _ => unreachable!(),
        }
    }

    arena.alloc(Mir::Curry {
        body: function,
        args: definitions,
    })
}

#[allow(clippy::only_used_in_recursion)]
fn lower_hir(
    db: &Database,
    arena: &mut Arena<Mir>,
    graph: &DependencyGraph,
    context: &mut FunctionContext,
    hir: HirId,
) -> MirId {
    match db.hir(hir).clone() {
        Hir::Unresolved => unreachable!(),
        Hir::Atom(atom) => arena.alloc(Mir::Atom(atom)),
        Hir::Reference(symbol) => {
            if let Some(index) = context.bindings.get_index_of(&symbol) {
                arena.alloc(Mir::Binding(index))
            } else if let Some(index) = context.parameters.get_index_of(&symbol) {
                arena.alloc(Mir::Parameter(index))
            } else {
                arena.alloc(Mir::Capture(context.captures.insert_full(symbol).0))
            }
        }
        Hir::Block(block) => {
            // TODO: Properly implement blocks
            lower_hir(db, arena, graph, context, block.body.unwrap())
        }
        Hir::Unary(op, hir) => {
            let mir = lower_hir(db, arena, graph, context, hir);
            arena.alloc(Mir::Unary(op, mir))
        }
        Hir::Binary(op, left, right) => {
            let left = lower_hir(db, arena, graph, context, left);
            let right = lower_hir(db, arena, graph, context, right);
            arena.alloc(Mir::Binary(op, left, right))
        }
    }
}
