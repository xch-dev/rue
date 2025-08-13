use id_arena::Arena;
use indexmap::IndexSet;
use rue_mir::{Mir, MirId};

use crate::{Database, DependencyGraph, Hir, HirId, Statement, Symbol, SymbolId, bigint_atom};

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
        Hir::Nil => arena.alloc(Mir::Atom(vec![])),
        Hir::String(value) => arena.alloc(Mir::Atom(value.as_bytes().to_vec())),
        Hir::Int(value) => arena.alloc(Mir::Atom(bigint_atom(value.clone()))),
        Hir::Bool(value) => arena.alloc(Mir::Atom(if value { vec![1] } else { vec![] })),
        Hir::Bytes(atom) => arena.alloc(Mir::Atom(atom)),
        Hir::Reference(symbol) => {
            if let Some(index) = context.bindings.get_index_of(&symbol) {
                arena.alloc(Mir::Binding(index))
            } else if let Some(index) = context.parameters.get_index_of(&symbol) {
                arena.alloc(Mir::Parameter(index))
            } else {
                arena.alloc(Mir::Capture(context.captures.insert_full(symbol).0))
            }
        }
        Hir::Block(mut block) => {
            // TODO: Implement statement only blocks

            let mut binding_groups = Vec::new();

            while !block.statements.is_empty() {
                let mut remaining_bindings = IndexSet::new();

                while let Some(stmt) = block.statements.last()
                    && let Statement::Let(symbol) = stmt
                {
                    remaining_bindings.insert(*symbol);
                    block.statements.pop();
                }

                while !remaining_bindings.is_empty() {
                    let mut symbols = IndexSet::new();

                    for &symbol in &remaining_bindings {
                        if graph
                            .dependencies(symbol)
                            .iter()
                            .all(|symbol| !remaining_bindings.contains(symbol))
                        {
                            symbols.insert(symbol);
                        }
                    }

                    let mut bindings = Vec::new();

                    for &symbol in &symbols {
                        remaining_bindings.shift_remove(&symbol);
                        bindings.push(symbol);
                    }

                    binding_groups.push(bindings);
                }

                while let Some(stmt) = block.statements.last() {
                    match stmt {
                        Statement::Expr(_) => {
                            block.statements.pop().unwrap();
                        }
                        Statement::Let(_) => {
                            break;
                        }
                        Statement::If(_, _) => {
                            todo!()
                        }
                        Statement::Return(_) => {
                            todo!()
                        }
                        Statement::Assert(_) => {
                            todo!()
                        }
                        Statement::Raise(_) => {
                            todo!()
                        }
                    }
                }
            }

            let all_bindings: IndexSet<SymbolId> =
                binding_groups.iter().flatten().copied().collect();

            let mut block_context = context.clone();
            block_context.bindings.extend(all_bindings);

            let mut expr = lower_hir(db, arena, graph, &mut block_context, block.body.unwrap());

            context.captures.extend(block_context.captures);

            for (i, group) in binding_groups.iter().enumerate().rev() {
                let mut bindings = Vec::new();

                let mut bind_context = context.clone();

                for existing_group in binding_groups.iter().take(i) {
                    bind_context.bindings.extend(existing_group.iter().copied());
                }

                for &symbol in group {
                    bindings.push(lower_symbol(db, arena, graph, &mut bind_context, symbol));
                }

                context.captures.extend(bind_context.captures);

                expr = arena.alloc(Mir::Bind {
                    bindings,
                    body: expr,
                });
            }

            expr
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

fn lower_symbol(
    db: &Database,
    arena: &mut Arena<Mir>,
    graph: &DependencyGraph,
    context: &mut FunctionContext,
    symbol: SymbolId,
) -> MirId {
    let Symbol::Binding(binding) = db.symbol(symbol) else {
        unreachable!()
    };

    lower_hir(db, arena, graph, context, binding.value)
}
