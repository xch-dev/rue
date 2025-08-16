use id_arena::Arena;
use indexmap::IndexSet;
use rue_lir::{Lir, LirId};

use crate::{
    BinaryOp, BindingSymbol, Block, Database, DependencyGraph, Environment, FunctionSymbol, Hir,
    HirId, Statement, Symbol, SymbolId, UnaryOp, bigint_atom,
};

pub fn lower_symbol(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    symbol: SymbolId,
) -> LirId {
    match db.symbol(symbol).clone() {
        Symbol::Function(function) => lower_function(db, arena, graph, env, symbol, function),
        Symbol::Parameter(_) => unreachable!(),
        Symbol::Binding(binding) => lower_binding(db, arena, graph, env, binding),
    }
}

fn lower_function(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    parent_env: &Environment,
    symbol: SymbolId,
    function: FunctionSymbol,
) -> LirId {
    let captures: Vec<SymbolId> = graph.dependencies(symbol, true).into_iter().collect();
    let function_env = Environment::new(&captures, &function.parameters);

    let body = lower_hir(db, arena, graph, &function_env, function.body);

    let mut definitions = Vec::new();

    for symbol in captures {
        let lir = lower_symbol(db, arena, graph, parent_env, symbol);
        definitions.push(lir);
    }

    let body = arena.alloc(Lir::Quote(body));

    arena.alloc(Lir::Curry(body, definitions))
}

fn lower_binding(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    binding: BindingSymbol,
) -> LirId {
    lower_hir(db, arena, graph, env, binding.value)
}

fn lower_hir(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    hir: HirId,
) -> LirId {
    match db.hir(hir).clone() {
        Hir::Unresolved => unreachable!(),
        Hir::Nil => arena.alloc(Lir::Atom(vec![])),
        Hir::String(value) => arena.alloc(Lir::Atom(value.as_bytes().to_vec())),
        Hir::Int(value) => arena.alloc(Lir::Atom(bigint_atom(value.clone()))),
        Hir::Bool(value) => arena.alloc(Lir::Atom(if value { vec![1] } else { vec![] })),
        Hir::Bytes(atom) => arena.alloc(Lir::Atom(atom)),
        Hir::Reference(symbol) => arena.alloc(Lir::Path(env.path(symbol))),
        Hir::Block(block) => lower_block(db, arena, graph, env, block),
        Hir::Unary(op, hir) => {
            let lir = lower_hir(db, arena, graph, env, hir);
            match op {
                UnaryOp::Listp => arena.alloc(Lir::Listp(lir)),
                UnaryOp::First => arena.alloc(Lir::First(lir)),
                UnaryOp::Rest => arena.alloc(Lir::Rest(lir)),
                UnaryOp::Strlen => arena.alloc(Lir::Strlen(lir)),
                UnaryOp::Not => arena.alloc(Lir::Not(lir)),
            }
        }
        Hir::Binary(op, left, right) => {
            let left = lower_hir(db, arena, graph, env, left);
            let right = lower_hir(db, arena, graph, env, right);
            match op {
                BinaryOp::Add => arena.alloc(Lir::Add(vec![left, right])),
                BinaryOp::Sub => arena.alloc(Lir::Sub(vec![left, right])),
                BinaryOp::Mul => arena.alloc(Lir::Mul(vec![left, right])),
                BinaryOp::Div => arena.alloc(Lir::Div(left, right)),
                BinaryOp::Eq => arena.alloc(Lir::Eq(left, right)),
                BinaryOp::And => {
                    let true_atom = arena.alloc(Lir::Atom(vec![1]));
                    let false_atom = arena.alloc(Lir::Atom(vec![]));
                    let right = arena.alloc(Lir::If(right, true_atom, false_atom));
                    arena.alloc(Lir::If(left, right, false_atom))
                }
                BinaryOp::Or => {
                    let true_atom = arena.alloc(Lir::Atom(vec![1]));
                    let false_atom = arena.alloc(Lir::Atom(vec![]));
                    let right = arena.alloc(Lir::If(right, true_atom, false_atom));
                    arena.alloc(Lir::If(left, true_atom, right))
                }
            }
        }
    }
}

fn lower_block(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    mut block: Block,
) -> LirId {
    // TODO: Implement statement only blocks

    let mut binding_groups = Vec::new();

    while !block.statements.is_empty() {
        let mut symbols = IndexSet::new();

        while let Some(stmt) = block.statements.last()
            && let Statement::Let(symbol) = stmt
        {
            symbols.insert(*symbol);
            block.statements.pop();
        }

        binding_groups.extend(group_symbols(symbols, graph));

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

    let all_bindings: Vec<SymbolId> = binding_groups.iter().rev().flatten().copied().collect();

    let body_env = env.with_bindings(&all_bindings);

    let mut expr = lower_hir(db, arena, graph, &body_env, block.body.unwrap());

    for (i, group) in binding_groups.iter().enumerate().rev() {
        expr = arena.alloc(Lir::Quote(expr));

        let mut bind_env = env.clone();

        for existing_group in binding_groups.iter().take(i) {
            bind_env = bind_env.with_bindings(existing_group);
        }

        let mut bindings = Vec::new();

        for &symbol in group {
            bindings.push(lower_symbol(db, arena, graph, &bind_env, symbol));
        }

        expr = arena.alloc(Lir::Curry(expr, bindings));
    }

    expr
}

fn group_symbols(mut symbols: IndexSet<SymbolId>, graph: &DependencyGraph) -> Vec<Vec<SymbolId>> {
    let mut groups = Vec::new();

    while !symbols.is_empty() {
        let mut group = Vec::new();

        let remaining = symbols.clone();

        symbols.retain(|&symbol| {
            if graph
                .dependencies(symbol, false)
                .iter()
                .all(|symbol| !remaining.contains(symbol))
            {
                group.push(symbol);
                false
            } else {
                true
            }
        });

        groups.push(group);
    }

    groups
}
