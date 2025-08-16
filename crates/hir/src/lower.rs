use id_arena::Arena;
use indexmap::IndexSet;
use rue_lir::{Lir, LirId};

use crate::{
    BinaryOp, BindingSymbol, Database, DependencyGraph, Environment, FunctionSymbol, Hir, HirId,
    Statement, Symbol, SymbolId, UnaryOp, bigint_atom,
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
        Hir::Block(block) => lower_block(db, arena, graph, env, block.statements, block.body),
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
                BinaryOp::Gt => arena.alloc(Lir::Gt(left, right)),
                BinaryOp::Lt => arena.alloc(Lir::Gt(right, left)),
                BinaryOp::Gte => {
                    let gt = arena.alloc(Lir::Gt(left, right));
                    let eq = arena.alloc(Lir::Eq(left, right));
                    arena.alloc(Lir::Any(vec![gt, eq]))
                }
                BinaryOp::Lte => {
                    let lt = arena.alloc(Lir::Gt(right, left));
                    let eq = arena.alloc(Lir::Eq(left, right));
                    arena.alloc(Lir::Any(vec![lt, eq]))
                }
                BinaryOp::Eq => arena.alloc(Lir::Eq(left, right)),
                BinaryOp::Ne => {
                    let eq = arena.alloc(Lir::Eq(left, right));
                    arena.alloc(Lir::Not(eq))
                }
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
    stmts: Vec<Statement>,
    body: Option<HirId>,
) -> LirId {
    let Some(stmt) = stmts.first() else {
        return if let Some(body) = body {
            lower_hir(db, arena, graph, env, body)
        } else {
            arena.alloc(Lir::Atom(vec![]))
        };
    };

    match stmt.clone() {
        Statement::Let(_) => lower_let_stmts(db, arena, graph, env, stmts, body),
        Statement::Return(hir) => lower_block(db, arena, graph, env, vec![], Some(hir)),
        Statement::Assert(condition) => lower_assert(db, arena, graph, env, stmts, condition, body),
        Statement::Expr(hir) => lower_block(db, arena, graph, env, stmts, body.or(Some(hir))),
        Statement::Raise(hir) => lower_raise(db, arena, graph, env, hir),
        Statement::If(condition, then) => {
            lower_if(db, arena, graph, env, stmts, condition, then, body)
        }
    }
}

fn lower_let_stmts(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    mut stmts: Vec<Statement>,
    body: Option<HirId>,
) -> LirId {
    let mut symbols = IndexSet::new();

    while let Some(stmt) = stmts.first()
        && let Statement::Let(symbol) = stmt
    {
        symbols.insert(*symbol);
        stmts.remove(0);
    }

    let binding_groups = group_symbols(symbols, graph);

    let all_bindings: Vec<SymbolId> = binding_groups.iter().rev().flatten().copied().collect();

    let body_env = env.with_bindings(&all_bindings);

    let mut expr = lower_block(db, arena, graph, &body_env, stmts, body);

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

fn lower_assert(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    mut stmts: Vec<Statement>,
    condition: HirId,
    body: Option<HirId>,
) -> LirId {
    stmts.remove(0);
    let condition = lower_hir(db, arena, graph, env, condition);
    let then_branch = lower_block(db, arena, graph, env, stmts, body);
    let else_branch = arena.alloc(Lir::Raise(vec![]));
    arena.alloc(Lir::If(condition, then_branch, else_branch))
}

fn lower_raise(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    hir: HirId,
) -> LirId {
    let lir = lower_hir(db, arena, graph, env, hir);
    arena.alloc(Lir::Raise(vec![lir]))
}

#[allow(clippy::too_many_arguments)]
fn lower_if(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    mut stmts: Vec<Statement>,
    condition: HirId,
    then: HirId,
    body: Option<HirId>,
) -> LirId {
    stmts.remove(0);
    let condition = lower_hir(db, arena, graph, env, condition);
    let then_branch = lower_hir(db, arena, graph, env, then);
    let else_branch = lower_block(db, arena, graph, env, stmts, body);
    arena.alloc(Lir::If(condition, then_branch, else_branch))
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
