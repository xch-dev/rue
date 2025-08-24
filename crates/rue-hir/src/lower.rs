use id_arena::Arena;
use indexmap::IndexSet;
use rue_lir::{Lir, LirId, bigint_atom};

use crate::{
    BinaryOp, BindingSymbol, Database, DependencyGraph, Environment, FunctionSymbol, Hir, HirId,
    Statement, Symbol, SymbolId, UnaryOp,
};

pub fn lower_symbol(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    symbol: SymbolId,
    is_main: bool,
) -> LirId {
    match db.symbol(symbol).clone() {
        Symbol::Function(function) => {
            lower_function(db, arena, graph, env, symbol, function, is_main)
        }
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
    is_main: bool,
) -> LirId {
    let captures: Vec<SymbolId> = graph.dependencies(symbol, true).into_iter().collect();
    let function_env = Environment::new(&captures, &function.parameters);

    let body = lower_hir(db, arena, graph, &function_env, function.body);

    if is_main {
        let mut definitions = Vec::new();

        for symbol in captures {
            let lir = lower_symbol(db, arena, graph, parent_env, symbol, false);
            definitions.push(arena.alloc(Lir::Quote(lir)));
        }

        let body = arena.alloc(Lir::Quote(body));

        arena.alloc(Lir::Curry(body, definitions))
    } else {
        body
    }
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
        Hir::Pair(first, rest) => {
            let first = lower_hir(db, arena, graph, env, first);
            let rest = lower_hir(db, arena, graph, env, rest);
            arena.alloc(Lir::Cons(first, rest))
        }
        Hir::Reference(symbol) => lower_reference(db, arena, graph, env, symbol),
        Hir::Block(block) => lower_block(db, arena, graph, env, block.statements, block.body),
        Hir::If(condition, then, else_) => {
            let condition = lower_hir(db, arena, graph, env, condition);
            let then = lower_hir(db, arena, graph, env, then);
            let else_ = lower_hir(db, arena, graph, env, else_);
            arena.alloc(Lir::If(condition, then, else_))
        }
        Hir::FunctionCall(function, args) => {
            let function = lower_hir(db, arena, graph, env, function);
            let args = args
                .iter()
                .map(|arg| lower_hir(db, arena, graph, env, *arg))
                .collect();
            arena.alloc(Lir::Run(function, args))
        }
        Hir::Unary(op, hir) => {
            let lir = lower_hir(db, arena, graph, env, hir);
            match op {
                UnaryOp::Listp => arena.alloc(Lir::Listp(lir)),
                UnaryOp::First => arena.alloc(Lir::First(lir)),
                UnaryOp::Rest => arena.alloc(Lir::Rest(lir)),
                UnaryOp::Strlen => arena.alloc(Lir::Strlen(lir)),
                UnaryOp::Not => arena.alloc(Lir::Not(lir)),
                UnaryOp::Neg => {
                    let zero = arena.alloc(Lir::Atom(vec![]));
                    arena.alloc(Lir::Sub(vec![zero, lir]))
                }
                UnaryOp::BitwiseNot => arena.alloc(Lir::Lognot(lir)),
                UnaryOp::Sha256 => arena.alloc(Lir::Sha256(vec![lir])),
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
                BinaryOp::Mod => arena.alloc(Lir::Mod(left, right)),
                BinaryOp::Concat => arena.alloc(Lir::Concat(vec![left, right])),
                BinaryOp::BitwiseAnd => arena.alloc(Lir::Logand(vec![left, right])),
                BinaryOp::BitwiseOr => arena.alloc(Lir::Logior(vec![left, right])),
                BinaryOp::BitwiseXor => arena.alloc(Lir::Logxor(vec![left, right])),
                BinaryOp::LeftShift => {
                    let zero = arena.alloc(Lir::Atom(vec![]));
                    let neg = arena.alloc(Lir::Sub(vec![zero, right]));
                    arena.alloc(Lir::Ash(left, neg))
                }
                BinaryOp::RightShift => arena.alloc(Lir::Ash(left, right)),
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
                BinaryOp::GtBytes => arena.alloc(Lir::GtBytes(left, right)),
                BinaryOp::LtBytes => arena.alloc(Lir::GtBytes(right, left)),
                BinaryOp::GteBytes => {
                    let gt = arena.alloc(Lir::GtBytes(left, right));
                    let eq = arena.alloc(Lir::Eq(left, right));
                    arena.alloc(Lir::Any(vec![gt, eq]))
                }
                BinaryOp::LteBytes => {
                    let lt = arena.alloc(Lir::GtBytes(right, left));
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
                BinaryOp::All => arena.alloc(Lir::All(vec![left, right])),
                BinaryOp::Any => arena.alloc(Lir::Any(vec![left, right])),
            }
        }
    }
}

fn lower_reference(
    db: &Database,
    arena: &mut Arena<Lir>,
    graph: &DependencyGraph,
    env: &Environment,
    symbol: SymbolId,
) -> LirId {
    if matches!(db.symbol(symbol).clone(), Symbol::Function(_)) {
        let captures: Vec<SymbolId> = graph.dependencies(symbol, true).into_iter().collect();

        let mut refs = Vec::new();

        for capture in captures {
            refs.push(arena.alloc(Lir::Path(env.path(capture))));
        }

        let reference = arena.alloc(Lir::Path(env.path(symbol)));

        return arena.alloc(Lir::Closure(reference, refs));
    }

    arena.alloc(Lir::Path(env.path(symbol)))
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
            bindings.push(lower_symbol(db, arena, graph, &bind_env, symbol, false));
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
                .dependencies(symbol, true)
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
