use indexmap::{IndexMap, IndexSet};

use crate::{Context, Hir, HirId, Mir, MirId, Symbol, SymbolId};

#[derive(Debug, Clone)]
pub struct Graph {
    dependencies: IndexMap<SymbolId, IndexSet<SymbolId>>,
    stack: Vec<SymbolId>,
}

impl Default for Graph {
    fn default() -> Self {
        Self {
            dependencies: IndexMap::new(),
            stack: Vec::new(),
        }
    }
}

impl Graph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, symbol: SymbolId) {
        self.stack.push(symbol);
    }

    pub fn pop(&mut self) {
        self.stack.pop().unwrap();
    }

    pub fn reference(&mut self, symbol: SymbolId) {
        self.dependencies
            .entry(*self.stack.last().unwrap())
            .or_default()
            .insert(symbol);
    }

    pub fn extend(&mut self, symbol: SymbolId) {
        if let Some(last) = self.stack.last() {
            let children = self.dependencies.get(&symbol).cloned().unwrap_or_default();
            self.dependencies.entry(*last).or_default().extend(children);
        }
    }
}

pub fn graph_hir(ctx: &Context, graph: &mut Graph, hir: HirId) {
    match ctx.hir(hir) {
        Hir::Unresolved | Hir::Atom(_) => {}
        Hir::Reference(symbol) => {
            graph_symbol(ctx, graph, *symbol);
        }
        Hir::Unary(_, hir) => {
            graph_hir(ctx, graph, *hir);
        }
        Hir::Binary(_, left, right) => {
            graph_hir(ctx, graph, *left);
            graph_hir(ctx, graph, *right);
        }
        Hir::Block(block) => {
            if let Some(body) = &block.body {
                graph_hir(ctx, graph, body.hir);
            }
        }
    }
}

pub fn graph_symbol(ctx: &Context, graph: &mut Graph, symbol: SymbolId) {
    graph.extend(symbol);
    graph.push(symbol);

    match ctx.symbol(symbol).clone() {
        Symbol::Function(function) => {
            graph_hir(ctx, graph, function.body);
        }
        Symbol::Parameter(_) => {}
        Symbol::Binding(binding) => {
            graph_hir(ctx, graph, binding.value);
        }
    }

    graph.pop();
    graph.extend(symbol);
}

pub fn lower_hir(ctx: &mut Context, hir: HirId) -> MirId {
    match ctx.hir(hir).clone() {
        Hir::Unresolved => ctx.builtins().unresolved_mir,
        Hir::Atom(atom) => ctx.alloc_mir(Mir::Atom(atom)),
        Hir::Reference(symbol) => lower_reference(ctx, symbol),
        Hir::Unary(op, hir) => {
            let mir = lower_hir(ctx, hir);
            ctx.alloc_mir(Mir::Unary(op, mir))
        }
        Hir::Binary(op, left, right) => {
            let left = lower_hir(ctx, left);
            let right = lower_hir(ctx, right);
            ctx.alloc_mir(Mir::Binary(op, left, right))
        }
        Hir::Block(block) => {
            assert!(block.statements.is_empty());
            let hir = block.body.unwrap().hir;
            lower_hir(ctx, hir)
        }
    }
}

pub fn lower_reference(ctx: &mut Context, symbol: SymbolId) -> MirId {
    ctx.alloc_mir(Mir::Reference(symbol))
}
