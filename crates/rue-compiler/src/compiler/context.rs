use std::collections::HashSet;

use clvmr::{Allocator, NodePtr};
use indexmap::IndexMap;
use rue_parser::{parse, Root};

use crate::{
    codegen::Codegen,
    dependency_graph::DependencyGraph,
    lowerer::Lowerer,
    optimizer::Optimizer,
    scope::Scope,
    symbol::{Module, Symbol},
    value::Type,
    Database, SymbolId,
};

use super::{builtins::builtins, item::Declarations, symbol_table::SymbolTable, Compiler};

pub struct CompilerContext<'a> {
    compiler: Compiler<'a>,
    roots: IndexMap<SymbolId, (Root, Declarations)>,
}

pub fn setup_compiler(db: &mut Database) -> CompilerContext<'_> {
    let builtins = builtins(db);
    let compiler = Compiler::new(db, builtins);
    CompilerContext {
        compiler,
        roots: IndexMap::new(),
    }
}

pub fn load_standard_library(ctx: &mut CompilerContext<'_>) -> SymbolId {
    let (root, parser_errors) = parse(include_str!("../../../../std/stdlib.rue"));
    assert_eq!(parser_errors, Vec::new());

    let (module_id, declarations) = ctx.compiler.declare_root(&root);
    ctx.compiler.compile_root(&root, module_id, declarations);

    let Symbol::Module(module) = ctx.compiler.db.symbol_mut(module_id).clone() else {
        unreachable!();
    };

    let mut scope = Scope::default();

    for &symbol_id in &module.exported_symbols {
        scope.define_symbol(
            ctx.compiler
                .db
                .scope(module.scope_id)
                .symbol_name(symbol_id)
                .unwrap()
                .to_string(),
            symbol_id,
        );
    }

    for &type_id in &module.exported_types {
        scope.define_type(
            ctx.compiler
                .db
                .scope(module.scope_id)
                .type_name(type_id)
                .unwrap()
                .to_string(),
            type_id,
        );
    }

    let scope_id = ctx.compiler.db.alloc_scope(scope);
    ctx.compiler.scope_stack.push(scope_id);
    module_id
}

pub fn load_module(ctx: &mut CompilerContext<'_>, root: &Root) -> SymbolId {
    let (module_id, declarations) = ctx.compiler.declare_root(root);
    ctx.roots.insert(module_id, (root.clone(), declarations));
    module_id
}

pub fn compile_modules(mut ctx: CompilerContext<'_>) -> SymbolTable {
    for (module_id, (root, declarations)) in ctx.roots {
        ctx.compiler.compile_root(&root, module_id, declarations);
    }
    ctx.compiler.finish()
}

pub fn build_graph(
    db: &mut Database,
    symbol_table: &SymbolTable,
    main_module_id: SymbolId,
    library_module_ids: &[SymbolId],
) -> DependencyGraph {
    let mut ignored_symbols = HashSet::new();
    let mut ignored_types = HashSet::new();

    for &module_id in library_module_ids {
        let Symbol::Module(module) = db.symbol_mut(module_id).clone() else {
            unreachable!();
        };
        ignored_symbols.extend(module.exported_symbols.iter().copied());
        ignored_types.extend(module.exported_types.iter().copied());
    }

    for type_id in ignored_types.clone() {
        let Type::Enum(enum_type) = db.ty(type_id) else {
            continue;
        };
        ignored_types.extend(enum_type.variants.values());
    }

    for symbol_id in ignored_symbols.clone() {
        let Symbol::Function(function) = db.symbol_mut(symbol_id).clone() else {
            continue;
        };
        ignored_types.extend(function.ty.generic_types.iter().copied());
    }

    let Symbol::Module(module) = db.symbol_mut(main_module_id).clone() else {
        unreachable!();
    };

    let graph = DependencyGraph::build(db, &module);
    symbol_table.calculate_unused(db, &graph, &ignored_symbols, &ignored_types);
    graph
}

pub fn codegen(
    allocator: &mut Allocator,
    db: &mut Database,
    graph: &DependencyGraph,
    entrypoint: SymbolId,
) -> NodePtr {
    let mut lowerer = Lowerer::new(db, graph);
    let (env_id, mir_id) = lowerer.lower_main(entrypoint);
    let mut optimizer = Optimizer::new(db);
    let lir_id = optimizer.opt_mir(env_id, mir_id);
    let mut codegen = Codegen::new(db, allocator);
    codegen.gen_lir(lir_id)
}

pub fn try_export_main(db: &mut Database, main_module_id: SymbolId) -> Option<SymbolId> {
    let Symbol::Module(Module { scope_id, .. }) = db.symbol_mut(main_module_id).clone() else {
        unreachable!();
    };

    if let Some(main_symbol_id) = db.scope_mut(scope_id).symbol("main") {
        let Symbol::Module(module) = db.symbol_mut(main_module_id) else {
            unreachable!();
        };
        module.exported_symbols.insert(main_symbol_id);
        Some(main_symbol_id)
    } else {
        None
    }
}
