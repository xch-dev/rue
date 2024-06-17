use clvmr::{Allocator, NodePtr};
use codegen::Codegen;
use compiler::Compiler;
use dependency_graph::GraphTraversal;
use optimizer::Optimizer;
use rue_parser::Root;

mod codegen;
mod compiler;
mod database;
mod dependency_graph;
mod error;
mod hir;
mod lir;
mod optimizer;
mod scope;
mod symbol;
mod ty;

pub use database::*;
pub use error::*;

use scope::Scope;
use symbol::Symbol;
use ty::Type;

pub struct Output {
    diagnostics: Vec<Diagnostic>,
    node_ptr: NodePtr,
}

impl Output {
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn node_ptr(&self) -> NodePtr {
        self.node_ptr
    }
}

pub fn analyze(root: Root) -> Vec<Diagnostic> {
    let mut database = Database::default();
    precompile(&mut database, root).0
}

fn precompile(db: &mut Database, root: Root) -> (Vec<Diagnostic>, Option<LirId>) {
    let root_scope_id = db.alloc_scope(Scope::default());

    let mut compiler = Compiler::new(db);

    let declarations = compiler.declare_root(root.clone(), root_scope_id);
    compiler.compile_root(root, root_scope_id, declarations);

    let (symbol_table, mut diagnostics) = compiler.finish();

    let Some(main_symbol_id) = db.scope_mut(root_scope_id).symbol("main") else {
        diagnostics.push(Diagnostic::new(
            DiagnosticKind::Error(ErrorKind::MissingMain),
            0..0,
        ));

        return (diagnostics, None);
    };

    let traversal = GraphTraversal::new(db);
    let dependency_graph = traversal.build_graph(&[main_symbol_id]);

    let unused =
        symbol_table.calculate_unused(db, &dependency_graph, root_scope_id, main_symbol_id);

    for &symbol_id in &unused.symbol_ids {
        let token = symbol_table.symbol_token(symbol_id).unwrap();
        let kind = match db.symbol(symbol_id).clone() {
            Symbol::Unknown => unreachable!(),
            Symbol::Function { .. } => WarningKind::UnusedFunction(token.to_string()),
            Symbol::Parameter { .. } => WarningKind::UnusedParameter(token.to_string()),
            Symbol::LetBinding { .. } => WarningKind::UnusedLet(token.to_string()),
            Symbol::ConstBinding { .. } => WarningKind::UnusedConst(token.to_string()),
        };
        let range = token.text_range();
        diagnostics.push(Diagnostic::new(
            DiagnosticKind::Warning(kind),
            range.start().into()..range.end().into(),
        ));
    }

    for &type_id in &unused.type_ids {
        let token = symbol_table.type_token(type_id).unwrap();
        let kind = match db.ty_raw(type_id) {
            Type::Alias(..) => WarningKind::UnusedTypeAlias(token.to_string()),
            Type::Struct { .. } => WarningKind::UnusedStruct(token.to_string()),
            Type::Enum { .. } => WarningKind::UnusedEnum(token.to_string()),
            Type::EnumVariant { .. } => WarningKind::UnusedEnumVariant(token.to_string()),
            _ => continue,
        };
        let range = token.text_range();
        diagnostics.push(Diagnostic::new(
            DiagnosticKind::Warning(kind),
            range.start().into()..range.end().into(),
        ));
    }

    let mut optimizer = Optimizer::new(db, dependency_graph);
    let lir_id = optimizer.opt_main(main_symbol_id);

    (diagnostics, Some(lir_id))
}

pub fn compile(allocator: &mut Allocator, root: Root, parsing_succeeded: bool) -> Output {
    let mut database = Database::default();
    let (diagnostics, lir_id) = precompile(&mut database, root);

    let node_ptr = if !diagnostics.iter().any(Diagnostic::is_error) && parsing_succeeded {
        let mut codegen = Codegen::new(&mut database, allocator);
        codegen.gen_lir(lir_id.unwrap())
    } else {
        NodePtr::NIL
    };

    Output {
        diagnostics,
        node_ptr,
    }
}
