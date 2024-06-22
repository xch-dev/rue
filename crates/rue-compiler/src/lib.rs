mod codegen;
mod compiler;
mod database;
mod error;
mod hir;
mod lir;
mod optimizer;
mod scope;
mod symbol;
mod ty;

use clvmr::{Allocator, NodePtr};
use codegen::Codegen;
use compiler::Compiler;
use optimizer::{DependencyGraph, Optimizer};
use rowan::TextRange;
use rue_parser::Root;
use scope::Scope;
use symbol::Symbol;
use ty::Type;

pub use database::*;
pub use error::*;

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
    let mut db = Database::default();
    precompile(&mut db, root);
    db.diagnostics().to_vec()
}

fn precompile(db: &mut Database, root: Root) -> Option<LirId> {
    let root_scope_id = db.alloc_scope(Scope::default());

    let mut compiler = Compiler::new(db);

    let declarations = compiler.declare_root(root.clone(), root_scope_id);
    compiler.compile_root(root, root_scope_id, declarations);

    let symbol_table = compiler.finish();

    let Some(main_symbol_id) = db.scope_mut(root_scope_id).symbol("main") else {
        db.error(ErrorKind::MissingMain, TextRange::new(0.into(), 0.into()));
        return None;
    };

    let graph = DependencyGraph::build(db, &[main_symbol_id]);
    let unused = symbol_table.calculate_unused(db, &graph, root_scope_id, main_symbol_id);

    for symbol_id in &unused.symbol_ids {
        if unused.exempt_symbols.contains(symbol_id) {
            continue;
        }
        let token = db.symbol_token(*symbol_id).unwrap();
        let kind = match db.symbol(*symbol_id).clone() {
            Symbol::Unknown => unreachable!(),
            Symbol::Function(..) => WarningKind::UnusedFunction(token.to_string()),
            Symbol::Parameter(..) => WarningKind::UnusedParameter(token.to_string()),
            Symbol::Let(..) => WarningKind::UnusedLet(token.to_string()),
            Symbol::Const(..) => WarningKind::UnusedConst(token.to_string()),
        };
        db.warning(kind, token.text_range());
    }

    for type_id in &unused.type_ids {
        if unused.exempt_types.contains(type_id) {
            continue;
        }
        let token = db.type_token(*type_id).unwrap();
        let kind = match db.ty_raw(*type_id) {
            Type::Alias(..) => WarningKind::UnusedTypeAlias(token.to_string()),
            Type::Struct(..) => WarningKind::UnusedStruct(token.to_string()),
            Type::Enum(..) => WarningKind::UnusedEnum(token.to_string()),
            Type::EnumVariant(..) => WarningKind::UnusedEnumVariant(token.to_string()),
            _ => continue,
        };
        db.warning(kind, token.text_range());
    }

    let mut optimizer = Optimizer::new(db, graph);
    Some(optimizer.opt_main(main_symbol_id))
}

pub fn compile(allocator: &mut Allocator, root: Root, parsing_succeeded: bool) -> Output {
    let mut db = Database::default();
    let lir_id = precompile(&mut db, root);
    let diagnostics = db.diagnostics().to_vec();

    let node_ptr = if !diagnostics.iter().any(Diagnostic::is_error) && parsing_succeeded {
        let mut codegen = Codegen::new(&mut db, allocator);
        codegen.gen_lir(lir_id.unwrap())
    } else {
        NodePtr::NIL
    };

    Output {
        diagnostics,
        node_ptr,
    }
}
