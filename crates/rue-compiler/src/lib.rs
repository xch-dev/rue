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
use symbol::{Module, Symbol};
use ty::Type;

pub use database::*;
pub use error::*;

#[derive(Debug)]
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

pub fn analyze(root: &Root) -> Vec<Diagnostic> {
    let mut db = Database::default();
    precompile(&mut db, root);
    db.diagnostics().to_vec()
}

fn precompile(db: &mut Database, root: &Root) -> Option<LirId> {
    let mut compiler = Compiler::new(db);

    let (module_id, declarations) = compiler.declare_root(root);
    compiler.compile_root(root, module_id, declarations);

    let symbol_table = compiler.finish();

    let Symbol::Module(Module { scope_id, .. }) = db.symbol_mut(module_id).clone() else {
        unreachable!();
    };

    let Some(main_symbol_id) = db.scope_mut(scope_id).symbol("main") else {
        db.error(ErrorKind::MissingMain, TextRange::new(0.into(), 0.into()));
        return None;
    };

    let Symbol::Module(module) = db.symbol_mut(module_id) else {
        unreachable!();
    };

    module.exported_symbols.insert(main_symbol_id);
    let module_clone = module.clone();

    let graph = DependencyGraph::build(db, &module_clone);
    let unused = symbol_table.calculate_unused(db, &graph, scope_id, main_symbol_id);

    for symbol_id in &unused.symbol_ids {
        if unused.exempt_symbols.contains(symbol_id) {
            continue;
        }
        let token = db.symbol_token(*symbol_id).unwrap();
        let kind = match db.symbol(*symbol_id).clone() {
            Symbol::Unknown => unreachable!(),
            Symbol::Module(..) => WarningKind::UnusedModule(token.to_string()),
            Symbol::Function(..) => WarningKind::UnusedFunction(token.to_string()),
            Symbol::InlineFunction(..) => WarningKind::UnusedInlineFunction(token.to_string()),
            Symbol::Parameter(..) => WarningKind::UnusedParameter(token.to_string()),
            Symbol::Let(..) => WarningKind::UnusedLet(token.to_string()),
            Symbol::Const(..) => WarningKind::UnusedConst(token.to_string()),
            Symbol::InlineConst(..) => WarningKind::UnusedInlineConst(token.to_string()),
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

pub fn compile(allocator: &mut Allocator, root: &Root, parsing_succeeded: bool) -> Output {
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
