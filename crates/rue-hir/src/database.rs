use std::collections::HashMap;

use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::SourceKind;
use rue_types::{Type, TypeId};

use crate::{Hir, HirId, Import, ImportId, ModuleSymbol, Scope, ScopeId, Symbol, SymbolId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Declaration {
    Symbol(SymbolId),
    Type(TypeId),
}

#[derive(Debug, Clone)]
pub struct Test {
    pub name: Option<String>,
    pub path: SourceKind,
    pub symbol: SymbolId,
}

#[derive(Debug, Default, Clone)]
pub struct Database {
    hir: Arena<Hir>,
    scopes: Arena<Scope>,
    imports: Arena<Import>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
    relevant_declarations: IndexSet<Declaration>,
    declared_by: HashMap<Declaration, IndexSet<Declaration>>,
    referenced_by: HashMap<Declaration, IndexSet<Declaration>>,
    relevant_imports: IndexSet<ImportId>,
    import_references: HashMap<ImportId, IndexSet<Declaration>>,
    tests: Vec<Test>,
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_hir(&mut self, hir: Hir) -> HirId {
        self.hir.alloc(hir)
    }

    pub fn hir(&self, id: HirId) -> &Hir {
        &self.hir[id]
    }

    pub fn hir_mut(&mut self, id: HirId) -> &mut Hir {
        &mut self.hir[id]
    }

    pub fn alloc_scope(&mut self, scope: Scope) -> ScopeId {
        self.scopes.alloc(scope)
    }

    pub fn scope(&self, id: ScopeId) -> &Scope {
        &self.scopes[id]
    }

    pub fn scope_mut(&mut self, id: ScopeId) -> &mut Scope {
        &mut self.scopes[id]
    }

    pub fn alloc_import(&mut self, import: Import) -> ImportId {
        self.imports.alloc(import)
    }

    pub fn import(&self, id: ImportId) -> &Import {
        &self.imports[id]
    }

    pub fn import_mut(&mut self, id: ImportId) -> &mut Import {
        &mut self.imports[id]
    }

    pub fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        self.symbols.alloc(symbol)
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id]
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id]
    }

    pub fn module(&self, id: SymbolId) -> &ModuleSymbol {
        match self.symbol(id) {
            Symbol::Module(module) => module,
            _ => unreachable!(),
        }
    }

    pub fn module_mut(&mut self, id: SymbolId) -> &mut ModuleSymbol {
        match self.symbol_mut(id) {
            Symbol::Module(module) => module,
            _ => unreachable!(),
        }
    }

    pub fn alloc_type(&mut self, ty: Type) -> TypeId {
        self.types.alloc(ty)
    }

    pub fn ty(&self, id: TypeId) -> &Type {
        &self.types[id]
    }

    pub fn ty_mut(&mut self, id: TypeId) -> &mut Type {
        &mut self.types[id]
    }

    pub fn types(&self) -> &Arena<Type> {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut Arena<Type> {
        &mut self.types
    }

    pub fn add_relevant_declaration(&mut self, declaration: Declaration) {
        self.relevant_declarations.insert(declaration);
    }

    pub fn relevant_declarations(&self) -> impl Iterator<Item = Declaration> {
        self.relevant_declarations.iter().copied()
    }

    pub fn add_reference(&mut self, outer: Declaration, inner: Declaration) {
        self.referenced_by.entry(inner).or_default().insert(outer);
    }

    pub fn add_declaration(&mut self, outer: Declaration, inner: Declaration) {
        self.declared_by.entry(inner).or_default().insert(outer);
    }

    pub fn reference_parents(&self, declaration: Declaration) -> Vec<Declaration> {
        self.referenced_by
            .get(&declaration)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect()
    }

    pub fn declaration_parents(&self, declaration: Declaration) -> Vec<Declaration> {
        self.declared_by
            .get(&declaration)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect()
    }

    pub fn add_relevant_import(&mut self, import: ImportId) {
        self.relevant_imports.insert(import);
    }

    pub fn relevant_imports(&self) -> impl Iterator<Item = ImportId> {
        self.relevant_imports.iter().copied()
    }

    pub fn add_import_reference(&mut self, import: ImportId, declaration: Declaration) {
        self.import_references
            .entry(import)
            .or_default()
            .insert(declaration);
    }

    pub fn import_references(&self, import: ImportId) -> Vec<Declaration> {
        self.import_references
            .get(&import)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect()
    }

    pub fn add_test(&mut self, test: Test) {
        self.tests.push(test);
    }

    pub fn tests(&self) -> impl Iterator<Item = &Test> {
        self.tests.iter()
    }

    pub fn debug_symbol(&self, id: SymbolId) -> String {
        let name = match self.symbol(id) {
            Symbol::Unresolved => None,
            Symbol::Module(module) => module.name.as_ref().map(|name| name.text().to_string()),
            Symbol::Function(function) => {
                function.name.as_ref().map(|name| name.text().to_string())
            }
            Symbol::Builtin(builtin) => Some(format!("{builtin:?}")),
            Symbol::Parameter(parameter) => {
                parameter.name.as_ref().map(|name| name.text().to_string())
            }
            Symbol::Constant(constant) => {
                constant.name.as_ref().map(|name| name.text().to_string())
            }
            Symbol::Binding(binding) => binding.name.as_ref().map(|name| name.text().to_string()),
        };

        if let Some(name) = name {
            format!("{}<{}>", name.replace('"', ""), id.index())
        } else {
            format!("<{}>", id.index())
        }
    }

    pub fn debug_hir(&self, id: HirId) -> String {
        match self.hir(id) {
            Hir::Unresolved => "{unknown}".to_string(),
            Hir::Nil => "nil".to_string(),
            Hir::String(value) => format!("\"{value}\""),
            Hir::Int(value) => format!("{value}"),
            Hir::Bytes(value) => {
                if value.is_empty() {
                    "nil".to_string()
                } else {
                    format!("0x{}", hex::encode(value))
                }
            }
            Hir::Bool(value) => format!("{value}"),
            Hir::Pair(first, rest) => {
                format!("({}, {})", self.debug_hir(*first), self.debug_hir(*rest))
            }
            Hir::Reference(symbol) => self.debug_symbol(*symbol),
            Hir::Block(block) => block
                .body
                .map_or("{empty}".to_string(), |body| self.debug_hir(body)),
            Hir::Lambda(lambda) => self.debug_symbol(*lambda),
            Hir::If(condition, then, else_, inline) => format!(
                "{}if {} {{ {} }} else {{ {} }}",
                if *inline { "inline " } else { "" },
                self.debug_hir(*condition),
                self.debug_hir(*then),
                self.debug_hir(*else_)
            ),
            Hir::FunctionCall(call) => format!(
                "{}({})",
                self.debug_hir(call.function),
                call.args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        let arg = self.debug_hir(*arg);

                        if i == call.args.len() - 1 && !call.nil_terminated {
                            format!("...{arg}")
                        } else {
                            arg
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Hir::Unary(op, hir) => format!("({op} {})", self.debug_hir(*hir)),
            Hir::Binary(op, left, right) => {
                format!(
                    "({} {} {})",
                    self.debug_hir(*left),
                    op,
                    self.debug_hir(*right)
                )
            }
            Hir::CoinId(parent, puzzle, amount) => {
                format!(
                    "coinid({}, {}, {})",
                    self.debug_hir(*parent),
                    self.debug_hir(*puzzle),
                    self.debug_hir(*amount)
                )
            }
            Hir::Substr(hir, start, end) => {
                if let Some(end) = end {
                    format!(
                        "substr({}, {}, {})",
                        self.debug_hir(*hir),
                        self.debug_hir(*start),
                        self.debug_hir(*end)
                    )
                } else {
                    format!(
                        "substr({}, {})",
                        self.debug_hir(*hir),
                        self.debug_hir(*start)
                    )
                }
            }
            Hir::G1Map(data, dst) => {
                if let Some(dst) = dst {
                    format!(
                        "g1_map({}, {})",
                        self.debug_hir(*data),
                        self.debug_hir(*dst)
                    )
                } else {
                    format!("g1_map({})", self.debug_hir(*data))
                }
            }
            Hir::G2Map(data, dst) => {
                if let Some(dst) = dst {
                    format!(
                        "g2_map({}, {})",
                        self.debug_hir(*data),
                        self.debug_hir(*dst)
                    )
                } else {
                    format!("g2_map({})", self.debug_hir(*data))
                }
            }
            Hir::Modpow(base, exponent, modulus) => {
                format!(
                    "modpow({}, {}, {})",
                    self.debug_hir(*base),
                    self.debug_hir(*exponent),
                    self.debug_hir(*modulus)
                )
            }
            Hir::BlsPairingIdentity(args) => {
                format!(
                    "bls_pairing_identity({})",
                    args.iter()
                        .map(|arg| self.debug_hir(*arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Hir::BlsVerify(sig, args) => {
                format!(
                    "bls_verify({}, {})",
                    self.debug_hir(*sig),
                    args.iter()
                        .map(|arg| self.debug_hir(*arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Hir::Secp256K1Verify(sig, pk, msg) => {
                format!(
                    "secp256k1_verify({}, {}, {})",
                    self.debug_hir(*sig),
                    self.debug_hir(*pk),
                    self.debug_hir(*msg)
                )
            }
            Hir::Secp256R1Verify(sig, pk, msg) => {
                format!(
                    "secp256r1_verify({}, {}, {})",
                    self.debug_hir(*sig),
                    self.debug_hir(*pk),
                    self.debug_hir(*msg)
                )
            }
            Hir::InfinityG1 => "INFINITY_G1".to_string(),
            Hir::InfinityG2 => "INFINITY_G2".to_string(),
            Hir::ClvmOp(op, args) => {
                format!("{:?}({})", op, self.debug_hir(*args))
            }
        }
    }
}
