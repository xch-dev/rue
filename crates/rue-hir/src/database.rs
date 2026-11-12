use std::collections::HashMap;

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet};
use rue_diagnostic::{SourceKind, SrcLoc};
use rue_types::{Type, TypeId};

use crate::{
    Hir, HirId, Import, ImportId, ImportUser, ModuleSymbol, Scope, ScopeId, Symbol, SymbolId, Value,
};

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
    const_exprs: Vec<HirId>,
    hir_types: HashMap<HirId, TypeId>,
    hir_srclocs: HashMap<HirId, SrcLoc>,
    hir_scopes: HashMap<HirId, ScopeId>,
    default_fields: HashMap<TypeId, IndexMap<String, Value>>,
    scopes: Arena<Scope>,
    imports: Arena<Import>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
    relevant_declarations: IndexSet<Declaration>,
    declared_by: HashMap<Declaration, IndexSet<Declaration>>,
    referenced_by: HashMap<Declaration, IndexSet<Declaration>>,
    relevant_imports: IndexSet<ImportId>,
    /// The declarations referenced through each import, along with where they were referenced.
    import_references: HashMap<ImportId, IndexMap<Declaration, IndexSet<ImportUser>>>,
    tests: Vec<Test>,
}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_hir(&mut self, hir: Hir) -> HirId {
        let is_const = matches!(&hir, Hir::Const(_));
        let hir = self.hir.alloc(hir);

        if is_const {
            self.const_exprs.push(hir);
        }

        hir
    }

    pub fn hir(&self, id: HirId) -> &Hir {
        &self.hir[id]
    }

    pub fn hir_mut(&mut self, id: HirId) -> &mut Hir {
        &mut self.hir[id]
    }

    pub fn const_exprs(&self) -> impl Iterator<Item = HirId> + '_ {
        self.const_exprs.iter().copied()
    }

    /// The value inside of any casts, which is what's evaluated at runtime.
    pub fn without_casts(&self, mut hir: HirId) -> HirId {
        while let Hir::Cast(value) = self.hir(hir) {
            hir = *value;
        }
        hir
    }

    /// Records the static type of a HIR node. Each expression compiles to a node of its own,
    /// except for a parenthesized expression, which compiles to the node of the expression inside
    /// of it. It records its type afterward, so the node ends up with the type of the outermost
    /// expression.
    pub fn set_hir_type(&mut self, hir: HirId, ty: TypeId) {
        self.hir_types.insert(hir, ty);
    }

    /// The static type of a HIR node, if it was produced by compiling an expression. Nodes that
    /// the compiler generates in other ways, such as the destructuring of a binding, don't have
    /// one.
    pub fn hir_type(&self, hir: HirId) -> Option<TypeId> {
        self.hir_types.get(&hir).copied()
    }

    /// Records the location of the source expression that a HIR node was compiled from. Like
    /// [`set_hir_type`](Self::set_hir_type), this is the outermost expression, so the location of
    /// a parenthesized expression includes the parentheses.
    pub fn set_hir_srcloc(&mut self, hir: HirId, srcloc: SrcLoc) {
        self.hir_srclocs.insert(hir, srcloc);
    }

    /// The location of the source expression that a HIR node was compiled from, which is only
    /// available for the same nodes as [`hir_type`](Self::hir_type).
    pub fn hir_srcloc(&self, hir: HirId) -> Option<&SrcLoc> {
        self.hir_srclocs.get(&hir)
    }

    /// Records the scope that the source expression of a HIR node is in, which is used to name
    /// types as they're written there.
    pub fn set_hir_scope(&mut self, hir: HirId, scope: ScopeId) {
        self.hir_scopes.insert(hir, scope);
    }

    /// The scope that the source expression of a HIR node is in, which is only available for the
    /// same nodes as [`hir_type`](Self::hir_type).
    pub fn hir_scope(&self, hir: HirId) -> Option<ScopeId> {
        self.hir_scopes.get(&hir).copied()
    }

    pub fn insert_default_field(&mut self, ty: TypeId, name: String, value: Value) {
        self.default_fields
            .entry(ty)
            .or_default()
            .insert(name, value);
    }

    pub fn default_field(&self, ty: TypeId, name: &str) -> Option<Value> {
        self.default_fields
            .get(&ty)
            .and_then(|fields| fields.get(name).cloned())
    }

    /// The default values of a struct's fields, in the order they were compiled.
    pub fn default_fields(&self, ty: TypeId) -> impl Iterator<Item = (&str, &Value)> {
        self.default_fields
            .get(&ty)
            .into_iter()
            .flatten()
            .map(|(name, value)| (name.as_str(), value))
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

    /// The symbol that a name refers to in a scope, and the import it comes from, if any.
    pub fn resolve_symbol_in(
        &self,
        scope: ScopeId,
        name: &str,
    ) -> Option<(SymbolId, Option<ImportId>)> {
        let mut current = Some(scope);

        while let Some(scope) = current {
            if let Some(symbol) = self.scope(scope).symbol(name) {
                return Some((symbol, self.scope(scope).symbol_import(symbol)));
            }
            current = self.scope(scope).parent();
        }

        None
    }

    /// The type that a name refers to in a scope, and the import it comes from, if any.
    pub fn resolve_type_in(
        &self,
        scope: ScopeId,
        name: &str,
    ) -> Option<(TypeId, Option<ImportId>)> {
        let mut current = Some(scope);

        while let Some(scope) = current {
            if let Some(ty) = self.scope(scope).ty(name) {
                return Some((ty, self.scope(scope).type_import(ty)));
            }
            current = self.scope(scope).parent();
        }

        None
    }

    /// The name of a type as seen from the given scope, which falls back to the structure of the
    /// type if it isn't named in that scope or any of its parents.
    pub fn type_name_in(&mut self, scope: ScopeId, ty: TypeId) -> String {
        let mut current = Some(scope);

        while let Some(scope) = current {
            if let Some(name) = self.scope(scope).type_name(ty) {
                return name.to_string();
            }
            current = self.scope(scope).parent();
        }

        rue_types::stringify(&mut self.types, ty)
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

    pub fn add_import_reference(
        &mut self,
        import: ImportId,
        declaration: Declaration,
        user: ImportUser,
    ) {
        self.import_references
            .entry(import)
            .or_default()
            .entry(declaration)
            .or_default()
            .insert(user);
    }

    /// The declarations referenced through the import.
    pub fn import_references(&self, import: ImportId) -> Vec<Declaration> {
        self.import_references
            .get(&import)
            .map(|references| references.keys().copied().collect())
            .unwrap_or_default()
    }

    /// The declarations referenced through the import, along with where they were referenced.
    pub fn import_uses(
        &self,
        import: ImportId,
    ) -> impl Iterator<Item = (Declaration, ImportUser)> + '_ {
        self.import_references
            .get(&import)
            .into_iter()
            .flatten()
            .flat_map(|(&declaration, users)| users.iter().map(move |&user| (declaration, user)))
    }

    pub fn add_test(&mut self, test: Test) {
        self.tests.push(test);
    }

    pub fn tests(&self) -> impl Iterator<Item = &Test> {
        self.tests.iter()
    }

    pub fn debug_symbol(&self, id: SymbolId) -> String {
        let name = self.symbol(id).name();

        if let Some(name) = name {
            format!("{}<{}>", name.text(), id.index())
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
            Hir::List(list) => format!(
                "[{}]",
                list.items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| {
                        let item = self.debug_hir(*item);

                        if i == list.items.len() - 1 && !list.nil_terminated {
                            format!("...{item}")
                        } else {
                            item
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Hir::Struct(initializer) => format!(
                "{{ {} }}",
                initializer
                    .fields
                    .iter()
                    .map(|field| format!("{}: {}", field.name, self.debug_hir(field.value)))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Hir::FieldAccess(access) => format!("{}.{}", self.debug_hir(access.value), access.name),
            Hir::Cast(value) => format!("({} as _)", self.debug_hir(*value)),
            Hir::Guard(guard) => format!("({} is _)", self.debug_hir(guard.value)),
            Hir::Const(expr) => format!("const {{ {} }}", self.debug_hir(expr.value)),
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
