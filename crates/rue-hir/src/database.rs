use std::collections::HashMap;

use id_arena::{Arena, ArenaBehavior, DefaultArenaBehavior};
use indexmap::{IndexMap, IndexSet};
use rue_diagnostic::{Name, SourceKind, SrcLoc};
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
    /// Types that nodes are reported as having in place of the ones they were recorded with (see
    /// `Database::report_type_as`).
    reported_types: HashMap<TypeId, TypeId>,
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
    /// The bindings each glob import shares with the glob import that bound the name, as that
    /// import, the declaration it bound, and the declaration this one brings in under the name.
    shared_glob_bindings: HashMap<ImportId, IndexSet<(ImportId, Declaration, Declaration)>>,
    /// The other declarations that a name could refer to, because glob imports bring in different
    /// declarations with it, keyed by the import that the name refers to a declaration through and
    /// that declaration.
    ambiguous_alternatives: IndexMap<(ImportId, Declaration), IndexSet<Declaration>>,
    /// The declarations that use each declaration through a path that names a module that glob
    /// imports bring in different modules with, where the path could refer to it in one of the
    /// other modules. They aren't references, since the path is only compiled as referring to
    /// the one in the module the name is bound to.
    ambiguous_uses: IndexMap<Declaration, IndexSet<Declaration>>,
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

    /// The id that the next node to be allocated will have.
    pub fn next_hir_id(&self) -> HirId {
        self.hir.next_id()
    }

    /// The nodes allocated from `start` up to `end`, both of which are from `next_hir_id`.
    pub fn hirs_between(&self, start: HirId, end: HirId) -> impl Iterator<Item = &Hir> {
        let arena = DefaultArenaBehavior::<Hir>::arena_id(start);

        (start.index()..end.index())
            .map(move |index| &self.hir[DefaultArenaBehavior::<Hir>::new_id(arena, index)])
    }

    /// How many symbols have been allocated, which is the index of the next one.
    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
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
        let ty = *self.hir_types.get(&hir)?;
        Some(self.reported_types.get(&ty).copied().unwrap_or(ty))
    }

    /// Reports the nodes recorded with the type as having another one instead, which it's the same
    /// type as, such as an untyped lambda parameter that's compiled as a reference of its own to its
    /// type (see `compile_lambda` in `rue-compiler`), so that it's the same type to lints too.
    pub fn report_type_as(&mut self, ty: TypeId, reported: TypeId) {
        self.reported_types.insert(ty, reported);
    }

    /// Whether nodes recorded with the type are reported as having another one (see
    /// `Database::report_type_as`).
    pub fn is_reported_as_another_type(&self, ty: TypeId) -> bool {
        self.reported_types.contains_key(&ty)
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

    /// The name that a declaration is declared with, if it's an item with a name that's written in
    /// the source (rather than a builtin, a parameter, or a type with no name).
    pub fn declaration_name(&self, declaration: Declaration) -> Option<Name> {
        match declaration {
            Declaration::Symbol(symbol) => match self.symbol(symbol) {
                Symbol::Function(_)
                | Symbol::Constant(_)
                | Symbol::Binding(_)
                | Symbol::Module(_) => self.symbol(symbol).name(),
                Symbol::Unresolved | Symbol::Builtin(_) | Symbol::Parameter(_) => None,
            },
            Declaration::Type(ty) => match self.ty(ty) {
                Type::Struct(ty) => ty.name.clone(),
                Type::Alias(ty) => ty.name.clone(),
                Type::Generic(ty) => ty.name.clone(),
                Type::Unresolved
                | Type::Any
                | Type::Never
                | Type::Ref(_)
                | Type::Atom(_)
                | Type::Pair(_)
                | Type::Function(_)
                | Type::Apply(_)
                | Type::Union(_) => None,
            },
        }
    }

    pub fn ty_mut(&mut self, id: TypeId) -> &mut Type {
        &mut self.types[id]
    }

    pub fn types(&self) -> &Arena<Type> {
        &self.types
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

    /// Where a declaration was referenced through the import itself, not including the references
    /// through a binding it shares.
    pub fn import_users(
        &self,
        import: ImportId,
        declaration: Declaration,
    ) -> impl Iterator<Item = ImportUser> + '_ {
        self.import_references
            .get(&import)
            .and_then(|references| references.get(&declaration))
            .into_iter()
            .flatten()
            .copied()
    }

    /// Records that a glob import brings a declaration in under a name that another glob import
    /// bound, so that a reference through the binding is a reference through both of them,
    /// whichever bound the name. Returns whether it wasn't already recorded.
    pub fn add_shared_glob_binding(
        &mut self,
        bound_by: ImportId,
        bound: Declaration,
        import: ImportId,
        declaration: Declaration,
    ) -> bool {
        self.shared_glob_bindings
            .entry(import)
            .or_default()
            .insert((bound_by, bound, declaration))
    }

    /// The declarations referenced through the import, including through a binding it shares.
    pub fn import_references(&self, import: ImportId) -> Vec<Declaration> {
        self.import_uses(import)
            .map(|(declaration, _)| declaration)
            .collect::<IndexSet<_>>()
            .into_iter()
            .collect()
    }

    /// The declarations referenced through the import, along with where they were referenced,
    /// including the references through a binding it shares, as references to the declaration
    /// that it brings in under the name.
    pub fn import_uses(
        &self,
        import: ImportId,
    ) -> impl Iterator<Item = (Declaration, ImportUser)> + '_ {
        let own = self
            .import_references
            .get(&import)
            .into_iter()
            .flatten()
            .flat_map(|(&declaration, users)| users.iter().map(move |&user| (declaration, user)));

        let shared = self
            .shared_glob_bindings
            .get(&import)
            .into_iter()
            .flatten()
            .flat_map(|&(bound_by, bound, declaration)| {
                self.import_users(bound_by, bound)
                    .map(move |user| (declaration, user))
            });

        own.chain(shared)
    }

    /// Records that a name that refers to the declaration through the import could also refer to
    /// what a name that refers to another declaration through another import could: that
    /// declaration, and its own alternatives. This is because glob imports bring in different
    /// declarations with the name, here or where it's imported from. Returns whether any of them
    /// weren't already recorded.
    pub fn add_ambiguous_alternatives(
        &mut self,
        import: ImportId,
        declaration: Declaration,
        from: Option<ImportId>,
        from_declaration: Declaration,
    ) -> bool {
        let others = std::iter::once(from_declaration)
            .chain(self.ambiguous_alternatives(from, from_declaration))
            .filter(|&other| other != declaration)
            .collect::<Vec<_>>();

        let mut added = false;

        for other in others {
            added |= self
                .ambiguous_alternatives
                .entry((import, declaration))
                .or_default()
                .insert(other);
        }

        added
    }

    /// The other declarations that a name could refer to, when it refers to the declaration
    /// through the import.
    pub fn ambiguous_alternatives(
        &self,
        import: Option<ImportId>,
        declaration: Declaration,
    ) -> impl Iterator<Item = Declaration> + '_ {
        import
            .and_then(|import| self.ambiguous_alternatives.get(&(import, declaration)))
            .into_iter()
            .flatten()
            .copied()
    }

    /// Every name that could refer to other declarations, as the import it refers to a
    /// declaration through and that declaration, along with the other declarations.
    pub fn ambiguous_bindings(
        &self,
    ) -> impl Iterator<Item = (ImportId, Declaration, &IndexSet<Declaration>)> {
        self.ambiguous_alternatives
            .iter()
            .map(|(&(import, declaration), others)| (import, declaration, others))
    }

    /// Records that a declaration uses another one through a path that could refer to it, since
    /// a module the path names is ambiguous (see [`Database::ambiguous_uses`]).
    pub fn add_ambiguous_use(&mut self, user: Declaration, declaration: Declaration) {
        self.ambiguous_uses
            .entry(declaration)
            .or_default()
            .insert(user);
    }

    /// Each declaration that a path could refer to in a module other than the one it's compiled
    /// as referring to, because glob imports bring in different modules with a name in the path,
    /// along with the declarations that use it through such a path.
    pub fn ambiguous_uses(&self) -> impl Iterator<Item = (Declaration, &IndexSet<Declaration>)> {
        self.ambiguous_uses
            .iter()
            .map(|(&declaration, users)| (declaration, users))
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
