use id_arena::Arena;

use crate::{Hir, HirId, Scope, ScopeId, Symbol, SymbolId, Type, TypeId};

#[derive(Debug, Default, Clone)]
pub struct Database {
    hir: Arena<Hir>,
    scopes: Arena<Scope>,
    symbols: Arena<Symbol>,
    types: Arena<Type>,
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

    pub fn alloc_symbol(&mut self, symbol: Symbol) -> SymbolId {
        self.symbols.alloc(symbol)
    }

    pub fn symbol(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id]
    }

    pub fn symbol_mut(&mut self, id: SymbolId) -> &mut Symbol {
        &mut self.symbols[id]
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

    pub fn debug_symbol(&self, id: SymbolId) -> String {
        let name = match self.symbol(id) {
            Symbol::Binding(binding) => binding.name.as_ref().map(|name| name.text().to_string()),
            Symbol::Function(function) => {
                function.name.as_ref().map(|name| name.text().to_string())
            }
            Symbol::Parameter(parameter) => {
                parameter.name.as_ref().map(|name| name.text().to_string())
            }
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
            Hir::If(condition, then, else_) => format!(
                "if {} {{ {} }} else {{ {} }}",
                self.debug_hir(*condition),
                self.debug_hir(*then),
                self.debug_hir(*else_)
            ),
            Hir::FunctionCall(function, args) => format!(
                "{}({})",
                self.debug_hir(*function),
                args.iter()
                    .map(|arg| self.debug_hir(*arg))
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
        }
    }

    pub fn debug_type(&self, ty: TypeId) -> String {
        match self.ty(ty) {
            Type::Unresolved => "{unknown}".to_string(),
            Type::Atom(atom) => atom.to_string(),
            Type::Pair(first, rest) => {
                let first = self.debug_type(*first);
                let rest = self.debug_type(*rest);
                format!("({first}, {rest})")
            }
            Type::Generic(generic) => generic
                .name
                .as_ref()
                .map_or("{generic}".to_string(), |name| name.text().to_string()),
            Type::Alias(alias) => {
                if let Some(name) = alias.name.as_ref() {
                    name.text().to_string()
                } else {
                    self.debug_type(alias.inner)
                }
            }
            Type::Union(types) => {
                let types = types
                    .iter()
                    .map(|ty| self.debug_type(*ty))
                    .collect::<Vec<_>>();
                types.join(" | ")
            }
            Type::Fn(function) => {
                let params = function
                    .params
                    .iter()
                    .map(|ty| self.debug_type(*ty))
                    .collect::<Vec<_>>();
                let ret = self.debug_type(function.ret);
                format!("fn({}) -> {}", params.join(", "), ret)
            }
        }
    }
}
