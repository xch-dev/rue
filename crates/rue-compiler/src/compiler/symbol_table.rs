use rue_typing::HashSet;

use indexmap::{IndexMap, IndexSet};
use rue_typing::{Type, TypeId, TypeSystem};

use crate::{Database, DependencyGraph, Symbol, SymbolId, WarningKind};

#[derive(Debug, Default)]
pub struct SymbolTable {
    symbol_type_references: IndexMap<SymbolId, IndexSet<TypeId>>,
    type_type_references: IndexMap<TypeId, IndexSet<TypeId>>,
}

impl SymbolTable {
    pub fn insert_symbol_type_reference(&mut self, symbol_id: SymbolId, type_id: TypeId) {
        self.symbol_type_references
            .entry(symbol_id)
            .or_default()
            .insert(type_id);
    }

    pub fn insert_type_type_reference(&mut self, type_id: TypeId, referenced_type_id: TypeId) {
        self.type_type_references
            .entry(type_id)
            .or_default()
            .insert(referenced_type_id);
    }

    pub fn type_referenced_by_symbol(&self, type_id: TypeId, symbol_id: SymbolId) -> bool {
        self.symbol_type_references
            .get(&symbol_id)
            .is_some_and(|type_ids| type_ids.contains(&type_id))
    }

    pub fn referenced_types_for_type(&self, type_id: TypeId) -> Vec<TypeId> {
        self.type_type_references
            .get(&type_id)
            .map(|type_ids| type_ids.iter().copied().collect())
            .unwrap_or_default()
    }

    pub fn calculate_unused(
        &self,
        db: &mut Database,
        ty: &TypeSystem,
        dependency_graph: &DependencyGraph,
        ignored_symbols: &HashSet<SymbolId>,
        ignored_types: &HashSet<TypeId>,
    ) {
        let mut type_ids = IndexSet::new();
        let mut symbol_ids = IndexSet::new();
        let mut exempt_types = IndexSet::new();
        let mut exempt_symbols = IndexSet::new();

        let mut used_symbols = HashSet::new();

        for symbol_id in dependency_graph
            .scopes()
            .into_iter()
            .flat_map(|scope_id| db.scope(scope_id).local_symbols())
            .collect::<Vec<SymbolId>>()
        {
            if dependency_graph.symbol_references(symbol_id) > 0
                || ignored_symbols.contains(&symbol_id)
            {
                used_symbols.insert(symbol_id);
                continue;
            }

            symbol_ids.insert(symbol_id);

            let token = db.symbol_token(symbol_id).unwrap();

            if token.text().starts_with('_') {
                exempt_symbols.insert(symbol_id);
            }
        }

        let directly_used_types = db.named_types().into_iter().filter(|type_id| {
            used_symbols
                .iter()
                .any(|symbol_id| self.type_referenced_by_symbol(*type_id, *symbol_id))
        });

        let mut used_types = HashSet::new();
        for type_id in directly_used_types {
            self.calculate_used_types(type_id, &mut used_types);
        }

        for type_id in db.named_types() {
            if used_types.contains(&type_id) || ignored_types.contains(&type_id) {
                continue;
            }

            type_ids.insert(type_id);

            let token = db.type_token(type_id).unwrap();

            if token.text().starts_with('_') {
                exempt_types.insert(type_id);
            }
        }

        for symbol_id in &symbol_ids {
            if exempt_symbols.contains(symbol_id) {
                continue;
            }
            let token = db.symbol_token(*symbol_id).unwrap();
            let kind = match db.symbol(*symbol_id).clone() {
                Symbol::Unknown => unreachable!(),
                // Symbol::Module(..) => WarningKind::UnusedModule(token.to_string()),
                Symbol::Module(..) => continue,
                Symbol::Function(..) => WarningKind::UnusedFunction(token.to_string()),
                Symbol::InlineFunction(..) => WarningKind::UnusedInlineFunction(token.to_string()),
                Symbol::Parameter(..) => WarningKind::UnusedParameter(token.to_string()),
                Symbol::Let(..) => WarningKind::UnusedLet(token.to_string()),
                Symbol::Const(..) => WarningKind::UnusedConst(token.to_string()),
                Symbol::InlineConst(..) => WarningKind::UnusedInlineConst(token.to_string()),
            };
            db.warning(kind, token.text_range());
        }

        for type_id in &type_ids {
            if exempt_types.contains(type_id) {
                continue;
            }
            let token = db.type_token(*type_id).unwrap();
            let kind = match ty.get_raw(*type_id) {
                Type::Generic => WarningKind::UnusedGenericType(token.to_string()),
                Type::Alias(..) => WarningKind::UnusedTypeAlias(token.to_string()),
                Type::Struct(..) => WarningKind::UnusedStruct(token.to_string()),
                Type::Enum(..) => WarningKind::UnusedEnum(token.to_string()),
                Type::Variant(..) => WarningKind::UnusedEnumVariant(token.to_string()),
                _ => continue,
            };
            db.warning(kind, token.text_range());
        }
    }

    fn calculate_used_types(&self, type_id: TypeId, used_types: &mut HashSet<TypeId>) {
        if !used_types.insert(type_id) {
            return;
        }

        for referenced_type_id in self.referenced_types_for_type(type_id) {
            self.calculate_used_types(referenced_type_id, used_types);
        }
    }
}
