use indexmap::IndexSet;
use rue_parser::{ModuleItem, Root};

use crate::{Compiler, Module, Scope, Symbol, SymbolId};

use super::Declarations;

impl Compiler<'_> {
    pub fn declare_root(&mut self, root: &Root) -> (SymbolId, Declarations) {
        let scope_id = self.db.alloc_scope(Scope::default());
        let module_id = self.db.alloc_symbol(Symbol::Module(Module {
            scope_id,
            exported_symbols: IndexSet::new(),
            exported_types: IndexSet::new(),
        }));

        self.scope_stack.push(scope_id);
        let declarations = self.declare_items(&root.items());
        self.scope_stack.pop().unwrap();

        (module_id, declarations)
    }

    /// Compile the root by lowering all items into scope.
    pub fn compile_root(&mut self, root: &Root, module_id: SymbolId, declarations: Declarations) {
        let Symbol::Module(Module {
            scope_id,
            exported_symbols,
            exported_types,
        }) = self.db.symbol_mut(module_id)
        else {
            unreachable!();
        };
        exported_symbols.extend(declarations.exported_symbols.clone());
        exported_types.extend(declarations.exported_types.clone());
        self.scope_stack.push(*scope_id);
        self.compile_items(&root.items(), declarations);
        self.scope_stack.pop().unwrap();
    }

    /// Define a module in the current scope.
    /// This creates a new scope for the module, and declares its items.
    /// The exports are added during this phase too.
    pub fn declare_module_item(&mut self, module_item: &ModuleItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let scope_id = self.db.alloc_scope(Scope::default());
        let symbol_id = self.db.alloc_symbol(Symbol::Module(Module {
            scope_id,
            exported_symbols: IndexSet::new(),
            exported_types: IndexSet::new(),
        }));

        if let Some(name) = module_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_scope_token(scope_id, name.clone());
            self.db.insert_symbol_token(symbol_id, name);
        }

        // Add the symbol to the stack early so you can track type references.
        self.symbol_stack.push(symbol_id);
        self.scope_stack.push(scope_id);

        let items = module_item.items();
        let declarations = self.declare_items(&items);
        self.compile_items(&items, declarations.clone());

        self.scope_stack.pop().unwrap();
        self.symbol_stack.pop().unwrap();

        let Symbol::Module(Module {
            exported_symbols,
            exported_types,
            ..
        }) = self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        exported_types.extend(declarations.exported_types);
        exported_symbols.extend(declarations.exported_symbols);

        symbol_id
    }
}
