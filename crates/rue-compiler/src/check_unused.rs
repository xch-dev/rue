use std::collections::HashSet;

use indexmap::IndexSet;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BindingSymbol, ConstantSymbol, Declaration, DependencyGraph, FunctionSymbol, ParameterSymbol,
    Symbol, SymbolId,
};
use rue_types::{Alias, Generic, Struct, Type};

use crate::Compiler;

pub fn check_unused(ctx: &mut Compiler, graph: &DependencyGraph, main: SymbolId) {
    let mut used_symbols = IndexSet::new();
    let mut unused_symbols = IndexSet::new();

    for declaration in ctx.relevant_declarations() {
        let Declaration::Symbol(symbol) = declaration else {
            continue;
        };

        if graph.references(symbol) > 0 || symbol == main {
            used_symbols.insert(symbol);
        } else {
            unused_symbols.insert(symbol);
        }
    }

    let mut used_types = IndexSet::new();
    let mut unused_types = IndexSet::new();

    for declaration in ctx.relevant_declarations() {
        let Declaration::Type(ty) = declaration else {
            continue;
        };

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            for parent in ctx.reference_parents(current) {
                stack.push(parent);

                if let Declaration::Symbol(symbol) = parent
                    && used_symbols.contains(&symbol)
                {
                    used_types.insert(ty);
                    break;
                }
            }

            if used_types.contains(&ty) {
                break;
            }
        }

        if !used_types.contains(&ty) {
            unused_types.insert(ty);
        }
    }

    for &symbol in &unused_symbols {
        let mut parent_is_unused = false;

        for parent in ctx.declaration_parents(Declaration::Symbol(symbol)) {
            if let Declaration::Symbol(parent) = parent
                && unused_symbols.contains(&parent)
            {
                parent_is_unused = true;
                break;
            }
        }

        if parent_is_unused {
            continue;
        }

        match ctx.symbol(symbol).clone() {
            Symbol::Unresolved | Symbol::Module(_) => {}
            Symbol::Function(FunctionSymbol { name, .. }) => {
                if let Some(name) = name {
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedFunction(name.text().to_string()),
                    );
                }
            }
            Symbol::Binding(BindingSymbol { name, .. }) => {
                if let Some(name) = name {
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedBinding(name.text().to_string()),
                    );
                }
            }
            Symbol::Constant(ConstantSymbol { name, .. }) => {
                if let Some(name) = name {
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedConstant(name.text().to_string()),
                    );
                }
            }
            Symbol::Parameter(ParameterSymbol { name, .. }) => {
                if let Some(name) = name {
                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedParameter(name.text().to_string()),
                    );
                }
            }
        }
    }

    for &ty in &unused_types {
        let mut parent_is_unused = false;

        for parent in ctx.declaration_parents(Declaration::Type(ty)) {
            if let Declaration::Symbol(parent) = parent
                && unused_symbols.contains(&parent)
            {
                parent_is_unused = true;
                break;
            }
        }

        if parent_is_unused {
            continue;
        }

        match ctx.ty(ty).clone() {
            Type::Generic(Generic { name: Some(name) }) => {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnusedGenericType(name.text().to_string()),
                );
            }
            Type::Struct(Struct {
                name: Some(name), ..
            }) => {
                ctx.diagnostic(&name, DiagnosticKind::UnusedStruct(name.text().to_string()));
            }
            Type::Alias(Alias {
                name: Some(name), ..
            }) => {
                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnusedTypeAlias(name.text().to_string()),
                );
            }
            _ => {}
        }
    }
}
