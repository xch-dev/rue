use std::collections::HashSet;

use indexmap::IndexSet;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BindingSymbol, ConstantSymbol, Declaration, FunctionSymbol, ParameterSymbol, Symbol,
};
use rue_types::{Alias, Generic, Struct, Type};

use crate::Compiler;

pub fn check_unused(ctx: &mut Compiler, entrypoints: &HashSet<Declaration>) {
    let mut used_symbols = IndexSet::new();
    let mut unused_symbols = IndexSet::new();

    for declaration in ctx.relevant_declarations().collect::<Vec<_>>() {
        let Declaration::Symbol(symbol) = declaration else {
            continue;
        };

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                if let Declaration::Symbol(current_symbol) = current
                    && current_symbol == symbol
                    && entrypoints.contains(&current)
                    && let Some(name) = ctx.symbol(symbol).clone().name()
                {
                    ctx.diagnostic(
                        name,
                        DiagnosticKind::RecursiveEntrypoint(name.text().to_string()),
                    );
                }

                continue;
            }

            for parent in ctx.reference_parents(current) {
                stack.push(parent);

                match parent {
                    Declaration::Symbol(parent_symbol) => {
                        if entrypoints.contains(&Declaration::Symbol(parent_symbol)) {
                            used_symbols.insert(symbol);
                        }
                    }
                    Declaration::Type(parent_ty) => {
                        if entrypoints.contains(&Declaration::Type(parent_ty)) {
                            used_symbols.insert(symbol);
                        }
                    }
                }
            }
        }

        if entrypoints.contains(&Declaration::Symbol(symbol)) {
            used_symbols.insert(symbol);
            continue;
        }

        if !used_symbols.contains(&symbol) {
            unused_symbols.insert(symbol);
        }
    }

    let mut used_types = IndexSet::new();
    let mut unused_types = IndexSet::new();

    for declaration in ctx.relevant_declarations() {
        let Declaration::Type(ty) = declaration else {
            continue;
        };

        if entrypoints.contains(&Declaration::Type(ty)) {
            used_types.insert(ty);
            continue;
        }

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            for parent in ctx.reference_parents(current) {
                stack.push(parent);

                match parent {
                    Declaration::Symbol(symbol) => {
                        if used_symbols.contains(&symbol) {
                            used_types.insert(ty);
                            break;
                        }
                    }
                    Declaration::Type(ty) => {
                        if entrypoints.contains(&Declaration::Type(ty)) {
                            used_types.insert(ty);
                            break;
                        }
                    }
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
        match ctx.symbol(symbol).clone() {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => {}
            Symbol::Function(FunctionSymbol { name, .. }) => {
                if let Some(name) = name {
                    if name.text().starts_with('_') {
                        continue;
                    }

                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedFunction(name.text().to_string()),
                    );
                }
            }
            Symbol::Binding(BindingSymbol { name, .. }) => {
                if let Some(name) = name {
                    if name.text().starts_with('_') {
                        continue;
                    }

                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedBinding(name.text().to_string()),
                    );
                }
            }
            Symbol::Constant(ConstantSymbol { name, .. }) => {
                if let Some(name) = name {
                    if name.text().starts_with('_') {
                        continue;
                    }

                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedConstant(name.text().to_string()),
                    );
                }
            }
            Symbol::Parameter(ParameterSymbol { name, .. }) => {
                if let Some(name) = name {
                    if name.text().starts_with('_') {
                        continue;
                    }

                    ctx.diagnostic(
                        &name,
                        DiagnosticKind::UnusedParameter(name.text().to_string()),
                    );
                }
            }
        }
    }

    for &ty in &unused_types {
        match ctx.ty(ty).clone() {
            Type::Generic(Generic { name: Some(name) }) => {
                if name.text().starts_with('_') {
                    continue;
                }

                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnusedGenericType(name.text().to_string()),
                );
            }
            Type::Struct(Struct {
                name: Some(name), ..
            }) => {
                if name.text().starts_with('_') {
                    continue;
                }

                ctx.diagnostic(&name, DiagnosticKind::UnusedStruct(name.text().to_string()));
            }
            Type::Alias(Alias {
                name: Some(name), ..
            }) => {
                if name.text().starts_with('_') {
                    continue;
                }

                ctx.diagnostic(
                    &name,
                    DiagnosticKind::UnusedTypeAlias(name.text().to_string()),
                );
            }
            _ => {}
        }
    }
}
