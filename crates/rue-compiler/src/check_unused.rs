use std::collections::HashSet;

use indexmap::IndexSet;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BindingSymbol, ConstantSymbol, Declaration, FunctionKind, FunctionSymbol, ParameterSymbol,
    Symbol,
};
use rue_types::{Alias, Generic, Struct, Type};

use crate::Compiler;

pub fn check_unused(ctx: &mut Compiler, entrypoints: &HashSet<Declaration>) {
    let mut used_declarations = IndexSet::new();
    let mut unused_declarations = IndexSet::new();

    for declaration in ctx.relevant_declarations().collect::<Vec<_>>() {
        let Declaration::Symbol(symbol) = declaration else {
            continue;
        };

        if !ctx.declaration_parents(declaration).is_empty()
            && !ctx.reference_parents(declaration).is_empty()
        {
            used_declarations.insert(declaration);
            continue;
        }

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                if let Declaration::Symbol(current_symbol) = current
                    && current_symbol == symbol
                    && let Some(name) = ctx.symbol(symbol).clone().name()
                {
                    if entrypoints.contains(&current) {
                        ctx.diagnostic(
                            name,
                            DiagnosticKind::RecursiveEntrypoint(name.text().to_string()),
                        );
                    } else if matches!(
                        ctx.symbol(symbol),
                        Symbol::Function(FunctionSymbol {
                            kind: FunctionKind::Inline,
                            ..
                        })
                    ) {
                        ctx.diagnostic(
                            name,
                            DiagnosticKind::RecursiveInlineFunction(name.text().to_string()),
                        );
                    } else if matches!(ctx.symbol(symbol), Symbol::Constant(ConstantSymbol { .. }))
                    {
                        ctx.diagnostic(
                            name,
                            DiagnosticKind::RecursiveConstant(name.text().to_string()),
                        );
                    }
                }

                continue;
            }

            for parent in ctx.reference_parents(current) {
                stack.push(parent);

                match parent {
                    Declaration::Symbol(parent_symbol) => {
                        if entrypoints.contains(&Declaration::Symbol(parent_symbol)) {
                            used_declarations.insert(declaration);
                        }
                    }
                    Declaration::Type(parent_ty) => {
                        if entrypoints.contains(&Declaration::Type(parent_ty)) {
                            used_declarations.insert(declaration);
                        }
                    }
                }
            }
        }

        if entrypoints.contains(&declaration) {
            used_declarations.insert(declaration);
            continue;
        }

        if !used_declarations.contains(&declaration) {
            unused_declarations.insert(declaration);
        }
    }

    for declaration in ctx.relevant_declarations() {
        if !matches!(declaration, Declaration::Type(_)) {
            continue;
        }

        if entrypoints.contains(&declaration) {
            used_declarations.insert(declaration);
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
                    Declaration::Symbol(parent_symbol) => {
                        if used_declarations.contains(&Declaration::Symbol(parent_symbol)) {
                            used_declarations.insert(declaration);
                            break;
                        }
                    }
                    Declaration::Type(parent_ty) => {
                        if entrypoints.contains(&Declaration::Type(parent_ty)) {
                            used_declarations.insert(declaration);
                            break;
                        }
                    }
                }
            }

            if used_declarations.contains(&declaration) {
                break;
            }
        }

        if !used_declarations.contains(&declaration) {
            unused_declarations.insert(declaration);
        }
    }

    for &declaration in &unused_declarations {
        match declaration {
            Declaration::Symbol(symbol) => match ctx.symbol(symbol).clone() {
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
            },
            Declaration::Type(ty) => match ctx.ty(ty).clone() {
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
            },
        }
    }
}
