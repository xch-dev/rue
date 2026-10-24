use std::collections::HashSet;

use rue_diagnostic::DiagnosticKind;
use rue_hir::{ConstantSymbol, Declaration, FunctionKind, FunctionSymbol, Symbol};

use crate::Compiler;

/// Reports inline functions and constants that reference themselves, which can't be compiled.
pub fn check_recursion(ctx: &mut Compiler) {
    for declaration in ctx.relevant_declarations().collect::<Vec<_>>() {
        let Declaration::Symbol(symbol) = declaration else {
            continue;
        };

        if !ctx.declaration_parents(declaration).is_empty()
            && !ctx.reference_parents(declaration).is_empty()
        {
            continue;
        }

        let mut visited = HashSet::new();
        let mut stack = vec![declaration];

        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                if current == declaration
                    && let Some(name) = ctx.symbol(symbol).name()
                {
                    match ctx.symbol(symbol) {
                        Symbol::Function(FunctionSymbol {
                            kind: FunctionKind::Inline,
                            ..
                        }) => {
                            ctx.diagnostic_name(
                                &name,
                                DiagnosticKind::RecursiveInlineFunction(name.text().to_string()),
                            );
                        }
                        Symbol::Constant(ConstantSymbol { .. }) => {
                            ctx.diagnostic_name(
                                &name,
                                DiagnosticKind::RecursiveConstant(name.text().to_string()),
                            );
                        }
                        _ => {}
                    }
                }

                continue;
            }

            stack.extend(ctx.reference_parents(current));
        }
    }
}
