use std::collections::{HashMap, HashSet};

use rue_diagnostic::DiagnosticKind;
use rue_hir::{ConstantSymbol, Declaration, FunctionKind, FunctionSymbol, Symbol};

use crate::Compiler;

/// Reports inline functions and constants that reference themselves, which can't be compiled.
pub fn check_recursion(ctx: &mut Compiler) {
    let declarations: Vec<Declaration> = ctx
        .relevant_declarations()
        .filter(|&declaration| {
            ctx.declaration_parents(declaration).is_empty()
                || ctx.reference_parents(declaration).is_empty()
        })
        .collect();

    let cyclic = cyclic_declarations(ctx, &declarations);

    for declaration in declarations {
        let Declaration::Symbol(symbol) = declaration else {
            continue;
        };

        if !cyclic.contains(&declaration) {
            continue;
        }

        let Some(name) = ctx.symbol(symbol).name() else {
            continue;
        };

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
}

/// A declaration that's being visited, along with the declarations that reference it and how many
/// of them have been visited so far.
struct Visit {
    declaration: Declaration,
    index: usize,
    parents: Vec<Declaration>,
    next: usize,
}

/// The state of Tarjan's algorithm, over the graph of declarations and the ones that reference them.
struct Components<'a> {
    ctx: &'a Compiler,
    /// The order in which each declaration was first visited.
    indices: HashMap<Declaration, usize>,
    /// The lowest index reachable from each declaration through the ones still on the stack.
    low: Vec<usize>,
    on_stack: Vec<bool>,
    stack: Vec<Declaration>,
}

impl Components<'_> {
    fn enter(&mut self, declaration: Declaration) -> Visit {
        let index = self.low.len();
        self.indices.insert(declaration, index);
        self.low.push(index);
        self.on_stack.push(true);
        self.stack.push(declaration);

        Visit {
            declaration,
            index,
            parents: self.ctx.reference_parents(declaration),
            next: 0,
        }
    }
}

/// Finds the declarations reachable from the roots that can reach themselves again by following
/// the declarations that reference them, which are the ones in a strongly connected component with
/// more than one declaration, or that reference themselves directly.
///
/// This uses Tarjan's algorithm, which visits each declaration once. It's iterative, so that a long
/// chain of references doesn't overflow the stack.
fn cyclic_declarations(ctx: &Compiler, roots: &[Declaration]) -> HashSet<Declaration> {
    let mut components = Components {
        ctx,
        indices: HashMap::new(),
        low: Vec::new(),
        on_stack: Vec::new(),
        stack: Vec::new(),
    };

    let mut cyclic = HashSet::new();

    for &root in roots {
        if components.indices.contains_key(&root) {
            continue;
        }

        let mut visits = vec![components.enter(root)];

        while let Some(visit) = visits.last_mut() {
            let index = visit.index;

            if let Some(&parent) = visit.parents.get(visit.next) {
                visit.next += 1;

                if let Some(&parent_index) = components.indices.get(&parent) {
                    if components.on_stack[parent_index] {
                        components.low[index] = components.low[index].min(parent_index);
                    }
                } else {
                    visits.push(components.enter(parent));
                }

                continue;
            }

            let visit = visits.pop().unwrap();

            if let Some(outer) = visits.last() {
                let outer = outer.index;
                components.low[outer] = components.low[outer].min(components.low[index]);
            }

            if components.low[index] != index {
                continue;
            }

            let mut component = Vec::new();

            loop {
                let declaration = components.stack.pop().unwrap();
                components.on_stack[components.indices[&declaration]] = false;
                component.push(declaration);

                if declaration == visit.declaration {
                    break;
                }
            }

            if component.len() > 1 || visit.parents.contains(&visit.declaration) {
                cyclic.extend(component);
            }
        }
    }

    cyclic
}
