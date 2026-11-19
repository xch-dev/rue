use std::collections::HashMap;

use rue_diagnostic::SrcLoc;
use rue_lir::{DebugTags, SpanScopes};
use rue_types::Type;

use crate::{Database, Environment, FunctionSymbol, HirId, SymbolId};

/// What lowering records for a debug sidecar: where each LIR node came from in the source, and
/// which nodes are the bodies of functions, along with how their environments are laid out. It's
/// only recorded when it's requested (see [`Lowerer::with_debug_info`](crate::Lowerer)).
#[derive(Debug, Default, Clone)]
pub struct LoweringDebugInfo {
    /// The source locations that the LIR's span tags refer to.
    pub spans: Vec<SrcLoc>,
    /// The functions that the LIR's function tags refer to.
    pub functions: Vec<LoweredFunction>,
    /// The tags of the LIR nodes.
    pub tags: DebugTags,
    /// The names of the parameters of the functions that were lowered or inlined, which parameter
    /// symbols don't have themselves.
    pub parameter_names: HashMap<SymbolId, String>,
    /// The span of each HIR node that's been entered, so that it's only added once.
    hir_spans: HashMap<HirId, usize>,
    /// The function of each symbol that's been lowered, so that it's only added once.
    function_indices: HashMap<SymbolId, usize>,
    pub(super) scopes: SpanScopes,
}

/// A function whose body was lowered, rather than inlined.
#[derive(Debug, Clone)]
pub struct LoweredFunction {
    pub symbol: SymbolId,
    /// The environment that the body is evaluated in, which includes its captures.
    pub environment: Environment,
}

impl LoweringDebugInfo {
    /// The span of the HIR node, if it has a location.
    pub(super) fn hir_span(&mut self, db: &Database, hir: HirId) -> Option<usize> {
        if let Some(&span) = self.hir_spans.get(&hir) {
            return Some(span);
        }

        let span = self.span(db.hir_srcloc(hir)?.clone());
        self.hir_spans.insert(hir, span);
        Some(span)
    }

    pub(super) fn span(&mut self, srcloc: SrcLoc) -> usize {
        self.spans.push(srcloc);
        self.spans.len() - 1
    }

    pub(super) fn function(&mut self, symbol: SymbolId, environment: &Environment) -> usize {
        *self.function_indices.entry(symbol).or_insert_with(|| {
            self.functions.push(LoweredFunction {
                symbol,
                environment: environment.clone(),
            });
            self.functions.len() - 1
        })
    }

    /// Remembers the names of the function's parameters, which are only kept in its type.
    pub(super) fn add_parameter_names(&mut self, db: &Database, function: &FunctionSymbol) {
        let Type::Function(ty) = db.ty(function.ty) else {
            return;
        };

        for (&parameter, param) in function.parameters.iter().zip(&ty.params) {
            if !param.name.is_empty() {
                self.parameter_names
                    .entry(parameter)
                    .or_insert_with(|| param.name.clone());
            }
        }
    }
}
