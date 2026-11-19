//! Debug metadata sidecars for compiled programs, in the structural CLVM debug metadata format
//! from Chia-Network/chialisp#579 (see [`format`]). Debuggers use a sidecar to symbolize the stack
//! of a program that failed: which function each frame is in, where in the source, and the values
//! of its parameters.

mod format;

pub use format::*;

use std::{collections::HashMap, path::Path};

use clvmr::{Allocator, NodePtr, SExp};
use num_bigint::BigUint;
use rue_diagnostic::{Name, SrcLoc};
use rue_hir::{Environment, LoweringDebugInfo, Symbol, SymbolId};
use rue_lir::NodeTags;
use rue_types::{
    Apply, AtomRestriction, Atoms, Comparison, Type, TypeId, compare, extract_atoms,
    unwrap_semantic,
};

use crate::Compiler;

/// Builds the debug metadata of a program from what was recorded while it was generated.
///
/// Each pair of the program is located at the innermost source that it was generated from, and
/// anything that wasn't generated from source (such as atoms, which can't be told apart) takes the
/// location of the pair it's in. The root of each function body that wasn't inlined is marked as
/// the function, along with the layout of the environment the body is evaluated in.
pub(crate) fn build_debug_metadata(
    ctx: &mut Compiler,
    allocator: &Allocator,
    program: NodePtr,
    lowering: &LoweringDebugInfo,
    nodes: &NodeTags,
    base_path: &Path,
) -> Result<Vec<u8>, EncodeError> {
    let mut builder = SidecarBuilder {
        ctx,
        lowering,
        base_path,
        spans: HashMap::new(),
        functions: HashMap::new(),
    };

    DebugMetadataBuilder::default().encode(allocator, program, |metadata, node, parent| {
        let span = nodes
            .spans
            .get(&node)
            .map(|&span| builder.span(metadata, span))
            .or(parent.span);

        let function = match allocator.sexp(node) {
            SExp::Pair(..) => nodes
                .functions
                .get(&node)
                .map(|&function| builder.function(metadata, function)),
            SExp::Atom => None,
        };

        Annotation {
            span,
            label: None,
            function,
        }
    })
}

struct SidecarBuilder<'a> {
    ctx: &'a mut Compiler,
    lowering: &'a LoweringDebugInfo,
    base_path: &'a Path,
    /// The index in the metadata of each span recorded while lowering.
    spans: HashMap<usize, usize>,
    /// The index in the metadata of each function recorded while lowering.
    functions: HashMap<usize, usize>,
}

impl SidecarBuilder<'_> {
    fn span(&mut self, metadata: &mut DebugMetadataBuilder, span: usize) -> usize {
        if let Some(&index) = self.spans.get(&span) {
            return index;
        }

        let source_span = source_span(metadata, &self.lowering.spans[span], self.base_path);
        let index = metadata.span(source_span);
        self.spans.insert(span, index);
        index
    }

    fn function(&mut self, metadata: &mut DebugMetadataBuilder, function: usize) -> usize {
        if let Some(&index) = self.functions.get(&function) {
            return index;
        }

        let lowered = &self.lowering.functions[function];

        let name = match self.ctx.symbol(lowered.symbol) {
            Symbol::Function(function) => function.name.as_ref().map(Name::text),
            _ => None,
        };
        let name = metadata.string(name.unwrap_or("<lambda>"));

        let parameters = self.parameters(metadata, &lowered.environment, BigUint::from(1u8));

        let index = metadata.function(Function {
            name,
            left_env: false,
            parameters,
        });
        self.functions.insert(function, index);
        index
    }

    /// The parameter pattern that mirrors the environment, with each value that's shown at its
    /// path in it. Captured functions and constants are part of the environment, but aren't
    /// shown.
    fn parameters(
        &mut self,
        metadata: &mut DebugMetadataBuilder,
        environment: &Environment,
        path: BigUint,
    ) -> Parameter {
        rue_types::grow_stack(|| match environment {
            Environment::Nil => Parameter::Nil,
            Environment::Pair(first, rest) => {
                let first = self.parameters(metadata, first, &path << 1);
                let rest = self.parameters(metadata, rest, (path << 1) | BigUint::from(1u8));
                Parameter::Pair(Box::new(first), Box::new(rest))
            }
            Environment::Leaf(symbol) => match self.value(*symbol) {
                Some((name, ty)) => Parameter::Name {
                    name: metadata.string(&name),
                    path,
                    constraint: constraint(self.ctx, ty),
                },
                None => Parameter::Nil,
            },
        })
    }

    /// The name and type of a value in an environment, if it's shown.
    fn value(&self, symbol: SymbolId) -> Option<(String, TypeId)> {
        match self.ctx.symbol(symbol) {
            Symbol::Parameter(parameter) => Some((
                self.lowering.parameter_names.get(&symbol)?.clone(),
                parameter.ty,
            )),
            Symbol::Binding(binding) => {
                Some((binding.name.as_ref()?.text().to_string(), binding.value.ty))
            }
            _ => None,
        }
    }
}

/// The span of a location, whose file is added to the metadata if it isn't there yet.
fn source_span(metadata: &mut DebugMetadataBuilder, srcloc: &SrcLoc, base_path: &Path) -> Span {
    let source = &srcloc.source;
    let file = metadata.file(&source.kind.display(base_path), || source.text.to_string());

    let lines = source.line_index();
    let position = |offset: usize| {
        let line = lines.line(offset);
        let start = lines.line_range(line).map_or(0, |range| range.start);
        (line + 1, column(&source.text[start..offset]))
    };

    let (start_line, start_column) = position(srcloc.span.start);
    let (end_line, end_column) = position(srcloc.span.end);

    Span {
        file,
        start_line,
        start_column,
        end_line,
        end_column,
    }
}

/// What a value of the type is expected to be at runtime, which debuggers use to display it. Types
/// that don't correspond to one of the constraints, such as unions of atoms and pairs, generics
/// and functions, are unknown.
pub(crate) fn constraint(ctx: &mut Compiler, ty: TypeId) -> Constraint {
    let builtins = ctx.builtins().types;
    let types = ctx.types_mut();

    // A struct is only assignable to other types by its fields, rather than as a whole
    let mut ty = ty;

    let semantic = loop {
        let semantic = unwrap_semantic(types, ty, true);

        if let Type::Struct(inner) = &types[semantic] {
            ty = inner.inner;
        } else {
            break semantic;
        }
    };

    if matches!(
        types[semantic],
        Type::Unresolved | Type::Any | Type::Never | Type::Generic(_)
    ) {
        return Constraint::Unknown;
    }

    if compare(types, &builtins, ty, builtins.int) == Comparison::Assign {
        return Constraint::Integer;
    }

    if let Some(atoms) = extract_atoms(types, ty, true) {
        let mut lengths = match &atoms {
            Atoms::Unrestricted => return Constraint::Atom,
            Atoms::Restricted(restrictions) => restrictions.iter().map(|restriction| {
                if let AtomRestriction::Length(length) = restriction {
                    Some(*length)
                } else {
                    None
                }
            }),
        };

        // Only atoms of a single length are bytes of that length
        return match lengths.next().flatten() {
            Some(length) if lengths.all(|other| other == Some(length)) => Constraint::Bytes(length),
            _ => Constraint::Atom,
        };
    }

    let any_list = types.alloc(Type::Apply(Apply::new(
        builtins.list,
        HashMap::from([(builtins.list_generic, builtins.permissive_any)]),
    )));

    if compare(types, &builtins, ty, any_list) == Comparison::Assign {
        return Constraint::ProperList;
    }

    if compare(types, &builtins, ty, builtins.recursive_any_pair) == Comparison::Assign {
        return Constraint::Pair;
    }

    Constraint::Unknown
}
