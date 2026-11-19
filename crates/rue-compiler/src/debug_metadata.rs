//! Debug metadata sidecars for compiled programs, in the structural CLVM debug metadata format
//! from Chia-Network/chialisp#579 (see [`format`]). Debuggers use a sidecar to symbolize the stack
//! of a program that failed: which function each frame is in, where in the source, and the values
//! of its parameters.

mod format;

pub use format::*;

use std::{collections::HashMap, path::Path};

use clvmr::{Allocator, NodePtr, SExp};
use rue_diagnostic::{Name, Source};
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
    // Values are checked against a list of anything, which is the same type for every parameter
    let builtins = ctx.builtins().types;
    let any_list = ctx.alloc_type(Type::Apply(Apply::new(
        builtins.list,
        HashMap::from([(builtins.list_generic, builtins.permissive_any)]),
    )));

    let mut builder = SidecarBuilder {
        ctx,
        any_list,
        lowering,
        base_path,
        spans: HashMap::new(),
        functions: HashMap::new(),
        positions: Positions::default(),
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
    /// `List<Any>`, which proper lists are assignable to.
    any_list: TypeId,
    lowering: &'a LoweringDebugInfo,
    base_path: &'a Path,
    /// The index in the metadata of each span recorded while lowering.
    spans: HashMap<usize, usize>,
    /// The index in the metadata of each function recorded while lowering.
    functions: HashMap<usize, usize>,
    positions: Positions,
}

impl SidecarBuilder<'_> {
    fn span(&mut self, metadata: &mut DebugMetadataBuilder, span: usize) -> usize {
        if let Some(&index) = self.spans.get(&span) {
            return index;
        }

        let srcloc = &self.lowering.spans[span];
        let source = &srcloc.source;
        let file = metadata.file(&source.kind.display(self.base_path), || {
            source.text.to_string()
        });

        let (start_line, start_column) = self.positions.position(file, source, srcloc.span.start);
        let (end_line, end_column) = self.positions.position(file, source, srcloc.span.end);

        let index = metadata.span(Span {
            file,
            start_line,
            start_column,
            end_line,
            end_column,
        });
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

        let parameters = self.parameters(metadata, &lowered.environment, &lowered.environment);

        let index = metadata.function(Function {
            name,
            left_env: false,
            parameters,
        });
        self.functions.insert(function, index);
        index
    }

    /// The parameter pattern that mirrors the part of the function's environment, with each value
    /// that's shown at its path in the whole environment, which is the path the function's body
    /// reads it from. Captured functions and constants are part of the environment, but aren't
    /// shown.
    fn parameters(
        &mut self,
        metadata: &mut DebugMetadataBuilder,
        root: &Environment,
        environment: &Environment,
    ) -> Parameter {
        rue_types::grow_stack(|| match environment {
            Environment::Nil => Parameter::Nil,
            Environment::Pair(first, rest) => {
                let first = self.parameters(metadata, root, first);
                let rest = self.parameters(metadata, root, rest);
                Parameter::Pair(Box::new(first), Box::new(rest))
            }
            Environment::Leaf(symbol) => match self.value(*symbol) {
                Some((name, ty)) => Parameter::Name {
                    name: metadata.string(&name),
                    path: root
                        .path(*symbol)
                        .expect("a symbol in the environment has a path"),
                    constraint: constraint(self.ctx, ty, self.any_list),
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

/// The lines and display columns of byte offsets in sources. The columns of a line are found once,
/// the first time an offset on it is needed, so that a long line isn't scanned again for each span
/// on it.
#[derive(Debug, Default)]
struct Positions {
    /// The column at each byte offset of each line, by the file's index in the metadata and the
    /// line's index.
    lines: HashMap<(usize, usize), Vec<usize>>,
}

impl Positions {
    /// The line (starting at 1) and display column of a byte offset in the source, which is the
    /// file with the index in the metadata. Lines end at line feeds, as the format counts them.
    fn position(&mut self, file: usize, source: &Source, offset: usize) -> (usize, usize) {
        let lines = source.line_index();
        let line = lines.line(offset);
        let range = lines
            .line_range(line)
            .expect("the line of an offset exists");

        let columns = self
            .lines
            .entry((file, line))
            .or_insert_with(|| line_columns(&source.text[range.clone()]));

        // An offset past the end of the line is at its line break
        (
            line + 1,
            columns[(offset - range.start).min(columns.len() - 1)],
        )
    }
}

/// What a value of the type is expected to be at runtime, which debuggers use to display it. Types
/// that don't correspond to one of the constraints, such as unions of atoms and pairs, generics
/// and functions, are unknown.
fn constraint(ctx: &mut Compiler, ty: TypeId, any_list: TypeId) -> Constraint {
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

    if compare(types, &builtins, ty, any_list) == Comparison::Assign {
        return Constraint::ProperList;
    }

    if compare(types, &builtins, ty, builtins.recursive_any_pair) == Comparison::Assign {
        return Constraint::Pair;
    }

    Constraint::Unknown
}

#[cfg(test)]
mod tests {
    use rue_diagnostic::SourceKind;

    use super::*;

    fn positions(text: &str, offsets: &[usize]) -> Vec<(usize, usize)> {
        let source = Source::new(text.into(), SourceKind::File("main.rue".to_string()));
        let mut positions = Positions::default();
        offsets
            .iter()
            .map(|&offset| positions.position(0, &source, offset))
            .collect()
    }

    #[test]
    fn positions_follow_the_format() {
        // The example in the format's specification
        let text = "λ\tX\r\n";
        let x = text.find('X').unwrap();
        assert_eq!(positions(text, &[x, x + 1]), [(1, 8), (1, 9)]);

        // Lines end at line feeds, so a carriage return on its own is part of the line, and the
        // end of a line (where its line break is) is after its last character
        let text = "a\r\n\tb\rc\nd";
        let offsets = [
            0,
            text.find('\r').unwrap(),
            text.find('b').unwrap(),
            text.find('c').unwrap(),
            text.rfind('\n').unwrap(),
            text.find('d').unwrap(),
            text.len(),
        ];
        assert_eq!(
            positions(text, &offsets),
            [(1, 1), (1, 2), (2, 8), (2, 10), (2, 11), (3, 1), (3, 2)]
        );
    }
}
