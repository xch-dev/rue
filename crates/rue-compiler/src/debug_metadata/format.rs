//! A writer for structural CLVM debug metadata, version 1, as specified by `DEBUG_METADATA_FORMAT.md`
//! in Chialisp (introduced in Chia-Network/chialisp#579). It's written here rather than with the
//! `chialisp` crate, so that Rue doesn't depend on Chialisp's compiler to emit it.
//!
//! The metadata is a CLVM serialization of:
//!
//! ```text
//! ("CHIALISP_DEBUG" 1 (1 8) PROGRAM_SHA256
//!   ((PATH SOURCE) ...)
//!   (STRING ...)
//!   ((FILE START_LINE START_COLUMN END_LINE END_COLUMN) ...)
//!   ((FUNCTION_NAME LEFT_ENV PARAMETER_TREE) ...)
//!   SHADOW_TREE)
//! ```

use std::collections::HashMap;

use clvmr::{Allocator, NodePtr, SExp, serde::node_to_bytes_limit};
use num_bigint::{BigInt, BigUint};
use sha2::{Digest, Sha256};

const MAGIC: &[u8] = b"CHIALISP_DEBUG";

/// The version of the format that's written, which only changes for incompatible changes.
pub const DEBUG_METADATA_VERSION: u32 = 1;

/// How far apart tab stops are when columns are counted.
pub const TAB_WIDTH: usize = 8;

/// A source file, with its full text, which readers display excerpts from.
#[derive(Debug, Clone, PartialEq, Eq)]
struct SourceFile {
    /// A display name for the file, which doesn't need to be a path that can be read.
    pub path: String,
    pub source: String,
}

/// A range of source text. Lines and columns start at 1, columns are counted in Unicode scalars
/// with tabs expanded to the next multiple of [`TAB_WIDTH`] (see [`line_columns`]), and the end is
/// exclusive.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: usize,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

/// What a parameter is expected to be, which readers can use to display its value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Constraint {
    Unknown,
    /// An atom that's a signed integer.
    Integer,
    Atom,
    /// An atom of exactly this many bytes.
    Bytes(usize),
    Pair,
    /// A nil-terminated list, including nil.
    ProperList,
}

/// The pattern of a function's parameters, which mirrors the environment its body is evaluated
/// in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Parameter {
    /// A part of the environment that isn't shown, such as a captured function. It still takes up
    /// its place in the environment.
    Nil,
    Name {
        /// The index of the name in the string table.
        name: usize,
        /// The path of the value in the environment.
        path: BigUint,
        constraint: Constraint,
    },
    Pair(Box<Parameter>, Box<Parameter>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// The index of the name in the string table.
    pub name: usize,
    /// Whether the first value in the environment is captured by the compiler, rather than being
    /// one of the parameters, in which case readers don't show it.
    pub left_env: bool,
    pub parameters: Parameter,
}

/// What a node of the program is annotated with, each of which is an index into a table.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Annotation {
    pub span: Option<usize>,
    pub label: Option<usize>,
    /// The function whose body the node is the root of.
    pub function: Option<usize>,
}

/// Builds debug metadata for a program. The tables are filled in first, and then the program is
/// annotated with them when it's encoded.
#[derive(Debug, Default, Clone)]
pub struct DebugMetadataBuilder {
    files: Vec<SourceFile>,
    file_indices: HashMap<String, usize>,
    strings: Vec<String>,
    string_indices: HashMap<String, usize>,
    spans: Vec<Span>,
    span_indices: HashMap<Span, usize>,
    functions: Vec<Function>,
}

impl DebugMetadataBuilder {
    /// The index of the file with the path, which is added with the source if it isn't there yet.
    pub fn file(&mut self, path: &str, source: impl FnOnce() -> String) -> usize {
        if let Some(&index) = self.file_indices.get(path) {
            return index;
        }

        self.files.push(SourceFile {
            path: path.to_string(),
            source: source(),
        });
        self.file_indices
            .insert(path.to_string(), self.files.len() - 1);
        self.files.len() - 1
    }

    /// The index of the string, which is added if it isn't there yet.
    pub fn string(&mut self, value: &str) -> usize {
        if let Some(&index) = self.string_indices.get(value) {
            return index;
        }

        self.strings.push(value.to_string());
        self.string_indices
            .insert(value.to_string(), self.strings.len() - 1);
        self.strings.len() - 1
    }

    /// The index of the span, which is added if it isn't there yet.
    pub fn span(&mut self, span: Span) -> usize {
        *self.span_indices.entry(span).or_insert_with(|| {
            self.spans.push(span);
            self.spans.len() - 1
        })
    }

    /// Adds a function, and returns its index.
    pub fn function(&mut self, function: Function) -> usize {
        self.functions.push(function);
        self.functions.len() - 1
    }

    /// Encodes the metadata for the program, which is in `program_allocator`. Each node of the
    /// program is annotated by `annotate`, which is given the builder (to add to its tables), the
    /// node, and the annotation of the pair the node is in (or the default for the root). Nodes
    /// are annotated from the root, and the first of each pair before the rest.
    pub fn encode(
        mut self,
        program_allocator: &Allocator,
        program: NodePtr,
        mut annotate: impl FnMut(&mut Self, NodePtr, &Annotation) -> Annotation,
    ) -> Result<Vec<u8>, EncodeError> {
        let mut a = Allocator::new();

        let program_bytes = serialize(program_allocator, program)?;
        let program_sha256: [u8; 32] = Sha256::digest(&program_bytes).into();

        // The shadow tree comes first, since annotating it can add to the tables
        let tree = self.encode_shadow_tree(&mut a, program_allocator, program, &mut annotate)?;

        let magic = a.new_atom(MAGIC)?;
        let version = a.new_small_number(DEBUG_METADATA_VERSION)?;
        let coordinates = {
            let one = a.new_small_number(1)?;
            let tab_width = number(&mut a, TAB_WIDTH)?;
            list(&mut a, &[one, tab_width])?
        };
        let program_sha256 = a.new_atom(&program_sha256)?;

        let mut files = Vec::with_capacity(self.files.len());
        for file in &self.files {
            let path = a.new_atom(file.path.as_bytes())?;
            let source = a.new_atom(file.source.as_bytes())?;
            files.push(list(&mut a, &[path, source])?);
        }
        let files = list(&mut a, &files)?;

        let mut strings = Vec::with_capacity(self.strings.len());
        for string in &self.strings {
            strings.push(a.new_atom(string.as_bytes())?);
        }
        let strings = list(&mut a, &strings)?;

        let mut spans = Vec::with_capacity(self.spans.len());
        for span in &self.spans {
            let fields = [
                span.file,
                span.start_line,
                span.start_column,
                span.end_line,
                span.end_column,
            ]
            .into_iter()
            .map(|value| number(&mut a, value))
            .collect::<Result<Vec<_>, _>>()?;
            spans.push(list(&mut a, &fields)?);
        }
        let spans = list(&mut a, &spans)?;

        let mut functions = Vec::with_capacity(self.functions.len());
        for function in &self.functions {
            let name = number(&mut a, function.name)?;
            let left_env = a.new_small_number(function.left_env.into())?;
            let parameters = encode_parameter(&mut a, &function.parameters)?;
            functions.push(list(&mut a, &[name, left_env, parameters])?);
        }
        let functions = list(&mut a, &functions)?;

        let root = list(
            &mut a,
            &[
                magic,
                version,
                coordinates,
                program_sha256,
                files,
                strings,
                spans,
                functions,
                tree,
            ],
        )?;

        serialize(&a, root)
    }

    /// Encodes the shadow tree, which mirrors the program with an annotation on each node:
    /// `(0 SPAN LABEL FUNCTION VALUE)` for an atom, and `(1 SPAN LABEL FUNCTION FIRST REST)` for
    /// a pair. Programs can be nested arbitrarily deeply, so this doesn't recurse.
    fn encode_shadow_tree(
        &mut self,
        a: &mut Allocator,
        program_allocator: &Allocator,
        program: NodePtr,
        annotate: &mut impl FnMut(&mut Self, NodePtr, &Annotation) -> Annotation,
    ) -> Result<NodePtr, EncodeError> {
        enum Step {
            Visit(NodePtr, Annotation),
            Pair(Annotation),
        }

        let mut steps = vec![Step::Visit(program, Annotation::default())];
        let mut encoded = Vec::new();

        while let Some(step) = steps.pop() {
            match step {
                Step::Visit(node, parent) => {
                    let annotation = annotate(self, node, &parent);

                    match program_allocator.sexp(node) {
                        SExp::Atom => {
                            let value = a.new_atom(program_allocator.atom(node).as_ref())?;
                            encoded.push(shadow_node(a, 0, annotation, &[value])?);
                        }
                        SExp::Pair(first, rest) => {
                            steps.push(Step::Pair(annotation));
                            steps.push(Step::Visit(rest, annotation));
                            steps.push(Step::Visit(first, annotation));
                        }
                    }
                }
                Step::Pair(annotation) => {
                    let rest = encoded.pop().expect("the rest is encoded");
                    let first = encoded.pop().expect("the first is encoded");
                    encoded.push(shadow_node(a, 1, annotation, &[first, rest])?);
                }
            }
        }

        Ok(encoded.pop().expect("the program is encoded"))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum EncodeError {
    #[error("CLVM error: {0}")]
    Clvm(#[from] clvmr::error::EvalErr),
}

/// The display column (see [`Span`]) at each byte offset of a line, including its end, so there's
/// one more column than there are bytes. Offsets inside of a character are at the character.
pub(crate) fn line_columns(line: &str) -> Vec<usize> {
    let mut columns = Vec::with_capacity(line.len() + 1);
    let mut column = 1;

    for char in line.chars() {
        columns.extend(std::iter::repeat_n(column, char.len_utf8()));

        column = if char == '\t' {
            TAB_WIDTH * (column / TAB_WIDTH + 1)
        } else {
            column + 1
        };
    }

    columns.push(column);
    columns
}

fn shadow_node(
    a: &mut Allocator,
    tag: u32,
    annotation: Annotation,
    children: &[NodePtr],
) -> Result<NodePtr, EncodeError> {
    let tag = a.new_small_number(tag)?;
    let span = optional_reference(a, annotation.span)?;
    let label = optional_reference(a, annotation.label)?;
    let function = optional_reference(a, annotation.function)?;
    let mut fields = vec![tag, span, label, function];
    fields.extend_from_slice(children);
    list(a, &fields)
}

/// Serializes the value, without the size limit of [`clvmr::serde::node_to_bytes`], since a sidecar
/// is many times larger than its program.
fn serialize(a: &Allocator, node: NodePtr) -> Result<Vec<u8>, EncodeError> {
    Ok(node_to_bytes_limit(a, node, usize::MAX)?)
}

/// An optional reference is encoded as its index plus one, with zero meaning there isn't one.
fn optional_reference(a: &mut Allocator, index: Option<usize>) -> Result<NodePtr, EncodeError> {
    match index {
        Some(index) => number(a, index + 1),
        None => Ok(NodePtr::NIL),
    }
}

fn encode_parameter(a: &mut Allocator, parameter: &Parameter) -> Result<NodePtr, EncodeError> {
    rue_types::grow_stack(|| match parameter {
        Parameter::Nil => {
            let tag = a.new_small_number(0)?;
            list(a, &[tag])
        }
        Parameter::Name {
            name,
            path,
            constraint,
        } => {
            let tag = a.new_small_number(1)?;
            let name = number(a, *name)?;
            let path = a.new_number(BigInt::from(path.clone()))?;
            let constraint = encode_constraint(a, *constraint)?;
            list(a, &[tag, name, path, constraint])
        }
        Parameter::Pair(first, rest) => {
            let tag = a.new_small_number(2)?;
            let first = encode_parameter(a, first)?;
            let rest = encode_parameter(a, rest)?;
            list(a, &[tag, first, rest])
        }
    })
}

fn encode_constraint(a: &mut Allocator, constraint: Constraint) -> Result<NodePtr, EncodeError> {
    let (tag, length) = match constraint {
        Constraint::Unknown => (0, None),
        Constraint::Integer => (1, None),
        Constraint::Atom => (2, None),
        Constraint::Bytes(length) => (3, Some(length)),
        Constraint::Pair => (4, None),
        Constraint::ProperList => (5, None),
    };

    let tag = a.new_small_number(tag)?;

    if let Some(length) = length {
        let length = number(a, length)?;
        list(a, &[tag, length])
    } else {
        list(a, &[tag])
    }
}

/// An unsigned integer, as the shortest CLVM integer.
fn number(a: &mut Allocator, value: usize) -> Result<NodePtr, EncodeError> {
    Ok(a.new_number(BigInt::from(value))?)
}

fn list(a: &mut Allocator, items: &[NodePtr]) -> Result<NodePtr, EncodeError> {
    let mut result = NodePtr::NIL;

    for &item in items.iter().rev() {
        result = a.new_pair(item, result)?;
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use clvmr::serde::{node_from_bytes, node_to_bytes};

    use super::*;

    fn program(hex: &str) -> (Allocator, NodePtr) {
        let mut allocator = Allocator::new();
        let program = node_from_bytes(&mut allocator, &hex::decode(hex).unwrap()).unwrap();
        (allocator, program)
    }

    // The vectors are from `resources/tests/debug_metadata_v1.json` in Chialisp, which were made
    // independently of its encoder

    #[test]
    fn minimal_vector() {
        let (allocator, program) = program("01");
        let metadata = DebugMetadataBuilder::default()
            .encode(&allocator, program, |_, _, _| Annotation::default())
            .unwrap();
        assert_eq!(
            hex::encode(metadata),
            "ff8e434849414c4953505f4445425547ff01ffff01ff0880ffa04bf5122f344554c53bde2ebb8cd2b7e3d1600ad631c385a5d7cce23c7785459aff80ff80ff80ff80ffff80ff80ff80ff80ff018080"
        );
    }

    #[test]
    fn raw_byte_zero_vector() {
        let (allocator, program) = program("00");
        let metadata = DebugMetadataBuilder::default()
            .encode(&allocator, program, |_, _, _| Annotation::default())
            .unwrap();
        assert_eq!(
            hex::encode(metadata),
            "ff8e434849414c4953505f4445425547ff01ffff01ff0880ffa06e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01dff80ff80ff80ff80ffff80ff80ff80ff80ff008080"
        );
    }

    #[test]
    fn annotated_pair_vector() {
        let (allocator, program) = program("ff012a");

        let mut builder = DebugMetadataBuilder::default();
        let file = builder.file("demo.clsp", || "λ\tX\r\n".to_string());
        let name = builder.string("f");
        let x = builder.string("X");
        let span = builder.span(Span {
            file,
            start_line: 1,
            start_column: 8,
            end_line: 1,
            end_column: 9,
        });
        let function = builder.function(Function {
            name,
            left_env: false,
            parameters: Parameter::Pair(
                Box::new(Parameter::Name {
                    name: x,
                    path: 2u8.into(),
                    constraint: Constraint::Integer,
                }),
                Box::new(Parameter::Nil),
            ),
        });

        // Only the root is annotated
        let metadata = builder
            .encode(&allocator, program, |_, node, _| {
                if node == program {
                    Annotation {
                        span: Some(span),
                        label: Some(name),
                        function: Some(function),
                    }
                } else {
                    Annotation::default()
                }
            })
            .unwrap();

        assert_eq!(
            hex::encode(metadata),
            "ff8e434849414c4953505f4445425547ff01ffff01ff0880ffa0aa084973ea140c5795b7288241c3e2c0902c6e0edcd03bc038ad255eab3c687bffffff8964656d6f2e636c7370ff86cebb09580d0a8080ffff66ff5880ffffff80ff01ff08ff01ff098080ffffff80ff80ffff02ffff01ff01ff02ffff018080ffff8080808080ffff01ff01ff01ff01ffff80ff80ff80ff80ff0180ffff80ff80ff80ff80ff2a808080"
        );
    }

    #[test]
    fn sidecars_can_be_larger_than_serialization_limits() {
        // A list of many items, whose sidecar is over the 2 MB that `node_to_bytes` allows
        let mut allocator = Allocator::new();
        let mut program = NodePtr::NIL;
        for i in 0..100_000 {
            let item = allocator.new_small_number(i).unwrap();
            program = allocator.new_pair(item, program).unwrap();
        }

        let metadata = DebugMetadataBuilder::default()
            .encode(&allocator, program, |_, _, _| Annotation::default())
            .unwrap();
        assert!(metadata.len() > 2_000_000, "{}", metadata.len());
    }

    #[test]
    fn constraint_vectors() {
        for (constraint, expected) in [
            (Constraint::Unknown, "ff8080"),
            (Constraint::Integer, "ff0180"),
            (Constraint::Atom, "ff0280"),
            (Constraint::Pair, "ff0480"),
            (Constraint::ProperList, "ff0580"),
            (Constraint::Bytes(0), "ff03ff8080"),
            (Constraint::Bytes(128), "ff03ff82008080"),
        ] {
            let mut a = Allocator::new();
            let node = encode_constraint(&mut a, constraint).unwrap();
            assert_eq!(
                hex::encode(node_to_bytes(&a, node).unwrap()),
                expected,
                "{constraint:?}"
            );
        }
    }

    #[test]
    fn coordinate_vectors() {
        // Each line is expanded to the column that `X` is at
        for (line, expanded) in [
            ("\tX", "       X"),
            ("1234567\tX", "1234567        X"),
            ("λ\tX", "λ      X"),
            // A combining character is a scalar of its own
            ("e\u{301}\u{754c}\tX", "e\u{301}\u{754c}    X"),
        ] {
            let x = line.find('X').unwrap();
            let columns = line_columns(line);
            assert_eq!(columns[x], expanded.chars().count(), "{line:?}");
            assert_eq!(columns[x + 1], columns[x] + 1, "{line:?}");
            assert_eq!(columns.len(), line.len() + 1);
        }

        // The bytes of a character are all at its column
        assert_eq!(line_columns("λ"), [1, 1, 2]);
    }
}
