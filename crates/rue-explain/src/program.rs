//! A program as a tree of nodes, each with what its debug metadata annotates it with.

use chialisp::compiler::debug_metadata::{
    DebugFunction, DebugMetadata, DebugNode, DebugParameter, DebugSourceFile, DebugSourceSpan,
    ParameterConstraint,
};
use clvmr::{Allocator, NodePtr, SExp, serde::node_from_bytes};
use num_bigint::BigUint;
use sha2::{Digest, Sha256};

use crate::ExplainError;

/// The atoms of the operators that are decompiled specially.
pub(crate) const QUOTE: &[u8] = &[1];
pub(crate) const APPLY: &[u8] = &[2];
pub(crate) const IF: &[u8] = &[3];
pub(crate) const CONS: &[u8] = &[4];
pub(crate) const RAISE: &[u8] = &[8];

/// The atom of code that reads the whole environment.
pub(crate) const WHOLE_ENV: &[u8] = &[1];

/// How much stack decoding a level of a sidecar can take, which is more than it does.
const STACK_PER_LEVEL: usize = 1024;

/// How much stack reading a program takes other than for the levels of its sidecar.
const STACK_BASE: usize = 1024 * 1024;

/// The index of a node in [`Program::nodes`]. Each position in the program is a node of its own,
/// even if it has the same value as another.
pub(crate) type NodeId = usize;

/// The steps from the root of a value to a part of it, where `false` is the first of a pair and
/// `true` is the rest.
pub(crate) type Path = Vec<bool>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NodeKind {
    Atom(Vec<u8>),
    Pair(NodeId, NodeId),
}

#[derive(Debug, Clone)]
pub(crate) struct Node {
    pub kind: NodeKind,
    /// The index of the node's source span in the metadata.
    pub span: Option<usize>,
    /// The index of the node's label in the metadata's strings.
    pub label: Option<usize>,
    /// The index of the function in the metadata whose body the node is the root of.
    pub function: Option<usize>,
}

/// A program, along with the tables of its debug metadata (which are empty without it).
#[derive(Debug)]
pub(crate) struct Program {
    pub nodes: Vec<Node>,
    pub root: NodeId,
    pub files: Vec<DebugSourceFile>,
    pub strings: Vec<String>,
    pub spans: Vec<DebugSourceSpan>,
    /// The functions that the metadata describes.
    pub functions: Vec<FunctionMetadata>,
    /// The tree hash of each node, which tells values apart regardless of their position.
    pub hashes: Vec<[u8; 32]>,
    /// Whether each node is or contains the body of a function.
    pub contains_function: Vec<bool>,
    /// How many nodes are in the subtree of each node, saturating.
    pub sizes: Vec<usize>,
    /// The program, for disassembling parts of it.
    pub allocator: Allocator,
    pub ptrs: Vec<NodePtr>,
}

impl Program {
    /// Reads a serialized program, along with its debug metadata sidecar if there is one, which
    /// must be for exactly this program.
    pub(crate) fn new(program: &[u8], sidecar: Option<&[u8]>) -> Result<Self, ExplainError> {
        // The sidecar is decoded (and dropped) recursively, as deeply as the program or its
        // parameter trees are nested, which is at most a level for each of its bytes
        let needed = sidecar
            .map_or(0, <[u8]>::len)
            .saturating_mul(STACK_PER_LEVEL)
            .saturating_add(STACK_BASE);
        if sidecar.is_none()
            || stacker::remaining_stack().is_some_and(|remaining| remaining > needed)
        {
            Self::read(program, sidecar)
        } else {
            stacker::grow(needed, || Self::read(program, sidecar))
        }
    }

    fn read(program: &[u8], sidecar: Option<&[u8]>) -> Result<Self, ExplainError> {
        let mut allocator = Allocator::new();
        let root = node_from_bytes(&mut allocator, program)
            .map_err(|error| ExplainError::Program(error.to_string()))?;

        let metadata = sidecar
            .map(|sidecar| {
                let metadata = DebugMetadata::decode(sidecar).map_err(ExplainError::Metadata)?;
                metadata
                    .verify_program(program)
                    .map_err(ExplainError::Metadata)?;
                Ok::<_, ExplainError>(metadata)
            })
            .transpose()?;

        let mut builder = Builder {
            allocator: &allocator,
            nodes: Vec::new(),
            ptrs: Vec::new(),
        };
        let root = builder.build(root, metadata.as_ref().map(|metadata| &metadata.tree));
        let Builder { nodes, ptrs, .. } = builder;

        // Only what's needed is kept, so that the rest (which is nested as deeply as the program or
        // a parameter tree is) is dropped on this stack
        let (files, strings, spans, functions) = match metadata {
            Some(metadata) => {
                let functions = metadata
                    .functions
                    .iter()
                    .map(|function| FunctionMetadata {
                        name: metadata.strings.get(function.name).cloned(),
                        layout: Layout::new(&metadata.strings, function),
                    })
                    .collect();
                (metadata.files, metadata.strings, metadata.spans, functions)
            }
            None => Default::default(),
        };

        // Children are always added before their parents, so one pass in order finds both
        let mut hashes = Vec::<[u8; 32]>::with_capacity(nodes.len());
        let mut contains_function = Vec::with_capacity(nodes.len());
        let mut sizes = Vec::<usize>::with_capacity(nodes.len());

        for node in &nodes {
            let (hash, contains, size) = match &node.kind {
                NodeKind::Atom(atom) => (
                    Sha256::new()
                        .chain_update([1])
                        .chain_update(atom)
                        .finalize(),
                    false,
                    1,
                ),
                &NodeKind::Pair(first, rest) => (
                    Sha256::new()
                        .chain_update([2])
                        .chain_update(hashes[first])
                        .chain_update(hashes[rest])
                        .finalize(),
                    contains_function[first] || contains_function[rest],
                    sizes[first].saturating_add(sizes[rest]).saturating_add(1),
                ),
            };
            hashes.push(hash.into());
            contains_function.push(contains || node.function.is_some());
            sizes.push(size);
        }

        Ok(Self {
            nodes,
            root,
            files,
            strings,
            spans,
            functions,
            hashes,
            contains_function,
            sizes,
            allocator,
            ptrs,
        })
    }

    pub(crate) fn kind(&self, node: NodeId) -> &NodeKind {
        &self.nodes[node].kind
    }

    pub(crate) fn atom(&self, node: NodeId) -> Option<&[u8]> {
        match &self.nodes[node].kind {
            NodeKind::Atom(atom) => Some(atom),
            NodeKind::Pair(..) => None,
        }
    }

    pub(crate) fn pair(&self, node: NodeId) -> Option<(NodeId, NodeId)> {
        match self.nodes[node].kind {
            NodeKind::Atom(_) => None,
            NodeKind::Pair(first, rest) => Some((first, rest)),
        }
    }

    pub(crate) fn is_nil(&self, node: NodeId) -> bool {
        self.atom(node).is_some_and(<[u8]>::is_empty)
    }

    /// The items of a list, if it's a proper list.
    pub(crate) fn list(&self, mut node: NodeId) -> Option<Vec<NodeId>> {
        let mut items = Vec::new();

        loop {
            match &self.nodes[node].kind {
                NodeKind::Atom(atom) => return atom.is_empty().then_some(items),
                &NodeKind::Pair(first, rest) => {
                    items.push(first);
                    node = rest;
                }
            }
        }
    }

    /// The part of the node at the path, if every step but the last is through a pair.
    pub(crate) fn descend(&self, mut node: NodeId, path: &[bool]) -> Option<NodeId> {
        for &step in path {
            let (first, rest) = self.pair(node)?;
            node = if step { rest } else { first };
        }
        Some(node)
    }

    /// Whether two nodes have the same value.
    pub(crate) fn same_value(&self, a: NodeId, b: NodeId) -> bool {
        a == b || self.hashes[a] == self.hashes[b]
    }

    /// Every node in the subtree of the node, including itself.
    pub(crate) fn subtree(&self, node: NodeId) -> Vec<NodeId> {
        let mut nodes = Vec::new();
        let mut stack = vec![node];

        while let Some(node) = stack.pop() {
            nodes.push(node);

            if let NodeKind::Pair(first, rest) = self.nodes[node].kind {
                stack.push(rest);
                stack.push(first);
            }
        }

        nodes
    }

    pub(crate) fn disassemble(&self, node: NodeId) -> String {
        rue_clvm::disassemble_program(&self.allocator, self.ptrs[node])
    }

    pub(crate) fn disassemble_value(&self, node: NodeId) -> String {
        rue_clvm::disassemble_value(&self.allocator, self.ptrs[node])
    }

    pub(crate) fn string(&self, index: usize) -> Option<&str> {
        self.strings.get(index).map(String::as_str)
    }
}

struct Builder<'a> {
    allocator: &'a Allocator,
    nodes: Vec<Node>,
    ptrs: Vec<NodePtr>,
}

impl Builder<'_> {
    /// Adds the nodes of the program, with the annotations of the shadow tree that mirrors it.
    /// Programs can be nested arbitrarily deeply, so this doesn't recurse.
    fn build(&mut self, root: NodePtr, tree: Option<&DebugNode>) -> NodeId {
        enum Step<'a> {
            Visit(NodePtr, Option<&'a DebugNode>),
            Pair(NodePtr, Option<&'a DebugNode>),
        }

        let mut steps = vec![Step::Visit(root, tree)];
        let mut built = Vec::new();

        while let Some(step) = steps.pop() {
            match step {
                Step::Visit(ptr, shadow) => match self.allocator.sexp(ptr) {
                    SExp::Atom => {
                        let atom = self.allocator.atom(ptr).as_ref().to_vec();
                        built.push(self.add(ptr, NodeKind::Atom(atom), shadow));
                    }
                    SExp::Pair(first, rest) => {
                        let (first_shadow, rest_shadow) = match shadow {
                            Some(DebugNode::Pair { left, right, .. }) => {
                                (Some(&**left), Some(&**right))
                            }
                            _ => (None, None),
                        };
                        steps.push(Step::Pair(ptr, shadow));
                        steps.push(Step::Visit(rest, rest_shadow));
                        steps.push(Step::Visit(first, first_shadow));
                    }
                },
                Step::Pair(ptr, shadow) => {
                    let rest = built.pop().expect("the rest is built");
                    let first = built.pop().expect("the first is built");
                    built.push(self.add(ptr, NodeKind::Pair(first, rest), shadow));
                }
            }
        }

        built.pop().expect("the program is built")
    }

    fn add(&mut self, ptr: NodePtr, kind: NodeKind, shadow: Option<&DebugNode>) -> NodeId {
        let (span, label, function) = match shadow {
            Some(
                DebugNode::Atom {
                    span,
                    label,
                    function,
                    ..
                }
                | DebugNode::Pair {
                    span,
                    label,
                    function,
                    ..
                },
            ) => (*span, *label, *function),
            None => (None, None, None),
        };

        self.nodes.push(Node {
            kind,
            span,
            label,
            function,
        });
        self.ptrs.push(ptr);
        self.nodes.len() - 1
    }
}

/// The path that an atom of code reads from the environment, which is `None` for nil (which
/// evaluates to nil rather than reading anything). Leading zero bytes don't change the path.
pub(crate) fn atom_path(atom: &[u8]) -> Option<Path> {
    let number = BigUint::from_bytes_be(atom);
    let bits = number.bits();

    if bits == 0 {
        return None;
    }

    Some((0..bits - 1).map(|bit| number.bit(bit)).collect())
}

/// The number of a path, as it's written in CLVM.
pub(crate) fn path_number(path: &[bool]) -> BigUint {
    let mut number = BigUint::from(1u8) << path.len();

    for (bit, &step) in path.iter().enumerate() {
        if step {
            number.set_bit(bit as u64, true);
        }
    }

    number
}

/// How many steps the paths of a function's parameters can have in all, which is far more than any
/// function has, before the parameters aren't used.
const MAX_LAYOUT_STEPS: usize = 4096;

/// What the debug metadata says about a function.
#[derive(Debug, Clone)]
pub(crate) struct FunctionMetadata {
    pub name: Option<String>,
    pub layout: Option<Layout>,
}

/// A value of a function's environment that the debug metadata names.
#[derive(Debug, Clone)]
pub(crate) struct NamedSlot {
    pub name: String,
    pub path: Path,
    pub constraint: ParameterConstraint,
}

/// The layout of a function's environment, from its parameter tree: the values it names, and the
/// parts that are hidden (such as captured functions and constants, and the end of the list).
#[derive(Debug, Clone, Default)]
pub(crate) struct Layout {
    pub named: Vec<NamedSlot>,
    pub hidden: Vec<Path>,
}

impl Layout {
    /// The layout of the function's environment, unless its parameter tree is inconsistent (a
    /// name whose path isn't where it is in the tree), in which case none of it is trusted, or
    /// larger than any function's is (since each call looks at each of its parts).
    pub(crate) fn new(strings: &[String], function: &DebugFunction) -> Option<Self> {
        let mut layout = Self::default();
        let mut stack = vec![(&function.parameters, Vec::new())];
        let mut steps = 0usize;

        while let Some((parameter, position)) = stack.pop() {
            steps += position.len();
            if steps > MAX_LAYOUT_STEPS {
                return None;
            }

            match parameter {
                DebugParameter::Nil => layout.hidden.push(position),
                DebugParameter::Name {
                    name,
                    path,
                    constraint,
                } => {
                    if atom_path(path).as_ref() != Some(&position) {
                        return None;
                    }

                    layout.named.push(NamedSlot {
                        name: strings.get(*name)?.clone(),
                        path: position,
                        constraint: constraint.clone(),
                    });
                }
                DebugParameter::Pair(first, rest) => {
                    let mut first_position = position.clone();
                    first_position.push(false);
                    let mut rest_position = position;
                    rest_position.push(true);
                    stack.push((rest, rest_position));
                    stack.push((first, first_position));
                }
            }
        }

        Some(layout)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paths_round_trip() {
        for (atom, path, number) in [
            (&[1u8][..], vec![], 1u32),
            (&[2], vec![false], 2),
            (&[3], vec![true], 3),
            (&[5], vec![true, false], 5),
            (&[6], vec![false, true], 6),
            (&[0, 11], vec![true, true, false], 11),
        ] {
            assert_eq!(atom_path(atom), Some(path.clone()));
            assert_eq!(path_number(&path), BigUint::from(number));
        }

        assert_eq!(atom_path(&[]), None);
        assert_eq!(atom_path(&[0, 0]), None);
    }
}
