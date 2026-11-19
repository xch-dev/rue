use std::collections::HashMap;

use clvmr::NodePtr;
use id_arena::Arena;

use crate::{Lir, LirId};

/// Debug information attached to LIR nodes, which the optimizer and codegen carry through to
/// the CLVM they generate (see [`optimize_with_tags`](crate::optimize_with_tags) and
/// [`codegen_with_tags`](crate::codegen_with_tags)).
///
/// The tags are opaque indices into tables kept by whatever produced them, such as the source
/// locations and functions recorded while lowering. They're only collected when a debug sidecar
/// is requested, so compiling without one doesn't pay for them.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct DebugTags {
    /// The source location that each node was generated from.
    pub spans: HashMap<LirId, usize>,
    /// The function whose body each node is the root of.
    pub functions: HashMap<LirId, usize>,
    /// A name for each node that's the root of something that isn't a function, such as the body
    /// of a function that was inlined.
    pub labels: HashMap<LirId, usize>,
}

/// The tags of the CLVM generated from tagged LIR. Only pairs are tagged: atoms that have the same
/// small value share a [`NodePtr`], so one can't be told apart from another, and they take the
/// location of the pair they're in instead.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NodeTags {
    /// The source location that each pair was generated from.
    pub spans: HashMap<NodePtr, usize>,
    /// The function whose body each pair is the root of.
    pub functions: HashMap<NodePtr, usize>,
    /// The name of each pair that's the root of something that isn't a function.
    pub labels: HashMap<NodePtr, usize>,
}

/// Keeps track of the innermost span being generated as nodes are allocated in an arena, so that
/// each node can be tagged with the span that was innermost when it was allocated. Nodes are
/// allocated in order, so this only needs to remember where the innermost span changed, rather
/// than tagging nodes as they're allocated.
#[derive(Debug, Default, Clone)]
pub struct SpanScopes {
    /// The arena length at which each change happened, and the innermost span from then on.
    changes: Vec<(usize, Option<usize>)>,
    /// The spans being generated, from the outermost to the innermost.
    stack: Vec<usize>,
}

impl SpanScopes {
    /// Starts generating nodes for the span, with the arena at its current length.
    pub fn enter(&mut self, arena_len: usize, span: usize) {
        self.stack.push(span);
        self.changes.push((arena_len, Some(span)));
    }

    /// Stops generating nodes for the innermost span, with the arena at its current length.
    pub fn exit(&mut self, arena_len: usize) {
        self.stack.pop();
        self.changes.push((arena_len, self.stack.last().copied()));
    }

    /// Tags each node allocated from `start` onwards with the span that was innermost when it was
    /// allocated, unless it's already tagged.
    pub fn apply(&self, arena: &Arena<Lir>, start: usize, tags: &mut HashMap<LirId, usize>) {
        let mut changes = self.changes.iter().peekable();
        let mut span = None;

        for (index, (id, _)) in arena.iter().enumerate().skip(start) {
            while let Some(&&(at, next)) = changes.peek()
                && at <= index
            {
                span = next;
                changes.next();
            }

            if let Some(span) = span {
                tags.entry(id).or_insert(span);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use clvmr::{Allocator, SExp};

    use super::*;
    use crate::{CodegenOptions, codegen, codegen_with_tags, optimize, optimize_with_tags};

    #[test]
    fn codegen_tags_the_pairs_each_node_generates() {
        let mut arena = Arena::new();
        let path = arena.alloc(Lir::Path(2u8.into()));
        let five = arena.alloc(Lir::Atom(vec![5]));
        let untagged = arena.alloc(Lir::First(path));
        let add = arena.alloc(Lir::Add(vec![untagged, five]));

        let tags = DebugTags {
            spans: HashMap::from([(add, 1), (five, 2)]),
            functions: HashMap::from([(add, 0), (path, 1)]),
            labels: HashMap::from([(add, 2), (path, 3)]),
        };

        let mut allocator = Allocator::new();
        let (ptr, nodes) = codegen_with_tags(
            &arena,
            &mut allocator,
            add,
            CodegenOptions::default(),
            &tags,
        )
        .unwrap();

        // The program is the same as without tags
        let plain = codegen(&arena, &mut allocator, add, CodegenOptions::default()).unwrap();
        assert_eq!(
            rue_clvm::disassemble_program(&allocator, ptr),
            rue_clvm::disassemble_program(&allocator, plain)
        );

        // (+ (f 2) (q . 5))
        let pairs = |ptr| match allocator.sexp(ptr) {
            SExp::Pair(first, rest) => (first, rest),
            SExp::Atom => panic!("expected a pair"),
        };
        let (_, args) = pairs(ptr);
        let (first, rest) = pairs(args);
        let (quote, _) = pairs(rest);

        assert_eq!(nodes.spans.get(&ptr), Some(&1));
        assert_eq!(nodes.spans.get(&args), Some(&1));
        // The untagged node takes the span of the node it's in
        assert_eq!(nodes.spans.get(&first), Some(&1));
        // The innermost tag wins
        assert_eq!(nodes.spans.get(&quote), Some(&2));

        // Only pairs are marked as functions, since atoms can't be told apart
        assert_eq!(nodes.functions, HashMap::from([(ptr, 0)]));
        assert_eq!(nodes.labels, HashMap::from([(ptr, 2)]));
    }

    #[test]
    fn optimizer_carries_tags_over() {
        let mut arena = Arena::new();
        let one = arena.alloc(Lir::Atom(vec![1]));
        let two = arena.alloc(Lir::Atom(vec![2]));
        let sum = arena.alloc(Lir::Add(vec![one, two]));
        let path = arena.alloc(Lir::Path(2u8.into()));
        let untagged = arena.alloc(Lir::Concat(vec![path, sum]));
        let body = arena.alloc(Lir::Not(untagged));

        let mut tags = DebugTags {
            spans: HashMap::from([(sum, 1), (body, 2)]),
            functions: HashMap::from([(body, 0)]),
            labels: HashMap::from([(body, 1)]),
        };

        let plain = optimize(&mut arena, body);
        let optimized = optimize_with_tags(&mut arena, body, &mut tags);

        // The LIR is the same as without tags
        let mut allocator = Allocator::new();
        let plain = codegen(&arena, &mut allocator, plain, CodegenOptions::default()).unwrap();
        let program =
            codegen(&arena, &mut allocator, optimized, CodegenOptions::default()).unwrap();
        assert_eq!(
            rue_clvm::disassemble_program(&allocator, program),
            rue_clvm::disassemble_program(&allocator, plain)
        );
        assert_eq!(
            rue_clvm::disassemble_program(&allocator, program),
            "(not (concat 2 (q . 3)))"
        );

        let Lir::Not(concat) = arena[optimized] else {
            panic!("expected not");
        };
        let Lir::Concat(args) = &arena[concat] else {
            panic!("expected concat");
        };

        // The function's body is optimized to a new node, which is still the function
        assert_eq!(tags.functions.get(&optimized), Some(&0));
        assert_eq!(tags.labels.get(&optimized), Some(&1));
        assert_eq!(tags.spans.get(&optimized), Some(&2));
        // The folded sum keeps its span, and the untagged node takes the span of the node it's in
        assert_eq!(tags.spans.get(&args[1]), Some(&1));
        assert_eq!(tags.spans.get(&concat), Some(&2));
    }

    #[test]
    fn nodes_take_the_innermost_span() {
        let mut arena = Arena::new();
        let mut scopes = SpanScopes::default();

        let before = arena.alloc(Lir::Atom(vec![0]));
        scopes.enter(arena.len(), 1);
        let outer = arena.alloc(Lir::Atom(vec![1]));
        scopes.enter(arena.len(), 2);
        // A span that's entered and exited without allocating anything doesn't change anything
        scopes.enter(arena.len(), 3);
        scopes.exit(arena.len());
        let inner = arena.alloc(Lir::Atom(vec![2]));
        let tagged = arena.alloc(Lir::Atom(vec![3]));
        scopes.exit(arena.len());
        let after_inner = arena.alloc(Lir::Atom(vec![4]));
        scopes.exit(arena.len());
        let after = arena.alloc(Lir::Atom(vec![5]));

        let mut tags = HashMap::from([(tagged, 7)]);
        scopes.apply(&arena, 0, &mut tags);

        assert_eq!(tags.get(&before), None);
        assert_eq!(tags.get(&outer), Some(&1));
        assert_eq!(tags.get(&inner), Some(&2));
        // Tags that are already there are kept
        assert_eq!(tags.get(&tagged), Some(&7));
        assert_eq!(tags.get(&after_inner), Some(&1));
        assert_eq!(tags.get(&after), None);

        // Nodes before the start aren't tagged
        let mut tags = HashMap::new();
        scopes.apply(&arena, 2, &mut tags);
        assert_eq!(tags.get(&outer), None);
        assert_eq!(tags.get(&inner), Some(&2));
    }
}
