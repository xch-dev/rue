use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    mem,
};

use indexmap::IndexSet;

/// Caches the results of a recursive computation over types, which keeps the items in progress on
/// a stack and returns early when it reaches one of them again (to handle recursive types).
///
/// Without this, a type that is referenced more than once (such as `T` in `(T, T)`) would be
/// visited again for every reference, which is exponential in the depth of the type.
///
/// A result is only reused if computing it again would produce the same result. That isn't the
/// case if it relied on an item that was in progress at the time (and has since finished), or if
/// it would reach an item that is in progress now (since that would return early this time).
///
/// For the latter, an item that is in progress now and that the result could reach must also be
/// able to reach the result (since that's how the current item was reached), so the item is part
/// of a cycle with the result. When the result was computed, the item would have reached it again
/// and relied on it while it was in progress. So it's enough to remember the items that relied on
/// an item outside of themselves, rather than everything that was visited.
#[derive(Debug)]
pub(crate) struct Memo<S, K, V> {
    /// The lowest index on the stack that was reached again since the current item started.
    low: usize,
    /// The items that relied on an item outside of themselves since the current item started.
    open: Vec<S>,
    done: HashMap<K, Done<S, V>>,
}

#[derive(Debug)]
struct Done<S, V> {
    value: V,
    open: HashSet<S>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Frame {
    index: usize,
    low: usize,
    open: usize,
}

impl<S, K, V> Default for Memo<S, K, V> {
    fn default() -> Self {
        Self {
            low: usize::MAX,
            open: Vec::new(),
            done: HashMap::new(),
        }
    }
}

impl<S, K, V> Memo<S, K, V>
where
    S: Copy + Eq + Hash,
    K: Eq + Hash,
    V: Clone,
{
    /// Records that the item at the given index on the stack was reached again.
    pub fn reached(&mut self, index: usize) {
        self.low = self.low.min(index);
    }

    /// Returns the previous result for the key, if computing it again would produce the same.
    pub fn get(&mut self, key: &K, mut stack: impl Iterator<Item = S>) -> Option<V> {
        let done = self.done.get(key)?;

        if stack.any(|item| done.open.contains(&item)) {
            return None;
        }

        self.open.extend(&done.open);

        Some(done.value.clone())
    }

    /// Starts computing the item that will be at the given index on the stack.
    pub fn enter(&mut self, index: usize) -> Frame {
        Frame {
            index,
            low: mem::replace(&mut self.low, usize::MAX),
            open: self.open.len(),
        }
    }

    /// Finishes computing the item, and caches the result under the key if possible.
    pub fn exit(&mut self, frame: Frame, item: S, key: Option<K>, value: V) {
        if self.low < frame.index {
            self.open.push(item);
        } else if let Some(key) = key {
            let open: HashSet<S> = self.open.drain(frame.open..).collect();
            self.open.extend(&open);
            self.done.insert(key, Done { value, open });
        }

        self.low = self.low.min(frame.low);
    }
}

/// Caches the results of a comparison whose result is the least fixed point of monotone equations
/// over a finite set of items, built from `max`, `min` and constants over a chain of values, where
/// returning early at an item in progress gives the least value. Unlike [`Memo`], a result is
/// cached even if it relied on items in progress, and it's reused while they're still in progress.
///
/// The results inside a cycle depend on where it was entered, so a reused result can differ from
/// what computing it again would give. But the result of the computation as a whole (which starts
/// from an empty stack) doesn't. Write `μ` for the least fixed point, and `μ[D]` for the least fixed
/// point with the items in `D` fixed to the least value instead of their equation:
///
/// - Every value computed is at most `μ`, since returning early gives the least value, reused
///   results are at most `μ` (by induction), and the equations are monotone with `μ` as a fixed
///   point.
/// - Every value computed for an item, given the items on the stack below it that it returned early
///   at or reused a result that relied on (`D`), is at least `μ[D]`. Its equation's inputs are at
///   least `μ[D ∪ {item}]` (by induction), and fixing only the item gives a unary function `h` of
///   the value it's fixed to, so the value is at least `h(least)`. That's `μ[D]` (which is the least
///   fixed point of `h`), since `h` is built from `max`, `min` and constants, so `h(h(least))` is
///   `h(least)` (it's `h(least) ∨ (y ∧ h(greatest))` at `y`).
/// - A cached result is reused while `D` is on the stack, where computing it again would give
///   `μ[S]` for the stack `S`. That's at most `μ[D]`, since fixing more items to the least value
///   can only make the fixed point smaller. So it's at least what computing it again would give.
///
/// So the result of a computation that starts from an empty stack (where `D` is empty) is exactly
/// `μ`, with or without reusing results, and since every value is at least what computing it would
/// give and the equations are monotone, so is the result.
#[derive(Debug)]
pub(crate) struct FixpointMemo<K, V> {
    /// The indices on the stack of the items that were reached again (or that a reused result
    /// relied on) since the current item started, which haven't been filtered out by an item that
    /// finished.
    reached: Vec<usize>,
    /// Each result, along with the items in progress that it relied on.
    done: HashMap<K, (V, Vec<K>)>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct FixpointFrame {
    index: usize,
    reached: usize,
}

impl<K, V> Default for FixpointMemo<K, V> {
    fn default() -> Self {
        Self {
            reached: Vec::new(),
            done: HashMap::new(),
        }
    }
}

impl<K, V> FixpointMemo<K, V>
where
    K: Copy + Eq + Hash,
    V: Copy,
{
    /// Records that the item at the given index on the stack was reached again.
    pub fn reached(&mut self, index: usize) {
        self.reached.push(index);
    }

    /// Returns the previous result for the key, if the items it relied on are still in progress.
    pub fn get(&mut self, key: &K, stack: &IndexSet<K>) -> Option<V> {
        let (value, dependencies) = self.done.get(key)?;

        let indices = dependencies
            .iter()
            .map(|item| stack.get_index_of(item))
            .collect::<Option<Vec<usize>>>()?;

        self.reached.extend(indices);

        Some(*value)
    }

    /// Starts computing the item that will be at the given index on the stack.
    pub fn enter(&self, index: usize) -> FixpointFrame {
        FixpointFrame {
            index,
            reached: self.reached.len(),
        }
    }

    /// Finishes computing the item (which has been removed from the stack), and caches the result
    /// under the key, along with the items below it that it relied on.
    pub fn exit(&mut self, frame: FixpointFrame, key: Option<K>, value: V, stack: &IndexSet<K>) {
        let mut dependencies: Vec<usize> = self
            .reached
            .drain(frame.reached..)
            .filter(|&index| index < frame.index)
            .collect();
        dependencies.sort_unstable();
        dependencies.dedup();

        if let Some(key) = key {
            let items = dependencies.iter().map(|&index| stack[index]).collect();
            self.done.insert(key, (value, items));
        }

        self.reached.extend(dependencies);
    }
}
