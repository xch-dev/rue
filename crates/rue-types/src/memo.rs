use std::{hash::Hash, mem};

use crate::{FxHashMap, FxHashSet};

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
    done: FxHashMap<K, Done<S, V>>,
}

#[derive(Debug)]
struct Done<S, V> {
    value: V,
    open: FxHashSet<S>,
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
            done: FxHashMap::default(),
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
            let open: FxHashSet<S> = self.open.drain(frame.open..).collect();
            self.open.extend(&open);
            self.done.insert(key, Done { value, open });
        }

        self.low = self.low.min(frame.low);
    }
}

/// Computes the least fixed point of monotone equations over a finite set of items, built from
/// `max`, `min` and constants over a chain of values (such as comparisons between recursive
/// types), where the computation recurses into the items an item's equation refers to, and an item
/// that's reached again while it's in progress is given its current approximation.
///
/// This is chaotic iteration. The computation as a whole (which starts from an empty stack) runs
/// in rounds, and each item is computed at most once per round. Within a round, an item that's
/// already been computed is given its value from this round, and an item in progress is given
/// its value from the previous round, or the least value if it has none. A round in which every
/// item that was reached again while in progress finished with the value it was given is the last
/// one, and its values are exact.
///
/// Write `μ` for the least fixed point.
///
/// - Every value is at most `μ`, since the approximations start at the least value, and the
///   equations are monotone with `μ` as a fixed point.
/// - The approximations only increase from one round to the next, since an item's inputs in a
///   round are at least what they were in the previous one (by induction). So each round that
///   doesn't end the computation increases at least one item, and there are at most twice as many
///   rounds as items for a chain of three values (and nearly always two or fewer).
/// - In the last round, every item's value is its equation applied to the values of the items it
///   refers to in that round, since the ones that were in progress finished with the values they
///   were given. The items reached in that round only refer to each other (a fallback that isn't
///   compared is a `min` that doesn't depend on it), so their values are a fixed point of their
///   equations, which is at least `μ`.
///
/// So the values in the last round are exactly `μ`, and they're kept for later computations, while
/// the approximations from earlier rounds are discarded.
#[derive(Debug)]
pub(crate) struct FixpointMemo<K, V> {
    /// Exact values, from computations that have finished.
    exact: FxHashMap<K, V>,
    /// The approximation of each item reached by the computation in progress, along with the
    /// round it was last computed in.
    approximations: FxHashMap<K, (V, usize)>,
    round: usize,
    /// For each item in progress, the approximation it was given when it was reached again, if it
    /// was.
    reached: Vec<Option<V>>,
    /// Whether an item in progress finished with another value than it was given when it was
    /// reached again, during this round.
    changed: bool,
}

impl<K, V> Default for FixpointMemo<K, V> {
    fn default() -> Self {
        Self {
            exact: FxHashMap::default(),
            approximations: FxHashMap::default(),
            round: 0,
            reached: Vec::new(),
            changed: false,
        }
    }
}

impl<K, V> FixpointMemo<K, V>
where
    K: Copy + Eq + Hash,
    V: Copy + Ord,
{
    /// Starts a round of the computation.
    pub fn start_round(&mut self) {
        self.round += 1;
        self.changed = false;
    }

    /// Finishes a round of the computation, and returns `true` if its values are exact. Otherwise,
    /// another round is needed.
    pub fn finish_round(&mut self) -> bool {
        if self.changed {
            return false;
        }

        let round = self.round;

        self.exact.extend(
            self.approximations
                .drain()
                .filter(|&(_, (_, computed))| computed == round)
                .map(|(key, (value, _))| (key, value)),
        );

        true
    }

    /// Returns the value of the item for an item in progress at the given index on the stack that
    /// was reached again, which is its approximation from the previous round (or the least value,
    /// if it has none).
    pub fn reached(&mut self, index: usize, key: Option<&K>, least: V) -> V {
        let value = key
            .and_then(|key| self.approximations.get(key))
            .map_or(least, |&(value, _)| value);

        let reached = &mut self.reached[index];
        debug_assert!(reached.is_none_or(|reached| reached == value));
        *reached = Some(value);

        value
    }

    /// Returns the item's value, if it's exact or has already been computed in this round.
    pub fn get(&self, key: &K) -> Option<V> {
        if let Some(&value) = self.exact.get(key) {
            return Some(value);
        }

        self.approximations
            .get(key)
            .filter(|&&(_, round)| round == self.round)
            .map(|&(value, _)| value)
    }

    /// Starts computing an item, which will be at the next index on the stack.
    pub fn enter(&mut self) {
        self.reached.push(None);
    }

    /// Finishes computing the item, and records its approximation under the key.
    pub fn exit(&mut self, key: Option<K>, value: V) {
        let reached = self.reached.pop().unwrap();

        let Some(key) = key else {
            return;
        };

        // The approximations only increase, since the equations are monotone, but this keeps the
        // computation finite even if they weren't
        let value = match self.approximations.get(&key) {
            Some(&(previous, _)) => {
                debug_assert!(previous <= value);
                previous.max(value)
            }
            None => value,
        };

        if reached.is_some_and(|reached| reached != value) {
            self.changed = true;
        }

        self.approximations.insert(key, (value, self.round));
    }
}
