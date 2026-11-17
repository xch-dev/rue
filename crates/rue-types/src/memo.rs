use std::{hash::Hash, mem};

use crate::{FxHashMap, FxHashSet, FxIndexSet};

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

/// Caches the results of a computation whose result is the least fixed point of monotone equations
/// over a finite set of items, built from `max`, `min` and constants over a chain of values (such
/// as comparisons between recursive types), where the computation recurses into the items an
/// item's equation refers to, and returns early at an item that's reached again while it's in
/// progress. Write `μ` for the least fixed point, and `μ[D]` for the least fixed point with the
/// items in `D` fixed to the least value instead of their equation.
///
/// A computation that starts from an empty stack runs in one of two ways.
///
/// **In a single pass**, as long as it doesn't take much longer than the number of items it
/// reaches. Returning early gives the least value, and a result is cached along with the items in
/// progress that it relied on (`D`), and reused while they're still in progress. The results
/// inside a cycle depend on where it was entered, so a reused result can differ from what
/// computing it again would give, but the result of the computation as a whole doesn't:
///
/// - Every value computed is at most `μ`, since returning early gives the least value, reused
///   results are at most `μ` (by induction), and the equations are monotone with `μ` as a fixed
///   point.
/// - Every value computed for an item is at least `μ[D]`. Its equation's inputs are at least
///   `μ[D ∪ {item}]` (by induction), and fixing only the item gives a unary function `h` of the
///   value it's fixed to, so the value is at least `h(least)`. That's `μ[D]` (which is the least
///   fixed point of `h`), since `h` is built from `max`, `min` and constants, so `h(h(least))` is
///   `h(least)` (it's `h(least) ∨ (y ∧ h(greatest))` at `y`).
/// - A cached result is reused while `D` is on the stack, where computing it again would give
///   `μ[S]` for the stack `S`. That's at most `μ[D]`, since fixing more items to the least value
///   can only make the fixed point smaller. So it's at least what computing it again would give.
///
/// So the result of a computation that starts from an empty stack (where `D` is empty) is exactly
/// `μ`, and so is every result with an empty `D`, which can be reused anywhere.
///
/// Inside a large cycle, a result is rarely reusable, since the items it relied on are no longer
/// in progress when it's reached again by another path. That can take exponential time, so a
/// computation that takes too long is abandoned (see `abandoned`) and run again **by iterating**
/// instead. This is chaotic iteration: the computation runs in rounds, and each item is computed
/// at most once per round. Within a round, an item that's already been computed is given its value
/// from this round, and an item reached again while it's in progress is given its value from the
/// previous round (or the least value, if it has none). A round in which every item reached again
/// while in progress finished with the value it was given is the last one:
///
/// - Every value is at most `μ`, since the approximations start at the least value, and the
///   equations are monotone with `μ` as a fixed point.
/// - The approximations only increase from one round to the next, since an item's inputs in a
///   round are at least what they were in the previous one (by induction). So each round that
///   doesn't end the computation increases at least one item, and there are at most twice as many
///   rounds as items for a chain of three values.
/// - In the last round, every item's value is its equation applied to the values of the items it
///   refers to in that round, since the ones that were in progress finished with the values they
///   were given. The items reached in that round only refer to each other (a fallback that isn't
///   compared is a `min` that doesn't depend on it), so their values are a fixed point of their
///   equations, which is at least `μ`.
///
/// So the values in the last round are exactly `μ`, and they're cached for later computations,
/// while the approximations from earlier rounds are discarded. Each round takes time proportional
/// to the number of items it reaches.
///
/// An item without a key isn't cached or iterated. It's given the least value when it's reached
/// again, as in a single pass, which is only exact if such an item never relies on itself.
#[derive(Debug)]
pub(crate) struct FixpointMemo<K, V> {
    /// Each result, along with the items in progress that it relied on. A result that relied on
    /// none of them is exact.
    done: FxHashMap<K, (V, Vec<K>)>,
    /// In a single pass, the indices on the stack of the items that were reached again (or that a
    /// reused result relied on) since the current item started, which haven't been filtered out by
    /// an item that finished.
    reached: Vec<usize>,
    /// The number of items computed in the current pass, and the number of results cached before
    /// it started, which bound how long it may take before it's abandoned.
    steps: usize,
    cached: usize,
    abandoned: bool,
    /// Whether the computation is iterating rather than running in a single pass.
    iterating: bool,
    /// While iterating, the approximation of each item reached, along with the round it was last
    /// computed in.
    approximations: FxHashMap<K, (V, usize)>,
    round: usize,
    /// While iterating, for each item in progress, the approximation it was given when it was
    /// reached again, if it was.
    given: Vec<Option<V>>,
    /// Whether an item in progress finished with another value than it was given when it was
    /// reached again, during this round.
    changed: bool,
}

/// A computation that starts from an empty stack is abandoned in favour of iterating once it has
/// computed more than this many items for each distinct item, plus `BASE_STEPS`. Comparing the
/// examples and tests computes at most about 25 per item, so they never iterate.
const STEPS_PER_ITEM: usize = 64;
const BASE_STEPS: usize = 16_384;

#[derive(Debug, Clone, Copy)]
pub(crate) struct FixpointFrame {
    index: usize,
    reached: usize,
}

impl<K, V> Default for FixpointMemo<K, V> {
    fn default() -> Self {
        Self {
            done: FxHashMap::default(),
            reached: Vec::new(),
            steps: 0,
            cached: 0,
            abandoned: false,
            iterating: false,
            approximations: FxHashMap::default(),
            round: 0,
            given: Vec::new(),
            changed: false,
        }
    }
}

impl<K, V> FixpointMemo<K, V>
where
    K: Copy + Eq + Hash,
    V: Copy + Ord,
{
    /// Starts a computation from an empty stack, in a single pass.
    pub fn start(&mut self) {
        self.steps = 0;
        self.cached = self.done.len();
        self.abandoned = false;
        self.iterating = false;
    }

    /// Whether the single pass took too long, so the computation must stop and iterate instead.
    /// Every item still in progress then returns right away, and isn't cached.
    pub fn abandoned(&self) -> bool {
        self.abandoned
    }

    /// Starts a round of iterating, after the single pass was abandoned.
    pub fn start_round(&mut self) {
        self.iterating = true;
        self.abandoned = false;
        self.reached.clear();
        self.round += 1;
        self.changed = false;
    }

    /// Finishes a round of iterating, and returns `true` if its values are exact. Otherwise,
    /// another round is needed.
    pub fn finish_round(&mut self) -> bool {
        if self.changed {
            return false;
        }

        let round = self.round;

        self.done.extend(
            self.approximations
                .drain()
                .filter(|&(_, (_, computed))| computed == round)
                .map(|(key, (value, _))| (key, (value, Vec::new()))),
        );

        true
    }

    /// Records that the item at the given index on the stack was reached again, and returns the
    /// value it's given: the least value in a single pass, or its approximation from the previous
    /// round while iterating.
    pub fn reached(&mut self, index: usize, key: Option<&K>, least: V) -> V {
        if !self.iterating {
            self.reached.push(index);
            return least;
        }

        let value = key
            .and_then(|key| self.approximations.get(key))
            .map_or(least, |&(value, _)| value);

        let given = &mut self.given[index];
        debug_assert!(given.is_none_or(|given| given == value));
        *given = Some(value);

        value
    }

    /// Returns the previous result for the key, if it can be reused.
    pub fn get(&mut self, key: &K, stack: &FxIndexSet<K>) -> Option<V> {
        let done = self.done.get(key);

        if self.iterating {
            if let Some((value, dependencies)) = done
                && dependencies.is_empty()
            {
                return Some(*value);
            }

            return self
                .approximations
                .get(key)
                .filter(|&&(_, round)| round == self.round)
                .map(|&(value, _)| value);
        }

        let (value, dependencies) = done?;

        let indices = dependencies
            .iter()
            .map(|item| stack.get_index_of(item))
            .collect::<Option<Vec<usize>>>()?;

        self.reached.extend(indices);

        Some(*value)
    }

    /// Starts computing the item that will be at the given index on the stack.
    pub fn enter(&mut self, index: usize) -> FixpointFrame {
        if self.iterating {
            self.given.push(None);
        } else {
            self.steps += 1;

            let items = self.done.len() - self.cached;

            if self.steps > STEPS_PER_ITEM * items + BASE_STEPS {
                self.abandoned = true;
            }
        }

        FixpointFrame {
            index,
            reached: self.reached.len(),
        }
    }

    /// Finishes computing the item (which has been removed from the stack), and caches the result
    /// under the key: along with the items below it that it relied on in a single pass, or as its
    /// approximation for this round while iterating.
    pub fn exit(&mut self, frame: FixpointFrame, key: Option<K>, value: V, stack: &FxIndexSet<K>) {
        if self.iterating {
            self.exit_round(key, value);
            return;
        }

        let mut dependencies: Vec<usize> = self
            .reached
            .drain(frame.reached..)
            .filter(|&index| index < frame.index)
            .collect();
        dependencies.sort_unstable();
        dependencies.dedup();

        if let Some(key) = key
            && !self.abandoned
        {
            let items = dependencies.iter().map(|&index| stack[index]).collect();
            self.done.insert(key, (value, items));
        }

        self.reached.extend(dependencies);
    }

    fn exit_round(&mut self, key: Option<K>, value: V) {
        let given = self.given.pop().unwrap();

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

        if given.is_some_and(|given| given != value) {
            self.changed = true;
        }

        self.approximations.insert(key, (value, self.round));
    }
}
