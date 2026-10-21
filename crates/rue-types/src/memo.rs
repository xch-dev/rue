use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    mem,
};

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
