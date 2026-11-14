//! Hash maps and sets for the operations on types, which are keyed by ids and hashed on almost every
//! step (by the stacks of items in progress and the memo tables). They use the hasher from
//! `rustc-hash` rather than the standard library's, which resists collision attacks that don't
//! apply to ids that the compiler allocates, but is much slower to hash them.

use indexmap::{IndexMap, IndexSet};
use rustc_hash::FxBuildHasher;

pub(crate) use rustc_hash::{FxHashMap, FxHashSet};

pub(crate) type FxIndexSet<T> = IndexSet<T, FxBuildHasher>;
pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;
