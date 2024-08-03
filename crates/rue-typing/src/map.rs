use std::hash::BuildHasherDefault;

use ahash::AHasher;

pub type HashMap<K, V> = hashbrown::HashMap<K, V, BuildHasherDefault<AHasher>>;
pub type HashSet<K> = hashbrown::HashSet<K, BuildHasherDefault<AHasher>>;
