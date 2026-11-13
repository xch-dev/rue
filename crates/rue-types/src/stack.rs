//! Types can be nested as deeply as a list literal is long, since its type is a chain of pairs
//! with one pair for each item, so the recursive operations on types can go arbitrarily deep.
//!
//! Every cycle of recursive calls must pass through [`grow_stack`]. The recursive functions that
//! the cycles go through (and the ones that call themselves directly) call it, then run their
//! bodies, which are in a function of the same name with an `_inner` suffix.

/// How much stack must be left for a recursive call to run on the current stack. This has to cover
/// the frames between two calls to [`grow_stack`], including in a debug build.
const RED_ZONE: usize = 256 * 1024;

/// How much stack to allocate when there isn't enough left.
const STACK_SIZE: usize = 4 * 1024 * 1024;

/// Runs `f`, on a new stack allocated on the heap if the current one is running out, so that a
/// deeply nested type doesn't overflow it.
#[inline]
pub(crate) fn grow_stack<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(RED_ZONE, STACK_SIZE, f)
}
