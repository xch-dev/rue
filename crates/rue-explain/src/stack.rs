//! Programs can be nested arbitrarily deeply, and so can what they're decompiled into, so the
//! recursive passes over them (including dropping and cloning) grow the stack as they need to.

use std::fmt;
use std::ops::{Deref, DerefMut};

/// How much stack must be left for a step of a recursive pass to run on the current stack.
const RED_ZONE: usize = 256 * 1024;

/// How much stack to allocate when there isn't enough left.
const STACK_SIZE: usize = 4 * 1024 * 1024;

/// Runs a step of a recursive pass, on a new stack (allocated on the heap) if the current one is
/// running out.
#[inline]
pub(crate) fn grow_stack<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(RED_ZONE, STACK_SIZE, f)
}

/// A box for a part of a recursive structure, which is cloned and dropped on a stack that's grown
/// as needed, so that a deeply nested structure can be.
pub(crate) struct Deep<T>(Option<Box<T>>);

impl<T> Deep<T> {
    pub(crate) fn new(value: T) -> Self {
        Self(Some(Box::new(value)))
    }

    pub(crate) fn into_inner(mut self) -> T {
        *self.0.take().expect("a deep value is only taken once")
    }
}

impl<T> Deref for Deep<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0.as_ref().expect("a deep value is only taken once")
    }
}

impl<T> DerefMut for Deep<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.as_mut().expect("a deep value is only taken once")
    }
}

impl<T: Clone> Clone for Deep<T> {
    fn clone(&self) -> Self {
        grow_stack(|| Self(self.0.clone()))
    }
}

impl<T> Drop for Deep<T> {
    fn drop(&mut self) {
        if let Some(value) = self.0.take() {
            grow_stack(|| drop(value));
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Deep<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        grow_stack(|| self.0.fmt(f))
    }
}
