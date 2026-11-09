//! Counts the steps taken by the recursive operations on types, so that tests can assert how the
//! work grows with the size of the types. A regression that makes the work exponential then fails
//! quickly, rather than leaving the test running until it times out.
//!
//! The counter only exists with the `step-counter` feature, so this is free otherwise.

#[cfg(feature = "step-counter")]
std::thread_local! {
    static STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Records a step. Called once per call of each recursive operation.
#[inline]
pub(crate) fn step() {
    #[cfg(feature = "step-counter")]
    STEPS.with(|steps| steps.set(steps.get() + 1));
}

/// Returns the number of steps taken on this thread by the given function.
#[cfg(feature = "step-counter")]
pub fn count_steps<T>(f: impl FnOnce() -> T) -> (T, usize) {
    let before = STEPS.with(std::cell::Cell::get);
    let value = f();
    let after = STEPS.with(std::cell::Cell::get);
    (value, after - before)
}
