//! Instrumentation for tests of the recursive operations on types.
//!
//! The step counter lets tests assert how the work grows with the size of the types. A regression
//! that makes the work exponential then fails quickly, rather than leaving the test running until
//! it times out. Turning off the comparison memo lets tests check that it doesn't change results,
//! with a step limit since the work can then be exponential.
//!
//! These only exist with the `step-counter` feature, so they're free otherwise.

#[cfg(feature = "step-counter")]
std::thread_local! {
    static STEPS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static STEP_LIMIT: std::cell::Cell<usize> = const { std::cell::Cell::new(usize::MAX) };
    static COMPARE_MEMO_OFF: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Records a step. Called once per call of each recursive operation.
#[inline]
pub(crate) fn step() {
    #[cfg(feature = "step-counter")]
    {
        let steps = STEPS.with(|steps| {
            steps.set(steps.get() + 1);
            steps.get()
        });

        assert!(
            steps <= STEP_LIMIT.with(std::cell::Cell::get),
            "step limit exceeded"
        );
    }
}

/// Returns the number of steps taken on this thread by the given function.
#[cfg(feature = "step-counter")]
pub fn count_steps<T>(f: impl FnOnce() -> T) -> (T, usize) {
    let before = STEPS.with(std::cell::Cell::get);
    let value = f();
    let after = STEPS.with(std::cell::Cell::get);
    (value, after - before)
}

/// Runs the given function on this thread, panicking if it takes more than the given number of
/// steps.
#[cfg(feature = "step-counter")]
pub fn with_step_limit<T>(limit: usize, f: impl FnOnce() -> T) -> T {
    let start = STEPS.with(std::cell::Cell::get);
    let _restore = Restore::new(&STEP_LIMIT, start.saturating_add(limit));
    f()
}

/// Whether comparisons may reuse the results of earlier ones.
#[inline]
pub(crate) fn compare_memo_enabled() -> bool {
    #[cfg(feature = "step-counter")]
    return !COMPARE_MEMO_OFF.with(std::cell::Cell::get);

    #[cfg(not(feature = "step-counter"))]
    true
}

/// Runs the given function on this thread without reusing the results of earlier comparisons.
#[cfg(feature = "step-counter")]
pub fn without_compare_memo<T>(f: impl FnOnce() -> T) -> T {
    let _restore = Restore::new(&COMPARE_MEMO_OFF, true);
    f()
}

/// Sets a thread-local value, and restores the previous one when dropped (even if `f` panics).
#[cfg(feature = "step-counter")]
struct Restore<T: Copy + 'static> {
    key: &'static std::thread::LocalKey<std::cell::Cell<T>>,
    previous: T,
}

#[cfg(feature = "step-counter")]
impl<T: Copy + 'static> Restore<T> {
    fn new(key: &'static std::thread::LocalKey<std::cell::Cell<T>>, value: T) -> Self {
        let previous = key.with(|cell| cell.replace(value));
        Self { key, previous }
    }
}

#[cfg(feature = "step-counter")]
impl<T: Copy + 'static> Drop for Restore<T> {
    fn drop(&mut self) {
        self.key.with(|cell| cell.set(self.previous));
    }
}
