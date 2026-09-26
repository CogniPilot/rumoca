use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct HotpathStatsSnapshot {
    pub solver_steps: u64,
    pub root_hits: u64,
}

static SOLVER_STEPS: AtomicU64 = AtomicU64::new(0);
static ROOT_HITS: AtomicU64 = AtomicU64::new(0);

/// Enabled when the `rumoca_solver::hotpath` trace target is active (i.e. under
/// `--trace=rumoca_solver::hotpath` in a tracing-enabled build).
fn enabled() -> bool {
    tracing::enabled!(target: "rumoca_solver::hotpath", tracing::Level::DEBUG)
}

fn reset_counter(counter: &AtomicU64) {
    counter.store(0, Ordering::Relaxed);
}

fn bump(counter: &AtomicU64) {
    if enabled() {
        counter.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn reset() {
    if !enabled() {
        return;
    }
    for counter in [&SOLVER_STEPS, &ROOT_HITS] {
        reset_counter(counter);
    }
}

pub fn snapshot() -> Option<HotpathStatsSnapshot> {
    if !enabled() {
        return None;
    }
    Some(HotpathStatsSnapshot {
        solver_steps: SOLVER_STEPS.load(Ordering::Relaxed),
        root_hits: ROOT_HITS.load(Ordering::Relaxed),
    })
}

thread_local! {
    static TORN_DECLINES: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// Coupled blocks this thread projected through the dense block Newton after
/// their constructor tearing declined, since [`reset_torn_declines`].
#[must_use]
pub fn torn_declines() -> u64 {
    TORN_DECLINES.with(std::cell::Cell::get)
}

pub fn reset_torn_declines() {
    TORN_DECLINES.with(|count| count.set(0));
}

pub(crate) fn inc_torn_decline() {
    TORN_DECLINES.with(|count| count.set(count.get() + 1));
}

pub(crate) fn inc_solver_step() {
    bump(&SOLVER_STEPS);
}

pub(crate) fn inc_root_hit() {
    bump(&ROOT_HITS);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshot_disabled_without_trace_subscriber() {
        // No tracing subscriber is installed in unit tests, so the hotpath
        // target is disabled and stats collection stays off.
        reset();
        assert_eq!(snapshot(), None);
    }
}
