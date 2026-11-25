use std::cell::Cell;

/// The accepted integration steps and located roots of this thread's
/// simulations since [`reset`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct HotpathStatsSnapshot {
    pub solver_steps: u64,
    pub root_hits: u64,
    /// The integration method the most recent simulation ran.
    pub integrator: Option<&'static str>,
}

/// Counts of the origin linearizations of affine blocks on this thread.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct AffineLinearizationCounts {
    /// Linearizations formed: a block's first, and each one whose reuse key
    /// no longer held.
    pub formed: u64,
    /// Linearizations a retained one stood for, its reuse key still holding.
    pub reused: u64,
}

thread_local! {
    static AFFINE: Cell<AffineLinearizationCounts> =
        const { Cell::new(AffineLinearizationCounts { formed: 0, reused: 0 }) };
    static SOLVER_STEPS: Cell<u64> = const { Cell::new(0) };
    static ROOT_HITS: Cell<u64> = const { Cell::new(0) };
    static INTEGRATOR: Cell<Option<&'static str>> = const { Cell::new(None) };
}

/// Zero this thread's step and root counts.
pub fn reset() {
    SOLVER_STEPS.set(0);
    ROOT_HITS.set(0);
    INTEGRATOR.set(None);
}

/// This thread's step and root counts since [`reset`].
#[must_use]
pub fn snapshot() -> HotpathStatsSnapshot {
    HotpathStatsSnapshot {
        solver_steps: SOLVER_STEPS.get(),
        root_hits: ROOT_HITS.get(),
        integrator: INTEGRATOR.get(),
    }
}

/// Coupled blocks this thread projected through the dense block Newton after
/// their constructor tearing declined, since [`reset_torn_declines`]: the
/// torn-to-dense fallbacks the projection fallback counts record.
#[must_use]
pub fn torn_declines() -> u64 {
    super::fallbacks::projection_fallbacks()
        .sites
        .values()
        .map(|counts| counts.count(super::fallbacks::ProjectionFallback::TornToDense))
        .sum()
}

/// Reset the projection fallback counts [`torn_declines`] reads.
pub fn reset_torn_declines() {
    super::fallbacks::reset_projection_fallbacks();
}

/// The affine linearization counts on this thread since the last
/// [`reset_affine_linearization_counts`].
#[must_use]
pub fn affine_linearization_counts() -> AffineLinearizationCounts {
    AFFINE.get()
}

/// Zero this thread's affine linearization counts.
pub fn reset_affine_linearization_counts() {
    AFFINE.set(AffineLinearizationCounts::default());
}

pub(crate) fn note_affine_formed() {
    let mut counts = AFFINE.get();
    counts.formed += 1;
    AFFINE.set(counts);
}

pub(crate) fn note_affine_reused() {
    let mut counts = AFFINE.get();
    counts.reused += 1;
    AFFINE.set(counts);
}

/// Record the integration method a simulation on this thread is about to run.
pub fn note_integrator(method: &'static str) {
    INTEGRATOR.set(Some(method));
}

pub(crate) fn inc_solver_step() {
    SOLVER_STEPS.set(SOLVER_STEPS.get() + 1);
}

pub(crate) fn inc_root_hit() {
    ROOT_HITS.set(ROOT_HITS.get() + 1);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_accumulate_per_thread_until_reset() {
        reset();
        inc_solver_step();
        inc_solver_step();
        inc_root_hit();
        note_integrator("bdf");
        assert_eq!(snapshot().integrator, Some("bdf"));
        INTEGRATOR.set(None);
        let other = std::thread::spawn(snapshot).join().expect("thread");
        assert_eq!(other, HotpathStatsSnapshot::default());
        assert_eq!(
            snapshot(),
            HotpathStatsSnapshot {
                solver_steps: 2,
                root_hits: 1,
                integrator: None,
            }
        );
        reset();
        assert_eq!(snapshot(), HotpathStatsSnapshot::default());
    }
}
