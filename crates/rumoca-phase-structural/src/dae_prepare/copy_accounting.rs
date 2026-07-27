//! Thread-local accounting for whole-DAE deep copies and full scalarization
//! passes.
//!
//! [`rumoca_ir_dae::Dae`] is `Clone` with no `Arc` sharing and
//! [`rumoca_core::Expression`] is boxed per node, so every whole-DAE copy
//! reallocates the entire expression graph. A structural pass that copies the
//! DAE to stage a rewrite is therefore one of the dominant costs of lowering a
//! large model, and reintroducing a copy that an earlier change removed is
//! invisible: nothing observable changes except wall time.
//!
//! The passes that make those copies record them here, so a caller can measure
//! how many copies a whole lowering funnel actually performed instead of
//! hand-maintaining a list of the copy sites it happens to know about. The
//! counters are per-thread, so tests that run concurrently never observe each
//! other's copies.
//!
//! This is instrumentation only: recording is a `Cell` increment on a
//! thread-local and never changes what a pass computes.

use std::cell::Cell;

use rumoca_ir_dae as dae;

/// Whole-DAE deep copies and full `scalarize_equations` passes recorded on the
/// current thread.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DaeCopyCounts {
    /// Whole-`Dae` deep copies.
    pub dae_clones: u32,
    /// Full `scalarize_equations` passes over a whole `Dae`.
    pub scalarizations: u32,
}

impl DaeCopyCounts {
    /// Copies recorded since `baseline` was taken.
    #[must_use]
    pub fn since(self, baseline: Self) -> Self {
        Self {
            dae_clones: self.dae_clones.saturating_sub(baseline.dae_clones),
            scalarizations: self.scalarizations.saturating_sub(baseline.scalarizations),
        }
    }
}

thread_local! {
    static COUNTS: Cell<DaeCopyCounts> = const { Cell::new(DaeCopyCounts {
        dae_clones: 0,
        scalarizations: 0,
    }) };
}

/// Copies recorded on this thread so far.
///
/// Take one of these before a lowering funnel runs and another after it, then
/// use [`DaeCopyCounts::since`] to get that funnel's own totals.
#[must_use]
pub fn counts() -> DaeCopyCounts {
    COUNTS.with(Cell::get)
}

/// Record one whole-DAE deep copy.
///
/// Prefer [`clone_dae`]; use this only where the copy is produced by something
/// other than a direct `Dae::clone` call (for example a nested pass whose own
/// module cannot record it).
pub fn record_dae_clone() {
    COUNTS.with(|counts| {
        let mut current = counts.get();
        current.dae_clones = current.dae_clones.saturating_add(1);
        counts.set(current);
    });
}

/// Record one full `scalarize_equations` pass over a whole DAE.
pub fn record_scalarization() {
    COUNTS.with(|counts| {
        let mut current = counts.get();
        current.scalarizations = current.scalarizations.saturating_add(1);
        counts.set(current);
    });
}

/// Deep-copy a DAE and record the copy.
#[must_use]
pub fn clone_dae(dae: &dae::Dae) -> dae::Dae {
    record_dae_clone();
    dae.clone()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_are_deltas_against_a_baseline() {
        let baseline = counts();
        let mut dae = dae::Dae::new();
        dae.variables.states.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable::new(
                rumoca_core::VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name("copy_accounting_test.mo"),
                    3,
                    4,
                ),
            ),
        );

        let copy = clone_dae(&dae);
        record_scalarization();

        assert_eq!(copy.variables.states.len(), 1);
        assert_eq!(
            counts().since(baseline),
            DaeCopyCounts {
                dae_clones: 1,
                scalarizations: 1,
            }
        );
    }
}
