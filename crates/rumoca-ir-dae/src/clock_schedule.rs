//! Exact rational view of [`ClockSchedule`] (MLS §16).
//!
//! `ClockSchedule` stores tick timing in seconds because that is what the
//! numeric scheduler consumes, but MLS §16.5 makes clock identity an exact
//! integer relation over a base clock. These accessors recover the reduced
//! rational lattice behind the seconds so identity and tick simultaneity can be
//! decided exactly instead of with a floating-point epsilon.

use rumoca_core::{ClockLattice, ClockLatticeErrorKind, Span};

use crate::ClockSchedule;

impl ClockSchedule {
    /// Build a schedule from an exact lattice, rounding to seconds once.
    pub fn from_lattice(lattice: ClockLattice, source_span: Span) -> Self {
        Self {
            period_seconds: lattice.period_seconds(),
            phase_seconds: lattice.phase_seconds(),
            source_span,
        }
    }

    /// The exact lattice behind this schedule.
    pub fn lattice(&self) -> Result<ClockLattice, ClockLatticeErrorKind> {
        ClockLattice::from_seconds(self.period_seconds, self.phase_seconds)
    }

    /// MLS §16.5 clock identity: same base period and same phase, decided by
    /// exact rational equality rather than a seconds epsilon.
    pub fn is_same_clock(&self, other: &Self) -> bool {
        match (self.lattice(), other.lattice()) {
            (Ok(lhs), Ok(rhs)) => lhs.is_same_clock(rhs),
            // Clocks with no rational form cannot be proven identical.
            _ => false,
        }
    }

    /// Whether both schedules activate at exactly the same instants.
    ///
    /// A schedule with no rational form answers `false`: the lattice cannot
    /// prove the clocks coincide, so callers keep them distinct.
    pub fn ticks_simultaneously_with(&self, other: &Self) -> bool {
        match (self.lattice(), other.lattice()) {
            (Ok(lhs), Ok(rhs)) => matches!(lhs.ticks_simultaneously_with(rhs), Ok(true)),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClockRational, SourceId};

    fn span() -> Span {
        Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
    }

    fn lattice(period: (i64, i64), phase: (i64, i64)) -> ClockLattice {
        ClockLattice::new(
            ClockRational::new(period.0, period.1).expect("period reduces"),
            ClockRational::new(phase.0, phase.1).expect("phase reduces"),
        )
        .expect("positive period")
    }

    #[test]
    fn schedules_from_equal_lattices_are_the_same_clock() {
        let derived = ClockSchedule::from_lattice(lattice((3, 10), (0, 1)), span());
        let declared = ClockSchedule {
            period_seconds: 0.3,
            phase_seconds: 0.0,
            source_span: span(),
        };

        // The naive f64 composition 0.1 * 3 is a different double from 0.3.
        assert_ne!(0.1_f64 * 3.0, derived.period_seconds);
        assert_eq!(derived.period_seconds, declared.period_seconds);
        assert!(derived.is_same_clock(&declared));
    }

    #[test]
    fn distinct_clocks_are_not_merged() {
        let fast = ClockSchedule::from_lattice(lattice((1, 10), (0, 1)), span());
        let slow = ClockSchedule::from_lattice(lattice((1, 5), (0, 1)), span());

        assert!(!fast.is_same_clock(&slow));
        assert!(!fast.ticks_simultaneously_with(&slow));
    }

    #[test]
    fn whole_period_phase_offsets_tick_together() {
        let base = ClockSchedule::from_lattice(lattice((1, 10), (0, 1)), span());
        let shifted = ClockSchedule::from_lattice(lattice((1, 10), (2, 10)), span());

        assert!(!base.is_same_clock(&shifted));
        assert!(base.ticks_simultaneously_with(&shifted));
    }

    #[test]
    fn half_period_phase_offsets_do_not_tick_together() {
        let base = ClockSchedule::from_lattice(lattice((1, 10), (0, 1)), span());
        let shifted = ClockSchedule::from_lattice(lattice((1, 10), (1, 20)), span());

        assert!(!base.ticks_simultaneously_with(&shifted));
    }

    #[test]
    fn non_finite_schedules_have_no_lattice() {
        let broken = ClockSchedule {
            period_seconds: f64::NAN,
            phase_seconds: 0.0,
            source_span: span(),
        };

        assert_eq!(
            broken.lattice(),
            Err(ClockLatticeErrorKind::NonFiniteSeconds)
        );
        assert!(!broken.is_same_clock(&broken));
    }
}
