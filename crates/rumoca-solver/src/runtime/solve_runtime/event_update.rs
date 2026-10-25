use rumoca_ir_solve as solve;

use crate::EventPreMode;
use crate::runtime::solve_events::event_eval_params_with_relation_overrides;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventUpdateRowFilter {
    All,
    FollowCurrentOnly,
    /// Settle rows without a typed clock owner.
    ///
    /// This separates two owners at coincident event boundaries. Initialization
    /// uses it before the phase-zero clock tick; the runtime uses it at a
    /// coincident root's right limit after clock-owned rows have already run at
    /// the semantic tick. In both cases typed ownership, not row position,
    /// decides which rows execute.
    UnownedOnly,
    /// No discrete row is re-evaluated; only the continuous projection moves.
    ///
    /// MLS Appendix B changes discrete values, conditions, and relation memory
    /// only at event instants. A projection evaluated strictly after an event
    /// instant — the synthetic right limit of the initial event — is therefore
    /// not allowed to redefine any of them.
    Hold,
}

impl EventUpdateRowFilter {
    pub(super) fn accepts(self, mode: EventPreMode, clock_owned: bool) -> bool {
        match self {
            Self::All => true,
            Self::FollowCurrentOnly => mode == EventPreMode::FollowCurrent,
            Self::UnownedOnly => !clock_owned,
            Self::Hold => false,
        }
    }
}

pub struct ProjectedEventUpdateInput<'a> {
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub t: f64,
    pub tol: f64,
    pub event_pre_y: &'a [f64],
    pub event_pre_p: &'a [f64],
    pub max_iters: usize,
    pub row_filter: EventUpdateRowFilter,
    pub root_relation_overrides: &'a mut Vec<(usize, f64)>,
}

pub(super) struct DiscretePreSnapshot<'a> {
    pub(super) row_filter: EventUpdateRowFilter,
    pub(super) root_relation_overrides: &'a [(usize, f64)],
    pub(super) event_iteration: usize,
}

pub(super) struct DiscreteRowsSettleInput<'a> {
    pub(super) y: &'a mut [f64],
    pub(super) p: &'a mut [f64],
    pub(super) t: f64,
    pub(super) tol: f64,
    pub(super) max_iters: usize,
}

pub(super) struct DiscreteRowEvalInput<'a, 'snapshot> {
    pub(super) snapshot: &'a DiscretePreSnapshot<'snapshot>,
    pub(super) row_idx: usize,
    pub(super) eval_y: &'a [f64],
    pub(super) eval_p: &'a [f64],
    pub(super) t: f64,
}

#[derive(Default)]
pub(super) struct EventEvalParamCache {
    pub(super) program: Option<usize>,
    pub(super) outputs: Vec<f64>,
    pub(super) guarded_program: Option<usize>,
    pub(super) guarded_outputs: Vec<f64>,
    pub(super) overridden: Option<OverriddenParams>,
}

/// `P` with one snapshot's root-relation overrides applied, and the base it was
/// built from (its address and length).
pub(super) struct OverriddenParams {
    base: (usize, usize),
    values: Vec<f64>,
}

/// The `P` one event row evaluates under `overrides`.
///
/// Without overrides this is `base_p` itself. With them, every row of one pass
/// reads the same overridden copy of the pass-entry `P`, so the copy is built
/// once per cache instead of once per row; a different base rebuilds it.
pub(super) fn overridden_event_params<'a>(
    overridden: &'a mut Option<OverriddenParams>,
    targets: &[Option<solve::ScalarSlot>],
    overrides: &[(usize, f64)],
    base_p: &'a [f64],
) -> Result<&'a [f64], crate::RuntimeSolveError> {
    if overrides.is_empty() {
        return Ok(base_p);
    }
    let base = (base_p.as_ptr().addr(), base_p.len());
    if overridden.as_ref().is_none_or(|cached| cached.base != base) {
        *overridden = Some(OverriddenParams {
            base,
            values: event_eval_params_with_relation_overrides(targets, overrides, base_p)?,
        });
    }
    Ok(overridden
        .as_ref()
        .map_or(base_p, |cached| cached.values.as_slice()))
}

impl EventEvalParamCache {
    pub(super) fn params<'a>(&mut self, base_p: &'a [f64]) -> &'a [f64] {
        // The live P vector is the canonical heterogeneous temporal view:
        // ordinary z/m pre lanes advance only between whole passes, while
        // continuous pre and clocked previous lanes remain at event entry.
        base_p
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn root_overrides_are_applied_once_per_base() {
        let targets = [
            Some(solve::ScalarSlot::P {
                index: 1,
                byte_offset: 8,
            }),
            None,
        ];
        let base = [10.0, 11.0, 12.0];
        let mut overridden = None;
        let plain = overridden_event_params(&mut overridden, &targets, &[], &base).unwrap();
        assert_eq!(
            plain.as_ptr(),
            base.as_ptr(),
            "no overrides reads the base itself"
        );
        assert!(overridden.is_none());
        let overrides = [(0, 1.0), (1, 5.0)];
        let first = overridden_event_params(&mut overridden, &targets, &overrides, &base)
            .unwrap()
            .to_vec();
        assert_eq!(first, [10.0, 1.0, 12.0]);
        let built = overridden.as_ref().unwrap().values.as_ptr();
        let again = overridden_event_params(&mut overridden, &targets, &overrides, &base).unwrap();
        assert_eq!(
            again.as_ptr(),
            built,
            "a second row of the pass reuses the copy"
        );
        let other = [20.0, 21.0, 22.0];
        let rebuilt = overridden_event_params(&mut overridden, &targets, &overrides, &other)
            .unwrap()
            .to_vec();
        assert_eq!(
            rebuilt,
            [20.0, 1.0, 22.0],
            "a different base is never served stale"
        );
    }

    #[test]
    fn initialization_defers_clock_owned_rows_until_the_first_tick() {
        assert!(
            EventUpdateRowFilter::UnownedOnly.accepts(EventPreMode::EventEntry, false),
            "an unowned event-entry row remains part of initialization"
        );
        assert!(
            !EventUpdateRowFilter::UnownedOnly.accepts(EventPreMode::EventEntry, true),
            "the typed clock row waits for its first superdense tick"
        );
    }
}
