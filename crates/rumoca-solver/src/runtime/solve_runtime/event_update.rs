use crate::{EventPreMode, EventPreSources, event_eval_params_for_row_pre_mode};
use rumoca_ir_solve as solve;

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
    /// Re-evaluate the rows whose meaning changes after `initial()` is cleared.
    ///
    /// A periodic clock that ticks at the simulation start owns rows at that
    /// same instant. Unowned Fixed/EventEntry rows remain part of the completed
    /// initialization event and must not execute a second time.
    PostInitialClockTick,
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
            Self::PostInitialClockTick => clock_owned || mode == EventPreMode::FollowCurrent,
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
    pub root_relation_overrides: &'a [(usize, f64)],
}

pub(super) struct DiscretePreSnapshot<'a> {
    pub(super) event_pre_y: &'a [f64],
    pub(super) event_pre_p: &'a [f64],
    pub(super) iter_pre_y: &'a [f64],
    pub(super) iter_pre_p: &'a [f64],
    pub(super) row_filter: EventUpdateRowFilter,
    pub(super) root_relation_overrides: &'a [(usize, f64)],
    pub(super) event_iteration: usize,
}

impl<'a> DiscretePreSnapshot<'a> {
    pub(super) fn event_pre_sources(&self) -> EventPreSources<'a> {
        EventPreSources {
            event_pre_y: self.event_pre_y,
            event_pre_p: self.event_pre_p,
            iter_pre_y: self.iter_pre_y,
            iter_pre_p: self.iter_pre_p,
            event_iteration: self.event_iteration,
        }
    }
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
    pub(super) tol: f64,
}

#[derive(Default)]
pub(super) struct EventEvalParamCache {
    event_entry: Option<Vec<f64>>,
    fixed: Option<Vec<f64>>,
    follow_current: Option<Vec<f64>>,
}

impl EventEvalParamCache {
    pub(super) fn params<'a>(
        &'a mut self,
        model: &solve::SolveModel,
        base_p: &[f64],
        mode: EventPreMode,
        sources: &EventPreSources<'_>,
        t: f64,
        tol: f64,
    ) -> &'a [f64] {
        let slot = match mode {
            EventPreMode::EventEntry => &mut self.event_entry,
            EventPreMode::Fixed => &mut self.fixed,
            EventPreMode::FollowCurrent => &mut self.follow_current,
        };
        slot.get_or_insert_with(|| {
            event_eval_params_for_row_pre_mode(model, base_p, mode, sources, t, tol)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_clock_tick_filters_follow_typed_ownership() {
        assert!(
            EventUpdateRowFilter::UnownedOnly.accepts(EventPreMode::EventEntry, false),
            "an unowned event-entry row remains part of initialization"
        );
        assert!(
            !EventUpdateRowFilter::UnownedOnly.accepts(EventPreMode::EventEntry, true),
            "the typed clock row waits for its first superdense tick"
        );
        assert!(
            EventUpdateRowFilter::PostInitialClockTick.accepts(EventPreMode::EventEntry, true),
            "the typed clock row executes after initial() clears"
        );
        assert!(
            !EventUpdateRowFilter::PostInitialClockTick.accepts(EventPreMode::EventEntry, false),
            "an unowned event-entry row cannot execute a second time"
        );
        assert!(
            EventUpdateRowFilter::PostInitialClockTick.accepts(EventPreMode::FollowCurrent, false),
            "the established post-initial refresh remains active"
        );
        assert!(
            !EventUpdateRowFilter::PostInitialClockTick.accepts(EventPreMode::Fixed, false),
            "fixed initialization rows remain settled"
        );
    }
}
