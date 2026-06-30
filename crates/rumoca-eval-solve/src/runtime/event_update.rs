use rumoca_ir_solve as solve;
use rumoca_solver::{EventPreMode, EventPreSources, event_eval_params_for_row_pre_mode};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventUpdateRowFilter {
    All,
    FollowCurrentOnly,
}

impl EventUpdateRowFilter {
    pub(super) fn accepts(self, mode: EventPreMode) -> bool {
        matches!(self, Self::All)
            || matches!(
                (self, mode),
                (Self::FollowCurrentOnly, EventPreMode::FollowCurrent)
            )
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
        tol: f64,
    ) -> &'a [f64] {
        let slot = match mode {
            EventPreMode::EventEntry => &mut self.event_entry,
            EventPreMode::Fixed => &mut self.fixed,
            EventPreMode::FollowCurrent => &mut self.follow_current,
        };
        slot.get_or_insert_with(|| {
            event_eval_params_for_row_pre_mode(model, base_p, mode, sources, tol)
        })
    }
}
