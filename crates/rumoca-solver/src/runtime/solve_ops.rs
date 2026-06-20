use rumoca_ir_solve as solve;

use crate::{
    SimResult, SimTermination, SimVariableMeta, runtime::pre_params::write_pre_params_from_sources,
};

#[derive(Debug, thiserror::Error)]
pub enum RuntimeSolveError {
    #[error(
        "solve-IR evaluation failed: {message}{}",
        span_suffix(*.span)
    )]
    SolveIr {
        message: String,
        span: Option<rumoca_core::Span>,
    },

    #[error("unsupported solve-IR runtime model: {reason}")]
    UnsupportedModel { reason: String },

    #[error("non-finite derivative evaluation for state '{state_name}'")]
    NonFiniteDerivative { state_name: String },

    #[error(
        "non-finite ({kind}) value computed for `{name}`{}",
        span_suffix(*.span)
    )]
    NonFiniteValue {
        name: String,
        kind: &'static str,
        span: Option<rumoca_core::Span>,
    },
}

fn span_suffix(span: Option<rumoca_core::Span>) -> String {
    match span {
        Some(span) => format!(" @ {span:?}"),
        None => String::new(),
    }
}

impl RuntimeSolveError {
    pub fn solve_ir(message: impl Into<String>) -> Self {
        Self::solve_ir_with_span(message, None)
    }

    pub fn solve_ir_with_span(message: impl Into<String>, span: Option<rumoca_core::Span>) -> Self {
        Self::SolveIr {
            message: message.into(),
            span,
        }
    }

    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::SolveIr { span, .. } | Self::NonFiniteValue { span, .. } => *span,
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EventActionOutcome {
    Continue,
    AssertionFailed { time: f64, message: String },
    Terminated { time: f64, message: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventPreMode {
    /// Use the pre snapshot captured at the start of the current event.
    EventEntry,
    /// Keep pre slots fixed within one event-iteration pass.
    Fixed,
    /// Read the current fixed-point values while evaluating the row.
    FollowCurrent,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RootCrossing {
    pub index: usize,
    pub post_relation_memory_value: f64,
}

impl EventPreMode {
    pub fn merge(self, other: Self) -> Self {
        match (self, other) {
            (Self::EventEntry, _) | (_, Self::EventEntry) => Self::EventEntry,
            (Self::Fixed, _) | (_, Self::Fixed) => Self::Fixed,
            (Self::FollowCurrent, Self::FollowCurrent) => Self::FollowCurrent,
        }
    }
}

impl From<solve::DiscreteEventPreMode> for EventPreMode {
    fn from(value: solve::DiscreteEventPreMode) -> Self {
        match value {
            solve::DiscreteEventPreMode::EventEntry => Self::EventEntry,
            solve::DiscreteEventPreMode::Fixed => Self::Fixed,
            solve::DiscreteEventPreMode::FollowCurrent => Self::FollowCurrent,
        }
    }
}

pub fn push_visible_values(data: &mut [Vec<f64>], values: &[f64]) -> Result<(), RuntimeSolveError> {
    if data.len() != values.len() {
        return Err(RuntimeSolveError::solve_ir(
            "visible trace storage does not match solve layout",
        ));
    }
    for (series, value) in data.iter_mut().zip(values.iter().copied()) {
        series.push(value);
    }
    Ok(())
}

pub fn replace_last_visible_values(
    data: &mut [Vec<f64>],
    values: &[f64],
) -> Result<(), RuntimeSolveError> {
    if data.len() != values.len() {
        return Err(RuntimeSolveError::solve_ir(
            "visible trace storage does not match solve layout",
        ));
    }
    for (series, value) in data.iter_mut().zip(values.iter().copied()) {
        let Some(slot) = series.last_mut() else {
            return Err(RuntimeSolveError::solve_ir(
                "visible trace value replacement requires an existing sample",
            ));
        };
        *slot = value;
    }
    Ok(())
}

pub fn discrete_row_pre_mode(model: &solve::SolveModel, row_idx: usize) -> EventPreMode {
    model
        .problem
        .discrete
        .pre_modes
        .get(row_idx)
        .copied()
        .map(EventPreMode::from)
        .unwrap_or(EventPreMode::FollowCurrent)
}

pub fn row_reads_solver_or_time(row: &[solve::LinearOp]) -> bool {
    row.iter().any(|op| {
        matches!(
            op,
            solve::LinearOp::LoadY { .. }
                | solve::LinearOp::LoadTime { .. }
                | solve::LinearOp::TableLookup { .. }
                | solve::LinearOp::TableLookupSlope { .. }
                | solve::LinearOp::TableNextEvent { .. }
        )
    })
}

pub fn event_eval_params_for_pre_mode(
    model: &solve::SolveModel,
    base_p: &[f64],
    pre_y: &[f64],
    pre_p: &[f64],
    tol: f64,
) -> Vec<f64> {
    let mut eval_p = base_p.to_vec();
    write_pre_params_from_sources(model, pre_y, pre_p, &mut eval_p, tol);
    eval_p
}

/// Candidate pre snapshots available while evaluating one event-iteration row.
pub struct EventPreSources<'a> {
    /// Snapshot captured at the start of the event.
    pub event_pre_y: &'a [f64],
    /// Parameter snapshot captured at the start of the event.
    pub event_pre_p: &'a [f64],
    /// Snapshot captured before the current event-iteration pass.
    pub iter_pre_y: &'a [f64],
    /// Parameter snapshot captured before the current event-iteration pass.
    pub iter_pre_p: &'a [f64],
    /// Zero-based event-iteration pass number.
    pub event_iteration: usize,
}

pub fn event_eval_params_for_row_pre_mode(
    model: &solve::SolveModel,
    base_p: &[f64],
    mode: EventPreMode,
    sources: &EventPreSources<'_>,
    tol: f64,
) -> Vec<f64> {
    let (pre_y, pre_p) = event_pre_sources_for_mode(mode, sources);
    event_eval_params_for_pre_mode(model, base_p, pre_y, pre_p, tol)
}

fn event_pre_sources_for_mode<'a>(
    mode: EventPreMode,
    sources: &'a EventPreSources<'_>,
) -> (&'a [f64], &'a [f64]) {
    match mode {
        EventPreMode::EventEntry => (sources.event_pre_y, sources.event_pre_p),
        EventPreMode::Fixed if sources.event_iteration == 0 => {
            (sources.event_pre_y, sources.event_pre_p)
        }
        EventPreMode::Fixed | EventPreMode::FollowCurrent => {
            (sources.iter_pre_y, sources.iter_pre_p)
        }
    }
}

pub fn convert_variable_meta(meta: &[solve::SolveVariableMeta]) -> Vec<SimVariableMeta> {
    meta.iter()
        .map(|item| SimVariableMeta {
            name: item.name.clone(),
            role: item.role.clone(),
            is_state: item.is_state,
            value_type: item.value_type.clone(),
            variability: item.variability.clone(),
            time_domain: item.time_domain.clone(),
            unit: item.unit.clone(),
            start: item.start.clone(),
            min: item.min.clone(),
            max: item.max.clone(),
            nominal: item.nominal.clone(),
            fixed: item.fixed,
            description: item.description.clone(),
        })
        .collect()
}

pub fn build_sim_result_from_solve_model(
    model: &solve::SolveModel,
    recorded_times: Vec<f64>,
    data: Vec<Vec<f64>>,
    termination: Option<SimTermination>,
) -> SimResult {
    SimResult {
        times: recorded_times,
        names: model.visible_names.clone(),
        data,
        n_states: model.state_scalar_count(),
        variable_meta: convert_variable_meta(&model.variable_meta),
        termination,
    }
}

pub fn root_crossed(before: &[f64], after: &[f64], tol: f64) -> bool {
    first_root_crossing(before, after, tol).is_some()
}

pub fn first_root_crossing(before: &[f64], after: &[f64], tol: f64) -> Option<RootCrossing> {
    root_crossings(before, after, tol).into_iter().next()
}

pub fn root_crossings(before: &[f64], after: &[f64], tol: f64) -> Vec<RootCrossing> {
    before
        .iter()
        .zip(after)
        .enumerate()
        .filter_map(|(index, (old, new))| root_crossing(index, *old, *new, tol))
        .collect()
}

pub fn root_crossings_with_relation_memory(
    before: &[f64],
    after: &[f64],
    tol: f64,
    root_relation_memory_targets: &[Option<solve::ScalarSlot>],
    params: &[f64],
) -> Vec<RootCrossing> {
    before
        .iter()
        .zip(after)
        .enumerate()
        .filter_map(|(index, (old, new))| {
            let target = root_relation_memory_targets.get(index).copied().flatten();
            if target.is_some() {
                if let Some(crossing) = relation_memory_root_crossing(index, *new, target, params) {
                    return Some(crossing);
                }
                if relation_memory_value_from_signed_root(*new).is_some() {
                    return None;
                }
                return signed_root_crossing(index, *old, *new, tol);
            }
            root_crossing(index, *old, *new, tol)
        })
        .collect()
}

pub fn root_value_crossed(before: f64, after: f64, tol: f64) -> bool {
    root_crossing(0, before, after, tol).is_some()
}

pub fn update_relation_memory_slots(
    roots: &[f64],
    p: &mut [f64],
    relation_memory_indices: &[usize],
) -> bool {
    let mut changed = false;
    for (root, &param_idx) in roots.iter().zip(relation_memory_indices.iter()) {
        if let Some(slot) = p.get_mut(param_idx) {
            let value = relation_memory_value_from_root(*root);
            changed |= (*slot - value).abs() > 0.0;
            *slot = value;
        }
    }
    changed
}

pub fn relation_memory_value_from_root(root: f64) -> f64 {
    if root < 0.0 { 1.0 } else { 0.0 }
}

fn signed_root_crossed(old: f64, new: f64, tol: f64) -> bool {
    let sign_changed = old.signum() != new.signum();
    if old.abs() > tol {
        return new.abs() <= tol || sign_changed;
    }
    new.abs() > tol && sign_changed
}

fn relation_toggled(old: f64, new: f64) -> bool {
    (old <= 0.5 && new > 0.5) || (old > 0.5 && new <= 0.5)
}

fn boolean_relation_value(value: f64, tol: f64) -> bool {
    value.abs() <= tol || (value - 1.0).abs() <= tol
}

fn relation_memory_root_crossing(
    index: usize,
    new: f64,
    target: Option<solve::ScalarSlot>,
    params: &[f64],
) -> Option<RootCrossing> {
    let Some(solve::ScalarSlot::P {
        index: param_idx, ..
    }) = target
    else {
        return None;
    };
    let current = *params.get(param_idx)?;
    let post = relation_memory_value_from_signed_root(new)?;
    relation_toggled(current, post).then_some(RootCrossing {
        index,
        post_relation_memory_value: post,
    })
}

fn relation_memory_value_from_signed_root(root: f64) -> Option<f64> {
    if root < 0.0 {
        Some(1.0)
    } else if root > 0.0 {
        Some(0.0)
    } else {
        None
    }
}

fn root_crossing(index: usize, old: f64, new: f64, tol: f64) -> Option<RootCrossing> {
    if boolean_relation_value(old, tol)
        && boolean_relation_value(new, tol)
        && relation_toggled(old, new)
    {
        return Some(RootCrossing {
            index,
            post_relation_memory_value: if new > 0.5 { 1.0 } else { 0.0 },
        });
    }
    if let Some(crossing) = signed_root_crossing(index, old, new, tol) {
        return Some(crossing);
    }
    None
}

fn signed_root_crossing(index: usize, old: f64, new: f64, tol: f64) -> Option<RootCrossing> {
    if signed_root_crossed(old, new, tol) {
        let post_root = if new.abs() > tol { new } else { -old };
        return Some(RootCrossing {
            index,
            post_relation_memory_value: relation_memory_value_from_root(post_root),
        });
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn root_crossings_detect_leaving_tolerance_on_opposite_side() {
        let crossings = root_crossings(&[1.0e-8, -1.0e-8], &[-1.0e-4, 1.0e-4], 1.0e-6);

        assert_eq!(
            crossings,
            vec![
                RootCrossing {
                    index: 0,
                    post_relation_memory_value: 1.0
                },
                RootCrossing {
                    index: 1,
                    post_relation_memory_value: 0.0
                }
            ]
        );
    }

    #[test]
    fn relation_memory_signed_root_does_not_treat_half_threshold_as_boolean_toggle() {
        let targets = [Some(solve::scalar_slot_p(0))];
        assert_eq!(
            root_crossings_with_relation_memory(&[0.49], &[0.51], 1.0e-6, &targets, &[0.0]),
            Vec::new()
        );
        assert_eq!(
            root_crossings_with_relation_memory(&[0.51], &[0.49], 1.0e-6, &targets, &[0.0]),
            Vec::new()
        );
    }

    #[test]
    fn relation_memory_signed_root_does_not_duplicate_same_side_events() {
        let targets = [Some(solve::scalar_slot_p(0))];
        assert_eq!(
            root_crossings_with_relation_memory(&[0.0], &[-0.1], 1.0e-6, &targets, &[1.0]),
            Vec::new()
        );
        assert_eq!(
            root_crossings_with_relation_memory(&[0.0], &[0.1], 1.0e-6, &targets, &[0.0]),
            Vec::new()
        );
    }

    #[test]
    fn boolean_root_crossing_still_detects_zero_one_toggle() {
        assert_eq!(
            root_crossings(&[0.0], &[1.0], 1.0e-6),
            vec![RootCrossing {
                index: 0,
                post_relation_memory_value: 1.0
            }]
        );
        assert_eq!(
            root_crossings(&[1.0], &[0.0], 1.0e-6),
            vec![RootCrossing {
                index: 0,
                post_relation_memory_value: 0.0
            }]
        );
    }
}
