use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use crate::{SimVariableMeta, runtime::pre_params::write_pre_params_from_sources};

/// The preparation or execution boundary at which selected native code failed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeExecutionStage {
    Compile,
    Call,
}

impl std::fmt::Display for NativeExecutionStage {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Compile => "compile",
            Self::Call => "call",
        })
    }
}

/// Construction-issued executable owner whose selected native obligation failed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeExecutionOwner {
    ImplicitResidual,
    ImplicitProjectionJacobian,
    ImplicitFullJacobian,
    InitialResidual,
    InitialResidualJacobian,
    DerivativeRhs,
    RootConditions,
    EventTransaction { index: usize },
    ExactAssignment { sequence: solve::RefreshSequenceId },
    PureCallTable,
}

impl std::fmt::Display for NativeExecutionOwner {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ImplicitResidual => formatter.write_str("implicit residual"),
            Self::ImplicitProjectionJacobian => formatter.write_str("implicit projection Jacobian"),
            Self::ImplicitFullJacobian => formatter.write_str("implicit full Jacobian"),
            Self::InitialResidual => formatter.write_str("initial residual"),
            Self::InitialResidualJacobian => formatter.write_str("initial residual Jacobian"),
            Self::DerivativeRhs => formatter.write_str("derivative RHS"),
            Self::RootConditions => formatter.write_str("root conditions"),
            Self::EventTransaction { index } => write!(formatter, "event transaction {index}"),
            Self::ExactAssignment { sequence } => {
                write!(formatter, "exact assignment schedule {sequence:?}")
            }
            Self::PureCallTable => formatter.write_str("pure-call table"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum RuntimeSolveError {
    #[error("native {stage} failed for {owner}: {reason}")]
    NativeExecution {
        stage: NativeExecutionStage,
        owner: NativeExecutionOwner,
        reason: String,
    },

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

    #[error(
        "algebraic refresh row {row} cannot be solved for '{target}': the residual does not depend on it{}",
        span_suffix(*.span)
    )]
    RefreshTargetUnassignable {
        row: usize,
        target: String,
        span: Option<rumoca_core::Span>,
    },

    #[error(
        "algebraic refresh row {row} cannot isolate y[{target_y_index}]: singular coefficient {coefficient}{}",
        span_suffix(*.span)
    )]
    RefreshTargetSingular {
        row: usize,
        target_y_index: usize,
        coefficient: f64,
        span: Option<rumoca_core::Span>,
    },

    #[error("non-finite derivative evaluation for state '{state_name}'")]
    NonFiniteDerivative { state_name: String },

    #[error("algebraic directional derivative is unavailable: {reason}")]
    DirectionalDerivativeUnavailable { reason: String },

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
            Self::SolveIr { span, .. }
            | Self::RefreshTargetUnassignable { span, .. }
            | Self::RefreshTargetSingular { span, .. }
            | Self::NonFiniteValue { span, .. } => *span,
            _ => None,
        }
    }

    pub(crate) fn native_compile(owner: NativeExecutionOwner, reason: impl Into<String>) -> Self {
        Self::NativeExecution {
            stage: NativeExecutionStage::Compile,
            owner,
            reason: reason.into(),
        }
    }

    pub(crate) fn native_call(owner: NativeExecutionOwner, reason: impl Into<String>) -> Self {
        Self::NativeExecution {
            stage: NativeExecutionStage::Call,
            owner,
            reason: reason.into(),
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

pub fn discrete_row_pre_mode(
    model: &solve::SolveModel,
    row_idx: usize,
) -> Result<EventPreMode, RuntimeSolveError> {
    model
        .problem()
        .discrete()
        .pre_modes
        .get(row_idx)
        .copied()
        .map(EventPreMode::from)
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "discrete pre-mode row index {row_idx} is out of bounds"
            ))
        })
}

pub fn discrete_row_active_at(
    model: &solve::SolveModel,
    row_idx: usize,
    t: f64,
) -> Result<bool, RuntimeSolveError> {
    let owner = model
        .problem()
        .discrete()
        .clock_owners
        .get(row_idx)
        .copied()
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "discrete clock-owner row index {row_idx} is out of bounds"
            ))
        })?;
    let Some(owner) = owner else {
        return Ok(true);
    };
    let schedule = model
        .problem()
        .clocks()
        .periodic_schedule(owner)
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "discrete row {row_idx} refers to periodic clock {} outside the clock partition",
                owner.index()
            ))
        })?;
    Ok(crate::timeline::periodic_schedule_matches_time(schedule, t))
}

pub fn apply_discrete_slot_value(
    target: solve::ScalarSlot,
    value: f64,
    y: &mut [f64],
    p: &mut [f64],
    _tol: f64,
) -> Result<bool, solve_eval::EvalSolveError> {
    solve_eval::apply_scalar_slot_value_exact(target, value, y, p)
}

pub fn row_reads_solver_or_time(row: &[solve::LinearOp]) -> bool {
    row.iter().any(op_reads_solver_or_time)
}

fn op_reads_solver_or_time(op: &solve::LinearOp) -> bool {
    match op {
        solve::LinearOp::LoadY { .. }
        | solve::LinearOp::LoadTime { .. }
        | solve::LinearOp::TensorLoad {
            input: solve::TensorInputKind::Y,
            ..
        } => true,
        solve::LinearOp::FunctionFold { program, .. }
        | solve::LinearOp::GuardedFunctionFold { program, .. }
        | solve::LinearOp::StoreOutputFunctionFold { program, .. } => {
            row_reads_solver_or_time(program.update())
        }
        solve::LinearOp::FunctionConditional { program, .. } => {
            program.arms().iter().any(|arm| {
                row_reads_solver_or_time(arm.condition()) || row_reads_solver_or_time(arm.result())
            }) || row_reads_solver_or_time(program.fallback())
        }
        _ => false,
    }
}

/// Compare runtime fixed-point values with a combined absolute/relative scale.
///
/// Projection routines certify their own residual tolerances. This comparison
/// only decides whether another coupled runtime pass is needed, so large
/// physical values must not require bit-level or absolute-tolerance agreement.
pub fn runtime_value_changed(old: f64, new: f64, tol: f64) -> bool {
    if old == new {
        return false;
    }
    if !old.is_finite() || !new.is_finite() {
        return true;
    }
    let scale = 1.0_f64.max(old.abs()).max(new.abs());
    (old - new).abs() > tol.abs() * scale
}

pub fn runtime_values_changed(before: &[f64], after: &[f64], tol: f64) -> bool {
    before.len() != after.len()
        || before
            .iter()
            .copied()
            .zip(after.iter().copied())
            .any(|(old, new)| runtime_value_changed(old, new, tol))
}

pub fn event_eval_params_for_pre_mode(
    model: &solve::SolveModel,
    base_p: &[f64],
    pre_y: &[f64],
    pre_p: &[f64],
    t: f64,
    tol: f64,
) -> Vec<f64> {
    let mut eval_p = base_p.to_vec();
    write_pre_params_from_sources(model, pre_y, pre_p, &mut eval_p, tol);
    write_clock_activation_params(model, &mut eval_p, t);
    eval_p
}

/// Derive mixed-condition clock leaves from their authoritative typed schedules.
///
/// The compiler reserves exactly one hidden lane per periodic clock and the
/// Solve shape contract proves the two dense arrays agree. This function only
/// projects the schedule at `t`; it does not create another timing owner.
pub fn write_clock_activation_params(model: &solve::SolveModel, p: &mut [f64], t: f64) {
    for (schedule, &index) in model
        .problem()
        .clocks()
        .periodic_event_schedules
        .iter()
        .zip(&model.problem().clocks().activation_parameter_indices)
    {
        p[index] = f64::from(crate::timeline::periodic_schedule_matches_time(schedule, t));
    }
}

/// Project periodic-clock leaves into one public observation coordinate.
///
/// Clock activations are event-engine pulses, not public state. Numeric times
/// immediately before or after a tick can still lie inside the scheduler's
/// tolerant recognition window, so public observation must not derive these
/// lanes from floating-point time at all.
pub fn write_observation_clock_activation_params(model: &solve::SolveModel, p: &mut [f64]) {
    for &index in &model.problem().clocks().activation_parameter_indices {
        p[index] = 0.0;
    }
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
    t: f64,
    tol: f64,
) -> Vec<f64> {
    let (pre_y, pre_p) = event_pre_sources_for_mode(mode, sources);
    event_eval_params_for_pre_mode(model, base_p, pre_y, pre_p, t, tol)
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
                if let Some(crossing) = signed_root_crossing(index, *old, *new, tol) {
                    return relation_memory_target_toggled(
                        crossing.post_relation_memory_value,
                        target,
                        params,
                    )
                    .then_some(crossing);
                }
                if relation_memory_value_from_signed_root(*new).is_some() {
                    return None;
                }
                return None;
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

/// Give an exactly zero root the compiler-owned side of its source relation.
///
/// Numerical root finders detect sign changes, so an unqualified `0.0` at an
/// accepted point cannot represent whether a strict or non-strict relation
/// owns that point. Solve IR preserves that semantic distinction explicitly;
/// the smallest ordinary dimensionless perturbation is enough to expose its
/// sign without moving the mathematical root. A nonzero value remains a signed
/// distance from the root even when it lies inside the solver's convergence
/// tolerance; changing that sign would contradict the source relation.
pub fn orient_typed_root_zeros(roots: &mut [f64], zero_domains: &[solve::RootZeroDomain]) {
    for (root, zero_domain) in roots.iter_mut().zip(zero_domains) {
        if *root != 0.0 {
            continue;
        }
        *root = match zero_domain {
            solve::RootZeroDomain::Positive => f64::EPSILON,
            solve::RootZeroDomain::NonPositive => -f64::EPSILON,
            solve::RootZeroDomain::Previous => continue,
        };
    }
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

fn relation_memory_target_toggled(
    post: f64,
    target: Option<solve::ScalarSlot>,
    params: &[f64],
) -> bool {
    let Some(solve::ScalarSlot::P {
        index: param_idx, ..
    }) = target
    else {
        return false;
    };
    let Some(current) = params.get(param_idx).copied() else {
        return false;
    };
    relation_toggled(current, post)
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

    use crate::test_support::empty_binary64_first_product_model;

    #[test]
    fn nested_function_conditionals_retain_time_dependency() {
        let conditional = solve::FunctionConditionalProgram::checked(
            0,
            [1],
            [(
                vec![
                    solve::LinearOp::LoadTime { dst: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            )],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        )
        .expect("nested conditional fixture is valid");
        let row = [solve::LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        }];

        assert!(row_reads_solver_or_time(&row));
    }

    #[test]
    fn compact_tensor_y_load_retains_solver_dependency() {
        let row = [solve::LinearOp::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 2,
            count: 3,
            seed_start: None,
            lanes: 1,
        }];

        assert!(row_reads_solver_or_time(&row));
    }

    #[test]
    fn periodic_clock_owner_activates_only_on_its_exact_lattice() {
        let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10)
            .expect("one-tenth-second lattice is valid");
        let schedule =
            solve::PeriodicEventSchedule::new(lattice).expect("periodic schedule is valid");
        let clocks = solve::SolveClockPartition {
            periodic_event_schedules: vec![schedule],
            activation_parameter_indices: vec![0],
        };
        let owner = clocks
            .periodic_clock_id(0)
            .expect("inserted periodic clock has a typed identity");
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("periodic_clock_owner.mo"),
            1,
            2,
        );
        let rhs = solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            span.require_provenance("periodic clock owner fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("periodic clock owner fixture is computable");
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 0, 2),
                solve::SolveLayout {
                    compiled_parameter_len: 2,
                    ..Default::default()
                },
                crate::test_support::ContinuousSystemFixture::empty(),
                solve::InitializationSolveSystem::empty(),
                solve::DiscreteSolveSystem {
                    rhs,
                    update_targets: vec![solve::scalar_slot_p(1)],
                    row_roles: vec![solve::DiscreteRowRole::EventAction],
                    pre_modes: vec![solve::DiscreteEventPreMode::EventEntry],
                    observation_refresh: vec![false],
                    integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
                    clock_owners: vec![Some(owner)],
                    clock_partition_order: vec![solve::ClockPartitionStep::ScalarRows {
                        start_row: 0,
                        count: 1,
                    }],
                    ..Default::default()
                },
                solve::SolveEventPartition::default(),
                clocks,
            )
            .expect("periodic clock fixture satisfies the checked root contract"),
            parameters: vec![0.0; 2],
            ..empty_binary64_first_product_model()
        };

        assert!(!discrete_row_active_at(&model, 0, 0.05).unwrap());
        assert!(discrete_row_active_at(&model, 0, 0.1).unwrap());
        assert!(!discrete_row_active_at(&model, 0, 0.15).unwrap());
        assert!(discrete_row_active_at(&model, 0, 0.2).unwrap());
    }

    #[test]
    fn typed_clock_activation_lanes_are_exact_and_target_independent() {
        let tenth = solve::PeriodicEventSchedule::new(
            rumoca_core::ClockLattice::from_seconds(0.1, 0.0).unwrap(),
        )
        .unwrap();
        let fifth = solve::PeriodicEventSchedule::new(
            rumoca_core::ClockLattice::from_seconds(0.2, 0.0).unwrap(),
        )
        .unwrap();
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 0, 3),
                solve::SolveLayout {
                    compiled_parameter_len: 3,
                    ..Default::default()
                },
                crate::test_support::ContinuousSystemFixture::empty(),
                solve::InitializationSolveSystem::empty(),
                solve::DiscreteSolveSystem::default(),
                solve::SolveEventPartition::default(),
                solve::SolveClockPartition {
                    periodic_event_schedules: vec![tenth, fifth],
                    // Deliberately not schedule order: typed identity owns the
                    // mapping, not a row target or incidental P-slot ordinal.
                    activation_parameter_indices: vec![2, 0],
                },
            )
            .expect("clock activation fixture satisfies the checked root contract"),
            parameters: vec![0.0; 3],
            ..empty_binary64_first_product_model()
        };
        let mut params = vec![-1.0; 3];

        write_clock_activation_params(&model, &mut params, 0.1);
        assert_eq!(params, [0.0, -1.0, 1.0]);

        // A non-clock event between ticks clears both lanes.
        write_clock_activation_params(&model, &mut params, 0.15);
        assert_eq!(params, [0.0, -1.0, 0.0]);

        // A root or other event coincident with both schedules observes both
        // clock leaves as true in the one event iteration at that instant.
        write_clock_activation_params(&model, &mut params, 0.2);
        assert_eq!(params, [1.0, -1.0, 1.0]);
    }

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
    fn relation_memory_same_side_stale_value_does_not_request_bisection() {
        let targets = [Some(solve::scalar_slot_p(0))];

        assert_eq!(
            root_crossings_with_relation_memory(&[1.0], &[1.0], 1.0e-6, &targets, &[1.0]),
            Vec::new()
        );
    }

    #[test]
    fn relation_memory_signed_root_crossing_requests_bisection_when_post_value_changes() {
        let targets = [Some(solve::scalar_slot_p(0))];

        assert_eq!(
            root_crossings_with_relation_memory(&[1.0], &[-1.0], 1.0e-6, &targets, &[0.0]),
            vec![RootCrossing {
                index: 0,
                post_relation_memory_value: 1.0
            }]
        );
    }

    #[test]
    fn typed_root_zero_orientation_exposes_relation_side_to_root_finders() {
        let mut roots = [0.0, -1.0e-8, 0.0, -2.0];
        orient_typed_root_zeros(
            &mut roots,
            &[
                solve::RootZeroDomain::Positive,
                solve::RootZeroDomain::NonPositive,
                solve::RootZeroDomain::Previous,
                solve::RootZeroDomain::Positive,
            ],
        );

        assert!(roots[0].is_sign_positive() && roots[0] > 0.0);
        assert_eq!(roots[1], -1.0e-8);
        assert_eq!(roots[2], 0.0);
        assert_eq!(roots[3], -2.0);
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

    #[test]
    fn runtime_change_detection_combines_absolute_and_relative_scale() {
        assert!(!runtime_value_changed(100_000.0, 100_000.01, 1.0e-6));
        assert!(runtime_value_changed(0.0, 2.0e-6, 1.0e-6));
        assert!(runtime_value_changed(f64::NAN, f64::NAN, 1.0e-6));
        assert!(runtime_values_changed(&[1.0], &[1.0, 2.0], 1.0e-6));
    }
}
