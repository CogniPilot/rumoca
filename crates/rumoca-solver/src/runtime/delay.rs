use std::cell::RefCell;
use std::rc::Rc;

use rumoca_ir_solve as solve;

use super::solve_runtime::{CompiledSolveExpression, SolveExecutionBackend};
use crate::RuntimeSolveError;
use rumoca_eval_solve::{EvalSolveError, PreparedScalarProgramBlock, RowEvalContext};

#[derive(Clone)]
enum DelayExecution {
    NoChannels,
    Reference,
    Compiled {
        source: Rc<dyn CompiledSolveExpression>,
        time: Rc<dyn CompiledSolveExpression>,
        maximum: Rc<dyn CompiledSolveExpression>,
    },
}

#[derive(Clone, Copy, Debug)]
struct DelayPoint {
    time: f64,
    value: f64,
}

#[derive(Clone, Debug, Default)]
struct DelayChannelHistory {
    points: Vec<DelayPoint>,
    points_head: usize,
    delay_time: f64,
    delay_max: f64,
    accepted_query_time: f64,
    // Accepted discontinuities are retained in source time. The delayed query
    // coordinate may move in either direction when delayTime is variable, so
    // consumed forward crossings cannot be discarded.
    discontinuity_times: Vec<f64>,
    discontinuity_head: usize,
    pruned_discontinuity_parity: bool,
    suppressed_discontinuity: Option<SuppressedDiscontinuity>,
}

#[derive(Clone, Copy, Debug)]
struct SuppressedDiscontinuity {
    source_time: f64,
    right_side: bool,
}

#[derive(Clone, Debug, Default)]
struct DelayState {
    initialized: bool,
    history_committed: bool,
    channels: Vec<DelayChannelHistory>,
}

/// Opaque accepted delay timeline captured with an ME component state.
#[derive(Clone)]
pub(crate) struct DelayRuntimeSnapshot(DelayState);

#[derive(Clone, Debug, Default)]
struct DelayRowScratch {
    sources: Vec<f64>,
    delay_times: Vec<f64>,
    delay_maxima: Vec<f64>,
    delayed_values: Vec<f64>,
}

#[derive(Clone)]
pub(crate) struct DelayRuntime {
    execution: DelayExecution,
    source_rhs: PreparedScalarProgramBlock,
    delay_time_rhs: PreparedScalarProgramBlock,
    delay_max_rhs: PreparedScalarProgramBlock,
    value_parameter_indices: Vec<usize>,
    source_is_discrete: Vec<bool>,
    // SolveRuntime is cloned when solver closures are built. Delay history is
    // model runtime state, so every clone must observe the same accepted
    // timeline rather than receiving a point-in-time copy.
    state: Rc<RefCell<DelayState>>,
    // Each SolveRuntime clone has independent evaluation scratch while sharing
    // only the accepted history above. Solver RHS/Jacobian/root callbacks
    // therefore reuse their buffers without coupling speculative evaluations.
    row_scratch: RefCell<DelayRowScratch>,
}

fn compile_delay(
    label: &str,
    block: &solve::ScalarProgramBlock,
    backend: &dyn SolveExecutionBackend,
) -> Result<Rc<dyn CompiledSolveExpression>, EvalSolveError> {
    backend
        .compile_expression(block)
        .map_err(|message| EvalSolveError::CompiledExecution {
            message: format!("required delay program `{label}` could not be lowered: {message}"),
            span: if block.row_count() == 1 {
                block.program_span(0)
            } else {
                None
            },
        })
}

impl DelayRuntime {
    #[cfg(any(test, feature = "native-wasm"))]
    pub(crate) fn new(partition: &solve::SolveDelayPartition) -> Result<Self, EvalSolveError> {
        Self::new_with_backend(partition, None)
    }

    pub(crate) fn new_with_backend(
        partition: &solve::SolveDelayPartition,
        backend: Option<&dyn SolveExecutionBackend>,
    ) -> Result<Self, EvalSolveError> {
        let source_count = partition.source_rhs.len();
        for (field, actual) in [
            ("delay_time_rhs", partition.delay_time_rhs.len()),
            ("delay_max_rhs", partition.delay_max_rhs.len()),
            (
                "value_parameter_indices",
                partition.value_parameter_indices.len(),
            ),
            ("source_is_discrete", partition.source_is_discrete.len()),
        ] {
            if actual != source_count {
                return Err(EvalSolveError::ShapeContract {
                    message: format!(
                        "events.delays.{field} expected {source_count} rows, got {actual}"
                    ),
                    span: None,
                });
            }
        }
        let execution = match backend {
            _ if source_count == 0 => DelayExecution::NoChannels,
            None => DelayExecution::Reference,
            Some(backend) => {
                let compile = |label, block| compile_delay(label, block, backend);
                DelayExecution::Compiled {
                    source: compile("source", &partition.source_rhs)?,
                    time: compile("time", &partition.delay_time_rhs)?,
                    maximum: compile("maximum", &partition.delay_max_rhs)?,
                }
            }
        };
        Ok(Self {
            execution,
            source_rhs: PreparedScalarProgramBlock::new(partition.source_rhs.clone())?,
            delay_time_rhs: PreparedScalarProgramBlock::new(partition.delay_time_rhs.clone())?,
            delay_max_rhs: PreparedScalarProgramBlock::new(partition.delay_max_rhs.clone())?,
            value_parameter_indices: partition.value_parameter_indices.clone(),
            source_is_discrete: partition.source_is_discrete.clone(),
            state: Rc::new(RefCell::new(DelayState::default())),
            row_scratch: RefCell::new(DelayRowScratch::default()),
        })
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.value_parameter_indices.is_empty()
    }

    pub(crate) fn reset(&self) {
        *self.state.borrow_mut() = DelayState::default();
    }

    pub(crate) fn snapshot(&self) -> DelayRuntimeSnapshot {
        DelayRuntimeSnapshot(self.state.borrow().clone())
    }

    pub(crate) fn restore(&self, snapshot: &DelayRuntimeSnapshot) {
        self.state.borrow_mut().clone_from(&snapshot.0);
    }

    #[cfg(test)]
    pub(crate) fn matches_snapshot(&self, snapshot: &DelayRuntimeSnapshot) -> bool {
        self.state.borrow().bit_eq(&snapshot.0)
    }

    pub(crate) fn initialize(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), RuntimeSolveError> {
        if self.is_empty() {
            return Ok(());
        }
        let mut rows = self.row_scratch.borrow_mut();
        self.evaluate_channel_rows(time, solver_y, params, context, &mut rows)?;
        validate_delay_values(&rows.delay_times, &rows.delay_maxima)?;
        write_delay_parameters(&self.value_parameter_indices, params, &rows.sources)?;

        let channels = rows
            .sources
            .iter()
            .copied()
            .zip(rows.delay_times.iter().copied())
            .zip(rows.delay_maxima.iter().copied())
            .map(|((value, delay_time), delay_max)| DelayChannelHistory {
                points: vec![DelayPoint { time, value }],
                points_head: 0,
                delay_time,
                delay_max,
                accepted_query_time: time - delay_time,
                discontinuity_times: Vec::new(),
                discontinuity_head: 0,
                pruned_discontinuity_parity: false,
                suppressed_discontinuity: None,
            })
            .collect();
        *self.state.borrow_mut() = DelayState {
            initialized: true,
            history_committed: false,
            channels,
        };
        Ok(())
    }

    pub(crate) fn refresh(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
        context: RowEvalContext<'_>,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        if self.is_empty() {
            return Ok(None);
        }
        if !self.state.borrow().initialized {
            self.initialize(time, solver_y, params, context)?;
        }
        let mut rows = self.row_scratch.borrow_mut();
        self.evaluate_channel_rows(time, solver_y, params, context, &mut rows)?;
        validate_delay_values(&rows.delay_times, &rows.delay_maxima)?;
        rows.delayed_values.clear();
        {
            let state = self.state.borrow();
            let history_committed = state.history_committed;
            for (index, channel) in state.channels.iter().enumerate() {
                let delayed = refresh_channel_value(
                    channel,
                    history_committed,
                    time,
                    rows.delay_times[index],
                    rows.sources[index],
                    self.source_is_discrete[index],
                );
                rows.delayed_values.push(delayed);
            }
        }
        write_delay_parameters(&self.value_parameter_indices, params, &rows.delayed_values)?;
        Ok(min_positive_delay(&rows.delay_times))
    }

    /// Extend every channel's accepted history to an event-entry coordinate.
    ///
    /// Event Mode is entered at the accepted point, which the host may have
    /// adopted from the last completed step at a roundoff-coincident
    /// coordinate. The accepted source values are the event's left limit, so
    /// they are held to `time` here. The settled values committed at the same
    /// coordinate then form the exact left/right pair that records a source
    /// jump as a transported discontinuity, rather than a continuous ramp
    /// between two distinct accepted samples.
    pub(crate) fn hold_accepted_history_to(&self, time: f64) {
        let mut state = self.state.borrow_mut();
        if !state.history_committed {
            return;
        }
        for channel in &mut state.channels {
            if let Some(last) = active_points(channel).last().copied()
                && last.time < time
            {
                channel.points.push(DelayPoint {
                    time,
                    value: last.value,
                });
            }
        }
    }

    pub(crate) fn commit(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.commit_evaluated_at(time, time, solver_y, params, context)
    }

    pub(crate) fn commit_evaluated_at(
        &self,
        accepted_time: f64,
        evaluation_time: f64,
        solver_y: &[f64],
        params: &[f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), RuntimeSolveError> {
        if self.is_empty() {
            return Ok(());
        }
        let mut rows = self.row_scratch.borrow_mut();
        self.evaluate_channel_rows(evaluation_time, solver_y, params, context, &mut rows)?;
        validate_delay_values(&rows.delay_times, &rows.delay_maxima)?;
        let mut state = self.state.borrow_mut();
        if !state.initialized {
            state.initialized = true;
            state.history_committed = true;
            state.channels = rows
                .sources
                .iter()
                .zip(&rows.delay_times)
                .zip(&rows.delay_maxima)
                .map(|((&value, &delay_time), &delay_max)| DelayChannelHistory {
                    points: vec![DelayPoint {
                        time: accepted_time,
                        value,
                    }],
                    points_head: 0,
                    delay_time,
                    delay_max,
                    accepted_query_time: accepted_time - delay_time,
                    discontinuity_times: Vec::new(),
                    discontinuity_head: 0,
                    pruned_discontinuity_parity: false,
                    suppressed_discontinuity: None,
                })
                .collect();
            return Ok(());
        }
        if !state.history_committed {
            state.channels = rows
                .sources
                .iter()
                .zip(&rows.delay_times)
                .zip(&rows.delay_maxima)
                .map(|((&value, &delay_time), &delay_max)| DelayChannelHistory {
                    points: vec![DelayPoint {
                        time: accepted_time,
                        value,
                    }],
                    points_head: 0,
                    delay_time,
                    delay_max,
                    accepted_query_time: accepted_time - delay_time,
                    discontinuity_times: Vec::new(),
                    discontinuity_head: 0,
                    pruned_discontinuity_parity: false,
                    suppressed_discontinuity: None,
                })
                .collect();
            state.history_committed = true;
            return Ok(());
        }

        for index in 0..state.channels.len() {
            let channel = &mut state.channels[index];
            channel.delay_time = rows.delay_times[index];
            channel.delay_max = rows.delay_maxima[index];
            let discontinuity = append_accepted_point(
                channel,
                accepted_time,
                rows.sources[index],
                self.source_is_discrete[index],
            );
            if discontinuity {
                insert_discontinuity_time(channel, accepted_time);
            }
            update_suppressed_discontinuity(channel, accepted_time - rows.delay_times[index]);
            prune_history(channel, accepted_time);
        }
        Ok(())
    }

    pub(crate) fn event_root_count(&self) -> usize {
        // A continuous Real may still jump at an event (for example through a
        // mode switch or reinit). Same-time accepted points record that jump,
        // and every delay channel therefore needs a transported-discontinuity
        // root. Channels without discontinuities remain on the positive side.
        self.value_parameter_indices.len()
    }

    pub(crate) fn evaluate_event_roots(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let expected = self.event_root_count();
        if out.len() != expected {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "delay event root output expected {expected} values, got {}",
                    out.len()
                ),
                span: None,
            }
            .into());
        }
        if expected == 0 {
            return Ok(());
        }
        if !self.state.borrow().initialized {
            out.fill(1.0);
            return Ok(());
        }
        let mut rows = self.row_scratch.borrow_mut();
        rows.delay_times.resize(self.delay_time_rhs.len(), 0.0);
        if let DelayExecution::Compiled { time: code, .. } = &self.execution {
            code.call(
                solver_y,
                params,
                time,
                context.external_tables.unwrap_or(&[]),
                &mut rows.delay_times,
            )?;
        } else {
            self.delay_time_rhs.eval_with_context(
                solver_y,
                params,
                time,
                context,
                &mut rows.delay_times,
            )?;
        }
        validate_delay_times(&rows.delay_times)?;
        let state = self.state.borrow();
        if state.channels.len() != expected {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "delay history expected {expected} channels, got {}",
                    state.channels.len()
                ),
                span: None,
            }
            .into());
        }
        for ((slot, channel), delay_time) in
            out.iter_mut().zip(&state.channels).zip(&rows.delay_times)
        {
            *slot = discrete_delay_root(channel, time - *delay_time);
        }
        Ok(())
    }

    #[cfg(test)]
    fn accepted_step_limit(&self) -> Option<f64> {
        self.state
            .borrow()
            .channels
            .iter()
            .map(|channel| channel.delay_time)
            .filter(|delay| *delay > 0.0 && delay.is_finite())
            .min_by(|left, right| left.total_cmp(right))
    }

    fn evaluate_channel_rows(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
        context: RowEvalContext<'_>,
        rows: &mut DelayRowScratch,
    ) -> Result<(), RuntimeSolveError> {
        rows.sources.resize(self.source_rhs.len(), 0.0);
        rows.delay_times.resize(self.delay_time_rhs.len(), 0.0);
        rows.delay_maxima.resize(self.delay_max_rhs.len(), 0.0);
        if let DelayExecution::Compiled {
            source,
            time: delay_time,
            maximum,
        } = &self.execution
        {
            let tables = context.external_tables.unwrap_or(&[]);
            source.call(solver_y, params, time, tables, &mut rows.sources)?;
            delay_time.call(solver_y, params, time, tables, &mut rows.delay_times)?;
            maximum.call(solver_y, params, time, tables, &mut rows.delay_maxima)?;
            return Ok(());
        }
        self.source_rhs
            .eval_with_context(solver_y, params, time, context, &mut rows.sources)?;
        self.delay_time_rhs.eval_with_context(
            solver_y,
            params,
            time,
            context,
            &mut rows.delay_times,
        )?;
        self.delay_max_rhs.eval_with_context(
            solver_y,
            params,
            time,
            context,
            &mut rows.delay_maxima,
        )?;
        Ok(())
    }
}

#[cfg(test)]
impl DelayState {
    fn bit_eq(&self, other: &Self) -> bool {
        self.initialized == other.initialized
            && self.history_committed == other.history_committed
            && self.channels.len() == other.channels.len()
            && self
                .channels
                .iter()
                .zip(&other.channels)
                .all(|(left, right)| left.bit_eq(right))
    }
}

#[cfg(test)]
impl DelayChannelHistory {
    fn bit_eq(&self, other: &Self) -> bool {
        self.points_head == other.points_head
            && self.points.len() == other.points.len()
            && self
                .points
                .iter()
                .zip(&other.points)
                .all(|(left, right)| left.bit_eq(*right))
            && self.delay_time.to_bits() == other.delay_time.to_bits()
            && self.delay_max.to_bits() == other.delay_max.to_bits()
            && self.accepted_query_time.to_bits() == other.accepted_query_time.to_bits()
            && self.discontinuity_head == other.discontinuity_head
            && float_slice_bit_eq(&self.discontinuity_times, &other.discontinuity_times)
            && self.pruned_discontinuity_parity == other.pruned_discontinuity_parity
            && suppressed_discontinuity_bit_eq(
                self.suppressed_discontinuity,
                other.suppressed_discontinuity,
            )
    }
}

#[cfg(test)]
impl DelayPoint {
    fn bit_eq(self, other: Self) -> bool {
        self.time.to_bits() == other.time.to_bits() && self.value.to_bits() == other.value.to_bits()
    }
}

#[cfg(test)]
fn float_slice_bit_eq(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.to_bits() == right.to_bits())
}

#[cfg(test)]
fn suppressed_discontinuity_bit_eq(
    left: Option<SuppressedDiscontinuity>,
    right: Option<SuppressedDiscontinuity>,
) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.source_time.to_bits() == right.source_time.to_bits()
                && left.right_side == right.right_side
        }
        (None, None) => true,
        _ => false,
    }
}

fn refresh_channel_value(
    channel: &DelayChannelHistory,
    history_committed: bool,
    time: f64,
    delay_time: f64,
    source: f64,
    source_is_discrete: bool,
) -> f64 {
    if history_committed {
        return delayed_value(channel, time, delay_time, source, source_is_discrete);
    }
    // At the initial instant, delay(u, ...) = u. Initialization may change u
    // while solving, so use each speculative source without mutating history.
    source
}

fn validate_delay_values(delay_times: &[f64], delay_maxima: &[f64]) -> Result<(), EvalSolveError> {
    validate_delay_times(delay_times)?;
    for (index, (&delay_time, &delay_max)) in delay_times.iter().zip(delay_maxima).enumerate() {
        if !delay_max.is_finite() || delay_max < delay_time {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "delay channel {index} produced delayMax {delay_max} below delayTime \
                     {delay_time}"
                ),
                span: None,
            });
        }
    }
    Ok(())
}

fn validate_delay_times(delay_times: &[f64]) -> Result<(), EvalSolveError> {
    for (index, &delay_time) in delay_times.iter().enumerate() {
        if !delay_time.is_finite() || delay_time < 0.0 {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "delay channel {index} produced invalid delayTime {delay_time}; expected a \
                     finite non-negative value"
                ),
                span: None,
            });
        }
    }
    Ok(())
}

fn write_delay_parameters(
    indices: &[usize],
    params: &mut [f64],
    values: &[f64],
) -> Result<(), EvalSolveError> {
    for (&index, &value) in indices.iter().zip(values) {
        let len = params.len();
        let slot = params.get_mut(index).ok_or(EvalSolveError::MissingInput {
            vector: "p",
            index,
            len,
            span: None,
        })?;
        *slot = value;
    }
    Ok(())
}

fn delayed_value(
    channel: &DelayChannelHistory,
    time: f64,
    delay_time: f64,
    current_source: f64,
    discrete: bool,
) -> f64 {
    let query_time = time - delay_time;
    let points = active_points(channel);
    // During forward trial evaluation, a zero delay is the current source,
    // including discrete source changes whose accepted point is not committed yet.
    if delay_time == 0.0 && points.last().is_none_or(|point| point.time <= time) {
        return current_source;
    }
    let Some(first) = points.first().copied() else {
        return current_source;
    };
    if query_time <= first.time {
        return first.value;
    }
    let upper = points.partition_point(|point| point.time <= query_time);
    let lower = points[upper.saturating_sub(1)];
    if discrete || upper < points.len() && points[upper].time == lower.time {
        return lower.value;
    }
    let next = points.get(upper).copied().unwrap_or(DelayPoint {
        time,
        value: current_source,
    });
    if next.time <= lower.time {
        return next.value;
    }
    if let Some(value) = smooth_interpolation(points, upper, next, query_time) {
        return value;
    }
    let fraction = ((query_time - lower.time) / (next.time - lower.time)).clamp(0.0, 1.0);
    lower.value + fraction * (next.value - lower.value)
}

/// Cubic (or, near the ends of a smooth run, quadratic) Lagrange
/// interpolation of a continuous history at `query_time`, which lies between
/// `points[upper - 1]` and `next`.
///
/// Linear interpolation between accepted steps is only second-order accurate,
/// so a delay of a smooth signal drifted from its source by the square of the
/// step size (a quadratic history read back with a 5e-5 error at rtol 1e-6).
/// The stencil takes the bracketing points and at most one neighbour on each
/// side, and only across strictly increasing times: an equal-time pair records
/// a jump, which the stencil never crosses. `None` leaves the linear form.
fn smooth_interpolation(
    points: &[DelayPoint],
    upper: usize,
    next: DelayPoint,
    query_time: f64,
) -> Option<f64> {
    let lower = upper.checked_sub(1)?;
    let mut stencil = Vec::with_capacity(4);
    if let Some(before) = lower.checked_sub(1).map(|index| points[index])
        && before.time < points[lower].time
    {
        stencil.push(before);
    }
    stencil.push(points[lower]);
    stencil.push(next);
    // `next` is an accepted point only when it lies inside the history; the
    // trial point past the last accepted step has no right neighbour.
    if let Some(after) = points.get(upper + 1).copied()
        && upper < points.len()
        && after.time > next.time
    {
        stencil.push(after);
    }
    if stencil.len() < 3 || stencil.iter().any(|point| !point.value.is_finite()) {
        return None;
    }
    // Interpolate offsets from the bracketing lower point, so a constant run
    // reads back bit-exactly (the weights sum to one only up to rounding).
    let base = points[lower].value;
    let offset = stencil
        .iter()
        .enumerate()
        .map(|(i, point)| {
            let weight = stencil
                .iter()
                .enumerate()
                .filter(|(j, _)| *j != i)
                .map(|(_, other)| (query_time - other.time) / (point.time - other.time))
                .product::<f64>();
            weight * (point.value - base)
        })
        .sum::<f64>();
    let value = base + offset;
    value.is_finite().then_some(value)
}

fn append_accepted_point(
    channel: &mut DelayChannelHistory,
    time: f64,
    value: f64,
    discrete: bool,
) -> bool {
    let Some(last) = active_points(channel).last().copied() else {
        channel.points.push(DelayPoint { time, value });
        return false;
    };
    if time < last.time {
        return false;
    }
    let changed = value.to_bits() != last.value.to_bits();
    // Accepted samples are the authoritative history timeline. Only the exact
    // same timestamp denotes an event left/right pair; a relative tolerance
    // would merge distinct samples at large absolute times.
    if time == last.time {
        if changed {
            channel.points.push(DelayPoint { time, value });
        }
        return changed;
    }
    channel.points.push(DelayPoint { time, value });
    discrete && changed
}

fn insert_discontinuity_time(channel: &mut DelayChannelHistory, source_time: f64) {
    let discontinuities = active_discontinuities(channel);
    let insertion =
        match discontinuities.binary_search_by(|candidate| candidate.total_cmp(&source_time)) {
            Ok(_) => return,
            Err(index)
                if discontinuities
                    .get(index)
                    .is_some_and(|candidate| delay_time_matches(*candidate, source_time))
                    || index
                        .checked_sub(1)
                        .and_then(|previous| discontinuities.get(previous))
                        .is_some_and(|candidate| delay_time_matches(*candidate, source_time)) =>
            {
                return;
            }
            Err(index) => index,
        };
    channel
        .discontinuity_times
        .insert(channel.discontinuity_head + insertion, source_time);
}

fn active_points(channel: &DelayChannelHistory) -> &[DelayPoint] {
    &channel.points[channel.points_head..]
}

fn active_discontinuities(channel: &DelayChannelHistory) -> &[f64] {
    &channel.discontinuity_times[channel.discontinuity_head..]
}

fn compact_consumed_prefix<T: Copy>(values: &mut Vec<T>, head: &mut usize) {
    let active_len = values.len().saturating_sub(*head);
    if *head == 0 || *head < active_len {
        return;
    }
    values.copy_within(*head.., 0);
    values.truncate(active_len);
    *head = 0;
}

fn matching_discontinuity_index(channel: &DelayChannelHistory, query_time: f64) -> Option<usize> {
    let discontinuities = active_discontinuities(channel);
    let index = discontinuities.partition_point(|source_time| {
        *source_time < query_time && !delay_time_matches(*source_time, query_time)
    });
    if discontinuities
        .get(index)
        .is_some_and(|source_time| delay_time_matches(*source_time, query_time))
    {
        return Some(index);
    }
    index
        .checked_sub(1)
        .filter(|previous| delay_time_matches(discontinuities[*previous], query_time))
}

fn update_suppressed_discontinuity(channel: &mut DelayChannelHistory, query_time: f64) {
    channel.suppressed_discontinuity =
        matching_discontinuity_index(channel, query_time).map(|index| {
            let source_time = active_discontinuities(channel)[index];
            let right_side = if query_time > source_time {
                true
            } else if query_time < source_time {
                false
            } else if channel.accepted_query_time > source_time {
                // The delayed coordinate reached the root while moving
                // backward. Suppress the newly accepted left side.
                false
            } else if channel.accepted_query_time < source_time {
                true
            } else {
                channel
                    .suppressed_discontinuity
                    .filter(|suppressed| delay_time_matches(suppressed.source_time, source_time))
                    .is_none_or(|suppressed| suppressed.right_side)
            };
            SuppressedDiscontinuity {
                source_time,
                right_side,
            }
        });
    channel.accepted_query_time = query_time;
}

fn discrete_delay_root(channel: &DelayChannelHistory, query_time: f64) -> f64 {
    let discontinuities = active_discontinuities(channel);
    if discontinuities.is_empty() {
        return 1.0;
    }
    if let Some(index) = matching_discontinuity_index(channel, query_time) {
        let source_time = discontinuities[index];
        if let Some(suppressed) = channel.suppressed_discontinuity
            && delay_time_matches(suppressed.source_time, source_time)
        {
            let interval = index + usize::from(suppressed.right_side);
            return delay_root_interval_sign(channel, interval);
        }
        return 0.0;
    }
    let next = discontinuities.partition_point(|source_time| *source_time < query_time);
    let left_distance = next
        .checked_sub(1)
        .map_or(f64::INFINITY, |index| query_time - discontinuities[index]);
    let right_distance = discontinuities
        .get(next)
        .map_or(f64::INFINITY, |source_time| source_time - query_time);
    delay_root_interval_sign(channel, next) * left_distance.min(right_distance)
}

fn delay_root_interval_sign(channel: &DelayChannelHistory, interval: usize) -> f64 {
    let odd = channel.pruned_discontinuity_parity ^ (interval % 2 == 1);
    if odd { -1.0 } else { 1.0 }
}

fn prune_history(channel: &mut DelayChannelHistory, time: f64) {
    let keep_after = time - channel.delay_max;
    let first_after = active_points(channel).partition_point(|point| point.time < keep_after);
    let consumed_points = first_after.saturating_sub(1);
    if consumed_points > 0 {
        channel.points_head += consumed_points;
        compact_consumed_prefix(&mut channel.points, &mut channel.points_head);
    }
    let consumed_discontinuities = active_discontinuities(channel).partition_point(|source_time| {
        *source_time < keep_after && !delay_time_matches(*source_time, keep_after)
    });
    if consumed_discontinuities > 0 {
        if consumed_discontinuities % 2 == 1 {
            channel.pruned_discontinuity_parity = !channel.pruned_discontinuity_parity;
        }
        channel.discontinuity_head += consumed_discontinuities;
        compact_consumed_prefix(
            &mut channel.discontinuity_times,
            &mut channel.discontinuity_head,
        );
        if channel.suppressed_discontinuity.is_some_and(|suppressed| {
            active_discontinuities(channel)
                .first()
                .is_none_or(|first| suppressed.source_time < *first)
        }) {
            channel.suppressed_discontinuity = None;
        }
    }
}

fn min_positive_delay(delays: &[f64]) -> Option<f64> {
    delays
        .iter()
        .copied()
        .filter(|delay| *delay > 0.0 && delay.is_finite())
        .min_by(|left, right| left.total_cmp(right))
}

fn delay_time_matches(left: f64, right: f64) -> bool {
    if left == right {
        return true;
    }
    if !left.is_finite() || !right.is_finite() {
        return false;
    }
    let ulp = local_time_ulp(left).max(local_time_ulp(right));
    ulp > 0.0 && (left - right).abs() <= 2.0 * ulp
}

fn local_time_ulp(value: f64) -> f64 {
    [
        (value.next_up() - value).abs(),
        (value - value.next_down()).abs(),
    ]
    .into_iter()
    .filter(|spacing| spacing.is_finite())
    .fold(0.0, f64::max)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    struct RootKernel {
        inner: Rc<dyn CompiledSolveExpression>,
        calls: Rc<Cell<usize>>,
        fault: Rc<Cell<u8>>,
        span: rumoca_core::Span,
    }

    impl CompiledSolveExpression for RootKernel {
        fn call(
            &self,
            y: &[f64],
            p: &[f64],
            t: f64,
            tables: &[rumoca_core::ExternalTableData],
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            self.calls.set(self.calls.get() + 1);
            match self.fault.get() {
                1 => {
                    out[0] = 777.0;
                    return Err(RuntimeSolveError::SourceFault {
                        message: "issued delay-time source fault".into(),
                        span: Some(self.span),
                    });
                }
                2 => {
                    out[0] = 777.0;
                    return Err(RuntimeSolveError::CompiledExecution {
                        context: "delay-time test kernel",
                        message: "issued delay-time backend fault".into(),
                        source_spans: vec![self.span].into(),
                    });
                }
                _ => {}
            }
            self.inner.call(y, p, t, tables, out)
        }
    }

    fn root_partition() -> solve::SolveDelayPartition {
        let delay = scalar_row(vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: delay.clone(),
            delay_max_rhs: delay,
            value_parameter_indices: vec![1],
            source_is_discrete: vec![true],
        }
    }

    fn compiled_root_runtime() -> (DelayRuntime, Rc<Cell<usize>>, Rc<Cell<u8>>) {
        let partition = root_partition();
        let backend = crate::runtime::solve_runtime::test_kernels::ReferenceBackend;
        let mut runtime = DelayRuntime::new_with_backend(&partition, Some(&backend)).unwrap();
        let calls = Rc::new(Cell::new(0));
        let fault = Rc::new(Cell::new(0));
        let DelayExecution::Compiled { time, .. } = &mut runtime.execution else {
            panic!("test backend must prepare compiled delay code");
        };
        *time = Rc::new(RootKernel {
            inner: time.clone(),
            calls: calls.clone(),
            fault: fault.clone(),
            span: partition.delay_time_rhs.program_span(0).unwrap(),
        });
        (runtime, calls, fault)
    }

    fn commit_root_history(runtime: &DelayRuntime) -> [f64; 2] {
        let mut params = [0.0, 0.0];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .unwrap();
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .unwrap();
        params[0] = 1.0;
        runtime
            .commit(1.0, &[], &params, RowEvalContext::default())
            .unwrap();
        params
    }

    #[test]
    fn compiled_delay_event_roots_execute_prepared_time_code() {
        let (compiled, calls, _) = compiled_root_runtime();
        let mut roots = [42.0];
        compiled
            .evaluate_event_roots(0.0, &[], &[0.0, 0.0], RowEvalContext::default(), &mut roots)
            .unwrap();
        assert_eq!(roots, [1.0]);
        assert_eq!(
            calls.get(),
            0,
            "uninitialized history needs no source evaluation"
        );
        let reference = DelayRuntime::new(&root_partition()).unwrap();
        let params = commit_root_history(&compiled);
        assert_eq!(params, commit_root_history(&reference));
        calls.set(0);
        for time in [1.1, 1.2, 1.3] {
            let mut expected = [0.0];
            compiled
                .evaluate_event_roots(time, &[], &params, RowEvalContext::default(), &mut roots)
                .unwrap();
            reference
                .evaluate_event_roots(time, &[], &params, RowEvalContext::default(), &mut expected)
                .unwrap();
            assert_eq!(roots.map(f64::to_bits), expected.map(f64::to_bits));
        }
        assert_eq!(
            calls.get(),
            3,
            "each root query must execute its prepared delay-time kernel"
        );
    }

    #[test]
    fn compiled_delay_event_root_faults_preserve_category_and_history() {
        let (runtime, calls, fault) = compiled_root_runtime();
        let params = commit_root_history(&runtime);
        let snapshot = runtime.snapshot();
        let span = root_partition().delay_time_rhs.program_span(0).unwrap();
        for kind in [1, 2] {
            calls.set(0);
            fault.set(kind);
            let mut roots = [42.0];
            let result = runtime.evaluate_event_roots(
                1.1,
                &[],
                &params,
                RowEvalContext::default(),
                &mut roots,
            );
            assert_eq!(
                calls.get(),
                1,
                "the admitted kernel must execute before returning its fault"
            );
            let error = result.unwrap_err();
            match (kind, error) {
                (
                    1,
                    RuntimeSolveError::SourceFault {
                        span: Some(issued), ..
                    },
                ) => assert_eq!(issued, span),
                (2, RuntimeSolveError::CompiledExecution { source_spans, .. }) => {
                    assert_eq!(source_spans.as_ref(), &[span])
                }
                (_, error) => panic!("fault category changed: {error}"),
            }
            assert_eq!(
                roots,
                [42.0],
                "failed kernel output must not publish a root"
            );
            assert!(
                runtime.state.borrow().bit_eq(&snapshot.0),
                "root failure must not change accepted history"
            );
        }
    }

    #[test]
    fn compiled_delay_rows_preserve_the_accepted_timeline() {
        let delay = scalar_row(vec![
            solve::LinearOp::Const { dst: 0, value: 0.5 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let partition = solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: delay.clone(),
            delay_max_rhs: delay,
            value_parameter_indices: vec![1],
            source_is_discrete: vec![true],
        };
        let backend = crate::runtime::solve_runtime::test_kernels::ReferenceBackend;
        let compiled = DelayRuntime::new_with_backend(&partition, Some(&backend)).unwrap();
        assert!(matches!(
            compiled.execution,
            DelayExecution::Compiled { .. }
        ));
        let reference = DelayRuntime::new(&partition).unwrap();
        let mut compiled_p = [1.0, 0.0];
        let mut reference_p = compiled_p;
        for (time, value) in [(0.0, 1.0), (1.0, 2.0), (1.25, 2.0), (1.5, 3.0), (2.0, 3.0)] {
            compiled_p[0] = value;
            reference_p[0] = value;
            for (runtime, params) in [(&compiled, &mut compiled_p), (&reference, &mut reference_p)]
            {
                runtime
                    .refresh(time, &[], params, RowEvalContext::default())
                    .unwrap();
                runtime
                    .commit(time, &[], params, RowEvalContext::default())
                    .unwrap();
            }
            assert_eq!(compiled_p.map(f64::to_bits), reference_p.map(f64::to_bits));
        }
    }

    fn scalar_row(ops: Vec<solve::LinearOp>) -> solve::ScalarProgramBlock {
        solve::ScalarProgramBlock::with_source_span(
            vec![ops],
            solve::source_span_from_offsets(52, 0, 1)
                .require_provenance("delay runtime fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable")
    }

    #[test]
    fn continuous_history_interpolates_between_accepted_points() {
        let channel = DelayChannelHistory {
            points: vec![
                DelayPoint {
                    time: 0.0,
                    value: 0.0,
                },
                DelayPoint {
                    time: 1.0,
                    value: 2.0,
                },
            ],
            points_head: 0,
            delay_time: 0.5,
            delay_max: 1.0,
            accepted_query_time: 0.5,
            discontinuity_times: Vec::new(),
            discontinuity_head: 0,
            pruned_discontinuity_parity: false,
            suppressed_discontinuity: None,
        };

        assert!((delayed_value(&channel, 1.25, 0.5, 3.0, false) - 1.5).abs() <= 1.0e-12);
    }

    /// A smooth history is read back with the Lagrange stencil, exactly for a
    /// quadratic; an equal-time jump pair keeps the stencil on its own side.
    #[test]
    fn smooth_history_reads_back_a_quadratic_exactly_without_crossing_jumps() {
        let quadratic = |t: f64| 1.0 - 1.5 * t + 0.5 * t * t;
        let point = |time: f64| DelayPoint {
            time,
            value: quadratic(time),
        };
        let mut channel = DelayChannelHistory {
            points: vec![point(0.0), point(0.3), point(0.7), point(1.0)],
            delay_time: 0.5,
            delay_max: 1.0,
            ..DelayChannelHistory::default()
        };
        let value = delayed_value(&channel, 1.0, 0.5, quadratic(1.5), false);
        assert!((value - quadratic(0.5)).abs() <= 1.0e-14, "{value}");

        // A jump at t = 0.7 (left and right values): the query on its left
        // side interpolates only the points before it.
        channel.points.insert(
            3,
            DelayPoint {
                time: 0.7,
                value: 10.0,
            },
        );
        let value = delayed_value(&channel, 1.0, 0.5, quadratic(1.5), false);
        assert!((value - quadratic(0.5)).abs() <= 1.0e-14, "{value}");
    }

    #[test]
    fn large_absolute_times_do_not_merge_distinct_accepted_samples() {
        let start = 1.0e15;
        let mut channel = DelayChannelHistory {
            points: vec![DelayPoint {
                time: start,
                value: 0.0,
            }],
            points_head: 0,
            delay_time: 0.0,
            delay_max: 10.0,
            accepted_query_time: start,
            discontinuity_times: Vec::new(),
            discontinuity_head: 0,
            pruned_discontinuity_parity: false,
            suppressed_discontinuity: None,
        };

        assert!(!append_accepted_point(
            &mut channel,
            start + 1.0,
            1.0,
            false
        ));
        assert_eq!(active_points(&channel).len(), 2);
        assert!(!delay_time_matches(start, start + 1.0));
        assert!((delayed_value(&channel, start + 0.5, 0.0, 1.0, false) - 0.5).abs() <= 1.0e-12);
    }

    #[test]
    fn history_pruning_advances_a_logical_head_before_compaction() {
        let mut channel = DelayChannelHistory {
            points: (0..200)
                .map(|time| DelayPoint {
                    time: time as f64,
                    value: time as f64,
                })
                .collect(),
            points_head: 0,
            delay_time: 0.0,
            delay_max: 198.0,
            accepted_query_time: 199.0,
            discontinuity_times: Vec::new(),
            discontinuity_head: 0,
            pruned_discontinuity_parity: false,
            suppressed_discontinuity: None,
        };

        prune_history(&mut channel, 200.0);

        assert_eq!(channel.points_head, 1);
        assert_eq!(channel.points.len(), 200);
        assert_eq!(active_points(&channel)[0].time, 1.0);
    }

    #[test]
    fn discrete_history_uses_right_limit_at_delayed_event() {
        let channel = DelayChannelHistory {
            points: vec![
                DelayPoint {
                    time: 0.0,
                    value: 0.0,
                },
                DelayPoint {
                    time: 1.0,
                    value: 0.0,
                },
                DelayPoint {
                    time: 1.0,
                    value: 1.0,
                },
            ],
            points_head: 0,
            delay_time: 0.5,
            delay_max: 1.0,
            accepted_query_time: 1.0,
            discontinuity_times: Vec::new(),
            discontinuity_head: 0,
            pruned_discontinuity_parity: false,
            suppressed_discontinuity: None,
        };

        assert_eq!(delayed_value(&channel, 1.5, 0.5, 1.0, true), 1.0);
        assert_eq!(
            delayed_value(&channel, 1.5_f64.next_down(), 0.5, 1.0, true),
            0.0
        );
    }

    #[test]
    fn discrete_source_change_exposes_and_applies_delayed_discontinuity_root() {
        let delay = scalar_row(vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: delay.clone(),
            delay_max_rhs: delay,
            value_parameter_indices: vec![1],
            source_is_discrete: vec![true],
        })
        .expect("valid delay partition should prepare");
        let mut params = vec![0.0, 0.0];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .expect("history should initialize");
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .expect("initialized history should commit");

        params[0] = 1.0;
        runtime
            .commit(1.0, &[], &params, RowEvalContext::default())
            .expect("accepted discrete change should commit");
        let mut roots = [0.0];
        runtime
            .evaluate_event_roots(1.1, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("delay event root should evaluate");
        assert!((roots[0].abs() - 0.1).abs() <= 1.0e-12);
        runtime
            .evaluate_event_roots(1.2, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("delay event root should evaluate at the discontinuity");
        assert!(roots[0].abs() <= 1.0e-12);

        runtime
            .refresh(
                1.2_f64.next_down(),
                &[],
                &mut params,
                RowEvalContext::default(),
            )
            .expect("left limit should evaluate");
        assert_eq!(params[1], 0.0);
        runtime
            .refresh(1.2, &[], &mut params, RowEvalContext::default())
            .expect("event value should evaluate");
        assert_eq!(params[1], 1.0);
        runtime
            .commit(1.2, &[], &params, RowEvalContext::default())
            .expect("delayed discontinuity should commit");
        runtime
            .evaluate_event_roots(1.2, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("consumed delay root should evaluate");
        assert_eq!(roots[0].abs(), 1.0);
    }

    #[test]
    fn snapshot_restore_rewinds_consumed_delay_discontinuity() {
        let delay = scalar_row(vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: delay.clone(),
            delay_max_rhs: delay,
            value_parameter_indices: vec![1],
            source_is_discrete: vec![true],
        })
        .expect("valid delay partition should prepare");
        let mut params = vec![0.0, 0.0];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .expect("history should initialize");
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .expect("initial history should commit");
        params[0] = 1.0;
        runtime
            .commit(1.0, &[], &params, RowEvalContext::default())
            .expect("source discontinuity should commit");
        let saved = runtime.snapshot();

        runtime
            .commit(1.2, &[], &params, RowEvalContext::default())
            .expect("delayed crossing should be consumed");
        let mut roots = [0.0];
        runtime
            .evaluate_event_roots(1.2, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("consumed root should evaluate");
        assert_eq!(roots[0].abs(), 1.0);

        runtime.restore(&saved);
        runtime
            .evaluate_event_roots(1.2, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("restored root should evaluate");
        assert!(roots[0].abs() <= 1.0e-12);
    }

    #[test]
    fn continuous_source_event_jump_exposes_delayed_discontinuity_root() {
        let delay = scalar_row(vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: delay.clone(),
            delay_max_rhs: delay,
            value_parameter_indices: vec![1],
            source_is_discrete: vec![false],
        })
        .expect("valid continuous delay partition should prepare");
        let mut params = vec![0.0, 0.0];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .expect("history should initialize");
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .expect("initial history should commit");
        runtime
            .commit(1.0, &[], &params, RowEvalContext::default())
            .expect("event left limit should commit");
        params[0] = 1.0;
        runtime
            .commit(1.0, &[], &params, RowEvalContext::default())
            .expect("event right-limit jump should commit");

        let mut roots = [0.0];
        runtime
            .evaluate_event_roots(1.2, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("transported continuous jump root should evaluate");
        assert!(roots[0].abs() <= 1.0e-12);
    }

    #[test]
    fn event_entry_holds_the_left_limit_at_an_adopted_event_coordinate() {
        let delay = scalar_row(vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: delay.clone(),
            delay_max_rhs: delay,
            value_parameter_indices: vec![1],
            source_is_discrete: vec![false],
        })
        .expect("valid continuous delay partition should prepare");
        let mut params = vec![0.0, 0.0];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .expect("history should initialize");
        // Holding before the history is committed leaves initialization alone.
        runtime.hold_accepted_history_to(0.5);
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .expect("initial history should commit");
        // The last completed step ends a roundoff short of the event at 1.
        let last_step = 1.0 - 2.0e-14;
        runtime
            .commit(last_step, &[], &params, RowEvalContext::default())
            .expect("last accepted step should commit");
        runtime.hold_accepted_history_to(1.0);
        params[0] = 1.0;
        runtime
            .commit(1.0, &[], &params, RowEvalContext::default())
            .expect("event right-limit jump should commit");

        let mut roots = [0.0];
        runtime
            .evaluate_event_roots(1.1, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("transported jump root should evaluate");
        let before = roots[0];
        runtime
            .evaluate_event_roots(1.3, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("transported jump root should evaluate");
        assert!(
            before * roots[0] < 0.0,
            "the jump at the adopted event coordinate is a discontinuity root at 1.2"
        );
    }

    #[test]
    fn variable_delay_root_uses_delay_at_query_time() {
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_max_rhs: scalar_row(vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            value_parameter_indices: vec![1],
            source_is_discrete: vec![true],
        })
        .expect("valid variable delay partition should prepare");
        let mut params = vec![0.0, 0.0, 0.2];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .expect("history should initialize");
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .expect("initialized history should commit");
        params[0] = 1.0;
        runtime
            .commit(1.0, &[], &params, RowEvalContext::default())
            .expect("accepted source discontinuity should commit");

        params[2] = 0.4;
        let mut roots = [0.0];
        runtime
            .evaluate_event_roots(1.2, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("variable delay root should evaluate");
        assert!((roots[0].abs() - 0.2).abs() <= 1.0e-12);
        runtime
            .evaluate_event_roots(1.4, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("variable delay root should evaluate");
        assert!(roots[0].abs() <= 1.0e-12);
        runtime
            .commit(1.4, &[], &params, RowEvalContext::default())
            .expect("forward delayed crossing should commit");
        runtime
            .evaluate_event_roots(1.4, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("accepted zero root should be suppressed");
        assert_eq!(roots[0].abs(), 1.0);

        params[2] = 0.6;
        runtime
            .evaluate_event_roots(1.5, &[], &params, RowEvalContext::default(), &mut roots)
            .expect("backward delayed crossing should remain visible");
        assert!(
            roots[0] > 0.0,
            "query-time reversal must cross the retained source discontinuity"
        );
    }

    #[test]
    fn accepted_backward_delay_crossing_suppresses_the_left_side() {
        let mut channel = DelayChannelHistory {
            points: Vec::new(),
            points_head: 0,
            delay_time: 0.5,
            delay_max: 1.0,
            accepted_query_time: 1.1,
            discontinuity_times: vec![1.0],
            discontinuity_head: 0,
            pruned_discontinuity_parity: false,
            suppressed_discontinuity: None,
        };

        update_suppressed_discontinuity(&mut channel, 1.0);
        assert_eq!(discrete_delay_root(&channel, 1.0), 1.0);
        assert!(
            discrete_delay_root(&channel, 0.9) > 0.0,
            "the accepted left interval must retain the root's suppressed sign"
        );
    }

    #[test]
    fn initialization_uses_current_source_and_runtime_clones_share_history() {
        let delay = scalar_row(vec![
            solve::LinearOp::Const { dst: 0, value: 0.2 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: delay.clone(),
            delay_max_rhs: delay,
            value_parameter_indices: vec![1],
            source_is_discrete: vec![false],
        })
        .expect("valid delay partition should prepare");
        let clone = runtime.clone();
        let mut params = vec![1.0, 0.0];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .expect("history should initialize");

        params[0] = 2.0;
        clone
            .refresh(0.0, &[], &mut params, RowEvalContext::default())
            .expect("initial delay value should follow the current source");
        assert_eq!(params[1], 2.0);
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .expect("converged initial history should commit");

        params[0] = 3.0;
        clone
            .refresh(0.1, &[], &mut params, RowEvalContext::default())
            .expect("clone should read the shared committed history");
        assert_eq!(params[1], 2.0);
    }

    #[test]
    fn speculative_variable_delay_does_not_change_accepted_step_limit() {
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_max_rhs: scalar_row(vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            value_parameter_indices: vec![1],
            source_is_discrete: vec![false],
        })
        .expect("valid delay partition should prepare");
        let mut params = vec![1.0, 0.0, 0.2];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .expect("history should initialize");
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .expect("initial history should commit");
        assert_eq!(runtime.accepted_step_limit(), Some(0.2));

        params[2] = 0.8;
        runtime
            .refresh(0.1, &[], &mut params, RowEvalContext::default())
            .expect("speculative delay should evaluate");
        assert_eq!(
            runtime.accepted_step_limit(),
            Some(0.2),
            "only an accepted point may change the method-of-steps limit"
        );
    }
    #[test]
    fn variable_zero_delay_reads_current_discrete_source() {
        let runtime = DelayRuntime::new(&solve::SolveDelayPartition {
            source_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_time_rhs: scalar_row(vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            delay_max_rhs: scalar_row(vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            value_parameter_indices: vec![1],
            source_is_discrete: vec![true],
        })
        .unwrap();
        let mut params = vec![0.0, 0.0, 0.2];
        runtime
            .initialize(0.0, &[], &mut params, RowEvalContext::default())
            .unwrap();
        runtime
            .commit(0.0, &[], &params, RowEvalContext::default())
            .unwrap();
        params[0] = 1.0;
        params[2] = 0.0;
        runtime
            .refresh(0.1, &[], &mut params, RowEvalContext::default())
            .unwrap();
        assert_eq!(
            params[1], 1.0,
            "zero delay must read the current source before accepting its point"
        );
    }
}
