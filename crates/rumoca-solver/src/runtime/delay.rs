use std::cell::RefCell;
use std::rc::Rc;

use rumoca_ir_solve as solve;

use rumoca_eval_solve::{EvalSolveError, PreparedScalarProgramBlock, RowEvalContext};

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

impl DelayRuntime {
    pub(crate) fn new(partition: &solve::SolveDelayPartition) -> Result<Self, EvalSolveError> {
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
        Ok(Self {
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

    #[cfg(any(test, kani))]
    pub(crate) fn matches_snapshot(&self, snapshot: &DelayRuntimeSnapshot) -> bool {
        self.state.borrow().bit_eq(&snapshot.0)
    }

    pub(crate) fn initialize(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), EvalSolveError> {
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
    ) -> Result<Option<f64>, EvalSolveError> {
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

    pub(crate) fn commit(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), EvalSolveError> {
        self.commit_evaluated_at(time, time, solver_y, params, context)
    }

    pub(crate) fn commit_evaluated_at(
        &self,
        accepted_time: f64,
        evaluation_time: f64,
        solver_y: &[f64],
        params: &[f64],
        context: RowEvalContext<'_>,
    ) -> Result<(), EvalSolveError> {
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
    ) -> Result<(), EvalSolveError> {
        let expected = self.event_root_count();
        if out.len() != expected {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "delay event root output expected {expected} values, got {}",
                    out.len()
                ),
                span: None,
            });
        }
        if expected == 0 {
            return Ok(());
        }
        if !self.state.borrow().initialized {
            out.fill(1.0);
            return Ok(());
        }
        let mut rows = self.row_scratch.borrow_mut();
        self.evaluate_channel_rows(time, solver_y, params, context, &mut rows)?;
        validate_delay_values(&rows.delay_times, &rows.delay_maxima)?;
        let state = self.state.borrow();
        if state.channels.len() != expected {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "delay history expected {expected} channels, got {}",
                    state.channels.len()
                ),
                span: None,
            });
        }
        for ((slot, channel), delay_time) in
            out.iter_mut().zip(&state.channels).zip(&rows.delay_times)
        {
            *slot = discrete_delay_root(channel, time - *delay_time);
        }
        Ok(())
    }

    pub(crate) fn step_limit(&self) -> Option<f64> {
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
    ) -> Result<(), EvalSolveError> {
        rows.sources.resize(self.source_rhs.len(), 0.0);
        rows.delay_times.resize(self.delay_time_rhs.len(), 0.0);
        rows.delay_maxima.resize(self.delay_max_rhs.len(), 0.0);
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

#[cfg(any(test, kani))]
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

#[cfg(any(test, kani))]
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

#[cfg(any(test, kani))]
impl DelayPoint {
    fn bit_eq(self, other: Self) -> bool {
        self.time.to_bits() == other.time.to_bits() && self.value.to_bits() == other.value.to_bits()
    }
}

#[cfg(any(test, kani))]
fn float_slice_bit_eq(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.to_bits() == right.to_bits())
}

#[cfg(any(test, kani))]
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
    for (index, (&delay_time, &delay_max)) in delay_times.iter().zip(delay_maxima).enumerate() {
        if !delay_time.is_finite() || delay_time < 0.0 {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "delay channel {index} produced invalid delayTime {delay_time}; expected a \
                     finite non-negative value"
                ),
                span: None,
            });
        }
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
    let fraction = ((query_time - lower.time) / (next.time - lower.time)).clamp(0.0, 1.0);
    lower.value + fraction * (next.value - lower.value)
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
        assert_eq!(runtime.step_limit(), Some(0.2));

        params[2] = 0.8;
        runtime
            .refresh(0.1, &[], &mut params, RowEvalContext::default())
            .expect("speculative delay should evaluate");
        assert_eq!(
            runtime.step_limit(),
            Some(0.2),
            "only an accepted point may change the method-of-steps limit"
        );
    }
}
