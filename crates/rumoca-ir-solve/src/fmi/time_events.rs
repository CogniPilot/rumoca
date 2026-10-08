//! Time events of the scalar event profile (SPEC_0044 ME-EVENT-002).
//!
//! A component announces its next time event to a Model Exchange importer
//! (`nextEventTime`, FMI 2.0.4 section 3.2.2, FMI 3.0 event mode) and a
//! Co-Simulation component stops its internal steps at it, so both read the
//! same two Solve facts: the static instants of the problem's events and the
//! periodic clock schedules. A schedule is the exact rational lattice the
//! simulation steps by; the profile carries each tick as the integer ratio
//! `(phase_numerator + k * period_numerator) / denominator`, which the
//! component divides once, as the lattice rounds a tick once.

use serde::Serialize;

use rumoca_core::ClockPhaseAnchor;

use crate::SolveModel;

/// The largest integer a double holds exactly: every numerator and the
/// denominator of a tick stay below it, so the C division rounds the tick as
/// the lattice does.
const EXACT_INTEGER_LIMIT: i128 = 1 << 53;

/// The time events a component stops at and announces.
#[derive(Debug, Serialize)]
pub(super) struct TimeEvents {
    /// Static instants of `when time >= c` events and discontinuous time
    /// relations (the problem's scheduled time events).
    instants: Vec<f64>,
    /// The periodic clocks, in clock-owner order.
    clocks: Vec<PeriodicClock>,
    /// The relative tolerance within which two instants are one instant
    /// (`SCHEDULE_TIME_RELATIVE_TOLERANCE`).
    match_tolerance: f64,
}

#[derive(Debug, Serialize)]
struct PeriodicClock {
    period: f64,
    phase: f64,
    /// Tick `k` is `(phase_numerator + k * period_numerator) / denominator`.
    phase_numerator: f64,
    period_numerator: f64,
    denominator: f64,
    /// The hidden parameter that reads 1 while this clock ticks.
    activation: usize,
}

pub(super) fn derive(model: &SolveModel) -> Result<TimeEvents, &'static str> {
    let problem = &model.problem;
    let schedules = &problem.clocks.periodic_event_schedules;
    let activations = &problem.clocks.activation_parameter_indices;
    if schedules.len() != activations.len() {
        return Err("the clock schedules and activation lanes disagree on the clock count");
    }
    let clocks = schedules
        .iter()
        .zip(activations)
        .map(|(schedule, activation)| {
            if schedule.anchor() != ClockPhaseAnchor::Absolute {
                return Err(
                    "the C profile cannot resolve a clock phase anchored at the simulation start",
                );
            }
            let lattice = schedule.lattice();
            let (period, phase) = (lattice.period(), lattice.phase());
            let exact = |value: Option<i128>| {
                value
                    .filter(|value| value.abs() <= EXACT_INTEGER_LIMIT)
                    .map(|value| value as f64)
                    .ok_or("a clock tick is not an exactly representable ratio")
            };
            Ok(PeriodicClock {
                period: schedule.period_seconds(),
                phase: schedule.phase_seconds(),
                phase_numerator: exact(phase.numerator().checked_mul(period.denominator()))?,
                period_numerator: exact(period.numerator().checked_mul(phase.denominator()))?,
                denominator: exact(phase.denominator().checked_mul(period.denominator()))?,
                activation: *activation,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(TimeEvents {
        instants: problem.events.scheduled_time_events.clone(),
        clocks,
        match_tolerance: rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE,
    })
}
