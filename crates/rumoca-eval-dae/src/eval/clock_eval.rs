use super::*;

use rumoca_core::ClockLattice;

#[derive(Debug, Clone, Copy)]
pub(super) struct ClockTiming {
    pub(super) period: f64,
    pub(super) phase: f64,
}

impl ClockTiming {
    /// Round an exact lattice to seconds once, at the evaluator boundary.
    pub(super) fn from_lattice(lattice: ClockLattice) -> Self {
        Self {
            period: lattice.period_seconds(),
            phase: lattice.phase_seconds(),
        }
    }

    /// Recover the exact lattice behind this timing (MLS §16.5).
    pub(super) fn lattice(self) -> Result<ClockLattice, EvalError> {
        ClockLattice::from_seconds(self.period, self.phase)
            .map_err(|kind| EvalError::ClockLattice { kind })
    }
}

pub(super) fn eval_time_seconds<T: SimFloat>(env: &VarEnv<T>) -> Result<f64, EvalError> {
    Ok(env.require("time")?.real())
}

/// MLS §16.3 clock activation at `time`.
///
/// The comparison is made on the dimensionless tick coordinate with the
/// scheduler's shared relative tolerance, so this predicate recognizes exactly
/// the instants that `rumoca_solver::timeline::periodic_schedule_matches_time`
/// and the lowered clock rows recognize. Using a different tolerance here is
/// what let interpreted clocked partitions tick on instants the scheduler had
/// not stopped at.
fn is_clock_tick(time: f64, period: f64, phase: f64) -> bool {
    if !time.is_finite() || !period.is_finite() || !phase.is_finite() || period <= 0.0 {
        return false;
    }

    let ticks = (time - phase) / period;
    if ticks < 0.0 {
        return false;
    }
    let nearest = ticks.round();
    (ticks - nearest).abs()
        <= rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE * (1.0 + ticks.abs().max(nearest.abs()))
}

pub(super) fn clock_tick_value<T: SimFloat>(
    env: &VarEnv<T>,
    timing: ClockTiming,
) -> Result<T, EvalError> {
    Ok(T::from_bool(is_clock_tick(
        eval_time_seconds(env)?,
        timing.period,
        timing.phase,
    )))
}

pub(super) fn valid_positive_period(period: f64) -> Option<f64> {
    (period.is_finite() && period > 0.0).then_some(period)
}

/// MLS §16.5: a statically periodic clock is a rational lattice point. Seconds
/// are rationalized once here so every derived-clock conversion below is exact
/// integer arithmetic.
fn periodic_timing_from_seconds(period: f64, phase: f64) -> Result<Option<ClockTiming>, EvalError> {
    let Some(period) = valid_positive_period(period) else {
        return Ok(None);
    };
    ClockLattice::from_seconds(period, phase)
        .map(ClockTiming::from_lattice)
        .map(Some)
        .map_err(|kind| EvalError::ClockLattice { kind })
}

/// Accept an evaluated scalar as a strictly positive MLS clock factor.
fn positive_integer(value: f64) -> Result<Option<i128>, EvalError> {
    let rounded = value.round();
    if !rounded.is_finite() || rounded <= 0.0 {
        return Ok(None);
    }
    integer_in_range(rounded)
}

/// Accept an evaluated scalar as a strictly positive MLS clock counter only
/// when it really is an integer, within the shared clock-factor tolerance.
fn exact_positive_integer(value: f64) -> Result<Option<i128>, EvalError> {
    let rounded = value.round();
    let tolerance = rumoca_core::CLOCK_FACTOR_INTEGER_TOLERANCE * rounded.abs().max(1.0);
    if (value - rounded).abs() > tolerance {
        return Ok(None);
    }
    positive_integer(rounded)
}

/// Accept an evaluated scalar as a non-negative MLS shift counter.
fn non_negative_integer(value: f64) -> Result<Option<i128>, EvalError> {
    let rounded = value.round();
    if !rounded.is_finite() || rounded < 0.0 {
        return Ok(None);
    }
    integer_in_range(rounded)
}

fn integer_in_range(rounded: f64) -> Result<Option<i128>, EvalError> {
    if rounded < i128::MIN as f64 || rounded >= i128::MAX as f64 {
        return Err(EvalError::ClockLattice {
            kind: rumoca_core::ClockLatticeErrorKind::IntegerOverflow,
        });
    }
    Ok(Some(rounded as i128))
}

pub(super) fn eval_positive_factor<T: SimFloat>(
    arg: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
) -> Result<Option<i128>, EvalError> {
    let Some(expr) = arg else {
        return Ok(None);
    };
    let raw = eval_expr::<T>(expr, env)?.real();
    positive_integer(raw)
}

pub(super) fn infer_clock_timing_from_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    match expr {
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            let short_name = name.last_segment();
            infer_clock_timing_from_call(short_name, args, env)
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            // MLS §16.5.1: sampled values and clocked variables keep the period
            // of their associated clock. Runtime metadata precomputes that
            // association for alias-backed explicit clock connectors.
            let Some(period) = env.clock_intervals.get(name.as_str()) else {
                return Ok(None);
            };
            periodic_timing_from_seconds(*period, 0.0)
        }
        _ => Ok(None),
    }
}

pub(super) fn infer_clock_timing_from_call<T: SimFloat>(
    short_name: &str,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    match short_name {
        "Clock" => eval_clock_constructor_timing(args, env),
        "subSample" => eval_sub_sample_timing(args, env),
        "superSample" => eval_super_sample_timing(args, env),
        "shiftSample" | "backSample" => eval_shift_like_timing(short_name, args, env),
        _ => Ok(None),
    }
}

fn eval_clock_constructor_timing<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    let Some(first) = args.first() else {
        return Ok(None);
    };
    if let Some(base) = infer_clock_timing_from_expr(first, env)? {
        return Ok(Some(base));
    }

    if args.len() >= 2 {
        // MLS §16.3 Clock(intervalCounter, resolution): an exact rational
        // period that never passes through seconds.
        let count = eval_expr::<T>(first, env)?.real();
        let resolution = eval_expr::<T>(&args[1], env)?.real();
        if resolution.is_finite() && resolution > 0.0 {
            return exact_interval_counter_timing(count, resolution);
        }
    }

    let period = eval_expr::<T>(first, env)?.real();
    periodic_timing_from_seconds(period, 0.0)
}

/// MLS §16.3 `Clock(intervalCounter, resolution)`.
///
/// With the Integer arguments the spec mandates, the period is the exact
/// rational `intervalCounter / resolution`. Non-integral arguments are not
/// legal Modelica; they keep the seconds quotient so no previously accepted
/// model changes behaviour here.
fn exact_interval_counter_timing(
    count: f64,
    resolution: f64,
) -> Result<Option<ClockTiming>, EvalError> {
    if let (Some(counter), Some(resolution)) = (
        exact_positive_integer(count)?,
        exact_positive_integer(resolution)?,
    ) {
        return ClockLattice::from_interval_counter(counter, resolution)
            .map(ClockTiming::from_lattice)
            .map(Some)
            .map_err(|kind| EvalError::ClockLattice { kind });
    }
    periodic_timing_from_seconds(count / resolution, 0.0)
}

fn eval_sub_sample_timing<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    // MLS §16.5.2 Operator 16.9: the clock of `subSample(u, factor)` is
    // `factor` times slower than the clock of `u`, so the period is multiplied.
    // MSL 4.1.0 `PeriodicExactClock` relies on exactly this when it emits
    // `subSample(Clock(factor), resolutionFactor)`, documented in its own
    // source as equivalent to `Clock(factor*resolutionFactor, 1)`.
    let Some(base) = base_clock_lattice(args.first(), env)? else {
        return Ok(None);
    };
    let factor = eval_positive_factor(args.get(1), env)?.unwrap_or(1);
    base.sub_sample(factor)
        .map(ClockTiming::from_lattice)
        .map(Some)
        .map_err(|kind| EvalError::ClockLattice { kind })
}

fn eval_super_sample_timing<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    let Some(base) = base_clock_lattice(args.first(), env)? else {
        return Ok(None);
    };
    let factor = eval_positive_factor(args.get(1), env)?.unwrap_or(1);
    base.super_sample(factor)
        .map(ClockTiming::from_lattice)
        .map(Some)
        .map_err(|kind| EvalError::ClockLattice { kind })
}

fn eval_shift_like_timing<T: SimFloat>(
    short_name: &str,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    let first = args.first().ok_or(EvalError::UnsupportedExpression {
        kind: "clock constructor",
    })?;
    let Some(base) = base_clock_lattice(Some(first), env)? else {
        return Ok(None);
    };
    let shift = eval_expr::<T>(args.get(1).unwrap_or(first), env)?.real();
    let Some(counter) = non_negative_integer(shift)? else {
        return Ok(None);
    };
    // MLS §16.5.2: shiftSample/backSample shift by `counter/resolution` of
    // interval(u) — an exact rational fraction of the source clock interval,
    // not an absolute number of seconds. `resolution` defaults to 1.
    let resolution = match args.get(2) {
        Some(expr) => {
            let Some(resolution) = positive_integer(eval_expr::<T>(expr, env)?.real())? else {
                return Ok(None);
            };
            resolution
        }
        None => 1,
    };
    let shifted = if short_name == "shiftSample" {
        base.shift_sample(counter, resolution)
    } else {
        base.back_sample(counter, resolution)
    };
    shifted
        .map(ClockTiming::from_lattice)
        .map(Some)
        .map_err(|kind| EvalError::ClockLattice { kind })
}

fn base_clock_lattice<T: SimFloat>(
    expr: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
) -> Result<Option<ClockLattice>, EvalError> {
    let expr = expr.ok_or(EvalError::UnsupportedExpression {
        kind: "clock constructor",
    })?;
    let Some(base) = infer_clock_constructor_timing_from_expr(expr, env)? else {
        return Ok(None);
    };
    base.lattice().map(Some)
}

fn infer_clock_constructor_timing_from_expr<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return Ok(None);
    };
    let short_name = name.last_segment();
    infer_clock_timing_from_call(short_name, args, env)
}

pub(super) fn eval_builtin_sample<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    match args {
        [] => Err(EvalError::UnsupportedExpression {
            kind: "sample arity",
        }),
        [value] => eval_expr::<T>(value, env),
        [value, clock, ..] if infer_clock_timing_from_expr(clock, env)?.is_some() => {
            eval_expr::<T>(value, env)
        }
        // Internal lowered representation may encode sample identifiers as the
        // first argument: sample(id, start, interval).
        [_, start, interval, ..] | [start, interval, ..] => {
            sample_start_interval_tick(start, interval, env)
        }
    }
}

/// MLS §16.5.1 `sample(start, interval)`: a periodic time event on the exact
/// rational lattice `start + k * interval`.
fn sample_start_interval_tick<T: SimFloat>(
    start: &rumoca_core::Expression,
    interval: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let phase = eval_expr::<T>(start, env)?.real();
    let period = eval_expr::<T>(interval, env)?.real();
    let timing =
        periodic_timing_from_seconds(period, phase)?.ok_or(EvalError::UnsupportedExpression {
            kind: "sample interval",
        })?;
    clock_tick_value(env, timing)
}

pub fn infer_clock_timing_seconds(
    expr: &rumoca_core::Expression,
    env: &VarEnv<f64>,
) -> Result<Option<(f64, f64)>, EvalError> {
    Ok(infer_clock_timing_from_expr(expr, env)?.map(|timing| (timing.period, timing.phase)))
}

#[cfg(test)]
mod tests;
