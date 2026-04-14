use super::*;

#[derive(Debug, Clone, Copy)]
pub(super) struct ClockTiming {
    pub(super) period: f64,
    pub(super) phase: f64,
}

pub(super) fn eval_time_seconds<T: SimFloat>(env: &VarEnv<T>) -> f64 {
    env.get("time").real()
}

fn is_clock_tick(time: f64, period: f64, phase: f64) -> bool {
    if !time.is_finite() || !period.is_finite() || !phase.is_finite() || period <= 0.0 {
        return false;
    }

    let shifted = time - phase;
    let tol = 1e-9 * period.max(1.0);
    if shifted < -tol {
        return false;
    }

    let k = (shifted / period).round();
    let nearest = k * period;
    (shifted - nearest).abs() <= tol
}

pub(super) fn clock_tick_value<T: SimFloat>(env: &VarEnv<T>, timing: ClockTiming) -> T {
    T::from_bool(is_clock_tick(
        eval_time_seconds(env),
        timing.period,
        timing.phase,
    ))
}

pub(super) fn valid_positive_period(period: f64) -> Option<f64> {
    (period.is_finite() && period > 0.0).then_some(period)
}

pub(super) fn eval_positive_factor<T: SimFloat>(
    arg: Option<&dae::Expression>,
    env: &VarEnv<T>,
) -> Option<f64> {
    let raw = arg.map(|expr| eval_expr::<T>(expr, env).real())?;
    let rounded = raw.round();
    (rounded.is_finite() && rounded > 0.0).then_some(rounded)
}

pub(super) fn infer_clock_timing_from_expr<T: SimFloat>(
    expr: &dae::Expression,
    env: &VarEnv<T>,
) -> Option<ClockTiming> {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            infer_clock_timing_from_call(short_name, args, env)
        }
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            // MLS §16.5.1: sampled values and clocked variables keep the period
            // of their associated clock. Runtime metadata precomputes that
            // association for alias-backed explicit clock connectors.
            env.clock_intervals
                .get(name.as_str())
                .and_then(|period| valid_positive_period(*period))
                .map(|period| ClockTiming { period, phase: 0.0 })
        }
        _ => None,
    }
}

pub(super) fn infer_clock_counter_form<T: SimFloat>(
    expr: &dae::Expression,
    env: &VarEnv<T>,
) -> Option<f64> {
    let dae::Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short_name != "Clock" || args.len() != 1 {
        return None;
    }

    let raw = eval_expr::<T>(&args[0], env).real();
    let rounded = raw.round();
    let tol = 1.0e-9 * rounded.abs().max(1.0);
    if !rounded.is_finite() || rounded <= 0.0 || (raw - rounded).abs() > tol {
        return None;
    }
    Some(rounded)
}

pub(super) fn infer_clock_timing_from_call<T: SimFloat>(
    short_name: &str,
    args: &[dae::Expression],
    env: &VarEnv<T>,
) -> Option<ClockTiming> {
    match short_name {
        "Clock" => {
            let first = args.first()?;
            if let Some(base) = infer_clock_timing_from_expr(first, env) {
                return Some(base);
            }

            if args.len() >= 2 {
                let count = eval_expr::<T>(first, env).real();
                let resolution = eval_expr::<T>(&args[1], env).real();
                if resolution.is_finite() && resolution > 0.0 {
                    return valid_positive_period(count / resolution)
                        .map(|period| ClockTiming { period, phase: 0.0 });
                }
            }

            let period = eval_expr::<T>(first, env).real();
            valid_positive_period(period).map(|period| ClockTiming { period, phase: 0.0 })
        }
        "subSample" => {
            // MLS exact-clock construction pattern used by PeriodicExactClock:
            // c = subSample(Clock(factor), resolutionFactor)
            // corresponds to period = factor / resolutionFactor.
            if let Some(counter) = args
                .first()
                .and_then(|expr| infer_clock_counter_form(expr, env))
            {
                let resolution = eval_positive_factor(args.get(1), env).unwrap_or(1.0);
                return valid_positive_period(counter / resolution)
                    .map(|period| ClockTiming { period, phase: 0.0 });
            }

            let base = infer_clock_timing_from_expr(args.first()?, env)?;
            let factor = eval_positive_factor(args.get(1), env).unwrap_or(1.0);
            valid_positive_period(base.period * factor).map(|period| ClockTiming {
                period,
                phase: base.phase,
            })
        }
        "superSample" => {
            let base = infer_clock_timing_from_expr(args.first()?, env)?;
            let factor = eval_positive_factor(args.get(1), env).unwrap_or(1.0);
            valid_positive_period(base.period / factor).map(|period| ClockTiming {
                period,
                phase: base.phase,
            })
        }
        "shiftSample" | "backSample" => {
            let base = infer_clock_timing_from_expr(args.first()?, env)?;
            let shift = eval_expr::<T>(args.get(1).unwrap_or(args.first()?), env).real();
            let offset = if args.len() >= 3 {
                let resolution = eval_expr::<T>(&args[2], env).real();
                if resolution.is_finite() && resolution != 0.0 {
                    // MLS §16.5.2: shiftSample/backSample shift by a fraction
                    // of interval(u), not by an absolute number of seconds.
                    (shift / resolution) * base.period
                } else {
                    shift * base.period
                }
            } else {
                shift * base.period
            };
            let phase = if short_name == "shiftSample" {
                base.phase + offset
            } else {
                base.phase - offset
            };
            valid_positive_period(base.period).map(|period| ClockTiming { period, phase })
        }
        _ => None,
    }
}

pub(super) fn eval_builtin_sample<T: SimFloat>(args: &[dae::Expression], env: &VarEnv<T>) -> T {
    match args {
        [] => T::zero(),
        [value] => eval_expr::<T>(value, env),
        [value, clock, ..] if infer_clock_timing_from_expr(clock, env).is_some() => {
            eval_expr::<T>(value, env)
        }
        [_internal_id, start, interval, ..] => {
            // Internal lowered representation may encode sample identifiers as
            // the first argument: sample(id, start, interval).
            let start_t = eval_expr::<T>(start, env).real();
            let period = eval_expr::<T>(interval, env).real();
            let Some(period) = valid_positive_period(period) else {
                return T::zero();
            };
            let timing = ClockTiming {
                period,
                phase: start_t,
            };
            clock_tick_value(env, timing)
        }
        [start, interval, ..] => {
            let start_t = eval_expr::<T>(start, env).real();
            let period = eval_expr::<T>(interval, env).real();
            let Some(period) = valid_positive_period(period) else {
                return T::zero();
            };
            let timing = ClockTiming {
                period,
                phase: start_t,
            };
            clock_tick_value(env, timing)
        }
    }
}

pub fn infer_clock_timing_seconds(expr: &dae::Expression, env: &VarEnv<f64>) -> Option<(f64, f64)> {
    infer_clock_timing_from_expr(expr, env).map(|timing| (timing.period, timing.phase))
}
