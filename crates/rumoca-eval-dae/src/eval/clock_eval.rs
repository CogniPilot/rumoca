use super::*;

#[derive(Debug, Clone, Copy)]
pub(super) struct ClockTiming {
    pub(super) period: f64,
    pub(super) phase: f64,
}

pub(super) fn eval_time_seconds<T: SimFloat>(env: &VarEnv<T>) -> Result<f64, EvalError> {
    Ok(env.require("time")?.real())
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

pub(super) fn eval_positive_factor<T: SimFloat>(
    arg: Option<&rumoca_core::Expression>,
    env: &VarEnv<T>,
) -> Result<Option<f64>, EvalError> {
    let Some(expr) = arg else {
        return Ok(None);
    };
    let raw = eval_expr::<T>(expr, env)?.real();
    let rounded = raw.round();
    Ok((rounded.is_finite() && rounded > 0.0).then_some(rounded))
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
            env.clock_intervals
                .get(name.as_str())
                .and_then(|period| valid_positive_period(*period))
                .map(|period| ClockTiming { period, phase: 0.0 })
                .map(Some)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "clock timing",
                })
                .or(Ok(None))
        }
        _ => Ok(None),
    }
}

pub(super) fn infer_clock_counter_form<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<Option<f64>, EvalError> {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return Ok(None);
    };
    let short_name = name.last_segment();
    if short_name != "Clock" || args.len() != 1 {
        return Ok(None);
    }

    let raw = eval_expr::<T>(&args[0], env)?.real();
    let rounded = raw.round();
    let tol = 1.0e-9 * rounded.abs().max(1.0);
    if !rounded.is_finite() || rounded <= 0.0 || (raw - rounded).abs() > tol {
        return Ok(None);
    }
    Ok(Some(rounded))
}

pub(super) fn infer_clock_timing_from_call<T: SimFloat>(
    short_name: &str,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<ClockTiming>, EvalError> {
    match short_name {
        "Clock" => {
            let Some(first) = args.first() else {
                return Ok(None);
            };
            if let Some(base) = infer_clock_timing_from_expr(first, env)? {
                return Ok(Some(base));
            }

            if args.len() >= 2 {
                let count = eval_expr::<T>(first, env)?.real();
                let resolution = eval_expr::<T>(&args[1], env)?.real();
                if resolution.is_finite() && resolution > 0.0 {
                    return Ok(valid_positive_period(count / resolution)
                        .map(|period| ClockTiming { period, phase: 0.0 }));
                }
            }

            let period = eval_expr::<T>(first, env)?.real();
            Ok(valid_positive_period(period).map(|period| ClockTiming { period, phase: 0.0 }))
        }
        "subSample" => {
            // MLS exact-clock construction pattern used by PeriodicExactClock:
            // c = subSample(Clock(factor), resolutionFactor)
            // corresponds to period = factor / resolutionFactor.
            if let Some(counter) = match args.first() {
                Some(expr) => infer_clock_counter_form(expr, env)?,
                None => None,
            } {
                let resolution = eval_positive_factor(args.get(1), env)?.unwrap_or(1.0);
                return Ok(valid_positive_period(counter / resolution)
                    .map(|period| ClockTiming { period, phase: 0.0 }));
            }

            let Some(base) = infer_clock_constructor_timing_from_expr(
                args.first().ok_or(EvalError::UnsupportedExpression {
                    kind: "clock constructor",
                })?,
                env,
            )?
            else {
                return Ok(None);
            };
            let factor = eval_positive_factor(args.get(1), env)?.unwrap_or(1.0);
            Ok(
                valid_positive_period(base.period * factor).map(|period| ClockTiming {
                    period,
                    phase: base.phase,
                }),
            )
        }
        "superSample" => {
            let Some(base) = infer_clock_constructor_timing_from_expr(
                args.first().ok_or(EvalError::UnsupportedExpression {
                    kind: "clock constructor",
                })?,
                env,
            )?
            else {
                return Ok(None);
            };
            let factor = eval_positive_factor(args.get(1), env)?.unwrap_or(1.0);
            Ok(
                valid_positive_period(base.period / factor).map(|period| ClockTiming {
                    period,
                    phase: base.phase,
                }),
            )
        }
        "shiftSample" | "backSample" => {
            let first = args.first().ok_or(EvalError::UnsupportedExpression {
                kind: "clock constructor",
            })?;
            let Some(base) = infer_clock_constructor_timing_from_expr(first, env)? else {
                return Ok(None);
            };
            let shift = eval_expr::<T>(args.get(1).unwrap_or(first), env)?.real();
            let offset = if args.len() >= 3 {
                let resolution = eval_expr::<T>(&args[2], env)?.real();
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
            Ok(valid_positive_period(base.period).map(|period| ClockTiming { period, phase }))
        }
        _ => Ok(None),
    }
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
        [_internal_id, start, interval, ..] => {
            // Internal lowered representation may encode sample identifiers as
            // the first argument: sample(id, start, interval).
            let start_t = eval_expr::<T>(start, env)?.real();
            let period = eval_expr::<T>(interval, env)?.real();
            let Some(period) = valid_positive_period(period) else {
                return Err(EvalError::UnsupportedExpression {
                    kind: "sample interval",
                });
            };
            let timing = ClockTiming {
                period,
                phase: start_t,
            };
            clock_tick_value(env, timing)
        }
        [start, interval, ..] => {
            let start_t = eval_expr::<T>(start, env)?.real();
            let period = eval_expr::<T>(interval, env)?.real();
            let Some(period) = valid_positive_period(period) else {
                return Err(EvalError::UnsupportedExpression {
                    kind: "sample interval",
                });
            };
            let timing = ClockTiming {
                period,
                phase: start_t,
            };
            clock_tick_value(env, timing)
        }
    }
}

pub fn infer_clock_timing_seconds(
    expr: &rumoca_core::Expression,
    env: &VarEnv<f64>,
) -> Result<Option<(f64, f64)>, EvalError> {
    Ok(infer_clock_timing_from_expr(expr, env)?.map(|timing| (timing.period, timing.phase)))
}
