use crate::runtime::assignment::canonical_var_ref_key;
use rumoca_eval_dae::runtime::sim_float::SimFloat;
use rumoca_eval_dae::runtime::{IMPLICIT_CLOCK_ACTIVE_ENV_KEY, VarEnv, get_pre_value};
use rumoca_ir_dae as dae;

#[derive(Clone, Copy, PartialEq, Eq)]
enum ScalarEvalMode {
    Current,
    LeftLimit,
}

#[derive(Clone, Copy)]
struct FastClockTiming {
    period: f64,
    phase: f64,
}

fn eval_time_seconds(env: &VarEnv<f64>) -> f64 {
    env.get("time")
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

fn valid_positive_period(period: f64) -> Option<f64> {
    (period.is_finite() && period > 0.0).then_some(period)
}

fn clock_tick_scalar(env: &VarEnv<f64>, timing: FastClockTiming) -> f64 {
    <f64 as SimFloat>::from_bool(is_clock_tick(
        eval_time_seconds(env),
        timing.period,
        timing.phase,
    ))
}

fn static_subscript_parts(subscripts: &[dae::Subscript]) -> Option<Vec<String>> {
    let mut parts = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let idx = match subscript {
            dae::Subscript::Index(index) => *index,
            dae::Subscript::Expr(expr) => match expr.as_ref() {
                dae::Expression::Literal(dae::Literal::Integer(index)) => *index,
                dae::Expression::Literal(dae::Literal::Real(value))
                    if value.is_finite() && value.fract() == 0.0 =>
                {
                    *value as i64
                }
                _ => return None,
            },
            dae::Subscript::Colon => return None,
        };
        parts.push(idx.to_string());
    }
    Some(parts)
}

fn raw_name_has_only_static_indices(name: &str) -> bool {
    let mut rest = name;
    while let Some(start) = rest.find('[') {
        let after_start = &rest[start + 1..];
        let Some(end) = after_start.find(']') else {
            return false;
        };
        let body = &after_start[..end];
        if body.split(',').any(|part| {
            let trimmed = part.trim();
            trimmed.is_empty() || trimmed.parse::<i64>().is_err()
        }) {
            return false;
        }
        rest = &after_start[end + 1..];
    }
    true
}

fn expr_env_key(expr: &dae::Expression) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty()
                && name.as_str().contains('[')
                && !raw_name_has_only_static_indices(name.as_str())
            {
                return None;
            }
            canonical_var_ref_key(name, subscripts)
        }
        dae::Expression::FieldAccess { base, field } => {
            Some(format!("{}.{}", expr_env_key(base)?, field))
        }
        dae::Expression::Index { base, subscripts } => {
            let parts = static_subscript_parts(subscripts)?;
            Some(format!("{}[{}]", expr_env_key(base)?, parts.join(",")))
        }
        _ => None,
    }
}

fn left_limit_time_value(time: f64) -> f64 {
    if !time.is_finite() {
        return time;
    }
    if time == 0.0 {
        return -f64::from_bits(1);
    }
    let bits = time.to_bits();
    if time > 0.0 {
        f64::from_bits(bits.saturating_sub(1))
    } else {
        f64::from_bits(bits.saturating_add(1))
    }
}

fn resolve_key_value(key: &str, env: &VarEnv<f64>, mode: ScalarEvalMode) -> f64 {
    if let Some(value) = resolve_lowered_pre_key_value(key, env) {
        return value;
    }
    if key == "time" {
        return match mode {
            ScalarEvalMode::Current => env.get("time"),
            ScalarEvalMode::LeftLimit => left_limit_time_value(env.get("time")),
        };
    }
    match mode {
        ScalarEvalMode::Current => env
            .vars
            .get(key)
            .copied()
            .or_else(|| {
                env.enum_literal_ordinals
                    .get(key)
                    .map(|ordinal| *ordinal as f64)
            })
            .unwrap_or(0.0),
        // Left-limit reads prefer `pre(...)` history, but MLS/runtime semantics
        // still fall back to the current env when no pre-history exists.
        ScalarEvalMode::LeftLimit => get_pre_value(key)
            .or_else(|| {
                dae::component_base_name(key)
                    .as_deref()
                    .and_then(get_pre_value)
            })
            .or_else(|| env.vars.get(key).copied())
            .or_else(|| {
                env.enum_literal_ordinals
                    .get(key)
                    .map(|ordinal| *ordinal as f64)
            })
            .unwrap_or(0.0),
    }
}

fn resolve_lowered_pre_key_value(key: &str, env: &VarEnv<f64>) -> Option<f64> {
    let target = key.strip_prefix("__pre__.")?;
    if let Some(value) = get_pre_value(target) {
        return Some(value);
    }
    if let Some(normalized) = dae::component_base_name(target)
        && let Some(value) = get_pre_value(normalized.as_str())
    {
        return Some(value);
    }
    if target.contains('[') {
        if let Some(value) = get_pre_value(target) {
            return Some(value);
        }
        if let Some(base_name) = dae::component_base_name(target)
            && let Some(value) = get_pre_value(base_name.as_str())
        {
            return Some(value);
        }
        if let Some(static_key) = expr_env_key(&dae::Expression::VarRef {
            name: dae::VarName::new(target),
            subscripts: vec![],
        }) && let Some(value) = get_pre_value(static_key.as_str())
        {
            return Some(value);
        }
    }
    let _ = env;
    None
}

fn eval_builtin_pre_fast(arg: &dae::Expression, env: &VarEnv<f64>) -> Option<f64> {
    if matches!(
        arg,
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            ..
        }
    ) {
        // MLS §8.6: `initial()` has a false left-limit, so `edge(initial())`
        // is true during the startup event pass.
        return Some(0.0);
    }
    if matches!(
        arg,
        dae::Expression::FunctionCall { name, args, .. }
            if name.as_str().rsplit('.').next().unwrap_or(name.as_str()) == "Clock"
                && args.is_empty()
    ) {
        // MLS §16.5.1 / Appendix B: the implicit partition clock is false on
        // the event-entry left-limit, so `edge(Clock())` fires on scheduled
        // ticks instead of reading the current implicit-clock flag from the
        // right-limit env.
        return Some(0.0);
    }
    if let Some(key) = expr_env_key(arg) {
        if let Some(value) = get_pre_value(key.as_str()) {
            return Some(value);
        }
        if let Some(base_name) = dae::component_base_name(key.as_str())
            && let Some(value) = get_pre_value(&base_name)
        {
            return Some(value);
        }
    }
    eval_scalar_expr_fast_mode(arg, env, ScalarEvalMode::LeftLimit)
}

fn eval_builtin_der_fast(arg: &dae::Expression, env: &VarEnv<f64>) -> Option<f64> {
    // Match the generic runtime evaluator: lowered direct assignments may read
    // `der(x)` from the current env after event-time derivative closure has
    // materialized it there.
    let key = expr_env_key(arg)?;
    Some(resolve_key_value(
        format!("der({key})").as_str(),
        env,
        ScalarEvalMode::Current,
    ))
}

fn eval_previous_fast(arg: &dae::Expression, env: &VarEnv<f64>) -> Option<f64> {
    if let Some(key) = expr_env_key(arg) {
        if let Some(value) = get_pre_value(key.as_str()) {
            return Some(value);
        }
        if let Some(base_name) = dae::component_base_name(key.as_str())
            && let Some(value) = get_pre_value(&base_name)
        {
            return Some(value);
        }
        if let Some(start) = env.start_exprs.get(key.as_str()) {
            return eval_scalar_expr_fast(start, env)
                .or_else(|| Some(rumoca_eval_dae::runtime::eval_expr::<f64>(start, env)));
        }
        if let Some(base_name) = dae::component_base_name(key.as_str())
            && let Some(start) = env.start_exprs.get(base_name.as_str())
        {
            return eval_scalar_expr_fast(start, env)
                .or_else(|| Some(rumoca_eval_dae::runtime::eval_expr::<f64>(start, env)));
        }
        // MLS §16.5.1 / §16.4: previous(v) at the first tick uses the
        // declared start value of v, or the type default when no explicit
        // start exists. It must not read the current env value.
        return Some(0.0);
    }
    eval_builtin_pre_fast(arg, env)
}

fn eval_binary_fast(
    op: &rumoca_ir_core::OpBinary,
    lhs: &dae::Expression,
    rhs: &dae::Expression,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let l = eval_scalar_expr_fast_mode(lhs, env, mode)?;
    let r = eval_scalar_expr_fast_mode(rhs, env, mode)?;
    Some(match op {
        rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => l + r,
        rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => l - r,
        rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => l * r,
        rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => l / r,
        rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => l.powf(r),
        rumoca_ir_core::OpBinary::And(_) => {
            <f64 as SimFloat>::from_bool(l.to_bool() && r.to_bool())
        }
        rumoca_ir_core::OpBinary::Or(_) => <f64 as SimFloat>::from_bool(l.to_bool() || r.to_bool()),
        rumoca_ir_core::OpBinary::Lt(_) => <f64 as SimFloat>::from_bool(l < r),
        rumoca_ir_core::OpBinary::Le(_) => <f64 as SimFloat>::from_bool(l <= r),
        rumoca_ir_core::OpBinary::Gt(_) => <f64 as SimFloat>::from_bool(l > r),
        rumoca_ir_core::OpBinary::Ge(_) => <f64 as SimFloat>::from_bool(l >= r),
        rumoca_ir_core::OpBinary::Eq(_) => <f64 as SimFloat>::from_bool(l.eq_approx(r)),
        rumoca_ir_core::OpBinary::Neq(_) => <f64 as SimFloat>::from_bool(!l.eq_approx(r)),
        rumoca_ir_core::OpBinary::Empty | rumoca_ir_core::OpBinary::Assign(_) => 0.0,
    })
}

fn eval_unary_fast(
    op: &rumoca_ir_core::OpUnary,
    rhs: &dae::Expression,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let value = eval_scalar_expr_fast_mode(rhs, env, mode)?;
    Some(match op {
        rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => -value,
        rumoca_ir_core::OpUnary::Plus(_)
        | rumoca_ir_core::OpUnary::DotPlus(_)
        | rumoca_ir_core::OpUnary::Empty => value,
        rumoca_ir_core::OpUnary::Not(_) => <f64 as SimFloat>::from_bool(!value.to_bool()),
    })
}

fn eval_if_fast(
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    for (condition, value) in branches {
        if eval_scalar_expr_fast_mode(condition, env, mode)?.to_bool() {
            return eval_scalar_expr_fast_mode(value, env, mode);
        }
    }
    eval_scalar_expr_fast_mode(else_branch, env, mode)
}

fn eval_positive_factor_fast(
    arg: Option<&dae::Expression>,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let raw = eval_scalar_expr_fast_mode(arg?, env, mode)?;
    let rounded = raw.round();
    (rounded.is_finite() && rounded > 0.0).then_some(rounded)
}

fn infer_clock_counter_form_fast(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let dae::Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short != "Clock" || args.len() != 1 {
        return None;
    }

    let raw = eval_scalar_expr_fast_mode(&args[0], env, mode)?;
    let rounded = raw.round();
    let tol = 1.0e-9 * rounded.abs().max(1.0);
    (rounded.is_finite() && rounded > 0.0 && (raw - rounded).abs() <= tol).then_some(rounded)
}

fn infer_clock_timing_fast(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<FastClockTiming> {
    let dae::Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    match short {
        // MLS §16.3 / §16.5.2: exact-clock constructors and derivations map to
        // periodic tick streams defined by period/phase pairs.
        "Clock" => {
            if args.is_empty() {
                return None;
            }
            if let Some(base) = infer_clock_timing_fast(&args[0], env, mode) {
                return Some(base);
            }
            if args.len() >= 2 {
                let count = eval_scalar_expr_fast_mode(&args[0], env, mode)?;
                let resolution = eval_scalar_expr_fast_mode(&args[1], env, mode)?;
                return valid_positive_period(count / resolution)
                    .map(|period| FastClockTiming { period, phase: 0.0 });
            }
            let period = eval_scalar_expr_fast_mode(&args[0], env, mode)?;
            valid_positive_period(period).map(|period| FastClockTiming { period, phase: 0.0 })
        }
        "subSample" => {
            if let Some(counter) = infer_clock_counter_form_fast(args.first()?, env, mode) {
                let resolution = eval_positive_factor_fast(args.get(1), env, mode).unwrap_or(1.0);
                return valid_positive_period(counter / resolution)
                    .map(|period| FastClockTiming { period, phase: 0.0 });
            }
            let base = infer_clock_timing_fast(args.first()?, env, mode)?;
            let factor = eval_positive_factor_fast(args.get(1), env, mode).unwrap_or(1.0);
            valid_positive_period(base.period * factor).map(|period| FastClockTiming {
                period,
                phase: base.phase,
            })
        }
        "superSample" => {
            let base = infer_clock_timing_fast(args.first()?, env, mode)?;
            let factor = eval_positive_factor_fast(args.get(1), env, mode).unwrap_or(1.0);
            valid_positive_period(base.period / factor).map(|period| FastClockTiming {
                period,
                phase: base.phase,
            })
        }
        "shiftSample" | "backSample" => {
            let base = infer_clock_timing_fast(args.first()?, env, mode)?;
            let shift =
                eval_scalar_expr_fast_mode(args.get(1).unwrap_or(args.first()?), env, mode)?;
            let offset = if args.len() >= 3 {
                let resolution = eval_scalar_expr_fast_mode(&args[2], env, mode)?;
                if resolution.is_finite() && resolution != 0.0 {
                    // MLS §16.5.2: shiftSample/backSample use a fraction of
                    // interval(u), not an absolute time in seconds.
                    (shift / resolution) * base.period
                } else {
                    shift * base.period
                }
            } else {
                shift * base.period
            };
            let phase = if short == "shiftSample" {
                base.phase + offset
            } else {
                base.phase - offset
            };
            valid_positive_period(base.period).map(|period| FastClockTiming { period, phase })
        }
        _ => None,
    }
}

fn clock_like_function_name(expr: &dae::Expression, env: &VarEnv<f64>) -> bool {
    if let dae::Expression::FunctionCall { name, .. } = expr {
        let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
        return matches!(
            short,
            "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
        );
    }
    if let Some(key) = expr_env_key(expr) {
        return env.clock_intervals.contains_key(key.as_str());
    }
    false
}

fn eval_distribution_clock_fast(
    short: &str,
    args: &[dae::Expression],
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    match short {
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" => {
            if short == "Clock" && args.is_empty() {
                return Some(env.get(IMPLICIT_CLOCK_ACTIVE_ENV_KEY));
            }
            let timing = infer_clock_timing_fast(
                &dae::Expression::FunctionCall {
                    name: dae::VarName::new(short),
                    args: args.to_vec(),
                    is_constructor: false,
                },
                env,
                mode,
            )?;
            // MLS §16.5.1 / Appendix B: exact-clock indicators are false on
            // the event left-limit, even though `time` is unchanged there.
            Some(if mode == ScalarEvalMode::LeftLimit {
                0.0
            } else {
                clock_tick_scalar(env, timing)
            })
        }
        // MLS §16.5.1 / §16.4: hold(), noClock(), and previous() are reads of
        // the current or stored component value, not arbitrary solver work.
        "hold" | "noClock" => args
            .first()
            .and_then(|arg| eval_scalar_expr_fast_mode(arg, env, mode)),
        "previous" => args.first().and_then(|arg| eval_previous_fast(arg, env)),
        "firstTick" => Some(if mode == ScalarEvalMode::LeftLimit {
            0.0
        } else {
            <f64 as SimFloat>::from_bool(eval_time_seconds(env).abs() <= 1.0e-12)
        }),
        _ => None,
    }
}

fn eval_builtin_sample_fast(
    args: &[dae::Expression],
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let (start_expr, interval_expr) = match args {
        [_internal_id, start, interval, ..] => (start, interval),
        [start, interval] if !clock_like_function_name(interval, env) => (start, interval),
        _ => return None,
    };
    let start_t = eval_scalar_expr_fast_mode(start_expr, env, mode)?;
    let period = valid_positive_period(eval_scalar_expr_fast_mode(interval_expr, env, mode)?)?;
    // MLS §16.5.1: sample(start, interval) is a periodic event indicator.
    Some(if mode == ScalarEvalMode::LeftLimit {
        0.0
    } else {
        clock_tick_scalar(
            env,
            FastClockTiming {
                period,
                phase: start_t,
            },
        )
    })
}

fn passthrough_builtin_arg<'a>(
    function: &dae::BuiltinFunction,
    args: &'a [dae::Expression],
) -> Option<&'a dae::Expression> {
    match function {
        // MLS §3.3 / §3.7.4.3 / §3.7.5: noEvent, homotopy(actual, ...),
        // and smooth(p, expr) preserve the selected value expression for
        // ordinary runtime evaluation; they only affect events/analysis.
        dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Homotopy => args.first(),
        dae::BuiltinFunction::Smooth if args.len() >= 2 => args.get(1),
        _ => None,
    }
}

fn scalar_size_from_env_entries(key: &str, env: &VarEnv<f64>) -> Option<f64> {
    if let Some((base, field)) = key.rsplit_once('.') {
        let prefix = format!("{base}[");
        let suffix = format!("].{field}");
        return env
            .vars
            .keys()
            .filter_map(|env_key| {
                let rest = env_key.strip_prefix(prefix.as_str())?;
                let index = rest.strip_suffix(suffix.as_str())?;
                (!index.contains(','))
                    .then(|| index.parse::<usize>().ok())
                    .flatten()
            })
            .max()
            .map(|len| len as f64);
    }

    let prefix = format!("{key}[");
    env.vars
        .keys()
        .filter_map(|env_key| {
            let rest = env_key.strip_prefix(prefix.as_str())?;
            let index = rest.strip_suffix(']')?;
            (!index.contains(','))
                .then(|| index.parse::<usize>().ok())
                .flatten()
        })
        .max()
        .map(|len| len as f64)
}

fn scalar_range_len_fast(
    start: &dae::Expression,
    step: Option<&dae::Expression>,
    end: &dae::Expression,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let start_v = eval_scalar_expr_fast_mode(start, env, mode)?;
    let end_v = eval_scalar_expr_fast_mode(end, env, mode)?;
    let step_v = if let Some(step_expr) = step {
        eval_scalar_expr_fast_mode(step_expr, env, mode)?
    } else if end_v >= start_v {
        1.0
    } else {
        -1.0
    };
    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return None;
    }
    let tol = step_v.abs() * 1e-9 + 1e-12;
    let mut count = 0usize;
    let mut value = start_v;
    for _ in 0..100_000 {
        let past_end =
            (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
        if past_end {
            break;
        }
        count += 1;
        value += step_v;
    }
    Some(count as f64)
}

fn eval_size_builtin_fast(
    args: &[dae::Expression],
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let target = args.first()?;
    let dim = if args.len() >= 2 {
        eval_scalar_expr_fast_mode(&args[1], env, mode)?.round() as usize
    } else {
        1
    };
    if dim == 0 {
        return None;
    }

    match target {
        // MLS Chapter 10 size(A, dim): use preserved array dimension metadata when present,
        // and only fall back to indexed runtime entries for 1-D runtime arrays.
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            (dim == 1).then_some(elements.len() as f64)
        }
        dae::Expression::Range { start, step, end } => {
            (dim == 1).then(|| scalar_range_len_fast(start, step.as_deref(), end, env, mode))?
        }
        _ => {
            let key = expr_env_key(target)?;
            if let Some(dims) = env.dims.get(key.as_str()) {
                return dims
                    .get(dim.saturating_sub(1))
                    .copied()
                    .map(|value| value as f64);
            }
            (dim == 1)
                .then(|| scalar_size_from_env_entries(key.as_str(), env))
                .flatten()
        }
    }
}

fn eval_function_math_fast(
    short: &str,
    args: &[dae::Expression],
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let arg = |index: usize| eval_scalar_expr_fast_mode(args.get(index)?, env, mode);
    match short {
        // MLS Chapter 3 standard operators/functions: common scalar math calls
        // lowered as FunctionCall still have scalar semantics on the runtime fast path.
        "abs" => Some(arg(0)?.abs()),
        "sign" => Some(arg(0)?.signum()),
        "sqrt" => Some(arg(0)?.sqrt()),
        "floor" | "integer" | "Integer" => Some(arg(0)?.floor()),
        "ceil" => Some(arg(0)?.ceil()),
        "min" => Some(arg(0)?.min(arg(1)?)),
        "max" => Some(arg(0)?.max(arg(1)?)),
        "sin" => Some(arg(0)?.sin()),
        "cos" => Some(arg(0)?.cos()),
        "tan" => Some(arg(0)?.tan()),
        "asin" => Some(arg(0)?.asin()),
        "acos" => Some(arg(0)?.acos()),
        "atan" => Some(arg(0)?.atan()),
        "atan2" => Some(arg(0)?.atan2(arg(1)?)),
        "sinh" => Some(arg(0)?.sinh()),
        "cosh" => Some(arg(0)?.cosh()),
        "tanh" => Some(arg(0)?.tanh()),
        "exp" => Some(arg(0)?.exp()),
        "log" => Some(arg(0)?.ln()),
        "log10" => Some(arg(0)?.log10()),
        "semiLinear" => {
            let x = arg(0)?;
            let k1 = arg(1)?;
            let k2 = arg(2)?;
            Some(if x >= 0.0 { k1 * x } else { k2 * x })
        }
        _ => None,
    }
}

fn eval_scalar_math_builtin_fast(
    function: &dae::BuiltinFunction,
    args: &[dae::Expression],
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    let arg = |index: usize| eval_scalar_expr_fast_mode(args.get(index)?, env, mode);
    match function {
        dae::BuiltinFunction::Abs => Some(arg(0)?.abs()),
        dae::BuiltinFunction::Sign => Some(arg(0)?.signum()),
        dae::BuiltinFunction::Sqrt => Some(arg(0)?.sqrt()),
        dae::BuiltinFunction::Floor | dae::BuiltinFunction::Integer => Some(arg(0)?.floor()),
        dae::BuiltinFunction::Ceil => Some(arg(0)?.ceil()),
        dae::BuiltinFunction::Min => Some(arg(0)?.min(arg(1)?)),
        dae::BuiltinFunction::Max => Some(arg(0)?.max(arg(1)?)),
        dae::BuiltinFunction::Sin => Some(arg(0)?.sin()),
        dae::BuiltinFunction::Cos => Some(arg(0)?.cos()),
        dae::BuiltinFunction::Tan => Some(arg(0)?.tan()),
        dae::BuiltinFunction::Asin => Some(arg(0)?.asin()),
        dae::BuiltinFunction::Acos => Some(arg(0)?.acos()),
        dae::BuiltinFunction::Atan => Some(arg(0)?.atan()),
        dae::BuiltinFunction::Atan2 => Some(arg(0)?.atan2(arg(1)?)),
        dae::BuiltinFunction::Sinh => Some(arg(0)?.sinh()),
        dae::BuiltinFunction::Cosh => Some(arg(0)?.cosh()),
        dae::BuiltinFunction::Tanh => Some(arg(0)?.tanh()),
        dae::BuiltinFunction::Exp => Some(arg(0)?.exp()),
        dae::BuiltinFunction::Log => Some(arg(0)?.ln()),
        dae::BuiltinFunction::Log10 => Some(arg(0)?.log10()),
        // MLS §8.6: `initial()` is true only during the initial event.
        dae::BuiltinFunction::Initial => Some(<f64 as SimFloat>::from_bool(env.is_initial)),
        dae::BuiltinFunction::Delay => Some(arg(0)?),
        dae::BuiltinFunction::Size => eval_size_builtin_fast(args, env, mode),
        dae::BuiltinFunction::SemiLinear => {
            let x = arg(0)?;
            let k1 = arg(1)?;
            let k2 = arg(2)?;
            Some(if x >= 0.0 { k1 * x } else { k2 * x })
        }
        _ => None,
    }
}

fn eval_scalar_expr_fast_mode(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    mode: ScalarEvalMode,
) -> Option<f64> {
    match expr {
        dae::Expression::Literal(dae::Literal::Real(value)) => Some(*value),
        dae::Expression::Literal(dae::Literal::Integer(value)) => Some(*value as f64),
        dae::Expression::Literal(dae::Literal::Boolean(value)) => {
            Some(<f64 as SimFloat>::from_bool(*value))
        }
        dae::Expression::VarRef { .. }
        | dae::Expression::FieldAccess { .. }
        | dae::Expression::Index { .. } => {
            Some(resolve_key_value(expr_env_key(expr)?.as_str(), env, mode))
        }
        dae::Expression::Unary { op, rhs } => eval_unary_fast(op, rhs, env, mode),
        dae::Expression::Binary { op, lhs, rhs } => eval_binary_fast(op, lhs, rhs, env, mode),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args,
        } => args.first().and_then(|arg| eval_builtin_der_fast(arg, env)),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args,
        } => eval_builtin_sample_fast(args, env, mode),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args,
        } => args.first().and_then(|arg| eval_builtin_pre_fast(arg, env)),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge,
            args,
        } => args.first().and_then(|arg| {
            let current = eval_scalar_expr_fast_mode(arg, env, mode)?.to_bool();
            let previous = eval_builtin_pre_fast(arg, env)?.to_bool();
            Some(<f64 as SimFloat>::from_bool(current && !previous))
        }),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Change,
            args,
        } => args.first().and_then(|arg| {
            let current = eval_scalar_expr_fast_mode(arg, env, mode)?;
            let previous = eval_builtin_pre_fast(arg, env)?;
            Some(<f64 as SimFloat>::from_bool(!current.eq_approx(previous)))
        }),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            ..
        } => Some(<f64 as SimFloat>::from_bool(env.is_initial)),
        dae::Expression::BuiltinCall { function, args } => {
            eval_scalar_math_builtin_fast(function, args, env, mode).or_else(|| {
                passthrough_builtin_arg(function, args)
                    .and_then(|inner| eval_scalar_expr_fast_mode(inner, env, mode))
            })
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            eval_distribution_clock_fast(short, args, env, mode)
                .or_else(|| eval_function_math_fast(short, args, env, mode))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => eval_if_fast(branches, else_branch, env, mode),
        dae::Expression::Array { elements, .. } if elements.len() == 1 => {
            eval_scalar_expr_fast_mode(&elements[0], env, mode)
        }
        dae::Expression::Tuple { elements } if elements.len() == 1 => {
            eval_scalar_expr_fast_mode(&elements[0], env, mode)
        }
        dae::Expression::Literal(dae::Literal::String(_))
        | dae::Expression::Array { .. }
        | dae::Expression::Tuple { .. }
        | dae::Expression::Range { .. }
        | dae::Expression::ArrayComprehension { .. }
        | dae::Expression::Empty => None,
    }
}

pub fn eval_scalar_expr_fast(expr: &dae::Expression, env: &VarEnv<f64>) -> Option<f64> {
    eval_scalar_expr_fast_mode(expr, env, ScalarEvalMode::Current)
}

pub fn eval_scalar_bool_expr_fast(expr: &dae::Expression, env: &VarEnv<f64>) -> Option<bool> {
    match expr {
        // MLS §8.3.5 / SPEC_0022 EQN-029: when-equation conditions may be
        // represented as Boolean vectors. The lowered runtime uses Array/Tuple
        // guards for `when {c1, c2, ...} then`, which are active when any
        // listed condition is true.
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            let mut any_true = false;
            for element in elements {
                any_true |= eval_scalar_bool_expr_fast(element, env)?;
            }
            Some(any_true)
        }
        _ => eval_scalar_expr_fast(expr, env).map(|value| value.to_bool()),
    }
}

pub(crate) fn eval_left_limit_scalar_expr_fast(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<f64> {
    eval_scalar_expr_fast_mode(expr, env, ScalarEvalMode::LeftLimit)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    #[test]
    fn eval_scalar_expr_fast_handles_simple_if_with_comparison() {
        let mut env = VarEnv::<f64>::new();
        env.set("x", 3.0);
        let expr = dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                    lhs: Box::new(var("x")),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                },
                dae::Expression::Literal(dae::Literal::Real(5.0)),
            )],
            else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(7.0))),
        };
        assert_eq!(eval_scalar_expr_fast(&expr, &env), Some(5.0));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_math_builtins() {
        let env = VarEnv::<f64>::new();
        let sin_expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sin,
            args: vec![dae::Expression::Literal(dae::Literal::Real(1.0))],
        };
        let min_expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Min,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(4.0)),
                dae::Expression::Literal(dae::Literal::Real(2.0)),
            ],
        };
        let semilinear_expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::SemiLinear,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(-2.0)),
                dae::Expression::Literal(dae::Literal::Real(5.0)),
                dae::Expression::Literal(dae::Literal::Real(3.0)),
            ],
        };

        assert!(
            (eval_scalar_expr_fast(&sin_expr, &env).unwrap_or(f64::NAN) - 1.0f64.sin()).abs()
                <= 1.0e-12
        );
        assert_eq!(eval_scalar_expr_fast(&min_expr, &env), Some(2.0));
        assert_eq!(eval_scalar_expr_fast(&semilinear_expr, &env), Some(-6.0));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_size_builtin_from_dims_and_entries() {
        let mut env = VarEnv::<f64>::new();
        env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("a".to_string(), vec![3])]));
        env.set("rec[1].im", 1.0);
        env.set("rec[2].im", 2.0);

        let size_dim = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Size,
            args: vec![var("a"), dae::Expression::Literal(dae::Literal::Integer(1))],
        };
        let size_field = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Size,
            args: vec![
                dae::Expression::FieldAccess {
                    base: Box::new(var("rec")),
                    field: "im".to_string(),
                },
                dae::Expression::Literal(dae::Literal::Integer(1)),
            ],
        };

        assert_eq!(eval_scalar_expr_fast(&size_dim, &env), Some(3.0));
        assert_eq!(eval_scalar_expr_fast(&size_field, &env), Some(2.0));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_indexed_varrefs_and_field_access() {
        let mut env = VarEnv::<f64>::new();
        env.set("a[2]", 4.0);
        env.set("rec.im", -2.0);
        let indexed = dae::Expression::VarRef {
            name: dae::VarName::new("a"),
            subscripts: vec![dae::Subscript::Index(2)],
        };
        let field = dae::Expression::FieldAccess {
            base: Box::new(var("rec")),
            field: "im".to_string(),
        };
        assert_eq!(eval_scalar_expr_fast(&indexed, &env), Some(4.0));
        assert_eq!(eval_scalar_expr_fast(&field, &env), Some(-2.0));
    }

    #[test]
    fn eval_left_limit_scalar_expr_fast_prefers_pre_history() {
        let mut env = VarEnv::<f64>::new();
        env.set("x", 5.0);
        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::set_pre_value("x", 2.0);
        let expr = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        };
        assert_eq!(eval_left_limit_scalar_expr_fast(&expr, &env), Some(3.0));
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_left_limit_scalar_expr_fast_handles_singleton_array_and_tuple() {
        let mut env = VarEnv::<f64>::new();
        env.set("x", 5.0);
        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::set_pre_value("x", 2.0);

        let array_expr = dae::Expression::Array {
            elements: vec![var("x")],
            is_matrix: false,
        };
        let tuple_expr = dae::Expression::Tuple {
            elements: vec![var("x")],
        };

        assert_eq!(
            eval_left_limit_scalar_expr_fast(&array_expr, &env),
            Some(2.0)
        );
        assert_eq!(
            eval_left_limit_scalar_expr_fast(&tuple_expr, &env),
            Some(2.0)
        );

        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_scalar_expr_fast_handles_math_function_calls() {
        let env = VarEnv::<f64>::new();
        let sin_expr = dae::Expression::FunctionCall {
            name: dae::VarName::new("sin"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(1.0))],
            is_constructor: false,
        };
        let max_expr = dae::Expression::FunctionCall {
            name: dae::VarName::new("max"),
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(2.0)),
                dae::Expression::Literal(dae::Literal::Real(5.0)),
            ],
            is_constructor: false,
        };
        assert!(
            (eval_scalar_expr_fast(&sin_expr, &env).unwrap_or(f64::NAN) - 1.0f64.sin()).abs()
                <= 1.0e-12
        );
        assert_eq!(eval_scalar_expr_fast(&max_expr, &env), Some(5.0));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_periodic_clock_constructors() {
        let mut env = VarEnv::<f64>::new();
        env.set("time", 1.0);
        env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);

        let clock = dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(0.5))],
            is_constructor: false,
        };
        let shifted = dae::Expression::FunctionCall {
            name: dae::VarName::new("shiftSample"),
            args: vec![
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Clock"),
                    args: vec![dae::Expression::Literal(dae::Literal::Real(1.0))],
                    is_constructor: false,
                },
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            ],
            is_constructor: false,
        };
        let implicit_clock = dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![],
            is_constructor: false,
        };

        assert_eq!(eval_scalar_expr_fast(&clock, &env), Some(1.0));
        assert_eq!(eval_scalar_expr_fast(&shifted, &env), Some(1.0));
        assert_eq!(eval_scalar_expr_fast(&implicit_clock, &env), Some(1.0));
    }

    #[test]
    fn eval_scalar_expr_fast_shift_sample_resolution_scales_base_interval() {
        let mut env = VarEnv::<f64>::new();
        env.set("time", 0.06);

        let shifted = dae::Expression::FunctionCall {
            name: dae::VarName::new("shiftSample"),
            args: vec![
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Clock"),
                    args: vec![dae::Expression::Literal(dae::Literal::Real(0.02))],
                    is_constructor: false,
                },
                dae::Expression::Literal(dae::Literal::Real(2.0)),
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            ],
            is_constructor: false,
        };

        assert_eq!(eval_scalar_expr_fast(&shifted, &env), Some(1.0));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_periodic_sample_forms() {
        let mut env = VarEnv::<f64>::new();
        env.set("time", 1.0);

        let periodic = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(0.0)),
                dae::Expression::Literal(dae::Literal::Real(0.5)),
            ],
        };
        let lowered = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Literal(dae::Literal::Integer(7)),
                dae::Expression::Literal(dae::Literal::Real(0.0)),
                dae::Expression::Literal(dae::Literal::Real(0.5)),
            ],
        };

        assert_eq!(eval_scalar_expr_fast(&periodic, &env), Some(1.0));
        assert_eq!(eval_scalar_expr_fast(&lowered, &env), Some(1.0));
    }

    #[test]
    fn eval_scalar_expr_fast_edge_of_implicit_clock_fires_on_active_tick() {
        let mut env = VarEnv::<f64>::new();
        env.set(IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
        let expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge,
            args: vec![dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![],
                is_constructor: false,
            }],
        };

        assert_eq!(eval_scalar_bool_expr_fast(&expr, &env), Some(true));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_previous_hold_and_first_tick() {
        let mut env = VarEnv::<f64>::new();
        env.set("time", 0.0);
        env.set("x", 5.0);
        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::set_pre_value("x", 2.0);

        let previous = dae::Expression::FunctionCall {
            name: dae::VarName::new("previous"),
            args: vec![var("x")],
            is_constructor: false,
        };
        let hold = dae::Expression::FunctionCall {
            name: dae::VarName::new("hold"),
            args: vec![var("x")],
            is_constructor: false,
        };
        let no_clock = dae::Expression::FunctionCall {
            name: dae::VarName::new("noClock"),
            args: vec![var("x")],
            is_constructor: false,
        };
        let first_tick = dae::Expression::FunctionCall {
            name: dae::VarName::new("firstTick"),
            args: vec![],
            is_constructor: false,
        };

        assert_eq!(eval_scalar_expr_fast(&previous, &env), Some(2.0));
        assert_eq!(eval_scalar_expr_fast(&hold, &env), Some(5.0));
        assert_eq!(eval_scalar_expr_fast(&no_clock, &env), Some(5.0));
        assert_eq!(eval_scalar_expr_fast(&first_tick, &env), Some(1.0));

        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_scalar_expr_fast_previous_uses_start_or_default_without_pre_store() {
        let mut env = VarEnv::<f64>::new();
        env.set("time", 0.0);
        env.set("x", 5.0);
        env.start_exprs = std::sync::Arc::new(indexmap::IndexMap::from([(
            "x".to_string(),
            dae::Expression::Literal(dae::Literal::Real(3.0)),
        )]));
        rumoca_eval_dae::runtime::clear_pre_values();

        let previous = dae::Expression::FunctionCall {
            name: dae::VarName::new("previous"),
            args: vec![var("x")],
            is_constructor: false,
        };

        assert_eq!(eval_scalar_expr_fast(&previous, &env), Some(3.0));

        env.start_exprs = std::sync::Arc::new(indexmap::IndexMap::new());
        assert_eq!(eval_scalar_expr_fast(&previous, &env), Some(0.0));

        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_scalar_expr_fast_prefers_pre_store_for_lowered_pre_parameters() {
        let mut env = VarEnv::<f64>::new();
        env.set("__pre__.reset", 0.0);
        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::set_pre_value("reset", 1.0);

        assert_eq!(
            eval_scalar_expr_fast(&var("__pre__.reset"), &env),
            Some(1.0)
        );

        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_scalar_expr_fast_edge_on_relational_expr_uses_pre_store() {
        let mut env = VarEnv::<f64>::new();
        env.set("trig", 4.0);
        env.enum_literal_ordinals = std::sync::Arc::new(indexmap::IndexMap::from([(
            "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
            4,
        )]));
        rumoca_eval_dae::runtime::clear_pre_values();
        rumoca_eval_dae::runtime::set_pre_value("trig", 2.0);

        let relation = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Eq(Default::default()),
            lhs: Box::new(var("trig")),
            rhs: Box::new(var("Modelica.Electrical.Digital.Interfaces.Logic.'1'")),
        };
        let edge_expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge,
            args: vec![relation],
        };

        assert_eq!(eval_scalar_expr_fast(&edge_expr, &env), Some(1.0));

        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_scalar_expr_fast_reads_derivative_vars_from_env() {
        let mut env = VarEnv::<f64>::new();
        env.set("der(load.phi)", 0.25);

        let expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("load.phi")],
        };

        assert_eq!(eval_scalar_expr_fast(&expr, &env), Some(0.25));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_wrapper_builtins() {
        let mut env = VarEnv::<f64>::new();
        env.set("x", 3.0);

        let no_event = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::NoEvent,
            args: vec![dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                lhs: Box::new(var("x")),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            }],
        };
        let smooth = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Smooth,
            args: vec![
                dae::Expression::Literal(dae::Literal::Integer(1)),
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(Default::default()),
                    lhs: Box::new(var("x")),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
                },
            ],
        };
        let homotopy = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Homotopy,
            args: vec![
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(var("x")),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                },
                dae::Expression::Literal(dae::Literal::Real(0.0)),
            ],
        };

        assert_eq!(eval_scalar_bool_expr_fast(&no_event, &env), Some(true));
        assert_eq!(eval_scalar_expr_fast(&smooth, &env), Some(5.0));
        assert_eq!(eval_scalar_expr_fast(&homotopy, &env), Some(2.0));
    }

    #[test]
    fn eval_scalar_expr_fast_handles_initial_builtin() {
        let mut env = VarEnv::default();
        let expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            args: vec![],
        };

        env.is_initial = true;
        assert_eq!(eval_scalar_bool_expr_fast(&expr, &env), Some(true));

        env.is_initial = false;
        assert_eq!(eval_scalar_bool_expr_fast(&expr, &env), Some(false));
    }

    #[test]
    fn eval_scalar_expr_fast_edge_of_initial_fires_at_startup() {
        let env = VarEnv {
            is_initial: true,
            ..VarEnv::default()
        };
        let expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge,
            args: vec![dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Initial,
                args: vec![],
            }],
        };

        assert_eq!(eval_scalar_bool_expr_fast(&expr, &env), Some(true));
    }

    #[test]
    fn eval_scalar_expr_fast_edge_of_time_ge_next_event_fires_at_event() {
        rumoca_eval_dae::runtime::clear_pre_values();

        let mut seed_env = VarEnv::default();
        seed_env.set("nextEvent", 1.0);
        rumoca_eval_dae::runtime::seed_pre_values_from_env(&seed_env);

        let mut env = VarEnv::default();
        env.set("time", 1.0);
        env.set("nextEvent", 1.0);
        let expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge,
            args: vec![dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Ge(Default::default()),
                lhs: Box::new(var("time")),
                rhs: Box::new(var("nextEvent")),
            }],
        };

        assert_eq!(eval_scalar_bool_expr_fast(&expr, &env), Some(true));
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_scalar_expr_fast_change_detects_logic_ordinal_transition() {
        rumoca_eval_dae::runtime::clear_pre_values();

        let mut seed_env = VarEnv::default();
        seed_env.set("logic", 1.0);
        rumoca_eval_dae::runtime::seed_pre_values_from_env(&seed_env);

        let mut env = VarEnv::default();
        env.set("logic", 3.0);
        let expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Change,
            args: vec![var("logic")],
        };

        assert_eq!(eval_scalar_bool_expr_fast(&expr, &env), Some(true));
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn eval_scalar_expr_fast_resolves_fully_qualified_enum_literals() {
        let mut env = VarEnv::default();
        env.set("trig", 4.0);
        env.enum_literal_ordinals = std::sync::Arc::new(indexmap::IndexMap::from([(
            "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
            4_i64,
        )]));
        let expr = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Eq(Default::default()),
            lhs: Box::new(var("trig")),
            rhs: Box::new(var("Modelica.Electrical.Digital.Interfaces.Logic.'1'")),
        };

        assert_eq!(eval_scalar_expr_fast(&expr, &env), Some(1.0));
        assert_eq!(eval_scalar_bool_expr_fast(&expr, &env), Some(true));
    }

    #[test]
    fn eval_scalar_bool_expr_fast_handles_when_condition_vectors() {
        let env = VarEnv::default();
        let mixed = dae::Expression::Array {
            elements: vec![
                dae::Expression::Literal(dae::Literal::Boolean(false)),
                dae::Expression::Literal(dae::Literal::Boolean(true)),
            ],
            is_matrix: false,
        };
        let none = dae::Expression::Tuple {
            elements: vec![
                dae::Expression::Literal(dae::Literal::Boolean(false)),
                dae::Expression::Literal(dae::Literal::Boolean(false)),
            ],
        };

        assert_eq!(eval_scalar_bool_expr_fast(&mixed, &env), Some(true));
        assert_eq!(eval_scalar_bool_expr_fast(&none, &env), Some(false));
    }
}
