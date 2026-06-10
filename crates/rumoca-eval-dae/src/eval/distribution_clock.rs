use super::*;

#[derive(Debug, Default, Clone)]
pub(super) struct ShiftSignalState {
    seen_tick: bool,
    tick_index: usize,
    last_tick_time: Option<f64>,
    last_output: f64,
}

fn shift_signal_state_key<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<String, EvalError> {
    let source_arg = required_shift_source_arg(args)?;
    let source =
        try_eval_field_access_path(source_arg, env)?.ok_or(EvalError::UnsupportedExpression {
            kind: "shift/back sample source path",
        })?;
    let shift = optional_real_arg(args, 1, env, 0.0)?.round() as i64;
    let resolution = (optional_real_arg(args, 2, env, 1.0)?.round() as i64).max(1);
    Ok(format!("{short_name}|{source}|{shift}|{resolution}"))
}

fn required_shift_source_arg(args: &[Expression]) -> Result<&Expression, EvalError> {
    args.first().ok_or(EvalError::UnsupportedExpression {
        kind: "shift/back sample source argument",
    })
}

fn shift_startup_ticks<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<usize, EvalError> {
    let shift = optional_real_arg(args, 1, env, 0.0)?.max(0.0);
    let resolution = optional_real_arg(args, 2, env, 1.0)?;
    if !resolution.is_finite() || resolution <= 0.0 {
        return Ok(0);
    }
    Ok((shift / resolution).ceil().max(0.0) as usize)
}

fn update_shift_signal_state(
    state: &mut ShiftSignalState,
    implicit_clock_active: bool,
    time: f64,
    input_value: f64,
    startup_ticks: usize,
    tick_tol: f64,
) {
    if !implicit_clock_active {
        return;
    }

    let is_new_tick = state
        .last_tick_time
        .is_none_or(|prev| (time - prev).abs() > tick_tol);
    if !is_new_tick {
        return;
    }

    if state.seen_tick {
        state.tick_index = state.tick_index.saturating_add(1);
    } else {
        state.seen_tick = true;
        state.tick_index = 0;
    }
    state.last_tick_time = Some(time);

    if state.tick_index < startup_ticks {
        return;
    }
    state.last_output = input_value;
}

fn eval_shift_sample_signal<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let key = shift_signal_state_key(short_name, args, env)?;
    let startup_ticks = shift_startup_ticks(args, env)?;
    let input_value = eval_expr::<T>(required_shift_source_arg(args)?, env)?.real();
    let time = eval_time_seconds(env)?;
    let implicit_clock_active = env.require(IMPLICIT_CLOCK_ACTIVE_ENV_KEY)?.to_bool();
    let tick_tol = 1.0e-12;

    let mut states = env
        .runtime
        .clock_special_states
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let state = states.entry(key).or_default();
    update_shift_signal_state(
        state,
        implicit_clock_active,
        time,
        input_value,
        startup_ticks,
        tick_tol,
    );
    Ok(T::from_f64(state.last_output))
}

fn optional_real_arg<T: SimFloat>(
    args: &[Expression],
    index: usize,
    env: &VarEnv<T>,
    default: f64,
) -> Result<f64, EvalError> {
    match args.get(index) {
        Some(arg) => Ok(eval_expr::<T>(arg, env)?.real()),
        None => Ok(default),
    }
}

fn eval_distribution_uniform<T: SimFloat>(u: T, u_min: T, u_max: T) -> T {
    let width = (u_max - u_min).real();
    if !width.is_finite() || width <= 0.0 {
        return T::zero();
    }
    let u_real = u.real();
    if u_real < u_min.real() || u_real > u_max.real() {
        return T::zero();
    }
    T::from_f64(1.0 / width)
}

fn eval_distribution_normal<T: SimFloat>(u: T, mu: T, sigma: T) -> T {
    let sigma_real = sigma.real();
    if !sigma_real.is_finite() || sigma_real <= 0.0 {
        return T::zero();
    }
    let z = (u - mu) / sigma;
    let coeff = 1.0 / (sigma_real * (2.0 * std::f64::consts::PI).sqrt());
    T::from_f64(coeff) * (T::from_f64(-0.5) * z * z).exp()
}

fn eval_distribution_weibull<T: SimFloat>(u: T, lambda: T, k: T) -> T {
    let lambda_real = lambda.real();
    let k_real = k.real();
    let u_real = u.real();
    if !lambda_real.is_finite()
        || !k_real.is_finite()
        || lambda_real <= 0.0
        || k_real <= 0.0
        || !u_real.is_finite()
        || u_real < 0.0
    {
        return T::zero();
    }
    let x = u / lambda;
    let density = (k / lambda) * x.powf(k - T::one()) * (-(x.powf(k))).exp();
    if density.real().is_finite() {
        density
    } else {
        T::zero()
    }
}

fn eval_distribution_uniform_cumulative<T: SimFloat>(u: T, u_min: T, u_max: T) -> T {
    let width = (u_max - u_min).real();
    if !width.is_finite() || width <= 0.0 {
        return T::zero();
    }
    if u.real() <= u_min.real() {
        return T::zero();
    }
    if u.real() >= u_max.real() {
        return T::one();
    }
    (u - u_min) / (u_max - u_min)
}

fn eval_distribution_weibull_cumulative<T: SimFloat>(u: T, lambda: T, k: T) -> T {
    let lambda_real = lambda.real();
    let k_real = k.real();
    if !lambda_real.is_finite() || !k_real.is_finite() || lambda_real <= 0.0 || k_real <= 0.0 {
        return T::zero();
    }
    if u.real() <= 0.0 {
        return T::zero();
    }
    T::one() - (-(u / lambda).powf(k)).exp()
}

fn normal_cdf_unit(z: f64) -> f64 {
    0.5 * (1.0 + erf_approx(z / std::f64::consts::SQRT_2))
}

fn erf_approx(x: f64) -> f64 {
    let sign = if x < 0.0 { -1.0 } else { 1.0 };
    let x_abs = x.abs();
    let t = 1.0 / (1.0 + 0.3275911 * x_abs);
    let poly = (((((1.061_405_429 * t - 1.453_152_027) * t) + 1.421_413_741) * t - 0.284_496_736)
        * t
        + 0.254_829_592)
        * t;
    sign * (1.0 - poly * (-x_abs * x_abs).exp())
}

fn normal_quantile_unit(p: f64) -> Result<f64, EvalError> {
    if p.is_nan() || !(0.0..=1.0).contains(&p) {
        return Err(EvalError::UnsupportedExpression {
            kind: "normal quantile domain",
        });
    }
    if p == 0.0 {
        return Ok(f64::NEG_INFINITY);
    }
    if p == 1.0 {
        return Ok(f64::INFINITY);
    }

    const P_LOW: f64 = 0.02425;
    const A: [f64; 6] = [
        -3.969_683_028_665_376e1,
        2.209_460_984_245_205e2,
        -2.759_285_104_469_687e2,
        1.383_577_518_672_69e2,
        -3.066_479_806_614_716e1,
        2.506_628_277_459_239,
    ];
    const B: [f64; 5] = [
        -5.447_609_879_822_406e1,
        1.615_858_368_580_409e2,
        -1.556_989_798_598_866e2,
        6.680_131_188_771_972e1,
        -1.328_068_155_288_572e1,
    ];
    const C: [f64; 6] = [
        -7.784_894_002_430_293e-3,
        -3.223_964_580_411_365e-1,
        -2.400_758_277_161_838,
        -2.549_732_539_343_734,
        4.374_664_141_464_968,
        2.938_163_982_698_783,
    ];
    const D: [f64; 4] = [
        7.784_695_709_041_462e-3,
        3.224_671_290_700_398e-1,
        2.445_134_137_142_996,
        3.754_408_661_907_416,
    ];

    if p < P_LOW {
        let q = (-2.0 * p.ln()).sqrt();
        return Ok(rational_tail(q, C, D));
    }
    if p > 1.0 - P_LOW {
        let q = (-2.0 * (1.0 - p).ln()).sqrt();
        return Ok(-rational_tail(q, C, D));
    }

    let q = p - 0.5;
    let r = q * q;
    let num = (((((A[0] * r + A[1]) * r + A[2]) * r + A[3]) * r + A[4]) * r + A[5]) * q;
    let den = ((((B[0] * r + B[1]) * r + B[2]) * r + B[3]) * r + B[4]) * r + 1.0;
    Ok(num / den)
}

fn rational_tail(q: f64, c: [f64; 6], d: [f64; 4]) -> f64 {
    let num = ((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5];
    let den = (((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1.0;
    num / den
}

fn eval_distribution_normal_cumulative<T: SimFloat>(u: T, mu: T, sigma: T) -> T {
    let sigma_real = sigma.real();
    if !sigma_real.is_finite() || sigma_real <= 0.0 {
        return T::zero();
    }
    T::from_f64(normal_cdf_unit(((u - mu) / sigma).real()))
}

fn eval_distribution_uniform_quantile<T: SimFloat>(u: T, y_min: T, y_max: T) -> T {
    y_min + u * (y_max - y_min)
}

fn eval_distribution_normal_quantile<T: SimFloat>(u: T, mu: T, sigma: T) -> Result<T, EvalError> {
    Ok(T::from_f64(
        mu.real() + sigma.real() * normal_quantile_unit(u.real())?,
    ))
}

fn eval_distribution_weibull_quantile<T: SimFloat>(u: T, lambda: T, k: T) -> Result<T, EvalError> {
    let u_real = u.real();
    if !(0.0..1.0).contains(&u_real) || lambda.real() <= 0.0 || k.real() <= 0.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "weibull quantile domain",
        });
    }
    Ok(lambda * (-(T::one() - u).ln()).powf(T::one() / k))
}

fn eval_distribution_truncated_normal_quantile<T: SimFloat>(
    u: T,
    y_min: T,
    y_max: T,
    mu: T,
    sigma: T,
) -> Result<T, EvalError> {
    let cdf_min = eval_distribution_normal_cumulative(y_min, mu, sigma);
    let cdf_max = eval_distribution_normal_cumulative(y_max, mu, sigma);
    eval_distribution_normal_quantile(cdf_min + u * (cdf_max - cdf_min), mu, sigma)
}

fn erf_inv_value(u: f64) -> Result<f64, EvalError> {
    if u.is_nan() || !(-1.0..=1.0).contains(&u) {
        return Err(EvalError::UnsupportedExpression {
            kind: "inverse error function domain",
        });
    }
    Ok(normal_quantile_unit(0.5 * (u + 1.0))? / std::f64::consts::SQRT_2)
}

fn eval_required_arg<T: SimFloat>(
    args: &[Expression],
    idx: usize,
    env: &VarEnv<T>,
    kind: &'static str,
) -> Result<T, EvalError> {
    eval_expr::<T>(
        args.get(idx)
            .ok_or(EvalError::UnsupportedExpression { kind })?,
        env,
    )
}

fn eval_optional_arg_or<T: SimFloat>(
    args: &[Expression],
    idx: usize,
    env: &VarEnv<T>,
    default: f64,
) -> Result<T, EvalError> {
    args.get(idx)
        .map(|arg| eval_expr::<T>(arg, env))
        .unwrap_or_else(|| Ok(T::from_f64(default)))
}

pub(super) fn is_qualified_distribution_function_name(name: &str) -> bool {
    matches!(
        name,
        "Modelica.Math.Distributions.Uniform.density"
            | "Modelica.Math.Distributions.Normal.density"
            | "Modelica.Math.Distributions.Weibull.density"
            | "Modelica.Math.Distributions.Uniform.cumulative"
            | "Modelica.Math.Distributions.Normal.cumulative"
            | "Modelica.Math.Distributions.Weibull.cumulative"
            | "Modelica.Math.Distributions.Uniform.quantile"
            | "Modelica.Math.Distributions.Normal.quantile"
            | "Modelica.Math.Distributions.Weibull.quantile"
            | "Modelica.Math.Distributions.TruncatedNormal.quantile"
    )
}

pub(super) fn eval_qualified_distribution_function<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match name {
        "Modelica.Math.Distributions.Uniform.density" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                Ok(eval_distribution_uniform(u, p2, p3))
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Normal.density" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                Ok(eval_distribution_normal(u, p2, p3))
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Weibull.density" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                Ok(eval_distribution_weibull(u, p2, p3))
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Uniform.cumulative" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                Ok(eval_distribution_uniform_cumulative(u, p2, p3))
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Normal.cumulative" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                Ok(eval_distribution_normal_cumulative(u, p2, p3))
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Weibull.cumulative" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                Ok(eval_distribution_weibull_cumulative(u, p2, p3))
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Uniform.quantile" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                Ok(eval_distribution_uniform_quantile(u, p2, p3))
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Normal.quantile" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                eval_distribution_normal_quantile(u, p2, p3)
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.Weibull.quantile" => {
            eval_qualified_distribution3(args, env, |u, p2, p3| {
                eval_distribution_weibull_quantile(u, p2, p3)
            })
            .map(Some)
        }
        "Modelica.Math.Distributions.TruncatedNormal.quantile" => {
            let u = eval_required_arg(args, 0, env, "distribution argument")?;
            let p2 = eval_required_arg(args, 1, env, "distribution argument")?;
            let p3 = eval_required_arg(args, 2, env, "distribution argument")?;
            let p4 = eval_optional_arg_or(args, 3, env, 0.0)?;
            let p5 = eval_optional_arg_or(args, 4, env, 1.0)?;
            Ok(Some(eval_distribution_truncated_normal_quantile(
                u, p2, p3, p4, p5,
            )?))
        }
        _ => Ok(None),
    }
}

fn eval_qualified_distribution3<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
    eval: impl FnOnce(T, T, T) -> Result<T, EvalError>,
) -> Result<T, EvalError> {
    let u = eval_required_arg(args, 0, env, "distribution argument")?;
    let p2 = eval_required_arg(args, 1, env, "distribution argument")?;
    let p3 = eval_required_arg(args, 2, env, "distribution argument")?;
    eval(u, p2, p3)
}

pub(super) fn is_qualified_math_special_function_name(name: &str) -> bool {
    matches!(
        name,
        "Modelica.Math.Special.erfInv" | "Modelica.Math.Special.erfcInv"
    )
}

pub(super) fn eval_qualified_math_special_function<T: SimFloat>(
    name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match name {
        "Modelica.Math.Special.erfInv" => {
            let u = eval_required_arg(args, 0, env, "math special argument")?.real();
            Ok(Some(T::from_f64(erf_inv_value(u)?)))
        }
        "Modelica.Math.Special.erfcInv" => {
            let u = eval_required_arg(args, 0, env, "math special argument")?.real();
            Ok(Some(T::from_f64(erf_inv_value(1.0 - u)?)))
        }
        _ => Ok(None),
    }
}

fn distribution_arg_hint(expr: Option<&Expression>) -> Option<String> {
    let Expression::VarRef { name, .. } = expr? else {
        return None;
    };
    let tail = rumoca_core::top_level_last_segment(name.as_str());
    let tail =
        rumoca_core::split_trailing_subscript_suffix(tail).map_or(tail, |(base, _subscript)| base);
    Some(tail.to_ascii_lowercase())
}

pub(super) fn eval_distribution_function<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match args.len() {
        // Overloaded MSL density helpers imported as `distribution(...)`.
        3 => {
            let u = eval_expr::<T>(&args[0], env)?;
            let p2 = eval_expr::<T>(&args[1], env)?;
            let p3 = eval_expr::<T>(&args[2], env)?;
            let h2 = distribution_arg_hint(args.get(1));
            let h3 = distribution_arg_hint(args.get(2));

            if matches!(h2.as_deref(), Some("u_min" | "y_min"))
                && matches!(h3.as_deref(), Some("u_max" | "y_max"))
            {
                return Ok(Some(eval_distribution_uniform(u, p2, p3)));
            }
            if matches!(h2.as_deref(), Some("mu")) && matches!(h3.as_deref(), Some("sigma")) {
                return Ok(Some(eval_distribution_normal(u, p2, p3)));
            }
            if matches!(h2.as_deref(), Some("lambda"))
                && matches!(h3.as_deref(), Some("k" | "shape"))
            {
                return Ok(Some(eval_distribution_weibull(u, p2, p3)));
            }

            Err(EvalError::UnsupportedExpression {
                kind: "ambiguous distribution helper",
            })
        }
        5 => {
            let r = eval_expr::<T>(&args[0], env)?;
            let y_min = eval_expr::<T>(&args[1], env)?;
            let y_max = eval_expr::<T>(&args[2], env)?;
            let span = y_max - y_min;
            let r_clamped = r.max(T::zero()).min(T::one());
            Ok(Some(y_min + span * r_clamped))
        }
        _ => Ok(None),
    }
}

pub(super) fn eval_clock_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match short_name {
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" => {
            if short_name == "Clock" && args.is_empty() {
                return Ok(Some(T::from_bool(
                    env.require(IMPLICIT_CLOCK_ACTIVE_ENV_KEY)?.to_bool(),
                )));
            }
            if let Some(timing) = infer_clock_timing_from_call(short_name, args, env)? {
                return Ok(Some(clock_tick_value(env, timing)?));
            }
            if matches!(short_name, "shiftSample" | "backSample") {
                return eval_shift_sample_signal(short_name, args, env).map(Some);
            }
            let Some(arg) = args.first() else {
                return Err(EvalError::UnsupportedExpression {
                    kind: "clock argument",
                });
            };
            eval_expr::<T>(arg, env).map(Some)
        }
        "hold" | "noClock" => {
            let Some(arg) = args.first() else {
                return Err(EvalError::UnsupportedExpression {
                    kind: "clocked value argument",
                });
            };
            eval_expr::<T>(arg, env).map(Some)
        }
        "previous" => eval_builtin_previous(args, env).map(Some),
        "interval" => {
            if let Some(arg) = args.first()
                && let Some(timing) = infer_clock_timing_from_expr(arg, env)?
            {
                return Ok(Some(T::from_f64(timing.period)));
            }
            // MLS §16 (synchronous language elements): interval(v) returns the
            // period of the clock associated with v. For implicit-clock lowering,
            // clock association is precomputed in DAE metadata.
            if let Some(Expression::VarRef {
                name, subscripts, ..
            }) = args.first()
                && subscripts.is_empty()
                && let Some(interval) = env.clock_intervals.get(name.as_str())
            {
                return Ok(Some(T::from_f64(*interval)));
            }
            Ok(Some(T::one()))
        }
        "firstTick" => {
            let tol = 1e-12;
            Ok(Some(T::from_bool(eval_time_seconds(env)?.abs() <= tol)))
        }
        _ => Ok(None),
    }
}
