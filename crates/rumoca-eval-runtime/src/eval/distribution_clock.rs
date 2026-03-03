use super::*;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
struct ShiftSignalState {
    seen_tick: bool,
    tick_index: usize,
    last_tick_time: Option<f64>,
    last_output: f64,
}

thread_local! {
    static SHIFT_SIGNAL_STATES: RefCell<HashMap<String, ShiftSignalState>> = RefCell::new(HashMap::new());
}

pub(super) fn clear_clock_special_states() {
    SHIFT_SIGNAL_STATES.with(|states| states.borrow_mut().clear());
}

fn shift_signal_state_key<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> String {
    let source = args
        .first()
        .and_then(|arg| eval_field_access_path(arg, env))
        .unwrap_or_else(|| format!("{:?}", args.first()));
    let shift = args
        .get(1)
        .map(|arg| eval_expr::<T>(arg, env).real().round() as i64)
        .unwrap_or(0);
    let resolution = args
        .get(2)
        .map(|arg| eval_expr::<T>(arg, env).real().round() as i64)
        .unwrap_or(1)
        .max(1);
    format!("{short_name}|{source}|{shift}|{resolution}")
}

fn shift_startup_ticks<T: SimFloat>(args: &[Expression], env: &VarEnv<T>) -> usize {
    let shift = args
        .get(1)
        .map(|arg| eval_expr::<T>(arg, env).real())
        .unwrap_or(0.0)
        .max(0.0);
    let resolution = args
        .get(2)
        .map(|arg| eval_expr::<T>(arg, env).real())
        .unwrap_or(1.0);
    if !resolution.is_finite() || resolution <= 0.0 {
        return 0;
    }
    (shift / resolution).ceil().max(0.0) as usize
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
) -> T {
    let key = shift_signal_state_key(short_name, args, env);
    let startup_ticks = shift_startup_ticks(args, env);
    let input_value = args
        .first()
        .map(|arg| eval_expr::<T>(arg, env).real())
        .unwrap_or(0.0);
    let time = eval_time_seconds(env);
    let implicit_clock_active = env.get(IMPLICIT_CLOCK_ACTIVE_ENV_KEY).to_bool();
    let tick_tol = 1.0e-12;

    SHIFT_SIGNAL_STATES.with(|states| {
        let mut states = states.borrow_mut();
        let state = states.entry(key).or_default();
        update_shift_signal_state(
            state,
            implicit_clock_active,
            time,
            input_value,
            startup_ticks,
            tick_tol,
        );
        T::from_f64(state.last_output)
    })
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

fn distribution_arg_hint(expr: Option<&Expression>) -> Option<String> {
    let Expression::VarRef { name, .. } = expr? else {
        return None;
    };
    let tail = name
        .as_str()
        .rsplit('.')
        .next()
        .unwrap_or(name.as_str())
        .split('[')
        .next()
        .unwrap_or(name.as_str());
    Some(tail.to_ascii_lowercase())
}

pub(super) fn eval_distribution_function<T: SimFloat>(
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match args.len() {
        // Overloaded MSL density helpers imported as `distribution(...)`.
        3 => {
            let u = eval_expr::<T>(&args[0], env);
            let p2 = eval_expr::<T>(&args[1], env);
            let p3 = eval_expr::<T>(&args[2], env);
            let h2 = distribution_arg_hint(args.get(1));
            let h3 = distribution_arg_hint(args.get(2));

            if matches!(h2.as_deref(), Some("u_min" | "y_min"))
                && matches!(h3.as_deref(), Some("u_max" | "y_max"))
            {
                return Some(eval_distribution_uniform(u, p2, p3));
            }
            if matches!(h2.as_deref(), Some("mu")) && matches!(h3.as_deref(), Some("sigma")) {
                return Some(eval_distribution_normal(u, p2, p3));
            }
            if matches!(h2.as_deref(), Some("lambda"))
                && matches!(h3.as_deref(), Some("k" | "shape"))
            {
                return Some(eval_distribution_weibull(u, p2, p3));
            }

            // Fallback when hints are unavailable.
            if p2.real() < 0.0 && p2.real() < p3.real() {
                Some(eval_distribution_uniform(u, p2, p3))
            } else if p2.real() > 0.0 && p3.real() > 0.0 && p2.real() > p3.real() {
                Some(eval_distribution_weibull(u, p2, p3))
            } else {
                Some(eval_distribution_normal(u, p2, p3))
            }
        }
        // Truncated distribution helper; if unresolved, keep finite bounded mapping.
        5 => {
            let r = eval_expr::<T>(&args[0], env);
            let y_min = eval_expr::<T>(&args[1], env);
            let y_max = eval_expr::<T>(&args[2], env);
            let span = y_max - y_min;
            let r_clamped = r.max(T::zero()).min(T::one());
            Some(y_min + span * r_clamped)
        }
        _ => None,
    }
}

pub(super) fn eval_clock_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" => {
            if short_name == "Clock" && args.is_empty() {
                return Some(T::from_bool(
                    env.get(IMPLICIT_CLOCK_ACTIVE_ENV_KEY).to_bool(),
                ));
            }
            if let Some(timing) = infer_clock_timing_from_call(short_name, args, env) {
                return Some(clock_tick_value(env, timing));
            }
            if short_name == "shiftSample" {
                return Some(eval_shift_sample_signal(short_name, args, env));
            }
            Some(
                args.first()
                    .map(|a| eval_expr::<T>(a, env))
                    .unwrap_or_else(T::zero),
            )
        }
        "hold" | "noClock" => Some(
            args.first()
                .map(|a| eval_expr::<T>(a, env))
                .unwrap_or_else(T::zero),
        ),
        "previous" => Some(eval_builtin(BuiltinFunction::Pre, args, env)),
        "interval" => {
            if let Some(arg) = args.first()
                && let Some(timing) = infer_clock_timing_from_expr(arg, env)
            {
                return Some(T::from_f64(timing.period));
            }
            Some(T::one())
        }
        "firstTick" => {
            let tol = 1e-12;
            Some(T::from_bool(eval_time_seconds(env).abs() <= tol))
        }
        _ => None,
    }
}
