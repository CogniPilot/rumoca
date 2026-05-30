use super::*;
use crate::eval::builtin_runtime::eval_builtin_math_and_event;

fn sample_call_is_event_indicator<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> bool {
    match args {
        // Lowered internal form: sample(id, start, interval).
        [_, _, _, ..] => true,
        // MLS §16.5.1: sample(value, clockExpr) is not an event boolean.
        [_, clock, ..] => infer_clock_timing_from_expr(clock, env).is_none(),
        _ => false,
    }
}

fn exact_clock_expr_is_left_limit_false<T: SimFloat>(
    name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> bool {
    let short = rumoca_core::top_level_last_segment(name.as_str());
    matches!(
        short,
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
    ) && infer_clock_timing_from_call(short, args, env).is_some()
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

fn eval_expr_left_limit<T: SimFloat>(expr: &rumoca_core::Expression, env: &VarEnv<T>) -> T {
    match expr {
        rumoca_core::Expression::Literal { value: _, .. } => eval_expr_or_default::<T>(expr, env),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if name.as_str() == "time" && subscripts.is_empty() => {
            T::from_f64(left_limit_time_value(env.get("time").real()))
        }
        rumoca_core::Expression::VarRef { .. } => eval_expr_or_default::<T>(expr, env),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let l = eval_expr_left_limit::<T>(lhs, env);
            let r = eval_expr_left_limit::<T>(rhs, env);
            match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => l + r,
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => l - r,
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => l * r,
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => l / r,
                rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => l.powf(r),
                rumoca_core::OpBinary::And => T::from_bool(l.to_bool() && r.to_bool()),
                rumoca_core::OpBinary::Or => T::from_bool(l.to_bool() || r.to_bool()),
                rumoca_core::OpBinary::Lt => T::from_bool(l.lt(r)),
                rumoca_core::OpBinary::Le => T::from_bool(l.le(r)),
                rumoca_core::OpBinary::Gt => T::from_bool(l.gt(r)),
                rumoca_core::OpBinary::Ge => T::from_bool(l.ge(r)),
                rumoca_core::OpBinary::Eq => T::from_bool(l.eq_approx(r)),
                rumoca_core::OpBinary::Neq => T::from_bool(!l.eq_approx(r)),
                rumoca_core::OpBinary::Empty | rumoca_core::OpBinary::Assign => T::zero(),
            }
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let r = eval_expr_left_limit::<T>(rhs, env);
            match op {
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => -r,
                rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotPlus
                | rumoca_core::OpUnary::Empty => r,
                rumoca_core::OpUnary::Not => T::from_bool(!r.to_bool()),
            }
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => match function {
            // MLS §16.5.1 / Appendix B: sample(start, interval) is an event
            // indicator, so its left-limit value is false at the tick.
            rumoca_core::BuiltinFunction::Sample if sample_call_is_event_indicator(args, env) => {
                T::zero()
            }
            // Derived event indicators are also false on the event left-limit.
            rumoca_core::BuiltinFunction::Edge | rumoca_core::BuiltinFunction::Change => T::zero(),
            rumoca_core::BuiltinFunction::NoEvent | rumoca_core::BuiltinFunction::Delay => args
                .first()
                .map(|arg| eval_expr_left_limit::<T>(arg, env))
                .unwrap_or_else(T::zero),
            rumoca_core::BuiltinFunction::Smooth if args.len() >= 2 => {
                eval_expr_left_limit::<T>(&args[1], env)
            }
            rumoca_core::BuiltinFunction::Homotopy => args
                .first()
                .map(|arg| eval_expr_left_limit::<T>(arg, env))
                .unwrap_or_else(T::zero),
            rumoca_core::BuiltinFunction::Pre => eval_builtin_pre(args, env),
            _ => eval_expr_or_default::<T>(expr, env),
        },
        rumoca_core::Expression::FunctionCall { name, args, .. }
            if exact_clock_expr_is_left_limit_false(name.var_name(), args, env) =>
        {
            T::zero()
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_expr_left_limit::<T>(condition, env).to_bool() {
                    return eval_expr_left_limit::<T>(value, env);
                }
            }
            eval_expr_left_limit::<T>(else_branch, env)
        }
        rumoca_core::Expression::FieldAccess { .. }
        | rumoca_core::Expression::FunctionCall { .. }
        | rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Index { .. }
        | rumoca_core::Expression::Range { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::ArrayComprehension { .. }
        | rumoca_core::Expression::Empty { .. } => eval_expr_or_default::<T>(expr, env),
    }
}

pub(in crate::eval) fn eval_builtin_pre<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    let Some(arg0) = args.first() else {
        return T::zero();
    };

    if matches!(
        arg0,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            ..
        }
    ) {
        // MLS §8.6: `initial()` is true only during the initial event, so its
        // left-limit value is false and `edge(initial())` fires exactly once.
        return T::zero();
    }

    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg0
    {
        let key = if subscripts.is_empty() {
            name.as_str().to_string()
        } else {
            let indices = eval_subscript_indices(subscripts, env);
            format!("{}[{}]", name.as_str(), indices.join(","))
        };

        if let Some(value) = lookup_pre_value_in(&env.runtime, &key) {
            return T::from_f64(value);
        }
        if let Some(normalized) = normalize_var_name::<T>(&key, env)
            && let Some(value) = lookup_pre_value_in(&env.runtime, normalized.as_str())
        {
            return T::from_f64(value);
        }
        if let Some(base_name) = unity_subscript_base_name(&key)
            && let Some(value) = lookup_pre_value_in(&env.runtime, base_name.as_str())
        {
            return T::from_f64(value);
        }
    }

    let mut pre_env = env.clone();
    for (name, value) in snapshot_pre_values_in(&env.runtime) {
        if name == "time" {
            continue;
        }
        pre_env.set(name.as_str(), T::from_f64(value));
    }
    eval_expr_left_limit::<T>(arg0, &pre_env)
}

pub(in crate::eval) fn eval_builtin_previous<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    let Some(arg0) = args.first() else {
        return T::zero();
    };

    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg0
    {
        let key = if subscripts.is_empty() {
            name.as_str().to_string()
        } else {
            let indices = eval_subscript_indices(subscripts, env);
            format!("{}[{}]", name.as_str(), indices.join(","))
        };

        if let Some(value) = lookup_pre_value(&key) {
            return T::from_f64(value);
        }
        if let Some(normalized) = normalize_var_name::<T>(&key, env)
            && let Some(value) = lookup_pre_value(normalized.as_str())
        {
            return T::from_f64(value);
        }
        if let Some(base_name) = unity_subscript_base_name(&key)
            && let Some(value) = lookup_pre_value(base_name.as_str())
        {
            return T::from_f64(value);
        }
        // MLS §16.5.1 / §16.4: at the first clock tick, previous(v) reads the
        // declared start value of v, or the type default when no explicit
        // start is present. It must not fall through to the current env value.
        return previous_start_or_default(arg0, env);
    }

    eval_builtin_pre(args, env)
}

pub(in crate::eval) fn eval_builtin<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    let arg = |i: usize| -> T {
        args.get(i)
            .map(|a| eval_expr_or_default::<T>(a, env))
            .unwrap_or(T::zero())
    };

    match function {
        rumoca_core::BuiltinFunction::Der => {
            let der_name = args.first().and_then(|arg| {
                if let rumoca_core::Expression::VarRef {
                    name, subscripts, ..
                } = arg
                {
                    build_indexed_name(name.as_str(), subscripts, env)
                        .ok()
                        .map(|full_name| format!("der({full_name})"))
                } else {
                    None
                }
            });
            if let Some(der_name) = der_name {
                env.get(&der_name)
            } else {
                T::zero()
            }
        }
        rumoca_core::BuiltinFunction::Pre => eval_builtin_pre(args, env),

        // Math functions
        rumoca_core::BuiltinFunction::Abs => arg(0).abs(),
        rumoca_core::BuiltinFunction::Sign => arg(0).sign(),
        rumoca_core::BuiltinFunction::Sqrt => arg(0).sqrt(),
        rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
            arg(0).floor()
        }
        rumoca_core::BuiltinFunction::Ceil => arg(0).ceil(),
        rumoca_core::BuiltinFunction::Min => eval_builtin_min(args, env),
        rumoca_core::BuiltinFunction::Max => eval_builtin_max(args, env),
        rumoca_core::BuiltinFunction::Div => eval_div_mod_rem(arg(0), arg(1), DivKind::Div),
        rumoca_core::BuiltinFunction::Mod => eval_div_mod_rem(arg(0), arg(1), DivKind::Mod),
        rumoca_core::BuiltinFunction::Rem => eval_div_mod_rem(arg(0), arg(1), DivKind::Rem),
        rumoca_core::BuiltinFunction::SemiLinear => {
            let x = arg(0);
            if x.real() >= 0.0 {
                arg(1) * x
            } else {
                arg(2) * x
            }
        }

        // Trig / hyperbolic / exp
        _ => eval_builtin_math_and_event(function, args, env),
    }
}

pub(super) fn eval_builtin_min<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        return reduce_array_argument(&args[0], env, |acc, v| acc.min(v), T::zero());
    }
    let mut it = args.iter().map(|expr| eval_expr_or_default::<T>(expr, env));
    let first = it.next().unwrap_or_else(T::zero);
    it.fold(first, |acc, v| acc.min(v))
}

pub(super) fn eval_builtin_max<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        return reduce_array_argument(&args[0], env, |acc, v| acc.max(v), T::zero());
    }
    let mut it = args.iter().map(|expr| eval_expr_or_default::<T>(expr, env));
    let first = it.next().unwrap_or_else(T::zero);
    it.fold(first, |acc, v| acc.max(v))
}

pub(super) fn reduce_array_argument<T: SimFloat, F: FnMut(T, T) -> T>(
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
    reduce: F,
    default: T,
) -> T {
    let mut values = eval_array_like_values(arg, env).into_iter();
    let Some(first) = values.next() else {
        return default;
    };
    values.fold(first, reduce)
}

pub(super) enum DivKind {
    Div,
    Mod,
    Rem,
}

pub(super) fn eval_div_mod_rem<T: SimFloat>(x: T, divisor: T, kind: DivKind) -> T {
    if divisor.real() == 0.0 {
        return T::zero();
    }
    match kind {
        DivKind::Div => (x / divisor).trunc(),
        DivKind::Mod => x - (x / divisor).floor() * divisor,
        DivKind::Rem => x.modulo(divisor),
    }
}
