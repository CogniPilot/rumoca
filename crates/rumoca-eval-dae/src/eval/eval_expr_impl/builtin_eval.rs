use super::*;
use crate::eval::builtin_runtime::eval_builtin_math_and_event;

fn sample_call_is_event_indicator<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    Ok(match args {
        // Lowered internal form: sample(id, start, interval).
        [_, _, _, ..] => true,
        // MLS §16.5.1: sample(value, clockExpr) is not an event boolean.
        [_, clock, ..] => infer_clock_timing_from_expr(clock, env)?.is_none(),
        _ => false,
    })
}

fn exact_clock_expr_is_left_limit_false<T: SimFloat>(
    name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let short = name.last_segment();
    Ok(matches!(
        short,
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
    ) && infer_clock_timing_from_call(short, args, env)?.is_some())
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

fn eval_var_ref_from_pre_store<T: SimFloat>(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let mut pre_env = env.clone();
    pre_env.vars = VarScope::new();
    for (key, value) in snapshot_pre_values_in(&env.runtime) {
        if key == "time" {
            continue;
        }
        pre_env.set(key.as_str(), T::from_f64(value));
    }
    match try_eval_var_ref(name, subscripts, span, &pre_env) {
        Ok(value) => Ok(Some(value)),
        Err(EvalError::MissingBinding { .. }) => Ok(None),
        Err(err) => Err(err),
    }
}

fn eval_expr_left_limit<T: SimFloat>(
    expr: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let value = match expr {
        rumoca_core::Expression::Literal { value: _, .. } => eval_expr::<T>(expr, env),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if name.as_str() == "time" && subscripts.is_empty() => Ok(T::from_f64(
            left_limit_time_value(env.require("time")?.real()),
        )),
        rumoca_core::Expression::VarRef { .. } => eval_expr::<T>(expr, env),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let l = eval_expr_left_limit::<T>(lhs, env)?;
            let r = eval_expr_left_limit::<T>(rhs, env)?;
            Ok(match op {
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
                rumoca_core::OpBinary::Empty | rumoca_core::OpBinary::Assign => {
                    return Err(EvalError::UnsupportedExpression {
                        kind: "placeholder binary operator",
                    });
                }
            })
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let r = eval_expr_left_limit::<T>(rhs, env)?;
            Ok(match op {
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => -r,
                rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotPlus
                | rumoca_core::OpUnary::Empty => r,
                rumoca_core::OpUnary::Not => T::from_bool(!r.to_bool()),
            })
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => match function {
            // MLS §16.5.1 / Appendix B: sample(start, interval) is an event
            // indicator, so its left-limit value is false at the tick.
            rumoca_core::BuiltinFunction::Sample if sample_call_is_event_indicator(args, env)? => {
                Ok(T::zero())
            }
            // Derived event indicators are also false on the event left-limit.
            rumoca_core::BuiltinFunction::Edge | rumoca_core::BuiltinFunction::Change => {
                Ok(T::zero())
            }
            rumoca_core::BuiltinFunction::NoEvent | rumoca_core::BuiltinFunction::Delay => {
                eval_expr_left_limit::<T>(require_builtin_arg(args, 0)?, env)
            }
            rumoca_core::BuiltinFunction::Smooth if args.len() >= 2 => {
                eval_expr_left_limit::<T>(&args[1], env)
            }
            rumoca_core::BuiltinFunction::Homotopy => {
                eval_expr_left_limit::<T>(require_builtin_arg(args, 0)?, env)
            }
            rumoca_core::BuiltinFunction::Pre => eval_builtin_pre(args, env),
            _ => eval_expr::<T>(expr, env),
        },
        rumoca_core::Expression::FunctionCall { name, args, .. }
            if exact_clock_expr_is_left_limit_false(name.var_name(), args, env)? =>
        {
            Ok(T::zero())
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_expr_left_limit::<T>(condition, env)?.to_bool() {
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
        | rumoca_core::Expression::Empty { .. } => eval_expr::<T>(expr, env),
    };
    value.map_err(|err| match expr.span() {
        Some(span) => err.with_span_if_missing(span),
        None => err,
    })
}

pub(in crate::eval) fn eval_builtin_pre<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let Some(arg0) = args.first() else {
        return Ok(T::zero());
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
        return Ok(T::zero());
    }

    if let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = arg0
    {
        if subscripts.is_empty()
            && let Some(value) = lookup_pre_value_in(&env.runtime, name.as_str())
        {
            return Ok(T::from_f64(value));
        }
        if let Some(value) = eval_var_ref_from_pre_store(name.var_name(), subscripts, *span, env)? {
            return Ok(value);
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
) -> Result<T, EvalError> {
    let Some(arg0) = args.first() else {
        return Ok(T::zero());
    };

    if let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = arg0
    {
        if subscripts.is_empty()
            && let Some(value) = lookup_pre_value(name.as_str())
        {
            return Ok(T::from_f64(value));
        }
        if let Some(value) = eval_var_ref_from_pre_store(name.var_name(), subscripts, *span, env)? {
            return Ok(value);
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
) -> Result<T, EvalError> {
    match function {
        rumoca_core::BuiltinFunction::Der => try_eval_der(args, env),
        rumoca_core::BuiltinFunction::Pre => {
            require_builtin_arg(args, 0)?;
            eval_builtin_pre(args, env)
        }

        rumoca_core::BuiltinFunction::Abs => Ok(eval_builtin_arg(args, 0, env)?.abs()),
        rumoca_core::BuiltinFunction::Sign => Ok(eval_builtin_arg(args, 0, env)?.sign()),
        rumoca_core::BuiltinFunction::Sqrt => Ok(eval_builtin_arg(args, 0, env)?.sqrt()),
        rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
            Ok(eval_builtin_arg(args, 0, env)?.floor())
        }
        rumoca_core::BuiltinFunction::Ceil => Ok(eval_builtin_arg(args, 0, env)?.ceil()),
        rumoca_core::BuiltinFunction::Min => eval_builtin_min(args, env),
        rumoca_core::BuiltinFunction::Max => eval_builtin_max(args, env),
        rumoca_core::BuiltinFunction::Size => eval_builtin_size(args, env),
        rumoca_core::BuiltinFunction::Div => try_eval_div_mod_rem(args, env, DivKind::Div),
        rumoca_core::BuiltinFunction::Mod => try_eval_div_mod_rem(args, env, DivKind::Mod),
        rumoca_core::BuiltinFunction::Rem => try_eval_div_mod_rem(args, env, DivKind::Rem),
        rumoca_core::BuiltinFunction::SemiLinear => {
            let x = eval_builtin_arg(args, 0, env)?;
            let positive = eval_builtin_arg(args, 1, env)?;
            let negative = eval_builtin_arg(args, 2, env)?;
            Ok(if x.real() >= 0.0 {
                positive * x
            } else {
                negative * x
            })
        }

        _ => eval_builtin_math_and_event(function, args, env),
    }
}

fn eval_builtin_size<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let array_arg = require_builtin_arg(args, 0)?;
    let dim = eval_builtin_arg(args, 1, env)?.real();
    if dim.fract().abs() > 1.0e-12 || dim < 1.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "size dimension",
        });
    }
    let dim_index = (dim as usize)
        .checked_sub(1)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "size dimension",
        })?;
    let dims = try_infer_runtime_expr_dims(array_arg, env)?;
    let value = dims
        .get(dim_index)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "size dimension",
        })?;
    Ok(T::from_f64(*value as f64))
}

fn try_eval_der<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let arg = require_builtin_arg(args, 0)?;
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg
    else {
        return Err(EvalError::UnsupportedExpression { kind: "der" });
    };
    if !subscripts.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "subscripted der() must be lowered before DAE evaluation",
        });
    }
    let full_name = name.as_str();
    eval_var_ref_no_subscripts(&format!("der({full_name})"), env)?.ok_or_else(|| {
        EvalError::MissingBinding {
            name: format!("der({full_name})"),
        }
    })
}

fn require_builtin_arg(
    args: &[rumoca_core::Expression],
    idx: usize,
) -> Result<&rumoca_core::Expression, EvalError> {
    args.get(idx).ok_or(EvalError::UnsupportedExpression {
        kind: "builtin arity",
    })
}

fn eval_builtin_arg<T: SimFloat>(
    args: &[rumoca_core::Expression],
    idx: usize,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    eval_expr::<T>(require_builtin_arg(args, idx)?, env)
}

fn eval_builtin_min<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if args.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "builtin arity",
        });
    }
    if args.len() == 1 {
        return reduce_array_argument_result(&args[0], env, |acc, v| acc.min(v));
    }
    let mut it = args.iter().map(|expr| eval_expr::<T>(expr, env));
    let first = it.next().ok_or(EvalError::UnsupportedExpression {
        kind: "builtin arity",
    })??;
    it.try_fold(first, |acc, value| value.map(|v| acc.min(v)))
}

fn eval_builtin_max<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if args.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "builtin arity",
        });
    }
    if args.len() == 1 {
        return reduce_array_argument_result(&args[0], env, |acc, v| acc.max(v));
    }
    let mut it = args.iter().map(|expr| eval_expr::<T>(expr, env));
    let first = it.next().ok_or(EvalError::UnsupportedExpression {
        kind: "builtin arity",
    })??;
    it.try_fold(first, |acc, value| value.map(|v| acc.max(v)))
}

fn reduce_array_argument_result<T: SimFloat, F: FnMut(T, T) -> T>(
    arg: &rumoca_core::Expression,
    env: &VarEnv<T>,
    reduce: F,
) -> Result<T, EvalError> {
    let mut values = eval_array_like_values(arg, env)?.into_iter();
    let first = values.next().ok_or(EvalError::UnsupportedExpression {
        kind: "array reduction",
    })?;
    Ok(values.fold(first, reduce))
}

fn try_eval_div_mod_rem<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    kind: DivKind,
) -> Result<T, EvalError> {
    Ok(eval_div_mod_rem(
        eval_builtin_arg(args, 0, env)?,
        eval_builtin_arg(args, 1, env)?,
        kind,
    ))
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
