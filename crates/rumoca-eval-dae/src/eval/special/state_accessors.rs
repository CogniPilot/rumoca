use super::*;

pub(super) fn eval_boolean_vector_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let values = || -> Result<Vec<f64>, EvalError> {
        match args.first() {
            Some(expr) => eval_array_like_f64_values(expr, env),
            None => Ok(Vec::new()),
        }
    };
    match short_name {
        "anyTrue" => Ok(Some(T::from_bool(values()?.iter().any(|v| *v != 0.0)))),
        "andTrue" => {
            let vals = values()?;
            Ok(Some(T::from_bool(
                !vals.is_empty() && vals.iter().all(|v| *v != 0.0),
            )))
        }
        "firstTrueIndex" => {
            let idx = values()?
                .iter()
                .position(|v| *v != 0.0)
                .map(|i| i + 1)
                .unwrap_or(0);
            Ok(Some(T::from_f64(idx as f64)))
        }
        _ => Ok(None),
    }
}

pub(super) fn eval_unit_conversion_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let arg0 = || -> Result<&Expression, EvalError> {
        args.first().ok_or(EvalError::UnsupportedExpression {
            kind: "unit conversion argument",
        })
    };
    match short_name {
        "to_degC" => Ok(Some(eval_expr::<T>(arg0()?, env)? + T::from_f64(-273.15))),
        "from_degC" => Ok(Some(eval_expr::<T>(arg0()?, env)? + T::from_f64(273.15))),
        "to_deg" => Ok(Some(
            eval_expr::<T>(arg0()?, env)? * T::from_f64(180.0 / std::f64::consts::PI),
        )),
        "from_deg" => Ok(Some(
            eval_expr::<T>(arg0()?, env)? * T::from_f64(std::f64::consts::PI / 180.0),
        )),
        _ => Ok(None),
    }
}

pub(super) fn eval_stream_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match short_name {
        // MLS stream operators are lowered to direct argument passthrough only
        // once connector flow semantics have been resolved upstream.
        "actualStream" | "inStream" => {
            let arg = args.first().ok_or(EvalError::UnsupportedExpression {
                kind: "stream operator argument",
            })?;
            eval_expr::<T>(arg, env).map(Some)
        }
        "writeRealMatrix" => Err(EvalError::UnsupportedExpression {
            kind: "writeRealMatrix side effect",
        }),
        _ => Ok(None),
    }
}

pub(super) fn eval_state_accessor_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let field = match short_name {
        "temperature" => "T",
        "pressure" => "p",
        "density" => "d",
        "specificEnthalpy" => "h",
        "specificInternalEnergy" => "u",
        "specificEntropy" => "s",
        _ => return Ok(None),
    };
    let Some(arg) = args.first() else {
        return Ok(None);
    };
    eval_state_accessor_from_expr(arg, field, env)
}

fn eval_state_accessor_from_expr<T: SimFloat>(
    expr: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => eval_state_accessor_from_var_ref(name, subscripts, field, env),
        Expression::FieldAccess {
            base,
            field: member,
            ..
        } => {
            let Some(base_path) = eval_state_accessor_field_path(base, env)? else {
                return Ok(None);
            };
            let state_name = VarName::new(format!("{base_path}.{member}"));
            let state_ref = rumoca_core::Reference::from_var_name(state_name);
            eval_state_accessor_from_var_ref(&state_ref, &[], field, env)
        }
        Expression::FunctionCall { name, args, .. } => {
            eval_state_accessor_from_set_state(name.var_name(), args, field, env)
        }
        _ => Ok(None),
    }
}

fn eval_state_accessor_field_path<T: SimFloat>(
    expr: &Expression,
    env: &VarEnv<T>,
) -> Result<Option<String>, EvalError> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Ok(Some(name.to_string()));
            }
            let Some(indices) = eval_state_accessor_subscripts(subscripts, env)? else {
                return Ok(None);
            };
            Ok(Some(dae::format_subscript_key(name.as_str(), &indices)))
        }
        Expression::FieldAccess { base, field, .. } => {
            let Some(prefix) = eval_state_accessor_field_path(base, env)? else {
                return Ok(None);
            };
            Ok(Some(format!("{prefix}.{field}")))
        }
        _ => Ok(None),
    }
}

fn eval_state_accessor_subscripts<T: SimFloat>(
    subscripts: &[Subscript],
    env: &VarEnv<T>,
) -> Result<Option<Vec<usize>>, EvalError> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let index = match subscript {
            Subscript::Index { value, span } => positive_state_accessor_index(*value, *span)?,
            Subscript::Expr { expr, span } => {
                let value = eval_expr::<T>(expr, env)?.real();
                finite_integer_state_accessor_index(value, *span)?
            }
            Subscript::Colon { .. } => return Ok(None),
        };
        indices.push(index);
    }
    Ok(Some(indices))
}

fn positive_state_accessor_index(index: i64, span: rumoca_core::Span) -> Result<usize, EvalError> {
    usize::try_from(index)
        .ok()
        .filter(|index| *index > 0)
        .ok_or_else(|| spanned_unsupported("state accessor subscript index", span))
}

fn finite_integer_state_accessor_index(
    value: f64,
    span: rumoca_core::Span,
) -> Result<usize, EvalError> {
    if !value.is_finite() || value.fract() != 0.0 {
        return Err(spanned_unsupported(
            "state accessor subscript expression",
            span,
        ));
    }
    positive_state_accessor_index(value as i64, span)
}

fn spanned_unsupported(kind: &'static str, span: rumoca_core::Span) -> EvalError {
    EvalError::Spanned {
        source: Box::new(EvalError::UnsupportedExpression { kind }),
        span,
    }
}

fn eval_state_accessor_from_var_ref<T: SimFloat>(
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let base = if subscripts.is_empty() {
        name.as_str().to_string()
    } else {
        let Some(indices) = eval_state_accessor_subscripts(subscripts, env)? else {
            return Ok(None);
        };
        dae::format_subscript_key(name.as_str(), &indices)
    };
    let key = format!("{base}.{field}");
    if let Some(value) = env.vars.get(&key).copied() {
        return Ok(Some(value));
    }
    Ok(None)
}

pub(in crate::eval) fn eval_state_accessor_from_set_state<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let short_name = name.last_segment();
    match short_name {
        // setState_pTX(p, T, X)
        "setState_pTX" | "setState_pT" => match field {
            "p" => optional_eval_arg(args.first(), env),
            "T" => optional_eval_arg(args.get(1), env),
            _ => Ok(None),
        },
        // setState_dTX(d, T, X)
        "setState_dTX" => match field {
            "d" => optional_eval_arg(args.first(), env),
            "T" => optional_eval_arg(args.get(1), env),
            _ => Ok(None),
        },
        // setState_phX(p, h, X)
        "setState_phX" | "setState_ph" => match field {
            "p" => optional_eval_arg(args.first(), env),
            "h" => optional_eval_arg(args.get(1), env),
            "T" => eval_state_accessor_via_user_helper(
                name,
                args,
                env,
                &["temperature_phX", "temperature_ph"],
            ),
            _ => Ok(None),
        },
        // setState_psX(p, s, X)
        "setState_psX" | "setState_ps" => match field {
            "p" => optional_eval_arg(args.first(), env),
            "s" => optional_eval_arg(args.get(1), env),
            "T" => eval_state_accessor_via_user_helper(
                name,
                args,
                env,
                &["temperature_psX", "temperature_ps"],
            ),
            _ => Ok(None),
        },
        // setSmoothState(x, state_a, state_b, x_small)
        "setSmoothState" => {
            let x = args
                .first()
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "setSmoothState arity",
                })
                .and_then(|e| eval_expr::<T>(e, env).map(|value| value.real()))?;
            let a = args
                .get(1)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "setSmoothState arity",
                })
                .and_then(|e| eval_state_accessor_from_expr(e, field, env))?
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "setSmoothState state",
                })?;
            let b = args
                .get(2)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "setSmoothState arity",
                })
                .and_then(|e| eval_state_accessor_from_expr(e, field, env))?
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "setSmoothState state",
                })?;
            let x_small = match args.get(3) {
                Some(expr) => eval_expr::<T>(expr, env)?.real().abs(),
                None => 0.0,
            };
            if x_small <= f64::EPSILON {
                return Ok(Some(if x >= 0.0 { a } else { b }));
            }
            if x >= x_small {
                return Ok(Some(a));
            }
            if x <= -x_small {
                return Ok(Some(b));
            }
            let alpha = (x / x_small + 1.0) * 0.5;
            Ok(Some(b + T::from_f64(alpha) * (a - b)))
        }
        _ => Ok(None),
    }
}

fn optional_eval_arg<T: SimFloat>(
    expr: Option<&Expression>,
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    match expr {
        Some(expr) => eval_expr::<T>(expr, env).map(Some),
        None => Ok(None),
    }
}

fn eval_state_accessor_via_user_helper<T: SimFloat>(
    constructor_name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
    helper_suffixes: &[&str],
) -> Result<Option<T>, EvalError> {
    let Some(prefix) = constructor_name.enclosing_scope() else {
        return Ok(None);
    };
    for suffix in helper_suffixes {
        let helper = VarName::new(format!("{prefix}.{suffix}"));
        if let Some(v) =
            eval_user_function_call(&rumoca_core::Reference::from_var_name(helper), args, env)?
        {
            return Ok(Some(v));
        }
    }
    Ok(None)
}

#[derive(Default, Debug)]
pub(in crate::eval) struct ImpureRandomRegistry {
    pub(super) next_id: i64,
    pub(super) auto_seed_counter: u64,
    pub(super) streams: HashMap<i64, u64>,
}
