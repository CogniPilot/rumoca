use super::*;

pub(super) fn eval_boolean_vector_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let values = || match args.first() {
        Some(expr) => eval_array_like_f64_values(expr, env),
        None => Vec::new(),
    };
    match short_name {
        "anyTrue" => Some(T::from_bool(values().iter().any(|v| *v != 0.0))),
        "andTrue" => {
            let vals = values();
            Some(T::from_bool(
                !vals.is_empty() && vals.iter().all(|v| *v != 0.0),
            ))
        }
        "firstTrueIndex" => {
            let idx = values()
                .iter()
                .position(|v| *v != 0.0)
                .map(|i| i + 1)
                .unwrap_or(0);
            Some(T::from_f64(idx as f64))
        }
        _ => None,
    }
}

pub(super) fn eval_unit_conversion_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        "to_degC" => Some(eval_expr_or_default::<T>(&args[0], env) + T::from_f64(-273.15)),
        "from_degC" => Some(eval_expr_or_default::<T>(&args[0], env) + T::from_f64(273.15)),
        "to_deg" => Some(
            eval_expr_or_default::<T>(&args[0], env) * T::from_f64(180.0 / std::f64::consts::PI),
        ),
        "from_deg" => Some(
            eval_expr_or_default::<T>(&args[0], env) * T::from_f64(std::f64::consts::PI / 180.0),
        ),
        _ => None,
    }
}

pub(super) fn eval_stream_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    match short_name {
        // MLS stream operators; runtime currently uses direct argument passthrough.
        // This avoids NaN cascades until full connector-flow-dependent semantics
        // are implemented in the simulator.
        "actualStream" | "inStream" => Some(
            args.first()
                .map(|arg| eval_expr_or_default::<T>(arg, env))
                .unwrap_or_else(T::zero),
        ),
        // Accept impure stream/file helpers in simulation so examples that only
        // observe the returned success flag remain executable.
        "writeRealMatrix" => Some(T::one()),
        _ => None,
    }
}

pub(super) fn eval_state_accessor_special_function<T: SimFloat>(
    short_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let field = match short_name {
        "temperature" => "T",
        "pressure" => "p",
        "density" => "d",
        "specificEnthalpy" => "h",
        "specificInternalEnergy" => "u",
        "specificEntropy" => "s",
        _ => return None,
    };
    eval_state_accessor_from_expr(args.first()?, field, env)
}

fn eval_state_accessor_from_expr<T: SimFloat>(
    expr: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => eval_state_accessor_from_var_ref(name, subscripts, field, env),
        Expression::FieldAccess {
            base,
            field: member,
            ..
        } => {
            let base_path = eval_state_accessor_field_path(base, env)?;
            let state_name = VarName::new(format!("{base_path}.{member}"));
            let state_ref = rumoca_core::Reference::from_var_name(state_name);
            eval_state_accessor_from_var_ref(&state_ref, &[], field, env)
        }
        Expression::FunctionCall { name, args, .. } => {
            eval_state_accessor_from_set_state(name.as_str(), args, field, env)
        }
        _ => None,
    }
}

fn eval_state_accessor_field_path<T: SimFloat>(
    expr: &Expression,
    env: &VarEnv<T>,
) -> Option<String> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Some(name.to_string());
            }
            let mut idx_parts = Vec::with_capacity(subscripts.len());
            for sub in subscripts {
                let idx = match sub {
                    Subscript::Index { value: i, .. } => *i,
                    Subscript::Expr { expr: e, .. } => {
                        eval_expr_or_default::<T>(e, env).real().round() as i64
                    }
                    Subscript::Colon { .. } => return None,
                };
                idx_parts.push(idx.to_string());
            }
            Some(format!("{}[{}]", name.as_str(), idx_parts.join(",")))
        }
        Expression::FieldAccess { base, field, .. } => {
            let prefix = eval_state_accessor_field_path(base, env)?;
            Some(format!("{prefix}.{field}"))
        }
        _ => None,
    }
}

fn eval_state_accessor_from_var_ref<T: SimFloat>(
    name: &rumoca_core::Reference,
    subscripts: &[Subscript],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let mut candidates = Vec::new();
    if subscripts.is_empty() {
        let base = name.as_str();
        candidates.push(base.to_string());
        // Scalar evaluator fallback for arrays of state records.
        candidates.push(format!("{base}[1]"));
    } else {
        let mut idx_parts = Vec::with_capacity(subscripts.len());
        for sub in subscripts {
            let idx = match sub {
                Subscript::Index { value: i, .. } => *i,
                Subscript::Expr { expr, .. } => {
                    eval_expr_or_default::<T>(expr, env).real().round() as i64
                }
                Subscript::Colon { .. } => return None,
            };
            idx_parts.push(idx.to_string());
        }
        candidates.push(format!("{}[{}]", name.as_str(), idx_parts.join(",")));
    }

    for base in candidates {
        let key = format!("{base}.{field}");
        if let Some(value) = env.vars.get(&key).copied() {
            return Some(value);
        }
    }
    None
}

pub(in crate::eval) fn eval_state_accessor_from_set_state<T: SimFloat>(
    name: &str,
    args: &[Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Option<T> {
    let short_name = rumoca_core::top_level_last_segment(name);
    match short_name {
        // setState_pTX(p, T, X)
        "setState_pTX" | "setState_pT" => match field {
            "p" => args.first().map(|e| eval_expr_or_default::<T>(e, env)),
            "T" => args.get(1).map(|e| eval_expr_or_default::<T>(e, env)),
            _ => None,
        },
        // setState_dTX(d, T, X)
        "setState_dTX" => match field {
            "d" => args.first().map(|e| eval_expr_or_default::<T>(e, env)),
            "T" => args.get(1).map(|e| eval_expr_or_default::<T>(e, env)),
            _ => None,
        },
        // setState_phX(p, h, X)
        "setState_phX" | "setState_ph" => match field {
            "p" => args.first().map(|e| eval_expr_or_default::<T>(e, env)),
            "h" => args.get(1).map(|e| eval_expr_or_default::<T>(e, env)),
            "T" => eval_state_accessor_via_user_helper(
                name,
                args,
                env,
                &["temperature_phX", "temperature_ph"],
            ),
            _ => None,
        },
        // setState_psX(p, s, X)
        "setState_psX" | "setState_ps" => match field {
            "p" => args.first().map(|e| eval_expr_or_default::<T>(e, env)),
            "s" => args.get(1).map(|e| eval_expr_or_default::<T>(e, env)),
            "T" => eval_state_accessor_via_user_helper(
                name,
                args,
                env,
                &["temperature_psX", "temperature_ps"],
            ),
            _ => None,
        },
        // setSmoothState(x, state_a, state_b, x_small)
        "setSmoothState" => {
            let x = args
                .first()
                .map(|e| eval_expr_or_default::<T>(e, env).real())?;
            let a = args
                .get(1)
                .and_then(|e| eval_state_accessor_from_expr(e, field, env))?;
            let b = args
                .get(2)
                .and_then(|e| eval_state_accessor_from_expr(e, field, env))?;
            let x_small = match args.get(3) {
                Some(expr) => eval_expr_or_default::<T>(expr, env).real().abs(),
                None => 0.0,
            };
            if x_small <= f64::EPSILON {
                return Some(if x >= 0.0 { a } else { b });
            }
            if x >= x_small {
                return Some(a);
            }
            if x <= -x_small {
                return Some(b);
            }
            let alpha = (x / x_small + 1.0) * 0.5;
            Some(b + T::from_f64(alpha) * (a - b))
        }
        _ => None,
    }
}

fn eval_state_accessor_via_user_helper<T: SimFloat>(
    constructor_name: &str,
    args: &[Expression],
    env: &VarEnv<T>,
    helper_suffixes: &[&str],
) -> Option<T> {
    let (prefix, _) = rumoca_core::split_last_top_level(constructor_name)?;
    for suffix in helper_suffixes {
        let helper = VarName::new(format!("{prefix}.{suffix}"));
        if let Some(v) = eval_user_function_call(&helper, args, env) {
            return Some(v);
        }
    }
    None
}

#[derive(Default, Debug)]
pub(in crate::eval) struct ImpureRandomRegistry {
    pub(super) next_id: i64,
    pub(super) auto_seed_counter: u64,
    pub(super) streams: HashMap<i64, u64>,
}
