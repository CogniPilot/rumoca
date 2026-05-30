use super::*;
use crate::eval::array_eval::eval_array_constructor_values;

pub(super) fn eval_builtin_math_and_event<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    if let Some(value) = eval_builtin_trigonometric(function, args, env) {
        return value;
    }
    if let Some(value) = eval_builtin_event_like(function, args, env) {
        return value;
    }
    eval_builtin_array_fallback(function, args, env)
}

fn eval_builtin_trigonometric<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let arg = |i: usize| -> T {
        let a = args
            .get(i)
            .expect("builtin trigonometric call must have validated arity");
        eval_expr_or_default::<T>(a, env)
    };

    match function {
        rumoca_core::BuiltinFunction::Sin => Some(arg(0).sin()),
        rumoca_core::BuiltinFunction::Cos => Some(arg(0).cos()),
        rumoca_core::BuiltinFunction::Tan => Some(arg(0).tan()),
        rumoca_core::BuiltinFunction::Asin => Some(arg(0).asin()),
        rumoca_core::BuiltinFunction::Acos => Some(arg(0).acos()),
        rumoca_core::BuiltinFunction::Atan => Some(arg(0).atan()),
        rumoca_core::BuiltinFunction::Atan2 => Some(arg(0).atan2(arg(1))),
        rumoca_core::BuiltinFunction::Sinh => Some(arg(0).sinh()),
        rumoca_core::BuiltinFunction::Cosh => Some(arg(0).cosh()),
        rumoca_core::BuiltinFunction::Tanh => Some(arg(0).tanh()),
        rumoca_core::BuiltinFunction::Exp => Some(arg(0).exp()),
        rumoca_core::BuiltinFunction::Log => Some(arg(0).ln()),
        rumoca_core::BuiltinFunction::Log10 => Some(arg(0).log10()),
        _ => None,
    }
}

fn eval_builtin_event_like<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let arg = |i: usize| -> T {
        let a = args
            .get(i)
            .expect("builtin event-like call must have validated arity");
        eval_expr_or_default::<T>(a, env)
    };

    match function {
        rumoca_core::BuiltinFunction::Edge => {
            let current = arg(0).to_bool();
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env).to_bool();
            Some(if current && !previous {
                T::one()
            } else {
                T::zero()
            })
        }
        rumoca_core::BuiltinFunction::Change => {
            let current = arg(0);
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env);
            Some(if !current.eq_approx(previous) {
                T::one()
            } else {
                T::zero()
            })
        }
        rumoca_core::BuiltinFunction::Initial => {
            Some(if env.is_initial { T::one() } else { T::zero() })
        }
        rumoca_core::BuiltinFunction::Terminal => Some(T::zero()),
        rumoca_core::BuiltinFunction::Sample => Some(eval_builtin_sample(args, env)),
        rumoca_core::BuiltinFunction::NoEvent | rumoca_core::BuiltinFunction::Delay => Some(arg(0)),
        rumoca_core::BuiltinFunction::Smooth => Some(arg(1)),
        rumoca_core::BuiltinFunction::Homotopy => Some(eval_builtin_homotopy(args, env)),
        _ => None,
    }
}

fn eval_builtin_homotopy<T: SimFloat>(args: &[rumoca_core::Expression], env: &VarEnv<T>) -> T {
    if !env.is_initial || args.len() < 2 {
        // MLS §3.7.4.3: during normal time steps, homotopy(actual, simplified) = actual.
        return eval_expr_or_default::<T>(
            args.first()
                .expect("homotopy() requires at least 1 argument — malformed Solve-IR"),
            env,
        );
    }
    // MLS §3.7.4.3: a translator may solve initialization equations
    // with a continuation from simplified -> actual, but ordinary
    // runtime evaluation must end at `actual`.
    let lambda = env
        .vars
        .get(INIT_HOMOTOPY_LAMBDA_KEY)
        .copied()
        .unwrap_or_else(T::one);
    let actual = eval_expr_or_default::<T>(&args[0], env);
    let simplified = eval_expr_or_default::<T>(&args[1], env);
    simplified * (T::one() - lambda) + actual * lambda
}

fn eval_builtin_array_fallback<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    // Best-effort scalar approximation for array builtins. These zero defaults are
    // intentional: this evaluator is not authoritative for array-valued results and
    // already emits a warning (see the `_ =>` arm below). The caller should not rely
    // on correct values for multi-element results.
    let arg = |i: usize| -> T {
        args.get(i)
            .map(|a| eval_expr_or_default::<T>(a, env))
            .unwrap_or(T::zero())
    };

    match function {
        rumoca_core::BuiltinFunction::Sum => eval_builtin_sum(args, env),
        rumoca_core::BuiltinFunction::Product => eval_builtin_product(args, env),
        rumoca_core::BuiltinFunction::Size => eval_builtin_size(args, env),
        rumoca_core::BuiltinFunction::Zeros => T::zero(),
        rumoca_core::BuiltinFunction::Ones => T::one(),
        rumoca_core::BuiltinFunction::Fill
        | rumoca_core::BuiltinFunction::Scalar
        | rumoca_core::BuiltinFunction::Vector
        | rumoca_core::BuiltinFunction::Matrix => arg(0),
        rumoca_core::BuiltinFunction::Transpose => {
            first_array_value_or_nan(eval_transpose_values(args, env))
        }
        rumoca_core::BuiltinFunction::Cross => {
            first_array_value_or_nan(eval_cross_values(args, env))
        }
        rumoca_core::BuiltinFunction::Skew => first_array_value_or_nan(eval_skew_values(args, env)),
        rumoca_core::BuiltinFunction::OuterProduct => {
            first_array_value_or_nan(eval_outer_product_values(args, env))
        }
        rumoca_core::BuiltinFunction::Symmetric => {
            first_array_value_or_nan(eval_symmetric_values(args, env))
        }
        rumoca_core::BuiltinFunction::Linspace => {
            first_array_value_or_nan(Some(eval_linspace_values(args, env)))
        }
        rumoca_core::BuiltinFunction::Identity => T::one(),
        rumoca_core::BuiltinFunction::Diagonal => {
            first_array_value_or_nan(eval_array_constructor_values(&function, args, env))
        }
        rumoca_core::BuiltinFunction::Cat => eval_cat_f64_values(args, env)
            .first()
            .copied()
            .map(T::from_f64)
            .unwrap_or_else(T::zero),
        _ => {
            if !env
                .runtime
                .warned_array_builtins
                .swap(true, Ordering::Relaxed)
            {
                tracing::warn!(
                    "Array builtin function {:?} not supported in scalar simulation evaluator, \
                     returning NaN. Results may be incorrect.",
                    function
                );
            }
            T::nan()
        }
    }
}

fn first_array_value_or_nan<T: SimFloat>(values: Option<Vec<T>>) -> T {
    values
        .and_then(|values| values.first().copied())
        .unwrap_or_else(T::nan)
}

fn eval_builtin_size<T: SimFloat>(args: &[rumoca_core::Expression], env: &VarEnv<T>) -> T {
    try_eval_builtin_size(args, env).unwrap_or_else(T::nan)
}

fn try_eval_builtin_size<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let arg = args.first()?;
    let dim_idx = match args.get(1) {
        Some(dim_expr) => {
            let raw = eval_expr_or_default::<T>(dim_expr, env).real();
            if !raw.is_finite() || raw < 1.0 || raw.fract() != 0.0 {
                return None;
            }
            raw as usize
        }
        None => 1,
    };
    let dims = infer_runtime_expr_dims(arg, env);
    let value = dims.get(dim_idx.checked_sub(1)?)?;
    Some(T::from_f64(*value as f64))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use std::sync::Arc;

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn int_literal(value: i64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn env_with_dims(name: &str, dims: Vec<i64>) -> VarEnv<f64> {
        let mut env = VarEnv::<f64>::new();
        let mut dim_map = IndexMap::new();
        dim_map.insert(name.to_string(), dims.clone());
        env.dims = Arc::new(dim_map);
        let values = vec![0.0; dims.iter().product::<i64>() as usize];
        set_array_entries(&mut env, name, &dims, values.as_slice());
        env
    }

    #[test]
    fn eval_builtin_size_returns_nan_for_out_of_range_dimension() {
        let env = env_with_dims("a", vec![2, 3]);
        let value = eval_builtin_size(&[var_ref("a"), int_literal(3)], &env);

        assert!(value.is_nan(), "size(a, 3) must not silently default");
    }

    #[test]
    fn eval_builtin_size_uses_state_shape_for_derivative() {
        let env = env_with_dims("x", vec![4]);
        let der_x = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![var_ref("x")],
            span: rumoca_core::Span::DUMMY,
        };

        let value = eval_builtin_size(&[der_x, int_literal(1)], &env);

        assert_eq!(value, 4.0);
    }
}
