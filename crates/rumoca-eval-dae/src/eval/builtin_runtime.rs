use super::*;
use crate::eval::array_eval::eval_array_constructor_values;

pub(super) fn eval_builtin_math_and_event<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if let Some(value) = eval_builtin_trigonometric(function, args, env)? {
        return Ok(value);
    }
    if let Some(value) = eval_builtin_event_like(function, args, env)? {
        return Ok(value);
    }
    eval_builtin_array_fallback(function, args, env)
}

fn eval_builtin_trigonometric<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let arg = |i: usize| -> Result<T, EvalError> {
        eval_expr::<T>(
            args.get(i).ok_or(EvalError::UnsupportedExpression {
                kind: "builtin arity",
            })?,
            env,
        )
    };

    Ok(match function {
        rumoca_core::BuiltinFunction::Sin => Some(arg(0)?.sin()),
        rumoca_core::BuiltinFunction::Cos => Some(arg(0)?.cos()),
        rumoca_core::BuiltinFunction::Tan => Some(arg(0)?.tan()),
        rumoca_core::BuiltinFunction::Asin => Some(arg(0)?.asin()),
        rumoca_core::BuiltinFunction::Acos => Some(arg(0)?.acos()),
        rumoca_core::BuiltinFunction::Atan => Some(arg(0)?.atan()),
        rumoca_core::BuiltinFunction::Atan2 => Some(arg(0)?.atan2(arg(1)?)),
        rumoca_core::BuiltinFunction::Sinh => Some(arg(0)?.sinh()),
        rumoca_core::BuiltinFunction::Cosh => Some(arg(0)?.cosh()),
        rumoca_core::BuiltinFunction::Tanh => Some(arg(0)?.tanh()),
        rumoca_core::BuiltinFunction::Exp => Some(arg(0)?.exp()),
        rumoca_core::BuiltinFunction::Log => Some(arg(0)?.ln()),
        rumoca_core::BuiltinFunction::Log10 => Some(arg(0)?.log10()),
        _ => None,
    })
}

fn eval_builtin_event_like<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<Option<T>, EvalError> {
    let arg = |i: usize| -> Result<T, EvalError> {
        eval_expr::<T>(
            args.get(i).ok_or(EvalError::UnsupportedExpression {
                kind: "builtin arity",
            })?,
            env,
        )
    };

    Ok(match function {
        rumoca_core::BuiltinFunction::Edge => {
            let current = arg(0)?.to_bool();
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env)?.to_bool();
            Some(if current && !previous {
                T::one()
            } else {
                T::zero()
            })
        }
        rumoca_core::BuiltinFunction::Change => {
            let current = arg(0)?;
            let previous = eval_builtin_pre(&args[..args.len().min(1)], env)?;
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
        rumoca_core::BuiltinFunction::Sample => Some(eval_builtin_sample(args, env)?),
        rumoca_core::BuiltinFunction::NoEvent | rumoca_core::BuiltinFunction::Delay => {
            Some(arg(0)?)
        }
        rumoca_core::BuiltinFunction::Smooth => Some(arg(1)?),
        rumoca_core::BuiltinFunction::Homotopy => Some(eval_builtin_homotopy(args, env)?),
        _ => None,
    })
}

fn eval_builtin_homotopy<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if !env.is_initial || args.len() < 2 {
        // MLS §3.7.4.3: during normal time steps, homotopy(actual, simplified) = actual.
        let actual = args.first().ok_or(EvalError::UnsupportedExpression {
            kind: "homotopy arity",
        })?;
        return eval_expr::<T>(actual, env);
    }
    // MLS §3.7.4.3: a translator may solve initialization equations
    // with a continuation from simplified -> actual, but ordinary
    // runtime evaluation must end at `actual`.
    let lambda = env
        .vars
        .get(INIT_HOMOTOPY_LAMBDA_KEY)
        .copied()
        .unwrap_or_else(T::one);
    let actual = eval_expr::<T>(&args[0], env)?;
    let simplified = eval_expr::<T>(&args[1], env)?;
    Ok(simplified * (T::one() - lambda) + actual * lambda)
}

fn eval_builtin_array_fallback<T: SimFloat>(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let arg = |i: usize| -> Result<T, EvalError> {
        eval_expr::<T>(
            args.get(i).ok_or(EvalError::UnsupportedExpression {
                kind: "builtin arity",
            })?,
            env,
        )
    };

    match function {
        rumoca_core::BuiltinFunction::Sum => eval_builtin_sum(args, env),
        rumoca_core::BuiltinFunction::Product => eval_builtin_product(args, env),
        rumoca_core::BuiltinFunction::Size => eval_builtin_size(args, env),
        rumoca_core::BuiltinFunction::Zeros
        | rumoca_core::BuiltinFunction::Ones
        | rumoca_core::BuiltinFunction::Fill
        | rumoca_core::BuiltinFunction::Identity
        | rumoca_core::BuiltinFunction::Diagonal => first_array_value(
            Some(eval_array_constructor_values(&function, args, env)?),
            "array constructor scalar projection",
        ),
        rumoca_core::BuiltinFunction::Scalar
        | rumoca_core::BuiltinFunction::Vector
        | rumoca_core::BuiltinFunction::Matrix => arg(0),
        rumoca_core::BuiltinFunction::Transpose => first_array_value(
            eval_transpose_values(args, env)?,
            "transpose scalar projection",
        ),
        rumoca_core::BuiltinFunction::Cross => {
            first_array_value(eval_cross_values(args, env)?, "cross")
        }
        rumoca_core::BuiltinFunction::Skew => {
            first_array_value(eval_skew_values(args, env)?, "skew")
        }
        rumoca_core::BuiltinFunction::OuterProduct => {
            first_array_value(eval_outer_product_values(args, env)?, "outer product")
        }
        rumoca_core::BuiltinFunction::Symmetric => {
            first_array_value(eval_symmetric_values(args, env)?, "symmetric")
        }
        rumoca_core::BuiltinFunction::Linspace => first_array_value(
            Some(eval_linspace_values(args, env)?),
            "linspace scalar projection",
        ),
        rumoca_core::BuiltinFunction::Cat => first_array_value(
            Some(try_eval_cat_values(args, env)?),
            "cat scalar projection",
        ),
        _ => Err(EvalError::UnsupportedExpression {
            kind: "builtin function",
        }),
    }
}

fn eval_builtin_sum<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if args.len() == 1 {
        validate_array_argument(&args[0], env)?;
        return Ok(eval_array_like_values(&args[0], env)?
            .into_iter()
            .fold(T::zero(), |acc, v| acc + v));
    }
    args.iter()
        .map(|expr| eval_expr::<T>(expr, env))
        .try_fold(T::zero(), |acc, value| value.map(|v| acc + v))
}

fn eval_builtin_product<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    if args.len() == 1 {
        validate_array_argument(&args[0], env)?;
        return Ok(eval_array_like_values(&args[0], env)?
            .into_iter()
            .fold(T::one(), |acc, v| acc * v));
    }
    args.iter()
        .map(|expr| eval_expr::<T>(expr, env))
        .try_fold(T::one(), |acc, value| value.map(|v| acc * v))
}

fn first_array_value<T: SimFloat>(
    values: Option<Vec<T>>,
    kind: &'static str,
) -> Result<T, EvalError> {
    values
        .and_then(|values| values.first().copied())
        .ok_or(EvalError::UnsupportedExpression { kind })
}

fn eval_builtin_size<T: SimFloat>(
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let arg = args
        .first()
        .ok_or(EvalError::UnsupportedExpression { kind: "size arity" })?;
    let dim_idx = match args.get(1) {
        Some(dim_expr) => {
            let raw = eval_expr::<T>(dim_expr, env)?.real();
            if !raw.is_finite() || raw < 1.0 || raw.fract() != 0.0 {
                return Err(EvalError::UnsupportedExpression {
                    kind: "size dimension",
                });
            }
            raw as usize
        }
        None => 1,
    };
    let dims = try_infer_runtime_expr_dims(arg, env)?;
    let value = dims
        .get(
            dim_idx
                .checked_sub(1)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "size dimension",
                })?,
        )
        .ok_or(EvalError::UnsupportedExpression {
            kind: "size dimension",
        })?;
    Ok(T::from_f64(*value as f64))
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
    fn eval_builtin_size_rejects_out_of_range_dimension() {
        let env = env_with_dims("a", vec![2, 3]);

        assert_eq!(
            eval_builtin_size(&[var_ref("a"), int_literal(3)], &env),
            Err(EvalError::UnsupportedExpression {
                kind: "size dimension",
            })
        );
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

        assert_eq!(value, Ok(4.0));
    }

    #[test]
    fn eval_builtin_size_preserves_singleton_array_constructor_rank() {
        let env = VarEnv::<f64>::new();
        let singleton_array = rumoca_core::Expression::Array {
            elements: vec![int_literal(7)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        };

        let value = eval_builtin_size(&[singleton_array, int_literal(1)], &env);

        assert_eq!(value, Ok(1.0));
    }
}
