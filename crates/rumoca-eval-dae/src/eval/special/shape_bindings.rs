use super::*;

pub(super) fn seed_function_input_shape_bindings<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    dims: &[i64],
) -> Result<(), EvalError> {
    if param.shape_expr.len() != dims.len() {
        return Ok(());
    }
    for (subscript, dim) in param.shape_expr.iter().zip(dims.iter().copied()) {
        if dim < 0 {
            return Err(EvalError::UnsupportedExpression {
                kind: "negative function shape dimension",
            });
        }
        let Subscript::Expr { expr, .. } = subscript else {
            continue;
        };
        let Expression::VarRef {
            name, subscripts, ..
        } = expr.as_ref()
        else {
            continue;
        };
        if !subscripts.is_empty() {
            continue;
        }
        local_env.set(name.as_str(), T::from_f64(dim as f64));
    }
    Ok(())
}

pub(super) fn seed_function_input_shape_bindings_from_arg<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<(), EvalError> {
    if param.shape_expr.is_empty() {
        return Ok(());
    }
    let dims = match try_infer_runtime_expr_dims(arg_expr, caller_env) {
        Ok(dims) => dims
            .into_iter()
            .map(|dim| {
                i64::try_from(dim).map_err(|_| EvalError::UnsupportedExpression {
                    kind: "array dimensions",
                })
            })
            .collect::<Result<Vec<_>, _>>()?,
        Err(EvalError::UnsupportedExpression { .. }) | Err(EvalError::MissingBinding { .. }) => {
            return Ok(());
        }
        Err(err) => return Err(err),
    };
    seed_function_input_shape_bindings(local_env, param, &dims)
}
