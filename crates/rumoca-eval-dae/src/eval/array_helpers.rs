use super::*;
use std::fmt::Write as _;

fn indexed_key<'a>(buffer: &'a mut String, name: &str, index: usize) -> &'a str {
    buffer.clear();
    write!(buffer, "{name}[{index}]").expect("write to String never fails");
    buffer.as_str()
}

fn indexed_field_key<'a>(buffer: &'a mut String, base: &str, index: usize, field: &str) -> &'a str {
    buffer.clear();
    write!(buffer, "{base}[{index}].{field}").expect("write to String never fails");
    buffer.as_str()
}

pub(super) fn infer_dims_from_values(dims: &[i64], len: usize) -> Result<Vec<usize>, EvalError> {
    if dims.is_empty() {
        return Ok((len > 1).then_some(len).into_iter().collect());
    }

    let mut inferred: Vec<usize> = dims
        .iter()
        .map(|&dim| {
            usize::try_from(dim).map_err(|_| EvalError::UnsupportedExpression {
                kind: "array dimensions",
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let unknown_idxs = inferred
        .iter()
        .enumerate()
        .filter_map(|(idx, dim)| (*dim == 0).then_some(idx))
        .collect::<Vec<_>>();
    if len > 0 && unknown_idxs.len() == 1 {
        let unknown_idx = unknown_idxs[0];
        let known_prod = inferred
            .iter()
            .enumerate()
            .filter(|(idx, _)| *idx != unknown_idx)
            .try_fold(1usize, |acc, (_, dim)| {
                acc.checked_mul(*dim)
                    .ok_or(EvalError::UnsupportedExpression {
                        kind: "array dimensions",
                    })
            })?;
        if known_prod == 0 || !len.is_multiple_of(known_prod) {
            return Err(EvalError::ShapeMismatch {
                context: "declared array dimensions",
                expected: known_prod,
                actual: len,
            });
        }
        inferred[unknown_idx] = len / known_prod;
        return Ok(inferred);
    }
    if len > 0 && unknown_idxs.len() > 1 {
        return Err(EvalError::UnsupportedExpression {
            kind: "array dimensions",
        });
    }

    let prod = inferred.iter().try_fold(1usize, |acc, dim| {
        acc.checked_mul(*dim)
            .ok_or(EvalError::UnsupportedExpression {
                kind: "array dimensions",
            })
    })?;
    if prod != len {
        return Err(EvalError::ShapeMismatch {
            context: "declared array dimensions",
            expected: prod,
            actual: len,
        });
    }
    Ok(inferred)
}

fn collect_indexed_array_values_generic<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let mut values = Vec::new();
    let mut key = String::with_capacity(name.len() + 8);
    for i in 1.. {
        let Some(value) = env.vars.get(indexed_key(&mut key, name, i)) else {
            break;
        };
        values.push(*value);
    }
    (!values.is_empty()).then_some(values)
}

fn split_record_array_field_name(name: &str) -> Option<(String, String)> {
    let mut parts = rumoca_core::ComponentPath::from_flat_path(name).into_parts();
    if parts.len() < 2 {
        return None;
    }
    let field = parts.pop()?;
    let base = parts.join(".");
    if base.is_empty() || field.is_empty() {
        return None;
    }
    Some((base, field))
}

fn collect_record_field_indexed_values_generic<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let (base, field) = split_record_array_field_name(name)?;
    let mut values = Vec::new();
    let mut key = String::with_capacity(base.len() + field.len() + 10);
    for i in 1.. {
        let Some(value) = env.vars.get(indexed_field_key(&mut key, &base, i, &field)) else {
            break;
        };
        values.push(*value);
    }
    (!values.is_empty()).then_some(values)
}

fn collect_dense_indexed_values_generic<T: SimFloat>(
    name: &str,
    scalar_count: usize,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let keys = cached_indexed_keys(&env.runtime, name, scalar_count);
    let mut values = Vec::with_capacity(scalar_count);
    for key in keys.iter() {
        values.push(env.vars.get(key.as_str()).copied()?);
    }
    Some(values)
}

fn collect_dense_record_field_indexed_values_generic<T: SimFloat>(
    name: &str,
    scalar_count: usize,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    let (base, field) = split_record_array_field_name(name)?;
    let keys = cached_indexed_field_keys(&env.runtime, &base, &field, scalar_count);
    let mut values = Vec::with_capacity(scalar_count);
    for key in keys.iter() {
        values.push(env.vars.get(key.as_str()).copied()?);
    }
    Some(values)
}

pub(super) fn array_values_from_env_name_generic<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    if let Some(dims) = env.dims.get(name) {
        let scalar_count = dims.iter().map(|&d| d.max(0) as usize).product::<usize>();
        if scalar_count > 1 {
            if let Some(values) = collect_dense_indexed_values_generic(name, scalar_count, env) {
                return Ok(Some(values));
            }
            if let Some(values) =
                collect_dense_record_field_indexed_values_generic(name, scalar_count, env)
            {
                return Ok(Some(values));
            }
        }
        if scalar_count == 0
            && let Some(values) = collect_indexed_array_values_generic(name, env)
        {
            return Ok(Some(values));
        }
        if scalar_count == 0
            && let Some(values) = collect_record_field_indexed_values_generic(name, env)
        {
            return Ok(Some(values));
        }
    }

    if let Some(dims) = env.dims.get(name)
        && !dims.is_empty()
        && let Some(start_expr) = env.start_exprs.get(name)
        && !matches!(start_expr, Expression::VarRef { name: start_name, .. } if start_name.as_str() == name)
    {
        let values = if dims.len() >= 2 {
            match eval_matrix_values(start_expr, env) {
                Ok(Some(matrix)) => matrix.into_iter().flatten().collect(),
                Ok(None) => eval_array_values::<T>(start_expr, env)?,
                Err(err) => return Err(err),
            }
        } else {
            eval_array_values::<T>(start_expr, env)?
        };
        if values.len() > 1 {
            return Ok(Some(values));
        }
    }

    Ok(collect_indexed_array_values_generic(name, env)
        .or_else(|| collect_record_field_indexed_values_generic(name, env)))
}

pub(super) fn array_values_from_env_name<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Result<Option<Vec<f64>>, EvalError> {
    Ok(array_values_from_env_name_generic(name, env)?
        .map(|values| values.into_iter().map(|v| v.real()).collect()))
}

fn parse_encoded_slice_field_varref(raw: &str) -> Option<(&str, &str)> {
    let (base, field) = raw.split_once("[:].")?;
    if base.is_empty() || field.is_empty() {
        return None;
    }
    Some((base, field))
}

pub(super) fn encoded_slice_field_values<T: SimFloat>(
    raw: &str,
    env: &VarEnv<T>,
) -> Result<Option<Vec<T>>, EvalError> {
    let Some((base, field)) = parse_encoded_slice_field_varref(raw) else {
        return Ok(None);
    };
    let Some(base_values) = array_values_from_env_name_generic(base, env)? else {
        return Ok(None);
    };
    let mut values = Vec::with_capacity(base_values.len());
    let indexed_field_keys =
        cached_indexed_field_keys(&env.runtime, base, field, base_values.len());
    let field_indexed_keys =
        cached_field_indexed_keys(&env.runtime, base, field, base_values.len());
    for (idx, base_value) in base_values.into_iter().enumerate() {
        if let Some(value) = env.vars.get(indexed_field_keys[idx].as_str()).copied() {
            values.push(value);
            continue;
        }
        if let Some(value) = env.vars.get(field_indexed_keys[idx].as_str()).copied() {
            values.push(value);
            continue;
        }
        values.push(base_value);
    }
    Ok(Some(values))
}

pub(super) fn eval_unary_builtin_array_values<T: SimFloat>(
    function: BuiltinFunction,
    values: Vec<T>,
) -> Option<Vec<T>> {
    let mapped = match function {
        BuiltinFunction::Abs => values.into_iter().map(|v| v.abs()).collect(),
        BuiltinFunction::Sign => values.into_iter().map(|v| v.sign()).collect(),
        BuiltinFunction::Sqrt => values.into_iter().map(|v| v.sqrt()).collect(),
        BuiltinFunction::Sin => values.into_iter().map(|v| v.sin()).collect(),
        BuiltinFunction::Cos => values.into_iter().map(|v| v.cos()).collect(),
        BuiltinFunction::Tan => values.into_iter().map(|v| v.tan()).collect(),
        BuiltinFunction::Asin => values.into_iter().map(|v| v.asin()).collect(),
        BuiltinFunction::Acos => values.into_iter().map(|v| v.acos()).collect(),
        BuiltinFunction::Atan => values.into_iter().map(|v| v.atan()).collect(),
        BuiltinFunction::Sinh => values.into_iter().map(|v| v.sinh()).collect(),
        BuiltinFunction::Cosh => values.into_iter().map(|v| v.cosh()).collect(),
        BuiltinFunction::Tanh => values.into_iter().map(|v| v.tanh()).collect(),
        BuiltinFunction::Exp => values.into_iter().map(|v| v.exp()).collect(),
        BuiltinFunction::Log => values.into_iter().map(|v| v.ln()).collect(),
        BuiltinFunction::Log10 => values.into_iter().map(|v| v.log10()).collect(),
        BuiltinFunction::Floor | BuiltinFunction::Integer => {
            values.into_iter().map(|v| v.floor()).collect()
        }
        BuiltinFunction::Ceil => values.into_iter().map(|v| v.ceil()).collect(),
        BuiltinFunction::NoEvent | BuiltinFunction::Delay => values,
        _ => return None,
    };
    Some(mapped)
}

pub(super) fn try_eval_field_access_array_values<T: SimFloat>(
    base: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if let Some(name) = eval_field_access_array_path(base, field, env)?
        && let Some(values) = array_values_from_env_name_generic(name.as_str(), env)?
    {
        return Ok(values);
    }
    if let Some(name) = flattened_field_access_name(base, field)
        && let Some(values) = array_values_from_env_name_generic(name.as_str(), env)?
    {
        return Ok(values);
    }
    match base {
        Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => match try_eval_function_record_field_array_values(name, args, field, env) {
            Ok(values) => Ok(values),
            Err(EvalError::MissingFunction { .. } | EvalError::UnsupportedExpression { .. })
                if set_state_array_field_arg_index(name.var_name(), field).is_some() =>
            {
                let arg_index = set_state_array_field_arg_index(name.var_name(), field)
                    .expect("checked by guard");
                let arg = args
                    .get(arg_index)
                    .ok_or(EvalError::UnsupportedExpression {
                        kind: "setState array field arity",
                    })?;
                eval_array_like_values(arg, env)
            }
            Err(err) => Err(err),
        },
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty()
            || subscripts
                .iter()
                .all(|sub| matches!(sub, Subscript::Colon { .. })) =>
        {
            let base_name = name.as_str();
            let base_values =
                array_values_from_env_name_generic(base_name, env)?.ok_or_else(|| {
                    EvalError::MissingBinding {
                        name: base_name.to_string(),
                    }
                })?;
            let mut values = Vec::with_capacity(base_values.len());
            let indexed_field_keys =
                cached_indexed_field_keys(&env.runtime, base_name, field, base_values.len());
            let field_indexed_keys =
                cached_field_indexed_keys(&env.runtime, base_name, field, base_values.len());
            for (idx, base_value) in base_values.into_iter().enumerate() {
                if let Some(value) = env.vars.get(indexed_field_keys[idx].as_str()).copied() {
                    values.push(value);
                    continue;
                }
                if let Some(value) = env.vars.get(field_indexed_keys[idx].as_str()).copied() {
                    values.push(value);
                    continue;
                }
                values.push(base_value);
            }
            Ok(values)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            let mut values = Vec::new();
            for element in elements {
                match try_eval_field_access_array_values(element, field, env) {
                    Ok(nested) => values.extend(nested),
                    Err(EvalError::UnsupportedExpression {
                        kind: "field access array value",
                    }) => {
                        let span = expression_source_span(element).ok_or(
                            EvalError::UnsupportedExpression {
                                kind: "field access array element missing source span",
                            },
                        )?;
                        values.push(eval_expr::<T>(
                            &Expression::FieldAccess {
                                base: Box::new(element.clone()),
                                field: field.to_string(),
                                span,
                            },
                            env,
                        )?);
                    }
                    Err(err) => return Err(err),
                }
            }
            Ok(values)
        }
        _ => Err(EvalError::UnsupportedExpression {
            kind: "field access array value",
        }),
    }
}

fn eval_field_access_array_path<T: SimFloat>(
    base: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<Option<String>, EvalError> {
    let Some(prefix) = eval_array_field_base_path(base, env)? else {
        return Ok(None);
    };
    Ok(Some(format!("{prefix}.{field}")))
}

fn eval_array_field_base_path<T: SimFloat>(
    expr: &Expression,
    env: &VarEnv<T>,
) -> Result<Option<String>, EvalError> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Ok(Some(name.as_str().to_string()));
            }
            if subscripts
                .iter()
                .any(|subscript| matches!(subscript, Subscript::Colon { .. }))
            {
                return Ok(None);
            }
            let indices = eval_array_field_subscripts(subscripts, env)?;
            Ok(Some(dae::format_subscript_key(name.as_str(), &indices)))
        }
        Expression::FieldAccess { base, field, .. } => {
            let Some(prefix) = eval_array_field_base_path(base, env)? else {
                return Ok(None);
            };
            Ok(Some(format!("{prefix}.{field}")))
        }
        _ => Ok(None),
    }
}

fn eval_array_field_subscripts<T: SimFloat>(
    subscripts: &[Subscript],
    env: &VarEnv<T>,
) -> Result<Vec<usize>, EvalError> {
    subscripts
        .iter()
        .map(|subscript| eval_array_field_subscript(subscript, env))
        .collect()
}

fn eval_array_field_subscript<T: SimFloat>(
    subscript: &Subscript,
    env: &VarEnv<T>,
) -> Result<usize, EvalError> {
    match subscript {
        Subscript::Index { value, .. } => positive_array_field_index(*value as f64),
        Subscript::Expr { expr, .. } => {
            positive_array_field_index(eval_expr::<T>(expr, env)?.real())
        }
        Subscript::Colon { .. } => unreachable!("colon subscripts are filtered before indexing"),
    }
}

fn positive_array_field_index(value: f64) -> Result<usize, EvalError> {
    if !value.is_finite() || value.fract() != 0.0 || value <= 0.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "field access array subscript",
        });
    }
    Ok(value as usize)
}

fn try_eval_function_record_field_array_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let function = env
        .functions
        .get(name.as_str())
        .ok_or_else(|| EvalError::MissingFunction {
            name: name.to_string(),
        })?;
    let output = function
        .outputs
        .first()
        .ok_or(EvalError::UnsupportedExpression {
            kind: "record function output",
        })?;
    let field_param = record_constructor_field_param(function, output, field, env).ok_or(
        EvalError::UnsupportedExpression {
            kind: "record function output field",
        },
    )?;
    let total = function_param_size(field_param).ok_or(EvalError::UnsupportedExpression {
        kind: "record function output field shape",
    })?;
    let output_name = format!("{}.{}", output.name, field);
    if total == 0 {
        return eval_unknown_size_function_record_field_array_values(
            name,
            args,
            &output_name,
            &field_param.dims,
            env,
        );
    }
    let mut values = Vec::with_capacity(total);
    for flat_index in 0..total {
        let output_path = function_output_path(&output_name, &field_param.dims, flat_index).ok_or(
            EvalError::UnsupportedExpression {
                kind: "record function output field index",
            },
        )?;
        values.push(eval_user_function_output_path_pub(
            name.var_name(),
            args,
            output_path.as_str(),
            env,
        )?);
    }
    Ok(values)
}

fn eval_unknown_size_function_record_field_array_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[Expression],
    output_name: &str,
    dims: &[i64],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if !matches!(dims, [0]) {
        return Ok(Vec::new());
    }

    let mut values = Vec::new();
    for flat_index in 0.. {
        let output_path = dae::format_subscript_key(output_name, &[flat_index + 1]);
        match eval_user_function_output_path_pub(name.var_name(), args, output_path.as_str(), env) {
            Ok(value) => values.push(value),
            Err(EvalError::MissingBinding { .. }) => break,
            Err(err) => return Err(err),
        }
    }
    Ok(values)
}

pub(super) fn record_constructor_field_param<'a>(
    function: &rumoca_core::Function,
    output: &rumoca_core::FunctionParam,
    field: &str,
    env: &'a VarEnv<impl SimFloat>,
) -> Option<&'a rumoca_core::FunctionParam> {
    record_constructor_fields_for_output(function, output, env)?
        .iter()
        .find(|input| input.name == field)
}

pub(super) fn record_constructor_fields_for_output<'a>(
    function: &rumoca_core::Function,
    output: &rumoca_core::FunctionParam,
    env: &'a VarEnv<impl SimFloat>,
) -> Option<&'a [rumoca_core::FunctionParam]> {
    let type_name = output.type_name.as_str();
    let qualified = function
        .name
        .enclosing_scope()
        .map(|prefix| format!("{prefix}.{type_name}"));
    let constructor = qualified
        .as_deref()
        .and_then(|name| env.functions.get(name))
        .or_else(|| env.functions.get(type_name))
        .or_else(|| {
            env.functions
                .values()
                .find(|candidate| candidate.name.last_segment() == type_name)
        })?;
    Some(constructor.inputs.as_slice())
}

pub(super) fn function_param_size(param: &rumoca_core::FunctionParam) -> Option<usize> {
    if param.dims.is_empty() {
        return Some(1);
    }
    param.dims.iter().try_fold(1usize, |acc, dim| {
        usize::try_from(*dim)
            .ok()
            .and_then(|dim| acc.checked_mul(dim))
    })
}

pub(super) fn function_output_path(
    output_name: &str,
    dims: &[i64],
    flat_index: usize,
) -> Option<String> {
    if dims.is_empty() {
        return Some(output_name.to_string());
    }
    let subscripts = dae::flat_index_to_subscripts(dims, flat_index)?;
    Some(dae::format_subscript_key(output_name, &subscripts))
}

pub(super) fn flattened_field_access_name(base: &Expression, field: &str) -> Option<String> {
    match base {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(format!("{}.{field}", name.as_str())),
        Expression::FieldAccess {
            base,
            field: base_field,
            ..
        } => flattened_field_access_name(base, base_field)
            .map(|base_name| format!("{base_name}.{field}")),
        _ => None,
    }
}
