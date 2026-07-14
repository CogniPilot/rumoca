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

    // Negative dims are size-expression placeholders; treat them like the
    // `0` unknown-extent sentinel so a single unknown axis is still inferred.
    let mut inferred: Vec<usize> = dims.iter().map(|&dim| dim.max(0) as usize).collect();
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

fn collect_dense_shaped_values_generic<T: SimFloat>(
    name: &str,
    dims: &[i64],
    scalar_count: usize,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    if dims.len() < 2 {
        return None;
    }
    (0..scalar_count)
        .map(|flat_index| {
            let subscripts = dae::flat_index_to_subscripts(dims, flat_index)?;
            env.vars
                .get(&dae::format_subscript_key(name, &subscripts))
                .copied()
        })
        .collect()
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
        if scalar_count > 0 {
            if let Some(values) = collect_dense_shaped_values_generic(name, dims, scalar_count, env)
            {
                return Ok(Some(values));
            }
            if let Some(values) = collect_dense_indexed_values_generic(name, scalar_count, env) {
                return Ok(Some(values));
            }
            if let Some(values) =
                collect_dense_record_field_indexed_values_generic(name, scalar_count, env)
            {
                return Ok(Some(values));
            }
            if env.vars.parent_namespace_is_hidden(name) {
                let keys = cached_indexed_keys(&env.runtime, name, scalar_count);
                let missing = keys
                    .iter()
                    .find(|key| env.vars.get(key.as_str()).is_none())
                    .map(|key| key.as_str().to_string())
                    .unwrap_or_else(|| name.to_string());
                return Err(EvalError::MissingBinding { name: missing });
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
        // Statically zero-sized array (every dim known, some dim zero): the
        // empty value is exact, unlike unknown-dim placeholders (negative).
        if scalar_count == 0 && dims.iter().all(|&dim| dim >= 0) {
            return Ok(Some(Vec::new()));
        }
    }

    if let Some(dims) = env.dims.get(name)
        && !dims.is_empty()
        && let Some(start_expr) = env.visible_start_expr(name)
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

    if env.vars.local_scalar_shadows_parent_namespace(name) {
        return Ok(None);
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

fn eval_function_call_field_array_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[Expression],
    fields: &[String],
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
    let output_path = if fields.first().is_some_and(|field| field == &output.name) {
        fields.join(".")
    } else {
        format!("{}.{}", output.name, fields.join("."))
    };
    eval_user_function_output_array_path_pub(name.var_name(), args, output_path.as_str(), env)
}

pub(super) fn try_eval_field_access_array_values<T: SimFloat>(
    base: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if let Some(name) = flattened_field_access_name(base, field)
        && let Some(values) = array_values_from_env_name_generic(name.as_str(), env)?
    {
        return Ok(values);
    }

    if let Some((name, args, fields)) = function_call_field_path(base, field) {
        return eval_function_call_field_array_values(name, args, &fields, env);
    }

    match base {
        Expression::Index {
            base, subscripts, ..
        } => {
            let indices = super::try_eval_index_subscripts(subscripts, env)?;
            let Some(path) = try_eval_field_access_path(base, env)? else {
                return Err(EvalError::UnsupportedExpression {
                    kind: "field access array value",
                });
            };
            let selected = dae::format_subscript_key(path.as_str(), &indices);
            let field_path = format!("{selected}.{field}");
            array_values_from_env_name_generic(field_path.as_str(), env)?
                .ok_or(EvalError::MissingBinding { name: field_path })
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } => try_eval_function_record_field_array_values(name, args, field, env),
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

/// Return the root user-function call and the complete selected field path for
/// an expression such as `f(x).pose.position`.  Keeping the path intact avoids
/// imposing a fixed record nesting depth or array rank on the evaluator.
pub(super) fn function_call_field_path<'a>(
    base: &'a Expression,
    field: &str,
) -> Option<(&'a rumoca_core::Reference, &'a [Expression], Vec<String>)> {
    let mut fields = vec![field.to_string()];
    let mut cursor = base;
    while let Expression::FieldAccess {
        base: nested,
        field: nested_field,
        ..
    } = cursor
    {
        fields.push(nested_field.clone());
        cursor = nested;
    }
    let Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        ..
    } = cursor
    else {
        return None;
    };
    fields.reverse();
    Some((name, args.as_slice(), fields))
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
