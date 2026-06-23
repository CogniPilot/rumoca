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

pub(super) fn infer_dims_from_values(dims: &[i64], len: usize) -> Vec<usize> {
    if len == 0 {
        return Vec::new();
    }
    if dims.is_empty() {
        return vec![len];
    }

    let mut inferred: Vec<usize> = dims.iter().map(|&d| d.max(0) as usize).collect();
    let unknown_idxs: Vec<usize> = inferred
        .iter()
        .enumerate()
        .filter_map(|(i, d)| (*d == 0).then_some(i))
        .collect();

    if unknown_idxs.is_empty() {
        let prod = inferred.iter().copied().product::<usize>();
        if prod == len {
            return inferred;
        }
        if inferred.len() == 2 && inferred[1] > 0 && len.is_multiple_of(inferred[1]) {
            inferred[0] = len / inferred[1];
            return inferred;
        }
        return vec![len];
    }

    if unknown_idxs.len() == 1 {
        let idx = unknown_idxs[0];
        let known_prod = inferred
            .iter()
            .enumerate()
            .filter(|(i, _)| *i != idx)
            .map(|(_, d)| *d.max(&1))
            .product::<usize>()
            .max(1);
        inferred[idx] = (len / known_prod).max(1);
        return inferred;
    }

    // Multiple unknown dimensions: prefer matrix shape if available.
    if inferred.len() == 2 {
        if let Some(value) = len.checked_div(inferred[1]) {
            inferred[0] = value.max(1);
        } else if let Some(value) = len.checked_div(inferred[0]) {
            inferred[1] = value.max(1);
        } else {
            inferred[0] = len;
            inferred[1] = 1;
        }
        return inferred;
    }

    inferred[0] = len;
    for d in inferred.iter_mut().skip(1) {
        if *d == 0 {
            *d = 1;
        }
    }
    inferred
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

fn split_record_array_field_name(name: &str) -> Option<(&str, &str)> {
    let (base, field) = rumoca_core::split_last_top_level(name)?;
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
        let Some(value) = env.vars.get(indexed_field_key(&mut key, base, i, field)) else {
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
    let keys = cached_indexed_field_keys(&env.runtime, base, field, scalar_count);
    let mut values = Vec::with_capacity(scalar_count);
    for key in keys.iter() {
        values.push(env.vars.get(key.as_str()).copied()?);
    }
    Some(values)
}

pub(super) fn array_values_from_env_name_generic<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    if let Some(dims) = env.dims.get(name) {
        let scalar_count = dims.iter().map(|&d| d.max(0) as usize).product::<usize>();
        if scalar_count == 0 {
            return Some(Vec::new());
        }
        if scalar_count > 1 {
            if let Some(values) = collect_dense_indexed_values_generic(name, scalar_count, env) {
                return Some(values);
            }
            if let Some(values) =
                collect_dense_record_field_indexed_values_generic(name, scalar_count, env)
            {
                return Some(values);
            }
        }
    }

    if let Some(dims) = env.dims.get(name)
        && !dims.is_empty()
        && let Some(start_expr) = env.start_exprs.get(name)
        && !matches!(start_expr, Expression::VarRef { name: start_name, .. } if start_name.as_str() == name)
    {
        let values = if dims.len() >= 2 {
            eval_matrix_values(start_expr, env)
                .map(|matrix| matrix.into_iter().flatten().collect())
                .unwrap_or_else(|| eval_array_values::<T>(start_expr, env))
        } else {
            eval_array_values::<T>(start_expr, env)
        };
        if values.len() > 1 {
            return Some(values);
        }
    }

    collect_indexed_array_values_generic(name, env)
        .or_else(|| collect_record_field_indexed_values_generic(name, env))
}

pub(super) fn array_values_from_env_name<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Option<Vec<f64>> {
    array_values_from_env_name_generic(name, env)
        .map(|values| values.into_iter().map(|v| v.real()).collect())
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
) -> Option<Vec<T>> {
    let (base, field) = parse_encoded_slice_field_varref(raw)?;
    let base_values = array_values_from_env_name_generic(base, env)?;
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
    Some(values)
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

pub(super) fn eval_field_access_array_values<T: SimFloat>(
    base: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    try_eval_field_access_array_values(base, field, env).ok()
}

pub(super) fn try_eval_field_access_array_values<T: SimFloat>(
    base: &Expression,
    field: &str,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if let Some(name) = flattened_field_access_name(base, field)
        && let Some(values) = array_values_from_env_name_generic(name.as_str(), env)
    {
        return Ok(values);
    }
    if let Some(path) = eval_field_access_path(base, env) {
        let key = format!("{path}.{field}");
        if let Some(values) = array_values_from_env_name_generic(key.as_str(), env) {
            return Ok(values);
        }
        if let Some(alias_field) = reference_field_alias(field) {
            let alias_key = format!("{path}.{alias_field}");
            if let Some(values) = array_values_from_env_name_generic(alias_key.as_str(), env) {
                return Ok(values);
            }
        }
        trace_field_access_array_miss(base, field, Some(path.as_str()), key.as_str(), env);
    } else {
        trace_field_access_array_miss(base, field, None, "", env);
    }

    match base {
        Expression::FunctionCall {
            args,
            is_constructor: true,
            ..
        } => try_eval_constructor_field_array_values(args, field, env),
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
                array_values_from_env_name_generic(base_name, env).ok_or_else(|| {
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
                if let Some(nested) = eval_field_access_array_values(element, field, env) {
                    values.extend(nested);
                } else {
                    values.push(eval_expr_or_default::<T>(
                        &Expression::FieldAccess {
                            base: Box::new(element.clone()),
                            field: field.to_string(),
                            span: rumoca_core::Span::DUMMY,
                        },
                        env,
                    ));
                }
            }
            Ok(values)
        }
        _ => Err(EvalError::UnsupportedExpression {
            kind: "field access array value",
        }),
    }
}

fn try_eval_constructor_field_array_values<T: SimFloat>(
    args: &[Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    if let Some(expr) = named_constructor_arg(args, field).or_else(|| {
        reference_field_alias(field).and_then(|alias| named_constructor_arg(args, alias))
    }) {
        return try_eval_array_like_values(expr, env);
    }
    Err(EvalError::UnsupportedExpression {
        kind: "field access array value",
    })
}

fn named_constructor_arg<'a>(args: &'a [Expression], field: &str) -> Option<&'a Expression> {
    for arg in args {
        if let Expression::FunctionCall {
            name,
            args: named_args,
            is_constructor: true,
            ..
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return named_args.first();
        }
    }
    None
}

fn reference_field_alias(field: &str) -> Option<&str> {
    field
        .strip_prefix("reference_")
        .filter(|alias| !alias.is_empty())
}

fn trace_field_access_array_miss<T: SimFloat>(
    base: &Expression,
    field: &str,
    path: Option<&str>,
    key: &str,
    env: &VarEnv<T>,
) {
    if std::env::var_os("RUMOCA_TRACE_FIELD_ACCESS_ARRAY").is_none() {
        return;
    }
    let needle = path.unwrap_or(field);
    let dims = env
        .dims
        .keys()
        .filter(|candidate| candidate.contains(needle) || candidate.ends_with(field))
        .take(24)
        .cloned()
        .collect::<Vec<_>>();
    let vars = env
        .vars
        .keys()
        .filter(|candidate| candidate.contains(needle) || candidate.ends_with(field))
        .take(24)
        .cloned()
        .collect::<Vec<_>>();
    eprintln!(
        "field access array miss field={field} path={path:?} key={key} base={base:#?} dims={dims:?} vars={vars:?}"
    );
}

fn try_eval_function_record_field_array_values<T: SimFloat>(
    name: &rumoca_core::Reference,
    args: &[Expression],
    field: &str,
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let (named_args, _) = split_named_and_positional_call_args(args);
    trace_record_field_array_named_args(name.as_str(), field, named_args.keys().copied());
    if let Some(arg) = named_args.get(field) {
        let values = try_eval_array_like_values(arg, env)
            .map(|values| complete_set_state_mass_fractions(name.as_str(), field, values));
        trace_record_field_array_named_arg_result(name.as_str(), field, &values);
        return values;
    }

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

fn complete_set_state_mass_fractions<T: SimFloat>(
    function: &str,
    field: &str,
    values: Vec<T>,
) -> Vec<T> {
    if field != "X" || values.len() != 1 || !is_set_state_function(function) {
        return values;
    }
    let sum = values
        .iter()
        .copied()
        .fold(T::zero(), |acc, value| acc + value);
    let mut completed = values;
    completed.push(T::one() - sum);
    completed
}

fn is_set_state_function(function: &str) -> bool {
    let short = rumoca_core::top_level_last_segment(function);
    matches!(short, "setState_pTX" | "setState_phX" | "setState_psX")
}

fn trace_record_field_array_named_args<'a>(
    function: &str,
    field: &str,
    keys: impl Iterator<Item = &'a str>,
) {
    if std::env::var_os("RUMOCA_TRACE_RECORD_FIELD_ARRAY").is_none() {
        return;
    }
    eprintln!(
        "record field array lookup function={function} field={field} named_args={:?}",
        keys.collect::<Vec<_>>()
    );
}

fn trace_record_field_array_named_arg_result<T: SimFloat>(
    function: &str,
    field: &str,
    result: &Result<Vec<T>, EvalError>,
) {
    if std::env::var_os("RUMOCA_TRACE_RECORD_FIELD_ARRAY").is_none() {
        return;
    }
    match result {
        Ok(values) => eprintln!(
            "record field array named arg result function={function} field={field} len={}",
            values.len()
        ),
        Err(err) => {
            eprintln!(
                "record field array named arg error function={function} field={field} {err:?}"
            )
        }
    }
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
    let qualified = rumoca_core::split_last_top_level(function.name.as_str())
        .map(|(prefix, _)| format!("{prefix}.{type_name}"));
    let constructor = qualified
        .as_deref()
        .and_then(|name| env.functions.get(name))
        .or_else(|| env.functions.get(type_name))
        .or_else(|| {
            env.functions.values().find(|candidate| {
                rumoca_core::top_level_last_segment(candidate.name.as_str()) == type_name
            })
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
            .filter(|dim| *dim > 0)
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
    let joined = subscripts
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    Some(format!("{output_name}[{joined}]"))
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
