use super::*;

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
    for i in 1.. {
        let key = format!("{name}[{i}]");
        let Some(value) = env.vars.get(&key) else {
            break;
        };
        values.push(*value);
    }
    (!values.is_empty()).then_some(values)
}

fn split_record_array_field_name(name: &str) -> Option<(&str, &str)> {
    let (base, field) = name.rsplit_once('.')?;
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
    for i in 1.. {
        let key = format!("{base}[{i}].{field}");
        let Some(value) = env.vars.get(&key) else {
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
    let mut values = Vec::with_capacity(scalar_count);
    for i in 1..=scalar_count {
        let key = format!("{name}[{i}]");
        values.push(env.vars.get(&key).copied()?);
    }
    Some(values)
}

pub(super) fn array_values_from_env_name_generic<T: SimFloat>(
    name: &str,
    env: &VarEnv<T>,
) -> Option<Vec<T>> {
    if let Some(dims) = env.dims.get(name) {
        let scalar_count = dims.iter().map(|&d| d.max(0) as usize).product::<usize>();
        if scalar_count > 1 {
            if let Some(values) = collect_dense_indexed_values_generic(name, scalar_count, env) {
                return Some(values);
            }
            if let Some(values) = collect_record_field_indexed_values_generic(name, env)
                && values.len() == scalar_count
            {
                return Some(values);
            }
        }
        if scalar_count == 0
            && let Some(values) = collect_indexed_array_values_generic(name, env)
        {
            return Some(values);
        }
        if scalar_count == 0
            && let Some(values) = collect_record_field_indexed_values_generic(name, env)
        {
            return Some(values);
        }
    }

    if let Some(start_expr) = env.start_exprs.get(name)
        && !matches!(start_expr, Expression::VarRef { name: start_name, .. } if start_name.as_str() == name)
    {
        let values = eval_array_values::<T>(start_expr, env);
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
    for (idx, base_value) in base_values.into_iter().enumerate() {
        let one_based = idx + 1;
        let indexed_field_key = format!("{base}[{one_based}].{field}");
        if let Some(value) = env.vars.get(&indexed_field_key).copied() {
            values.push(value);
            continue;
        }
        let field_indexed_key = format!("{base}.{field}[{one_based}]");
        if let Some(value) = env.vars.get(&field_indexed_key).copied() {
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
    match base {
        Expression::VarRef { name, subscripts }
            if subscripts.is_empty()
                || subscripts.iter().all(|sub| matches!(sub, Subscript::Colon)) =>
        {
            let base_name = name.as_str();
            let base_values = array_values_from_env_name_generic(base_name, env)?;
            let mut values = Vec::with_capacity(base_values.len());
            for (idx, base_value) in base_values.into_iter().enumerate() {
                let one_based = idx + 1;
                let indexed_field_key = format!("{base_name}[{one_based}].{field}");
                if let Some(value) = env.vars.get(&indexed_field_key).copied() {
                    values.push(value);
                    continue;
                }
                let field_indexed_key = format!("{base_name}.{field}[{one_based}]");
                if let Some(value) = env.vars.get(&field_indexed_key).copied() {
                    values.push(value);
                    continue;
                }
                values.push(base_value);
            }
            Some(values)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            let mut values = Vec::new();
            for element in elements {
                if let Some(nested) = eval_field_access_array_values(element, field, env) {
                    values.extend(nested);
                } else {
                    values.push(eval_expr::<T>(
                        &Expression::FieldAccess {
                            base: Box::new(element.clone()),
                            field: field.to_string(),
                        },
                        env,
                    ));
                }
            }
            Some(values)
        }
        _ => None,
    }
}
