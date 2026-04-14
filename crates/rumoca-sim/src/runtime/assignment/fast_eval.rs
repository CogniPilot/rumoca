use super::canonical_var_ref_key;
use crate::runtime::scalar_eval::{eval_scalar_bool_expr_fast, eval_scalar_expr_fast};
use rumoca_eval_dae::runtime::{VarEnv, sim_float::SimFloat};
use rumoca_ir_dae as dae;

fn dynamic_subscript_token_index(token: &str, env: &VarEnv<f64>) -> Option<i64> {
    let trimmed = token.trim();
    if trimmed.is_empty() {
        return None;
    }
    if let Ok(index) = trimmed.parse::<i64>() {
        return Some(index);
    }
    if let Ok(value) = trimmed.parse::<f64>()
        && value.is_finite()
        && value.fract() == 0.0
    {
        return Some(value as i64);
    }
    let value = env.vars.get(trimmed).copied()?;
    let rounded = value.round();
    let tol = 1.0e-9 * rounded.abs().max(1.0);
    (rounded.is_finite() && (value - rounded).abs() <= tol).then_some(rounded as i64)
}

fn resolve_dynamic_raw_varref_key(raw: &str, env: &VarEnv<f64>) -> Option<String> {
    if !raw.contains('[') {
        return None;
    }
    let mut resolved = String::with_capacity(raw.len());
    let mut chars = raw.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '[' {
            resolved.push(ch);
            continue;
        }
        let mut body = String::new();
        let mut closed = false;
        for next in chars.by_ref() {
            if next == ']' {
                closed = true;
                break;
            }
            body.push(next);
        }
        if !closed {
            return None;
        }
        let mut indices = Vec::new();
        for token in body.split(',') {
            indices.push(dynamic_subscript_token_index(token, env)?.to_string());
        }
        resolved.push('[');
        resolved.push_str(&indices.join(","));
        resolved.push(']');
    }
    Some(resolved)
}

fn dynamic_raw_varref_value_from_env(name: &dae::VarName, env: &VarEnv<f64>) -> Option<f64> {
    let raw = name.as_str();
    env.vars.get(raw).copied().or_else(|| {
        let resolved = resolve_dynamic_raw_varref_key(raw, env)?;
        env.vars.get(resolved.as_str()).copied()
    })
}

fn dynamic_raw_varref_history_value_from_runtime(
    name: &dae::VarName,
    env: &VarEnv<f64>,
) -> Option<f64> {
    let raw = name.as_str();
    rumoca_eval_dae::runtime::get_pre_value(raw).or_else(|| {
        let resolved = resolve_dynamic_raw_varref_key(raw, env)?;
        rumoca_eval_dae::runtime::get_pre_value(resolved.as_str())
            .or_else(|| env.vars.get(resolved.as_str()).copied())
    })
}

fn indexed_values_with<F>(expected_len: usize, mut get_value: F) -> Option<Vec<f64>>
where
    F: FnMut(usize) -> Option<f64>,
{
    if expected_len == 0 {
        return None;
    }
    let mut values = Vec::with_capacity(expected_len);
    for index in 1..=expected_len {
        values.push(get_value(index)?);
    }
    Some(values)
}

fn indexed_history_values_from_runtime(
    name: &dae::VarName,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    indexed_values_with(expected_len, |index| {
        let key = format!("{}[{}]", name.as_str(), index);
        rumoca_eval_dae::runtime::get_pre_value(&key)
            .or_else(|| env.vars.get(key.as_str()).copied())
    })
}

fn indexed_varref_values_from_env(
    name: &dae::VarName,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    indexed_values_with(expected_len, |index| {
        let key = format!("{}[{}]", name.as_str(), index);
        env.vars.get(key.as_str()).copied()
    })
}

fn indexed_field_values_from_env(
    name: &dae::VarName,
    field: &str,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    indexed_values_with(expected_len, |index| {
        let key = format!("{}[{}].{}", name.as_str(), index, field);
        env.vars.get(key.as_str()).copied()
    })
}

fn indexed_field_history_values_from_runtime(
    name: &dae::VarName,
    field: &str,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    indexed_values_with(expected_len, |index| {
        let key = format!("{}[{}].{}", name.as_str(), index, field);
        rumoca_eval_dae::runtime::get_pre_value(&key)
            .or_else(|| env.vars.get(key.as_str()).copied())
    })
}

fn apply_fast_subscripts_to_values(
    values: Vec<f64>,
    subscripts: &[dae::Subscript],
    env: &VarEnv<f64>,
) -> Option<Vec<f64>> {
    if subscripts.len() != 1 {
        return None;
    }
    match subscripts.first()? {
        dae::Subscript::Colon => Some(values),
        _ => {
            let index = fast_subscript_index(subscripts.first()?, env)?;
            values
                .get(index.checked_sub(1)?)
                .copied()
                .map(|value| vec![value])
        }
    }
}

fn current_array_source_values(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if name.as_str().contains('[') {
                dynamic_raw_varref_value_from_env(name, env).map(|value| vec![value])
            } else {
                None
            }
            .or_else(|| indexed_varref_values_from_env(name, env, expected_len))
        }
        dae::Expression::FieldAccess { base, field } => {
            let dae::Expression::VarRef { name, subscripts } = base.as_ref() else {
                return None;
            };
            if !subscripts.is_empty() {
                return None;
            }
            indexed_field_values_from_env(name, field, env, expected_len)
        }
        dae::Expression::Index { base, subscripts } => {
            let base_len = inferred_fast_array_len(base, env)?;
            let values = current_array_source_values(base, env, base_len)?;
            apply_fast_subscripts_to_values(values, subscripts, env)
        }
        _ => None,
    }
}

fn history_array_source_values(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if name.as_str().contains('[') {
                dynamic_raw_varref_history_value_from_runtime(name, env).map(|value| vec![value])
            } else {
                None
            }
            .or_else(|| indexed_history_values_from_runtime(name, env, expected_len))
        }
        dae::Expression::FieldAccess { base, field } => {
            let dae::Expression::VarRef { name, subscripts } = base.as_ref() else {
                return None;
            };
            if !subscripts.is_empty() {
                return None;
            }
            indexed_field_history_values_from_runtime(name, field, env, expected_len)
        }
        dae::Expression::Index { base, subscripts } => {
            let base_len = inferred_fast_array_len(base, env)?;
            let values = history_array_source_values(base, env, base_len)?;
            apply_fast_subscripts_to_values(values, subscripts, env)
        }
        _ => None,
    }
}

fn active_if_branch_fast<'a>(
    expr: &'a dae::Expression,
    env: &VarEnv<f64>,
) -> Option<&'a dae::Expression> {
    let dae::Expression::If {
        branches,
        else_branch,
    } = expr
    else {
        return None;
    };
    for (condition, value) in branches {
        if eval_scalar_bool_expr_fast(condition, env)? {
            return Some(value);
        }
    }
    Some(else_branch)
}

fn env_array_len_from_dims(key: &str, env: &VarEnv<f64>) -> Option<usize> {
    let dims = env.dims.get(key)?;
    if dims.is_empty() {
        return None;
    }
    let mut total = 1usize;
    for dim in dims {
        if *dim <= 0 {
            return None;
        }
        total = total.checked_mul(usize::try_from(*dim).ok()?)?;
    }
    Some(total)
}

fn env_array_len_from_entries(key: &str, env: &VarEnv<f64>) -> Option<usize> {
    if let Some((base, field)) = key.rsplit_once('.') {
        let prefix = format!("{base}[");
        let suffix = format!("].{field}");
        return env
            .vars
            .keys()
            .filter_map(|env_key| {
                let rest = env_key.strip_prefix(prefix.as_str())?;
                let index = rest.strip_suffix(suffix.as_str())?;
                (!index.contains(','))
                    .then(|| index.parse::<usize>().ok())
                    .flatten()
            })
            .max();
    }

    let prefix = format!("{key}[");
    env.vars
        .keys()
        .filter_map(|env_key| {
            let rest = env_key.strip_prefix(prefix.as_str())?;
            let index = rest.strip_suffix(']')?;
            (!index.contains(','))
                .then(|| index.parse::<usize>().ok())
                .flatten()
        })
        .max()
}

fn env_array_len(key: &str, env: &VarEnv<f64>) -> Option<usize> {
    env_array_len_from_dims(key, env).or_else(|| env_array_len_from_entries(key, env))
}

fn eval_range_values_fast(
    start: &dae::Expression,
    step: Option<&dae::Expression>,
    end: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<Vec<f64>> {
    let start_v = eval_assignment_scalar_fast(start, env)?;
    let end_v = eval_assignment_scalar_fast(end, env)?;
    let step_v = if let Some(step_expr) = step {
        eval_assignment_scalar_fast(step_expr, env)?
    } else if end_v >= start_v {
        1.0
    } else {
        -1.0
    };

    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return None;
    }

    let tol = step_v.abs() * 1e-9 + 1e-12;
    let mut values = Vec::new();
    let mut value = start_v;
    for _ in 0..100_000 {
        let past_end =
            (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
        if past_end {
            break;
        }
        values.push(value);
        value += step_v;
    }
    Some(values)
}

fn inferred_fast_array_len(expr: &dae::Expression, env: &VarEnv<f64>) -> Option<usize> {
    if let Some(branch_expr) = active_if_branch_fast(expr, env) {
        return inferred_fast_array_len(branch_expr, env);
    }
    match expr {
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            Some(elements.len())
        }
        dae::Expression::Range { start, step, end } => {
            Some(eval_range_values_fast(start, step.as_deref(), end, env)?.len())
        }
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            env_array_len(name.as_str(), env)
        }
        dae::Expression::FieldAccess { base, field } => {
            let dae::Expression::VarRef { name, subscripts } = base.as_ref() else {
                return None;
            };
            if !subscripts.is_empty() {
                return None;
            }
            env_array_len(format!("{}.{}", name.as_str(), field).as_str(), env)
        }
        dae::Expression::BuiltinCall { function, args } => match function {
            dae::BuiltinFunction::Pre if args.len() == 1 => inferred_fast_array_len(&args[0], env),
            dae::BuiltinFunction::Scalar
            | dae::BuiltinFunction::Vector
            | dae::BuiltinFunction::Matrix
            | dae::BuiltinFunction::NoEvent
            | dae::BuiltinFunction::Homotopy
                if !args.is_empty() =>
            {
                inferred_fast_array_len(&args[0], env)
            }
            dae::BuiltinFunction::Smooth if args.len() >= 2 => {
                inferred_fast_array_len(&args[1], env)
            }
            dae::BuiltinFunction::Cat if args.len() >= 2 => {
                args.iter().skip(1).try_fold(0usize, |acc, arg| {
                    acc.checked_add(inferred_fast_array_len(arg, env)?)
                })
            }
            _ => None,
        },
        dae::Expression::FunctionCall { name, args, .. }
            if args.len() == 1
                && name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .is_some_and(|short| short == "previous") =>
        {
            inferred_fast_array_len(&args[0], env)
        }
        _ => None,
    }
}

fn eval_cat_values_fast(args: &[dae::Expression], env: &VarEnv<f64>) -> Option<Vec<f64>> {
    let mut values = Vec::new();
    for arg in args.iter().skip(1) {
        let arg_len = inferred_fast_array_len(arg, env).unwrap_or(1);
        values.extend(eval_assignment_raw_values(arg, env, arg_len));
    }
    Some(values)
}

fn fast_subscript_index(subscript: &dae::Subscript, env: &VarEnv<f64>) -> Option<usize> {
    let raw = match subscript {
        dae::Subscript::Index(index) => *index as f64,
        dae::Subscript::Expr(expr) => eval_assignment_scalar_fast(expr, env)?,
        dae::Subscript::Colon => return None,
    };
    let rounded = raw.round();
    let tol = 1.0e-9 * rounded.abs().max(1.0);
    (rounded.is_finite() && rounded > 0.0 && (raw - rounded).abs() <= tol)
        .then_some(rounded as usize)
}

fn eval_index_values_fast(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    if subscripts.len() != 1 {
        return None;
    }

    let base_len = match subscripts.first()? {
        dae::Subscript::Colon => inferred_fast_array_len(base, env).or(Some(expected_len))?,
        _ => inferred_fast_array_len(base, env)?,
    };
    let base_values = eval_assignment_raw_values(base, env, base_len);
    apply_fast_subscripts_to_values(base_values, subscripts, env)
}

fn eval_array_comprehension_fast(
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: Option<&dae::Expression>,
    env: &VarEnv<f64>,
) -> Option<Vec<f64>> {
    let [index] = indices else {
        return None;
    };
    let range_len = inferred_fast_array_len(&index.range, env)?;
    let range_values = eval_assignment_raw_values(&index.range, env, range_len);
    let mut local_env = env.clone();
    let mut values = Vec::new();
    for value in range_values {
        local_env.set(index.name.as_str(), value);
        if let Some(filter_expr) = filter
            && !eval_scalar_bool_expr_fast(filter_expr, &local_env)?
        {
            continue;
        }
        values.push(eval_assignment_scalar_fast(expr, &local_env)?);
    }
    Some(values)
}

fn eval_binary_array_values(
    op: &rumoca_ir_core::OpBinary,
    lhs: &[f64],
    rhs: &[f64],
) -> Option<Vec<f64>> {
    if lhs.len() != rhs.len() {
        return None;
    }
    lhs.iter()
        .zip(rhs.iter())
        .map(|(l, r)| {
            Some(match op {
                rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => l + r,
                rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => l - r,
                rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => l * r,
                rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => l / r,
                _ => return None,
            })
        })
        .collect()
}

fn eval_scalar_reduction_builtin_fast(
    function: &dae::BuiltinFunction,
    args: &[dae::Expression],
    env: &VarEnv<f64>,
) -> Option<f64> {
    let [arg] = args else {
        return None;
    };
    let arg_len = inferred_fast_array_len(arg, env)?;
    let values = eval_assignment_raw_values(arg, env, arg_len);
    match function {
        dae::BuiltinFunction::Sum => Some(values.into_iter().sum()),
        dae::BuiltinFunction::Product => Some(values.into_iter().product()),
        _ => None,
    }
}

fn eval_boolean_vector_function_fast(
    name: &dae::VarName,
    args: &[dae::Expression],
    env: &VarEnv<f64>,
) -> Option<f64> {
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    let [arg, ..] = args else {
        return None;
    };
    let arg_len = inferred_fast_array_len(arg, env)?;
    let values = eval_assignment_raw_values(arg, env, arg_len);
    match short {
        "anyTrue" => Some(<f64 as SimFloat>::from_bool(
            values.iter().any(|value| *value != 0.0),
        )),
        "andTrue" => Some(<f64 as SimFloat>::from_bool(
            !values.is_empty() && values.iter().all(|value| *value != 0.0),
        )),
        "oneTrue" => Some(<f64 as SimFloat>::from_bool(
            values.iter().filter(|value| **value != 0.0).count() == 1,
        )),
        "firstTrueIndex" => Some(
            values
                .iter()
                .position(|value| *value != 0.0)
                .map(|index| (index + 1) as f64)
                .unwrap_or(0.0),
        ),
        _ => None,
    }
}

fn eval_scalar_binary_reduction_fast(
    op: &rumoca_ir_core::OpBinary,
    lhs: &dae::Expression,
    rhs: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<f64> {
    if !matches!(op, rumoca_ir_core::OpBinary::Mul(_)) {
        return None;
    }
    let lhs_len = inferred_fast_array_len(lhs, env)?;
    let rhs_len = inferred_fast_array_len(rhs, env)?;
    if lhs_len == 0 || lhs_len != rhs_len {
        return None;
    }

    let lhs_values = eval_assignment_raw_values(lhs, env, lhs_len);
    let rhs_values = eval_assignment_raw_values(rhs, env, rhs_len);
    (lhs_values.len() == rhs_len).then(|| {
        lhs_values
            .into_iter()
            .zip(rhs_values)
            .map(|(l, r)| l * r)
            .sum()
    })
}

fn eval_assignment_scalar_reduction_fast(expr: &dae::Expression, env: &VarEnv<f64>) -> Option<f64> {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            eval_scalar_reduction_builtin_fast(function, args, env)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            eval_boolean_vector_function_fast(name, args, env)
        }
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Not(_),
            rhs,
        } => eval_assignment_scalar_reduction_fast(rhs, env)
            .map(|value| <f64 as SimFloat>::from_bool(!value.to_bool())),
        dae::Expression::Binary { op, lhs, rhs } => {
            eval_scalar_binary_reduction_fast(op, lhs, rhs, env)
        }
        _ => None,
    }
}

fn eval_binary_array_fast(
    op: &rumoca_ir_core::OpBinary,
    lhs: &dae::Expression,
    rhs: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    let lhs_values = eval_array_expr_fast(lhs, env, expected_len)
        .or_else(|| eval_assignment_scalar_fast(lhs, env).map(|value| vec![value; expected_len]));
    let rhs_values = eval_array_expr_fast(rhs, env, expected_len)
        .or_else(|| eval_assignment_scalar_fast(rhs, env).map(|value| vec![value; expected_len]));
    eval_binary_array_values(op, &lhs_values?, &rhs_values?)
}

fn eval_unary_array_fast(
    op: &rumoca_ir_core::OpUnary,
    rhs: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    let rhs_values = eval_array_expr_fast(rhs, env, expected_len)?;
    rhs_values
        .into_iter()
        .map(|value| {
            Some(match op {
                rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => -value,
                rumoca_ir_core::OpUnary::Plus(_)
                | rumoca_ir_core::OpUnary::DotPlus(_)
                | rumoca_ir_core::OpUnary::Empty => value,
                _ => return None,
            })
        })
        .collect()
}

fn eval_unary_builtin_array_values_fast(
    function: &dae::BuiltinFunction,
    values: Vec<f64>,
) -> Option<Vec<f64>> {
    let mapped = match function {
        dae::BuiltinFunction::Abs => values.into_iter().map(|v| v.abs()).collect(),
        dae::BuiltinFunction::Sign => values.into_iter().map(|v| v.signum()).collect(),
        dae::BuiltinFunction::Sqrt => values.into_iter().map(|v| v.sqrt()).collect(),
        dae::BuiltinFunction::Sin => values.into_iter().map(|v| v.sin()).collect(),
        dae::BuiltinFunction::Cos => values.into_iter().map(|v| v.cos()).collect(),
        dae::BuiltinFunction::Tan => values.into_iter().map(|v| v.tan()).collect(),
        dae::BuiltinFunction::Asin => values.into_iter().map(|v| v.asin()).collect(),
        dae::BuiltinFunction::Acos => values.into_iter().map(|v| v.acos()).collect(),
        dae::BuiltinFunction::Atan => values.into_iter().map(|v| v.atan()).collect(),
        dae::BuiltinFunction::Sinh => values.into_iter().map(|v| v.sinh()).collect(),
        dae::BuiltinFunction::Cosh => values.into_iter().map(|v| v.cosh()).collect(),
        dae::BuiltinFunction::Tanh => values.into_iter().map(|v| v.tanh()).collect(),
        dae::BuiltinFunction::Exp => values.into_iter().map(|v| v.exp()).collect(),
        dae::BuiltinFunction::Log => values.into_iter().map(|v| v.ln()).collect(),
        dae::BuiltinFunction::Log10 => values.into_iter().map(|v| v.log10()).collect(),
        dae::BuiltinFunction::Floor | dae::BuiltinFunction::Integer => {
            values.into_iter().map(|v| v.floor()).collect()
        }
        dae::BuiltinFunction::Ceil => values.into_iter().map(|v| v.ceil()).collect(),
        dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Delay => values,
        _ => return None,
    };
    Some(mapped)
}

fn eval_linspace_values_fast(args: &[dae::Expression], env: &VarEnv<f64>) -> Option<Vec<f64>> {
    if args.len() != 3 {
        return None;
    }

    let start = eval_assignment_scalar_fast(&args[0], env)?;
    let end = eval_assignment_scalar_fast(&args[1], env)?;
    let n_raw = eval_assignment_scalar_fast(&args[2], env)?;
    let n = n_raw.round() as i64;
    if !n_raw.is_finite() || n < 2 {
        return None;
    }
    let n_usize = usize::try_from(n).ok()?;
    let step = (end - start) / ((n_usize - 1) as f64);
    let mut values = (0..n_usize)
        .map(|i| start + step * i as f64)
        .collect::<Vec<_>>();
    if let Some(last) = values.last_mut() {
        *last = end;
    }
    Some(values)
}

fn eval_builtin_array_fast(
    function: &dae::BuiltinFunction,
    args: &[dae::Expression],
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    match function {
        dae::BuiltinFunction::Pre if args.len() == 1 => {
            history_array_source_values(&args[0], env, expected_len)
        }
        dae::BuiltinFunction::Zeros => Some(vec![0.0; expected_len]),
        dae::BuiltinFunction::Ones => Some(vec![1.0; expected_len]),
        dae::BuiltinFunction::Fill if !args.is_empty() => {
            Some(vec![eval_scalar_expr_fast(&args[0], env)?; expected_len])
        }
        dae::BuiltinFunction::Cat if args.len() >= 2 => eval_cat_values_fast(args, env),
        dae::BuiltinFunction::Linspace => eval_linspace_values_fast(args, env),
        dae::BuiltinFunction::Scalar
        | dae::BuiltinFunction::Vector
        | dae::BuiltinFunction::Matrix
            if args.len() == 1 =>
        {
            eval_array_expr_fast(&args[0], env, expected_len)
        }
        dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Homotopy if !args.is_empty() => {
            eval_array_expr_fast(&args[0], env, expected_len)
        }
        dae::BuiltinFunction::Smooth if args.len() >= 2 => {
            eval_array_expr_fast(&args[1], env, expected_len)
        }
        _ if args.len() == 1 => {
            let arg_len = inferred_fast_array_len(&args[0], env).unwrap_or(expected_len);
            let values = eval_array_expr_fast(&args[0], env, arg_len)?;
            eval_unary_builtin_array_values_fast(function, values)
        }
        _ => None,
    }
}

fn eval_array_expr_fast(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Option<Vec<f64>> {
    if let Some(branch_expr) = active_if_branch_fast(expr, env) {
        return eval_array_expr_fast(branch_expr, env, expected_len);
    }

    if let Some(values) = current_array_source_values(expr, env, expected_len) {
        return Some(values);
    }

    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            eval_builtin_array_fast(function, args, env, expected_len)
        }
        dae::Expression::Unary { op, rhs } => eval_unary_array_fast(op, rhs, env, expected_len),
        dae::Expression::Binary { op, lhs, rhs } => {
            eval_binary_array_fast(op, lhs, rhs, env, expected_len)
        }
        dae::Expression::FunctionCall { name, args, .. }
            if args.len() == 1
                && name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .is_some_and(|short| short == "previous") =>
        {
            history_array_source_values(&args[0], env, expected_len)
        }
        dae::Expression::Array { elements, .. } => elements
            .iter()
            .map(|element| eval_assignment_scalar_fast(element, env))
            .collect(),
        dae::Expression::Tuple { elements } => elements
            .iter()
            .map(|element| eval_assignment_scalar_fast(element, env))
            .collect(),
        dae::Expression::Range { start, step, end } => {
            eval_range_values_fast(start, step.as_deref(), end, env)
        }
        dae::Expression::Index { base, subscripts } => {
            eval_index_values_fast(base, subscripts, env, expected_len)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => eval_array_comprehension_fast(expr, indices, filter.as_deref(), env),
        _ => None,
    }
}

fn expr_contains_runtime_table_value_helper(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "getTimeTableValueNoDer"
                    | "getTimeTableValueNoDer2"
                    | "getTimeTableValue"
                    | "getTable1DValueNoDer"
                    | "getTable1DValueNoDer2"
                    | "getTable1DValue"
                    | "getTimeTableTmax"
                    | "getTimeTableTmin"
                    | "getTable1DAbscissaUmax"
                    | "getTable1DAbscissaUmin"
            ) || args.iter().any(expr_contains_runtime_table_value_helper)
        }
        dae::Expression::BuiltinCall { args, .. } => {
            args.iter().any(expr_contains_runtime_table_value_helper)
        }
        dae::Expression::Unary { rhs, .. } | dae::Expression::FieldAccess { base: rhs, .. } => {
            expr_contains_runtime_table_value_helper(rhs)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_runtime_table_value_helper(lhs)
                || expr_contains_runtime_table_value_helper(rhs)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_runtime_table_value_helper(condition)
                    || expr_contains_runtime_table_value_helper(value)
            }) || expr_contains_runtime_table_value_helper(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(expr_contains_runtime_table_value_helper),
        dae::Expression::Range { start, step, end } => {
            expr_contains_runtime_table_value_helper(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_runtime_table_value_helper)
                || expr_contains_runtime_table_value_helper(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_runtime_table_value_helper(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_runtime_table_value_helper(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_runtime_table_value_helper)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_runtime_table_value_helper(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_runtime_table_value_helper(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn static_subscript_parts(subscripts: &[dae::Subscript]) -> Option<Vec<String>> {
    let mut parts = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let idx = match subscript {
            dae::Subscript::Index(index) => *index,
            dae::Subscript::Expr(expr) => match expr.as_ref() {
                dae::Expression::Literal(dae::Literal::Integer(index)) => *index,
                dae::Expression::Literal(dae::Literal::Real(value))
                    if value.is_finite() && value.fract() == 0.0 =>
                {
                    *value as i64
                }
                _ => return None,
            },
            dae::Subscript::Colon => return None,
        };
        parts.push(idx.to_string());
    }
    Some(parts)
}

fn raw_name_has_only_static_indices(name: &str) -> bool {
    let mut rest = name;
    while let Some(start) = rest.find('[') {
        let after_start = &rest[start + 1..];
        let Some(end) = after_start.find(']') else {
            return false;
        };
        let body = &after_start[..end];
        if body.split(',').any(|part| {
            let trimmed = part.trim();
            trimmed.is_empty() || trimmed.parse::<i64>().is_err()
        }) {
            return false;
        }
        rest = &after_start[end + 1..];
    }
    true
}

fn scalar_access_env_key(expr: &dae::Expression) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            if subscripts.is_empty()
                && name.as_str().contains('[')
                && !raw_name_has_only_static_indices(name.as_str())
            {
                return None;
            }
            canonical_var_ref_key(name, subscripts)
        }
        dae::Expression::FieldAccess { base, field } => {
            Some(format!("{}.{}", scalar_access_env_key(base)?, field))
        }
        dae::Expression::Index { base, subscripts } => {
            let parts = static_subscript_parts(subscripts)?;
            Some(format!(
                "{}[{}]",
                scalar_access_env_key(base)?,
                parts.join(",")
            ))
        }
        _ => None,
    }
}

fn expr_contains_unresolved_scalar_access(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::VarRef { .. } => scalar_access_env_key(expr).is_none(),
        dae::Expression::FieldAccess { base, .. } => {
            scalar_access_env_key(expr).is_none() || expr_contains_unresolved_scalar_access(base)
        }
        dae::Expression::Index { base, subscripts } => {
            scalar_access_env_key(expr).is_none()
                || expr_contains_unresolved_scalar_access(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_unresolved_scalar_access(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_unresolved_scalar_access(rhs),
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_unresolved_scalar_access(lhs)
                || expr_contains_unresolved_scalar_access(rhs)
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_unresolved_scalar_access)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_unresolved_scalar_access(condition)
                    || expr_contains_unresolved_scalar_access(value)
            }) || expr_contains_unresolved_scalar_access(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_unresolved_scalar_access)
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_unresolved_scalar_access(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_unresolved_scalar_access)
                || expr_contains_unresolved_scalar_access(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_unresolved_scalar_access(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_unresolved_scalar_access(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_unresolved_scalar_access)
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

pub(crate) fn eval_assignment_scalar_fast(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
) -> Option<f64> {
    if let Some(branch_expr) = active_if_branch_fast(expr, env) {
        return eval_assignment_scalar_fast(branch_expr, env);
    }
    if let Some(value) = eval_assignment_scalar_reduction_fast(expr, env) {
        return Some(value);
    }
    if let Some(values) = current_array_source_values(expr, env, 1) {
        return values.first().copied();
    }
    if let Some(values) = eval_array_expr_fast(expr, env, 1)
        && values.len() == 1
    {
        return values.first().copied();
    }
    match expr {
        dae::Expression::VarRef { subscripts, .. } if !subscripts.is_empty() => {
            return Some(rumoca_eval_dae::runtime::eval_expr::<f64>(expr, env));
        }
        dae::Expression::BuiltinCall { function, args } => {
            if let Some(values) = eval_builtin_array_fast(function, args, env, 1) {
                return values.first().copied();
            }
        }
        dae::Expression::FunctionCall { name, args, .. }
            if args.len() == 1
                && name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .is_some_and(|short| short == "previous") =>
        {
            if let Some(values) = history_array_source_values(&args[0], env, 1) {
                return values.first().copied();
            }
        }
        dae::Expression::Array { elements, .. } if elements.len() == 1 => {
            return eval_assignment_scalar_fast(&elements[0], env);
        }
        dae::Expression::Tuple { elements } if elements.len() == 1 => {
            return eval_assignment_scalar_fast(&elements[0], env);
        }
        _ => {}
    }
    if matches!(
        expr,
        dae::Expression::Index { .. } | dae::Expression::FieldAccess { .. }
    ) {
        return Some(rumoca_eval_dae::runtime::eval_expr::<f64>(expr, env));
    }
    let fast = eval_scalar_expr_fast(expr, env);
    if fast.is_some() {
        return fast;
    }
    if expr_contains_runtime_table_value_helper(expr)
        || expr_contains_unresolved_scalar_access(expr)
    {
        return Some(rumoca_eval_dae::runtime::eval_expr::<f64>(expr, env));
    }
    None
}

fn eval_array_or_scalar_assignment(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    if expected_len == 1
        && let Some(value) = eval_assignment_scalar_fast(expr, env)
    {
        return vec![value];
    }
    eval_assignment_scalar_fast(expr, env).map_or_else(Vec::new, |value| vec![value])
}

fn eval_assignment_raw_values(
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    if expected_len == 1
        && let Some(value) = eval_assignment_scalar_reduction_fast(expr, env)
    {
        return vec![value];
    }
    if let Some(values) = eval_array_expr_fast(expr, env, expected_len) {
        return values;
    }
    if let Some(branch_expr) = active_if_branch_fast(expr, env) {
        return eval_assignment_raw_values(branch_expr, env, expected_len);
    }
    if let Some(values) = current_array_source_values(expr, env, expected_len) {
        return values;
    }
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if let Some(values) = eval_builtin_array_fast(function, args, env, expected_len) {
                return values;
            }
            eval_array_or_scalar_assignment(expr, env, expected_len)
        }
        dae::Expression::FunctionCall { name, args, .. }
            if args.len() == 1
                && name
                    .as_str()
                    .rsplit('.')
                    .next()
                    .is_some_and(|short| short == "previous") =>
        {
            if let Some(values) = history_array_source_values(&args[0], env, expected_len) {
                return values;
            }
            eval_array_or_scalar_assignment(expr, env, expected_len)
        }
        _ => eval_array_or_scalar_assignment(expr, env, expected_len),
    }
}

fn expand_values_to_size(raw: Vec<f64>, size: usize) -> Vec<f64> {
    if size == 0 {
        return Vec::new();
    }
    if raw.len() == size {
        return raw;
    }
    if raw.is_empty() {
        return vec![0.0; size];
    }
    if raw.len() == 1 {
        return vec![raw[0]; size];
    }
    let last = *raw.last().unwrap_or(&0.0);
    let mut out = Vec::with_capacity(size);
    for index in 0..size {
        out.push(raw.get(index).copied().unwrap_or(last));
    }
    out
}

pub fn evaluate_direct_assignment_values(
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
) -> Vec<f64> {
    let raw = eval_assignment_raw_values(solution, env, expected_len);
    expand_values_to_size(raw, expected_len)
}
