use super::builtin_table::{
    eval_external_table_function, resolve_function_closure, resolve_user_function,
};
use super::distribution_clock::{
    eval_clock_special_function, eval_distribution_function, eval_qualified_distribution_function,
    eval_qualified_math_special_function, is_qualified_distribution_function_name,
    is_qualified_math_special_function_name,
};
use super::*;

mod runtime_specials;
mod state_accessors;
pub(super) use runtime_specials::*;
pub use runtime_specials::{
    deterministic_automatic_global_seed, is_runtime_special_function_name,
    is_runtime_special_function_short_name, modelica_strings_hash_string,
};
pub(super) use state_accessors::*;

#[derive(Clone)]
struct OutputSelection {
    output_name: String,
    indices: Vec<i64>,
}

fn parse_selection_suffix(suffix: &str) -> Option<OutputSelection> {
    if suffix.is_empty() {
        return None;
    }
    let (output_name, indices) = match rumoca_core::parse_scalar_name(suffix) {
        Some(scalar) => {
            if rumoca_core::split_path_with_indices(scalar.base).len() != 1 {
                return None;
            }
            (scalar.base.to_string(), scalar.indices)
        }
        None if suffix.contains('[') || suffix.contains(']') => return None,
        None => (suffix.to_string(), Vec::new()),
    };
    Some(OutputSelection {
        output_name,
        indices,
    })
}

fn resolve_user_function_target<T: SimFloat>(
    requested_name: &str,
    env: &VarEnv<T>,
) -> Option<(VarName, Option<OutputSelection>)> {
    if resolve_user_function(requested_name, env).is_some() {
        return Some((VarName::new(requested_name), None));
    }

    rumoca_core::find_map_top_level_splits_rev(requested_name, |base_name, suffix| {
        let selection = parse_selection_suffix(suffix)?;
        if resolve_user_function(base_name, env).is_some() {
            Some((VarName::new(base_name), Some(selection)))
        } else {
            None
        }
    })
}

/// Resolve a function call target and return its output names in declaration order.
///
/// This is used by algorithm statement evaluation for multi-output assignments.
pub fn resolve_function_call_outputs_pub<T: SimFloat>(
    name: &VarName,
    env: &VarEnv<T>,
) -> Option<(VarName, Vec<String>)> {
    if let Some((resolved_name, selection)) = resolve_runtime_special_target(name.as_str()) {
        if selection.is_some() {
            return None;
        }
        let output_names = runtime_special_output_names(resolved_name.as_str())?
            .iter()
            .map(|name| (*name).to_string())
            .collect();
        return Some((resolved_name, output_names));
    }

    let (resolved_name, selection) = resolve_user_function_target(name.as_str(), env)?;
    if selection.is_some() {
        return None;
    }
    let func = resolve_user_function(resolved_name.as_str(), env)?;
    let output_names = func
        .outputs
        .iter()
        .map(|param| param.name.clone())
        .collect();
    Some((resolved_name, output_names))
}

/// DAE-IR wrapper for `resolve_function_call_outputs_pub`.
pub fn resolve_function_call_outputs_pub_dae<T: SimFloat>(
    name: &rumoca_core::Reference,
    env: &VarEnv<T>,
) -> Option<(rumoca_core::VarName, Vec<String>)> {
    let flat_name = VarName::new(name.as_str());
    let (resolved, outputs) = resolve_function_call_outputs_pub(&flat_name, env)?;
    Some((rumoca_core::VarName::new(resolved.as_str()), outputs))
}

fn selected_function_output_name(
    resolved_name: &VarName,
    output_name: &str,
    suffix: &str,
) -> VarName {
    if suffix.is_empty() {
        return VarName::new(format!("{}.{}", resolved_name.as_str(), output_name));
    }
    VarName::new(format!(
        "{}.{}{}",
        resolved_name.as_str(),
        output_name,
        suffix
    ))
}

struct RuntimeRecursionDepthGuard {
    runtime: Arc<EvalRuntimeState>,
}

impl Drop for RuntimeRecursionDepthGuard {
    fn drop(&mut self) {
        self.runtime
            .function_recursion_depth
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |depth| {
                Some(depth.saturating_sub(1))
            })
            .ok();
    }
}

fn try_enter_function_recursion_for(
    runtime: &Arc<EvalRuntimeState>,
) -> Option<RuntimeRecursionDepthGuard> {
    let depth = runtime
        .function_recursion_depth
        .fetch_add(1, Ordering::Relaxed);
    if depth >= MAX_FUNC_RECURSION {
        runtime
            .function_recursion_depth
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |depth| {
                Some(depth.saturating_sub(1))
            })
            .ok();
        return None;
    }
    Some(RuntimeRecursionDepthGuard {
        runtime: runtime.clone(),
    })
}

fn build_local_function_env<T: SimFloat>(env: &VarEnv<T>) -> VarEnv<T> {
    let mut local_env = VarEnv::<T>::new();
    local_env.vars = crate::eval::VarScope::child_of(&env.vars);
    local_env.runtime = env.runtime.clone();
    local_env.functions = env.functions.clone();
    local_env.dims = env.dims.clone();
    local_env.start_exprs = env.start_exprs.clone();
    local_env.clock_intervals = env.clock_intervals.clone();
    local_env.enum_literal_ordinals = env.enum_literal_ordinals.clone();
    local_env.is_initial = env.is_initial;
    local_env.function_closures = env.function_closures.clone();
    local_env
}

fn seed_static_function_scope_dims<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    inputs: &[FunctionParam],
    outputs: &[FunctionParam],
    locals: &[FunctionParam],
) {
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    for param in inputs.iter().chain(outputs.iter()).chain(locals.iter()) {
        if param.dims.is_empty() || param.dims.iter().any(|dim| *dim <= 0) {
            continue;
        }
        dims.insert(param.name.clone(), param.dims.clone());
    }
}

fn seed_resolved_function_scope_dims<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    params: &[FunctionParam],
) -> Result<(), EvalError> {
    for param in params {
        let Some(dims) = resolved_function_param_dims(param, local_env)? else {
            continue;
        };
        std::sync::Arc::make_mut(&mut local_env.dims).insert(param.name.clone(), dims);
    }
    Ok(())
}

fn resolved_function_param_dims<T: SimFloat>(
    param: &FunctionParam,
    env: &VarEnv<T>,
) -> Result<Option<Vec<i64>>, EvalError> {
    if !param.shape_expr.is_empty() {
        return eval_function_shape_expr(&param.shape_expr, env).map(Some);
    }
    if param.dims.is_empty() || param.dims.iter().any(|dim| *dim <= 0) {
        return Ok(None);
    }
    Ok(Some(param.dims.clone()))
}

fn eval_function_shape_expr<T: SimFloat>(
    shape_expr: &[Subscript],
    env: &VarEnv<T>,
) -> Result<Vec<i64>, EvalError> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for subscript in shape_expr {
        let dim = match subscript {
            Subscript::Index { value, .. } => *value,
            Subscript::Expr { expr, .. } => eval_shape_expr_dim(expr, env)?,
            Subscript::Colon { .. } => {
                return Err(EvalError::UnsupportedExpression {
                    kind: "dynamic function shape colon",
                });
            }
        };
        if dim < 0 {
            return Err(EvalError::UnsupportedExpression {
                kind: "negative function shape dimension",
            });
        }
        dims.push(dim);
    }
    Ok(dims)
}

fn eval_shape_expr_dim<T: SimFloat>(expr: &Expression, env: &VarEnv<T>) -> Result<i64, EvalError> {
    let value = eval_expr::<T>(expr, env)?.real().round();
    if !value.is_finite() || value < 0.0 || value > i64::MAX as f64 {
        return Err(EvalError::UnsupportedExpression {
            kind: "invalid function shape dimension",
        });
    }
    Ok(value as i64)
}

fn bind_user_function_inputs<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    function_name: &str,
    inputs: &[FunctionParam],
    args: &[Expression],
    caller_env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let mut positional_idx = 0usize;
    for param in inputs {
        let arg_expr = named_args.get(param.name.as_str()).copied().or_else(|| {
            let next = positional_args.get(positional_idx).copied();
            if next.is_some() {
                positional_idx += 1;
            }
            next
        });

        if let Some(arg_expr) = arg_expr {
            if let Ok(value) = eval_expr::<T>(arg_expr, caller_env) {
                local_env.set(&param.name, value);
            }
            maybe_bind_function_input_alias(local_env, function_name, param, arg_expr, caller_env);
            if let Some(arg_path) = eval_field_access_path(arg_expr, caller_env) {
                copy_selected_input_fields(local_env, &param.name, &arg_path, caller_env)?;
            }
            copy_record_function_output_fields(local_env, param, arg_expr, caller_env)?;
            copy_array_input_entries(local_env, param, arg_expr, caller_env)?;
            continue;
        }

        let Some(default_expr) = &param.default else {
            return Err(EvalError::MissingBinding {
                name: param.name.clone(),
            });
        };
        let val = eval_expr::<T>(default_expr, local_env)?;
        local_env.set(&param.name, val);
        maybe_bind_function_input_alias(local_env, function_name, param, default_expr, caller_env);
    }
    Ok(())
}

fn copy_array_literal_vector_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    elements: &[Expression],
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    if elements.is_empty() {
        return bind_empty_array_literal_input(local_env, param);
    }
    if param.shape_expr.is_empty()
        && let Some(expected) = concrete_param_size(&param.dims)
        && expected != elements.len()
    {
        return Err(EvalError::ShapeMismatch {
            context: "function array input",
            expected,
            actual: elements.len(),
        });
    }

    let param_name = param.name.as_str();
    let mut first = None;
    let selection_field = selected_component_field_in_current_call(caller_env);
    for (idx, element) in elements.iter().enumerate() {
        let value = eval_expr::<T>(element, caller_env)?;
        if first.is_none() {
            first = Some(value);
        }
        local_env.set(&format!("{param_name}[{}]", idx + 1), value);
        if let Some(field) = selection_field {
            local_env.set(&format!("{param_name}.{field}[{}]", idx + 1), value);
            local_env.set(&format!("{param_name}[{}].{field}", idx + 1), value);
        }
    }
    if let Some(value) = first {
        local_env.set(param_name, value);
        if let Some(field) = selection_field {
            local_env.set(&format!("{param_name}.{field}"), value);
        }
    }
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    let shape = vec![elements.len() as i64];
    dims.insert(param_name.to_string(), shape.clone());
    if let Some(field) = selection_field {
        dims.insert(format!("{param_name}.{field}"), shape);
    }
    Ok(true)
}

fn bind_empty_array_literal_input<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
) -> Result<bool, EvalError> {
    let dims = if let Some(expected) = concrete_param_size(&param.dims) {
        if expected != 0 {
            return Err(EvalError::ShapeMismatch {
                context: "function array input",
                expected,
                actual: 0,
            });
        }
        param.dims.clone()
    } else if accepts_empty_vector_literal(param) {
        vec![0]
    } else if !param.shape_expr.is_empty() {
        return Err(EvalError::ShapeMismatch {
            context: "function array input rank",
            expected: param.shape_expr.len(),
            actual: 1,
        });
    } else {
        return Ok(false);
    };
    std::sync::Arc::make_mut(&mut local_env.dims).insert(param.name.clone(), dims);
    Ok(true)
}

fn accepts_empty_vector_literal(param: &FunctionParam) -> bool {
    param.dims.len() == 1 && param.dims[0] < 0
        || param.dims.is_empty() && param.shape_expr.len() == 1
}

fn copy_array_literal_matrix_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    rows: &[Expression],
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    if rows.is_empty() {
        return Ok(false);
    }

    let param_name = param.name.as_str();
    let mut first = None;
    let mut max_cols = 0usize;
    let mut actual = 0usize;
    let selection_field = selected_component_field_in_current_call(caller_env);
    for (row_idx, row_expr) in rows.iter().enumerate() {
        let row_values: Vec<&Expression> = match row_expr {
            Expression::Array { elements, .. } => elements.iter().collect(),
            _ => vec![row_expr],
        };
        max_cols = max_cols.max(row_values.len());
        actual += row_values.len();
        for (col_idx, value_expr) in row_values.iter().enumerate() {
            let value = eval_expr::<T>(value_expr, caller_env)?;
            if first.is_none() {
                first = Some(value);
            }
            local_env.set(
                &format!("{param_name}[{},{}]", row_idx + 1, col_idx + 1),
                value,
            );
            if let Some(field) = selection_field {
                local_env.set(
                    &format!("{param_name}.{field}[{},{}]", row_idx + 1, col_idx + 1),
                    value,
                );
                local_env.set(
                    &format!("{param_name}[{},{}].{field}", row_idx + 1, col_idx + 1),
                    value,
                );
            }
        }
    }

    if max_cols == 0 {
        return Ok(false);
    }
    if param.shape_expr.is_empty()
        && let Some(expected) = concrete_param_size(&param.dims)
        && expected != actual
    {
        return Err(EvalError::ShapeMismatch {
            context: "function matrix input",
            expected,
            actual,
        });
    }
    if let Some(value) = first {
        local_env.set(param_name, value);
        if let Some(field) = selection_field {
            local_env.set(&format!("{param_name}.{field}"), value);
        }
    }
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    let shape = vec![rows.len() as i64, max_cols as i64];
    dims.insert(param_name.to_string(), shape.clone());
    if let Some(field) = selection_field {
        dims.insert(format!("{param_name}.{field}"), shape);
    }
    Ok(true)
}

fn selected_component_field_in_current_call<T: SimFloat>(env: &VarEnv<T>) -> Option<&'static str> {
    let caller = current_function_call_name(&env.runtime)?;
    complex_field_selection_from_path(&caller)
}

fn copy_array_literal_input_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let Expression::Array {
        elements,
        is_matrix,
        ..
    } = arg_expr
    else {
        return Ok(false);
    };

    if *is_matrix {
        copy_array_literal_matrix_entries(local_env, param, elements, caller_env)
    } else {
        copy_array_literal_vector_entries(local_env, param, elements, caller_env)
    }
}

fn is_pre_like_call_name(name: &rumoca_core::Reference) -> bool {
    let short = rumoca_core::top_level_last_segment(name.as_str());
    short.eq_ignore_ascii_case("pre") || short.eq_ignore_ascii_case("previous")
}

fn pre_like_array_source_name<T: SimFloat>(
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Option<String> {
    let pre_arg = match arg_expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args,
            ..
        } => args.first(),
        Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } if is_pre_like_call_name(name) => args.first(),
        _ => None,
    }?;
    match pre_arg {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.as_str().to_string()),
        _ => eval_field_access_path(pre_arg, caller_env),
    }
}

fn lookup_pre_with_normalization<T: SimFloat>(name: &str, env: &VarEnv<T>) -> Option<T> {
    if let Some(value) = lookup_pre_value(name) {
        return Some(T::from_f64(value));
    }
    if let Some(normalized) = normalize_var_name::<T>(name, env)
        && let Some(value) = lookup_pre_value(normalized.as_str())
    {
        return Some(T::from_f64(value));
    }
    if let Some(base_name) = unity_subscript_base_name(name)
        && let Some(value) = lookup_pre_value(base_name.as_str())
    {
        return Some(T::from_f64(value));
    }
    None
}

fn copy_array_input_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let param_name = param.name.as_str();
    if param.dims.is_empty() && param.shape_expr.is_empty() {
        return Ok(());
    }
    if copy_array_literal_input_entries(local_env, param, arg_expr, caller_env)? {
        return Ok(());
    }

    let trace_array_bind = crate::trace::sim_or_introspect_enabled();
    let pre_source_name = pre_like_array_source_name(arg_expr, caller_env);
    let use_pre_values = pre_source_name.is_some();
    let source_name = pre_source_name.or_else(|| match arg_expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.as_str().to_string()),
        _ => eval_field_access_path(arg_expr, caller_env),
    });
    let Some(source_name) = source_name else {
        return bind_evaluated_array_input(local_env, param, arg_expr, caller_env);
    };

    if let Some(value) = if use_pre_values {
        lookup_pre_with_normalization::<T>(&source_name, caller_env)
    } else {
        caller_env.vars.get(&source_name).copied()
    } {
        local_env.set(param_name, value);
    }

    let source_index_prefix = format!("{source_name}[");
    let mut copied_entries = 0usize;
    for (key, value) in &caller_env.vars {
        if let Some(index_suffix) = key.strip_prefix(&source_name)
            && index_suffix.starts_with('[')
        {
            let source_index_key = format!("{source_name}{index_suffix}");
            let copied_value = if use_pre_values {
                lookup_pre_with_normalization::<T>(&source_index_key, caller_env).unwrap_or(*value)
            } else {
                *value
            };
            local_env.set(&format!("{param_name}{index_suffix}"), copied_value);
            copied_entries += 1;
            continue;
        }
        if let Some(index_suffix) = key.strip_prefix(&source_index_prefix) {
            let source_index_key = format!("{source_index_prefix}{index_suffix}");
            let copied_value = if use_pre_values {
                lookup_pre_with_normalization::<T>(&source_index_key, caller_env).unwrap_or(*value)
            } else {
                *value
            };
            local_env.set(&format!("{param_name}[{index_suffix}"), copied_value);
            copied_entries += 1;
        }
    }

    if let Some(dims) = caller_env.dims.get(&source_name) {
        std::sync::Arc::make_mut(&mut local_env.dims).insert(param_name.to_string(), dims.clone());
    }

    if trace_array_bind && source_name.contains("timeTable.table") {
        let t11 = caller_env
            .vars
            .get(&format!("{}[1,1]", source_name))
            .copied();
        let t21 = caller_env
            .vars
            .get(&format!("{}[2,1]", source_name))
            .copied();
        let t22 = caller_env
            .vars
            .get(&format!("{}[2,2]", source_name))
            .copied();
        tracing::debug!(
            target: "rumoca_eval_dae::sim",
            "function array-arg bind source='{}' param='{}' copied_entries={} dims={:?} base_present={} sample_entries=[1,1]={:?} [2,1]={:?} [2,2]={:?}",
            source_name,
            param_name,
            copied_entries,
            caller_env.dims.get(&source_name),
            caller_env.vars.contains_key(&source_name),
            t11,
            t21,
            t22
        );
    }
    Ok(())
}

fn bind_evaluated_array_input<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let values = try_eval_array_like_values::<T>(arg_expr, caller_env)?;
    let Some(dims) =
        resolved_array_input_dims(param, arg_expr, caller_env, local_env, values.len())?
    else {
        return Ok(());
    };
    let expected = concrete_param_size(&dims).ok_or(EvalError::UnsupportedExpression {
        kind: "function array input shape",
    })?;
    if values.len() != expected {
        return Err(EvalError::ShapeMismatch {
            context: "function array input",
            expected,
            actual: values.len(),
        });
    }
    set_array_entries(local_env, &param.name, &dims, &values);
    std::sync::Arc::make_mut(&mut local_env.dims).insert(param.name.clone(), dims);
    Ok(())
}

fn resolved_array_input_dims<T: SimFloat>(
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
    local_env: &VarEnv<T>,
    value_count: usize,
) -> Result<Option<Vec<i64>>, EvalError> {
    if !param.shape_expr.is_empty() {
        return infer_dynamic_array_input_dims(
            &param.shape_expr,
            arg_expr,
            caller_env,
            local_env,
            value_count,
        )
        .map(Some);
    }
    if concrete_param_size(&param.dims).is_some() {
        return Ok(Some(param.dims.clone()));
    }
    if !param.dims.is_empty() && param.dims.iter().any(|dim| *dim <= 0) {
        return infer_dynamic_array_input_dims_from_declared(&param.dims, value_count).map(Some);
    }
    Ok(None)
}

fn infer_dynamic_array_input_dims<T: SimFloat>(
    shape_expr: &[Subscript],
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
    local_env: &VarEnv<T>,
    value_count: usize,
) -> Result<Vec<i64>, EvalError> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    let mut dynamic_indices = Vec::new();
    for subscript in shape_expr {
        match subscript {
            Subscript::Index { value, .. } => dims.push(*value),
            Subscript::Expr { expr, .. } => dims.push(eval_shape_expr_dim(expr, local_env)?),
            Subscript::Colon { .. } => {
                dynamic_indices.push(dims.len());
                dims.push(0);
            }
        }
    }
    fill_dynamic_array_input_dims(
        &mut dims,
        &dynamic_indices,
        arg_expr,
        caller_env,
        value_count,
    )?;
    validate_array_input_dims(&dims, value_count)?;
    Ok(dims)
}

fn infer_dynamic_array_input_dims_from_declared(
    declared_dims: &[i64],
    value_count: usize,
) -> Result<Vec<i64>, EvalError> {
    let mut dims = declared_dims.to_vec();
    let dynamic_indices = dims
        .iter()
        .enumerate()
        .filter_map(|(idx, dim)| (*dim <= 0).then_some(idx))
        .collect::<Vec<_>>();
    fill_dynamic_array_input_dims_from_count(&mut dims, &dynamic_indices, value_count)?;
    validate_array_input_dims(&dims, value_count)?;
    Ok(dims)
}

fn fill_dynamic_array_input_dims<T: SimFloat>(
    dims: &mut [i64],
    dynamic_indices: &[usize],
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
    value_count: usize,
) -> Result<(), EvalError> {
    if dynamic_indices.is_empty() {
        return Ok(());
    }
    if dynamic_indices.len() == 1 {
        return fill_dynamic_array_input_dims_from_count(dims, dynamic_indices, value_count);
    }
    let arg_dims = infer_array_arg_dims(arg_expr, caller_env, value_count)?;
    if arg_dims.len() != dims.len() {
        return Err(EvalError::ShapeMismatch {
            context: "function array input rank",
            expected: dims.len(),
            actual: arg_dims.len(),
        });
    }
    for idx in dynamic_indices {
        dims[*idx] = arg_dims[*idx];
    }
    Ok(())
}

fn fill_dynamic_array_input_dims_from_count(
    dims: &mut [i64],
    dynamic_indices: &[usize],
    value_count: usize,
) -> Result<(), EvalError> {
    let known_product = dims
        .iter()
        .enumerate()
        .filter(|(idx, dim)| !dynamic_indices.contains(idx) && **dim > 0)
        .try_fold(1usize, |acc, (_, dim)| {
            usize::try_from(*dim)
                .ok()
                .and_then(|dim| acc.checked_mul(dim))
        })
        .ok_or(EvalError::UnsupportedExpression {
            kind: "function array input shape",
        })?;
    if dynamic_indices.len() != 1
        || known_product == 0
        || !value_count.is_multiple_of(known_product)
    {
        return Err(EvalError::ShapeMismatch {
            context: "function array input shape",
            expected: known_product,
            actual: value_count,
        });
    }
    let inferred = value_count / known_product;
    dims[dynamic_indices[0]] = inferred as i64;
    Ok(())
}

fn infer_array_arg_dims<T: SimFloat>(
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
    value_count: usize,
) -> Result<Vec<i64>, EvalError> {
    match arg_expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            caller_env
                .dims
                .get(name.as_str())
                .cloned()
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "function array input argument shape",
                })
        }
        Expression::Array {
            elements,
            is_matrix: true,
            ..
        } => {
            let cols = elements
                .iter()
                .map(|element| eval_array_values::<T>(element, caller_env).len())
                .max()
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "matrix literal shape",
                })?;
            Ok(vec![elements.len() as i64, cols as i64])
        }
        Expression::Array { .. }
        | Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::ArrayComprehension { .. } => Ok(vec![value_count as i64]),
        Expression::BuiltinCall {
            function: BuiltinFunction::Cross,
            ..
        } => Ok(vec![3]),
        Expression::BuiltinCall {
            function: BuiltinFunction::Skew,
            ..
        } => Ok(vec![3, 3]),
        _ => Err(EvalError::UnsupportedExpression {
            kind: "function array input argument shape",
        }),
    }
}

fn validate_array_input_dims(dims: &[i64], value_count: usize) -> Result<(), EvalError> {
    if dims.is_empty() || dims.iter().any(|dim| *dim < 0) {
        return Err(EvalError::UnsupportedExpression {
            kind: "function array input shape",
        });
    }
    let expected = concrete_param_size(dims).ok_or(EvalError::UnsupportedExpression {
        kind: "function array input shape",
    })?;
    if expected != value_count {
        return Err(EvalError::ShapeMismatch {
            context: "function array input shape",
            expected,
            actual: value_count,
        });
    }
    Ok(())
}

fn concrete_param_size(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        usize::try_from(*dim)
            .ok()
            .and_then(|dim| acc.checked_mul(dim))
    })
}

fn initialize_user_function_scope_values<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    outputs: &[FunctionParam],
    locals: &[FunctionParam],
) -> Result<(), EvalError> {
    for param in outputs.iter().chain(locals.iter()) {
        let dims = local_env
            .dims
            .get(param.name.as_str())
            .cloned()
            .unwrap_or_else(|| param.dims.clone());
        let Some(size) = concrete_param_size(&dims) else {
            let val = param
                .default
                .as_ref()
                .map(|default| eval_expr::<T>(default, local_env))
                .transpose()?
                .unwrap_or(T::zero());
            local_env.set(&param.name, val);
            continue;
        };
        let values = match &param.default {
            Some(default) => eval_shaped_array_values::<T>(default, local_env, size)?,
            None => vec![T::zero(); size],
        };
        set_array_entries(local_env, &param.name, &dims, &values);
    }
    Ok(())
}

fn selected_output_name(selection: &OutputSelection) -> String {
    if selection.indices.is_empty() {
        return selection.output_name.clone();
    }
    let joined = selection
        .indices
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",");
    format!("{}[{joined}]", selection.output_name)
}

fn requested_function_output_name(
    selection: Option<&OutputSelection>,
    outputs: &[FunctionParam],
) -> Option<String> {
    selection
        .map(selected_output_name)
        .or_else(|| outputs.first().map(|output| output.name.clone()))
}

fn function_output_cache_key(
    function: &VarName,
    args: &[Expression],
    output_name: impl Into<String>,
    env: &VarEnv<impl SimFloat>,
) -> FunctionOutputCacheKey {
    FunctionOutputCacheKey::new(function, args, output_name, env.is_initial)
}

fn cache_user_function_outputs<T: SimFloat>(
    env: &VarEnv<T>,
    function: &VarName,
    args: &[Expression],
    outputs: &[FunctionParam],
    local_env: &VarEnv<T>,
) {
    for output in outputs {
        let output_name = output.name.as_str();
        if let Some(value) = local_env.vars.get(output_name).copied() {
            let key = function_output_cache_key(function, args, output_name, env);
            env.insert_function_output_cache(key, value);
        }

        let field_prefix = format!("{output_name}.");
        let indexed_prefix = format!("{output_name}[");
        for (name, value) in local_env.vars.local_iter() {
            if name.starts_with(&field_prefix) || name.starts_with(&indexed_prefix) {
                let key = function_output_cache_key(function, args, name, env);
                env.insert_function_output_cache(key, *value);
            }
        }
    }
}

struct FunctionCallSummary {
    args_len: usize,
    inputs_len: usize,
    outputs_len: usize,
    locals_len: usize,
    body_len: usize,
    is_external: bool,
}

fn trace_function_call_summary(
    trace_call: bool,
    requested_name: &VarName,
    resolved_name: &VarName,
    summary: &FunctionCallSummary,
) {
    if !trace_call {
        return;
    }
    tracing::debug!(
        target: "rumoca_eval_dae::function_match",
        "function-call name='{}' resolved='{}' args={} inputs={} outputs={} locals={} body={} external={}",
        requested_name.as_str(),
        resolved_name.as_str(),
        summary.args_len,
        summary.inputs_len,
        summary.outputs_len,
        summary.locals_len,
        summary.body_len,
        summary.is_external
    );
}

fn trace_function_call_body_once(
    trace_call: bool,
    resolved_name: &VarName,
    body: &[rumoca_core::Statement],
) {
    if !trace_call {
        return;
    }
    static PRINT_FUNCTION_BODY_ONCE: std::sync::Once = std::sync::Once::new();
    PRINT_FUNCTION_BODY_ONCE.call_once(|| {
        tracing::debug!(
            target: "rumoca_eval_dae::function_match",
            "function-call first traced body for '{}': {:?}",
            resolved_name.as_str(),
            body
        );
    });
}

fn trace_function_call_inputs<T: SimFloat>(
    trace_call: bool,
    local_env: &VarEnv<T>,
    inputs: &[FunctionParam],
) {
    if !trace_call || !crate::trace::function_inputs_enabled() {
        return;
    }
    for input in inputs {
        let name = input.name.as_str();
        let dim = local_env.dims.get(name).cloned();
        let first = local_env.vars.get(&format!("{name}[1]")).copied();
        let second = local_env.vars.get(&format!("{name}[2]")).copied();
        let first_re = local_env.vars.get(&format!("{name}[1].re")).copied();
        let second_re = local_env.vars.get(&format!("{name}[2].re")).copied();
        let first_im = local_env.vars.get(&format!("{name}[1].im")).copied();
        let second_im = local_env.vars.get(&format!("{name}[2].im")).copied();
        tracing::debug!(
            target: "rumoca_eval_dae::function_inputs",
            "function-call input {} dims={:?} {}[1]={:?} {}[2]={:?} {}[1].re={:?} {}[2].re={:?} {}[1].im={:?} {}[2].im={:?}",
            name,
            dim,
            name,
            first,
            name,
            second,
            name,
            first_re,
            name,
            second_re,
            name,
            first_im,
            name,
            second_im
        );
    }
}

fn trace_function_call_outputs<T: SimFloat>(
    trace_call: bool,
    local_env: &VarEnv<T>,
    outputs: &[FunctionParam],
) {
    if !trace_call {
        return;
    }
    for output in outputs {
        let base = output.name.as_str();
        tracing::debug!(
            target: "rumoca_eval_dae::function_match",
            "function-call output {} = {} | {}.re = {} | {}.im = {}",
            base,
            local_env.get(base).real(),
            base,
            local_env.get(&format!("{base}.re")).real(),
            base,
            local_env.get(&format!("{base}.im")).real()
        );
    }
}

fn maybe_trace_interpolation_coefficients_state<T: SimFloat>(
    resolved_name: &VarName,
    local_env: &VarEnv<T>,
    body: &[rumoca_core::Statement],
) {
    if !crate::trace::sim_or_introspect_enabled()
        || !resolved_name
            .as_str()
            .ends_with("Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients")
    {
        return;
    }
    static PRINT_GETINTERP_BODY: std::sync::Once = std::sync::Once::new();
    PRINT_GETINTERP_BODY.call_once(|| {
        tracing::debug!(target: "rumoca_eval_dae::sim", "getInterpolationCoefficients body={:?}", body);
    });
    tracing::debug!(
        target: "rumoca_eval_dae::sim",
        "getInterpolationCoefficients state: startTimeScaled={} shiftTimeScaled={} timeScaled={} last_in={} next={} nrow={} tp={} dt={} a={} b={} nextEventScaled={}",
        local_env.get("startTimeScaled").real(),
        local_env.get("shiftTimeScaled").real(),
        local_env.get("timeScaled").real(),
        local_env.get("last").real(),
        local_env.get("next").real(),
        local_env.get("nrow").real(),
        local_env.get("tp").real(),
        local_env.get("dt").real(),
        local_env.get("a").real(),
        local_env.get("b").real(),
        local_env.get("nextEventScaled").real()
    );
}

fn resolve_selection_value<T: SimFloat>(local_env: &VarEnv<T>, selection: &OutputSelection) -> T {
    let selected_name = selected_output_name(selection);
    if let Some(value) = local_env.vars.get(&selected_name).copied() {
        return value;
    }
    if let Some((base_output, _)) = rumoca_core::split_first_top_level(&selection.output_name)
        && let Some(value) = local_env.vars.get(base_output).copied()
    {
        // Scalar evaluator executes user functions in selection context
        // (`*.re` / `*.im` caller names). If field materialization is absent
        // in the local env, use the base output bound in that context.
        return value;
    }
    T::zero()
}

fn eval_user_function_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let (resolved_name, selection) = resolve_user_function_target(name.as_str(), env)?;
    let func = resolve_user_function(resolved_name.as_str(), env)?;

    // External/empty stubs cannot be evaluated here; allow special handlers
    // (e.g. BooleanVectors/Clock wrappers) to resolve after this path.
    if func.external.is_some() || func.body.is_empty() {
        return None;
    }

    // Clone function data to release borrow on env.functions
    let inputs = func.inputs.clone();
    let outputs = func.outputs.clone();
    let locals = func.locals.clone();
    let body = func.body.clone();
    if let Some(output_name) = requested_function_output_name(selection.as_ref(), &outputs) {
        let key = function_output_cache_key(&resolved_name, args, output_name, env);
        if let Some(value) = env.get_function_output_cache(&key) {
            return Some(value);
        }
    }

    let Some(_recursion_depth_guard) = try_enter_function_recursion_for(&env.runtime) else {
        return Some(T::nan());
    };

    let trace_call = function_trace_match_enabled(name.as_str(), resolved_name.as_str());
    let summary = FunctionCallSummary {
        args_len: args.len(),
        inputs_len: inputs.len(),
        outputs_len: outputs.len(),
        locals_len: locals.len(),
        body_len: body.len(),
        is_external: func.external.is_some(),
    };
    trace_function_call_summary(trace_call, name, &resolved_name, &summary);
    trace_function_call_body_once(trace_call, &resolved_name, &body);

    let mut local_env = build_local_function_env(env);
    seed_static_function_scope_dims(&mut local_env, &inputs, &outputs, &locals);
    // Bind arguments/defaults and execute the body under the call-stack context
    // so selected function calls (`*.re` / `*.im`) propagate correctly.
    let eval_result = with_function_call_stack(&env.runtime, name.as_str(), || {
        bind_user_function_inputs(&mut local_env, name.as_str(), &inputs, args, env)?;
        seed_resolved_function_scope_dims(&mut local_env, &outputs)?;
        seed_resolved_function_scope_dims(&mut local_env, &locals)?;
        trace_function_call_inputs(trace_call, &local_env, &inputs);
        initialize_user_function_scope_values(&mut local_env, &outputs, &locals)?;
        crate::statement::eval_statements(&body, &mut local_env)
    });
    if let Err(err) = eval_result {
        warn_once!(
            env.runtime.warned_user_functions,
            "User function '{}' failed during scalar DAE evaluation: {}",
            resolved_name.as_str(),
            err
        );
        return Some(T::nan());
    }
    trace_function_call_outputs(trace_call, &local_env, &outputs);
    maybe_trace_interpolation_coefficients_state(&resolved_name, &local_env, &body);
    cache_user_function_outputs(env, &resolved_name, args, &outputs, &local_env);

    if let Some(ref selection) = selection {
        return Some(resolve_selection_value(&local_env, selection));
    }

    Some(
        outputs
            .first()
            .map_or_else(T::zero, |out| local_env.get(&out.name)),
    )
}

pub fn eval_user_function_output_path_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    output_path: &str,
    env: &VarEnv<T>,
) -> Result<T, EvalError> {
    let (_, _, local_env) = eval_user_function_local_env(name, args, env)?;
    local_env
        .vars
        .get(output_path)
        .copied()
        .ok_or_else(|| EvalError::MissingBinding {
            name: output_path.to_string(),
        })
}

fn eval_user_function_local_env<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<(VarName, Vec<FunctionParam>, VarEnv<T>), EvalError> {
    let (resolved_name, selection) =
        resolve_user_function_target(name.as_str(), env).ok_or_else(|| {
            EvalError::MissingFunction {
                name: name.to_string(),
            }
        })?;
    if selection.is_some() {
        return Err(EvalError::UnsupportedExpression {
            kind: "selected function output path",
        });
    }
    let func = resolve_user_function(resolved_name.as_str(), env).ok_or_else(|| {
        EvalError::MissingFunction {
            name: resolved_name.to_string(),
        }
    })?;
    if func.external.is_some() || func.body.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "external function output path",
        });
    }

    let inputs = func.inputs.clone();
    let outputs = func.outputs.clone();
    let locals = func.locals.clone();
    let body = func.body.clone();
    let Some(_recursion_depth_guard) = try_enter_function_recursion_for(&env.runtime) else {
        return Err(EvalError::UnsupportedExpression {
            kind: "function recursion limit",
        });
    };

    let mut local_env = build_local_function_env(env);
    seed_static_function_scope_dims(&mut local_env, &inputs, &outputs, &locals);
    with_function_call_stack(&env.runtime, name.as_str(), || {
        bind_user_function_inputs(&mut local_env, name.as_str(), &inputs, args, env)?;
        seed_resolved_function_scope_dims(&mut local_env, &outputs)?;
        seed_resolved_function_scope_dims(&mut local_env, &locals)?;
        initialize_user_function_scope_values(&mut local_env, &outputs, &locals)?;
        crate::statement::eval_statements(&body, &mut local_env)
    })?;
    cache_user_function_outputs(env, &resolved_name, args, &outputs, &local_env);
    Ok((resolved_name, outputs, local_env))
}

pub fn eval_user_function_array_output_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let (resolved_name, selection) =
        resolve_user_function_target(name.as_str(), env).ok_or_else(|| {
            EvalError::MissingFunction {
                name: name.to_string(),
            }
        })?;
    if selection.is_some() {
        return Err(EvalError::UnsupportedExpression {
            kind: "selected function array output",
        });
    }
    let func = resolve_user_function(resolved_name.as_str(), env).ok_or_else(|| {
        EvalError::MissingFunction {
            name: resolved_name.to_string(),
        }
    })?;
    if func.external.is_some() || func.body.is_empty() {
        return Err(EvalError::UnsupportedExpression {
            kind: "external function array output",
        });
    }

    let inputs = func.inputs.clone();
    let outputs = func.outputs.clone();
    let locals = func.locals.clone();
    let body = func.body.clone();
    let output = outputs
        .first()
        .ok_or(EvalError::UnsupportedExpression {
            kind: "function array output",
        })?
        .clone();
    let Some(_recursion_depth_guard) = try_enter_function_recursion_for(&env.runtime) else {
        return Err(EvalError::UnsupportedExpression {
            kind: "function recursion limit",
        });
    };

    let mut local_env = build_local_function_env(env);
    seed_static_function_scope_dims(&mut local_env, &inputs, &outputs, &locals);
    with_function_call_stack(&env.runtime, name.as_str(), || {
        bind_user_function_inputs(&mut local_env, name.as_str(), &inputs, args, env)?;
        seed_resolved_function_scope_dims(&mut local_env, &outputs)?;
        seed_resolved_function_scope_dims(&mut local_env, &locals)?;
        initialize_user_function_scope_values(&mut local_env, &outputs, &locals)?;
        crate::statement::eval_statements(&body, &mut local_env)
    })?;
    cache_user_function_outputs(env, &resolved_name, args, &outputs, &local_env);
    collect_function_array_output(&output, &local_env)
}

fn collect_function_array_output<T: SimFloat>(
    output: &FunctionParam,
    local_env: &VarEnv<T>,
) -> Result<Vec<T>, EvalError> {
    let declared_dims = local_env.dims.get(output.name.as_str()).cloned();
    let Some(dims) = declared_dims.filter(|dims| !dims.is_empty()) else {
        if let Some(values) = array_values_from_env_name_generic(output.name.as_str(), local_env)
            && values.len() > 1
        {
            return Ok(values);
        }
        return local_env
            .vars
            .get(output.name.as_str())
            .copied()
            .map(|value| vec![value])
            .ok_or_else(|| EvalError::MissingBinding {
                name: output.name.clone(),
            });
    };

    let expected = concrete_param_size(&dims).ok_or(EvalError::UnsupportedExpression {
        kind: "function array output shape",
    })?;
    if expected == 0 {
        return Ok(Vec::new());
    }
    let values =
        array_values_from_env_name_generic(output.name.as_str(), local_env).ok_or_else(|| {
            EvalError::MissingBinding {
                name: output.name.clone(),
            }
        })?;
    if values.len() != expected {
        return Err(EvalError::ShapeMismatch {
            context: "function array output",
            expected,
            actual: values.len(),
        });
    }
    Ok(values)
}

fn function_trace_match_enabled(_name: &str, _resolved_name: &str) -> bool {
    // Gated by the `rumoca_eval_dae::function_match` trace target. (The former
    // env var took a comma-separated name filter; the boolean trace target now
    // traces all function calls when enabled.)
    crate::trace::function_match_enabled()
}

fn copy_selected_input_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param_name: &str,
    arg_path: &str,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let src_prefix = format!("{arg_path}.");
    let dst_prefix = format!("{param_name}.");
    for (field_name, field_value) in selected_input_fields(env, &src_prefix, &dst_prefix) {
        local_env.set(&field_name, field_value);
    }
    copy_selected_input_start_fields(local_env, env, &src_prefix, &dst_prefix)?;
    Ok(())
}

fn selected_input_fields<T: SimFloat>(
    env: &VarEnv<T>,
    src_prefix: &str,
    dst_prefix: &str,
) -> Vec<(String, T)> {
    let mut selected = Vec::new();
    for (key, value) in &env.vars {
        if let Some(suffix) = key.strip_prefix(src_prefix) {
            selected.push((format!("{dst_prefix}{suffix}"), *value));
        }
    }
    selected
}

fn copy_selected_input_start_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    env: &VarEnv<T>,
    src_prefix: &str,
    dst_prefix: &str,
) -> Result<(), EvalError> {
    for (key, start_expr) in env.start_exprs.iter() {
        let Some(suffix) = key.strip_prefix(src_prefix) else {
            continue;
        };
        let dst = format!("{dst_prefix}{suffix}");
        if local_env.vars.contains_key(dst.as_str()) {
            continue;
        }
        match eval_expr::<T>(start_expr, env) {
            Ok(value) => local_env.set(&dst, value),
            Err(err) if err.missing_binding_name().is_some() => {}
            Err(err) => return Err(err),
        }
    }
    Ok(())
}

fn copy_record_function_output_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    if param.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(());
    }
    let Expression::FunctionCall { name, args, .. } = arg_expr else {
        return Ok(());
    };
    let Some(function) = resolve_user_function(name.as_str(), env) else {
        return Ok(());
    };
    let Some(output) = function.outputs.first() else {
        return Ok(());
    };
    if output.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(());
    }
    if copy_materialized_record_function_output_fields(
        local_env,
        param,
        name.var_name(),
        args,
        env,
    )? {
        return Ok(());
    }

    if let Some(fields) =
        super::array_helpers::record_constructor_fields_for_output(function, output, env)
    {
        for field in fields {
            copy_record_function_output_field(
                local_env,
                param,
                name.var_name(),
                args,
                output,
                field,
                env,
            )?;
        }
        return Ok(());
    }
    Err(EvalError::UnsupportedExpression {
        kind: "record function output fields",
    })
}

fn copy_record_function_output_field<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    function_name: &VarName,
    args: &[Expression],
    output: &FunctionParam,
    field: &FunctionParam,
    env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let total = super::array_helpers::function_param_size(field).ok_or(
        EvalError::UnsupportedExpression {
            kind: "record function output field shape",
        },
    )?;
    let output_name = format!("{}.{}", output.name, field.name);
    let input_name = format!("{}.{}", param.name, field.name);
    for flat_index in 0..total {
        let output_path =
            super::array_helpers::function_output_path(&output_name, &field.dims, flat_index)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "record function output field index",
                })?;
        let input_path =
            super::array_helpers::function_output_path(&input_name, &field.dims, flat_index)
                .ok_or(EvalError::UnsupportedExpression {
                    kind: "record function input field index",
                })?;
        let value = eval_user_function_output_path_pub(function_name, args, &output_path, env)?;
        local_env.set(&input_path, value);
    }
    Ok(())
}

fn copy_materialized_record_function_output_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    function_name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let (_, outputs, function_env) = eval_user_function_local_env(function_name, args, env)?;
    let Some(output) = outputs
        .iter()
        .find(|output| output.type_class == Some(rumoca_core::ClassType::Record))
    else {
        return Err(EvalError::UnsupportedExpression {
            kind: "record function output fields",
        });
    };
    let src_prefix = format!("{}.", output.name);
    let dst_prefix = format!("{}.", param.name);
    let mut copied = false;
    for (key, value) in &function_env.vars {
        let Some(suffix) = key.strip_prefix(&src_prefix) else {
            continue;
        };
        local_env.set(&format!("{dst_prefix}{suffix}"), *value);
        copied = true;
    }
    Ok(copied)
}

fn maybe_bind_function_input_alias<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    current_function_name: &str,
    param: &FunctionParam,
    arg_expr: &Expression,
    env: &VarEnv<T>,
) {
    // Only function-typed inputs can be invoked as callable values.
    if !param.type_name.to_ascii_lowercase().contains("function") {
        return;
    }
    let Some(closure) = function_closure_from_arg(arg_expr, env) else {
        return;
    };
    local_env
        .function_closures
        .insert(param.name.clone(), closure.clone());
    local_env
        .function_closures
        .insert(format!("{current_function_name}.{}", param.name), closure);
}

pub(super) fn function_closure_from_arg<T: SimFloat>(
    arg_expr: &Expression,
    env: &VarEnv<T>,
) -> Option<FunctionClosure> {
    match arg_expr {
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } if !is_constructor && resolve_user_function(name.as_str(), env).is_some() => {
            Some(FunctionClosure {
                target_name: name.var_name().clone(),
                bound_args: args.clone(),
            })
        }
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolve_function_closure(name.as_str(), env).cloned(),
        _ => None,
    }
}

fn eval_function_closure_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> Option<T> {
    let closure = resolve_function_closure(name.as_str(), env)?;
    let target = resolve_user_function(closure.target_name.as_str(), env)?;
    let mut merged_args = Vec::with_capacity(args.len().saturating_add(closure.bound_args.len()));
    // Modelica partial function application leaves at least one open argument
    // (e.g., `f(u)`), so runtime invocation supplies those first.
    merged_args.extend(args.iter().cloned());
    merged_args.extend(closure.bound_args.iter().cloned());
    if merged_args.len() > target.inputs.len() {
        return None;
    }
    eval_user_function_call(&closure.target_name, &merged_args, env)
}

fn eval_constructor_call<T: SimFloat>(name: &VarName, args: &[Expression], env: &VarEnv<T>) -> T {
    if args.is_empty() {
        return T::zero();
    }
    if args.len() == 1 {
        if let Some(caller) = current_function_call_name(&env.runtime)
            && complex_field_selection_from_path(&caller) == Some("im")
        {
            // Constructors like Complex(1) imply zero imaginary part.
            return T::zero();
        }
        return eval_expr_or_default::<T>(&args[0], env);
    }

    // When constructor calls survive into scalar evaluation, prefer component
    // selection by caller suffix if available (e.g., generated `*.im` helpers).
    if let Some(caller) = current_function_call_name(&env.runtime) {
        match complex_field_selection_from_path(&caller) {
            Some("im") => return eval_expr_or_default::<T>(&args[1], env),
            Some("re") => return eval_expr_or_default::<T>(&args[0], env),
            _ => {}
        }
    }

    let _ = name;
    eval_expr_or_default::<T>(&args[0], env)
}

pub(super) fn eval_function_call<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    env: &VarEnv<T>,
) -> T {
    let short_name = rumoca_core::top_level_last_segment(name.as_str());
    if let Some(result) = eval_external_table_function(short_name, args, env) {
        return result;
    }
    if is_constructor {
        return eval_constructor_call(name, args, env);
    }
    if name.as_str() == "Complex" {
        // MLS §6.7.1: Complex is the built-in operator-record constructor.
        // Flattened/runtime paths may still surface it as a plain function call,
        // so preserve constructor semantics even when `is_constructor` is lost.
        return eval_constructor_call(name, args, env);
    }
    // Try qualified builtin resolution:
    // "Modelica.Math.sin" → "sin" → BuiltinFunction::Sin
    if let Some(builtin) = BuiltinFunction::from_name(short_name) {
        return eval_builtin(builtin, args, env);
    }
    if let Some(builtin) = BuiltinFunction::from_name(&short_name.to_ascii_lowercase()) {
        return eval_builtin(builtin, args, env);
    }

    if let Some(result) = eval_function_closure_call(name, args, env) {
        return result;
    }

    if is_runtime_special_function_name(name.as_str())
        && let Some(result) = eval_special_function_call(name.as_str(), args, env)
    {
        return result;
    }

    // Runtime special functions must bypass structured user-function bodies so
    // standard library helpers like BooleanVectors.firstTrueIndex keep their
    // declared runtime semantics on the simulation path.
    if let Some(result) = eval_user_function_call(name, args, env) {
        return result;
    }
    if let Some(result) = eval_special_function_call(name.as_str(), args, env) {
        return result;
    }
    trace_unresolved_user_function(name.as_str(), env);

    warn_once!(
        env.runtime.warned_user_functions,
        "User-defined function '{}' not supported in simulation evaluator, \
         returning NaN. Results may be incorrect.",
        name.as_str()
    );
    T::nan()
}

fn trace_unresolved_user_function<T: SimFloat>(name: &str, env: &VarEnv<T>) {
    if !crate::trace::sim_or_introspect_enabled() {
        return;
    }
    let short = rumoca_core::top_level_last_segment(name);
    let direct_hit = env.functions.contains_key(name);
    let short_hits: Vec<String> = env
        .functions
        .keys()
        .filter(|candidate| rumoca_core::top_level_last_segment(candidate) == short)
        .take(16)
        .cloned()
        .collect();
    tracing::debug!(
        target: "rumoca_eval_dae::sim",
        "unresolved user function: name='{}' direct_hit={} total_functions={} short='{}' short_hits={:?}",
        name,
        direct_hit,
        env.functions.len(),
        short,
        short_hits
    );
}

/// Public wrapper for `eval_builtin`, used by the statement evaluator.
pub fn eval_builtin_pub<T: SimFloat>(
    function: BuiltinFunction,
    args: &[Expression],
    env: &VarEnv<T>,
) -> T {
    eval_builtin(function, args, env)
}

/// Public wrapper for `eval_function_call`, used by the statement evaluator.
pub fn eval_function_call_pub<T: SimFloat>(
    name: &VarName,
    args: &[Expression],
    env: &VarEnv<T>,
) -> T {
    eval_function_call(name, args, false, env)
}

/// DAE-IR wrapper for `eval_function_call_pub`.
pub fn eval_function_call_pub_dae<T: SimFloat>(
    name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    let flat_name = VarName::new(name.as_str());
    let flat_args: Vec<Expression> = args.to_vec();
    eval_function_call(&flat_name, &flat_args, false, env)
}

/// Evaluate a specific selected function output via its resolved base function name.
pub fn eval_selected_function_output_pub<T: SimFloat>(
    resolved_name: &VarName,
    output_name: &str,
    suffix: &str,
    args: &[Expression],
    env: &VarEnv<T>,
) -> T {
    let selected = selected_function_output_name(resolved_name, output_name, suffix);
    eval_function_call(&selected, args, false, env)
}

/// DAE-IR wrapper for `eval_selected_function_output_pub`.
pub fn eval_selected_function_output_pub_dae<T: SimFloat>(
    resolved_name: &rumoca_core::VarName,
    output_name: &str,
    suffix: &str,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
) -> T {
    let flat_name = VarName::new(resolved_name.as_str());
    let flat_args: Vec<Expression> = args.to_vec();
    eval_selected_function_output_pub(&flat_name, output_name, suffix, &flat_args, env)
}

pub(super) fn eval_if<T: SimFloat>(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    env: &VarEnv<T>,
) -> T {
    for (cond, then_expr) in branches {
        if crate::eval::eval_condition_truth(cond, env) {
            return eval_expr_or_default::<T>(then_expr, env);
        }
    }
    eval_expr_or_default::<T>(else_branch, env)
}

/// Evaluate a condition expression as a smooth zero-crossing function for root finding (f64-only).
pub fn eval_condition_as_root(expr: &Expression, env: &VarEnv<f64>) -> f64 {
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => {
            let l = eval_expr_or_default::<f64>(lhs, env);
            let r = eval_expr_or_default::<f64>(rhs, env);
            match op {
                OpBinary::Lt | OpBinary::Le => l - r,
                OpBinary::Gt | OpBinary::Ge => r - l,
                _ => {
                    let val = eval_expr_or_default::<f64>(expr, env);
                    if val.to_bool() { -1.0 } else { 1.0 }
                }
            }
        }
        _ => {
            let val = eval_expr_or_default::<f64>(expr, env);
            if val.to_bool() { -1.0 } else { 1.0 }
        }
    }
}
