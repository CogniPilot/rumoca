//! Binding of user-function inputs into a local evaluation scope.
//!
//! Covers scalar/record/array argument binding, record constructor field
//! copying, pre()-like array sources, and array input dimension inference.

use super::*;

pub(super) fn bind_user_function_inputs<T: SimFloat>(
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
            if bind_function_input_alias(local_env, function_name, param, arg_expr, caller_env)? {
                continue;
            }
            if copy_record_constructor_input_fields(local_env, param, arg_expr, caller_env)? {
                continue;
            }
            let scalar_bound = if let Ok(value) = eval_expr::<T>(arg_expr, caller_env) {
                bind_function_scalar_input(local_env, function_name, &param.name, value);
                true
            } else {
                false
            };
            let may_need_record_fields = param.type_class == Some(rumoca_core::ClassType::Record)
                || (!scalar_bound && param.dims.is_empty() && param.shape_expr.is_empty());
            if may_need_record_fields
                && let Some(arg_path) = try_eval_field_access_path(arg_expr, caller_env)?
            {
                copy_selected_input_fields(local_env, &param.name, &arg_path, caller_env)?;
            }
            let _ = copy_record_function_output_fields(local_env, param, arg_expr, caller_env)?;
            copy_array_input_entries(local_env, param, arg_expr, caller_env)?;
            continue;
        }

        let Some(default_expr) = &param.default else {
            return Err(EvalError::MissingBinding {
                name: param.name.clone(),
            });
        };
        if bind_function_input_alias(local_env, function_name, param, default_expr, caller_env)? {
            continue;
        }
        if !param.dims.is_empty() || !param.shape_expr.is_empty() {
            let dims = resolved_function_param_dims(param, local_env)?.ok_or(
                EvalError::UnsupportedExpression {
                    kind: "default function array input shape",
                },
            )?;
            let total = concrete_param_size(&dims).ok_or(EvalError::UnsupportedExpression {
                kind: "default function array input shape",
            })?;
            let values = eval_shaped_array_values(default_expr, local_env, total)?;
            set_array_entries(local_env, &param.name, &dims, &values);
            std::sync::Arc::make_mut(&mut local_env.dims).insert(param.name.clone(), dims);
            continue;
        }
        let val = eval_expr::<T>(default_expr, local_env)?;
        bind_function_scalar_input(local_env, function_name, &param.name, val);
    }
    Ok(())
}

pub(super) fn copy_record_constructor_input_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    if param.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(false);
    }
    let Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = arg_expr
    else {
        return Ok(false);
    };
    if !is_constructor {
        return Ok(false);
    }

    if let Some(constructor) = resolve_record_constructor(name, caller_env)? {
        return bind_declared_record_constructor_fields(
            local_env,
            param,
            constructor,
            args,
            caller_env,
        );
    }
    let mut copied = false;
    for arg in args {
        let Some((field, value_expr)) = decode_named_constructor_arg(arg) else {
            continue;
        };
        let value = eval_expr::<T>(value_expr, caller_env)?;
        local_env.set(&format!("{}.{field}", param.name), value);
        copied = true;
    }
    Ok(copied)
}

fn resolve_record_constructor<'a, T: SimFloat>(
    name: &rumoca_core::Reference,
    env: &'a VarEnv<T>,
) -> Result<Option<&'a rumoca_core::Function>, EvalError> {
    if let Some(def_id) = name.target_def_id() {
        return rumoca_core::resolve_record_constructor(
            env.functions.values(),
            name.as_str(),
            def_id,
        )
        .map(Some)
        .map_err(|error| EvalError::MissingFunction {
            name: error.to_string(),
        });
    }
    Ok(env
        .functions
        .get(name.as_str())
        .filter(|function| function.is_constructor))
}

fn bind_declared_record_constructor_fields<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    constructor: &rumoca_core::Function,
    args: &[Expression],
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let (named_args, positional_args) = split_named_and_positional_call_args(args);
    let mut positional_index = 0usize;
    let mut constructor_env = caller_env.clone();
    constructor_env.vars = VarScope::child_of(&caller_env.vars);
    let mut copied = false;
    for field in &constructor.inputs {
        let actual = named_args.get(field.name.as_str()).copied().or_else(|| {
            let value = positional_args.get(positional_index).copied();
            positional_index += usize::from(value.is_some());
            value
        });
        let value_expr = actual.or(field.default.as_ref());
        let Some(value_expr) = value_expr else {
            continue;
        };
        let value = eval_expr::<T>(value_expr, &constructor_env)?;
        constructor_env.set(&field.name, value);
        local_env.set(&format!("{}.{}", param.name, field.name), value);
        copied = true;
    }
    for (field, value_expr) in named_args {
        if constructor.inputs.iter().any(|input| input.name == field) {
            continue;
        }
        let value = eval_expr::<T>(value_expr, &constructor_env)?;
        local_env.set(&format!("{}.{field}", param.name), value);
        copied = true;
    }
    Ok(copied)
}

pub(super) fn copy_array_literal_vector_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    elements: &[Expression],
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    if elements.is_empty() {
        return Ok(false);
    }
    let selection_field = selected_component_field_in_current_call(caller_env);
    let values = elements
        .iter()
        .map(|element| eval_array_values::<T>(element, caller_env))
        .collect::<Result<Vec<_>, EvalError>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    let shape = try_runtime_array_literal_dims(elements, caller_env)?
        .into_iter()
        .map(|dimension| {
            i64::try_from(dimension).map_err(|_| EvalError::UnsupportedExpression {
                kind: "function array input shape",
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if param.shape_expr.is_empty()
        && param.dims.iter().all(|dimension| *dimension > 0)
        && param.dims != shape
    {
        let expected =
            concrete_param_size(&param.dims).ok_or(EvalError::UnsupportedExpression {
                kind: "function array input shape",
            })?;
        if expected != values.len() {
            return Err(EvalError::ShapeMismatch {
                context: "function array input",
                expected,
                actual: values.len(),
            });
        }
        return Err(EvalError::InvalidShape {
            context: "function array input",
            reason: format!("expected dimensions {:?}, got {shape:?}", param.dims),
        });
    }
    set_array_entries(local_env, &param.name, &shape, &values);
    if let Some(field) = selection_field {
        set_array_entries(
            local_env,
            &format!("{}.{field}", param.name),
            &shape,
            &values,
        );
    }
    let dims = std::sync::Arc::make_mut(&mut local_env.dims);
    dims.insert(param.name.clone(), shape.clone());
    if let Some(field) = selection_field {
        dims.insert(format!("{}.{field}", param.name), shape);
    }
    Ok(true)
}

pub(super) fn copy_array_literal_matrix_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let Expression::Array {
        elements,
        is_matrix: true,
        ..
    } = arg_expr
    else {
        return Ok(false);
    };
    if elements.is_empty() {
        return Ok(false);
    }

    // Matrix constructors are concatenations (MLS §10.4.2), so an element
    // may itself be an array-valued row or column expression.  Use the same
    // canonical evaluator as every other array context instead of imposing a
    // second, row-only interpretation at the function-call boundary.
    let values = eval_array_like_values::<T>(arg_expr, caller_env)?;
    let dims = resolved_array_input_dims(param, arg_expr, caller_env, local_env, values.len())?
        .ok_or(EvalError::UnsupportedExpression {
            kind: "function matrix input shape",
        })?;
    let expected = concrete_param_size(&dims).ok_or(EvalError::UnsupportedExpression {
        kind: "function matrix input shape",
    })?;
    if expected != values.len() {
        return Err(EvalError::ShapeMismatch {
            context: "function matrix input",
            expected,
            actual: values.len(),
        });
    }

    let selection_field = selected_component_field_in_current_call(caller_env);
    set_array_entries(local_env, &param.name, &dims, &values);
    if let Some(field) = selection_field {
        set_array_entries(
            local_env,
            &format!("{}.{field}", param.name),
            &dims,
            &values,
        );
    }
    let local_dims = std::sync::Arc::make_mut(&mut local_env.dims);
    local_dims.insert(param.name.clone(), dims.clone());
    if let Some(field) = selection_field {
        local_dims.insert(format!("{}.{field}", param.name), dims);
    }
    Ok(true)
}

pub(super) fn selected_component_field_in_current_call<T: SimFloat>(
    env: &VarEnv<T>,
) -> Option<&'static str> {
    current_function_complex_component(&env.runtime)
}

pub(super) fn copy_array_literal_input_entries<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<bool, EvalError> {
    let Expression::Array { elements, .. } = arg_expr else {
        return Ok(false);
    };

    if matches!(
        arg_expr,
        Expression::Array {
            is_matrix: true,
            ..
        }
    ) {
        copy_array_literal_matrix_entries(local_env, param, arg_expr, caller_env)
    } else {
        copy_array_literal_vector_entries(local_env, param, elements, caller_env)
    }
}

pub(super) fn is_pre_like_call_name(name: &rumoca_core::Reference) -> bool {
    let short = name.last_segment();
    short.eq_ignore_ascii_case("pre") || short.eq_ignore_ascii_case("previous")
}

pub(super) fn pre_like_array_source_name<T: SimFloat>(
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<Option<String>, EvalError> {
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
    };
    let Some(pre_arg) = pre_arg else {
        return Ok(None);
    };
    match pre_arg {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Ok(Some(name.as_str().to_string())),
        _ => try_eval_field_access_path(pre_arg, caller_env),
    }
}

pub(super) fn lookup_pre_binding<T: SimFloat>(name: &str) -> Option<T> {
    if let Some(value) = lookup_pre_value(name) {
        return Some(T::from_f64(value));
    }
    None
}

pub(super) fn collect_pre_array_values<T: SimFloat>(
    source_name: &str,
    dims: &[i64],
) -> Result<Vec<T>, EvalError> {
    let expected = concrete_param_size(dims).ok_or(EvalError::UnsupportedExpression {
        kind: "pre array input shape",
    })?;
    let mut values = Vec::with_capacity(expected);
    for flat_index in 0..expected {
        let key = dae::scalar_name_text_for_flat_index(source_name, dims, flat_index);
        let value = lookup_pre_binding::<T>(&key)
            .or_else(|| {
                (flat_index == 0)
                    .then(|| lookup_pre_binding::<T>(source_name))
                    .flatten()
            })
            .ok_or_else(|| EvalError::MissingBinding { name: key.clone() })?;
        values.push(value);
    }
    Ok(values)
}

pub(super) fn source_array_dims<T: SimFloat>(
    param: &FunctionParam,
    source_name: &str,
    caller_env: &VarEnv<T>,
) -> Result<Vec<i64>, EvalError> {
    if let Some(dims) = caller_env.dims.get(source_name).cloned() {
        return Ok(dims);
    }
    if concrete_param_size(&param.dims).is_some() {
        return Ok(param.dims.clone());
    }
    Err(EvalError::UnsupportedExpression {
        kind: "function array input argument shape",
    })
}

pub(super) fn copy_array_input_entries<T: SimFloat>(
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

    let pre_source_name = pre_like_array_source_name(arg_expr, caller_env)?;
    let use_pre_values = pre_source_name.is_some();
    let source_name = match pre_source_name {
        Some(source_name) => Some(source_name),
        None => match arg_expr {
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => Some(name.as_str().to_string()),
            _ => try_eval_field_access_path(arg_expr, caller_env)?,
        },
    };
    let Some(source_name) = source_name else {
        return bind_evaluated_array_input(local_env, param, arg_expr, caller_env);
    };

    let source_dims = source_array_dims(param, &source_name, caller_env)?;
    if concrete_param_size(&source_dims) == Some(0) {
        let dims = resolved_array_input_dims(param, arg_expr, caller_env, local_env, 0)?
            .unwrap_or(source_dims);
        // Zero-sized arrays (e.g. `Real marker[0]` record fields) have no
        // scalar entries to copy; registering the shape is the whole binding.
        std::sync::Arc::make_mut(&mut local_env.dims).insert(param_name.to_string(), dims);
        return Ok(());
    }
    let values = if use_pre_values {
        collect_pre_array_values(&source_name, &source_dims)?
    } else {
        array_values_from_env_name_generic(source_name.as_str(), caller_env)?.ok_or_else(|| {
            EvalError::MissingBinding {
                name: source_name.clone(),
            }
        })?
    };
    let dims = resolved_array_input_dims(param, arg_expr, caller_env, local_env, values.len())?
        .unwrap_or(source_dims);
    validate_array_input_dims(&dims, values.len())?;
    set_array_entries(local_env, param_name, &dims, &values);
    std::sync::Arc::make_mut(&mut local_env.dims).insert(param_name.to_string(), dims.clone());

    Ok(())
}

pub(super) fn env_array_sample<T: SimFloat>(
    env: &VarEnv<T>,
    name: &str,
    subscripts: &[usize],
) -> Option<T> {
    env.vars
        .get(&dae::format_subscript_key(name, subscripts))
        .copied()
}

pub(super) fn bind_evaluated_array_input<T: SimFloat>(
    local_env: &mut VarEnv<T>,
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let values = eval_array_like_values::<T>(arg_expr, caller_env)?;
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

pub(super) fn resolved_array_input_dims<T: SimFloat>(
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
    local_env: &VarEnv<T>,
    value_count: usize,
) -> Result<Option<Vec<i64>>, EvalError> {
    if !param.shape_expr.is_empty() {
        return infer_dynamic_array_input_dims(param, arg_expr, caller_env, local_env, value_count)
            .map(Some);
    }
    if concrete_param_size(&param.dims).is_some() {
        return Ok(Some(param.dims.clone()));
    }
    if !param.dims.is_empty() && param.dims.iter().any(|dim| *dim < 0) {
        return infer_dynamic_array_input_dims_from_declared(&param.dims, value_count).map(Some);
    }
    Ok(None)
}

pub(super) fn infer_dynamic_array_input_dims<T: SimFloat>(
    param: &FunctionParam,
    arg_expr: &Expression,
    caller_env: &VarEnv<T>,
    local_env: &VarEnv<T>,
    value_count: usize,
) -> Result<Vec<i64>, EvalError> {
    let shape_expr = &param.shape_expr;
    let mut dims = Vec::with_capacity(shape_expr.len());
    let mut dynamic_indices = Vec::new();
    for subscript in shape_expr {
        match subscript {
            Subscript::Index { value, .. } => dims.push(*value),
            // A self-referential shape (`A[:, size(A, 1)]`) is determined by
            // the argument: the param is not bound while its own dims resolve.
            Subscript::Expr { expr, .. } if expr_references_name(expr, &param.name) => {
                dynamic_indices.push(dims.len());
                dims.push(0);
            }
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
    validate_self_referential_shape_constraints(param, &dims, local_env)?;
    Ok(dims)
}

fn validate_self_referential_shape_constraints<T: SimFloat>(
    param: &FunctionParam,
    dims: &[i64],
    local_env: &VarEnv<T>,
) -> Result<(), EvalError> {
    let mut shape_env = local_env.clone();
    std::sync::Arc::make_mut(&mut shape_env.dims).insert(param.name.clone(), dims.to_vec());
    for (axis, subscript) in param.shape_expr.iter().enumerate() {
        let Subscript::Expr { expr, .. } = subscript else {
            continue;
        };
        if !expr_references_name(expr, &param.name) {
            continue;
        }
        let expected = eval_shape_expr_dim(expr, &shape_env)?;
        let actual = dims.get(axis).copied().ok_or(EvalError::ShapeMismatch {
            context: "function array input rank",
            expected: param.shape_expr.len(),
            actual: dims.len(),
        })?;
        if expected != actual {
            return Err(EvalError::ShapeMismatch {
                context: "function array input shape constraint",
                expected: usize::try_from(expected).unwrap_or(0),
                actual: usize::try_from(actual).unwrap_or(0),
            });
        }
    }
    Ok(())
}

fn expr_references_name(expr: &Expression, name: &str) -> bool {
    struct NameFinder<'a> {
        name: &'a str,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for NameFinder<'_> {
        fn visit_expression(&mut self, expr: &Expression) {
            if self.found {
                return;
            }
            if let Expression::VarRef { name, .. } = expr
                && name.as_str() == self.name
            {
                self.found = true;
                return;
            }
            self.walk_expression(expr);
        }
    }
    let mut finder = NameFinder { name, found: false };
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expr);
    finder.found
}

pub(super) fn infer_dynamic_array_input_dims_from_declared(
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

pub(super) fn fill_dynamic_array_input_dims<T: SimFloat>(
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

pub(super) fn fill_dynamic_array_input_dims_from_count(
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
    if inferred == 0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "empty dynamic function array input",
        });
    }
    dims[dynamic_indices[0]] = inferred as i64;
    Ok(())
}

pub(in crate::eval) fn infer_array_arg_dims<T: SimFloat>(
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
                .map(|element| {
                    eval_array_values::<T>(element, caller_env).map(|values| values.len())
                })
                .collect::<Result<Vec<_>, EvalError>>()?
                .into_iter()
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
        _ => {
            // General expressions (builtin calls, function calls, slices):
            // defer to the runtime shape inferencer.
            let dims = try_infer_runtime_expr_dims(arg_expr, caller_env)?;
            if dims.is_empty() {
                return Err(EvalError::UnsupportedExpression {
                    kind: "function array input argument shape",
                });
            }
            dims.into_iter()
                .map(|dim| {
                    i64::try_from(dim).map_err(|_| EvalError::UnsupportedExpression {
                        kind: "function array input argument shape",
                    })
                })
                .collect()
        }
    }
}

pub(super) fn validate_array_input_dims(dims: &[i64], value_count: usize) -> Result<(), EvalError> {
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

pub(super) fn concrete_param_size(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        usize::try_from(*dim)
            .ok()
            .and_then(|dim| acc.checked_mul(dim))
    })
}
