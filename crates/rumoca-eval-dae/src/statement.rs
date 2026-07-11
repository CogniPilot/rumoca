//! Generic statement evaluator for algorithm sections.
//!
//! Evaluates algorithm statements to update the variable environment.

use crate::eval::{self, EvalError, VarEnv};
use crate::sim_float::SimFloat;

const MAX_WHILE_ITERATIONS: usize = 10_000;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum StatementFlow {
    Continue,
    Break,
    Return,
}

/// Evaluate a list of statements, updating the environment.
pub fn eval_statements<T: SimFloat>(
    stmts: &[rumoca_core::Statement],
    env: &mut VarEnv<T>,
) -> Result<(), EvalError> {
    let _ = eval_statement_list(stmts, env)?;
    Ok(())
}

fn eval_statement_list<T: SimFloat>(
    stmts: &[rumoca_core::Statement],
    env: &mut VarEnv<T>,
) -> Result<StatementFlow, EvalError> {
    for stmt in stmts {
        match eval_statement(stmt, env)? {
            StatementFlow::Continue => {}
            flow => return Ok(flow),
        }
    }
    Ok(StatementFlow::Continue)
}

fn trace_algorithm_calls_enabled() -> bool {
    crate::trace::sim_or_introspect_enabled()
}

fn maybe_log_introspect_assignment<T: SimFloat>(name: &str, value: T) {
    if crate::trace::introspect_enabled()
        && (name.contains("vIn.signalSource.T_start") || name.contains("vIn.signalSource.count"))
    {
        tracing::debug!(
            target: "rumoca_eval_dae::introspect",
            "algorithm assignment {} = {}",
            name,
            value.real()
        );
    }
}

fn eval_assignment_statement<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    value: &rumoca_core::Expression,
    env: &mut VarEnv<T>,
) -> Result<(), EvalError> {
    if assign_array_slice(comp, value, env)? {
        return Ok(());
    }
    let name = component_ref_to_string(comp, env)?;
    let has_explicit_subscripts = comp.parts.iter().any(|part| !part.subs.is_empty());
    if !has_explicit_subscripts && materialize_constructor_assignment(&name, value, env)? {
        return Ok(());
    }
    if !has_explicit_subscripts && materialize_record_alias_assignment(&name, value, env)? {
        return Ok(());
    }
    if materialize_record_function_assignment(&name, value, env)? {
        return Ok(());
    }
    if !has_explicit_subscripts
        && let Some(dims) = env.dims.get(name.as_str()).cloned()
        && !dims.is_empty()
    {
        let values = eval::eval_array_values(value, env)?;
        eval::set_array_entries(env, &name, &dims, &values);
        return Ok(());
    }
    if !has_explicit_subscripts
        && name.contains('.')
        && let Ok(inferred_dims) = eval::infer_runtime_expr_dims(value, env)
        && !inferred_dims.is_empty()
    {
        let dims = inferred_dims
            .into_iter()
            .map(|dim| {
                i64::try_from(dim).map_err(|_| EvalError::UnsupportedExpression {
                    kind: "inferred assignment dimensions",
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let values = eval::eval_array_values(value, env)?;
        std::sync::Arc::make_mut(&mut env.dims).insert(name.clone(), dims.clone());
        eval::set_array_entries(env, &name, &dims, &values);
        return Ok(());
    }
    let val = eval::eval_statement_value(value, env)?;
    maybe_log_introspect_assignment(&name, val);
    env.set(&name, val);
    Ok(())
}

fn assign_array_slice<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    value: &rumoca_core::Expression,
    env: &mut VarEnv<T>,
) -> Result<bool, EvalError> {
    let Some(target) = resolve_array_slice_target(comp, env)? else {
        return Ok(false);
    };
    let values = eval::eval_shaped_array_values(value, env, target.total)?;
    write_array_slice_values(&target, &values, env)?;
    Ok(true)
}

struct ArraySliceTarget {
    base_name: String,
    selections: Vec<Vec<usize>>,
    selection_dims: Vec<i64>,
    total: usize,
}

fn resolve_array_slice_target<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    env: &VarEnv<T>,
) -> Result<Option<ArraySliceTarget>, EvalError> {
    let Some(last) = comp.parts.last() else {
        return Ok(None);
    };
    let has_slice = last.subs.iter().any(|subscript| {
        matches!(subscript, rumoca_core::Subscript::Colon { .. })
            || matches!(subscript, rumoca_core::Subscript::Expr { expr, .. }
                if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }))
    });
    if !has_slice {
        return Ok(None);
    }
    if comp.parts[..comp.parts.len() - 1]
        .iter()
        .any(|part| !part.subs.is_empty())
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "slice assignment through an indexed aggregate",
        });
    }

    let base_name = comp
        .parts
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".");
    let dims =
        env.dims
            .get(base_name.as_str())
            .cloned()
            .ok_or_else(|| EvalError::MissingBinding {
                name: format!("{base_name} dimensions"),
            })?;
    if last.subs.len() > dims.len() {
        return Err(EvalError::ShapeMismatch {
            context: "slice assignment rank",
            expected: dims.len(),
            actual: last.subs.len(),
        });
    }

    let mut selections = Vec::with_capacity(dims.len());
    for (axis, dim) in dims.iter().enumerate() {
        let extent = usize::try_from(*dim).map_err(|_| EvalError::UnsupportedExpression {
            kind: "slice assignment dimensions",
        })?;
        let selection = match last.subs.get(axis) {
            Some(rumoca_core::Subscript::Index { value, .. }) => {
                vec![positive_slice_index(*value, extent)?]
            }
            Some(rumoca_core::Subscript::Expr { expr, .. })
                if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. }) =>
            {
                eval::eval_array_values(expr.as_ref(), env)?
                    .into_iter()
                    .map(|item| finite_slice_index(item.real(), extent))
                    .collect::<Result<Vec<_>, _>>()?
            }
            Some(rumoca_core::Subscript::Expr { expr, .. }) => {
                vec![finite_slice_index(
                    eval::eval_expr::<T>(expr, env)?.real(),
                    extent,
                )?]
            }
            Some(rumoca_core::Subscript::Colon { .. }) | None => (1..=extent).collect(),
        };
        selections.push(selection);
    }

    let selection_dims = selections
        .iter()
        .map(|selection| {
            i64::try_from(selection.len()).map_err(|_| EvalError::UnsupportedExpression {
                kind: "slice assignment dimensions",
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let total = selection_dims.iter().try_fold(1usize, |count, extent| {
        usize::try_from(*extent)
            .ok()
            .and_then(|extent| count.checked_mul(extent))
    });
    let total = total.ok_or(EvalError::UnsupportedExpression {
        kind: "slice assignment dimensions",
    })?;
    Ok(Some(ArraySliceTarget {
        base_name,
        selections,
        selection_dims,
        total,
    }))
}

fn write_array_slice_values<T: SimFloat>(
    target: &ArraySliceTarget,
    values: &[T],
    env: &mut VarEnv<T>,
) -> Result<(), EvalError> {
    if values.len() != target.total {
        return Err(EvalError::ShapeMismatch {
            context: "slice assignment value",
            expected: target.total,
            actual: values.len(),
        });
    }
    for (flat_index, item) in values.iter().enumerate() {
        let selection_index =
            rumoca_ir_dae::flat_index_to_subscripts(&target.selection_dims, flat_index).ok_or(
                EvalError::UnsupportedExpression {
                    kind: "slice assignment index",
                },
            )?;
        let actual_index = selection_index
            .iter()
            .enumerate()
            .map(|(axis, index)| target.selections[axis][index - 1])
            .collect::<Vec<_>>();
        env.set(
            &rumoca_ir_dae::format_subscript_key(target.base_name.as_str(), &actual_index),
            *item,
        );
    }
    Ok(())
}

fn positive_slice_index(value: i64, extent: usize) -> Result<usize, EvalError> {
    usize::try_from(value)
        .ok()
        .filter(|index| *index >= 1 && *index <= extent)
        .ok_or(EvalError::UnsupportedExpression {
            kind: "slice assignment index",
        })
}

fn finite_slice_index(value: f64, extent: usize) -> Result<usize, EvalError> {
    if !value.is_finite() || value.fract() != 0.0 {
        return Err(EvalError::UnsupportedExpression {
            kind: "slice assignment index",
        });
    }
    positive_slice_index(value as i64, extent)
}

fn materialize_record_function_assignment<T: SimFloat>(
    target: &str,
    value: &rumoca_core::Expression,
    env: &mut VarEnv<T>,
) -> Result<bool, EvalError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        ..
    } = value
    else {
        return Ok(false);
    };
    let Some((values, field_dims)) =
        eval::eval_user_function_record_output_pub(name.var_name(), args, env)?
    else {
        return Ok(false);
    };
    for (suffix, field_value) in values {
        env.set(&format!("{target}.{suffix}"), field_value);
    }
    let dims = std::sync::Arc::make_mut(&mut env.dims);
    for (suffix, value) in field_dims {
        dims.insert(format!("{target}.{suffix}"), value);
    }
    Ok(true)
}

fn materialize_record_alias_assignment<T: SimFloat>(
    target: &str,
    value: &rumoca_core::Expression,
    env: &mut VarEnv<T>,
) -> Result<bool, EvalError> {
    let Some(source) = eval::eval_field_access_path(value, env)? else {
        return Ok(false);
    };
    let source_prefix = format!("{source}.");
    let target_prefix = format!("{target}.");
    let selected = env
        .vars
        .iter()
        .filter_map(|(key, value)| {
            key.strip_prefix(source_prefix.as_str())
                .map(|suffix| (format!("{target_prefix}{suffix}"), *value))
        })
        .collect::<Vec<_>>();
    if selected.is_empty() {
        return Ok(false);
    }

    for (field_name, field_value) in selected {
        env.set(&field_name, field_value);
    }
    if let Some(value) = env.vars.get(source.as_str()).copied() {
        env.set(target, value);
    }
    copy_record_alias_dims(target, source.as_str(), env);
    Ok(true)
}

fn copy_record_alias_dims<T: SimFloat>(target: &str, source: &str, env: &mut VarEnv<T>) {
    let source_prefix = format!("{source}.");
    let target_prefix = format!("{target}.");
    let selected = env
        .dims
        .iter()
        .filter_map(|(key, dims)| {
            key.strip_prefix(source_prefix.as_str())
                .map(|suffix| (format!("{target_prefix}{suffix}"), dims.clone()))
        })
        .collect::<Vec<_>>();
    if selected.is_empty() {
        return;
    }
    let dims = std::sync::Arc::make_mut(&mut env.dims);
    for (field_name, field_dims) in selected {
        dims.insert(field_name, field_dims);
    }
}

fn materialize_constructor_assignment<T: SimFloat>(
    target: &str,
    value: &rumoca_core::Expression,
    env: &mut VarEnv<T>,
) -> Result<bool, EvalError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = value
    else {
        return Ok(false);
    };
    let Some(constructor) = env.functions.get(name.as_str()).cloned() else {
        return Ok(false);
    };
    if constructor.inputs.is_empty() {
        return Ok(false);
    }

    let mut positional_idx = 0usize;
    let mut wrote = false;
    for input in &constructor.inputs {
        let Some(arg) = constructor_arg_for_input(args, &input.name, &mut positional_idx) else {
            continue;
        };
        let field = format!("{target}.{}", input.name);
        if input.dims.is_empty() {
            let value = eval::eval_statement_value(arg, env)?;
            env.set(&field, value);
        } else {
            let values = eval::eval_array_values(arg, env)?;
            let expected =
                concrete_array_size(&input.dims).ok_or(EvalError::UnsupportedExpression {
                    kind: "constructor field with dynamic shape",
                })?;
            if values.len() != expected {
                return Err(EvalError::ShapeMismatch {
                    context: "constructor field array value",
                    expected,
                    actual: values.len(),
                });
            }
            std::sync::Arc::make_mut(&mut env.dims).insert(field.clone(), input.dims.clone());
            eval::set_array_entries(env, &field, &input.dims, &values);
        }
        wrote = true;
    }
    Ok(wrote)
}

fn concrete_array_size(dims: &[i64]) -> Option<usize> {
    dims.iter().try_fold(1usize, |acc, dim| {
        usize::try_from(*dim)
            .ok()
            .and_then(|dim| acc.checked_mul(dim))
    })
}

fn constructor_arg_for_input<'a>(
    args: &'a [rumoca_core::Expression],
    input_name: &str,
    positional_idx: &mut usize,
) -> Option<&'a rumoca_core::Expression> {
    if let Some(named) = args
        .iter()
        .find_map(|arg| named_constructor_arg(arg).filter(|(name, _)| *name == input_name))
        .map(|(_, value)| value)
    {
        return Some(named);
    }
    while let Some(arg) = args.get(*positional_idx) {
        *positional_idx += 1;
        if named_constructor_arg(arg).is_none() {
            return Some(arg);
        }
    }
    None
}

fn named_constructor_arg(
    expr: &rumoca_core::Expression,
) -> Option<(&str, &rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    let field = name.as_str().strip_prefix("__rumoca_named_arg__.")?;
    Some((field, args.first()?))
}

fn eval_if_statement<T: SimFloat>(
    cond_blocks: &[rumoca_core::StatementBlock],
    else_block: &Option<Vec<rumoca_core::Statement>>,
    env: &mut VarEnv<T>,
) -> Result<StatementFlow, EvalError> {
    for block in cond_blocks {
        if eval::try_eval_condition_truth(&block.cond, env)? {
            return eval_statement_list(&block.stmts, env);
        }
    }
    if let Some(else_stmts) = else_block {
        return eval_statement_list(else_stmts, env);
    }
    Ok(StatementFlow::Continue)
}

fn eval_for_statement<T: SimFloat>(
    indices: &[rumoca_core::ForIndex],
    equations: &[rumoca_core::Statement],
    env: &mut VarEnv<T>,
) -> Result<StatementFlow, EvalError> {
    if let Some(index) = indices.first() {
        let loop_var = index.ident.clone();
        let (start, end) = extract_for_range::<T>(&index.range, env)?;
        for i in start..=end {
            env.set(&loop_var, T::from_f64(i as f64));
            match eval_statement_list(equations, env)? {
                StatementFlow::Continue => {}
                StatementFlow::Break => return Ok(StatementFlow::Continue),
                StatementFlow::Return => return Ok(StatementFlow::Return),
            }
        }
    }
    Ok(StatementFlow::Continue)
}

fn eval_while_statement<T: SimFloat>(
    block: &rumoca_core::StatementBlock,
    env: &mut VarEnv<T>,
) -> Result<StatementFlow, EvalError> {
    for _ in 0..MAX_WHILE_ITERATIONS {
        if !eval::try_eval_condition_truth(&block.cond, env)? {
            return Ok(StatementFlow::Continue);
        }
        match eval_statement_list(&block.stmts, env)? {
            StatementFlow::Continue => {}
            StatementFlow::Break => return Ok(StatementFlow::Continue),
            StatementFlow::Return => return Ok(StatementFlow::Return),
        }
    }
    Err(EvalError::StatementIterationLimit {
        statement: "while",
        max_iterations: MAX_WHILE_ITERATIONS,
    })
}

fn eval_when_statement<T: SimFloat>(
    blocks: &[rumoca_core::StatementBlock],
    env: &mut VarEnv<T>,
) -> Result<StatementFlow, EvalError> {
    for block in blocks {
        if eval::try_eval_condition_truth(&block.cond, env)? {
            return eval_statement_list(&block.stmts, env);
        }
    }
    Ok(StatementFlow::Continue)
}

fn maybe_log_unsupported_output_target(
    trace_algorithm_calls: bool,
    func_name: &rumoca_core::VarName,
    idx: usize,
    target: &rumoca_core::ComponentReference,
) {
    if trace_algorithm_calls {
        tracing::debug!(
            target: "rumoca_eval_dae::sim",
            "algorithm function call '{}' output[{}] target unsupported: {:?}",
            func_name.as_str(),
            idx,
            target
        );
    }
}

fn maybe_log_timetable_assignment(trace_algorithm_calls: bool, target_key: &str, value: f64) {
    if trace_algorithm_calls && target_key.starts_with("timeTable.") {
        tracing::debug!(
            target: "rumoca_eval_dae::sim",
            "algorithm assignment {} = {}",
            target_key, value
        );
    }
}

fn maybe_log_function_call_header(
    trace_algorithm_calls: bool,
    func_name: &rumoca_core::VarName,
    outputs: usize,
) {
    if trace_algorithm_calls && outputs > 1 {
        tracing::debug!(
            target: "rumoca_eval_dae::sim",
            "algorithm function call '{}' outputs={}",
            func_name.as_str(),
            outputs
        );
    }
}

fn maybe_log_function_call_summary(
    trace_algorithm_calls: bool,
    func_name: &rumoca_core::VarName,
    outputs_len: usize,
    resolved_name: &rumoca_core::VarName,
    declared_outputs: usize,
    assigned_outputs: usize,
) {
    if trace_algorithm_calls && outputs_len > 1 {
        tracing::debug!(
            target: "rumoca_eval_dae::sim",
            "algorithm function call '{}' resolved='{}' declared_outputs={} assigned_outputs={}",
            func_name.as_str(),
            resolved_name.as_str(),
            declared_outputs,
            assigned_outputs
        );
    }
}

fn apply_materialized_function_outputs<T: SimFloat>(
    materialized: eval::MaterializedOutputs<T>,
    outputs: &[rumoca_core::ComponentReference],
    output_names: &[String],
    env: &mut VarEnv<T>,
) -> Result<(), EvalError> {
    let by_name = materialized
        .into_iter()
        .collect::<std::collections::HashMap<_, _>>();
    for (idx, target) in outputs.iter().enumerate() {
        let Some(output_name) = output_names.get(idx) else {
            break;
        };
        let Some(values) = by_name.get(output_name) else {
            return Err(EvalError::MissingBinding {
                name: output_name.clone(),
            });
        };
        if let Some(slice_target) = resolve_array_slice_target(target, env)? {
            write_array_slice_values(&slice_target, values, env)?;
            continue;
        }
        let target_key = component_ref_to_string(target, env)?;
        if values.len() == 1 {
            env.set(&target_key, values[0]);
            continue;
        }
        let dims = env.dims.get(target_key.as_str()).cloned().ok_or_else(|| {
            EvalError::MissingBinding {
                name: format!("{target_key} dimensions"),
            }
        })?;
        let expected = dims.iter().try_fold(1usize, |count, extent| {
            usize::try_from(*extent)
                .ok()
                .and_then(|extent| count.checked_mul(extent))
        });
        if expected != Some(values.len()) {
            return Err(EvalError::ShapeMismatch {
                context: "function output assignment",
                expected: expected.unwrap_or(0),
                actual: values.len(),
            });
        }
        eval::set_array_entries(env, &target_key, &dims, values);
    }
    Ok(())
}

fn apply_selected_function_outputs<T: SimFloat>(
    func_name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    outputs: &[rumoca_core::ComponentReference],
    env: &mut VarEnv<T>,
    trace_algorithm_calls: bool,
) -> Result<bool, EvalError> {
    let Some((resolved_name, output_names)) =
        eval::resolve_function_call_outputs_pub(func_name, env)
    else {
        return Ok(false);
    };

    if let Some(materialized) = eval::eval_user_function_outputs_pub(&resolved_name, args, env)? {
        apply_materialized_function_outputs(materialized, outputs, &output_names, env)?;
        return Ok(true);
    }

    let mut assigned_outputs = 0usize;
    for (idx, target) in outputs.iter().enumerate() {
        let Some(output_name) = output_names.get(idx) else {
            break;
        };
        let Some((target_key, target_indices)) = output_target_to_key_and_indices(target, env)?
        else {
            maybe_log_unsupported_output_target(trace_algorithm_calls, func_name, idx, target);
            continue;
        };

        if target_indices.is_empty() {
            let dims = match env.dims.get(target_key.as_str()) {
                Some(dims) => dims.clone(),
                None => Vec::new(),
            };
            let total = dims
                .iter()
                .try_fold(1usize, |acc, dim| match usize::try_from(*dim) {
                    Ok(dim) => acc.checked_mul(dim),
                    Err(_) => None,
                });
            if !dims.is_empty()
                && let Some(total) = total
                && total > 1
            {
                let values = eval_selected_function_output_array(
                    &resolved_name,
                    output_name,
                    args,
                    env,
                    total,
                )?;
                eval::set_array_entries(env, &target_key, &dims, &values);
                assigned_outputs += 1;
                continue;
            }
        }

        let value = eval::eval_selected_function_output_pub(
            &resolved_name,
            output_name,
            &target_indices,
            args,
            env,
        )?;
        env.set(&target_key, value);
        maybe_log_timetable_assignment(trace_algorithm_calls, &target_key, value.real());
        assigned_outputs += 1;
    }

    maybe_log_function_call_summary(
        trace_algorithm_calls,
        func_name,
        outputs.len(),
        &resolved_name,
        output_names.len(),
        assigned_outputs,
    );
    Ok(assigned_outputs > 0)
}

fn eval_selected_function_output_array<T: SimFloat>(
    resolved_name: &rumoca_core::VarName,
    output_name: &str,
    args: &[rumoca_core::Expression],
    env: &VarEnv<T>,
    total: usize,
) -> Result<Vec<T>, EvalError> {
    let mut values = Vec::with_capacity(total);
    for i in 1..=total {
        values.push(eval::eval_selected_function_output_pub(
            resolved_name,
            output_name,
            &[i64::try_from(i).map_err(|_| EvalError::ShapeMismatch {
                context: "function output array index",
                expected: i,
                actual: i64::MAX as usize,
            })?],
            args,
            env,
        )?);
    }
    Ok(values)
}

fn eval_function_call_statement<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    args: &[rumoca_core::Expression],
    outputs: &[rumoca_core::ComponentReference],
    env: &mut VarEnv<T>,
) -> Result<(), EvalError> {
    let func_name = comp.to_var_name();
    match func_name.last_segment() {
        "assert" => {
            let condition = args.first().ok_or(EvalError::UnsupportedExpression {
                kind: "assert statement without condition",
            })?;
            if eval::eval_expr::<T>(condition, env)?.to_bool() {
                return Ok(());
            }
            return Err(EvalError::AssertionFailed {
                message: statement_message(args.get(1), "assertion failed"),
            });
        }
        "terminate" => {
            return Err(EvalError::Terminated {
                message: statement_message(args.first(), "simulation terminated"),
            });
        }
        _ => {}
    }
    let trace_algorithm_calls = trace_algorithm_calls_enabled();
    maybe_log_function_call_header(trace_algorithm_calls, &func_name, outputs.len());

    if apply_selected_function_outputs(&func_name, args, outputs, env, trace_algorithm_calls)? {
        return Ok(());
    }

    let result = eval::eval_function_call_pub(&func_name, args, env)?;
    if outputs.len() == 1
        && let Some((target_key, _)) = output_target_to_key_and_indices(&outputs[0], env)?
    {
        env.set(&target_key, result);
    }
    Ok(())
}

fn statement_message(expr: Option<&rumoca_core::Expression>, fallback: &str) -> String {
    match expr {
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(message),
            ..
        }) => message.clone(),
        _ => fallback.to_string(),
    }
}

/// Evaluate a single statement.
fn eval_statement<T: SimFloat>(
    stmt: &rumoca_core::Statement,
    env: &mut VarEnv<T>,
) -> Result<StatementFlow, EvalError> {
    match stmt {
        rumoca_core::Statement::Assignment { comp, value, .. } => {
            eval_assignment_statement(comp, value, env)?;
            Ok(StatementFlow::Continue)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => eval_if_statement(cond_blocks, else_block, env),
        rumoca_core::Statement::For {
            indices, equations, ..
        } => eval_for_statement(indices, equations, env),
        rumoca_core::Statement::While { block, .. } => eval_while_statement(block, env),
        rumoca_core::Statement::When { blocks, .. } => eval_when_statement(blocks, env),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            ..
        } => {
            eval_function_call_statement(comp, args, outputs, env)?;
            Ok(StatementFlow::Continue)
        }
        rumoca_core::Statement::Reinit {
            variable, value, ..
        } => {
            let name = component_ref_to_string(variable, env)?;
            let val = eval::eval_statement_value(value, env)?;
            env.set(&name, val);
            Ok(StatementFlow::Continue)
        }
        rumoca_core::Statement::Break { .. } => Ok(StatementFlow::Break),
        rumoca_core::Statement::Return { .. } => Ok(StatementFlow::Return),
        rumoca_core::Statement::Assert { .. } | rumoca_core::Statement::Empty { .. } => {
            Ok(StatementFlow::Continue)
        }
    }
}

/// Convert a rumoca_core::ComponentReference to a dot-separated string.
fn component_ref_to_string<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    env: &VarEnv<T>,
) -> Result<String, EvalError> {
    let mut rendered = Vec::with_capacity(comp.parts.len());
    for part in &comp.parts {
        if part.subs.is_empty() {
            rendered.push(part.ident.clone());
            continue;
        }
        let subscript_text = part
            .subs
            .iter()
            .map(|sub| match sub {
                rumoca_core::Subscript::Index { value: i, .. } => Ok(i.to_string()),
                rumoca_core::Subscript::Expr { expr, .. } => {
                    Ok(eval::eval_expr::<T>(expr, env)?.real().round().to_string())
                }
                rumoca_core::Subscript::Colon { .. } => Ok(":".to_string()),
            })
            .collect::<Result<Vec<_>, EvalError>>()?
            .join(",");
        rendered.push(format!("{}[{}]", part.ident, subscript_text));
    }
    Ok(rendered.join("."))
}

fn subscripts_to_indices<T: SimFloat>(
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<Option<Vec<i64>>, EvalError> {
    if subscripts.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let mut values: Vec<i64> = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        match sub {
            rumoca_core::Subscript::Index { value: i, .. } => values.push(*i),
            rumoca_core::Subscript::Expr { expr, .. } => {
                values.push(eval::eval_expr::<T>(expr, env)?.real().round() as i64);
            }
            rumoca_core::Subscript::Colon { .. } => return Ok(None),
        }
    }
    Ok(Some(values))
}

fn output_target_to_key_and_indices<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    env: &VarEnv<T>,
) -> Result<Option<(String, Vec<i64>)>, EvalError> {
    let Some(last) = comp.parts.last() else {
        return Ok(None);
    };
    let Some(indices) = subscripts_to_indices(&last.subs, env)? else {
        return Ok(None);
    };
    Ok(Some((component_ref_to_string(comp, env)?, indices)))
}

/// Extract a for-loop range as (start, end) integers.
fn extract_for_range<T: SimFloat>(
    range: &rumoca_core::Expression,
    env: &VarEnv<T>,
) -> Result<(i64, i64), EvalError> {
    if let rumoca_core::Expression::Range { start, end, .. } = range {
        let s = eval::eval_expr::<T>(start, env)?.real() as i64;
        let e = eval::eval_expr::<T>(end, env)?.real() as i64;
        Ok((s, e))
    } else {
        let n = eval::eval_expr::<T>(range, env)?.real() as i64;
        Ok((1, n))
    }
}

/// Run all algorithm sections of a DAE, updating the environment.
pub fn eval_algorithms<T: SimFloat>(_dae: &rumoca_ir_dae::Dae, _env: &mut VarEnv<T>) {}

#[cfg(test)]
mod tests {
    use super::*;

    fn env_value<T: crate::sim_float::SimFloat>(env: &VarEnv<T>, name: &str) -> T {
        match env.require(name) {
            Ok(value) => value,
            Err(err) => panic!("test env binding should exist: {err}"),
        }
    }

    fn comp_ref(parts: &[&str]) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: parts
                .iter()
                .map(|ident| rumoca_core::ComponentRefPart {
                    ident: (*ident).to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: vec![],
                })
                .collect(),
            def_id: None,
        }
    }

    fn var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn bool_lit(value: bool) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn string_lit(value: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value.to_string()),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn call_statement(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Statement {
        rumoca_core::Statement::FunctionCall {
            comp: comp_ref(&[name]),
            args,
            outputs: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_eval_empty_statements() {
        let mut env = VarEnv::<f64>::new();
        eval_statements(&[], &mut env).expect("empty statements should evaluate");
    }

    #[test]
    fn true_assert_statement_allows_function_evaluation_to_continue() {
        let mut env = VarEnv::<f64>::new();
        eval_statements(
            &[call_statement(
                "assert",
                vec![bool_lit(true), string_lit("must not fail")],
            )],
            &mut env,
        )
        .expect("a satisfied Modelica assert is a no-op");
    }

    #[test]
    fn false_assert_statement_reports_its_modelica_message() {
        let mut env = VarEnv::<f64>::new();
        let err = eval_statements(
            &[call_statement(
                "assert",
                vec![bool_lit(false), string_lit("radius must be positive")],
            )],
            &mut env,
        )
        .expect_err("a failed Modelica assert must stop function evaluation");
        assert_eq!(
            err,
            EvalError::AssertionFailed {
                message: "radius must be positive".to_string()
            }
        );
    }

    #[test]
    fn assignment_infers_nested_record_array_shape_from_expression() {
        let mut env = VarEnv::<f64>::new();
        let rhs = rumoca_core::Expression::If {
            branches: vec![(
                bool_lit(true),
                rumoca_core::Expression::Array {
                    elements: vec![real(1.0), real(2.0), real(3.0)],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::Array {
                elements: vec![real(4.0), real(5.0), real(6.0)],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        eval_statements(
            &[rumoca_core::Statement::Assignment {
                comp: comp_ref(&["result", "pose", "position"]),
                value: rhs,
                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("record-array assignments should infer their rank and extent");

        assert_eq!(env.dims.get("result.pose.position"), Some(&vec![3]));
        assert_eq!(env_value(&env, "result.pose.position[1]"), 1.0);
        assert_eq!(env_value(&env, "result.pose.position[3]"), 3.0);
    }

    #[test]
    fn test_for_loop_subscript_uses_local_index() {
        let mut env = VarEnv::<f64>::new();
        env.set("n", 2.0);
        eval::set_array_entries(&mut env, "tbl", &[2], &[10.0, 20.0]);
        std::sync::Arc::make_mut(&mut env.dims).insert("tbl".to_string(), vec![2]);
        env.set("y", 0.0);

        let for_stmt = rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(real(1.0)),
                    step: None,
                    end: Box::new(var("n")),
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y"]),
                value: rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("tbl"),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(
                        Box::new(var("i")),
                        rumoca_core::Span::DUMMY,
                    )],
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],

            span: rumoca_core::Span::DUMMY,
        };

        eval_statements(&[for_stmt], &mut env).expect("for statement should evaluate");
        assert_eq!(env_value(&env, "y"), 20.0);
    }

    #[test]
    fn assignment_statement_rejects_missing_binding_instead_of_defaulting_zero() {
        let mut env = VarEnv::<f64>::new();
        env.set("y", 7.0);
        let stmt = rumoca_core::Statement::Assignment {
            comp: comp_ref(&["y"]),
            value: var("missing"),
            span: rumoca_core::Span::DUMMY,
        };

        let err = eval_statements(&[stmt], &mut env).expect_err("missing binding should fail");

        assert_eq!(
            err,
            EvalError::MissingBinding {
                name: "missing".to_string()
            }
        );
        assert_eq!(env_value(&env, "y"), 7.0);
    }

    #[test]
    fn record_alias_assignment_copies_selected_fields() {
        let mut env = VarEnv::<f64>::new();
        env.set("in_c.alpha", 2.0);
        env.set("in_c.beta", 3.0);
        std::sync::Arc::make_mut(&mut env.dims).insert("in_c.beta".to_string(), vec![2]);

        let stmt = rumoca_core::Statement::Assignment {
            comp: comp_ref(&["out_c"]),
            value: var("in_c"),
            span: rumoca_core::Span::DUMMY,
        };

        eval_statements(&[stmt], &mut env).expect("record alias assignment should evaluate");

        assert_eq!(env_value(&env, "out_c.alpha"), 2.0);
        assert_eq!(env_value(&env, "out_c.beta"), 3.0);
        assert_eq!(env.dims.get("out_c.beta"), Some(&vec![2]));
    }

    #[test]
    fn for_loop_range_rejects_missing_bound_instead_of_defaulting_zero() {
        let mut env = VarEnv::<f64>::new();
        let for_stmt = rumoca_core::Statement::For {
            indices: vec![rumoca_core::ForIndex {
                ident: "i".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(real(1.0)),
                    step: None,
                    end: Box::new(var("missing")),
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            equations: vec![rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y"]),
                value: real(1.0),
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        };

        let err = eval_statements(&[for_stmt], &mut env).expect_err("missing bound should fail");

        assert_eq!(
            err,
            EvalError::MissingBinding {
                name: "missing".to_string()
            }
        );
        assert!(env.get_optional("y").is_none());
    }

    #[test]
    fn while_loop_runs_until_condition_false() {
        let mut env = VarEnv::<f64>::new();
        env.set("i", 0.0);

        let while_stmt = rumoca_core::Statement::While {
            block: rumoca_core::StatementBlock {
                cond: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Lt,
                    lhs: Box::new(var("i")),
                    rhs: Box::new(real(3.0)),
                    span: rumoca_core::Span::DUMMY,
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: comp_ref(&["i"]),
                    value: rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Add,
                        lhs: Box::new(var("i")),
                        rhs: Box::new(real(1.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    span: rumoca_core::Span::DUMMY,
                }],
            },
            span: rumoca_core::Span::DUMMY,
        };

        eval_statements(&[while_stmt], &mut env).expect("finite while should evaluate");
        assert_eq!(env_value(&env, "i"), 3.0);
    }

    #[test]
    fn break_exits_while_loop() {
        let mut env = VarEnv::<f64>::new();
        env.set("i", 0.0);

        let while_stmt = rumoca_core::Statement::While {
            block: rumoca_core::StatementBlock {
                cond: bool_lit(true),
                stmts: vec![
                    rumoca_core::Statement::Assignment {
                        comp: comp_ref(&["i"]),
                        value: rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var("i")),
                            rhs: Box::new(real(1.0)),
                            span: rumoca_core::Span::DUMMY,
                        },
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Statement::If {
                        cond_blocks: vec![rumoca_core::StatementBlock {
                            cond: rumoca_core::Expression::Binary {
                                op: rumoca_core::OpBinary::Ge,
                                lhs: Box::new(var("i")),
                                rhs: Box::new(real(3.0)),
                                span: rumoca_core::Span::DUMMY,
                            },
                            stmts: vec![rumoca_core::Statement::Break {
                                span: rumoca_core::Span::DUMMY,
                            }],
                        }],
                        else_block: None,
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
            },
            span: rumoca_core::Span::DUMMY,
        };

        eval_statements(&[while_stmt], &mut env).expect("break should terminate while");
        assert_eq!(env_value(&env, "i"), 3.0);
    }

    #[test]
    fn while_loop_iteration_limit_is_error() {
        let mut env = VarEnv::<f64>::new();
        let while_stmt = rumoca_core::Statement::While {
            block: rumoca_core::StatementBlock {
                cond: bool_lit(true),
                stmts: vec![],
            },
            span: rumoca_core::Span::DUMMY,
        };

        let err = eval_statements(&[while_stmt], &mut env)
            .expect_err("nonterminating while must report an error");

        assert_eq!(
            err,
            EvalError::StatementIterationLimit {
                statement: "while",
                max_iterations: MAX_WHILE_ITERATIONS,
            }
        );
    }

    #[test]
    fn test_function_call_statement_assigns_multiple_outputs() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.multi", rumoca_core::Span::DUMMY);
        f.add_input(rumoca_core::FunctionParam::new(
            "u",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_output(rumoca_core::FunctionParam::new(
            "y1",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_output(rumoca_core::FunctionParam::new(
            "y2",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y1"]),
                value: var("u"),

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y2"]),
                value: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs: Box::new(var("u")),
                    rhs: Box::new(real(2.0)),
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
        ];
        functions.insert("Pkg.multi".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        env.set("out1", 0.0);
        env.set("out2", 0.0);

        eval_statements(
            &[rumoca_core::Statement::FunctionCall {
                comp: comp_ref(&["Pkg", "multi"]),
                args: vec![real(2.5)],
                outputs: vec![comp_ref(&["out1"]), comp_ref(&["out2"])],

                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("function call statement should evaluate");

        assert_eq!(env_value(&env, "out1"), 2.5);
        assert_eq!(env_value(&env, "out2"), 5.0);
    }

    #[test]
    fn function_call_statement_assigns_array_output_to_slice() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.vectorPair", rumoca_core::Span::DUMMY);
        f.add_input(rumoca_core::FunctionParam::new(
            "u",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_output(
            rumoca_core::FunctionParam::new(
                "values",
                "Real",
                rumoca_core::Span::source_free_serde_default(),
            )
            .with_dims(vec![2]),
        );
        f.add_output(rumoca_core::FunctionParam::new(
            "accepted",
            "Boolean",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["values"]),
                value: rumoca_core::Expression::Array {
                    elements: vec![
                        var("u"),
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var("u")),
                            rhs: Box::new(real(1.0)),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["accepted"]),
                value: bool_lit(true),
                span: rumoca_core::Span::DUMMY,
            },
        ];
        functions.insert("Pkg.vectorPair".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        eval::set_array_entries(&mut env, "matrix", &[3, 2], &[0.0; 6]);
        std::sync::Arc::make_mut(&mut env.dims).insert("matrix".to_string(), vec![3, 2]);
        let mut slice = comp_ref(&["matrix"]);
        slice.parts[0].subs = vec![
            rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            rumoca_core::Subscript::colon(rumoca_core::Span::DUMMY),
        ];

        eval_statements(
            &[rumoca_core::Statement::FunctionCall {
                comp: comp_ref(&["Pkg", "vectorPair"]),
                args: vec![real(5.0)],
                outputs: vec![slice, comp_ref(&["accepted"])],
                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("array function output should assign to a matrix row slice");

        assert_eq!(env_value(&env, "matrix[2,1]"), 5.0);
        assert_eq!(env_value(&env, "matrix[2,2]"), 6.0);
        assert_eq!(env_value(&env, "matrix[1,1]"), 0.0);
        assert_eq!(env_value(&env, "accepted"), 1.0);
    }

    #[test]
    fn test_function_call_statement_binds_array_inputs_for_multi_output() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.pickTable", rumoca_core::Span::DUMMY);
        f.add_input(
            rumoca_core::FunctionParam::new(
                "table",
                "Real",
                rumoca_core::Span::source_free_serde_default(),
            )
            .with_dims(vec![2, 2]),
        );
        f.add_output(rumoca_core::FunctionParam::new(
            "y1",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_output(rumoca_core::FunctionParam::new(
            "y2",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y1"]),
                value: rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("table"),
                    subscripts: vec![
                        rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
                        rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                    ],
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y2"]),
                value: rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("table"),
                    subscripts: vec![
                        rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                        rumoca_core::Subscript::generated_index(2, rumoca_core::Span::DUMMY),
                    ],
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
        ];
        functions.insert("Pkg.pickTable".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        eval::set_array_entries(&mut env, "srcTable", &[2, 2], &[0.0, 2.1, 1.0, 4.2]);
        std::sync::Arc::make_mut(&mut env.dims).insert("srcTable".to_string(), vec![2, 2]);

        eval_statements(
            &[rumoca_core::Statement::FunctionCall {
                comp: comp_ref(&["Pkg", "pickTable"]),
                args: vec![var("srcTable")],
                outputs: vec![comp_ref(&["out1"]), comp_ref(&["out2"])],

                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("function call statement should evaluate");

        assert_eq!(env_value(&env, "out1"), 2.1);
        assert_eq!(env_value(&env, "out2"), 4.2);
    }

    #[test]
    fn test_function_call_statement_binds_pre_array_inputs_for_multi_output() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.pickSeedTail", rumoca_core::Span::DUMMY);
        f.add_input(
            rumoca_core::FunctionParam::new(
                "seedIn",
                "Integer",
                rumoca_core::Span::source_free_serde_default(),
            )
            .with_dims(vec![3]),
        );
        f.add_output(rumoca_core::FunctionParam::new(
            "y2",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_output(rumoca_core::FunctionParam::new(
            "y3",
            "Integer",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y2"]),
                value: rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("seedIn"),
                    subscripts: vec![rumoca_core::Subscript::generated_index(
                        2,
                        rumoca_core::Span::DUMMY,
                    )],
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y3"]),
                value: rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("seedIn"),
                    subscripts: vec![rumoca_core::Subscript::generated_index(
                        3,
                        rumoca_core::Span::DUMMY,
                    )],
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
        ];
        functions.insert("Pkg.pickSeedTail".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        env.set("seedState", 23.0);
        eval::set_array_entries(&mut env, "seedState", &[3], &[23.0, 87.0, 187.0]);
        std::sync::Arc::make_mut(&mut env.dims).insert("seedState".to_string(), vec![3]);
        eval::clear_pre_values();
        eval::set_pre_value("seedState", 3933.0);
        eval::set_pre_value("seedState[1]", 3933.0);
        eval::set_pre_value("seedState[2]", 14964.0);
        eval::set_pre_value("seedState[3]", 1467.0);

        eval_statements(
            &[rumoca_core::Statement::FunctionCall {
                comp: comp_ref(&["Pkg", "pickSeedTail"]),
                args: vec![rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: vec![var("seedState")],
                    span: rumoca_core::Span::DUMMY,
                }],
                outputs: vec![comp_ref(&["out2"]), comp_ref(&["out3"])],

                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("function call statement should evaluate");

        assert_eq!(env_value(&env, "out2"), 14964.0);
        assert_eq!(env_value(&env, "out3"), 1467.0);
        eval::clear_pre_values();
    }

    #[test]
    fn test_function_local_array_assignment_preserves_indexed_entries() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.sumArray", rumoca_core::Span::DUMMY);
        f.add_output(rumoca_core::FunctionParam::new(
            "y",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_local(
            rumoca_core::FunctionParam::new(
                "x",
                "Real",
                rumoca_core::Span::source_free_serde_default(),
            )
            .with_dims(vec![3]),
        );
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["x"]),
                value: rumoca_core::Expression::Array {
                    elements: vec![real(1.0), real(2.0), real(3.0)],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["y"]),
                value: real(0.0),

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(real(1.0)),
                        step: None,
                        end: Box::new(rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::Reference::new("size"),
                            args: vec![var("x"), real(1.0)],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                }],
                equations: vec![rumoca_core::Statement::Assignment {
                    comp: comp_ref(&["y"]),
                    value: rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Add,
                        lhs: Box::new(var("y")),
                        rhs: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::new("x"),
                            subscripts: vec![rumoca_core::Subscript::generated_expr(
                                Box::new(var("i")),
                                rumoca_core::Span::DUMMY,
                            )],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                    span: rumoca_core::Span::DUMMY,
                }],

                span: rumoca_core::Span::DUMMY,
            },
        ];
        functions.insert("Pkg.sumArray".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        let y = eval::eval_expr(
            &rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.sumArray"),
                args: vec![],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            &env,
        )
        .unwrap();
        assert_eq!(y, 6.0);
    }

    #[test]
    fn repro_noelse_scalar_if_does_not_run_unconditionally() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        // function f(a) -> o:  x := a; if x < 0 then x := -x; end if; o := x;
        let mut f = rumoca_core::Function::new("Pkg.f", rumoca_core::Span::DUMMY);
        f.add_input(rumoca_core::FunctionParam::new(
            "a",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_output(rumoca_core::FunctionParam::new(
            "o",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.add_local(rumoca_core::FunctionParam::new(
            "x",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["x"]),
                value: var("a"),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::If {
                cond_blocks: vec![rumoca_core::StatementBlock {
                    cond: rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Lt,
                        lhs: Box::new(var("x")),
                        rhs: Box::new(real(0.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    stmts: vec![rumoca_core::Statement::Assignment {
                        comp: comp_ref(&["x"]),
                        value: rumoca_core::Expression::Unary {
                            op: rumoca_core::OpUnary::Minus,
                            rhs: Box::new(var("x")),
                            span: rumoca_core::Span::DUMMY,
                        },
                        span: rumoca_core::Span::DUMMY,
                    }],
                }],
                else_block: None,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref(&["o"]),
                value: var("x"),
                span: rumoca_core::Span::DUMMY,
            },
        ];
        functions.insert("Pkg.f".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        let y = eval::eval_expr(
            &rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![real(0.7)],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            &env,
        )
        .unwrap();
        assert_eq!(y, 0.7, "no-else if must not run when condition is false");

        // Same call but with the Dual (AD) float type, as used by sim/inspect.
        let mut denv = VarEnv::<crate::dual::Dual>::new();
        denv.functions = env.functions.clone();
        let dy = eval::eval_expr(
            &rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.f"),
                args: vec![real(0.7)],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            &denv,
        )
        .unwrap();
        assert_eq!(
            dy.re, 0.7,
            "no-else if (Dual) must not run when cond is false"
        );
    }

    #[test]
    fn user_function_body_missing_binding_returns_error_instead_of_defaulting() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.bad", rumoca_core::Span::DUMMY);
        f.add_output(rumoca_core::FunctionParam::new(
            "y",
            "Real",
            rumoca_core::Span::source_free_serde_default(),
        ));
        f.body = vec![rumoca_core::Statement::Assignment {
            comp: comp_ref(&["y"]),
            value: var("missing"),
            span: rumoca_core::Span::DUMMY,
        }];
        functions.insert("Pkg.bad".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        let result = eval::eval_expr::<f64>(
            &rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.bad"),
                args: vec![],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            &env,
        );

        assert_eq!(
            result,
            Err(EvalError::MissingBinding {
                name: "missing".to_string()
            })
        );
    }
}
