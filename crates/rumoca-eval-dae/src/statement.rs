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
    let name = component_ref_to_string(comp, env)?;
    let has_explicit_subscripts = comp.parts.iter().any(|part| !part.subs.is_empty());
    if !has_explicit_subscripts && materialize_constructor_assignment(&name, value, env)? {
        return Ok(());
    }
    if !has_explicit_subscripts && materialize_record_alias_assignment(&name, value, env) {
        return Ok(());
    }
    if !has_explicit_subscripts
        && let Some(dims) = env.dims.get(name.as_str()).cloned()
        && !dims.is_empty()
    {
        let values = eval::eval_array_values(value, env);
        if values.len() > 1 {
            eval::set_array_entries(env, &name, &dims, &values);
            return Ok(());
        }
    }
    let val = eval::eval_statement_value(value, env)?;
    maybe_log_introspect_assignment(&name, val);
    env.set(&name, val);
    Ok(())
}

fn materialize_record_alias_assignment<T: SimFloat>(
    target: &str,
    value: &rumoca_core::Expression,
    env: &mut VarEnv<T>,
) -> bool {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = value
    else {
        return false;
    };
    if !subscripts.is_empty() {
        return false;
    }

    let source = name.as_str();
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
        return false;
    }

    for (field_name, field_value) in selected {
        env.set(&field_name, field_value);
    }
    if let Some(value) = env.vars.get(source).copied() {
        env.set(target, value);
    }
    copy_record_alias_dims(target, source, env);
    true
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
            let values = eval::eval_array_values(arg, env);
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
            .filter(|dim| *dim > 0)
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

    let mut assigned_outputs = 0usize;
    for (idx, target) in outputs.iter().enumerate() {
        let Some(output_name) = output_names.get(idx) else {
            break;
        };
        let Some((target_key, target_suffix)) = output_target_to_string(target, env)? else {
            maybe_log_unsupported_output_target(trace_algorithm_calls, func_name, idx, target);
            continue;
        };

        if target_suffix.is_empty() {
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
                );
                eval::set_array_entries(env, &target_key, &dims, &values);
                assigned_outputs += 1;
                continue;
            }
        }

        let value = eval::eval_selected_function_output_pub(
            &resolved_name,
            output_name,
            &target_suffix,
            args,
            env,
        );
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
) -> Vec<T> {
    let mut values = Vec::with_capacity(total);
    for i in 1..=total {
        values.push(eval::eval_selected_function_output_pub(
            resolved_name,
            output_name,
            &format!("[{i}]"),
            args,
            env,
        ));
    }
    values
}

fn eval_function_call_statement<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    args: &[rumoca_core::Expression],
    outputs: &[rumoca_core::ComponentReference],
    env: &mut VarEnv<T>,
) -> Result<(), EvalError> {
    let func_name = comp.to_var_name();
    let trace_algorithm_calls = trace_algorithm_calls_enabled();
    maybe_log_function_call_header(trace_algorithm_calls, &func_name, outputs.len());

    if apply_selected_function_outputs(&func_name, args, outputs, env, trace_algorithm_calls)? {
        return Ok(());
    }

    let result = eval::eval_function_call_pub(&func_name, args, env);
    if outputs.len() == 1
        && let Some((target_key, _)) = output_target_to_string(&outputs[0], env)?
    {
        env.set(&target_key, result);
    }
    Ok(())
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

fn subscripts_to_string<T: SimFloat>(
    subscripts: &[rumoca_core::Subscript],
    env: &VarEnv<T>,
) -> Result<Option<String>, EvalError> {
    if subscripts.is_empty() {
        return Ok(Some(String::new()));
    }
    let mut values: Vec<String> = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        match sub {
            rumoca_core::Subscript::Index { value: i, .. } => values.push(i.to_string()),
            rumoca_core::Subscript::Expr { expr, .. } => {
                values.push(eval::eval_expr::<T>(expr, env)?.real().round().to_string());
            }
            rumoca_core::Subscript::Colon { .. } => return Ok(None),
        }
    }
    Ok(Some(format!("[{}]", values.join(","))))
}

fn output_target_to_string<T: SimFloat>(
    comp: &rumoca_core::ComponentReference,
    env: &VarEnv<T>,
) -> Result<Option<(String, String)>, EvalError> {
    let Some(last) = comp.parts.last() else {
        return Ok(None);
    };
    let Some(suffix) = subscripts_to_string(&last.subs, env)? else {
        return Ok(None);
    };
    Ok(Some((component_ref_to_string(comp, env)?, suffix)))
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

    fn comp_ref(name: &str) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: rumoca_core::split_path_with_indices(name)
                .into_iter()
                .map(|ident| rumoca_core::ComponentRefPart {
                    ident: ident.to_string(),
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

    #[test]
    fn test_eval_empty_statements() {
        let mut env = VarEnv::<f64>::new();
        eval_statements(&[], &mut env).expect("empty statements should evaluate");
    }

    #[test]
    fn test_for_loop_subscript_uses_local_index() {
        let mut env = VarEnv::<f64>::new();
        env.set("n", 2.0);
        env.set("tbl[1]", 10.0);
        env.set("tbl[2]", 20.0);
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
                comp: comp_ref("y"),
                value: var("tbl[i]"),
                span: rumoca_core::Span::DUMMY,
            }],

            span: rumoca_core::Span::DUMMY,
        };

        eval_statements(&[for_stmt], &mut env).expect("for statement should evaluate");
        assert_eq!(env.get("y"), 20.0);
    }

    #[test]
    fn assignment_statement_rejects_missing_binding_instead_of_defaulting_zero() {
        let mut env = VarEnv::<f64>::new();
        env.set("y", 7.0);
        let stmt = rumoca_core::Statement::Assignment {
            comp: comp_ref("y"),
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
        assert_eq!(env.get("y"), 7.0);
    }

    #[test]
    fn record_alias_assignment_copies_selected_fields() {
        let mut env = VarEnv::<f64>::new();
        env.set("in_c.alpha", 2.0);
        env.set("in_c.beta", 3.0);
        std::sync::Arc::make_mut(&mut env.dims).insert("in_c.beta".to_string(), vec![2]);

        let stmt = rumoca_core::Statement::Assignment {
            comp: comp_ref("out_c"),
            value: var("in_c"),
            span: rumoca_core::Span::DUMMY,
        };

        eval_statements(&[stmt], &mut env).expect("record alias assignment should evaluate");

        assert_eq!(env.get("out_c.alpha"), 2.0);
        assert_eq!(env.get("out_c.beta"), 3.0);
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
                comp: comp_ref("y"),
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
        assert_eq!(env.get("y"), 0.0);
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
                    comp: comp_ref("i"),
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
        assert_eq!(env.get("i"), 3.0);
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
                        comp: comp_ref("i"),
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
        assert_eq!(env.get("i"), 3.0);
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

        let mut f = rumoca_core::Function::new("Pkg.multi", Default::default());
        f.add_input(rumoca_core::FunctionParam::new("u", "Real"));
        f.add_output(rumoca_core::FunctionParam::new("y1", "Real"));
        f.add_output(rumoca_core::FunctionParam::new("y2", "Real"));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref("y1"),
                value: var("u"),

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref("y2"),
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
                comp: comp_ref("Pkg.multi"),
                args: vec![real(2.5)],
                outputs: vec![comp_ref("out1"), comp_ref("out2")],

                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("function call statement should evaluate");

        assert_eq!(env.get("out1"), 2.5);
        assert_eq!(env.get("out2"), 5.0);
    }

    #[test]
    fn test_function_call_statement_binds_array_inputs_for_multi_output() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.pickTable", Default::default());
        f.add_input(rumoca_core::FunctionParam::new("table", "Real").with_dims(vec![2, 2]));
        f.add_output(rumoca_core::FunctionParam::new("y1", "Real"));
        f.add_output(rumoca_core::FunctionParam::new("y2", "Real"));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref("y1"),
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
                comp: comp_ref("y2"),
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

        env.set("srcTable[1,1]", 0.0);
        env.set("srcTable[1,2]", 2.1);
        env.set("srcTable[2,1]", 1.0);
        env.set("srcTable[2,2]", 4.2);
        std::sync::Arc::make_mut(&mut env.dims).insert("srcTable".to_string(), vec![2, 2]);

        eval_statements(
            &[rumoca_core::Statement::FunctionCall {
                comp: comp_ref("Pkg.pickTable"),
                args: vec![var("srcTable")],
                outputs: vec![comp_ref("out1"), comp_ref("out2")],

                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("function call statement should evaluate");

        assert_eq!(env.get("out1"), 2.1);
        assert_eq!(env.get("out2"), 4.2);
    }

    #[test]
    fn test_function_call_statement_binds_pre_array_inputs_for_multi_output() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.pickSeedTail", Default::default());
        f.add_input(rumoca_core::FunctionParam::new("seedIn", "Integer").with_dims(vec![3]));
        f.add_output(rumoca_core::FunctionParam::new("y2", "Integer"));
        f.add_output(rumoca_core::FunctionParam::new("y3", "Integer"));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref("y2"),
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
                comp: comp_ref("y3"),
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
        env.set("seedState[1]", 23.0);
        env.set("seedState[2]", 87.0);
        env.set("seedState[3]", 187.0);
        std::sync::Arc::make_mut(&mut env.dims).insert("seedState".to_string(), vec![3]);
        eval::clear_pre_values();
        eval::set_pre_value("seedState", 3933.0);
        eval::set_pre_value("seedState[1]", 3933.0);
        eval::set_pre_value("seedState[2]", 14964.0);
        eval::set_pre_value("seedState[3]", 1467.0);

        eval_statements(
            &[rumoca_core::Statement::FunctionCall {
                comp: comp_ref("Pkg.pickSeedTail"),
                args: vec![rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: vec![var("seedState")],
                    span: rumoca_core::Span::DUMMY,
                }],
                outputs: vec![comp_ref("out2"), comp_ref("out3")],

                span: rumoca_core::Span::DUMMY,
            }],
            &mut env,
        )
        .expect("function call statement should evaluate");

        assert_eq!(env.get("out2"), 14964.0);
        assert_eq!(env.get("out3"), 1467.0);
        eval::clear_pre_values();
    }

    #[test]
    fn test_function_local_array_assignment_preserves_indexed_entries() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.sumArray", Default::default());
        f.add_output(rumoca_core::FunctionParam::new("y", "Real"));
        f.add_local(rumoca_core::FunctionParam::new("x", "Real").with_dims(vec![3]));
        f.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref("x"),
                value: rumoca_core::Expression::Array {
                    elements: vec![real(1.0), real(2.0), real(3.0)],
                    is_matrix: false,
                    span: rumoca_core::Span::DUMMY,
                },

                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref("y"),
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
                    comp: comp_ref("y"),
                    value: rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Add,
                        lhs: Box::new(var("y")),
                        rhs: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::new("x"),
                            subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                                var("i"),
                            ))],
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

        let y = eval::eval_expr_or_default(
            &rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.sumArray"),
                args: vec![],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            &env,
        );
        assert_eq!(y, 6.0);
    }

    #[test]
    fn user_function_body_missing_binding_returns_nan_instead_of_zero() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = rumoca_core::Function::new("Pkg.bad", Default::default());
        f.add_output(rumoca_core::FunctionParam::new("y", "Real"));
        f.body = vec![rumoca_core::Statement::Assignment {
            comp: comp_ref("y"),
            value: var("missing"),
            span: rumoca_core::Span::DUMMY,
        }];
        functions.insert("Pkg.bad".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        let y = eval::eval_expr_or_default(
            &rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.bad"),
                args: vec![],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            &env,
        );

        assert!(y.is_nan());
    }
}
