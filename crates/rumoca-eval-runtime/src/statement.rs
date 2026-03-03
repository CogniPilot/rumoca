//! Generic statement evaluator for algorithm sections.
//!
//! Evaluates flattened algorithm statements to update the variable environment.

use rumoca_ir_flat as flat;

use crate::eval::{self, VarEnv};
use crate::sim_float::SimFloat;

/// Evaluate a list of statements, updating the environment.
pub fn eval_statements<T: SimFloat>(stmts: &[flat::Statement], env: &mut VarEnv<T>) {
    for stmt in stmts {
        eval_statement(stmt, env);
    }
}

fn trace_algorithm_calls_enabled() -> bool {
    std::env::var("RUMOCA_SIM_TRACE").is_ok() || std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

fn maybe_log_introspect_assignment<T: SimFloat>(name: &str, value: T) {
    if std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
        && (name.contains("vIn.signalSource.T_start") || name.contains("vIn.signalSource.count"))
    {
        eprintln!(
            "[sim-introspect] algorithm assignment {} = {}",
            name,
            value.real()
        );
    }
}

fn eval_assignment_statement<T: SimFloat>(
    comp: &flat::ComponentReference,
    value: &flat::Expression,
    env: &mut VarEnv<T>,
) {
    let name = component_ref_to_string(comp, env);
    let has_explicit_subscripts = comp.parts.iter().any(|part| !part.subs.is_empty());
    if !has_explicit_subscripts
        && let Some(dims) = env.dims.get(name.as_str()).cloned()
        && !dims.is_empty()
    {
        let values = eval::eval_array_values(value, env);
        if values.len() > 1 {
            eval::set_array_entries(env, &name, &dims, &values);
            return;
        }
    }
    let val = eval::eval_expr(value, env);
    maybe_log_introspect_assignment(&name, val);
    env.set(&name, val);
}

fn eval_if_statement<T: SimFloat>(
    cond_blocks: &[flat::StatementBlock],
    else_block: &Option<Vec<flat::Statement>>,
    env: &mut VarEnv<T>,
) {
    for block in cond_blocks {
        if eval::eval_expr(&block.cond, env).to_bool() {
            eval_statements(&block.stmts, env);
            return;
        }
    }
    if let Some(else_stmts) = else_block {
        eval_statements(else_stmts, env);
    }
}

fn eval_for_statement<T: SimFloat>(
    indices: &[flat::ForIndex],
    equations: &[flat::Statement],
    env: &mut VarEnv<T>,
) {
    if let Some(index) = indices.first() {
        let loop_var = index.ident.clone();
        let (start, end) = extract_for_range::<T>(&index.range, env);
        for i in start..=end {
            env.set(&loop_var, T::from_f64(i as f64));
            eval_statements(equations, env);
        }
    }
}

fn eval_while_statement<T: SimFloat>(block: &flat::StatementBlock, env: &mut VarEnv<T>) {
    let max_iterations = 10_000;
    for _ in 0..max_iterations {
        if !eval::eval_expr(&block.cond, env).to_bool() {
            break;
        }
        eval_statements(&block.stmts, env);
    }
}

fn eval_when_statement<T: SimFloat>(blocks: &[flat::StatementBlock], env: &mut VarEnv<T>) {
    for block in blocks {
        if eval::eval_expr(&block.cond, env).to_bool() {
            eval_statements(&block.stmts, env);
            return;
        }
    }
}

fn projected_output_name(resolved_name: &flat::VarName, output_name: &str, suffix: &str) -> String {
    if suffix.is_empty() {
        return format!("{}.{}", resolved_name.as_str(), output_name);
    }
    format!("{}.{}{}", resolved_name.as_str(), output_name, suffix)
}

fn maybe_log_unsupported_output_target(
    trace_algorithm_calls: bool,
    func_name: &flat::VarName,
    idx: usize,
    target_expr: &flat::Expression,
) {
    if trace_algorithm_calls {
        eprintln!(
            "[sim-trace] algorithm function call '{}' output[{}] target unsupported: {:?}",
            func_name.as_str(),
            idx,
            target_expr
        );
    }
}

fn maybe_log_timetable_assignment(trace_algorithm_calls: bool, target_key: &str, value: f64) {
    if trace_algorithm_calls && target_key.starts_with("timeTable.") {
        eprintln!(
            "[sim-trace] algorithm assignment {} = {}",
            target_key, value
        );
    }
}

fn maybe_log_function_call_header(
    trace_algorithm_calls: bool,
    func_name: &flat::VarName,
    outputs: usize,
) {
    if trace_algorithm_calls && outputs > 1 {
        eprintln!(
            "[sim-trace] algorithm function call '{}' outputs={}",
            func_name.as_str(),
            outputs
        );
    }
}

fn maybe_log_function_call_summary(
    trace_algorithm_calls: bool,
    func_name: &flat::VarName,
    outputs_len: usize,
    resolved_name: &flat::VarName,
    declared_outputs: usize,
    assigned_outputs: usize,
) {
    if trace_algorithm_calls && outputs_len > 1 {
        eprintln!(
            "[sim-trace] algorithm function call '{}' resolved='{}' declared_outputs={} assigned_outputs={}",
            func_name.as_str(),
            resolved_name.as_str(),
            declared_outputs,
            assigned_outputs
        );
    }
}

fn apply_projected_function_outputs<T: SimFloat>(
    func_name: &flat::VarName,
    args: &[flat::Expression],
    outputs: &[flat::Expression],
    env: &mut VarEnv<T>,
    trace_algorithm_calls: bool,
) -> bool {
    let Some((resolved_name, output_names)) =
        eval::resolve_function_call_outputs_pub(func_name, env)
    else {
        return false;
    };

    let mut assigned_outputs = 0usize;
    for (idx, target_expr) in outputs.iter().enumerate() {
        let Some(output_name) = output_names.get(idx) else {
            break;
        };
        let Some((target_key, target_suffix)) = output_target_to_string(target_expr, env) else {
            maybe_log_unsupported_output_target(trace_algorithm_calls, func_name, idx, target_expr);
            continue;
        };

        let projected = projected_output_name(&resolved_name, output_name, &target_suffix);
        let value = eval::eval_function_call_pub(&flat::VarName::new(projected), args, env);
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
    assigned_outputs > 0
}

fn eval_function_call_statement<T: SimFloat>(
    comp: &flat::ComponentReference,
    args: &[flat::Expression],
    outputs: &[flat::Expression],
    env: &mut VarEnv<T>,
) {
    let func_name = comp.to_var_name();
    let trace_algorithm_calls = trace_algorithm_calls_enabled();
    maybe_log_function_call_header(trace_algorithm_calls, &func_name, outputs.len());

    if apply_projected_function_outputs(&func_name, args, outputs, env, trace_algorithm_calls) {
        return;
    }

    let result = eval::eval_function_call_pub(&func_name, args, env);
    if outputs.len() == 1
        && let Some((target_key, _)) = output_target_to_string(&outputs[0], env)
    {
        env.set(&target_key, result);
    }
}

/// Evaluate a single statement.
fn eval_statement<T: SimFloat>(stmt: &flat::Statement, env: &mut VarEnv<T>) {
    match stmt {
        flat::Statement::Assignment { comp, value } => eval_assignment_statement(comp, value, env),
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => eval_if_statement(cond_blocks, else_block, env),
        flat::Statement::For { indices, equations } => eval_for_statement(indices, equations, env),
        flat::Statement::While(block) => eval_while_statement(block, env),
        flat::Statement::When(blocks) => eval_when_statement(blocks, env),
        flat::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => eval_function_call_statement(comp, args, outputs, env),
        flat::Statement::Reinit { variable, value } => {
            let name = component_ref_to_string(variable, env);
            let val = eval::eval_expr(value, env);
            env.set(&name, val);
        }
        flat::Statement::Assert { .. }
        | flat::Statement::Return
        | flat::Statement::Break
        | flat::Statement::Empty => {}
    }
}

/// Convert a flat::ComponentReference to a dot-separated string.
fn component_ref_to_string<T: SimFloat>(
    comp: &flat::ComponentReference,
    env: &VarEnv<T>,
) -> String {
    comp.parts
        .iter()
        .map(|part| {
            if part.subs.is_empty() {
                return part.ident.clone();
            }
            let subscript_text = part
                .subs
                .iter()
                .map(|sub| match sub {
                    flat::Subscript::Index(i) => i.to_string(),
                    flat::Subscript::Expr(expr) => {
                        eval::eval_expr::<T>(expr, env).real().round().to_string()
                    }
                    flat::Subscript::Colon => ":".to_string(),
                })
                .collect::<Vec<_>>()
                .join(",");
            format!("{}[{}]", part.ident, subscript_text)
        })
        .collect::<Vec<_>>()
        .join(".")
}

fn subscripts_to_string<T: SimFloat>(
    subscripts: &[flat::Subscript],
    env: &VarEnv<T>,
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(String::new());
    }
    let mut values: Vec<String> = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        match sub {
            flat::Subscript::Index(i) => values.push(i.to_string()),
            flat::Subscript::Expr(expr) => {
                values.push(eval::eval_expr::<T>(expr, env).real().round().to_string());
            }
            flat::Subscript::Colon => return None,
        }
    }
    Some(format!("[{}]", values.join(",")))
}

fn output_target_to_string<T: SimFloat>(
    expr: &flat::Expression,
    env: &VarEnv<T>,
) -> Option<(String, String)> {
    match expr {
        flat::Expression::VarRef { name, subscripts } => {
            let suffix = subscripts_to_string(subscripts, env)?;
            if suffix.is_empty() {
                Some((name.as_str().to_string(), suffix))
            } else {
                Some((format!("{}{}", name.as_str(), suffix), suffix))
            }
        }
        flat::Expression::FieldAccess { base, field } => {
            let (base_name, _) = output_target_to_string(base, env)?;
            Some((format!("{base_name}.{field}"), String::new()))
        }
        _ => None,
    }
}

/// Extract a for-loop range as (start, end) integers.
fn extract_for_range<T: SimFloat>(range: &flat::Expression, env: &VarEnv<T>) -> (i64, i64) {
    if let flat::Expression::Range { start, end, .. } = range {
        let s = eval::eval_expr::<T>(start, env).real() as i64;
        let e = eval::eval_expr::<T>(end, env).real() as i64;
        (s, e)
    } else {
        let n = eval::eval_expr::<T>(range, env).real() as i64;
        (1, n)
    }
}

/// Run all algorithm sections of a DAE, updating the environment.
pub fn eval_algorithms<T: SimFloat>(_dae: &rumoca_ir_dae::Dae, _env: &mut VarEnv<T>) {}

#[cfg(test)]
mod tests {
    use super::*;

    fn comp_ref(name: &str) -> flat::ComponentReference {
        flat::ComponentReference {
            local: false,
            parts: name
                .split('.')
                .map(|ident| flat::ComponentRefPart {
                    ident: ident.to_string(),
                    subs: vec![],
                })
                .collect(),
            def_id: None,
        }
    }

    fn var(name: &str) -> flat::Expression {
        flat::Expression::VarRef {
            name: flat::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn real(value: f64) -> flat::Expression {
        flat::Expression::Literal(flat::Literal::Real(value))
    }

    #[test]
    fn test_eval_empty_statements() {
        let mut env = VarEnv::<f64>::new();
        eval_statements(&[], &mut env);
    }

    #[test]
    fn test_for_loop_subscript_uses_local_index() {
        let mut env = VarEnv::<f64>::new();
        env.set("n", 2.0);
        env.set("tbl[1]", 10.0);
        env.set("tbl[2]", 20.0);
        env.set("y", 0.0);

        let for_stmt = flat::Statement::For {
            indices: vec![flat::ForIndex {
                ident: "i".to_string(),
                range: flat::Expression::Range {
                    start: Box::new(real(1.0)),
                    step: None,
                    end: Box::new(var("n")),
                },
            }],
            equations: vec![flat::Statement::Assignment {
                comp: comp_ref("y"),
                value: var("tbl[i]"),
            }],
        };

        eval_statements(&[for_stmt], &mut env);
        assert_eq!(env.get("y"), 20.0);
    }

    #[test]
    fn test_function_call_statement_assigns_multiple_outputs() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = flat::Function::new("Pkg.multi", Default::default());
        f.add_input(flat::FunctionParam::new("u", "Real"));
        f.add_output(flat::FunctionParam::new("y1", "Real"));
        f.add_output(flat::FunctionParam::new("y2", "Real"));
        f.body = vec![
            flat::Statement::Assignment {
                comp: comp_ref("y1"),
                value: var("u"),
            },
            flat::Statement::Assignment {
                comp: comp_ref("y2"),
                value: flat::Expression::Binary {
                    op: flat::OpBinary::Mul(Default::default()),
                    lhs: Box::new(var("u")),
                    rhs: Box::new(real(2.0)),
                },
            },
        ];
        functions.insert("Pkg.multi".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        env.set("out1", 0.0);
        env.set("out2", 0.0);

        eval_statements(
            &[flat::Statement::FunctionCall {
                comp: comp_ref("Pkg.multi"),
                args: vec![real(2.5)],
                outputs: vec![var("out1"), var("out2")],
            }],
            &mut env,
        );

        assert_eq!(env.get("out1"), 2.5);
        assert_eq!(env.get("out2"), 5.0);
    }

    #[test]
    fn test_function_call_statement_binds_array_inputs_for_multi_output() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = flat::Function::new("Pkg.pickTable", Default::default());
        f.add_input(flat::FunctionParam::new("table", "Real").with_dims(vec![2, 2]));
        f.add_output(flat::FunctionParam::new("y1", "Real"));
        f.add_output(flat::FunctionParam::new("y2", "Real"));
        f.body = vec![
            flat::Statement::Assignment {
                comp: comp_ref("y1"),
                value: flat::Expression::VarRef {
                    name: flat::VarName::new("table"),
                    subscripts: vec![flat::Subscript::Index(1), flat::Subscript::Index(2)],
                },
            },
            flat::Statement::Assignment {
                comp: comp_ref("y2"),
                value: flat::Expression::VarRef {
                    name: flat::VarName::new("table"),
                    subscripts: vec![flat::Subscript::Index(2), flat::Subscript::Index(2)],
                },
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
            &[flat::Statement::FunctionCall {
                comp: comp_ref("Pkg.pickTable"),
                args: vec![var("srcTable")],
                outputs: vec![var("out1"), var("out2")],
            }],
            &mut env,
        );

        assert_eq!(env.get("out1"), 2.1);
        assert_eq!(env.get("out2"), 4.2);
    }

    #[test]
    fn test_function_call_statement_binds_pre_array_inputs_for_multi_output() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = flat::Function::new("Pkg.pickSeedTail", Default::default());
        f.add_input(flat::FunctionParam::new("seedIn", "Integer").with_dims(vec![3]));
        f.add_output(flat::FunctionParam::new("y2", "Integer"));
        f.add_output(flat::FunctionParam::new("y3", "Integer"));
        f.body = vec![
            flat::Statement::Assignment {
                comp: comp_ref("y2"),
                value: flat::Expression::VarRef {
                    name: flat::VarName::new("seedIn"),
                    subscripts: vec![flat::Subscript::Index(2)],
                },
            },
            flat::Statement::Assignment {
                comp: comp_ref("y3"),
                value: flat::Expression::VarRef {
                    name: flat::VarName::new("seedIn"),
                    subscripts: vec![flat::Subscript::Index(3)],
                },
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
            &[flat::Statement::FunctionCall {
                comp: comp_ref("Pkg.pickSeedTail"),
                args: vec![flat::Expression::BuiltinCall {
                    function: flat::BuiltinFunction::Pre,
                    args: vec![var("seedState")],
                }],
                outputs: vec![var("out2"), var("out3")],
            }],
            &mut env,
        );

        assert_eq!(env.get("out2"), 14964.0);
        assert_eq!(env.get("out3"), 1467.0);
        eval::clear_pre_values();
    }

    #[test]
    fn test_function_local_array_assignment_preserves_indexed_entries() {
        let mut env = VarEnv::<f64>::new();
        let mut functions = indexmap::IndexMap::new();

        let mut f = flat::Function::new("Pkg.sumArray", Default::default());
        f.add_output(flat::FunctionParam::new("y", "Real"));
        f.add_local(flat::FunctionParam::new("x", "Real").with_dims(vec![3]));
        f.body = vec![
            flat::Statement::Assignment {
                comp: comp_ref("x"),
                value: flat::Expression::Array {
                    elements: vec![real(1.0), real(2.0), real(3.0)],
                    is_matrix: false,
                },
            },
            flat::Statement::Assignment {
                comp: comp_ref("y"),
                value: real(0.0),
            },
            flat::Statement::For {
                indices: vec![flat::ForIndex {
                    ident: "i".to_string(),
                    range: flat::Expression::Range {
                        start: Box::new(real(1.0)),
                        step: None,
                        end: Box::new(flat::Expression::FunctionCall {
                            name: flat::VarName::new("size"),
                            args: vec![var("x"), real(1.0)],
                            is_constructor: false,
                        }),
                    },
                }],
                equations: vec![flat::Statement::Assignment {
                    comp: comp_ref("y"),
                    value: flat::Expression::Binary {
                        op: flat::OpBinary::Add(Default::default()),
                        lhs: Box::new(var("y")),
                        rhs: Box::new(flat::Expression::VarRef {
                            name: flat::VarName::new("x"),
                            subscripts: vec![flat::Subscript::Expr(Box::new(var("i")))],
                        }),
                    },
                }],
            },
        ];
        functions.insert("Pkg.sumArray".to_string(), f);
        env.functions = std::sync::Arc::new(functions);

        let y = eval::eval_expr(
            &flat::Expression::FunctionCall {
                name: flat::VarName::new("Pkg.sumArray"),
                args: vec![],
                is_constructor: false,
            },
            &env,
        );
        assert_eq!(y, 6.0);
    }
}
