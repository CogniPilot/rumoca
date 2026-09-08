use super::*;
use crate::function_budget::{AstFunctionWorkBudget, materialize_integer_range};

pub(super) fn interpret_function_statements(
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<FunctionStmtFlow> {
    if statements
        .iter()
        .any(|statement| ast::statement_required_value_violation(statement).is_some())
    {
        return None;
    }
    let root_budget = AstFunctionWorkBudget::new();
    let env = env.with_work_budget(&root_budget);
    interpret_function_statements_inner(statements, env, depth, locals, &mut Vec::new())
}

fn interpret_function_statements_inner(
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    for statement in statements {
        let flow =
            interpret_function_statement_inner(statement, env, depth, locals, active_loop_indices)?;
        if flow != FunctionStmtFlow::Continue {
            return Some(flow);
        }
    }
    Some(FunctionStmtFlow::Continue)
}

fn interpret_function_statement_inner(
    statement: &ast::Statement,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    match statement {
        ast::Statement::Assignment { comp, value } => {
            if assignment_targets_loop_index(comp, active_loop_indices) {
                return None;
            }
            let evaluated = eval_function_expr(value, env, depth + 1, locals)?;
            insert_local_value(&comp.to_string(), evaluated, locals);
            Some(FunctionStmtFlow::Continue)
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => interpret_function_if(
            cond_blocks,
            else_block.as_deref(),
            env,
            depth,
            locals,
            active_loop_indices,
        ),
        ast::Statement::For { indices, equations } => {
            interpret_function_for(indices, equations, env, depth, locals, active_loop_indices)
        }
        ast::Statement::While(block) => {
            interpret_function_while(block, env, depth, locals, active_loop_indices)
        }
        ast::Statement::Break { .. } => Some(FunctionStmtFlow::Break),
        ast::Statement::Return { .. } => Some(FunctionStmtFlow::Return),
        // `Empty` is parser recovery, not an empty algorithm section.  An
        // empty section is represented by an empty statement vector, so this
        // evaluator must not turn recovery syntax into a successful fold.
        ast::Statement::Empty => None,
        ast::Statement::Assert { condition, .. } => try_eval_bool_expr_with_local_values(
            condition,
            env,
            depth + 1,
            Some(&locals.ints),
            Some(&locals.bools),
        )
        .filter(|condition_holds| *condition_holds)
        .map(|_| FunctionStmtFlow::Continue),
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } if outputs.is_empty() && comp.to_string() == "assert" => args
            .first()
            .and_then(|condition| {
                try_eval_bool_expr_with_local_values(
                    condition,
                    env,
                    depth + 1,
                    Some(&locals.ints),
                    Some(&locals.bools),
                )
            })
            .filter(|condition_holds| *condition_holds)
            .map(|_| FunctionStmtFlow::Continue),
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => interpret_function_call(comp, args, outputs, env, depth, locals, active_loop_indices),
        ast::Statement::When(_) | ast::Statement::Reinit { .. } => None,
    }
}

fn assignment_targets_loop_index(
    comp: &ast::ComponentReference,
    active_loop_indices: &[String],
) -> bool {
    comp.parts.first().is_some_and(|root| {
        active_loop_indices
            .iter()
            .any(|index| index == root.ident.text.as_ref())
    })
}

fn interpret_function_if(
    cond_blocks: &[rumoca_ir_ast::StatementBlock],
    else_block: Option<&[ast::Statement]>,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    for block in cond_blocks {
        if eval_function_condition(&block.cond, env, depth + 1, locals)? {
            return interpret_function_statements_inner(
                &block.stmts,
                env,
                depth + 1,
                locals,
                active_loop_indices,
            );
        }
    }
    if let Some(else_stmts) = else_block {
        return interpret_function_statements_inner(
            else_stmts,
            env,
            depth + 1,
            locals,
            active_loop_indices,
        );
    }
    Some(FunctionStmtFlow::Continue)
}

fn interpret_function_for(
    indices: &[rumoca_ir_ast::ForIndex],
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    if indices.len() != 1 {
        return None;
    }
    let index = &indices[0];
    let loop_name = index.ident.text.to_string();
    let values = evaluate_for_index_values(&index.range, env, depth + 1, Some(&locals.ints))?;
    let saved = take_mixed_local(&loop_name, locals);
    active_loop_indices.push(loop_name.clone());
    let result = (|| {
        for value in values {
            insert_local_value(&loop_name, LocalValue::Integer(value), locals);
            match interpret_function_statements_inner(
                statements,
                env,
                depth + 1,
                locals,
                active_loop_indices,
            )? {
                FunctionStmtFlow::Continue => {}
                FunctionStmtFlow::Break => return Some(FunctionStmtFlow::Continue),
                FunctionStmtFlow::Return => return Some(FunctionStmtFlow::Return),
            }
        }
        Some(FunctionStmtFlow::Continue)
    })();
    active_loop_indices.pop();
    restore_mixed_local(&loop_name, saved, locals);
    result
}

fn interpret_function_while(
    block: &rumoca_ir_ast::StatementBlock,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    loop {
        if !eval_function_condition(&block.cond, env, depth + 1, locals)? {
            return Some(FunctionStmtFlow::Continue);
        }
        if !env.work_budget?.try_spend(1) {
            return None;
        }
        match interpret_function_statements_inner(
            &block.stmts,
            env,
            depth + 1,
            locals,
            active_loop_indices,
        )? {
            FunctionStmtFlow::Continue => {}
            FunctionStmtFlow::Break => return Some(FunctionStmtFlow::Continue),
            FunctionStmtFlow::Return => return Some(FunctionStmtFlow::Return),
        }
    }
}

fn interpret_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    outputs: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
    active_loop_indices: &[String],
) -> Option<FunctionStmtFlow> {
    let [ast::Expression::ComponentReference(output_ref)] = outputs else {
        return None;
    };
    if assignment_targets_loop_index(output_ref, active_loop_indices) {
        return None;
    }
    let value = eval_integer_function_call(comp, args, env, depth + 1, Some(&locals.ints))?;
    insert_local_value(&output_ref.to_string(), LocalValue::Integer(value), locals);
    Some(FunctionStmtFlow::Continue)
}

fn evaluate_for_index_values(
    range: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<Vec<i64>> {
    match range {
        ast::Expression::Range {
            start, step, end, ..
        } => {
            let start_value =
                try_eval_integer_expr_with_env_and_locals(start, env, depth + 1, local_ints)?;
            let end_value =
                try_eval_integer_expr_with_env_and_locals(end, env, depth + 1, local_ints)?;
            let step_value = if let Some(step_expr) = step {
                try_eval_integer_expr_with_env_and_locals(step_expr, env, depth + 1, local_ints)?
            } else {
                1
            };
            if step_value == 0 {
                return None;
            }
            collect_integer_range(start_value, step_value, end_value, env)
        }
        _ => {
            let end_value =
                try_eval_integer_expr_with_env_and_locals(range, env, depth + 1, local_ints)?;
            collect_integer_range(1, 1, end_value, env)
        }
    }
}

fn collect_integer_range(
    start: i64,
    step: i64,
    end: i64,
    env: IntegerEvalEnv<'_>,
) -> Option<Vec<i64>> {
    materialize_integer_range(start, step, end, env.work_budget?)
}
