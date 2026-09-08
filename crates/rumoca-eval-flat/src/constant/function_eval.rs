//! User-defined function evaluation for compile-time constant expressions.
//!
//! This module interprets Modelica function algorithm bodies to evaluate
//! user-defined functions with constant arguments at compile time.
//!
//! Implements compile-time constant folding of user-defined functions per MLS:
//! - Pure functions with constant arguments → constant expression (MLS §12.3)
//! - Statement-by-statement interpretation with environment tracking (MLS §12.4)
//! - For loop semantics per MLS §11.2.2.2
//! - While loop semantics per MLS §11.2.2.3
//! - If statement semantics per MLS §11.2.2.1
//! - Assignment semantics per MLS §11.2.1
//! - Recursion and iteration limits for safety

mod assignment;
mod validation;

use indexmap::{IndexMap, IndexSet};
use rumoca_core::{
    ComponentReference, ComprehensionIndex, Expression, ForIndex, Function, Literal, Span,
    Statement, StatementBlock, Subscript,
};

use rumoca_core::ExpressionVisitor;

use super::errors::EvalError;
use super::value::{
    Value, materialized_node_count, rectangular_materialized_node_count,
    rectangular_shape_is_representable,
};
use super::{DEFAULT_EVAL_BUDGET, DEFAULT_MATERIALIZED_RANK_BUDGET, EvalContext};
use assignment::{apply_subscripts_flat, assign_array_selection, subscripted_assignment_target};
use validation::validate_function_semantic_ir;

/// Execution limits for function evaluation.
#[derive(Debug, Clone)]
pub struct EvalLimits {
    /// Maximum recursion depth for nested function calls.
    pub recursion_depth: usize,
    /// Maximum total iterations across all loops.
    pub max_iterations: usize,
}

impl Default for EvalLimits {
    fn default() -> Self {
        Self {
            recursion_depth: 256,
            max_iterations: DEFAULT_EVAL_BUDGET,
        }
    }
}

/// Evaluation state bundling common parameters to reduce argument count.
#[derive(Clone, Copy)]
pub struct EvalState<'a> {
    pub ctx: &'a EvalContext,
    pub limits: &'a EvalLimits,
    pub depth: usize,
    pub span: Span,
}

/// Control flow during statement execution.
#[derive(Debug, Clone, Copy, PartialEq)]
enum FlowControl {
    /// Normal statement execution, continue to next statement.
    Continue,
    /// Break from current loop.
    Break,
    /// Return from function (output values are ready).
    Return,
}

mod environment;
#[cfg(test)]
use environment::FunctionCallArg;
use environment::FunctionEnv;

/// Evaluate a user-defined function with constant arguments.
///
/// Returns the function result or an error if evaluation fails.
#[cfg(test)]
fn eval_function(
    func: &Function,
    args: Vec<Value>,
    ctx: &EvalContext,
    limits: &EvalLimits,
    depth: usize,
    span: Span,
) -> Result<Value, EvalError> {
    let args = args.into_iter().map(FunctionCallArg::positional).collect();
    eval_function_with_call_args(func, args, ctx, limits, depth, span)
}

/// Evaluate a user-defined function with already evaluated positional/named arguments.
#[cfg(test)]
fn eval_function_with_call_args(
    func: &Function,
    args: Vec<FunctionCallArg>,
    ctx: &EvalContext,
    limits: &EvalLimits,
    depth: usize,
    span: Span,
) -> Result<Value, EvalError> {
    validate_function_entry(func, ctx, limits, depth, span)?;
    let eval = EvalState {
        ctx,
        limits,
        depth,
        span,
    };
    let env = FunctionEnv::new_with_call_args(func, args, &eval)?;
    execute_checked_function(func, env, &eval)
}

pub(in crate::constant) fn eval_function_with_checked_call(
    call: crate::constant::EvaluatedCall<'_>,
    ctx: &EvalContext,
    limits: &EvalLimits,
    depth: usize,
    span: Span,
) -> Result<Value, EvalError> {
    let plan = call.plan();
    let func = plan.function();
    validate_function_entry(func, ctx, limits, depth, span)?;
    let eval = EvalState {
        ctx,
        limits,
        depth,
        span,
    };
    let env = FunctionEnv::new_with_checked_call(&call, &eval)?;
    execute_checked_function(func, env, &eval)
}

fn validate_function_entry(
    func: &Function,
    ctx: &EvalContext,
    limits: &EvalLimits,
    depth: usize,
    span: Span,
) -> Result<(), EvalError> {
    validate_function_semantic_ir(func, ctx, span)?;
    if depth > limits.recursion_depth {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "recursion depth exceeded ({}) in function {}",
                limits.recursion_depth, func.name
            ),
            span,
        });
    }
    // Refused before any environment or result construction: a body the
    // evaluator cannot execute must never yield the zero-valued outputs an
    // empty environment would produce.
    if !func.pure {
        return Err(EvalError::not_constant(
            format!("impure function: {}", func.name),
            span,
        ));
    }
    if func.external.is_some() {
        return Err(EvalError::not_constant(
            format!("external function: {}", func.name),
            span,
        ));
    }
    Ok(())
}

fn execute_checked_function(
    func: &Function,
    mut env: FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    if func.is_constructor {
        return record_constructor_value(func, &env, eval.span);
    }
    let mut iteration_count = 0;
    let mut state = StmtState {
        env: &mut env,
        iteration_count: &mut iteration_count,
    };

    for stmt in func.body.iter() {
        let flow = eval_statement(stmt, &mut state, eval)?;
        match flow {
            FlowControl::Return => break,
            FlowControl::Break => {
                return Err(EvalError::function_error(
                    "break outside of loop".to_string(),
                    eval.span,
                ));
            }
            FlowControl::Continue => {}
        }
    }

    env.return_value(eval.span)
}

/// The record a record-constructor call folds to.
///
/// MLS 3.6 §12.6: the implicitly defined record constructor takes the record's
/// declared components as its inputs and returns an instance of the record, so
/// the call is the record built from the bound arguments in declaration order.
/// It has no algorithm to interpret and no output formal parameter, and reading
/// it as an ordinary function is what folded `Complex(re, im)` to the empty
/// tuple of its (absent) outputs.
fn record_constructor_value(
    func: &Function,
    env: &FunctionEnv,
    span: Span,
) -> Result<Value, EvalError> {
    let mut fields = IndexMap::with_capacity(func.inputs.len());
    for component in &func.inputs {
        let value = env.inputs.get(&component.name).ok_or_else(|| {
            EvalError::function_error(
                format!(
                    "record constructor {} has no value for component {}",
                    func.name, component.name
                ),
                span,
            )
        })?;
        fields.insert(component.name.clone(), value.clone());
    }
    Ok(Value::Record(fields))
}

/// Mutable state during statement evaluation.
struct StmtState<'a> {
    env: &'a mut FunctionEnv,
    iteration_count: &'a mut usize,
}

/// Evaluate a statement, returning control flow indication.
fn eval_statement(
    stmt: &Statement,
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    match stmt {
        Statement::Empty { span } => Err(EvalError::InvalidSemanticIr {
            reason: "empty statement reached constant function evaluation".to_string(),
            span: *span,
        }),
        Statement::Assignment { comp, value, .. } => eval_assignment(comp, value, state.env, eval),
        Statement::Return { .. } => Ok(FlowControl::Return),
        Statement::Break { .. } => Ok(FlowControl::Break),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => eval_if_statement(cond_blocks, else_block, state, eval),
        Statement::For {
            indices, equations, ..
        } => eval_for_statement(indices, equations, state, eval),
        Statement::While { block, .. } => eval_while_statement(block, state, eval),
        Statement::FunctionCall { .. } => eval_fn_call_stmt(stmt, state.env, eval),
        Statement::When { .. } => Err(EvalError::not_constant(
            "when statement in function",
            eval.span,
        )),
        Statement::Reinit { .. } => Err(EvalError::InvalidSemanticIr {
            reason: "reinit statement reached constant function evaluation".to_string(),
            span: eval.span,
        }),
        Statement::Assert {
            condition, message, ..
        } => eval_assert_statement(condition, message, state.env, eval),
    }
}

fn eval_assert_statement(
    condition: &Expression,
    message: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    let condition = eval_expr_in_function(condition, env, eval)?;
    let passed = condition
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", condition.type_name(), eval.span))?;
    if passed {
        return Ok(FlowControl::Continue);
    }
    let message = eval_expr_in_function(message, env, eval)?;
    Err(EvalError::function_error(
        format!("constant function assertion failed: {message:?}"),
        eval.span,
    ))
}

/// Evaluate an assignment statement.
fn eval_assignment(
    comp: &ComponentReference,
    value: &Expression,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    let val = eval_expr_in_function(value, env, eval)?;
    // MLS 3.6 §10.5 "Indexing": "The array indexing operator `name[…]` is used
    // to access array elements for retrieval of their values or for updating
    // these values." So `y[i] := e` and `y[a:b] := e` update elements of `y`;
    // the whole-value assignment below is reachable only for an unsubscripted
    // target. Dispatching on the joined name first made `orientation[1:3] := v`
    // replace the whole vector with `v`, which folded `symmetricOrientation(6)`
    // to three elements instead of six.
    assign_target_value(comp, val, env, eval)?;
    Ok(FlowControl::Continue)
}

/// Assign one already-evaluated value through the exact statement lvalue.
///
/// Ordinary assignments and call-output receivers share this path. Keeping
/// the two together prevents an indexed receiver such as `(y[2]) := f()` from
/// being flattened to the spelling `y` and replacing the entire array.
fn assign_target_value(
    target: &ComponentReference,
    value: Value,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    if let Some((base_name, subscripts)) = subscripted_assignment_target(target, eval.span)? {
        return assign_subscripted(env, &base_name, &subscripts, value, eval);
    }
    let name = component_ref_to_name(target);
    if env.set(&name, value) {
        return Ok(());
    }
    Err(EvalError::function_error(
        format!("cannot assign to variable: {name}"),
        eval.span,
    ))
}

/// Assign through a subscripted target, writing into the component's value.
fn assign_subscripted(
    env: &mut FunctionEnv,
    base_name: &str,
    subscripts: &[Subscript],
    value: Value,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    assign_array_selection(env, base_name, subscripts, value, eval)
}

/// Evaluate an if statement.
fn eval_if_statement(
    cond_blocks: &[StatementBlock],
    else_block: &Option<Vec<Statement>>,
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    for block in cond_blocks {
        let cond_val = eval_expr_in_function(&block.cond, state.env, eval)?;
        let is_true = cond_val
            .as_bool()
            .ok_or_else(|| EvalError::type_mismatch("Boolean", cond_val.type_name(), eval.span))?;
        if is_true {
            return eval_stmt_list(&block.stmts, state, eval);
        }
    }
    // All conditions false, execute else branch
    if let Some(else_stmts) = else_block {
        return eval_stmt_list(else_stmts, state, eval);
    }
    Ok(FlowControl::Continue)
}

/// Evaluate a list of statements.
fn eval_stmt_list(
    stmts: &[Statement],
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    for stmt in stmts {
        let flow = eval_statement(stmt, state, eval)?;
        if flow != FlowControl::Continue {
            return Ok(flow);
        }
    }
    Ok(FlowControl::Continue)
}

/// Evaluate a function call statement.
fn eval_fn_call_stmt(
    statement: &Statement,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    let Statement::FunctionCall {
        comp,
        args: _,
        outputs,
        ..
    } = statement
    else {
        return Err(EvalError::InvalidSemanticIr {
            reason: "function-call evaluator received a non-call statement".to_string(),
            span: eval.span,
        });
    };
    let func_name = comp.as_str();

    // Runtime operations cannot be discarded merely because their return value
    // is unused: folding the enclosing function would erase their semantics.
    match func_name {
        "assert" | "print" | "terminate" | "Modelica.Utilities.Streams.print" => {
            return Err(EvalError::UnsupportedExpression {
                kind: format!("runtime statement `{func_name}` in constant function evaluation"),
                span: eval.span,
            });
        }
        _ => {}
    }

    let target = crate::constant::resolve_context_function_occurrence(comp, eval.ctx, eval.span)?;
    let plan = crate::constant::checked_statement_call_plan(target, statement, eval.span)?;
    let result_arity = function_result_arity(plan.function());
    let call =
        plan.evaluate_explicit(|_, argument| eval_checked_call_argument(argument, env, eval))?;
    let result =
        eval_function_with_checked_call(call, eval.ctx, eval.limits, eval.depth + 1, eval.span)?;
    if !outputs.is_empty() {
        assign_fn_outputs(outputs, result, result_arity, env, eval)?;
    }
    Ok(FlowControl::Continue)
}

fn eval_checked_call_argument(
    argument: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    if !is_partial_application(argument) {
        return eval_expr_in_function(argument, env, eval);
    }
    let partial_span = argument.span().ok_or_else(|| {
        EvalError::missing_source_context(
            "function-valued statement argument has no source provenance",
        )
    })?;
    Err(EvalError::UnsupportedExpression {
        kind: "function-valued partial application".to_string(),
        span: partial_span,
    })
}

pub(super) fn function_result_arity(function: &Function) -> usize {
    if function.is_constructor {
        1
    } else {
        function.outputs.len()
    }
}

/// Assign function outputs to variables.
fn assign_fn_outputs(
    outputs: &[Option<ComponentReference>],
    result: Value,
    result_arity: usize,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    if outputs.len() > result_arity {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "call statement has {} receiver slots for {result_arity} result(s)",
                outputs.len()
            ),
            span: eval.span,
        });
    }
    if result_arity == 1 {
        if let Some(output) = outputs.first().and_then(Option::as_ref) {
            return assign_target_value(output, result, env, eval);
        }
        return Ok(());
    }
    let Value::Array(results) = result else {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "call declared {result_arity} results but evaluation produced a scalar value"
            ),
            span: eval.span,
        });
    };
    if results.len() != result_arity {
        return Err(EvalError::InvalidSemanticIr {
            reason: format!(
                "call declared {result_arity} results but evaluation produced {}",
                results.len()
            ),
            span: eval.span,
        });
    }

    // MLS §11.2.1.1 assigns receivers from left to right and permits the
    // same target more than once. Apply that ordered relation to a private
    // candidate environment, then publish it atomically only if every indexed
    // receiver succeeds. A late bounds/type failure therefore cannot leave an
    // earlier receiver written.
    let mut candidate = env.clone();
    for (output, value) in outputs.iter().zip(&results) {
        if let Some(output) = output {
            assign_target_value(output, value.clone(), &mut candidate, eval)?;
        }
    }
    *env = candidate;
    Ok(())
}

/// Evaluate a for loop.
fn eval_for_statement(
    indices: &[ForIndex],
    body: &[Statement],
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    if indices.is_empty() {
        return Ok(FlowControl::Continue);
    }
    let index = &indices[0];
    let var_name = index.ident.to_string();
    let range_values = eval_range_expr(&index.range, state.env, eval)?;
    eval_for_loop_body(&var_name, &range_values, indices, body, state, eval)
}

/// Execute the for loop iterations.
fn eval_for_loop_body(
    var_name: &str,
    range_values: &[Value],
    indices: &[ForIndex],
    body: &[Statement],
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    for val in range_values {
        check_iteration_limit(state.iteration_count, eval)?;
        let binding_depth = state.env.loop_bindings.len();
        state
            .env
            .loop_bindings
            .push((var_name.to_string(), val.clone()));

        let flow = if indices.len() > 1 {
            eval_for_statement(&indices[1..], body, state, eval)
        } else {
            eval_stmt_list(body, state, eval)
        };
        state.env.loop_bindings.truncate(binding_depth);
        let flow = flow?;

        match flow {
            FlowControl::Break => break,
            FlowControl::Return => return Ok(FlowControl::Return),
            FlowControl::Continue => {}
        }
    }
    Ok(FlowControl::Continue)
}

/// Check iteration limit and increment counter.
fn check_iteration_limit(
    iteration_count: &mut usize,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    *iteration_count += 1;
    if *iteration_count > eval.limits.max_iterations {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "iteration limit exceeded ({}) in function",
                eval.limits.max_iterations
            ),
            span: eval.span,
        });
    }
    Ok(())
}

/// Evaluate a while loop.
fn eval_while_statement(
    block: &StatementBlock,
    state: &mut StmtState<'_>,
    eval: &EvalState<'_>,
) -> Result<FlowControl, EvalError> {
    loop {
        check_iteration_limit(state.iteration_count, eval)?;

        let cond_val = eval_expr_in_function(&block.cond, state.env, eval)?;
        let is_true = cond_val
            .as_bool()
            .ok_or_else(|| EvalError::type_mismatch("Boolean", cond_val.type_name(), eval.span))?;

        if !is_true {
            break;
        }

        let flow = eval_stmt_list(&block.stmts, state, eval)?;
        match flow {
            FlowControl::Break => return Ok(FlowControl::Continue),
            FlowControl::Return => return Ok(FlowControl::Return),
            FlowControl::Continue => {}
        }
    }
    Ok(FlowControl::Continue)
}

mod expression_eval;
#[cfg(test)]
use expression_eval::eval_var_ref;
use expression_eval::{component_ref_to_name, eval_expr_in_function, eval_range_expr};
pub(super) use expression_eval::{is_exact_single_record_output, is_partial_application};

#[cfg(test)]
mod tests;
