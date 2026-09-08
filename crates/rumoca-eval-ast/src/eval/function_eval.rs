use super::*;
use crate::function_budget::{AstFunctionWorkBudget, materialize_integer_range};

pub(super) const MAX_FUNC_EVAL_DEPTH: usize = 10;

pub(super) fn lookup_function<'a>(
    func_name: &str,
    ctx: &'a TypeCheckEvalContext,
) -> Option<&'a ClassDef> {
    ctx.functions.get(func_name)
}

fn function_declarations_are_invalid(function: &ClassDef) -> bool {
    function.components.values().any(|component| {
        let binding_is_invalid = component.binding.as_ref().is_some_and(|binding| {
            rumoca_ir_ast::expression_required_value_violation(binding).is_some()
        });
        component.has_explicit_binding != component.binding.is_some()
            || binding_is_invalid
            || component.shape_expr.iter().any(|subscript| {
                rumoca_ir_ast::declaration_subscript_required_value_violation(subscript).is_some()
            })
    })
}

fn find_func_output_name(func_def: &ClassDef) -> Option<String> {
    func_def
        .components
        .iter()
        .find(|(_, comp)| matches!(comp.causality, Causality::Output(_)))
        .map(|(name, _)| name.clone())
}

fn integral_real_to_i64(
    value: f64,
    ctx: &TypeCheckEvalContext,
    span: Span,
    context: &str,
) -> Option<i64> {
    checked_integral_real_to_i64(value, ctx, span, context)
}

fn local_has_scalar(local: &TypeCheckEvalContext, name: &str) -> bool {
    local.integers.contains_key(name)
        || local.reals.contains_key(name)
        || local.booleans.contains_key(name)
}

enum LocalScalarValue {
    Integer(i64),
    Real { value: f64, integral: Option<i64> },
    Boolean(bool),
}

struct SavedLocalScalarValue {
    integer: Option<i64>,
    real: Option<f64>,
    boolean: Option<bool>,
    span: Option<Span>,
}

fn take_local_scalar_value(local: &mut TypeCheckEvalContext, name: &str) -> SavedLocalScalarValue {
    SavedLocalScalarValue {
        integer: local.integers.remove(name),
        real: local.reals.remove(name),
        boolean: local.booleans.remove(name),
        span: local.scalar_spans.remove(name),
    }
}

fn restore_local_scalar_value(
    local: &mut TypeCheckEvalContext,
    name: &str,
    saved: SavedLocalScalarValue,
) {
    local.integers.remove(name);
    local.reals.remove(name);
    local.booleans.remove(name);
    local.scalar_spans.remove(name);
    if let Some(value) = saved.integer {
        local.integers.insert(name.to_string(), value);
    }
    if let Some(value) = saved.real {
        local.reals.insert(name.to_string(), value);
    }
    if let Some(value) = saved.boolean {
        local.booleans.insert(name.to_string(), value);
    }
    if let Some(span) = saved.span {
        local.scalar_spans.insert(name.to_string(), span);
    }
}

fn replace_local_scalar_value(
    local: &mut TypeCheckEvalContext,
    name: &str,
    value: LocalScalarValue,
    span: Span,
) {
    local.integers.remove(name);
    local.reals.remove(name);
    local.booleans.remove(name);
    local.scalar_spans.remove(name);
    match value {
        LocalScalarValue::Integer(value) => {
            local.integers.insert(name.to_string(), value);
            local.reals.insert(name.to_string(), value as f64);
            local.remember_scalar_span(name, span);
        }
        LocalScalarValue::Real { value, integral } => {
            local.reals.insert(name.to_string(), value);
            if let Some(integral) = integral {
                local.integers.insert(name.to_string(), integral);
            }
            local.remember_scalar_span(name, span);
        }
        LocalScalarValue::Boolean(value) => {
            local.booleans.insert(name.to_string(), value);
        }
    }
}

fn bind_local_scalar_value(
    local: &mut TypeCheckEvalContext,
    name: &str,
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> bool {
    let Some(value) = eval_local_scalar_value(expr, ctx, scope) else {
        return false;
    };
    replace_local_scalar_value(local, name, value, expr.span());
    true
}

fn eval_local_scalar_value(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<LocalScalarValue> {
    if let Some(value) = eval_integer_with_scope(expr, ctx, scope) {
        return Some(LocalScalarValue::Integer(value));
    }
    if let Some(value) = eval_real_with_scope(expr, ctx, scope) {
        let integral = integral_real_to_i64(value, ctx, expr.span(), "local scalar binding");
        return Some(LocalScalarValue::Real { value, integral });
    }
    eval_boolean_with_scope(expr, ctx, scope).map(LocalScalarValue::Boolean)
}

/// Build a local evaluation context for interpreting a function call (MLS §12.4).
///
/// Maps formal input parameters to actual argument values. Falls back to
/// default values when arguments are not provided.
pub(super) fn build_func_eval_context(
    func_def: &ClassDef,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<TypeCheckEvalContext> {
    // The interpreted body evaluates under the caller's call-identity
    // category; a nested call cannot escape into a more permissive policy.
    let mut local = match ctx.call_identity_policy() {
        CallIdentityPolicy::RequireResolvedIdentity => {
            TypeCheckEvalContext::for_resolved_identities()
        }
        CallIdentityPolicy::PreIdentityStructural => {
            TypeCheckEvalContext::for_pre_identity_structural()
        }
    };
    local.functions = Arc::clone(&ctx.functions);
    local.predefined_functions = ctx.predefined_functions.clone();
    local.func_eval_depth = ctx.func_eval_depth + 1;
    if local.func_eval_depth > MAX_FUNC_EVAL_DEPTH {
        return None;
    }
    local.function_work_budget = Some(
        ctx.function_work_budget
            .clone()
            .unwrap_or_else(|| Arc::new(AstFunctionWorkBudget::new())),
    );
    let inputs = crate::ast_call_binding::bind_ast_function_call(func_def, args)?;
    for input in inputs.iter().filter(|input| input.argument.is_some()) {
        if !bind_local_scalar_value(&mut local, input.name, input.argument?, ctx, scope) {
            return None;
        }
    }
    // Pass 3: fill remaining inputs from their declaration binding (MLS §12.4.1:
    // an input not supplied by the call takes its default from the declaration).
    //
    // The `start` attribute is not a default argument. MLS §4.9 makes it an
    // initial guess and the parser seeds it with the declared type's default, so
    // reading it would hand an unsupplied input a value the function never
    // declared (SPEC_0008). An input left unbound simply stays absent, and the
    // fold that needs it declines.
    for input in &inputs {
        if local_has_scalar(&local, input.name) {
            continue;
        }
        let binding = input.component.binding.as_ref()?;
        let value = eval_local_scalar_value(binding, &local, "")?;
        replace_local_scalar_value(&mut local, input.name, value, binding.span());
    }
    Some(local)
}

/// Try to evaluate a user-defined pure function returning a scalar integer (MLS §12.4).
///
/// Looks up the function definition, builds a local context with input values,
/// interprets the algorithm section, and returns the output variable's value.
pub(super) fn eval_user_func_integer(
    func_name: &str,
    args: &[Expression],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<i64> {
    if ctx.func_eval_depth >= MAX_FUNC_EVAL_DEPTH {
        return None;
    }
    let func_def = lookup_function(func_name, ctx)?;
    if func_def.class_type != ClassType::Function || !func_def.pure || func_def.external.is_some() {
        return None;
    }
    if function_declarations_are_invalid(func_def) {
        return None;
    }
    let mut local_ctx = build_func_eval_context(func_def, args, ctx, scope)?;
    let output_name = find_func_output_name(func_def)?;
    for algo in &func_def.algorithms {
        if matches!(
            interpret_stmts(algo, &mut local_ctx)?,
            FunctionStmtFlow::Return
        ) {
            break;
        }
    }
    local_ctx.integers.get(&output_name).copied().or_else(|| {
        let span = local_ctx.scalar_span(&output_name)?;
        local_ctx
            .reals
            .get(&output_name)
            .and_then(|v| integral_real_to_i64(*v, &local_ctx, span, "function return"))
    })
}

/// Interpret a sequence of algorithm statements (MLS §11.1).
pub(super) fn interpret_stmts(
    stmts: &[Statement],
    ctx: &mut TypeCheckEvalContext,
) -> Option<FunctionStmtFlow> {
    if stmts
        .iter()
        .any(|statement| rumoca_ir_ast::statement_required_value_violation(statement).is_some())
    {
        return None;
    }
    let installed_budget = ctx.function_work_budget.is_none();
    if installed_budget {
        ctx.function_work_budget = Some(Arc::new(AstFunctionWorkBudget::new()));
    }
    let result = interpret_stmts_inner(stmts, ctx, &mut Vec::new());
    if installed_budget {
        ctx.function_work_budget = None;
    }
    result
}

fn interpret_stmts_inner(
    stmts: &[Statement],
    ctx: &mut TypeCheckEvalContext,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    for stmt in stmts {
        let flow = interpret_stmt_inner(stmt, ctx, active_loop_indices)?;
        if flow != FunctionStmtFlow::Continue {
            return Some(flow);
        }
    }
    Some(FunctionStmtFlow::Continue)
}

/// Interpret a single algorithm statement for compile-time function evaluation.
///
/// Handles assignment and if-elseif-else branching. Returns None if the
/// statement cannot be interpreted (unsupported construct or evaluation failure).
fn interpret_stmt_inner(
    stmt: &Statement,
    ctx: &mut TypeCheckEvalContext,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    match stmt {
        Statement::Assignment { comp, value } => {
            let var_name = comp.to_string();
            if assignment_targets_loop_index(comp, active_loop_indices) {
                return None;
            }
            if let Some(val) = eval_integer_with_scope(value, ctx, "") {
                replace_local_scalar_value(
                    ctx,
                    &var_name,
                    LocalScalarValue::Integer(val),
                    value.span(),
                );
                return Some(FunctionStmtFlow::Continue);
            }
            if let Some(val) = eval_real_with_scope(value, ctx, "") {
                let integral = integral_real_to_i64(val, ctx, value.span(), "algorithm assignment");
                replace_local_scalar_value(
                    ctx,
                    &var_name,
                    LocalScalarValue::Real {
                        value: val,
                        integral,
                    },
                    value.span(),
                );
                return Some(FunctionStmtFlow::Continue);
            }
            if let Some(val) = eval_boolean_with_scope(value, ctx, "") {
                replace_local_scalar_value(
                    ctx,
                    &var_name,
                    LocalScalarValue::Boolean(val),
                    value.span(),
                );
                return Some(FunctionStmtFlow::Continue);
            }
            None
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => interpret_if_stmt(cond_blocks, else_block.as_deref(), ctx, active_loop_indices),
        Statement::For { indices, equations } => {
            interpret_for_stmt(indices, equations, ctx, active_loop_indices)
        }
        Statement::While(block) => interpret_while_stmt(block, ctx, active_loop_indices),
        Statement::Break { .. } => Some(FunctionStmtFlow::Break),
        Statement::Return { .. } => Some(FunctionStmtFlow::Return),
        // `Empty` is parser recovery, not an empty algorithm section.  An
        // empty section is represented by an empty statement vector, so this
        // evaluator must not turn recovery syntax into a successful fold.
        Statement::Empty => None,
        _ => None,
    }
}

fn assignment_targets_loop_index(
    comp: &rumoca_ir_ast::ComponentReference,
    active_loop_indices: &[String],
) -> bool {
    comp.parts.first().is_some_and(|root| {
        active_loop_indices
            .iter()
            .any(|index| index == root.ident.text.as_ref())
    })
}

/// Interpret an if-elseif-else statement (MLS §11.2.6).
fn interpret_if_stmt(
    cond_blocks: &[StatementBlock],
    else_block: Option<&[Statement]>,
    ctx: &mut TypeCheckEvalContext,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    for block in cond_blocks {
        match eval_boolean_with_scope(&block.cond, ctx, "") {
            Some(true) => {
                return interpret_stmts_inner(&block.stmts, ctx, active_loop_indices);
            }
            Some(false) => continue,
            None => return None,
        }
    }
    if let Some(else_stmts) = else_block {
        interpret_stmts_inner(else_stmts, ctx, active_loop_indices)
    } else {
        Some(FunctionStmtFlow::Continue)
    }
}

/// Interpret a for-loop statement (MLS §11.2.4).
fn interpret_for_stmt(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[Statement],
    ctx: &mut TypeCheckEvalContext,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    if indices.len() != 1 {
        return None;
    }
    let idx = &indices[0];
    let var_name = idx.ident.text.to_string();
    let values = eval_for_range(&idx.range, ctx)?;
    let saved = take_local_scalar_value(ctx, &var_name);
    active_loop_indices.push(var_name.clone());
    let result = (|| {
        for value in values {
            replace_local_scalar_value(
                ctx,
                &var_name,
                LocalScalarValue::Integer(value),
                idx.ident.location.span(),
            );
            match interpret_stmts_inner(equations, ctx, active_loop_indices)? {
                FunctionStmtFlow::Continue => {}
                FunctionStmtFlow::Break => return Some(FunctionStmtFlow::Continue),
                FunctionStmtFlow::Return => return Some(FunctionStmtFlow::Return),
            }
        }
        Some(FunctionStmtFlow::Continue)
    })();
    active_loop_indices.pop();
    restore_local_scalar_value(ctx, &var_name, saved);
    result
}

/// Interpret a while-loop statement (MLS §11.2.5).
fn interpret_while_stmt(
    block: &StatementBlock,
    ctx: &mut TypeCheckEvalContext,
    active_loop_indices: &mut Vec<String>,
) -> Option<FunctionStmtFlow> {
    loop {
        match eval_boolean_with_scope(&block.cond, ctx, "") {
            Some(true) => {
                if !ctx.function_work_budget.as_ref()?.try_spend(1) {
                    return None;
                }
                match interpret_stmts_inner(&block.stmts, ctx, active_loop_indices)? {
                    FunctionStmtFlow::Continue => {}
                    FunctionStmtFlow::Break => return Some(FunctionStmtFlow::Continue),
                    FunctionStmtFlow::Return => return Some(FunctionStmtFlow::Return),
                }
            }
            Some(false) => return Some(FunctionStmtFlow::Continue),
            None => return None,
        }
    }
}

/// Materialize a bounded for-loop range with its declared direction and step.
fn eval_for_range(range: &Expression, ctx: &TypeCheckEvalContext) -> Option<Vec<i64>> {
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return None;
    };
    let start = eval_integer_with_scope(start, ctx, "")?;
    let end = eval_integer_with_scope(end, ctx, "")?;
    let step = step
        .as_deref()
        .map_or(Some(1), |step| eval_integer_with_scope(step, ctx, ""))?;
    collect_integer_range(start, step, end, ctx)
}

fn collect_integer_range(
    start: i64,
    step: i64,
    end: i64,
    ctx: &TypeCheckEvalContext,
) -> Option<Vec<i64>> {
    materialize_integer_range(
        start,
        step,
        end,
        ctx.function_work_budget.as_ref()?.as_ref(),
    )
}
