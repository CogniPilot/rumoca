use crate::runtime::assignment::canonical_var_ref_key;
use rumoca_eval_runtime::eval::{VarEnv, eval_expr};
use rumoca_eval_runtime::sim_float::SimFloat;
use rumoca_ir_dae as dae;
use std::collections::{HashMap, HashSet};

struct ClockInferenceContext<'a, FClockName, FClockEdge, FSampleActive>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
    sources: &'a HashMap<String, &'a dae::Expression>,
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
}

fn infer_clock_active_next<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    infer_clock_active_from_expression(ctx, expr, remaining_depth.saturating_sub(1), visiting)
}

fn infer_clock_active_from_function_call<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    short: &str,
    args: &[dae::Expression],
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if (ctx.is_clock_function_name)(short) {
        if let Some(value) = (ctx.eval_clock_edge_assignment)(ctx.dae, expr, ctx.env) {
            return Some(value != 0.0);
        }
        if rumoca_eval_runtime::eval::infer_clock_timing_seconds(expr, ctx.env).is_some() {
            return Some(eval_expr::<f64>(expr, ctx.env).to_bool());
        }
        if matches!(short, "shiftSample" | "backSample")
            && let Some(source_expr) = args.first()
        {
            return infer_clock_active_next(ctx, source_expr, remaining_depth, visiting);
        }
    }

    if matches!(short, "previous" | "hold")
        && let Some(source_expr) = args.first()
    {
        return infer_clock_active_next(ctx, source_expr, remaining_depth, visiting);
    }
    None
}

fn infer_clock_active_from_builtin_call<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    function: &dae::BuiltinFunction,
    args: &[dae::Expression],
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    match function {
        dae::BuiltinFunction::Sample if args.len() >= 2 => {
            Some((ctx.eval_sample_clock_active)(ctx.dae, &args[1], ctx.env))
        }
        dae::BuiltinFunction::Pre if !args.is_empty() => {
            infer_clock_active_next(ctx, &args[0], remaining_depth, visiting)
        }
        _ => None,
    }
}

fn infer_clock_active_from_var_ref<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if !subscripts.is_empty() {
        return None;
    }
    let key = canonical_var_ref_key(name, subscripts)?;
    if !visiting.insert(key.clone()) {
        return None;
    }
    let inferred = ctx
        .sources
        .get(key.as_str())
        .copied()
        .and_then(|rhs| infer_clock_active_next(ctx, rhs, remaining_depth, visiting));
    visiting.remove(&key);
    inferred
}

fn infer_clock_active_from_if_expression<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    for (condition, value) in branches {
        if eval_expr::<f64>(condition, ctx.env).to_bool() {
            return infer_clock_active_next(ctx, value, remaining_depth, visiting);
        }
    }
    infer_clock_active_next(ctx, else_branch, remaining_depth, visiting)
}

fn infer_clock_active_from_expression<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if remaining_depth == 0 {
        return None;
    }

    match expr {
        dae::Expression::FunctionCall { name, args, .. } => infer_clock_active_from_function_call(
            ctx,
            expr,
            name.as_str().rsplit('.').next().unwrap_or(name.as_str()),
            args,
            remaining_depth,
            visiting,
        ),
        dae::Expression::BuiltinCall { function, args } => {
            infer_clock_active_from_builtin_call(ctx, function, args, remaining_depth, visiting)
        }
        dae::Expression::VarRef { name, subscripts } => {
            infer_clock_active_from_var_ref(ctx, name, subscripts, remaining_depth, visiting)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => infer_clock_active_from_if_expression(
            ctx,
            branches,
            else_branch,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Binary { lhs, rhs, .. } => {
            infer_clock_active_next(ctx, lhs, remaining_depth, visiting)
                .or_else(|| infer_clock_active_next(ctx, rhs, remaining_depth, visiting))
        }
        dae::Expression::Unary { rhs, .. }
        | dae::Expression::FieldAccess { base: rhs, .. }
        | dae::Expression::Index { base: rhs, .. } => {
            infer_clock_active_next(ctx, rhs, remaining_depth, visiting)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .find_map(|element| infer_clock_active_next(ctx, element, remaining_depth, visiting)),
        dae::Expression::Range { start, step, end } => {
            infer_clock_active_next(ctx, start, remaining_depth, visiting)
                .or_else(|| {
                    step.as_deref().and_then(|value| {
                        infer_clock_active_next(ctx, value, remaining_depth, visiting)
                    })
                })
                .or_else(|| infer_clock_active_next(ctx, end, remaining_depth, visiting))
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => infer_clock_active_next(ctx, expr, remaining_depth, visiting)
            .or_else(|| {
                indices.iter().find_map(|index| {
                    infer_clock_active_next(ctx, &index.range, remaining_depth, visiting)
                })
            })
            .or_else(|| {
                filter.as_deref().and_then(|value| {
                    infer_clock_active_next(ctx, value, remaining_depth, visiting)
                })
            }),
        dae::Expression::Literal(_) | dae::Expression::Empty => None,
    }
}

fn expression_requires_clock_inference(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(short, "shiftSample" | "backSample" | "previous" | "hold")
                || args.iter().any(expression_requires_clock_inference)
        }
        dae::Expression::BuiltinCall { function, args } => {
            *function == dae::BuiltinFunction::Pre
                || args.iter().any(expression_requires_clock_inference)
        }
        dae::Expression::VarRef { .. } => true,
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expression_requires_clock_inference(condition)
                    || expression_requires_clock_inference(value)
            }) || expression_requires_clock_inference(else_branch)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expression_requires_clock_inference(lhs) || expression_requires_clock_inference(rhs)
        }
        dae::Expression::Unary { rhs, .. }
        | dae::Expression::FieldAccess { base: rhs, .. }
        | dae::Expression::Index { base: rhs, .. } => expression_requires_clock_inference(rhs),
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expression_requires_clock_inference)
        }
        dae::Expression::Range { start, step, end } => {
            expression_requires_clock_inference(start)
                || step
                    .as_ref()
                    .is_some_and(|value| expression_requires_clock_inference(value.as_ref()))
                || expression_requires_clock_inference(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_requires_clock_inference(expr)
                || indices
                    .iter()
                    .any(|index| expression_requires_clock_inference(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|value| expression_requires_clock_inference(value.as_ref()))
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

struct ClockEventContext<'a, FClockName, FClockEdge, FSampleActive>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
}

fn expression_has_active_clock_event<FClockName, FClockEdge, FSampleActive>(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    let ctx = ClockEventContext {
        dae,
        env,
        is_clock_function_name,
        eval_clock_edge_assignment,
        eval_sample_clock_active,
    };
    expression_has_active_clock_event_in_context(&ctx, expr)
}

fn function_call_has_active_clock_event<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockEventContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    name: &dae::VarName,
    args: &[dae::Expression],
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short == "Clock" && args.is_empty() {
        return false;
    }
    if (ctx.is_clock_function_name)(short) {
        if (ctx.eval_clock_edge_assignment)(ctx.dae, expr, ctx.env)
            .is_some_and(|value| value != 0.0)
        {
            return true;
        }
        if rumoca_eval_runtime::eval::infer_clock_timing_seconds(expr, ctx.env)
            .is_some_and(|_| eval_expr::<f64>(expr, ctx.env).to_bool())
        {
            return true;
        }
    }
    args.iter()
        .any(|arg| expression_has_active_clock_event_in_context(ctx, arg))
}

fn builtin_call_has_active_clock_event<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockEventContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    function: &dae::BuiltinFunction,
    args: &[dae::Expression],
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if *function == dae::BuiltinFunction::Sample && args.len() >= 2 {
        let clock_arg = &args[1];
        let has_clock_expr = matches!(clock_arg, dae::Expression::VarRef { .. })
            || rumoca_eval_runtime::eval::infer_clock_timing_seconds(clock_arg, ctx.env).is_some();
        if has_clock_expr {
            return (ctx.eval_sample_clock_active)(ctx.dae, clock_arg, ctx.env);
        }
        if args.len() == 2 {
            return eval_expr::<f64>(expr, ctx.env).to_bool();
        }
    }
    args.iter()
        .any(|arg| expression_has_active_clock_event_in_context(ctx, arg))
}

fn expression_has_active_clock_event_in_context<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockEventContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            function_call_has_active_clock_event(ctx, expr, name, args)
        }
        dae::Expression::BuiltinCall { function, args } => {
            builtin_call_has_active_clock_event(ctx, expr, function, args)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expression_has_active_clock_event_in_context(ctx, lhs)
                || expression_has_active_clock_event_in_context(ctx, rhs)
        }
        dae::Expression::Unary { rhs, .. } | dae::Expression::FieldAccess { base: rhs, .. } => {
            expression_has_active_clock_event_in_context(ctx, rhs)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_has_active_clock_event_in_context(ctx, cond)
                    || expression_has_active_clock_event_in_context(ctx, value)
            }) || expression_has_active_clock_event_in_context(ctx, else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expression_has_active_clock_event_in_context(ctx, element)),
        dae::Expression::Range { start, step, end } => {
            expression_has_active_clock_event_in_context(ctx, start)
                || step
                    .as_deref()
                    .is_some_and(|value| expression_has_active_clock_event_in_context(ctx, value))
                || expression_has_active_clock_event_in_context(ctx, end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_has_active_clock_event_in_context(ctx, expr)
                || indices
                    .iter()
                    .any(|index| expression_has_active_clock_event_in_context(ctx, &index.range))
                || filter
                    .as_deref()
                    .is_some_and(|value| expression_has_active_clock_event_in_context(ctx, value))
        }
        dae::Expression::Index { base, subscripts } => {
            expression_has_active_clock_event_in_context(ctx, base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => {
                        expression_has_active_clock_event_in_context(ctx, value)
                    }
                    _ => false,
                })
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

pub fn discrete_clock_event_active_from_sources<'a, FClockName, FClockEdge, FSampleActive>(
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
    sources: &HashMap<String, &'a dae::Expression>,
    active_solutions: &[&'a dae::Expression],
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    let ctx = ClockInferenceContext {
        dae,
        env,
        sources,
        is_clock_function_name,
        eval_clock_edge_assignment,
        eval_sample_clock_active,
    };
    active_solutions.iter().any(|solution| {
        if expression_has_active_clock_event(
            dae,
            solution,
            env,
            is_clock_function_name,
            eval_clock_edge_assignment,
            eval_sample_clock_active,
        ) {
            return true;
        }
        if !expression_requires_clock_inference(solution) {
            return false;
        }
        infer_clock_active_from_expression(&ctx, solution, 16, &mut HashSet::new())
            .is_some_and(|active| active)
    })
}
