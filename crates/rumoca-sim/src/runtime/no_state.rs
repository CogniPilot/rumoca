use std::collections::{HashMap, HashSet};

use rumoca_eval_dae::runtime as eval;
use rumoca_ir_core::OpBinary;
use rumoca_ir_dae as dae;
use rumoca_phase_solve::EliminationResult;

use crate::runtime::event::build_runtime_state_env;
use crate::{reconstruct, timeline};

/// Shared inputs for solver-agnostic no-state runtime sampling.
pub struct NoStateSampleContext<'a> {
    pub dae: &'a dae::Dae,
    pub elim: &'a EliminationResult,
    pub param_values: &'a [f64],
    pub all_names: &'a [String],
    pub clock_event_times: &'a [f64],
    pub(crate) direct_assignment_ctx:
        &'a crate::runtime::assignment::RuntimeDirectAssignmentContext,
    pub(crate) alias_ctx: &'a crate::runtime::alias::RuntimeAliasPropagationContext,
    pub needs_eliminated_env: bool,
    pub dynamic_time_event_names: &'a [String],
    pub solver_name_to_idx: &'a HashMap<String, usize>,
    pub n_x: usize,
    pub t_start: f64,
    pub requires_projection: bool,
    pub projection_needs_event_refresh: bool,
    pub requires_live_pre_values: bool,
}

/// Errors from no-state runtime sampling.
#[derive(Debug)]
pub enum NoStateSampleError<E> {
    Callback(E),
    SampleScheduleMismatch { captured: usize, expected: usize },
}

/// No-state sampling output tuple: final solver vector + per-output channel samples.
pub type NoStateSampleData = (Vec<f64>, Vec<Vec<f64>>);

type NoStateSampleDataWithTimes = (Vec<f64>, Vec<f64>, Vec<Vec<f64>>);

/// Result type for no-state runtime sampling.
pub type NoStateSampleResult<E> = Result<NoStateSampleData, NoStateSampleError<E>>;

struct ObservationSchedules<'a> {
    evaluation_schedule: &'a mut Vec<f64>,
    output_times: &'a mut Vec<f64>,
}

struct NoStateEvalPoint {
    t: f64,
    matched_event_t: Option<f64>,
    dynamic_event_time: bool,
    event_time: bool,
}

struct NoStateSettleOptions {
    advance_pre_between_samples: bool,
    refresh_discrete_between_samples: bool,
}

fn observation_schedules<'a>(
    evaluation_schedule: &'a mut Vec<f64>,
    output_times: &'a mut Vec<f64>,
) -> ObservationSchedules<'a> {
    ObservationSchedules {
        evaluation_schedule,
        output_times,
    }
}

fn matched_scheduled_clock_event_time(ctx: &NoStateSampleContext<'_>, t: f64) -> Option<f64> {
    ctx.clock_event_times
        .iter()
        .copied()
        .find(|event_t| timeline::sample_time_match_with_tol(*event_t, t))
}

fn matched_dynamic_time_event_time(
    ctx: &NoStateSampleContext<'_>,
    t: f64,
    carried_env: Option<&eval::VarEnv<f64>>,
) -> Option<f64> {
    ctx.dynamic_time_event_names.iter().find_map(|name| {
        carried_env
            .and_then(|env| env.vars.get(name).copied())
            .filter(|event_t| timeline::sample_time_match_with_tol(*event_t, t))
            .or_else(|| {
                rumoca_eval_dae::runtime::get_pre_value(name)
                    .filter(|event_t| timeline::sample_time_match_with_tol(*event_t, t))
            })
    })
}

fn dynamic_time_threshold_exprs<'a>(
    ctx: &'a NoStateSampleContext<'a>,
) -> impl Iterator<Item = &'a dae::Expression> + 'a {
    let partition_thresholds = ctx
        .dae
        .f_z
        .iter()
        .chain(ctx.dae.f_m.iter())
        .chain(ctx.dae.f_c.iter())
        .map(|eq| &eq.rhs)
        .filter(|expr| {
            expr_uses_explicit_event_operator(expr) && expr_has_direct_time_event_threshold(expr)
        });
    let synthetic_root_thresholds = ctx
        .dae
        .synthetic_root_conditions
        .iter()
        .filter(|expr| expr_has_direct_time_event_threshold(expr));
    partition_thresholds.chain(synthetic_root_thresholds)
}

fn matched_direct_time_event_time(
    ctx: &NoStateSampleContext<'_>,
    t: f64,
    carried_env: Option<&eval::VarEnv<f64>>,
) -> Option<f64> {
    let env = carried_env?;
    let mut thresholds = Vec::new();
    for expr in dynamic_time_threshold_exprs(ctx) {
        collect_direct_time_event_thresholds_from_expr(expr, env, &mut thresholds);
    }
    thresholds
        .into_iter()
        .find(|event_t| timeline::sample_time_match_with_tol(*event_t, t))
}

fn matched_no_state_event_time(
    ctx: &NoStateSampleContext<'_>,
    t: f64,
    carried_env: Option<&eval::VarEnv<f64>>,
) -> Option<f64> {
    if timeline::sample_time_match_with_tol(t, ctx.t_start) {
        return Some(ctx.t_start);
    }
    matched_scheduled_clock_event_time(ctx, t)
        .or_else(|| matched_dynamic_time_event_time(ctx, t, carried_env))
        .or_else(|| matched_direct_time_event_time(ctx, t, carried_env))
}

fn should_advance_pre_values(
    ctx: &NoStateSampleContext<'_>,
    t: f64,
    carried_env: Option<&eval::VarEnv<f64>>,
) -> bool {
    // MLS Appendix B / §8.6: dynamic time events are driven by the current
    // event-entry discrete state. If the global pre-store has already been
    // advanced past a pending table event, keep honoring the carried runtime
    // env's current next-event slots so the no-state loop still takes the
    // required full settle at that time instant.
    matched_no_state_event_time(ctx, t, carried_env).is_some()
}

fn no_state_sample_is_initial(ctx: &NoStateSampleContext<'_>, t: f64) -> bool {
    timeline::sample_time_match_with_tol(t, ctx.t_start)
}

fn event_time_guard_name(expr: &dae::Expression) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            Some(name.to_string())
        }
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args,
        } if args.len() == 1 => match &args[0] {
            dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
                Some(name.to_string())
            }
            _ => None,
        },
        _ => None,
    }
}

fn comparison_uses_time_and_event_var(
    lhs: &dae::Expression,
    rhs: &dae::Expression,
) -> Option<String> {
    match lhs {
        dae::Expression::VarRef { name, subscripts }
            if name.as_str() == "time" && subscripts.is_empty() =>
        {
            event_time_guard_name(rhs)
        }
        _ => None,
    }
}

fn expr_is_time_var(expr: &dae::Expression) -> bool {
    matches!(
        expr,
        dae::Expression::VarRef { name, subscripts }
            if name.as_str() == "time" && subscripts.is_empty()
    )
}

fn eval_time_event_threshold(expr: &dae::Expression, env: &eval::VarEnv<f64>) -> Option<f64> {
    crate::runtime::scalar_eval::eval_scalar_expr_fast(expr, env)
        .or_else(|| Some(rumoca_eval_dae::runtime::eval_expr::<f64>(expr, env)))
        .filter(|value| value.is_finite())
}

fn expr_has_direct_time_event_threshold(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::Binary {
            op: OpBinary::Ge(_) | OpBinary::Gt(_) | OpBinary::Le(_) | OpBinary::Lt(_),
            lhs,
            rhs,
        } => {
            expr_is_time_var(lhs)
                || expr_is_time_var(rhs)
                || expr_has_direct_time_event_threshold(lhs)
                || expr_has_direct_time_event_threshold(rhs)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_has_direct_time_event_threshold(lhs) || expr_has_direct_time_event_threshold(rhs)
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_has_direct_time_event_threshold)
        }
        dae::Expression::Unary { rhs, .. } => expr_has_direct_time_event_threshold(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_direct_time_event_threshold(condition)
                    || expr_has_direct_time_event_threshold(value)
            }) || expr_has_direct_time_event_threshold(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_has_direct_time_event_threshold)
        }
        dae::Expression::Range { start, step, end } => {
            expr_has_direct_time_event_threshold(start)
                || step
                    .as_deref()
                    .is_some_and(expr_has_direct_time_event_threshold)
                || expr_has_direct_time_event_threshold(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_has_direct_time_event_threshold(expr)
                || indices
                    .iter()
                    .any(|index| expr_has_direct_time_event_threshold(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_has_direct_time_event_threshold)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_has_direct_time_event_threshold(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_has_direct_time_event_threshold(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_has_direct_time_event_threshold(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn expr_uses_explicit_event_operator(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
                    | dae::BuiltinFunction::Reinit
                    | dae::BuiltinFunction::Initial
            ) || args.iter().any(expr_uses_explicit_event_operator)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "previous"
                    | "hold"
                    | "Clock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "firstTick"
                    | "noClock"
                    | "interval"
            ) || args.iter().any(expr_uses_explicit_event_operator)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_explicit_event_operator(lhs) || expr_uses_explicit_event_operator(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_uses_explicit_event_operator(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_uses_explicit_event_operator(condition)
                    || expr_uses_explicit_event_operator(value)
            }) || expr_uses_explicit_event_operator(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_explicit_event_operator)
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_explicit_event_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_uses_explicit_event_operator)
                || expr_uses_explicit_event_operator(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_explicit_event_operator(expr)
                || indices
                    .iter()
                    .any(|index| expr_uses_explicit_event_operator(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_uses_explicit_event_operator)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_explicit_event_operator(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_uses_explicit_event_operator(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_uses_explicit_event_operator(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn collect_direct_time_event_thresholds_from_expr(
    expr: &dae::Expression,
    env: &eval::VarEnv<f64>,
    thresholds: &mut Vec<f64>,
) {
    match expr {
        dae::Expression::Binary {
            op: OpBinary::Ge(_) | OpBinary::Gt(_) | OpBinary::Le(_) | OpBinary::Lt(_),
            lhs,
            rhs,
        } => {
            if expr_is_time_var(lhs)
                && let Some(event_t) = eval_time_event_threshold(rhs, env)
            {
                thresholds.push(event_t);
            }
            if expr_is_time_var(rhs)
                && let Some(event_t) = eval_time_event_threshold(lhs, env)
            {
                thresholds.push(event_t);
            }
            collect_direct_time_event_thresholds_from_expr(lhs, env, thresholds);
            collect_direct_time_event_thresholds_from_expr(rhs, env, thresholds);
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_direct_time_event_thresholds_from_expr(lhs, env, thresholds);
            collect_direct_time_event_thresholds_from_expr(rhs, env, thresholds);
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_direct_time_event_thresholds_from_expr(arg, env, thresholds);
            }
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_direct_time_event_thresholds_from_expr(rhs, env, thresholds);
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                collect_direct_time_event_thresholds_from_expr(condition, env, thresholds);
                collect_direct_time_event_thresholds_from_expr(value, env, thresholds);
            }
            collect_direct_time_event_thresholds_from_expr(else_branch, env, thresholds);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_direct_time_event_thresholds_from_expr(element, env, thresholds);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_direct_time_event_thresholds_from_expr(start, env, thresholds);
            if let Some(step) = step.as_deref() {
                collect_direct_time_event_thresholds_from_expr(step, env, thresholds);
            }
            collect_direct_time_event_thresholds_from_expr(end, env, thresholds);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_direct_time_event_thresholds_from_expr(expr, env, thresholds);
            for index in indices {
                collect_direct_time_event_thresholds_from_expr(&index.range, env, thresholds);
            }
            if let Some(filter) = filter.as_deref() {
                collect_direct_time_event_thresholds_from_expr(filter, env, thresholds);
            }
        }
        dae::Expression::Index { base, subscripts } => {
            collect_direct_time_event_thresholds_from_expr(base, env, thresholds);
            for subscript in subscripts {
                if let dae::Subscript::Expr(expr) = subscript {
                    collect_direct_time_event_thresholds_from_expr(expr, env, thresholds);
                }
            }
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_direct_time_event_thresholds_from_expr(base, env, thresholds);
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn collect_dynamic_time_event_names_from_expr(
    expr: &dae::Expression,
    names: &mut indexmap::IndexSet<String>,
) {
    match expr {
        dae::Expression::Binary { op, lhs, rhs } => {
            if matches!(
                op,
                OpBinary::Ge(_) | OpBinary::Gt(_) | OpBinary::Le(_) | OpBinary::Lt(_)
            ) {
                // MLS Appendix B time events may guard on either the current
                // event time variable (`time >= t_next`) or its left-limit
                // (`time >= pre(nextEvent)`), depending on how the model was
                // flattened. No-state scheduling must recognize both forms.
                if let Some(name) = comparison_uses_time_and_event_var(lhs, rhs) {
                    names.insert(name);
                }
                if let Some(name) = comparison_uses_time_and_event_var(rhs, lhs) {
                    names.insert(name);
                }
            }
            collect_dynamic_time_event_names_from_expr(lhs, names);
            collect_dynamic_time_event_names_from_expr(rhs, names);
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_dynamic_time_event_names_from_expr(arg, names);
            }
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_dynamic_time_event_names_from_expr(rhs, names);
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_dynamic_time_event_names_from_expr(cond, names);
                collect_dynamic_time_event_names_from_expr(value, names);
            }
            collect_dynamic_time_event_names_from_expr(else_branch, names);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_dynamic_time_event_names_from_expr(element, names);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_dynamic_time_event_names_from_expr(start, names);
            if let Some(step) = step.as_deref() {
                collect_dynamic_time_event_names_from_expr(step, names);
            }
            collect_dynamic_time_event_names_from_expr(end, names);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_dynamic_time_event_names_from_expr(expr, names);
            for index in indices {
                collect_dynamic_time_event_names_from_expr(&index.range, names);
            }
            if let Some(filter) = filter.as_deref() {
                collect_dynamic_time_event_names_from_expr(filter, names);
            }
        }
        dae::Expression::Index { base, subscripts } => {
            collect_dynamic_time_event_names_from_expr(base, names);
            for subscript in subscripts {
                if let dae::Subscript::Expr(expr) = subscript {
                    collect_dynamic_time_event_names_from_expr(expr, names);
                }
            }
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_dynamic_time_event_names_from_expr(base, names);
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

pub(crate) fn collect_dynamic_time_event_names(dae_model: &dae::Dae) -> Vec<String> {
    let mut names = indexmap::IndexSet::new();
    for expr in dae_model
        .f_z
        .iter()
        .chain(dae_model.f_m.iter())
        .chain(dae_model.f_c.iter())
        .map(|eq| &eq.rhs)
    {
        collect_dynamic_time_event_names_from_expr(expr, &mut names);
    }
    names.into_iter().collect()
}

fn inject_dynamic_time_events(
    ctx: &NoStateSampleContext<'_>,
    env: &eval::VarEnv<f64>,
    evaluation_times: &mut Vec<f64>,
    output_times: &mut Vec<f64>,
    eval_idx: usize,
    t: f64,
    t_end: f64,
) {
    let trace_dynamic_events = std::env::var("RUMOCA_SIM_TRACE_DYNAMIC_EVENTS").is_ok();
    if trace_dynamic_events {
        let values: Vec<(String, f64)> = ctx
            .dynamic_time_event_names
            .iter()
            .filter_map(|name| {
                env.vars
                    .get(name)
                    .copied()
                    .map(|value| (name.clone(), value))
            })
            .collect();
        eprintln!(
            "[sim-trace] dynamic-events t={} eval_idx={} current={:?}",
            t, eval_idx, values
        );
    }
    for name in ctx.dynamic_time_event_names {
        let Some(event_t) = env.vars.get(name).copied() else {
            continue;
        };
        if !event_t.is_finite()
            || event_t <= t
            || timeline::sample_time_match_with_tol(event_t, t)
            || event_t >= t_end
            || timeline::sample_time_match_with_tol(event_t, t_end)
        {
            continue;
        }
        if evaluation_times
            .iter()
            .skip(eval_idx + 1)
            .any(|scheduled| timeline::sample_time_match_with_tol(*scheduled, event_t))
        {
            continue;
        }
        let insert_at = evaluation_times.partition_point(|scheduled| *scheduled < event_t);
        evaluation_times.insert(insert_at, event_t);
        if !output_times
            .iter()
            .any(|scheduled| timeline::sample_time_match_with_tol(*scheduled, event_t))
        {
            let output_insert_at = output_times.partition_point(|scheduled| *scheduled < event_t);
            output_times.insert(output_insert_at, event_t);
        }
        if trace_dynamic_events {
            eprintln!(
                "[sim-trace] dynamic-events inserted name={} event_t={} at={}",
                name, event_t, insert_at
            );
        }
    }

    // MLS §8.5 / Appendix B: time events may come directly from relation
    // thresholds such as `time >= expr`, not only from named next-event
    // variables. The no-state schedule must observe those instants exactly so
    // event iteration does not slip to the next coarse output sample.
    let mut direct_event_times = Vec::new();
    for expr in dynamic_time_threshold_exprs(ctx) {
        collect_direct_time_event_thresholds_from_expr(expr, env, &mut direct_event_times);
    }
    for event_t in direct_event_times {
        if !event_t.is_finite()
            || event_t <= t
            || timeline::sample_time_match_with_tol(event_t, t)
            || event_t >= t_end
            || timeline::sample_time_match_with_tol(event_t, t_end)
        {
            continue;
        }
        if evaluation_times
            .iter()
            .skip(eval_idx + 1)
            .any(|scheduled| timeline::sample_time_match_with_tol(*scheduled, event_t))
        {
            continue;
        }
        let insert_at = evaluation_times.partition_point(|scheduled| *scheduled < event_t);
        evaluation_times.insert(insert_at, event_t);
        if !output_times
            .iter()
            .any(|scheduled| timeline::sample_time_match_with_tol(*scheduled, event_t))
        {
            let output_insert_at = output_times.partition_point(|scheduled| *scheduled < event_t);
            output_times.insert(output_insert_at, event_t);
        }
        if trace_dynamic_events {
            eprintln!(
                "[sim-trace] dynamic-events inserted direct-threshold event_t={} at={}",
                event_t, insert_at
            );
        }
    }
}

fn insert_observation_time(times: &mut Vec<f64>, t: f64) {
    if times
        .iter()
        .any(|scheduled| timeline::sample_time_match_with_tol(*scheduled, t))
    {
        return;
    }
    let insert_at = times.partition_point(|scheduled| *scheduled < t);
    times.insert(insert_at, t);
}

fn relation_op_is_event_sensitive(op: &OpBinary) -> bool {
    matches!(
        op,
        OpBinary::Eq(_)
            | OpBinary::Neq(_)
            | OpBinary::Lt(_)
            | OpBinary::Le(_)
            | OpBinary::Gt(_)
            | OpBinary::Ge(_)
    )
}

fn builtin_call_is_event_sensitive(function: dae::BuiltinFunction) -> bool {
    matches!(
        function,
        // MLS §3.7.2 / SPEC_0022 EXPR-040: these operators can change only at
        // events and trigger events as needed.
        dae::BuiltinFunction::Div
            | dae::BuiltinFunction::Floor
            | dae::BuiltinFunction::Ceil
            | dae::BuiltinFunction::Integer
            // MLS §8.6 / §16.5: these operators depend on event or clock state.
            | dae::BuiltinFunction::Pre
            | dae::BuiltinFunction::Sample
            | dae::BuiltinFunction::Edge
            | dae::BuiltinFunction::Change
            | dae::BuiltinFunction::Reinit
            | dae::BuiltinFunction::Initial
    )
}

fn is_event_updated_discrete_name(dae_model: &dae::Dae, name: &dae::VarName) -> bool {
    dae_model.discrete_reals.contains_key(name)
        || dae_model.discrete_valued.contains_key(name)
        || dae::component_base_name(name.as_str()).is_some_and(|base| {
            let base = dae::VarName::new(base);
            dae_model.discrete_reals.contains_key(&base)
                || dae_model.discrete_valued.contains_key(&base)
        })
}

fn expr_uses_event_dependent_discrete_with_noevent(
    expr: &dae::Expression,
    suppress_relations: bool,
) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            let relation_suppressed =
                suppress_relations || *function == dae::BuiltinFunction::NoEvent;
            builtin_call_is_event_sensitive(*function)
                || args.iter().any(|arg| {
                    expr_uses_event_dependent_discrete_with_noevent(arg, relation_suppressed)
                })
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "previous"
                    | "hold"
                    | "Clock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "firstTick"
                    | "noClock"
                    | "interval"
            ) || args
                .iter()
                .any(|arg| expr_uses_event_dependent_discrete_with_noevent(arg, suppress_relations))
        }
        dae::Expression::Binary { op, lhs, rhs } => {
            (!suppress_relations && relation_op_is_event_sensitive(op))
                || expr_uses_event_dependent_discrete_with_noevent(lhs, suppress_relations)
                || expr_uses_event_dependent_discrete_with_noevent(rhs, suppress_relations)
        }
        dae::Expression::Unary { rhs, .. } => {
            expr_uses_event_dependent_discrete_with_noevent(rhs, suppress_relations)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_event_dependent_discrete_with_noevent(cond, suppress_relations)
                    || expr_uses_event_dependent_discrete_with_noevent(value, suppress_relations)
            }) || expr_uses_event_dependent_discrete_with_noevent(else_branch, suppress_relations)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(|element| {
                expr_uses_event_dependent_discrete_with_noevent(element, suppress_relations)
            })
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_event_dependent_discrete_with_noevent(start, suppress_relations)
                || step.as_deref().is_some_and(|value| {
                    expr_uses_event_dependent_discrete_with_noevent(value, suppress_relations)
                })
                || expr_uses_event_dependent_discrete_with_noevent(end, suppress_relations)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_event_dependent_discrete_with_noevent(expr, suppress_relations)
                || indices.iter().any(|index| {
                    expr_uses_event_dependent_discrete_with_noevent(
                        &index.range,
                        suppress_relations,
                    )
                })
                || filter.as_deref().is_some_and(|value| {
                    expr_uses_event_dependent_discrete_with_noevent(value, suppress_relations)
                })
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_event_dependent_discrete_with_noevent(base, suppress_relations)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => {
                        expr_uses_event_dependent_discrete_with_noevent(value, suppress_relations)
                    }
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => {
            expr_uses_event_dependent_discrete_with_noevent(base, suppress_relations)
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn expr_uses_event_dependent_discrete(expr: &dae::Expression) -> bool {
    expr_uses_event_dependent_discrete_with_noevent(expr, false)
}

pub(crate) fn expr_reads_event_updated_discrete_var(
    dae_model: &dae::Dae,
    expr: &dae::Expression,
) -> bool {
    match expr {
        // MLS Appendix B B.1b/B.1c with MLS §8.4 discrete persistence:
        // if a projected algebraic/output equation reads an event-updated
        // discrete variable directly, its right-limit must be reprojected
        // after the discrete settle finishes at the event instant.
        dae::Expression::VarRef { name, .. } => is_event_updated_discrete_name(dae_model, name),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter()
                .any(|arg| expr_reads_event_updated_discrete_var(dae_model, arg))
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_reads_event_updated_discrete_var(dae_model, lhs)
                || expr_reads_event_updated_discrete_var(dae_model, rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_reads_event_updated_discrete_var(dae_model, rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_reads_event_updated_discrete_var(dae_model, condition)
                    || expr_reads_event_updated_discrete_var(dae_model, value)
            }) || expr_reads_event_updated_discrete_var(dae_model, else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_reads_event_updated_discrete_var(dae_model, element)),
        dae::Expression::Range { start, step, end } => {
            expr_reads_event_updated_discrete_var(dae_model, start)
                || step
                    .as_deref()
                    .is_some_and(|value| expr_reads_event_updated_discrete_var(dae_model, value))
                || expr_reads_event_updated_discrete_var(dae_model, end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_reads_event_updated_discrete_var(dae_model, expr)
                || indices
                    .iter()
                    .any(|index| expr_reads_event_updated_discrete_var(dae_model, &index.range))
                || filter
                    .as_deref()
                    .is_some_and(|value| expr_reads_event_updated_discrete_var(dae_model, value))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_reads_event_updated_discrete_var(dae_model, base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => {
                        expr_reads_event_updated_discrete_var(dae_model, value)
                    }
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => {
            expr_reads_event_updated_discrete_var(dae_model, base)
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

fn expr_uses_inter_sample_pre_values(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall {
            // MLS §16.5.1: sample(value, clock) reads the left-limit of value
            // at clock ticks, so inter-sample right-limits must advance into
            // the runtime pre-store before the next clock event.
            function:
                dae::BuiltinFunction::Sample | dae::BuiltinFunction::Edge | dae::BuiltinFunction::Change,
            ..
        } => true,
        dae::Expression::BuiltinCall { args, .. } => {
            args.iter().any(expr_uses_inter_sample_pre_values)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "previous" | "hold" | "subSample" | "superSample" | "shiftSample" | "backSample"
            ) || args.iter().any(expr_uses_inter_sample_pre_values)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_inter_sample_pre_values(lhs) || expr_uses_inter_sample_pre_values(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_uses_inter_sample_pre_values(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_inter_sample_pre_values(cond) || expr_uses_inter_sample_pre_values(value)
            }) || expr_uses_inter_sample_pre_values(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_inter_sample_pre_values)
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_inter_sample_pre_values(start)
                || step
                    .as_deref()
                    .is_some_and(expr_uses_inter_sample_pre_values)
                || expr_uses_inter_sample_pre_values(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_inter_sample_pre_values(expr)
                || indices
                    .iter()
                    .any(|index| expr_uses_inter_sample_pre_values(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_uses_inter_sample_pre_values)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_inter_sample_pre_values(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => expr_uses_inter_sample_pre_values(value),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_uses_inter_sample_pre_values(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn expr_uses_lowered_pre_next_event_alias(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::VarRef { name, .. } => {
            let name = name.as_str();
            name.starts_with("__pre__.")
                && (name.contains(".nextEvent") || name.contains(".nextTimeEvent"))
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_uses_lowered_pre_next_event_alias)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_lowered_pre_next_event_alias(lhs)
                || expr_uses_lowered_pre_next_event_alias(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_uses_lowered_pre_next_event_alias(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_lowered_pre_next_event_alias(cond)
                    || expr_uses_lowered_pre_next_event_alias(value)
            }) || expr_uses_lowered_pre_next_event_alias(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_lowered_pre_next_event_alias)
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_lowered_pre_next_event_alias(start)
                || step
                    .as_deref()
                    .is_some_and(expr_uses_lowered_pre_next_event_alias)
                || expr_uses_lowered_pre_next_event_alias(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_lowered_pre_next_event_alias(expr)
                || indices
                    .iter()
                    .any(|index| expr_uses_lowered_pre_next_event_alias(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_uses_lowered_pre_next_event_alias)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_lowered_pre_next_event_alias(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => expr_uses_lowered_pre_next_event_alias(value),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_uses_lowered_pre_next_event_alias(base),
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

pub(crate) fn no_state_projection_needs_event_refresh(dae_model: &dae::Dae) -> bool {
    dae_model.f_x.iter().any(|eq| {
        expr_uses_event_dependent_discrete(&eq.rhs)
            || expr_reads_event_updated_discrete_var(dae_model, &eq.rhs)
    }) || dae_model
        .synthetic_root_conditions
        .iter()
        .chain(dae_model.triggered_clock_conditions.iter())
        .chain(dae_model.clock_constructor_exprs.iter())
        .any(expr_uses_event_dependent_discrete)
}

pub(crate) fn no_state_projection_uses_lowered_pre_next_event_aliases(
    dae_model: &dae::Dae,
) -> bool {
    dae_model
        .f_x
        .iter()
        .any(|eq| expr_uses_lowered_pre_next_event_alias(&eq.rhs))
}

pub(crate) fn no_state_requires_live_pre_values(dae_model: &dae::Dae) -> bool {
    !dae_model.clock_schedules.is_empty()
        || dae_model
            .f_x
            .iter()
            .chain(dae_model.f_z.iter())
            .chain(dae_model.f_m.iter())
            .chain(dae_model.f_c.iter())
            .any(|eq| expr_uses_event_dependent_discrete(&eq.rhs))
        || dae_model
            .synthetic_root_conditions
            .iter()
            .chain(dae_model.triggered_clock_conditions.iter())
            .chain(dae_model.clock_constructor_exprs.iter())
            .any(expr_uses_event_dependent_discrete)
}

fn expr_uses_frozen_event_pre_values(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "previous"
                    | "hold"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "Clock"
                    | "firstTick"
                    | "noClock"
            ) || args.iter().any(expr_uses_frozen_event_pre_values)
        }
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args,
        } => args.len() == 1 || args.iter().any(expr_uses_frozen_event_pre_values),
        dae::Expression::BuiltinCall { args, .. } => {
            args.iter().any(expr_uses_frozen_event_pre_values)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_uses_frozen_event_pre_values(lhs) || expr_uses_frozen_event_pre_values(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_uses_frozen_event_pre_values(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_frozen_event_pre_values(cond) || expr_uses_frozen_event_pre_values(value)
            }) || expr_uses_frozen_event_pre_values(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_frozen_event_pre_values)
        }
        dae::Expression::Range { start, step, end } => {
            expr_uses_frozen_event_pre_values(start)
                || step
                    .as_deref()
                    .is_some_and(expr_uses_frozen_event_pre_values)
                || expr_uses_frozen_event_pre_values(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_frozen_event_pre_values(expr)
                || indices
                    .iter()
                    .any(|index| expr_uses_frozen_event_pre_values(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_uses_frozen_event_pre_values)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_uses_frozen_event_pre_values(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => expr_uses_frozen_event_pre_values(value),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_uses_frozen_event_pre_values(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

pub(crate) fn no_state_requires_frozen_event_pre_values(dae_model: &dae::Dae) -> bool {
    // MLS §8.6 / Appendix B: ordinary event iteration advances pre(v) between
    // settle passes. MLS §16.5.1/§16.4 is narrower: clocked previous/hold
    // semantics stay anchored to the event-entry left limit for the full tick
    // settle round. Use the frozen-pre path only for clocked partitions.
    !dae_model.clock_schedules.is_empty()
        || !dae_model.triggered_clock_conditions.is_empty()
        || !dae_model.clock_constructor_exprs.is_empty()
        || dae_model
            .f_x
            .iter()
            .chain(dae_model.f_z.iter())
            .chain(dae_model.f_m.iter())
            .chain(dae_model.f_c.iter())
            .any(|eq| expr_uses_frozen_event_pre_values(&eq.rhs))
}

fn no_state_requires_inter_sample_pre_values(dae_model: &dae::Dae) -> bool {
    dae_model
        .f_x
        .iter()
        .chain(dae_model.f_z.iter())
        .chain(dae_model.f_m.iter())
        .chain(dae_model.f_c.iter())
        .any(|eq| expr_uses_inter_sample_pre_values(&eq.rhs))
        || dae_model
            .synthetic_root_conditions
            .iter()
            .chain(dae_model.triggered_clock_conditions.iter())
            .chain(dae_model.clock_constructor_exprs.iter())
            .any(expr_uses_inter_sample_pre_values)
}

fn can_reapply_runtime_direct_assignments_after_discrete_refresh(
    ctx: &NoStateSampleContext<'_>,
) -> bool {
    !ctx.requires_projection
        && !ctx.projection_needs_event_refresh
        && !ctx.requires_live_pre_values
        && !crate::runtime::clock::dae_may_have_discrete_clock_activity(ctx.dae)
}

fn apply_no_state_runtime_refresh_phases(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    env: &mut eval::VarEnv<f64>,
) -> bool {
    let mut propagate_runtime_direct_assignments =
        |dae: &dae::Dae, y: &mut [f64], n_x: usize, env: &mut eval::VarEnv<f64>| {
            crate::runtime::assignment::propagate_runtime_direct_assignments_from_env_with_context(
                ctx.direct_assignment_ctx,
                dae,
                y,
                n_x,
                env,
            )
        };
    let mut propagate_runtime_alias_components =
        |_dae: &dae::Dae, y: &mut [f64], n_x: usize, env: &mut eval::VarEnv<f64>| {
            crate::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
                ctx.alias_ctx,
                y,
                n_x,
                env,
            )
        };
    let mut sync_solver_values_from_env = crate::runtime::layout::sync_solver_values_from_env;
    let mut changed = crate::runtime::event::apply_runtime_pre_discrete_phase(
        ctx.dae,
        y,
        ctx.n_x,
        env,
        &mut propagate_runtime_direct_assignments,
        &mut propagate_runtime_alias_components,
    );
    changed |= crate::runtime::event::apply_runtime_post_discrete_phase(
        ctx.dae,
        y,
        ctx.n_x,
        env,
        &mut propagate_runtime_alias_components,
        &mut sync_solver_values_from_env,
    );
    changed
}

fn refresh_no_state_sample_env_in_place(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
    env: &mut eval::VarEnv<f64>,
    refresh_discrete_partition: bool,
) {
    eval::refresh_env_solver_and_parameter_values(env, ctx.dae, y, ctx.param_values, t);
    // MLS §8.6: `initial()` is true only during the initial event iteration.
    env.is_initial = no_state_sample_is_initial(ctx, t);
    apply_no_state_runtime_refresh_phases(ctx, y, env);
    if refresh_discrete_partition {
        // MLS §8.6 / §16.5.1: between no-state sample points, time-dependent
        // discrete equations must still be re-evaluated at the current
        // observation time. Otherwise sources such as BooleanPulse and
        // SampleTrigger stay latched to the previous event value.
        let discrete_changed =
            crate::runtime::discrete::apply_discrete_partition_updates(ctx.dae, env);
        if discrete_changed && can_reapply_runtime_direct_assignments_after_discrete_refresh(ctx) {
            // MLS §8.6 / §16.5.1: a discrete refresh may change sources that
            // feed ordinary runtime direct assignments and alias components.
            // Re-materialize those derived values before sampling the current
            // right-limit observation; otherwise stale carried values can win
            // until the next sample.
            apply_no_state_runtime_refresh_phases(ctx, y, env);
        }
        crate::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
            ctx.alias_ctx,
            y,
            ctx.n_x,
            env,
        );
    }
    if ctx.needs_eliminated_env {
        reconstruct::apply_eliminated_substitutions_to_env(ctx.elim, env);
    }
}

fn refresh_dynamic_event_right_limit_env_in_place(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
    env: &mut eval::VarEnv<f64>,
    guard_env: &eval::VarEnv<f64>,
) {
    let max_passes = (ctx.dae.f_z.len() + ctx.dae.f_m.len()).clamp(1, 16);
    for _ in 0..max_passes {
        eval::refresh_env_solver_and_parameter_values(env, ctx.dae, y, ctx.param_values, t);
        // MLS §8.6: `initial()` is true only during the initial event iteration.
        env.is_initial = no_state_sample_is_initial(ctx, t);
        if !ctx.elim.substitutions.is_empty() {
            reconstruct::apply_eliminated_substitutions_to_env(ctx.elim, env);
        }
        apply_no_state_runtime_refresh_phases(ctx, y, env);
        if !ctx.elim.substitutions.is_empty() {
            reconstruct::apply_eliminated_substitutions_to_env(ctx.elim, env);
        }
        let changed = crate::runtime::discrete::apply_discrete_partition_updates_with_guard_env_and_scalar_override(
            ctx.dae,
            env,
            guard_env,
            |_eq, _target, _solution, _env, _implicit_clock_active| None,
        );
        crate::runtime::alias::propagate_discrete_alias_equalities(
            ctx.dae,
            env,
            &mut HashSet::new(),
            |_| {},
        );
        if !ctx.elim.substitutions.is_empty() {
            reconstruct::apply_eliminated_substitutions_to_env(ctx.elim, env);
        }
        if !changed {
            break;
        }
        // MLS Appendix B / SPEC_0022 SIM-001: dynamic time events still use
        // event iteration. The right-limit observation must therefore advance
        // pre(z) and pre(m) to the previous pass result before retrying.
        eval::seed_pre_values_from_env(env);
    }
}

#[derive(Clone, Copy)]
enum EventPreMode {
    Auto,
    Frozen,
}

fn build_settled_runtime_env(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
) -> eval::VarEnv<f64> {
    build_settled_runtime_env_with_pre_mode(ctx, y, t, EventPreMode::Auto)
}

fn build_event_entry_runtime_env(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
) -> eval::VarEnv<f64> {
    build_settled_runtime_env_with_pre_mode(ctx, y, t, EventPreMode::Frozen)
}

fn apply_eliminated_substitutions_if_any(
    ctx: &NoStateSampleContext<'_>,
    env: &mut eval::VarEnv<f64>,
) -> bool {
    if ctx.elim.substitutions.is_empty() {
        return false;
    }
    reconstruct::apply_eliminated_substitutions_to_env_changed(ctx.elim, env)
}

fn settle_discrete_partition_round(
    ctx: &NoStateSampleContext<'_>,
    dae: &dae::Dae,
    env: &mut eval::VarEnv<f64>,
    guard_env: &mut Option<eval::VarEnv<f64>>,
) -> bool {
    let mut changed = apply_eliminated_substitutions_if_any(ctx, env);
    let guard_env = guard_env.get_or_insert_with(|| env.clone());
    changed |= crate::runtime::discrete::apply_discrete_partition_updates_with_guard_env_and_scalar_override(
        dae,
        env,
        guard_env,
        |_eq, _target, _solution, _env, _implicit_clock_active| None,
    );
    changed |= apply_eliminated_substitutions_if_any(ctx, env);
    changed
}

fn refresh_dynamic_event_right_limit_if_needed<E, FRefreshProjectedEnv>(
    ctx: &NoStateSampleContext<'_>,
    y: &mut Vec<f64>,
    t_right: f64,
    env: &mut eval::VarEnv<f64>,
    schedules: &mut ObservationSchedules<'_>,
    refresh_projected_env: &mut FRefreshProjectedEnv,
    dynamic_event_time: bool,
) -> Result<(), NoStateSampleError<E>>
where
    FRefreshProjectedEnv: FnMut(&mut Vec<f64>, f64, &mut eval::VarEnv<f64>) -> Result<(), E>,
{
    if !dynamic_event_time {
        return Ok(());
    }
    let guard_env = env.clone();
    refresh_projected_env(y, t_right, env).map_err(NoStateSampleError::Callback)?;
    refresh_dynamic_event_right_limit_env_in_place(ctx, y, t_right, env, &guard_env);
    insert_observation_time(schedules.evaluation_schedule, t_right);
    insert_observation_time(schedules.output_times, t_right);
    Ok(())
}

fn build_settled_runtime_env_with_pre_mode(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
    pre_mode: EventPreMode,
) -> eval::VarEnv<f64> {
    let mut env = if no_state_sample_is_initial(ctx, t) {
        build_initial_settled_runtime_env(ctx, y, t)
    } else {
        let mut guard_env: Option<eval::VarEnv<f64>> = None;
        let settle_input = crate::runtime::event::EventSettleInput {
            dae: ctx.dae,
            y,
            p: ctx.param_values,
            n_x: ctx.n_x,
            t_eval: t,
            is_initial: false,
        };
        let use_frozen_pre = match pre_mode {
            EventPreMode::Auto => no_state_requires_frozen_event_pre_values(ctx.dae),
            EventPreMode::Frozen => true,
        };
        if use_frozen_pre {
            crate::runtime::event::settle_runtime_event_updates_frozen_pre(
                settle_input,
                |dae, y, n_x, env| {
                    crate::runtime::assignment::propagate_runtime_direct_assignments_from_env_with_context(
                        ctx.direct_assignment_ctx,
                        dae,
                        y,
                        n_x,
                        env,
                    )
                },
                |_dae, y, n_x, env| {
                    crate::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
                        ctx.alias_ctx,
                        y,
                        n_x,
                        env,
                    )
                },
                |dae, env| {
                    // MLS §16.5.1 / §16.4: clocked settle keeps pre(z)/pre(m)
                    // anchored to the event-entry left limit. Reconstruct any
                    // eliminated aliases before each pass without advancing that
                    // pre-store.
                    settle_discrete_partition_round(ctx, dae, env, &mut guard_env)
                },
                crate::runtime::layout::sync_solver_values_from_env,
            )
        } else {
            crate::runtime::event::settle_runtime_event_updates(
                settle_input,
                |dae, y, n_x, env| {
                    crate::runtime::assignment::propagate_runtime_direct_assignments_from_env_with_context(
                        ctx.direct_assignment_ctx,
                        dae,
                        y,
                        n_x,
                        env,
                    )
                },
                |_dae, y, n_x, env| {
                    crate::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
                        ctx.alias_ctx,
                        y,
                        n_x,
                        env,
                    )
                },
                |dae, env| {
                    // MLS §8.6 / Appendix B: ordinary event iteration advances
                    // pre(z)/pre(m) between passes. Reconstruct eliminated
                    // aliases before each pass, but do not freeze them to the
                    // original left-limit across the whole settle round.
                    settle_discrete_partition_round(ctx, dae, env, &mut guard_env)
                },
                crate::runtime::layout::sync_solver_values_from_env,
            )
        }
    };
    if std::env::var_os("RUMOCA_DEBUG_COUNTER_ENABLE").is_some()
        && env.vars.contains_key("Enable.before")
    {
        eprintln!(
            "DEBUG no_state t={t:.6} before={} after={} stepTime={} Enable.y={} Counter.enable={} And1.y={} And1.aux_n={} FF1.q={} RS1.TD1.y={} RS2.TD1.y={}",
            env.get("Enable.before"),
            env.get("Enable.after"),
            env.get("Enable.stepTime"),
            env.get("Enable.y"),
            env.get("Counter.enable"),
            env.get("Counter.FF[1].And1.y"),
            env.get("Counter.FF[1].And1.auxiliary_n"),
            env.get("Counter.FF[1].q"),
            env.get("Counter.FF[1].RS1.TD1.y"),
            env.get("Counter.FF[1].RS2.TD1.y"),
        );
    }
    if ctx.needs_eliminated_env {
        reconstruct::apply_eliminated_substitutions_to_env(ctx.elim, &mut env);
    }
    env
}

pub(crate) fn build_initial_settled_runtime_env(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
) -> eval::VarEnv<f64> {
    // MLS §8.6: runtime-only discrete variables assigned in `initial
    // equation`/initial algorithms must already be visible at the first
    // observation instant of a no-state simulation.
    crate::runtime::startup::refresh_pre_values_from_state_with_initial_assignments(
        ctx.dae,
        y,
        ctx.param_values,
        t,
    );

    let mut env =
        crate::runtime::startup::build_initial_section_env(ctx.dae, y, ctx.param_values, t);
    if std::env::var_os("RUMOCA_DEBUG_DIGITAL_START").is_some() && env.vars.contains_key("a.y0") {
        eprintln!(
            "DEBUG initial base t={t} a.y0={} a.y={} a.t[1]={} a.x[1]={}",
            env.get("a.y0"),
            env.get("a.y"),
            env.get("a.t[1]"),
            env.get("a.x[1]"),
        );
    }
    let frozen_guard_env = env.clone();
    let needs_pre_fixed_point = initial_no_state_needs_pre_fixed_point(ctx, &env);
    if !needs_pre_fixed_point {
        eval::seed_pre_values_from_env(&env);
        let mut settled = settle_initial_runtime_event_pass(ctx, y, t, env, &frozen_guard_env);
        if can_reapply_runtime_direct_assignments_after_discrete_refresh(ctx) {
            refresh_no_state_sample_env_in_place(ctx, y, t, &mut settled, false);
            eval::seed_pre_values_from_env(&settled);
        }
        if std::env::var_os("RUMOCA_DEBUG_DIGITAL_START").is_some()
            && settled.vars.contains_key("a.y0")
        {
            eprintln!(
                "DEBUG initial settled t={t} a.y0={} a.y={} Adder.a={} Adder.AND.x[2]={}",
                settled.get("a.y0"),
                settled.get("a.y"),
                settled.get("Adder.a"),
                settled.get("Adder.AND.x[2]"),
            );
        }
        return settled;
    }

    let max_passes = (ctx.dae.f_m.len() + ctx.dae.f_z.len() + ctx.dae.f_c.len()).max(8);

    for _ in 0..max_passes {
        // MLS §8.6 / SPEC_0022 EQN-035: the initial event must converge to a
        // fixed point where ordinary discrete variables satisfy pre(v)=v after
        // the initialization section has produced the current startup values.
        eval::seed_pre_values_from_env(&env);
        let mut next_env =
            settle_initial_runtime_event_pass(ctx, y, t, env.clone(), &frozen_guard_env);
        if can_reapply_runtime_direct_assignments_after_discrete_refresh(ctx) {
            refresh_no_state_sample_env_in_place(ctx, y, t, &mut next_env, false);
        }
        if std::env::var_os("RUMOCA_DEBUG_DIGITAL_START").is_some()
            && next_env.vars.contains_key("a.y0")
        {
            eprintln!(
                "DEBUG initial fp t={t} a.y0={} a.y={} Adder.a={} Adder.AND.x[2]={}",
                next_env.get("a.y0"),
                next_env.get("a.y"),
                next_env.get("Adder.a"),
                next_env.get("Adder.AND.x[2]"),
            );
        }
        if runtime_env_vars_stable(&env, &next_env) {
            if can_reapply_runtime_direct_assignments_after_discrete_refresh(ctx) {
                eval::seed_pre_values_from_env(&next_env);
            }
            return next_env;
        }
        env = next_env;
    }

    env
}

fn initial_no_state_needs_pre_fixed_point(
    ctx: &NoStateSampleContext<'_>,
    env: &eval::VarEnv<f64>,
) -> bool {
    // MLS §8.6 / Appendix B: initial-event convergence for ordinary discrete
    // `pre(v)` feedback is required even if the model also contains later
    // scheduled clock events. The `previous(...)` startup loop used by MLS
    // §16.5 clocked partitions is different: repeatedly applying the ordinary
    // `pre(v)=v` initialization fixed point there can advance clocked feedback
    // multiple ticks at `t_start`.
    ctx.dae
        .f_m
        .iter()
        .chain(ctx.dae.f_z.iter())
        .filter_map(crate::runtime::assignment::direct_assignment_from_equation)
        .any(|(_target, solution)| {
            let has_plain_pre = expr_contains_plain_pre_var(solution);
            let has_previous = expr_contains_previous_feedback(solution);
            let has_clocked_feedback = expr_contains_clocked_feedback(ctx.dae, solution, env);
            has_plain_pre && !has_previous && !has_clocked_feedback
        })
}

fn expr_contains_clocked_feedback(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &eval::VarEnv<f64>,
) -> bool {
    match expr {
        dae::Expression::VarRef { .. } => {
            crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, expr, env)
        }
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            ..
        } => true,
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge | dae::BuiltinFunction::Change,
            args,
        } => {
            let first_is_clocked = args.first().is_some_and(|arg| {
                crate::runtime::clock::sample_clock_arg_is_explicit_clock(dae, arg, env)
                    || expr_contains_clocked_feedback(dae, arg, env)
            });
            first_is_clocked
                || args
                    .iter()
                    .skip(1)
                    .any(|arg| expr_contains_clocked_feedback(dae, arg, env))
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            let has_clock_function = matches!(
                expr,
                dae::Expression::FunctionCall { name, .. }
                    if matches!(
                        name.as_str().rsplit('.').next().unwrap_or(name.as_str()),
                        "Clock"
                            | "subSample"
                            | "superSample"
                            | "shiftSample"
                            | "backSample"
                            | "firstTick"
                            | "hold"
                            | "previous"
                            | "noClock"
                    )
            );
            has_clock_function
                || args
                    .iter()
                    .any(|arg| expr_contains_clocked_feedback(dae, arg, env))
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_clocked_feedback(dae, lhs, env)
                || expr_contains_clocked_feedback(dae, rhs, env)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_clocked_feedback(dae, rhs, env),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_clocked_feedback(dae, condition, env)
                    || expr_contains_clocked_feedback(dae, value, env)
            }) || expr_contains_clocked_feedback(dae, else_branch, env)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_contains_clocked_feedback(dae, element, env)),
        dae::Expression::Range { start, step, end } => {
            expr_contains_clocked_feedback(dae, start, env)
                || step
                    .as_deref()
                    .is_some_and(|value| expr_contains_clocked_feedback(dae, value, env))
                || expr_contains_clocked_feedback(dae, end, env)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_clocked_feedback(dae, expr, env)
                || indices
                    .iter()
                    .any(|index| expr_contains_clocked_feedback(dae, &index.range, env))
                || filter
                    .as_deref()
                    .is_some_and(|value| expr_contains_clocked_feedback(dae, value, env))
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_clocked_feedback(dae, base, env)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_clocked_feedback(dae, expr, env),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_clocked_feedback(dae, base, env),
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

fn expr_contains_plain_pre_var(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args,
        } => matches!(args.as_slice(), [dae::Expression::VarRef { .. }]),
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_plain_pre_var)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_plain_pre_var(lhs) || expr_contains_plain_pre_var(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_plain_pre_var(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_plain_pre_var(condition) || expr_contains_plain_pre_var(value)
            }) || expr_contains_plain_pre_var(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_plain_pre_var)
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_plain_pre_var(start)
                || step.as_deref().is_some_and(expr_contains_plain_pre_var)
                || expr_contains_plain_pre_var(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_plain_pre_var(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_plain_pre_var(&index.range))
                || filter.as_deref().is_some_and(expr_contains_plain_pre_var)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_plain_pre_var(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_contains_plain_pre_var(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_plain_pre_var(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn expr_contains_previous_feedback(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { args, .. } => {
            args.iter().any(expr_contains_previous_feedback)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            short == "previous" || args.iter().any(expr_contains_previous_feedback)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_previous_feedback(lhs) || expr_contains_previous_feedback(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_previous_feedback(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_contains_previous_feedback(cond) || expr_contains_previous_feedback(value)
            }) || expr_contains_previous_feedback(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_previous_feedback)
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_previous_feedback(start)
                || step.as_deref().is_some_and(expr_contains_previous_feedback)
                || expr_contains_previous_feedback(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_previous_feedback(expr)
                || indices
                    .iter()
                    .any(|index| expr_contains_previous_feedback(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_previous_feedback)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_previous_feedback(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(value) => expr_contains_previous_feedback(value),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_previous_feedback(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn settle_initial_runtime_event_pass(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
    env: eval::VarEnv<f64>,
    frozen_guard_env: &eval::VarEnv<f64>,
) -> eval::VarEnv<f64> {
    crate::runtime::event::settle_runtime_event_updates_frozen_pre_from_env(
        crate::runtime::event::EventSettleInput {
            dae: ctx.dae,
            y,
            p: ctx.param_values,
            n_x: ctx.n_x,
            t_eval: t,
            is_initial: true,
        },
        env,
        |dae, y, n_x, env| {
            crate::runtime::assignment::propagate_runtime_direct_assignments_from_env_with_context(
                ctx.direct_assignment_ctx,
                dae,
                y,
                n_x,
                env,
            )
        },
        |_dae, y, n_x, env| {
            crate::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
                ctx.alias_ctx,
                y,
                n_x,
                env,
            )
        },
        |dae, env| {
            let mut changed = false;
            if !ctx.elim.substitutions.is_empty() {
                // MLS §8.6 / Appendix B: event iteration solves the current
                // event-pass z/m system while pre(z)/pre(m) stays fixed to the
                // event left-limit. Eliminated substitution aliases that feed
                // discrete equations must therefore be reconstructed before
                // each discrete settle pass, even when they are not sampled
                // outputs.
                changed |=
                    reconstruct::apply_eliminated_substitutions_to_env_changed(ctx.elim, env);
            }
            // MLS §8.6 / Appendix B: discrete event iteration must observe the
            // event-entry values across settle passes, especially for lowered
            // multi-output function assignments represented as projected RHSs.
            changed |= crate::runtime::discrete::apply_discrete_partition_updates_with_guard_env_and_scalar_override(
                dae,
                env,
                frozen_guard_env,
                |_eq, _target, _solution, _env, _implicit_clock_active| None,
            );
            if !ctx.elim.substitutions.is_empty() {
                changed |=
                    reconstruct::apply_eliminated_substitutions_to_env_changed(ctx.elim, env);
            }
            changed
        },
        crate::runtime::layout::sync_solver_values_from_env,
    )
}

fn runtime_env_vars_stable(lhs: &eval::VarEnv<f64>, rhs: &eval::VarEnv<f64>) -> bool {
    lhs.vars.len() == rhs.vars.len()
        && lhs.vars.iter().zip(rhs.vars.iter()).all(
            |((lhs_name, lhs_value), (rhs_name, rhs_value))| {
                lhs_name == rhs_name && runtime_env_value_stable(*lhs_value, *rhs_value)
            },
        )
}

fn runtime_env_value_stable(lhs: f64, rhs: f64) -> bool {
    // MLS §8.6 fixed-point convergence should stop once the runtime env stops
    // changing. Identical NaN placeholders on untouched slots do not represent
    // semantic progress and must not force extra settle passes.
    if lhs.is_nan() && rhs.is_nan() {
        return true;
    }
    lhs == rhs || (lhs - rhs).abs() <= 1.0e-12
}

fn settle_runtime_env<'a>(
    ctx: &NoStateSampleContext<'_>,
    y: &mut [f64],
    t: f64,
    carried_env: &'a mut Option<eval::VarEnv<f64>>,
    advance_pre_between_samples: bool,
    refresh_discrete_between_samples: bool,
) -> &'a mut eval::VarEnv<f64> {
    crate::runtime::hotpath_stats::inc_no_state_settle();
    // MLS §8.6 / App B: discrete variables only change at event instants, so
    // injected dynamic time events must take the same full-settle path as
    // scheduled clock events.
    let settle_t = matched_no_state_event_time(ctx, t, carried_env.as_ref()).unwrap_or(t);
    let needs_full_settle =
        should_advance_pre_values(ctx, t, carried_env.as_ref()) || carried_env.is_none();
    if needs_full_settle {
        // When the output grid lands just left of an event instant due to
        // repeated floating-point addition, settle at the matched event time
        // itself so event-triggered table lookups advance to the next pending
        // breakpoint instead of reusing the previous one.
        *carried_env = Some(build_settled_runtime_env(ctx, y, settle_t));
    } else if let Some(env) = carried_env.as_mut() {
        if advance_pre_between_samples {
            // MLS §3.7.5 / §8.6: `edge(...)` and `change(...)` between explicit
            // scheduled events must observe the previous settled discrete
            // right-limit, not an older clock-only snapshot. Advancing the
            // pre-store from the carried env before the next refresh keeps
            // pulse-driven guards aligned with the last observed event state.
            eval::seed_pre_values_from_env(env);
        }
        // MLS §16.5.1: sampled values hold between active ticks. Preserve the
        // settled discrete env and refresh only runtime-tail and
        // alias/direct-assignment projections between scheduled clock events.
        refresh_no_state_sample_env_in_place(ctx, y, t, env, refresh_discrete_between_samples);
    }
    carried_env
        .as_mut()
        .expect("no-state carried env must be initialized before use")
}

fn project_algebraics_if_changed<E, FProjectOrSeed>(
    ctx: &NoStateSampleContext<'_>,
    y: &mut Vec<f64>,
    t: f64,
    project_or_seed: &mut FProjectOrSeed,
) -> Result<bool, NoStateSampleError<E>>
where
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
{
    if !ctx.requires_projection {
        return Ok(false);
    }

    let before = y.clone();
    project_or_seed(y, t, true).map_err(NoStateSampleError::Callback)?;
    let changed = before
        .iter()
        .zip(y.iter())
        .any(|(old, new)| (old - new).abs() > 1.0e-12);
    Ok(changed)
}

fn refresh_projection_with_event_context<E, FProjectOrSeed>(
    ctx: &NoStateSampleContext<'_>,
    y: &mut Vec<f64>,
    t: f64,
    carried_env: &mut Option<eval::VarEnv<f64>>,
    advance_pre_between_samples: bool,
    refresh_discrete_between_samples: bool,
    project_or_seed: &mut FProjectOrSeed,
) -> Result<(), NoStateSampleError<E>>
where
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
{
    let pre_snapshot = {
        let env = settle_runtime_env(
            ctx,
            y,
            t,
            carried_env,
            advance_pre_between_samples,
            refresh_discrete_between_samples,
        );
        let snapshot = eval::snapshot_pre_values();
        eval::seed_pre_values_from_env(env);
        snapshot
    };
    if project_algebraics_if_changed(ctx, y, t, project_or_seed)? {
        let env = settle_runtime_env(
            ctx,
            y,
            t,
            carried_env,
            advance_pre_between_samples,
            refresh_discrete_between_samples,
        );
        eval::seed_pre_values_from_env(env);
    }
    eval::restore_pre_values(pre_snapshot);
    Ok(())
}

mod output;

#[cfg(test)]
pub(crate) use output::collect_algebraic_samples_with_schedule;
#[cfg(test)]
pub(crate) use output::{can_sample_solver_outputs_directly, sampled_names_need_eliminated_env};
pub use output::{
    collect_algebraic_samples, collect_reconstruction_discrete_context_names,
    finalize_algebraic_outputs,
};
pub(crate) use output::{
    collect_algebraic_samples_with_schedule_and_env_refresh,
    sampled_names_need_eliminated_env_with_runtime_closure,
};

#[cfg(test)]
mod tests;
