use super::*;

pub(super) struct RuntimeChannelCapture {
    pub(super) names: Vec<String>,
    pub(super) solver_name_to_idx: HashMap<String, usize>,
    pub(super) settle_ctx: RuntimeDiscreteCaptureContext,
}

pub(super) struct RuntimeDynamicStopHints {
    dynamic_time_event_names: Vec<String>,
    direct_time_event_exprs: Vec<Expression>,
}

impl RuntimeDynamicStopHints {
    pub(super) fn from_dae(dae: &Dae) -> Option<Self> {
        let dynamic_time_event_names =
            rumoca_sim::runtime::no_state::collect_dynamic_time_event_names(dae);
        let direct_time_event_exprs: Vec<Expression> = dae
            .f_z
            .iter()
            .chain(dae.f_m.iter())
            .chain(dae.f_c.iter())
            .map(|eq| &eq.rhs)
            .chain(dae.synthetic_root_conditions.iter())
            .filter(|expr| {
                expr_uses_explicit_event_operator(expr)
                    && expr_has_direct_time_event_threshold(expr)
            })
            .cloned()
            .collect();
        if dynamic_time_event_names.is_empty() && direct_time_event_exprs.is_empty() {
            None
        } else {
            Some(Self {
                dynamic_time_event_names,
                direct_time_event_exprs,
            })
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum RuntimeSampleMode {
    Initialization,
    Regular,
}

pub(super) struct RuntimeDiscreteCaptureContext {
    direct_assignment_ctx: rumoca_sim::runtime::assignment::RuntimeDirectAssignmentContext,
    alias_ctx: rumoca_sim::runtime::alias::RuntimeAliasPropagationContext,
    pub(super) needs_eliminated_env: bool,
}

pub(crate) struct EventObservationResult {
    pub(crate) state: Vec<f64>,
    pub(crate) runtime_env: eval::VarEnv<f64>,
}

pub(crate) struct IntegrationRunInput<'a> {
    pub dae: &'a Dae,
    pub elim: &'a eliminate::EliminationResult,
    pub opts: &'a SimOptions,
    pub n_total: usize,
    pub mass_matrix: &'a MassMatrix,
    pub param_values: &'a [f64],
    pub budget: &'a TimeoutBudget,
}

pub(super) struct RuntimeDynamicStopInput<'a> {
    pub(super) dae: &'a Dae,
    pub(super) elim: &'a eliminate::EliminationResult,
    pub(super) p: &'a [f64],
    pub(super) n_x: usize,
    pub(super) hints: &'a RuntimeDynamicStopHints,
}

pub(super) fn runtime_event_matches_schedule(dae: &Dae, opts: &SimOptions, t_event: f64) -> bool {
    rumoca_sim::timeline::collect_runtime_schedule_events(dae, opts.t_start, opts.t_end)
        .into_iter()
        .any(|scheduled_t| rumoca_sim::timeline::sample_time_match_with_tol(scheduled_t, t_event))
}

pub(super) fn runtime_event_uses_frozen_pre_values(
    dae: &Dae,
    opts: &SimOptions,
    y_event: &[f64],
    p: &[f64],
    t_event: f64,
) -> bool {
    if runtime_event_matches_schedule(dae, opts, t_event) {
        return true;
    }
    if dae.synthetic_root_conditions.is_empty() {
        return false;
    }

    let mut y_eval = y_event.to_vec();
    let env = rumoca_sim::runtime::event::build_runtime_env(dae, y_eval.as_mut_slice(), p, t_event);
    let mut thresholds = Vec::new();
    for expr in &dae.synthetic_root_conditions {
        collect_direct_time_event_thresholds_from_expr(expr, &env, &mut thresholds);
    }

    // MLS Appendix B time events trigger at explicit time thresholds. When a
    // synthetic root is just such a time surface, keep `pre(...)` anchored to
    // the event-entry left limit for the whole settle round instead of
    // re-iterating it like an ordinary relation event.
    thresholds
        .into_iter()
        .any(|threshold| rumoca_sim::timeline::sample_time_match_with_tol(threshold, t_event))
}

pub(super) fn runtime_capture_target_names(dae: &Dae, _solver_names: &[String]) -> Vec<String> {
    collect_discrete_channel_names(dae)
}

pub(super) fn collect_runtime_capture_dependency_seed_names(
    dae: &Dae,
    observed_names: &[String],
) -> Vec<String> {
    let observed: HashSet<&str> = observed_names.iter().map(String::as_str).collect();
    let mut seeds = indexmap::IndexSet::new();
    for name in observed_names {
        seeds.insert(name.clone());
    }

    for eq in dae.f_x.iter().chain(dae.f_z.iter()).chain(dae.f_m.iter()) {
        let target = eq
            .lhs
            .as_ref()
            .map(|lhs| lhs.as_str().to_string())
            .or_else(|| {
                rumoca_sim::runtime::assignment::direct_assignment_from_equation(eq)
                    .map(|(target, _)| target)
            });
        let Some(target) = target else {
            continue;
        };
        if !observed.contains(target.as_str()) {
            continue;
        }

        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        for name in refs {
            seeds.insert(name.as_str().to_string());
        }
    }

    seeds.into_iter().collect()
}

pub(super) fn build_runtime_discrete_capture_context(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    y_len: usize,
    n_x: usize,
    observed_names: &[String],
) -> RuntimeDiscreteCaptureContext {
    let direct_assignment_ctx =
        rumoca_sim::runtime::assignment::build_runtime_direct_assignment_context(dae, y_len, n_x);
    let alias_ctx =
        rumoca_sim::runtime::alias::build_runtime_alias_propagation_context(dae, y_len, n_x);
    let mut all_names = collect_runtime_capture_dependency_seed_names(dae, observed_names);
    all_names.extend(
        rumoca_sim::runtime::no_state::collect_reconstruction_discrete_context_names(
            dae,
            elim,
            observed_names,
        ),
    );
    let needs_eliminated_env =
        rumoca_sim::runtime::no_state::sampled_names_need_eliminated_env_with_runtime_closure(
            &all_names,
            elim,
            &direct_assignment_ctx,
            &alias_ctx,
        );

    RuntimeDiscreteCaptureContext {
        direct_assignment_ctx,
        alias_ctx,
        needs_eliminated_env,
    }
}

pub(super) fn expr_is_time_var(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::VarRef { name, subscripts }
            if name.as_str() == "time" && subscripts.is_empty()
    )
}

pub(super) fn eval_time_event_threshold(expr: &Expression, env: &eval::VarEnv<f64>) -> Option<f64> {
    rumoca_sim::runtime::scalar_eval::eval_scalar_expr_fast(expr, env)
        .or_else(|| Some(rumoca_eval_dae::runtime::eval_expr::<f64>(expr, env)))
        .filter(|value| value.is_finite())
}

pub(super) fn expr_has_direct_time_event_threshold(expr: &Expression) -> bool {
    match expr {
        Expression::Binary {
            op:
                rumoca_ir_core::OpBinary::Ge(_)
                | rumoca_ir_core::OpBinary::Gt(_)
                | rumoca_ir_core::OpBinary::Le(_)
                | rumoca_ir_core::OpBinary::Lt(_),
            lhs,
            rhs,
        } => {
            expr_is_time_var(lhs)
                || expr_is_time_var(rhs)
                || expr_has_direct_time_event_threshold(lhs)
                || expr_has_direct_time_event_threshold(rhs)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_has_direct_time_event_threshold(lhs) || expr_has_direct_time_event_threshold(rhs)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_has_direct_time_event_threshold)
        }
        Expression::Unary { rhs, .. } => expr_has_direct_time_event_threshold(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_has_direct_time_event_threshold(condition)
                    || expr_has_direct_time_event_threshold(value)
            }) || expr_has_direct_time_event_threshold(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_has_direct_time_event_threshold)
        }
        Expression::Range { start, step, end } => {
            expr_has_direct_time_event_threshold(start)
                || step
                    .as_deref()
                    .is_some_and(expr_has_direct_time_event_threshold)
                || expr_has_direct_time_event_threshold(end)
        }
        Expression::ArrayComprehension {
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
        Expression::Index { base, subscripts } => {
            expr_has_direct_time_event_threshold(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_has_direct_time_event_threshold(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        Expression::FieldAccess { base, .. } => expr_has_direct_time_event_threshold(base),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

pub(super) fn expr_uses_explicit_event_operator(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                BuiltinFunction::Pre
                    | BuiltinFunction::Sample
                    | BuiltinFunction::Edge
                    | BuiltinFunction::Change
                    | BuiltinFunction::Reinit
                    | BuiltinFunction::Initial
            ) || args.iter().any(expr_uses_explicit_event_operator)
        }
        Expression::FunctionCall { name, args, .. } => {
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
        Expression::Binary { lhs, rhs, .. } => {
            expr_uses_explicit_event_operator(lhs) || expr_uses_explicit_event_operator(rhs)
        }
        Expression::Unary { rhs, .. } => expr_uses_explicit_event_operator(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_uses_explicit_event_operator(condition)
                    || expr_uses_explicit_event_operator(value)
            }) || expr_uses_explicit_event_operator(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_explicit_event_operator)
        }
        Expression::Range { start, step, end } => {
            expr_uses_explicit_event_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_uses_explicit_event_operator)
                || expr_uses_explicit_event_operator(end)
        }
        Expression::ArrayComprehension {
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
        Expression::Index { base, subscripts } => {
            expr_uses_explicit_event_operator(base)
                || subscripts.iter().any(|subscript| match subscript {
                    dae::Subscript::Expr(expr) => expr_uses_explicit_event_operator(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        Expression::FieldAccess { base, .. } => expr_uses_explicit_event_operator(base),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

pub(super) fn collect_direct_time_event_thresholds_from_expr(
    expr: &Expression,
    env: &eval::VarEnv<f64>,
    thresholds: &mut Vec<f64>,
) {
    match expr {
        Expression::Binary {
            op:
                rumoca_ir_core::OpBinary::Ge(_)
                | rumoca_ir_core::OpBinary::Gt(_)
                | rumoca_ir_core::OpBinary::Le(_)
                | rumoca_ir_core::OpBinary::Lt(_),
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
        Expression::Binary { lhs, rhs, .. } => {
            collect_direct_time_event_thresholds_from_expr(lhs, env, thresholds);
            collect_direct_time_event_thresholds_from_expr(rhs, env, thresholds);
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_direct_time_event_thresholds_from_expr(arg, env, thresholds);
            }
        }
        Expression::Unary { rhs, .. } => {
            collect_direct_time_event_thresholds_from_expr(rhs, env, thresholds);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                collect_direct_time_event_thresholds_from_expr(condition, env, thresholds);
                collect_direct_time_event_thresholds_from_expr(value, env, thresholds);
            }
            collect_direct_time_event_thresholds_from_expr(else_branch, env, thresholds);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                collect_direct_time_event_thresholds_from_expr(element, env, thresholds);
            }
        }
        Expression::Range { start, step, end } => {
            collect_direct_time_event_thresholds_from_expr(start, env, thresholds);
            if let Some(step) = step.as_deref() {
                collect_direct_time_event_thresholds_from_expr(step, env, thresholds);
            }
            collect_direct_time_event_thresholds_from_expr(end, env, thresholds);
        }
        Expression::ArrayComprehension {
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
        Expression::Index { base, subscripts } => {
            collect_direct_time_event_thresholds_from_expr(base, env, thresholds);
            for subscript in subscripts {
                if let dae::Subscript::Expr(expr) = subscript {
                    collect_direct_time_event_thresholds_from_expr(expr, env, thresholds);
                }
            }
        }
        Expression::FieldAccess { base, .. } => {
            collect_direct_time_event_thresholds_from_expr(base, env, thresholds);
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
}

pub(super) fn seed_capture_pre_values(env: &mut eval::VarEnv<f64>, names: &[String]) {
    for name in names {
        if let Some(pre) = eval::get_pre_value(name.as_str()) {
            env.set(name.as_str(), pre);
        }
    }
}

pub(super) fn observed_runtime_sample_value(
    capture: &RuntimeChannelCapture,
    event_observation: &EventObservationResult,
    name: &str,
) -> f64 {
    event_observation
        .runtime_env
        .vars
        .get(name)
        .copied()
        .or_else(|| {
            capture
                .solver_name_to_idx
                .get(name)
                .and_then(|idx| event_observation.state.get(*idx).copied())
        })
        .unwrap_or(0.0)
}

pub(super) fn next_dynamic_runtime_stop_time(
    input: &RuntimeDynamicStopInput<'_>,
    y: &[f64],
    current_t: f64,
    stop_time: f64,
) -> Option<f64> {
    let mut y_eval = y.to_vec();
    let env = settle_runtime_discrete_capture_env(
        input.dae,
        input.elim,
        y_eval.as_mut_slice(),
        input.p,
        input.n_x,
        current_t,
    );
    let mut next_stop: Option<f64> = None;

    for name in &input.hints.dynamic_time_event_names {
        let Some(event_t) = env.vars.get(name).copied() else {
            continue;
        };
        if !event_t.is_finite()
            || event_t <= current_t
            || rumoca_sim::timeline::sample_time_match_with_tol(event_t, current_t)
            || event_t >= stop_time
            || rumoca_sim::timeline::sample_time_match_with_tol(event_t, stop_time)
        {
            continue;
        }
        next_stop = Some(next_stop.map_or(event_t, |best| best.min(event_t)));
    }

    let mut thresholds = Vec::new();
    for expr in &input.hints.direct_time_event_exprs {
        collect_direct_time_event_thresholds_from_expr(expr, &env, &mut thresholds);
    }
    for event_t in thresholds {
        if !event_t.is_finite()
            || event_t <= current_t
            || rumoca_sim::timeline::sample_time_match_with_tol(event_t, current_t)
            || event_t >= stop_time
            || rumoca_sim::timeline::sample_time_match_with_tol(event_t, stop_time)
        {
            continue;
        }
        next_stop = Some(next_stop.map_or(event_t, |best| best.min(event_t)));
    }

    next_stop
}

pub(super) fn sample_clock_arg_is_explicit_clock(
    dae: &Dae,
    clock_expr: &Expression,
    env: &eval::VarEnv<f64>,
) -> bool {
    rumoca_sim::runtime::clock::sample_clock_arg_is_explicit_clock(dae, clock_expr, env)
}

pub(super) fn expr_uses_implicit_sample_clock(
    dae: &Dae,
    expr: &Expression,
    env: &eval::VarEnv<f64>,
) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            let is_implicit_sample = if *function == BuiltinFunction::Sample {
                if args.len() <= 1 {
                    true
                } else {
                    !sample_clock_arg_is_explicit_clock(dae, &args[1], env)
                }
            } else {
                false
            };
            is_implicit_sample
                || args
                    .iter()
                    .any(|arg| expr_uses_implicit_sample_clock(dae, arg, env))
        }
        Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_uses_implicit_sample_clock(dae, arg, env)),
        Expression::Binary { lhs, rhs, .. } => {
            expr_uses_implicit_sample_clock(dae, lhs, env)
                || expr_uses_implicit_sample_clock(dae, rhs, env)
        }
        Expression::Unary { rhs, .. } => expr_uses_implicit_sample_clock(dae, rhs, env),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_implicit_sample_clock(dae, cond, env)
                    || expr_uses_implicit_sample_clock(dae, value, env)
            }) || expr_uses_implicit_sample_clock(dae, else_branch, env)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(|item| expr_uses_implicit_sample_clock(dae, item, env)),
        Expression::Range { start, step, end } => {
            expr_uses_implicit_sample_clock(dae, start, env)
                || step
                    .as_ref()
                    .is_some_and(|value| expr_uses_implicit_sample_clock(dae, value, env))
                || expr_uses_implicit_sample_clock(dae, end, env)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_implicit_sample_clock(dae, expr, env)
                || indices
                    .iter()
                    .any(|idx| expr_uses_implicit_sample_clock(dae, &idx.range, env))
                || filter
                    .as_ref()
                    .is_some_and(|value| expr_uses_implicit_sample_clock(dae, value, env))
        }
        Expression::Index { base, subscripts } => {
            expr_uses_implicit_sample_clock(dae, base, env)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => expr_uses_implicit_sample_clock(dae, value, env),
                    _ => false,
                })
        }
        Expression::FieldAccess { base, .. } => expr_uses_implicit_sample_clock(dae, base, env),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

pub(super) fn settle_runtime_discrete_capture_env(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
) -> eval::VarEnv<f64> {
    let settle_ctx = RuntimeDiscreteCaptureContext {
        direct_assignment_ctx:
            rumoca_sim::runtime::assignment::build_runtime_direct_assignment_context(
                dae,
                y.len(),
                n_x,
            ),
        alias_ctx: rumoca_sim::runtime::alias::build_runtime_alias_propagation_context(
            dae,
            y.len(),
            n_x,
        ),
        needs_eliminated_env: !elim.substitutions.is_empty(),
    };
    settle_runtime_discrete_capture_env_with_context(dae, elim, y, p, n_x, t_eval, &settle_ctx)
}

pub(super) fn settle_runtime_discrete_capture_env_with_context(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
    settle_ctx: &RuntimeDiscreteCaptureContext,
) -> eval::VarEnv<f64> {
    rumoca_sim::runtime::event::settle_runtime_event_updates_frozen_pre(
        rumoca_sim::EventSettleInput {
            dae,
            y,
            p,
            n_x,
            t_eval,
            is_initial: false,
        },
        |dae, y, n_x, env| {
            rumoca_sim::runtime::assignment::propagate_runtime_direct_assignments_from_env_with_context(
                &settle_ctx.direct_assignment_ctx,
                dae,
                y,
                n_x,
                env,
            )
        },
        |_dae, y, n_x, env| {
            rumoca_sim::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
                &settle_ctx.alias_ctx,
                y,
                n_x,
                env,
            )
        },
        |dae, env| {
            let mut changed = false;
            if settle_ctx.needs_eliminated_env {
                // MLS §16.5.1: stateful sampled equations must observe the
                // event-entry values of their continuous sources. If prepare
                // eliminated a source alias such as `sample1.u`, reconstruct it
                // into the runtime env before running the sampled equations.
                changed |= rumoca_sim::reconstruct::apply_eliminated_substitutions_to_env_changed(
                    elim, env,
                );
            }
            changed |=
                rumoca_sim::runtime::discrete::apply_discrete_partition_updates_with_scalar_override(
                    dae,
                    env,
                    |_eq, target, solution, env, implicit_clock_active| {
                        if implicit_clock_active
                            && expr_uses_implicit_sample_clock(dae, solution, env)
                        {
                            return Some(env.vars.get(target).copied().unwrap_or(0.0));
                        }
                        None
                    },
                );
            if settle_ctx.needs_eliminated_env {
                changed |= rumoca_sim::reconstruct::apply_eliminated_substitutions_to_env_changed(
                    elim, env,
                );
            }
            changed
        },
        rumoca_sim::runtime::layout::sync_solver_values_from_env,
    )
}

pub(super) type BdfTraceCtx = rumoca_sim::RuntimeTraceContext;
pub(super) type BdfProgressSnapshot = rumoca_sim::RuntimeProgressSnapshot;
