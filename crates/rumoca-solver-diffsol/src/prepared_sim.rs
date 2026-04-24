use super::*;
use rumoca_sim::runtime::timeout::{WallClockInstant, wall_clock_elapsed_seconds, wall_clock_now};

fn trace_projection_failed_at_time(t: f64) {
    if sim_trace_enabled() {
        eprintln!("[sim-trace] runtime projection failed at t={}", t);
    }
}

struct NoStateProjectionCaches {
    jacobian: Option<nalgebra::DMatrix<f64>>,
    seed_env: Option<rumoca_eval_dae::runtime::VarEnv<f64>>,
    y_scratch: Vec<f64>,
    newton_scratch: problem::RuntimeProjectionScratch,
}

struct AlgebraicResultSetup {
    times: Vec<f64>,
    eval_times: Vec<f64>,
    clock_events: Vec<f64>,
    y: Vec<f64>,
    param_values: Vec<f64>,
    n_x: usize,
    all_names: Vec<String>,
    visible_name_set: HashSet<String>,
    solver_name_to_idx: HashMap<String, usize>,
    runtime_direct_assignment_ctx: rumoca_sim::runtime::assignment::RuntimeDirectAssignmentContext,
    runtime_alias_ctx: rumoca_sim::runtime::alias::RuntimeAliasPropagationContext,
    needs_eliminated_env: bool,
    dynamic_time_event_names: Vec<String>,
    direct_seed_ctx: problem::RuntimeDirectSeedContext,
    requires_projection: bool,
    projection_needs_event_refresh: bool,
    projection_runtime_ctx: Option<problem::CompiledRuntimeNewtonContext>,
    projection_masks: Option<problem::RuntimeProjectionMasks>,
}

struct NoStateTimelineSetup {
    y: Vec<f64>,
    param_values: Vec<f64>,
    clock_events: Vec<f64>,
    times: Vec<f64>,
    eval_times: Vec<f64>,
}

struct NoStateRuntimeSetup {
    all_names: Vec<String>,
    visible_name_set: HashSet<String>,
    solver_name_to_idx: HashMap<String, usize>,
    runtime_direct_assignment_ctx: rumoca_sim::runtime::assignment::RuntimeDirectAssignmentContext,
    runtime_alias_ctx: rumoca_sim::runtime::alias::RuntimeAliasPropagationContext,
    needs_eliminated_env: bool,
    dynamic_time_event_names: Vec<String>,
    direct_seed_ctx: problem::RuntimeDirectSeedContext,
    requires_projection: bool,
    projection_needs_event_refresh: bool,
    projection_runtime_ctx: Option<problem::CompiledRuntimeNewtonContext>,
    projection_masks: Option<problem::RuntimeProjectionMasks>,
}

fn trace_no_state_setup_stage(trace_enabled: bool, label: &str, started_at: WallClockInstant) {
    if trace_enabled {
        eprintln!(
            "[sim-trace] no-state setup {label} {:.3}s",
            wall_clock_elapsed_seconds(started_at)
        );
    }
}

fn build_no_state_timeline_setup(
    dae: &Dae,
    opts: &SimOptions,
    param_values: Vec<f64>,
    n_total: usize,
    trace_setup_timing: bool,
) -> Result<NoStateTimelineSetup, SimError> {
    let dt = opts.dt.unwrap_or(opts.t_end / 500.0);
    let coarse_times = timeline::build_output_times(opts.t_start, opts.t_end, dt);
    let mut y = vec![0.0; n_total];
    problem::initialize_state_vector_with_params(dae, &mut y, &param_values);

    let stage_started = wall_clock_now();
    dump_parameter_vector_for_diffsol(dae, &param_values);
    let clock_events =
        collect_no_state_schedule_events(dae, &param_values, opts.t_start, opts.t_end);
    trace_no_state_setup_stage(trace_setup_timing, "collect_schedule_events", stage_started);
    if sim_introspect_enabled() {
        let preview: Vec<f64> = clock_events.iter().copied().take(12).collect();
        eprintln!(
            "[sim-introspect] no-state clock events count={} preview={:?}",
            clock_events.len(),
            preview
        );
    }
    let times = timeline::merge_output_times_with_event_observations(
        &coarse_times,
        &clock_events,
        opts.t_end,
    );
    let eval_times = timeline::merge_evaluation_times(&times, &clock_events);
    Ok(NoStateTimelineSetup {
        y,
        param_values,
        clock_events,
        times,
        eval_times,
    })
}

fn build_no_state_runtime_setup(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    y_len: usize,
    n_x: usize,
    trace_setup_timing: bool,
) -> Result<NoStateRuntimeSetup, SimError> {
    let stage_started = wall_clock_now();
    let runtime_direct_assignment_ctx =
        rumoca_sim::runtime::assignment::build_runtime_direct_assignment_context(dae, y_len, n_x);
    trace_no_state_setup_stage(
        trace_setup_timing,
        "build_runtime_direct_assignment_ctx",
        stage_started,
    );
    let stage_started = wall_clock_now();
    let runtime_alias_ctx =
        rumoca_sim::runtime::alias::build_runtime_alias_propagation_context(dae, y_len, n_x);
    trace_no_state_setup_stage(trace_setup_timing, "build_runtime_alias_ctx", stage_started);

    let visible_names = build_visible_result_names(dae);
    let mut all_names = visible_names.clone();
    all_names.extend(rumoca_sim::collect_reconstruction_discrete_context_names(
        dae, elim, &all_names,
    ));
    let needs_eliminated_env =
        rumoca_sim::runtime::no_state::sampled_names_need_eliminated_env_with_runtime_closure(
            &all_names,
            elim,
            &runtime_direct_assignment_ctx,
            &runtime_alias_ctx,
        );
    let stage_started = wall_clock_now();
    let dynamic_time_event_names =
        rumoca_sim::runtime::no_state::collect_dynamic_time_event_names(dae);
    trace_no_state_setup_stage(
        trace_setup_timing,
        "collect_dynamic_time_event_names",
        stage_started,
    );
    let visible_name_set: HashSet<String> = visible_names
        .iter()
        .filter(|name| *name != DUMMY_STATE_NAME)
        .cloned()
        .collect();
    let solver_names = rumoca_sim::runtime::layout::solver_vector_names(dae, y_len);
    let solver_name_to_idx: HashMap<String, usize> = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    let stage_started = wall_clock_now();
    let requires_projection = problem::no_state_runtime_projection_required(dae, n_x);
    trace_no_state_setup_stage(trace_setup_timing, "classify_projection", stage_started);
    let stage_started = wall_clock_now();
    let direct_seed_ctx = problem::build_runtime_direct_seed_context(dae, y_len, n_x);
    trace_no_state_setup_stage(
        trace_setup_timing,
        "build_runtime_direct_seed_ctx",
        stage_started,
    );
    let projection_needs_event_refresh = requires_projection
        && (rumoca_sim::runtime::no_state::no_state_projection_needs_event_refresh(dae)
            || rumoca_sim::runtime::no_state::no_state_projection_uses_lowered_pre_next_event_aliases(
                dae,
            ));
    let projection_runtime_ctx = requires_projection
        .then(|| build_no_state_projection_runtime_ctx(dae, y_len))
        .transpose()?
        .flatten();
    let projection_masks =
        requires_projection.then(|| problem::build_runtime_projection_masks(dae, n_x, y_len));
    Ok(NoStateRuntimeSetup {
        all_names,
        visible_name_set,
        solver_name_to_idx,
        runtime_direct_assignment_ctx,
        runtime_alias_ctx,
        needs_eliminated_env,
        dynamic_time_event_names,
        direct_seed_ctx,
        requires_projection,
        projection_needs_event_refresh,
        projection_runtime_ctx,
        projection_masks,
    })
}

fn build_no_state_projection_runtime_ctx(
    dae: &Dae,
    n_total: usize,
) -> Result<Option<problem::CompiledRuntimeNewtonContext>, SimError> {
    match problem::build_compiled_runtime_newton_context(dae, n_total) {
        Ok(ctx) => Ok(Some(ctx)),
        // MLS §3.7.5 / Appendix B / §16.5.1: no-state projection still has to
        // preserve event/right-limit runtime semantics even when the compiled
        // PR2 path cannot represent history/tick-sensitive operators such as
        // `change(...)`. Keep the runtime projection path alive instead of
        // failing the simulation setup outright.
        Err(SimError::CompiledEval(_)) => Ok(None),
        Err(err) => Err(err),
    }
}

fn expr_periodic_schedule(
    dae: &Dae,
    expr: &Expression,
    env: &eval::VarEnv<f64>,
) -> Option<dae::ClockSchedule> {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
        } if args.len() >= 2 => {
            let timing = if rumoca_sim::runtime::clock::sample_clock_arg_is_explicit_clock(
                dae, &args[1], env,
            ) {
                eval::infer_clock_timing_seconds(&args[1], env)
            } else {
                let start = eval::eval_expr::<f64>(&args[0], env);
                let period = eval::eval_expr::<f64>(&args[1], env);
                Some((period, start))
            }?;
            (timing.0.is_finite() && timing.0 > 0.0 && timing.1.is_finite()).then_some(
                dae::ClockSchedule {
                    period_seconds: timing.0,
                    phase_seconds: timing.1,
                },
            )
        }
        _ => eval::infer_clock_timing_seconds(expr, env).and_then(|(period, phase)| {
            (period.is_finite() && period > 0.0 && phase.is_finite()).then_some(
                dae::ClockSchedule {
                    period_seconds: period,
                    phase_seconds: phase,
                },
            )
        }),
    }
}

fn expr_is_time_var(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::VarRef { name, subscripts }
            if name.as_str() == "time" && subscripts.is_empty()
    )
}

fn expr_is_pre_plus_one(expr: &Expression) -> bool {
    match expr {
        Expression::Binary {
            op: OpBinary::Add(_),
            lhs,
            rhs,
        } => {
            (matches!(
                lhs.as_ref(),
                Expression::BuiltinCall {
                    function: BuiltinFunction::Pre,
                    args,
                } if args.len() == 1
            ) && matches!(rhs.as_ref(), Expression::Literal(dae::Literal::Integer(1))))
                || (matches!(
                    rhs.as_ref(),
                    Expression::BuiltinCall {
                        function: BuiltinFunction::Pre,
                        args,
                    } if args.len() == 1
                ) && matches!(lhs.as_ref(), Expression::Literal(dae::Literal::Integer(1))))
        }
        _ => false,
    }
}

fn extract_periodic_time_guard_schedule_expr(
    expr: &Expression,
) -> Option<(&Expression, &Expression)> {
    let Expression::Binary { op, lhs, rhs } = expr else {
        return None;
    };
    if !matches!(
        op,
        OpBinary::Ge(_) | OpBinary::Gt(_) | OpBinary::Le(_) | OpBinary::Lt(_)
    ) {
        return None;
    }
    let threshold = if expr_is_time_var(lhs) {
        rhs.as_ref()
    } else if expr_is_time_var(rhs) {
        lhs.as_ref()
    } else {
        return None;
    };
    let Expression::Binary {
        op: add_op,
        lhs: add_lhs,
        rhs: add_rhs,
    } = threshold
    else {
        return None;
    };
    if !matches!(add_op, OpBinary::Add(_)) {
        return None;
    }
    let (period_expr, phase_expr) = if let Expression::Binary {
        op: mul_op,
        lhs: mul_lhs,
        rhs: mul_rhs,
    } = add_lhs.as_ref()
    {
        if matches!(mul_op, OpBinary::Mul(_)) && expr_is_pre_plus_one(mul_lhs) {
            (mul_rhs.as_ref(), add_rhs.as_ref())
        } else if matches!(mul_op, OpBinary::Mul(_)) && expr_is_pre_plus_one(mul_rhs) {
            (mul_lhs.as_ref(), add_rhs.as_ref())
        } else {
            return None;
        }
    } else if let Expression::Binary {
        op: mul_op,
        lhs: mul_lhs,
        rhs: mul_rhs,
    } = add_rhs.as_ref()
    {
        if matches!(mul_op, OpBinary::Mul(_)) && expr_is_pre_plus_one(mul_lhs) {
            (mul_rhs.as_ref(), add_lhs.as_ref())
        } else if matches!(mul_op, OpBinary::Mul(_)) && expr_is_pre_plus_one(mul_rhs) {
            (mul_lhs.as_ref(), add_lhs.as_ref())
        } else {
            return None;
        }
    } else {
        return None;
    };
    Some((period_expr, phase_expr))
}

fn expr_periodic_guard_schedule(
    expr: &Expression,
    env: &eval::VarEnv<f64>,
) -> Option<dae::ClockSchedule> {
    let (period_expr, phase_expr) = extract_periodic_time_guard_schedule_expr(expr)?;
    let period = eval::eval_expr::<f64>(period_expr, env);
    let phase = eval::eval_expr::<f64>(phase_expr, env);
    (period.is_finite() && period > 0.0 && phase.is_finite()).then_some(dae::ClockSchedule {
        period_seconds: period,
        phase_seconds: phase,
    })
}

fn expr_is_schedule_free_data_literal(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_) | Expression::Empty => true,
        Expression::VarRef { name, subscripts } => subscripts.is_empty() && name.as_str() != "time",
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            expr_is_schedule_free_data_literal(rhs)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().all(expr_is_schedule_free_data_literal)
        }
        _ => false,
    }
}

fn function_name_may_define_periodic_schedule(name: &VarName) -> bool {
    matches!(
        name.as_str().rsplit('.').next().unwrap_or(name.as_str()),
        // MLS §16.5 / §16.7 clock constructors and clock transforms can
        // define periodic schedules for no-state observation points.
        "Clock"
            | "hold"
            | "previous"
            | "noClock"
            | "firstTick"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
    )
}

fn expr_may_define_periodic_schedule(expr: &Expression) -> bool {
    match expr {
        Expression::Literal(_) | Expression::Empty => false,
        Expression::VarRef { name, subscripts } => subscripts.is_empty() && name.as_str() == "time",
        Expression::BuiltinCall { function, args } => {
            matches!(function, BuiltinFunction::Sample | BuiltinFunction::Pre)
                || args.iter().any(expr_may_define_periodic_schedule)
        }
        Expression::FunctionCall { name, args, .. } => {
            function_name_may_define_periodic_schedule(name)
                || args.iter().any(expr_may_define_periodic_schedule)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_is_time_var(lhs)
                || expr_is_time_var(rhs)
                || expr_may_define_periodic_schedule(lhs)
                || expr_may_define_periodic_schedule(rhs)
        }
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            expr_may_define_periodic_schedule(rhs)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_may_define_periodic_schedule(cond) || expr_may_define_periodic_schedule(value)
            }) || expr_may_define_periodic_schedule(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_may_define_periodic_schedule)
        }
        Expression::Range { start, step, end } => {
            expr_may_define_periodic_schedule(start)
                || step
                    .as_deref()
                    .is_some_and(expr_may_define_periodic_schedule)
                || expr_may_define_periodic_schedule(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_may_define_periodic_schedule(expr)
                || indices
                    .iter()
                    .any(|index| expr_may_define_periodic_schedule(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_may_define_periodic_schedule)
        }
        Expression::Index { base, subscripts } => {
            (!expr_is_schedule_free_data_literal(base) && expr_may_define_periodic_schedule(base))
                || subscripts.iter().any(|subscript| match subscript {
                    Subscript::Expr(expr) => expr_may_define_periodic_schedule(expr),
                    Subscript::Index(_) | Subscript::Colon => false,
                })
        }
    }
}

fn collect_expr_periodic_schedules(
    dae: &Dae,
    expr: &Expression,
    env: &eval::VarEnv<f64>,
    schedules: &mut Vec<dae::ClockSchedule>,
) {
    if !expr_may_define_periodic_schedule(expr) {
        return;
    }
    if let Some(schedule) = expr_periodic_schedule(dae, expr, env) {
        schedules.push(schedule);
    }
    if let Some(schedule) = expr_periodic_guard_schedule(expr, env) {
        schedules.push(schedule);
    }
    match expr {
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_expr_periodic_schedules(dae, arg, env, schedules);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_expr_periodic_schedules(dae, lhs, env, schedules);
            collect_expr_periodic_schedules(dae, rhs, env, schedules);
        }
        Expression::Unary { rhs, .. } => {
            collect_expr_periodic_schedules(dae, rhs, env, schedules);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                collect_expr_periodic_schedules(dae, cond, env, schedules);
                collect_expr_periodic_schedules(dae, value, env, schedules);
            }
            collect_expr_periodic_schedules(dae, else_branch, env, schedules);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                collect_expr_periodic_schedules(dae, element, env, schedules);
            }
        }
        Expression::Range { start, step, end } => {
            collect_expr_periodic_schedules(dae, start, env, schedules);
            if let Some(step) = step.as_deref() {
                collect_expr_periodic_schedules(dae, step, env, schedules);
            }
            collect_expr_periodic_schedules(dae, end, env, schedules);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_expr_periodic_schedules(dae, expr, env, schedules);
            for index in indices {
                collect_expr_periodic_schedules(dae, &index.range, env, schedules);
            }
            if let Some(filter) = filter.as_deref() {
                collect_expr_periodic_schedules(dae, filter, env, schedules);
            }
        }
        Expression::Index { base, subscripts } => {
            if !expr_is_schedule_free_data_literal(base) {
                collect_expr_periodic_schedules(dae, base, env, schedules);
            }
            for subscript in subscripts {
                if let Subscript::Expr(expr) = subscript {
                    collect_expr_periodic_schedules(dae, expr, env, schedules);
                }
            }
        }
        Expression::FieldAccess { base, .. } => {
            collect_expr_periodic_schedules(dae, base, env, schedules);
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
}

pub(crate) fn collect_no_state_schedule_events(
    dae: &Dae,
    param_values: &[f64],
    t_start: f64,
    t_end: f64,
) -> Vec<f64> {
    let env = eval::build_runtime_parameter_tail_env(dae, param_values, t_start);
    let mut schedules = Vec::new();
    let use_runtime_precompute_metadata = !dae.scheduled_time_events.is_empty()
        || !dae.clock_schedules.is_empty()
        || !dae.triggered_clock_conditions.is_empty()
        || !dae.clock_constructor_exprs.is_empty();
    let exprs: Vec<&Expression> = if use_runtime_precompute_metadata {
        dae.triggered_clock_conditions
            .iter()
            .chain(dae.clock_constructor_exprs.iter())
            .collect()
    } else {
        dae.f_x
            .iter()
            .chain(dae.f_z.iter())
            .chain(dae.f_m.iter())
            .chain(dae.f_c.iter())
            .map(|eq| &eq.rhs)
            .chain(dae.synthetic_root_conditions.iter())
            .collect()
    };
    for expr in exprs {
        collect_expr_periodic_schedules(dae, expr, &env, &mut schedules);
    }

    let mut events = timeline::collect_runtime_schedule_events(dae, t_start, t_end);
    events.extend(timeline::collect_periodic_clock_events(
        &schedules, t_start, t_end,
    ));
    events.sort_by(f64::total_cmp);
    events.dedup_by(|a, b| timeline::sample_time_match_with_tol(*a, *b));
    events
}

fn prepare_algebraic_result_setup(
    dae: &Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &TimeoutBudget,
    parameter_overrides: &IndexMap<String, Vec<f64>>,
) -> Result<AlgebraicResultSetup, SimError> {
    let trace_setup_timing = sim_trace_enabled();
    let n_total = dae.f_x.len();
    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    let stage_started = wall_clock_now();
    let param_values = build_parameter_values_with_overrides(dae, budget, parameter_overrides)?;
    trace_no_state_setup_stage(trace_setup_timing, "build_parameter_values", stage_started);
    let NoStateTimelineSetup {
        y,
        param_values,
        clock_events,
        times,
        eval_times,
    } = build_no_state_timeline_setup(dae, opts, param_values, n_total, trace_setup_timing)?;
    let NoStateRuntimeSetup {
        all_names,
        visible_name_set,
        solver_name_to_idx,
        runtime_direct_assignment_ctx,
        runtime_alias_ctx,
        needs_eliminated_env,
        dynamic_time_event_names,
        direct_seed_ctx,
        requires_projection,
        projection_needs_event_refresh,
        projection_runtime_ctx,
        projection_masks,
    } = build_no_state_runtime_setup(dae, elim, y.len(), n_x, trace_setup_timing)?;
    if std::env::var_os("RUMOCA_DEBUG_COUNTER_ENABLE").is_some() {
        let mut solver_names: Vec<(usize, String)> = solver_name_to_idx
            .iter()
            .map(|(name, idx)| (*idx, name.clone()))
            .collect();
        solver_names.sort_by_key(|(idx, _)| *idx);
        let solver_names: Vec<String> = solver_names.into_iter().map(|(_, name)| name).collect();
        let fx_targets: Vec<String> = dae
            .f_x
            .iter()
            .filter_map(|eq| eq.lhs.as_ref().map(|lhs| lhs.as_str().to_string()))
            .collect();
        let fz_targets: Vec<String> = dae
            .f_z
            .iter()
            .filter_map(|eq| eq.lhs.as_ref().map(|lhs| lhs.as_str().to_string()))
            .collect();
        let fm_targets: Vec<String> = dae
            .f_m
            .iter()
            .filter_map(|eq| eq.lhs.as_ref().map(|lhs| lhs.as_str().to_string()))
            .collect();
        let fc_targets: Vec<String> = dae
            .f_c
            .iter()
            .filter_map(|eq| eq.lhs.as_ref().map(|lhs| lhs.as_str().to_string()))
            .collect();
        let trace_eq = |label: &str, equations: &[dae::Equation]| {
            for (idx, eq) in equations.iter().enumerate() {
                let rhs_dbg = format!("{:?}", eq.rhs);
                let lhs_dbg = eq.lhs.as_ref().map(|lhs| lhs.as_str()).unwrap_or("<none>");
                trace_enable_equation(label, idx, eq, &rhs_dbg, lhs_dbg);
            }
        };
        eprintln!(
            "DEBUG setup n_x={n_x} n_total={n_total} requires_projection={requires_projection} projection_needs_event_refresh={projection_needs_event_refresh} all_names={:?} solver_names={:?} fx_targets={:?} fz_targets={:?} fm_targets={:?} fc_targets={:?}",
            all_names, solver_names, fx_targets, fz_targets, fm_targets, fc_targets,
        );
        trace_eq("f_x", &dae.f_x);
        trace_eq("f_z", &dae.f_z);
        trace_eq("f_m", &dae.f_m);
    }
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] no-state runtime projection required={}",
            requires_projection
        );
    }

    Ok(AlgebraicResultSetup {
        times,
        eval_times,
        clock_events,
        y,
        param_values,
        n_x,
        all_names,
        visible_name_set,
        solver_name_to_idx,
        runtime_direct_assignment_ctx,
        runtime_alias_ctx,
        needs_eliminated_env,
        dynamic_time_event_names,
        direct_seed_ctx,
        requires_projection,
        projection_needs_event_refresh,
        projection_runtime_ctx,
        projection_masks,
    })
}

fn project_no_state_sample(
    dae: &Dae,
    opts: &SimOptions,
    budget: &TimeoutBudget,
    setup: &AlgebraicResultSetup,
    y_values: &mut [f64],
    t: f64,
    caches: &mut NoStateProjectionCaches,
) -> Result<bool, SimError> {
    let projection_masks = setup
        .projection_masks
        .as_ref()
        .expect("projection masks required when projection is enabled");
    caches.y_scratch.clear();
    caches.y_scratch.extend_from_slice(y_values);
    if let Some(cached_jacobian) = caches.jacobian.as_ref()
        && problem::project_algebraics_with_cached_runtime_jacobian_step_in_place(
            dae,
            caches.y_scratch.as_mut_slice(),
            problem::RuntimeProjectionContext {
                p: &setup.param_values,
                compiled_runtime: setup.projection_runtime_ctx.as_ref(),
                fixed_cols: &projection_masks.fixed_cols,
                ignored_rows: &projection_masks.ignored_rows,
                branch_local_analog_cols: &projection_masks.branch_local_analog_cols,
                direct_seed_ctx: Some(&setup.direct_seed_ctx),
                direct_seed_env_cache: Some(&mut caches.seed_env),
            },
            problem::RuntimeProjectionStep {
                y_seed: y_values,
                n_x: setup.n_x,
                t_eval: t,
                tol: opts.atol.max(1.0e-8),
                timeout: budget,
            },
            cached_jacobian,
            &mut caches.newton_scratch,
        )?
    {
        y_values.copy_from_slice(caches.y_scratch.as_slice());
        return Ok(true);
    }

    caches.y_scratch.clear();
    caches.y_scratch.extend_from_slice(y_values);
    if problem::project_algebraics_with_fixed_states_at_time_with_context_and_cache_in_place(
        dae,
        caches.y_scratch.as_mut_slice(),
        problem::RuntimeProjectionContext {
            p: &setup.param_values,
            compiled_runtime: setup.projection_runtime_ctx.as_ref(),
            fixed_cols: &projection_masks.fixed_cols,
            ignored_rows: &projection_masks.ignored_rows,
            branch_local_analog_cols: &projection_masks.branch_local_analog_cols,
            direct_seed_ctx: Some(&setup.direct_seed_ctx),
            direct_seed_env_cache: Some(&mut caches.seed_env),
        },
        problem::RuntimeProjectionStep {
            y_seed: y_values,
            n_x: setup.n_x,
            t_eval: t,
            tol: opts.atol.max(1.0e-8),
            timeout: budget,
        },
        Some(&mut caches.jacobian),
        &mut caches.newton_scratch,
    )? {
        y_values.copy_from_slice(caches.y_scratch.as_slice());
        return Ok(true);
    }
    Ok(false)
}

fn copy_discrete_runtime_tail_binding(
    dst: &mut rumoca_eval_dae::runtime::VarEnv<f64>,
    src: &rumoca_eval_dae::runtime::VarEnv<f64>,
    name: &str,
) {
    if let Some(value) = src.vars.get(name) {
        dst.set(name, *value);
    }
    let prefix = format!("{name}[");
    for (key, value) in src
        .vars
        .iter()
        .filter(|(key, _)| key.starts_with(prefix.as_str()))
    {
        dst.set(key, *value);
    }
}

fn bootstrap_initial_runtime_direct_seed_env(
    sample_ctx: &rumoca_sim::NoStateSampleContext<'_>,
    dae: &Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> rumoca_eval_dae::runtime::VarEnv<f64> {
    let mut startup_y = y.to_vec();
    let settled = rumoca_sim::runtime::no_state::build_initial_settled_runtime_env(
        sample_ctx,
        startup_y.as_mut_slice(),
        t,
    );
    let mut env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(dae, p, t);
    // MLS §8.6 / Appendix B: the first ordinary runtime projection after the
    // initial event must observe the settled current discrete values from the
    // initialization section, not the raw runtime-tail starts or stale pre(v).
    for (name, _) in dae.discrete_reals.iter().chain(dae.discrete_valued.iter()) {
        copy_discrete_runtime_tail_binding(&mut env, &settled, name.as_str());
    }
    env
}

fn collect_no_state_sample_data(
    dae: &Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &TimeoutBudget,
    setup: &AlgebraicResultSetup,
) -> Result<(Vec<f64>, Vec<Vec<f64>>), SimError> {
    let sample_ctx = build_no_state_sample_context(dae, elim, opts, setup);
    let projection_run = NoStateProjectionRun {
        sample_ctx: &sample_ctx,
        opts,
        budget,
        setup,
    };
    let mut projection_caches = NoStateProjectionCaches {
        jacobian: None,
        seed_env: None,
        y_scratch: Vec::new(),
        newton_scratch: problem::RuntimeProjectionScratch::default(),
    };

    let mut output_times = setup.times.clone();
    let (_, output_times, data) = rumoca_sim::runtime::no_state::collect_algebraic_samples_with_schedule_and_env_refresh(
        &sample_ctx,
        &mut output_times,
        &setup.eval_times,
        setup.y.clone(),
        || budget.check().map_err(SimError::from),
        |y_values, t, do_projection| {
            project_or_seed_no_state_sample(
                &projection_run,
                y_values,
                t,
                do_projection,
                &mut projection_caches,
            )
        },
        |y_values, t, env| refresh_no_state_projected_env(&projection_run, y_values, t, env),
    )
    .map_err(|err| match err {
        rumoca_sim::NoStateSampleError::Callback(sim_err) => sim_err,
        rumoca_sim::NoStateSampleError::SampleScheduleMismatch { captured, expected } => {
            SimError::SolverError(format!(
                "no-state sample schedule mismatch: captured {captured}/{expected} output samples"
            ))
        }
    })?;

    Ok((output_times, data))
}

fn build_no_state_sample_context<'a>(
    dae: &'a Dae,
    elim: &'a eliminate::EliminationResult,
    opts: &'a SimOptions,
    setup: &'a AlgebraicResultSetup,
) -> rumoca_sim::NoStateSampleContext<'a> {
    rumoca_sim::NoStateSampleContext {
        dae,
        elim,
        param_values: &setup.param_values,
        all_names: &setup.all_names,
        clock_event_times: &setup.clock_events,
        direct_assignment_ctx: &setup.runtime_direct_assignment_ctx,
        alias_ctx: &setup.runtime_alias_ctx,
        needs_eliminated_env: setup.needs_eliminated_env,
        dynamic_time_event_names: &setup.dynamic_time_event_names,
        solver_name_to_idx: &setup.solver_name_to_idx,
        n_x: setup.n_x,
        t_start: opts.t_start,
        requires_projection: setup.requires_projection,
        projection_needs_event_refresh: setup.projection_needs_event_refresh,
        requires_live_pre_values: rumoca_sim::runtime::no_state::no_state_requires_live_pre_values(
            dae,
        ),
    }
}

fn seed_no_state_runtime_direct_assignments(
    setup: &AlgebraicResultSetup,
    dae: &Dae,
    y_values: &mut [f64],
    t: f64,
) {
    let _ = problem::seed_runtime_direct_assignment_values_with_context(
        &setup.direct_seed_ctx,
        dae,
        y_values,
        &setup.param_values,
        t,
    );
}

struct NoStateProjectionRun<'a> {
    sample_ctx: &'a rumoca_sim::NoStateSampleContext<'a>,
    opts: &'a SimOptions,
    budget: &'a TimeoutBudget,
    setup: &'a AlgebraicResultSetup,
}

fn project_or_seed_no_state_sample(
    run: &NoStateProjectionRun<'_>,
    y_values: &mut [f64],
    t: f64,
    do_projection: bool,
    projection_caches: &mut NoStateProjectionCaches,
) -> Result<(), SimError> {
    if projection_caches.seed_env.is_none() && (t - run.opts.t_start).abs() <= 1.0e-12 {
        projection_caches.seed_env = Some(bootstrap_initial_runtime_direct_seed_env(
            run.sample_ctx,
            run.sample_ctx.dae,
            y_values,
            &run.setup.param_values,
            t,
        ));
    }
    if do_projection
        && project_no_state_sample(
            run.sample_ctx.dae,
            run.opts,
            run.budget,
            run.setup,
            y_values,
            t,
            projection_caches,
        )?
    {
        seed_no_state_runtime_direct_assignments(run.setup, run.sample_ctx.dae, y_values, t);
        return Ok(());
    }
    if do_projection {
        trace_projection_failed_at_time(t);
    }
    seed_no_state_runtime_direct_assignments(run.setup, run.sample_ctx.dae, y_values, t);
    Ok(())
}

fn refresh_no_state_projected_env(
    run: &NoStateProjectionRun<'_>,
    y_values: &mut [f64],
    t: f64,
    env: &mut rumoca_eval_dae::runtime::VarEnv<f64>,
) -> Result<(), SimError> {
    let mut env_slot = Some(std::mem::replace(
        env,
        rumoca_eval_dae::runtime::VarEnv::new(),
    ));
    let _ = problem::seed_runtime_direct_assignment_values_with_context_and_env(
        &run.setup.direct_seed_ctx,
        run.sample_ctx.dae,
        y_values,
        &run.setup.param_values,
        t,
        Some(&mut env_slot),
    );
    let mut refreshed_env = env_slot.expect("projected no-state seed env must be restored");
    rumoca_eval_dae::runtime::refresh_env_solver_and_parameter_values(
        &mut refreshed_env,
        run.sample_ctx.dae,
        y_values,
        &run.setup.param_values,
        t,
    );
    refreshed_env.is_initial = env.is_initial;
    if rumoca_sim::runtime::discrete::apply_discrete_partition_updates(
        run.sample_ctx.dae,
        &mut refreshed_env,
    ) {
        rumoca_sim::runtime::assignment::propagate_runtime_direct_assignments_from_env_with_context(
            &run.setup.runtime_direct_assignment_ctx,
            run.sample_ctx.dae,
            y_values,
            run.setup.n_x,
            &mut refreshed_env,
        );
        rumoca_sim::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
            &run.setup.runtime_alias_ctx,
            y_values,
            run.setup.n_x,
            &mut refreshed_env,
        );
        rumoca_sim::runtime::layout::sync_solver_values_from_env(
            run.sample_ctx.dae,
            y_values,
            &refreshed_env,
        );
        rumoca_sim::runtime::alias::propagate_runtime_alias_components_from_env_with_context(
            &run.setup.runtime_alias_ctx,
            y_values,
            run.setup.n_x,
            &mut refreshed_env,
        );
    }
    *env = refreshed_env;
    Ok(())
}

fn should_trace_enable_equation(lhs_dbg: &str, rhs_dbg: &str, origin: &str) -> bool {
    lhs_dbg.contains("Enable")
        || lhs_dbg.contains("Counter.enable")
        || rhs_dbg.contains("Enable.y")
        || rhs_dbg.contains("Counter.enable")
        || origin.contains("Enable")
        || origin.contains("Counter.enable")
}

fn trace_enable_equation(
    label: &str,
    idx: usize,
    eq: &dae::Equation,
    rhs_dbg: &str,
    lhs_dbg: &str,
) {
    if !should_trace_enable_equation(lhs_dbg, rhs_dbg, &eq.origin) {
        return;
    }
    eprintln!(
        "DEBUG {label}[{idx}] lhs={lhs_dbg} origin={} rhs={rhs_dbg}",
        eq.origin
    );
}

fn filter_visible_output_series(
    recon_names: &[String],
    recon_data: &[Vec<f64>],
    visible_name_set: &HashSet<String>,
) -> (Vec<String>, Vec<Vec<f64>>) {
    let mut final_names: Vec<String> = Vec::new();
    let mut final_data: Vec<Vec<f64>> = Vec::new();
    for (name, series) in recon_names.iter().zip(recon_data.iter()) {
        if visible_name_set.contains(name) {
            final_names.push(name.clone());
            final_data.push(series.clone());
        }
    }
    (final_names, final_data)
}

fn merge_reconstructed_series(
    final_names: &mut Vec<String>,
    final_data: &mut Vec<Vec<f64>>,
    extra_names: Vec<String>,
    extra_data: Vec<Vec<f64>>,
) {
    for (name, series) in extra_names.into_iter().zip(extra_data) {
        if let Some(existing_idx) = final_names.iter().position(|existing| existing == &name) {
            final_data[existing_idx] = series;
            continue;
        }
        final_names.push(name);
        final_data.push(series);
    }
}

fn build_prepared_algebraic_simulation(
    dae: Dae,
    opts: &SimOptions,
    elim: eliminate::EliminationResult,
) -> Result<PreparedSimulation, SimError> {
    Ok(PreparedSimulation {
        dae,
        elim,
        opts: opts.clone(),
        parameter_overrides: IndexMap::new(),
        state: PreparedSimulationState::Algebraic(PreparedAlgebraicSimulation {}),
    })
}

fn build_prepared_dynamic_simulation(
    dae: Dae,
    opts: &SimOptions,
    elim: eliminate::EliminationResult,
    mass_matrix: MassMatrix,
    ic_blocks: Vec<rumoca_phase_structural::IcBlock>,
) -> Result<PreparedSimulation, SimError> {
    Ok(PreparedSimulation {
        dae,
        elim,
        opts: opts.clone(),
        parameter_overrides: IndexMap::new(),
        state: PreparedSimulationState::Dynamic(PreparedDynamicSimulation {
            mass_matrix,
            ic_blocks,
        }),
    })
}

fn run_prepared_algebraic_simulation(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    opts: &SimOptions,
    parameter_overrides: &IndexMap<String, Vec<f64>>,
    budget: &TimeoutBudget,
) -> Result<SimResult, SimError> {
    let setup = prepare_algebraic_result_setup(dae, opts, elim, budget, parameter_overrides)?;
    let (output_times, data) = collect_no_state_sample_data(dae, opts, elim, budget, &setup)?;
    let (recon_names, recon_data, final_n_states) = rumoca_sim::finalize_algebraic_outputs(
        setup.all_names.clone(),
        data,
        setup.n_x,
        DUMMY_STATE_NAME,
    );
    let (mut final_names, mut final_data) =
        filter_visible_output_series(&recon_names, &recon_data, &setup.visible_name_set);

    if !elim.substitutions.is_empty() {
        let (extra_names, extra_data) = rumoca_sim::reconstruct::reconstruct_eliminated(
            elim,
            dae,
            &setup.param_values,
            &output_times,
            &recon_names,
            &recon_data,
        );
        merge_reconstructed_series(&mut final_names, &mut final_data, extra_names, extra_data);
    }

    let variable_meta = build_variable_meta(dae, &final_names, final_n_states);
    Ok(SimResult {
        times: output_times,
        names: final_names,
        data: final_data,
        n_states: final_n_states,
        variable_meta,
    })
}

fn run_prepared_dynamic_simulation(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    opts: &SimOptions,
    state: &PreparedDynamicSimulation,
    parameter_overrides: &IndexMap<String, Vec<f64>>,
    budget: &TimeoutBudget,
    sim_start: Option<std::time::Instant>,
) -> Result<SimResult, SimError> {
    let mut dae = dae.clone();
    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    let n_total = dae.f_x.len();
    let param_values = build_parameter_values_with_overrides(&dae, budget, parameter_overrides)?;
    solve_initial_conditions(
        &mut dae,
        &state.ic_blocks,
        n_x,
        &param_values,
        opts.atol,
        budget,
    )?;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage solve_initial_conditions {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    validate_no_initial_division_by_zero(&dae, &param_values, opts.t_start)?;
    dump_initial_vector_for_diffsol(&dae, &param_values);
    dump_initial_residual_summary_for_diffsol(&dae, n_x, &param_values)?;
    let (buf, _) = run_with_timeout_panic_handling(budget, || {
        integrate_with_fallbacks(
            &dae,
            elim,
            opts,
            n_total,
            &state.mass_matrix,
            &param_values,
            budget,
        )
    })?;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage integrate_with_fallbacks {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    Ok(finalize_dynamic_result(
        &dae,
        elim,
        &param_values,
        n_x,
        n_total,
        buf,
    ))
}

fn run_with_timeout_panic_handling<T, F>(budget: &TimeoutBudget, f: F) -> Result<T, SimError>
where
    F: FnOnce() -> Result<T, SimError>,
{
    let _solver_deadline_guard = SolverDeadlineGuard::install(budget.deadline());
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(payload) => {
            if is_solver_timeout_panic(payload.as_ref()) {
                return Err(budget.timeout_error().into());
            }
            Err(SimError::SolverError(format!(
                "integration panic: {}",
                panic_payload_message(payload)
            )))
        }
    }
}

fn finalize_dynamic_result(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    param_values: &[f64],
    n_x: usize,
    n_total: usize,
    buf: OutputBuffers,
) -> SimResult {
    let mut names = build_output_names(dae);
    names.truncate(n_total);
    let solver_names = names.clone();
    let OutputBuffers {
        times: output_times,
        data: output_data,
        n_total: _,
        runtime_names,
        runtime_data,
    } = buf;
    let mut refreshed_output_data = output_data;
    refresh_runtime_observed_solver_channels(
        dae,
        n_x,
        param_values,
        &output_times,
        &solver_names,
        &mut refreshed_output_data,
    );
    let (mut final_names, mut final_data, final_n_states) = (names, refreshed_output_data, n_x);
    let runtime_capture_complete =
        !runtime_names.is_empty() && runtime_data.iter().all(|s| s.len() == output_times.len());
    if runtime_capture_complete {
        merge_runtime_discrete_channels(
            &mut final_names,
            &mut final_data,
            runtime_names,
            runtime_data,
        );
    }
    let (discrete_names, discrete_data) = evaluate_runtime_discrete_channels(
        dae,
        n_x,
        param_values,
        &output_times,
        &solver_names,
        &final_data,
    );
    merge_runtime_discrete_channels(
        &mut final_names,
        &mut final_data,
        discrete_names,
        discrete_data,
    );
    if !elim.substitutions.is_empty() {
        let (extra_names, extra_data) = rumoca_sim::reconstruct::reconstruct_eliminated(
            elim,
            dae,
            param_values,
            &output_times,
            &final_names,
            &final_data,
        );
        merge_reconstructed_series(&mut final_names, &mut final_data, extra_names, extra_data);
    }
    let variable_meta = build_variable_meta(dae, &final_names, final_n_states);
    SimResult {
        times: output_times,
        names: final_names,
        data: final_data,
        n_states: final_n_states,
        variable_meta,
    }
}

pub fn build_simulation(dae: &Dae, opts: &SimOptions) -> Result<PreparedSimulation, SimError> {
    eval::clear_pre_values();
    rumoca_sim::runtime::clock::reset_runtime_clock_caches();
    let budget = TimeoutBudget::new(opts.max_wall_seconds);
    (|| {
        validate_simulation_function_support(dae)?;
        let sim_start = trace_timer_start_if(sim_trace_enabled());
        let prepared = prepare_dae(dae, opts.scalarize, &budget)?;
        let dae = prepared.dae;
        let has_dummy = prepared.has_dummy_state;
        let elim = prepared.elimination;
        let ic_blocks = prepared.ic_blocks;
        let mass_matrix = prepared.mass_matrix;
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] stage prepare_dae {:.3}s",
                trace_timer_elapsed_seconds(sim_start)
            );
        }
        validate_simulation_function_support(&dae)?;
        dump_transformed_dae_for_diffsol(&dae, &mass_matrix);

        if has_dummy {
            return build_prepared_algebraic_simulation(dae, opts, elim);
        }
        build_prepared_dynamic_simulation(dae, opts, elim, mass_matrix, ic_blocks)
    })()
}

pub(crate) fn validate_parameter_override(
    prepared: &PreparedSimulation,
    name: &str,
    values: &[f64],
) -> Result<(), SimError> {
    let Some((_, _, expected_len)) = parameter_slice_range(&prepared.dae, name) else {
        return Err(SimError::SolverError(format!(
            "unknown parameter override '{name}'"
        )));
    };
    if values.len() != expected_len {
        return Err(SimError::SolverError(format!(
            "parameter override '{name}' expected {expected_len} value(s), got {}",
            values.len()
        )));
    }
    Ok(())
}

pub fn run_prepared_simulation(prepared: &PreparedSimulation) -> Result<SimResult, SimError> {
    eval::clear_pre_values();
    rumoca_sim::runtime::hotpath_stats::reset();
    rumoca_sim::runtime::clock::reset_runtime_clock_caches();
    let budget = TimeoutBudget::new(prepared.opts.max_wall_seconds);
    let sim_start = trace_timer_start_if(sim_trace_enabled());
    let result = match &prepared.state {
        PreparedSimulationState::Algebraic(_state) => run_timeout_result(&budget, || {
            run_prepared_algebraic_simulation(
                &prepared.dae,
                &prepared.elim,
                &prepared.opts,
                &prepared.parameter_overrides,
                &budget,
            )
        }),
        PreparedSimulationState::Dynamic(state) => run_prepared_dynamic_simulation(
            &prepared.dae,
            &prepared.elim,
            &prepared.opts,
            state,
            &prepared.parameter_overrides,
            &budget,
            sim_start,
        ),
    };
    dump_hotpath_stats_if_enabled();
    result
}

pub fn simulate(dae: &Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    let prepared = build_simulation(dae, opts)?;
    run_prepared_simulation(&prepared)
}

#[cfg(test)]
mod schedule_scan_tests {
    use super::*;

    fn int_lit(value: i64) -> Expression {
        Expression::Literal(dae::Literal::Integer(value))
    }

    fn real_lit(value: f64) -> Expression {
        Expression::Literal(dae::Literal::Real(value))
    }

    fn make_lookup_table(size: usize) -> Expression {
        let row = Expression::Array {
            elements: (0..size).map(|idx| int_lit(idx as i64)).collect(),
            is_matrix: true,
        };
        Expression::Array {
            elements: std::iter::repeat_n(row, size).collect(),
            is_matrix: true,
        }
    }

    #[test]
    fn schedule_free_data_literal_accepts_nested_lookup_tables() {
        let expr = Expression::Index {
            base: Box::new(make_lookup_table(8)),
            subscripts: vec![
                Subscript::Expr(Box::new(int_lit(1))),
                Subscript::Expr(Box::new(int_lit(2))),
            ],
        };

        assert!(expr_is_schedule_free_data_literal(match &expr {
            Expression::Index { base, .. } => base,
            _ => unreachable!(),
        }));
        assert!(!expr_may_define_periodic_schedule(&expr));
    }

    #[test]
    fn collect_no_state_schedule_events_keeps_real_sample_schedule_beside_lookup_table() {
        let mut dae = Dae::default();
        dae.f_x.push(dae::Equation::explicit(
            VarName::new("tick"),
            Expression::BuiltinCall {
                function: BuiltinFunction::Sample,
                args: vec![real_lit(0.0), real_lit(0.5)],
            },
            rumoca_core::Span::DUMMY,
            "tick = sample(0, 0.5)",
        ));
        dae.f_x.push(dae::Equation::explicit(
            VarName::new("lookup"),
            Expression::Index {
                base: Box::new(make_lookup_table(20)),
                subscripts: vec![
                    Subscript::Expr(Box::new(int_lit(1))),
                    Subscript::Expr(Box::new(int_lit(2))),
                ],
            },
            rumoca_core::Span::DUMMY,
            "lookup = table[1, 2]",
        ));

        let events = collect_no_state_schedule_events(&dae, &[], 0.0, 1.0);

        assert_eq!(events, vec![0.0, 0.5]);
    }

    #[test]
    fn build_output_times_handles_zero_step_singleton_interval() {
        assert_eq!(timeline::build_output_times(1.0, 1.0, 0.0), vec![1.0]);
    }

    #[test]
    fn build_output_times_handles_zero_step_distinct_interval() {
        assert_eq!(timeline::build_output_times(1.0, 2.0, 0.0), vec![1.0, 2.0]);
    }
}
