use super::*;

fn runtime_output_value(
    ctx: &NoStateSampleContext<'_>,
    env: &eval::VarEnv<f64>,
    y: &[f64],
    name: &str,
) -> f64 {
    env.vars
        .get(name)
        .copied()
        .or_else(|| {
            ctx.solver_name_to_idx
                .get(name)
                .and_then(|idx| y.get(*idx).copied())
        })
        .unwrap_or(0.0)
}

pub(crate) fn can_sample_solver_outputs_directly(ctx: &NoStateSampleContext<'_>) -> bool {
    let needs_inter_sample_pre = no_state_requires_inter_sample_pre_values(ctx.dae);
    let only_initial_event_markers = ctx
        .clock_event_times
        .iter()
        .all(|event_t| timeline::sample_time_match_with_tol(*event_t, ctx.t_start));
    let solver_backed_names = ctx
        .all_names
        .iter()
        .all(|name| ctx.solver_name_to_idx.contains_key(name.as_str()));
    let strict_solver_only = ctx.requires_projection
        && ctx.clock_event_times.is_empty()
        && !ctx.projection_needs_event_refresh
        && !ctx.requires_live_pre_values
        && !ctx.needs_eliminated_env
        && solver_backed_names;
    let pure_continuous_initial_only = ctx.requires_projection
        && only_initial_event_markers
        && !ctx.needs_eliminated_env
        && !needs_inter_sample_pre
        && !no_state_projection_uses_lowered_pre_next_event_aliases(ctx.dae)
        && ctx.dae.f_z.is_empty()
        && ctx.dae.f_m.is_empty()
        && ctx.dae.discrete_reals.is_empty()
        && ctx.dae.discrete_valued.is_empty()
        && solver_backed_names;
    // MLS §16.5.1: sampled/held values only require a settled discrete env when
    // event-dependent operators participate in the runtime value. If all sampled
    // names are solver-backed and there is no clock/pre/elimination dependency,
    // the projected solver vector is already the observable result. Plain
    // algebraic relation/time events do not need a second runtime-env settle
    // because evaluating the projection at the current observation time already
    // selects the correct branch. An initial observation marker at `t_start`
    // also does not require the clocked/discrete settle path.
    strict_solver_only || pure_continuous_initial_only
}

fn append_solver_output_samples_at_time(
    ctx: &NoStateSampleContext<'_>,
    output_times: &[f64],
    t: f64,
    out_idx: &mut usize,
    data: &mut [Vec<f64>],
    y: &[f64],
) {
    while *out_idx < output_times.len()
        && timeline::sample_time_match_with_tol(output_times[*out_idx], t)
    {
        for (name, series) in ctx.all_names.iter().zip(data.iter_mut()) {
            if *out_idx != series.len() {
                continue;
            }
            let value = ctx
                .solver_name_to_idx
                .get(name.as_str())
                .and_then(|idx| y.get(*idx).copied())
                .unwrap_or(0.0);
            series.push(value);
        }
        *out_idx += 1;
    }
}

fn append_output_samples_at_time(
    ctx: &NoStateSampleContext<'_>,
    output_times: &[f64],
    t: f64,
    out_idx: &mut usize,
    data: &mut [Vec<f64>],
    env: &eval::VarEnv<f64>,
    y: &[f64],
) {
    while *out_idx < output_times.len()
        && timeline::sample_time_match_with_tol(output_times[*out_idx], t)
    {
        for (name, series) in ctx.all_names.iter().zip(data.iter_mut()) {
            if *out_idx != series.len() {
                continue;
            }
            series.push(runtime_output_value(ctx, env, y, name));
        }
        *out_idx += 1;
    }
}

/// Collect sampled outputs for a no-state system.
///
/// `check_deadline` is called before each evaluation time.
/// `project_or_seed` performs backend-specific projection and/or direct-assignment seeding.
pub fn collect_algebraic_samples<E, FCheck, FProjectOrSeed>(
    ctx: &NoStateSampleContext<'_>,
    output_times: &[f64],
    evaluation_times: &[f64],
    y: Vec<f64>,
    check_deadline: FCheck,
    project_or_seed: FProjectOrSeed,
) -> NoStateSampleResult<E>
where
    FCheck: FnMut() -> Result<(), E>,
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
{
    let mut output_schedule = output_times.to_vec();
    let (y, _, data) = collect_algebraic_samples_with_schedule(
        ctx,
        &mut output_schedule,
        evaluation_times,
        y,
        check_deadline,
        project_or_seed,
    )?;
    Ok((y, data))
}

pub(crate) fn collect_algebraic_samples_with_schedule<E, FCheck, FProjectOrSeed>(
    ctx: &NoStateSampleContext<'_>,
    output_times: &mut Vec<f64>,
    evaluation_times: &[f64],
    y: Vec<f64>,
    check_deadline: FCheck,
    project_or_seed: FProjectOrSeed,
) -> Result<NoStateSampleDataWithTimes, NoStateSampleError<E>>
where
    FCheck: FnMut() -> Result<(), E>,
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
{
    collect_algebraic_samples_with_schedule_and_env_refresh(
        ctx,
        output_times,
        evaluation_times,
        y,
        check_deadline,
        project_or_seed,
        |_y, _t, _env| Ok::<(), E>(()),
    )
}

fn build_no_state_eval_point(
    ctx: &NoStateSampleContext<'_>,
    t: f64,
    carried_env: Option<&eval::VarEnv<f64>>,
) -> NoStateEvalPoint {
    NoStateEvalPoint {
        t,
        matched_event_t: matched_no_state_event_time(ctx, t, carried_env),
        dynamic_event_time: matched_dynamic_time_event_time(ctx, t, carried_env).is_some(),
        event_time: should_advance_pre_values(ctx, t, carried_env),
    }
}

fn sample_solver_outputs_direct<E, FProjectOrSeed>(
    ctx: &NoStateSampleContext<'_>,
    step: &NoStateEvalPoint,
    output_times: &[f64],
    out_idx: &mut usize,
    data: &mut [Vec<f64>],
    y: &mut Vec<f64>,
    project_or_seed: &mut FProjectOrSeed,
) -> Result<(), NoStateSampleError<E>>
where
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
{
    project_or_seed(y, step.t, true).map_err(NoStateSampleError::Callback)?;
    append_solver_output_samples_at_time(ctx, output_times, step.t, out_idx, data, y);
    if ctx.requires_live_pre_values && step.event_time {
        let env = build_runtime_state_env(
            ctx.dae,
            y,
            ctx.param_values,
            step.matched_event_t.unwrap_or(step.t),
        );
        eval::seed_pre_values_from_env(&env);
    }
    Ok(())
}

fn refresh_no_state_sample_env<E, FProjectOrSeed, FRefreshProjectedEnv>(
    ctx: &NoStateSampleContext<'_>,
    step: &NoStateEvalPoint,
    y: &mut Vec<f64>,
    carried_env: &mut Option<eval::VarEnv<f64>>,
    project_or_seed: &mut FProjectOrSeed,
    refresh_projected_env: &mut FRefreshProjectedEnv,
    settle_options: NoStateSettleOptions,
) -> Result<Option<eval::VarEnv<f64>>, NoStateSampleError<E>>
where
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
    FRefreshProjectedEnv: FnMut(&mut Vec<f64>, f64, &mut eval::VarEnv<f64>) -> Result<(), E>,
{
    let mut event_entry_env = None;
    if ctx.requires_projection {
        project_or_seed(y, step.t, true).map_err(NoStateSampleError::Callback)?;
        if step.dynamic_event_time {
            let mut event_entry_y = y.clone();
            event_entry_env = Some(build_event_entry_runtime_env(
                ctx,
                &mut event_entry_y,
                step.matched_event_t.unwrap_or(step.t),
            ));
        }
        if ctx.projection_needs_event_refresh && step.event_time {
            // MLS Appendix B / §8.6: a second projection pass is only
            // required at actual event instants, after the discrete
            // right-limit settles. Ordinary observation points should reuse
            // the current projection result and avoid a redundant
            // settle/reproject cycle.
            refresh_projection_with_event_context(
                ctx,
                y,
                step.t,
                carried_env,
                settle_options.advance_pre_between_samples,
                settle_options.refresh_discrete_between_samples,
                project_or_seed,
            )?;
        } else {
            let _ = settle_runtime_env(
                ctx,
                y,
                step.t,
                carried_env,
                settle_options.advance_pre_between_samples,
                settle_options.refresh_discrete_between_samples,
            );
        }
        if let Some(env) = carried_env.as_mut() {
            refresh_projected_env(y, step.t, env).map_err(NoStateSampleError::Callback)?;
        }
        return Ok(event_entry_env);
    }

    if step.dynamic_event_time {
        let mut event_entry_y = y.clone();
        event_entry_env = Some(build_event_entry_runtime_env(
            ctx,
            &mut event_entry_y,
            step.matched_event_t.unwrap_or(step.t),
        ));
    }
    let env = settle_runtime_env(
        ctx,
        y,
        step.t,
        carried_env,
        settle_options.advance_pre_between_samples,
        settle_options.refresh_discrete_between_samples,
    );
    // Solver-backed runtime direct seeds (for example `table_y - u = 0`)
    // must materialize on the no-projection path before the current
    // observation is sampled.
    refresh_projected_env(y, step.t, env).map_err(NoStateSampleError::Callback)?;
    Ok(event_entry_env)
}

fn refresh_no_state_event_right_limit_if_needed<E, FRefreshProjectedEnv>(
    ctx: &NoStateSampleContext<'_>,
    step: &NoStateEvalPoint,
    y: &mut Vec<f64>,
    carried_env: &mut Option<eval::VarEnv<f64>>,
    t_end: f64,
    schedules: &mut ObservationSchedules<'_>,
    refresh_projected_env: &mut FRefreshProjectedEnv,
) -> Result<(), NoStateSampleError<E>>
where
    FRefreshProjectedEnv: FnMut(&mut Vec<f64>, f64, &mut eval::VarEnv<f64>) -> Result<(), E>,
{
    if !(ctx.requires_live_pre_values && step.event_time) {
        return Ok(());
    }
    let env = carried_env
        .as_ref()
        .expect("no-state carried env must be initialized before seeding pre values");
    eval::seed_pre_values_from_env(env);
    if let Some(env) = carried_env.as_mut() {
        let t_right = crate::runtime::event::event_right_limit_time(
            ctx.t_start,
            t_end,
            step.matched_event_t.unwrap_or(step.t),
        );
        refresh_dynamic_event_right_limit_if_needed(
            ctx,
            y,
            t_right,
            env,
            schedules,
            refresh_projected_env,
            step.dynamic_event_time,
        )?;
        let _ = crate::runtime::discrete::refresh_post_event_observation_values_excluding_at_time(
            ctx.dae,
            env,
            t_right,
            ctx.dynamic_time_event_names,
        );
    }
    Ok(())
}

pub(crate) fn collect_algebraic_samples_with_schedule_and_env_refresh<
    E,
    FCheck,
    FProjectOrSeed,
    FRefreshProjectedEnv,
>(
    ctx: &NoStateSampleContext<'_>,
    output_times: &mut Vec<f64>,
    evaluation_times: &[f64],
    mut y: Vec<f64>,
    mut check_deadline: FCheck,
    mut project_or_seed: FProjectOrSeed,
    mut refresh_projected_env: FRefreshProjectedEnv,
) -> Result<NoStateSampleDataWithTimes, NoStateSampleError<E>>
where
    FCheck: FnMut() -> Result<(), E>,
    FProjectOrSeed: FnMut(&mut Vec<f64>, f64, bool) -> Result<(), E>,
    FRefreshProjectedEnv: FnMut(&mut Vec<f64>, f64, &mut eval::VarEnv<f64>) -> Result<(), E>,
{
    let mut data: Vec<Vec<f64>> = vec![Vec::with_capacity(output_times.len()); ctx.all_names.len()];
    let mut evaluation_schedule = evaluation_times.to_vec();
    let sample_solver_direct = can_sample_solver_outputs_directly(ctx);
    let has_direct_time_event_thresholds = dynamic_time_threshold_exprs(ctx).next().is_some();
    let advance_pre_between_samples =
        ctx.requires_live_pre_values && no_state_requires_inter_sample_pre_values(ctx.dae);
    let refresh_discrete_between_samples = ctx.requires_live_pre_values;

    eval::clear_pre_values();
    project_or_seed(&mut y, ctx.t_start, ctx.requires_projection)
        .map_err(NoStateSampleError::Callback)?;
    crate::runtime::startup::refresh_pre_values_from_state_with_initial_assignments(
        ctx.dae,
        &y,
        ctx.param_values,
        ctx.t_start,
    );

    let mut out_idx = 0usize;
    let t_end = output_times.last().copied().unwrap_or(ctx.t_start);
    let mut carried_env: Option<eval::VarEnv<f64>> = None;
    let mut eval_idx = 0usize;
    while eval_idx < evaluation_schedule.len() {
        let step =
            build_no_state_eval_point(ctx, evaluation_schedule[eval_idx], carried_env.as_ref());
        crate::runtime::hotpath_stats::inc_no_state_eval_point();
        check_deadline().map_err(NoStateSampleError::Callback)?;
        if sample_solver_direct {
            sample_solver_outputs_direct(
                ctx,
                &step,
                output_times,
                &mut out_idx,
                &mut data,
                &mut y,
                &mut project_or_seed,
            )?;
            eval_idx += 1;
            continue;
        }

        let event_entry_env = refresh_no_state_sample_env(
            ctx,
            &step,
            &mut y,
            &mut carried_env,
            &mut project_or_seed,
            &mut refresh_projected_env,
            NoStateSettleOptions {
                advance_pre_between_samples,
                refresh_discrete_between_samples,
            },
        )?;
        let mut schedules = observation_schedules(&mut evaluation_schedule, output_times);
        refresh_no_state_event_right_limit_if_needed(
            ctx,
            &step,
            &mut y,
            &mut carried_env,
            t_end,
            &mut schedules,
            &mut refresh_projected_env,
        )?;
        {
            let settled_env = carried_env
                .as_ref()
                .expect("no-state carried env must be initialized before sampling");
            let sample_env = event_entry_env.as_ref().unwrap_or(settled_env);
            if !ctx.dynamic_time_event_names.is_empty() || has_direct_time_event_thresholds {
                inject_dynamic_time_events(
                    ctx,
                    settled_env,
                    schedules.evaluation_schedule,
                    schedules.output_times,
                    eval_idx,
                    step.t,
                    t_end,
                );
            }
            // MLS Appendix B / §8.6: event iteration computes the event right-limit
            // OMC traces record the event-entry value at the event instant and the
            // right-limit immediately after. Match that shape by sampling the
            // pre-event env at `t` and the settled post-event env at the injected
            // right-limit observation time.
            append_output_samples_at_time(
                ctx,
                schedules.output_times,
                step.t,
                &mut out_idx,
                &mut data,
                sample_env,
                &y,
            );
            if advance_pre_between_samples && !step.event_time {
                // Between explicit event instants, the next observation still
                // needs the current no-state right-limit as its left-limit
                // history. Advance the pre-store after sampling so later
                // edge/change guards see transitions that occurred on ordinary
                // observation-time refreshes (for example pulse falling edges).
                eval::seed_pre_values_from_env(settled_env);
            }
        }
        eval_idx += 1;
    }

    if out_idx != output_times.len() {
        return Err(NoStateSampleError::SampleScheduleMismatch {
            captured: out_idx,
            expected: output_times.len(),
        });
    }

    Ok((y, output_times.clone(), data))
}

/// Remove backend-internal dummy state channels from no-state outputs.
pub fn finalize_algebraic_outputs(
    all_names: Vec<String>,
    data: Vec<Vec<f64>>,
    n_x: usize,
    dummy_state_name: &str,
) -> (Vec<String>, Vec<Vec<f64>>, usize) {
    let (mut final_names, mut final_data, mut final_n_states) = (all_names, data, n_x);
    if let Some(dummy_idx) = final_names.iter().position(|name| name == dummy_state_name) {
        final_names.remove(dummy_idx);
        if dummy_idx < final_data.len() {
            final_data.remove(dummy_idx);
        }
        final_n_states = final_n_states.saturating_sub(1);
    }
    (final_names, final_data, final_n_states)
}

/// Collect additional discrete names needed for reconstruction context.
pub fn collect_reconstruction_discrete_context_names(
    dae_model: &dae::Dae,
    elim: &EliminationResult,
    existing_names: &[String],
) -> Vec<String> {
    let existing: HashSet<String> = existing_names.iter().cloned().collect();
    let mut extras: indexmap::IndexSet<String> = indexmap::IndexSet::new();

    for sub in &elim.substitutions {
        let mut refs = HashSet::new();
        sub.expr.collect_var_refs(&mut refs);
        for name in refs {
            let raw = name.as_str();
            if existing.contains(raw) {
                continue;
            }
            let key = dae::VarName::new(raw);
            if dae_model.discrete_reals.contains_key(&key)
                || dae_model.discrete_valued.contains_key(&key)
            {
                extras.insert(raw.to_string());
            }
        }
    }

    extras.into_iter().collect()
}

#[cfg(test)]
pub(crate) fn sampled_names_need_eliminated_env(
    all_names: &[String],
    elim: &EliminationResult,
) -> bool {
    if elim.substitutions.is_empty() {
        return false;
    }
    let sampled_names: HashSet<&str> = all_names.iter().map(String::as_str).collect();
    elim.substitutions.iter().any(|sub| {
        sub.env_keys
            .iter()
            .any(|name| sampled_names.contains(name.as_str()))
    })
}

fn insert_runtime_dependency_seed(names: &mut HashSet<String>, name: &str) {
    names.insert(name.to_string());
    if let Some(base) = dae::component_base_name(name)
        && base != name
    {
        names.insert(base);
    }
}

fn collect_runtime_dependency_closure(
    all_names: &[String],
    direct_assignment_ctx: &crate::runtime::assignment::RuntimeDirectAssignmentContext,
    alias_ctx: &crate::runtime::alias::RuntimeAliasPropagationContext,
) -> HashSet<String> {
    let mut names = HashSet::new();
    for name in all_names {
        insert_runtime_dependency_seed(&mut names, name);
    }

    loop {
        let before = names.len();
        crate::runtime::assignment::extend_runtime_direct_assignment_dependency_closure(
            direct_assignment_ctx,
            &mut names,
        );
        crate::runtime::alias::extend_runtime_alias_dependency_closure(alias_ctx, &mut names);
        if names.len() == before {
            break;
        }
    }

    names
}

pub(crate) fn sampled_names_need_eliminated_env_with_runtime_closure(
    all_names: &[String],
    elim: &EliminationResult,
    direct_assignment_ctx: &crate::runtime::assignment::RuntimeDirectAssignmentContext,
    alias_ctx: &crate::runtime::alias::RuntimeAliasPropagationContext,
) -> bool {
    if elim.substitutions.is_empty() {
        return false;
    }
    // MLS §8.6 / §16.5.1: sampled/discrete observation must see the current
    // equality-closed runtime inputs. If an observed name reaches an
    // eliminated substitution target through a direct assignment or alias
    // equality, keep that substitution available in the runtime env.
    let dependency_names =
        collect_runtime_dependency_closure(all_names, direct_assignment_ctx, alias_ctx);
    elim.substitutions.iter().any(|sub| {
        sub.env_keys
            .iter()
            .any(|name| dependency_names.contains(name.as_str()))
    })
}
