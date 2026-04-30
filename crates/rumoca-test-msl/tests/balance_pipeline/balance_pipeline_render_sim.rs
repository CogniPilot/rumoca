use super::*;
use rumoca_compile::codegen::{
    render_dae_template_with_name, render_flat_template_with_name, templates,
};

// =============================================================================
// Render + simulation orchestration helpers
// =============================================================================

pub(super) fn maybe_render_model_outputs(
    name: &str,
    result: &rumoca_compile::compile::CompilationResult,
    ctx: &RenderSimContext<'_>,
) {
    if !msl_render_enabled() {
        return;
    }
    let is_root_example = is_root_standalone_msl_example_model(name, result);
    let is_partial = result.dae.is_partial;
    let should_render = !is_partial
        && (!ctx.run_simulation || (is_root_example && is_selected_sim_target(name, ctx)));
    if !should_render {
        return;
    }

    write_rendered_artifact(
        render_dae_template_with_name(&result.dae, templates::DAE_MODELICA, name),
        ctx.dae_dir.join(format!("{name}.mo")),
        ctx.dae_rendered,
        ctx.render_errors,
    );

    write_rendered_artifact(
        render_flat_template_with_name(&result.flat, templates::FLAT_MODELICA, name),
        ctx.flat_dir.join(format!("{name}.mo")),
        ctx.flat_rendered,
        ctx.render_errors,
    );

    let done = ctx.render_completed.fetch_add(1, Ordering::Relaxed) + 1;
    maybe_log_render_progress(ctx.run_simulation, done, ctx.total_render_targets);
}

pub(super) fn simulation_solver_override() -> Option<String> {
    std::env::var("RUMOCA_MSL_SIM_SOLVER")
        .ok()
        .map(|raw| raw.trim().to_string())
        .filter(|value| !value.is_empty())
}

pub(super) fn simulation_stop_time_override() -> Option<f64> {
    std::env::var("RUMOCA_MSL_SIM_STOP_TIME_OVERRIDE")
        .ok()
        .and_then(|raw| raw.trim().parse::<f64>().ok())
        .filter(|value| value.is_finite() && *value > 0.0)
}

pub(super) fn simulation_settings_from_result(
    result: &rumoca_compile::compile::CompilationResult,
) -> SimExecutionSettings {
    let mut t_start = result
        .experiment_start_time
        .filter(|seconds| seconds.is_finite())
        .unwrap_or(0.0);
    let mut t_end = result
        .experiment_stop_time
        .filter(|seconds| seconds.is_finite() && *seconds > t_start)
        .unwrap_or(t_start + DEFAULT_SIM_END_TIME_SECS);

    if !t_start.is_finite() || !t_end.is_finite() || t_end <= t_start {
        t_start = 0.0;
        t_end = DEFAULT_SIM_END_TIME_SECS;
    }
    if let Some(stop_time) = simulation_stop_time_override()
        && stop_time > t_start
    {
        t_end = stop_time;
    }

    let tolerance = result
        .experiment_tolerance
        .filter(|value| value.is_finite() && *value > 0.0);

    SimExecutionSettings {
        t_start,
        t_end,
        dt: result
            .experiment_interval
            .filter(|value| value.is_finite() && *value > 0.0),
        rtol: tolerance,
        atol: tolerance,
        solver: simulation_solver_override()
            .or_else(|| result.experiment_solver.clone())
            .unwrap_or_else(|| "auto".to_string()),
        timeout_seconds: None,
    }
}

pub(super) fn maybe_run_simulation(
    name: &str,
    result: &rumoca_compile::compile::CompilationResult,
    ctx: &RenderSimContext<'_>,
    remaining_budget_secs: Option<f64>,
) -> Option<MslSimModelResult> {
    if !ctx.run_simulation
        || result.dae.is_partial
        || !is_root_standalone_msl_example_model(name, result)
        || !is_selected_sim_target(name, ctx)
    {
        return None;
    }
    ctx.sim_attempted.fetch_add(1, Ordering::Relaxed);
    let settings = match gate_simulation_settings_by_compile_budget(
        simulation_settings_from_result(result),
        remaining_budget_secs,
    ) {
        Ok(settings) => settings,
        Err(_) => {
            return Some(MslSimModelResult {
                name: name.to_string(),
                status: SimStatus::Timeout,
                error: Some(
                    "model attempt timeout exhausted before simulation could start".to_string(),
                ),
                n_states: Some(result.dae.states.len()),
                n_algebraics: Some(result.dae.algebraics.len()),
                sim_seconds: Some(0.0),
                sim_build_seconds: Some(0.0),
                sim_run_seconds: Some(0.0),
                sim_wall_seconds: Some(0.0),
                sim_trace_file: None,
                sim_trace_error: None,
            });
        }
    };
    Some(try_simulate_dae_with_settings(&result.dae, name, &settings))
}

pub(super) fn maybe_log_sim_progress(done: usize, ctx: &RenderSimContext<'_>) {
    if !done.is_multiple_of(10) && done != ctx.total_sim_targets {
        return;
    }
    let attempted = ctx.sim_attempted.load(Ordering::Relaxed);
    let ok = ctx.sim_ok_live.load(Ordering::Relaxed);
    let nan = ctx.sim_nan_live.load(Ordering::Relaxed);
    let timeout = ctx.sim_timeout_live.load(Ordering::Relaxed);
    let solver = ctx.sim_solver_fail_live.load(Ordering::Relaxed);
    let balance = ctx.sim_balance_fail_live.load(Ordering::Relaxed);
    let fail = nan + timeout + solver + balance;
    let progress_pct = pct(done, ctx.total_sim_targets);
    let ok_pct = pct(ok, done);
    let fail_pct = pct(fail, done);
    eprintln!(
        "  simulation progress: completed={done}/{total} ({progress_pct:.1}%) attempted={attempted} | ok={ok} ({ok_pct:.1}%) fail={fail} ({fail_pct:.1}%) [timeout={timeout}, solver={solver}, nan={nan}, balance={balance}]",
        total = ctx.total_sim_targets
    );
}

pub(super) fn update_live_sim_status(sim: &MslSimModelResult, ctx: &RenderSimContext<'_>) {
    match sim.status {
        SimStatus::Ok => {
            ctx.sim_ok_live.fetch_add(1, Ordering::Relaxed);
        }
        SimStatus::Nan => {
            ctx.sim_nan_live.fetch_add(1, Ordering::Relaxed);
        }
        SimStatus::Timeout => {
            ctx.sim_timeout_live.fetch_add(1, Ordering::Relaxed);
        }
        SimStatus::SolverFail => {
            ctx.sim_solver_fail_live.fetch_add(1, Ordering::Relaxed);
        }
        SimStatus::BalanceFail => {
            ctx.sim_balance_fail_live.fetch_add(1, Ordering::Relaxed);
        }
    }
}

pub(super) fn convert_compile_result_entry(
    entry: ModelCompileEntry,
    ctx: &RenderSimContext<'_>,
) -> MslModelResult {
    let ModelCompileEntry {
        model_name: name,
        compile_outcome,
        remaining_budget_secs,
        compile_seconds,
    } = entry;

    let sim_result = if let Some(result) = compile_outcome.success_result() {
        maybe_dump_model_introspection(&name, result, ctx);
        maybe_render_model_outputs(&name, result, ctx);
        maybe_run_simulation(&name, result, ctx, remaining_budget_secs)
    } else {
        None
    };

    let mut model_result = convert_compile_outcome(name, compile_outcome);
    model_result.compile_seconds = Some(compile_seconds);
    if let Some(sim) = sim_result {
        let done = ctx.sim_completed.fetch_add(1, Ordering::Relaxed) + 1;
        update_live_sim_status(&sim, ctx);
        maybe_log_sim_progress(done, ctx);
        model_result.sim_status = Some(sim.status.to_string());
        model_result.sim_error = sim.error;
        model_result.sim_seconds = sim.sim_seconds;
        model_result.sim_build_seconds = sim.sim_build_seconds;
        model_result.sim_run_seconds = sim.sim_run_seconds;
        model_result.sim_wall_seconds = sim.sim_wall_seconds;
        model_result.sim_trace_file = sim.sim_trace_file;
        model_result.sim_trace_error = sim.sim_trace_error;
    }

    model_result
}

pub(super) struct RenderSimSetup {
    dae_dir: PathBuf,
    flat_dir: PathBuf,
    dae_rendered: AtomicUsize,
    flat_rendered: AtomicUsize,
    render_errors: AtomicUsize,
    sim_attempted: AtomicUsize,
    sim_completed: AtomicUsize,
    sim_ok_live: AtomicUsize,
    sim_nan_live: AtomicUsize,
    sim_timeout_live: AtomicUsize,
    sim_solver_fail_live: AtomicUsize,
    sim_balance_fail_live: AtomicUsize,
    render_completed: AtomicUsize,
    sim_target_names: Option<HashSet<String>>,
    sim_target_models: Vec<String>,
    total_render_targets: usize,
    total_sim_targets: usize,
}

impl RenderSimSetup {
    fn new_from_compile_scope(compile_scope_names: &[String], run_simulation: bool) -> Self {
        let dae_dir = get_msl_cache_dir().join("results").join("rumoca_dae");
        let flat_dir = get_msl_cache_dir().join("results").join("rumoca_flat");
        let _ = fs::create_dir_all(&dae_dir);
        let _ = fs::create_dir_all(&flat_dir);

        let sim_target_names =
            select_sim_target_names_from_compile_scope(compile_scope_names, run_simulation);
        let sim_target_models = match sim_target_names.as_ref() {
            Some(names) => names.clone(),
            None => Vec::new(),
        };
        let sim_target_name_set = sim_target_models.iter().cloned().collect();
        let total_sim_targets = sim_target_models.len();
        let total_render_targets = if !msl_render_enabled() {
            0
        } else if run_simulation {
            total_sim_targets
        } else {
            compile_scope_names.len()
        };

        println!(
            "Target models for render artifacts: {}",
            total_render_targets
        );
        if run_simulation {
            println!(
                "Target standalone root MSL examples for simulation: {}",
                total_sim_targets
            );
        }

        Self {
            dae_dir,
            flat_dir,
            dae_rendered: AtomicUsize::new(0),
            flat_rendered: AtomicUsize::new(0),
            render_errors: AtomicUsize::new(0),
            sim_attempted: AtomicUsize::new(0),
            sim_completed: AtomicUsize::new(0),
            sim_ok_live: AtomicUsize::new(0),
            sim_nan_live: AtomicUsize::new(0),
            sim_timeout_live: AtomicUsize::new(0),
            sim_solver_fail_live: AtomicUsize::new(0),
            sim_balance_fail_live: AtomicUsize::new(0),
            render_completed: AtomicUsize::new(0),
            sim_target_names: if run_simulation {
                Some(sim_target_name_set)
            } else {
                None
            },
            sim_target_models,
            total_render_targets,
            total_sim_targets,
        }
    }

    pub(super) fn context(&self, run_simulation: bool) -> RenderSimContext<'_> {
        RenderSimContext {
            run_simulation,
            sim_target_names: self.sim_target_names.as_ref(),
            total_render_targets: self.total_render_targets,
            total_sim_targets: self.total_sim_targets,
            dae_dir: &self.dae_dir,
            flat_dir: &self.flat_dir,
            dae_rendered: &self.dae_rendered,
            flat_rendered: &self.flat_rendered,
            render_errors: &self.render_errors,
            sim_attempted: &self.sim_attempted,
            sim_completed: &self.sim_completed,
            sim_ok_live: &self.sim_ok_live,
            sim_nan_live: &self.sim_nan_live,
            sim_timeout_live: &self.sim_timeout_live,
            sim_solver_fail_live: &self.sim_solver_fail_live,
            sim_balance_fail_live: &self.sim_balance_fail_live,
            render_completed: &self.render_completed,
        }
    }

    pub(super) fn print_summary(&self, run_simulation: bool) {
        if msl_render_enabled() {
            println!(
                "DAE Modelica: {}/{} rendered to {:?}",
                self.dae_rendered.load(Ordering::Relaxed),
                self.total_render_targets,
                self.dae_dir,
            );
            println!(
                "Flat Modelica: {}/{} rendered to {:?}",
                self.flat_rendered.load(Ordering::Relaxed),
                self.total_render_targets,
                self.flat_dir,
            );
            if self.render_errors.load(Ordering::Relaxed) > 0 {
                println!(
                    "Render errors: {}",
                    self.render_errors.load(Ordering::Relaxed),
                );
            }
        } else {
            println!("DAE/Flat artifact rendering: disabled (set RUMOCA_MSL_RENDER=1 to enable).");
        }
        if run_simulation {
            println!(
                "Simulated {} standalone root MSL Example models (target={})",
                self.sim_attempted.load(Ordering::Relaxed),
                self.total_sim_targets,
            );
        } else {
            println!("Simulation skipped (compile+balance mode).");
        }
    }

    pub(super) fn sim_target_models(&self) -> Vec<String> {
        self.sim_target_models.clone()
    }
}

fn select_sim_target_names_from_compile_scope(
    compile_scope_names: &[String],
    run_simulation: bool,
) -> Option<Vec<String>> {
    if !run_simulation {
        return None;
    }

    let mut names: Vec<String> = compile_scope_names.to_vec();
    let subset_requested = apply_sim_subset_filters(&mut names, "Simulation");
    if !subset_requested {
        apply_default_sim_set_mode_selection(&mut names);
    }

    Some(names)
}

fn apply_default_sim_set_mode_selection(names: &mut Vec<String>) {
    let mode = sim_set_mode();
    if mode == SimSetMode::Full {
        println!(
            "Simulation set mode ({mode}): keeping all {} compile-scope models",
            names.len()
        );
        return;
    }

    let limit = sim_set_limit();
    if limit >= names.len() {
        println!(
            "Simulation set mode ({mode}) selected {}/{} compile-scope models (RUMOCA_MSL_SIM_SET_LIMIT={})",
            names.len(),
            names.len(),
            limit
        );
        return;
    }

    eprintln!(
        "WARNING: simulation set mode ({mode}) is using compile-scope order fallback for streaming compile (full live state-count ranking still requires retaining all compile results)"
    );
    apply_lexical_mode_limit(names, mode, limit);
}

fn apply_lexical_mode_limit(names: &mut Vec<String>, mode: SimSetMode, limit: usize) {
    match mode {
        SimSetMode::Short => names.truncate(limit),
        SimSetMode::Long => {
            let keep_from = names.len().saturating_sub(limit);
            *names = names.split_off(keep_from);
        }
        SimSetMode::Full => {}
    }
}

pub(super) fn collect_render_sim_results(
    compile_results: Vec<ModelCompileEntry>,
    run_simulation: bool,
    context: &RenderSimContext<'_>,
    simulation_threads: usize,
    log_parallelism: bool,
) -> Vec<MslModelResult> {
    if !run_simulation {
        return compile_results
            .into_par_iter()
            .map(|entry| convert_compile_result_entry(entry, context))
            .collect();
    }

    if log_parallelism {
        println!("Simulation execution parallelism: {simulation_threads}");
    }
    match rayon::ThreadPoolBuilder::new()
        .num_threads(simulation_threads.max(1))
        .build()
    {
        Ok(pool) => pool.install(|| {
            compile_results
                .into_par_iter()
                .map(|entry| convert_compile_result_entry(entry, context))
                .collect()
        }),
        Err(err) => {
            eprintln!(
                "WARNING: failed to build simulation thread pool ({err}); falling back to global rayon pool"
            );
            compile_results
                .into_par_iter()
                .map(|entry| convert_compile_result_entry(entry, context))
                .collect()
        }
    }
}

pub(super) fn begin_chunked_render_sim_setup(
    compile_scope_names: &[String],
    run_simulation: bool,
) -> RenderSimSetup {
    RenderSimSetup::new_from_compile_scope(compile_scope_names, run_simulation)
}
