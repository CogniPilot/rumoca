use super::*;

pub(super) struct ModelWorkerQueue<'a> {
    pub(super) source_root_path: &'a Path,
    pub(super) names_chunk: &'a [String],
    pub(super) budget_secs: f64,
    pub(super) run_simulation: bool,
    pub(super) sim_target_names: Option<&'a HashSet<String>>,
    pub(super) explicit_sim_target: bool,
    pub(super) cpu_core_id: Option<usize>,
    pub(super) memory_tokens: Option<std::sync::Arc<ResourceTokenLimiter>>,
    pub(super) cpu_tokens: std::sync::Arc<ResourceTokenLimiter>,
    pub(super) cpu_costs: &'a HashMap<String, usize>,
    pub(super) startup_barrier: std::sync::Arc<std::sync::Barrier>,
    pub(super) next_model: &'a AtomicUsize,
    pub(super) result_tx: std::sync::mpsc::SyncSender<(usize, MslModelResult)>,
}

pub(super) fn run_model_worker_queue(queue: ModelWorkerQueue<'_>) {
    let mut worker: Option<ModelWorkerDaemon> = None;
    let mut _worker_memory_permit: Option<ResourceTokenPermit> = None;
    let startup_timeout_secs = model_worker_startup_timeout_secs(queue.budget_secs);
    queue.startup_barrier.wait();
    loop {
        let idx = queue.next_model.fetch_add(1, Ordering::Relaxed);
        let Some(name) = queue.names_chunk.get(idx) else {
            if let Some(worker) = worker.as_mut() {
                worker.shutdown_and_join();
            }
            return;
        };
        if _worker_memory_permit.is_none() {
            _worker_memory_permit = queue
                .memory_tokens
                .as_ref()
                .map(|tokens| tokens.acquire(compile_model_memory_mb()));
        }
        if worker.is_none() {
            match ModelWorkerDaemon::spawn(
                queue.source_root_path,
                startup_timeout_secs,
                queue.cpu_core_id,
            ) {
                Ok(spawned) => worker = Some(spawned),
                Err(error) => {
                    let entry = model_worker_failure_result(
                        name,
                        MODEL_WORKER_ERROR_CODE,
                        format!("model worker failed to start: {error}"),
                    );
                    let _ = queue.result_tx.send((idx, entry));
                    continue;
                }
            }
        }
        let cpu_cost = queue.cpu_costs.get(name).copied().unwrap_or(1).max(1);
        let _cpu_permit = queue.cpu_tokens.acquire(cpu_cost);
        let selected_for_simulation = queue
            .sim_target_names
            .is_some_and(|names| names.contains(name));
        let (entry, _keep_worker) = run_compile_model_in_process_worker(
            &mut worker,
            InProcessWorkerRequest {
                source_root_path: queue.source_root_path,
                cpu_core_id: queue.cpu_core_id,
                model_name: name,
                budget_secs: queue.budget_secs,
                startup_timeout_secs,
                run_simulation: queue.run_simulation,
                selected_for_simulation,
                explicit_sim_target: queue.explicit_sim_target,
            },
        );
        let _ = queue.result_tx.send((idx, entry));
    }
}

pub(super) fn run_compile_chunk_progress_loop(
    compile_in_flight_flag: std::sync::Arc<std::sync::atomic::AtomicBool>,
    chunk_idx: usize,
    chunk_count: usize,
    chunk_models: usize,
) {
    let start = Instant::now();
    let log_interval = Duration::from_secs(COMPILE_CHUNK_PROGRESS_INTERVAL_SECS);
    let poll_interval = Duration::from_millis(COMPILE_CHUNK_PROGRESS_POLL_MILLIS);
    let mut next_log_at = log_interval;
    while compile_in_flight_flag.load(Ordering::Relaxed) {
        let elapsed = start.elapsed();
        if elapsed >= next_log_at {
            if chunk_count == 1 {
                eprintln!(
                    "    global compile queue still running after {:.1}s ({} models)",
                    elapsed.as_secs_f64(),
                    chunk_models
                );
            } else {
                eprintln!(
                    "    chunk {}/{} compile still running after {:.1}s ({} models)",
                    chunk_idx,
                    chunk_count,
                    elapsed.as_secs_f64(),
                    chunk_models
                );
            }
            next_log_at += log_interval;
        }
        std::thread::sleep(poll_interval);
    }
}

pub(super) struct StreamingChunkOutput {
    model_results: Vec<MslModelResult>,
    compile_seconds: f64,
    drain_seconds: f64,
}

pub(super) enum StreamingPreparedEntry {
    Final(Box<MslModelResult>),
    PendingSimulation {
        model_result: Box<MslModelResult>,
        prepared_simulation: Box<PreparedSimulationRun>,
        _memory_permit: Option<ResourceTokenPermit>,
        _cpu_permit: ResourceTokenPermit,
    },
}

pub(super) struct StreamingChunkPlan<'a> {
    source_root_path: &'a Path,
    names_chunk: &'a [String],
    simulation_threads: usize,
    model_budget_secs: f64,
    chunk_idx: usize,
    chunk_count: usize,
    log_parallelism: bool,
}

pub(super) struct StreamingPreparationResources<'a> {
    remaining_budget_secs: Option<f64>,
    sim_memory_tokens: Option<&'a std::sync::Arc<ResourceTokenLimiter>>,
    cpu_tokens: &'a std::sync::Arc<ResourceTokenLimiter>,
}

pub(super) fn finalize_simulation_into_model_result(
    mut model_result: MslModelResult,
    sim: MslSimModelResult,
    ctx: &RenderSimContext<'_>,
) -> MslModelResult {
    let done = ctx.sim_completed.fetch_add(1, Ordering::Relaxed) + 1;
    update_live_sim_status(&sim, ctx);
    maybe_log_sim_progress(done, ctx);
    model_result.sim_status = Some(sim.status.to_string());
    model_result.sim_error = sim.error;
    model_result.ic_status = sim.ic_status;
    model_result.ic_error = sim.ic_error;
    model_result.ic_seconds = sim.ic_seconds;
    model_result.sim_seconds = sim.sim_seconds;
    model_result.sim_build_seconds = sim.sim_build_seconds;
    model_result.ir_solve_seconds = sim.ir_solve_seconds;
    model_result.ir_solve_structural_dae_seconds = sim.ir_solve_structural_dae_seconds;
    model_result.ir_solve_lower_seconds = sim.ir_solve_lower_seconds;
    model_result.sim_backend_build_seconds = sim.sim_backend_build_seconds;
    model_result.sim_run_seconds = sim.sim_run_seconds;
    model_result.sim_wall_seconds = sim.sim_wall_seconds;
    model_result.sim_trace_file = sim.sim_trace_file;
    model_result.sim_perf_profile_file = sim.sim_perf_profile_file;
    model_result.sim_trace_error = sim.sim_trace_error;
    model_result.ir_dae_file = sim.ir_dae_file;
    model_result.ir_solve_file = sim.ir_solve_file;
    model_result.ir_solve_error = sim.ir_solve_error;
    model_result
}

pub(super) fn attach_compile_metadata(
    model_result: &mut MslModelResult,
    compile_seconds: f64,
    compile_perf_profile_file: Option<String>,
) {
    model_result.compile_seconds = Some(compile_seconds);
    model_result.compile_perf_profile_file = compile_perf_profile_file;
}

pub(super) fn prepare_successful_streaming_entry(
    model_name: String,
    result: rumoca_compile::compile::CompilationResult,
    compile_seconds: f64,
    compile_perf_profile_file: Option<String>,
    ctx: &RenderSimContext<'_>,
    resources: StreamingPreparationResources<'_>,
) -> StreamingPreparedEntry {
    maybe_dump_model_introspection(&model_name, &result, ctx);
    maybe_render_model_outputs(&model_name, &result, ctx);

    let mut model_result = summarize_success_result(model_name.clone(), &result);
    attach_compile_metadata(
        &mut model_result,
        compile_seconds,
        compile_perf_profile_file,
    );
    let should_simulate = should_simulate_compilation_result(&model_name, &result, ctx);
    if !should_simulate {
        return StreamingPreparedEntry::Final(Box::new(model_result));
    }

    ctx.sim_attempted.fetch_add(1, Ordering::Relaxed);
    let settings =
        match gate_simulation_settings_by_compile_budget(
            simulation_settings_from_result(&result),
            resources.remaining_budget_secs,
        ) {
            Ok(settings) => settings,
            Err(_) => {
                let timeout_result = MslSimModelResult {
                    name: model_name,
                    status: SimStatus::Timeout,
                    error: Some(
                        "model attempt timeout exhausted before simulation could start".to_string(),
                    ),
                    ic_status: None,
                    ic_error: None,
                    ic_seconds: None,
                    n_states: Some(result.dae.variables.states.len()),
                    n_algebraics: Some(result.dae.variables.algebraics.len()),
                    sim_seconds: Some(0.0),
                    sim_build_seconds: Some(0.0),
                    ir_solve_seconds: Some(0.0),
                    ir_solve_structural_dae_seconds: Some(0.0),
                    ir_solve_lower_seconds: Some(0.0),
                    sim_backend_build_seconds: Some(0.0),
                    sim_run_seconds: Some(0.0),
                    sim_wall_seconds: Some(0.0),
                    sim_trace_file: None,
                    sim_perf_profile_file: None,
                    sim_trace_error: None,
                    ir_dae_file: None,
                    ir_solve_file: None,
                    ir_solve_error: None,
                };
                return StreamingPreparedEntry::Final(Box::new(
                    finalize_simulation_into_model_result(model_result, timeout_result, ctx),
                ));
            }
        };

    let n_states = result.dae.variables.states.len();
    let n_algebraics = result.dae.variables.algebraics.len();
    let n_state_scalars: usize = result
        .dae
        .variables
        .states
        .values()
        .map(|value| value.size())
        .sum();
    let output_samples = output_samples_for_model(n_state_scalars);
    let memory_permit = resources
        .sim_memory_tokens
        .map(|tokens| tokens.acquire(simulation_preparation_memory_cost_mb()));
    let cpu_permit = resources.cpu_tokens.acquire(1);
    match prepare_simulation_run(
        &result.dae,
        &model_name,
        settings,
        output_samples,
        n_states,
        n_algebraics,
    ) {
        Ok(prepared_simulation) => StreamingPreparedEntry::PendingSimulation {
            model_result: Box::new(model_result),
            prepared_simulation: Box::new(prepared_simulation),
            _memory_permit: memory_permit,
            _cpu_permit: cpu_permit,
        },
        Err(sim_result) => StreamingPreparedEntry::Final(Box::new(
            finalize_simulation_into_model_result(model_result, *sim_result, ctx),
        )),
    }
}

pub(super) fn prepare_successful_dae_streaming_entry(
    model_name: String,
    result: Box<rumoca_compile::compile::DaeCompilationResult>,
    compile_seconds: f64,
    compile_perf_profile_file: Option<String>,
    ctx: &RenderSimContext<'_>,
    resources: StreamingPreparationResources<'_>,
) -> StreamingPreparedEntry {
    let mut model_result = summarize_dae_success_result(model_name.clone(), &result);
    attach_compile_metadata(
        &mut model_result,
        compile_seconds,
        compile_perf_profile_file,
    );
    let should_simulate = should_simulate_dae_compilation_result(&model_name, &result, ctx);
    if !should_simulate {
        return StreamingPreparedEntry::Final(Box::new(model_result));
    }

    ctx.sim_attempted.fetch_add(1, Ordering::Relaxed);
    let settings =
        match gate_simulation_settings_by_compile_budget(
            simulation_settings_from_dae_result(&result),
            resources.remaining_budget_secs,
        ) {
            Ok(settings) => settings,
            Err(_) => {
                let timeout_result = MslSimModelResult {
                    name: model_name,
                    status: SimStatus::Timeout,
                    error: Some(
                        "model attempt timeout exhausted before simulation could start".to_string(),
                    ),
                    ic_status: None,
                    ic_error: None,
                    ic_seconds: None,
                    n_states: Some(result.dae.variables.states.len()),
                    n_algebraics: Some(result.dae.variables.algebraics.len()),
                    sim_seconds: Some(0.0),
                    sim_build_seconds: Some(0.0),
                    ir_solve_seconds: Some(0.0),
                    ir_solve_structural_dae_seconds: Some(0.0),
                    ir_solve_lower_seconds: Some(0.0),
                    sim_backend_build_seconds: Some(0.0),
                    sim_run_seconds: Some(0.0),
                    sim_wall_seconds: Some(0.0),
                    sim_trace_file: None,
                    sim_perf_profile_file: None,
                    sim_trace_error: None,
                    ir_dae_file: None,
                    ir_solve_file: None,
                    ir_solve_error: None,
                };
                return StreamingPreparedEntry::Final(Box::new(
                    finalize_simulation_into_model_result(model_result, timeout_result, ctx),
                ));
            }
        };

    let n_states = result.dae.variables.states.len();
    let n_algebraics = result.dae.variables.algebraics.len();
    let n_state_scalars: usize = result
        .dae
        .variables
        .states
        .values()
        .map(|value| value.size())
        .sum();
    let output_samples = output_samples_for_model(n_state_scalars);
    let memory_permit = resources
        .sim_memory_tokens
        .map(|tokens| tokens.acquire(simulation_preparation_memory_cost_mb()));
    let cpu_permit = resources.cpu_tokens.acquire(1);
    match prepare_simulation_run(
        result.dae.as_ref(),
        &model_name,
        settings,
        output_samples,
        n_states,
        n_algebraics,
    ) {
        Ok(prepared_simulation) => StreamingPreparedEntry::PendingSimulation {
            model_result: Box::new(model_result),
            prepared_simulation: Box::new(prepared_simulation),
            _memory_permit: memory_permit,
            _cpu_permit: cpu_permit,
        },
        Err(sim_result) => StreamingPreparedEntry::Final(Box::new(
            finalize_simulation_into_model_result(model_result, *sim_result, ctx),
        )),
    }
}

pub(super) fn prepare_streaming_compile_result_entry(
    entry: ModelCompileEntry,
    ctx: &RenderSimContext<'_>,
    sim_memory_tokens: Option<&std::sync::Arc<ResourceTokenLimiter>>,
    cpu_tokens: &std::sync::Arc<ResourceTokenLimiter>,
) -> StreamingPreparedEntry {
    let ModelCompileEntry {
        model_name,
        compile_outcome,
        remaining_budget_secs,
        compile_seconds,
        compile_perf_profile_file,
    } = entry;

    match compile_outcome {
        ModelCompileOutcome::Phase(PhaseResult::Success(result)) => {
            prepare_successful_streaming_entry(
                model_name,
                *result,
                compile_seconds,
                compile_perf_profile_file,
                ctx,
                StreamingPreparationResources {
                    remaining_budget_secs,
                    sim_memory_tokens,
                    cpu_tokens,
                },
            )
        }
        ModelCompileOutcome::Phase(phase_result) => {
            let mut model_result = convert_phase_result(model_name, phase_result);
            attach_compile_metadata(
                &mut model_result,
                compile_seconds,
                compile_perf_profile_file,
            );
            StreamingPreparedEntry::Final(Box::new(model_result))
        }
        ModelCompileOutcome::StrictDaeSuccess(result) => prepare_successful_dae_streaming_entry(
            model_name,
            result,
            compile_seconds,
            compile_perf_profile_file,
            ctx,
            StreamingPreparationResources {
                remaining_budget_secs,
                sim_memory_tokens,
                cpu_tokens,
            },
        ),
        ModelCompileOutcome::StrictDaeFailure(summary) => {
            let mut model_result =
                convert_compile_outcome(model_name, ModelCompileOutcome::StrictDaeFailure(summary));
            attach_compile_metadata(
                &mut model_result,
                compile_seconds,
                compile_perf_profile_file,
            );
            StreamingPreparedEntry::Final(Box::new(model_result))
        }
        ModelCompileOutcome::StrictReport(report) => {
            let report = *report;
            let requested_success = report.failures.is_empty()
                && matches!(
                    report.requested_result.as_ref(),
                    Some(PhaseResult::Success(_))
                );
            if requested_success {
                let result = match report.requested_result {
                    Some(PhaseResult::Success(result)) => result,
                    _ => unreachable!("requested_success implies success result"),
                };
                prepare_successful_streaming_entry(
                    model_name,
                    *result,
                    compile_seconds,
                    compile_perf_profile_file,
                    ctx,
                    StreamingPreparationResources {
                        remaining_budget_secs,
                        sim_memory_tokens,
                        cpu_tokens,
                    },
                )
            } else {
                let mut model_result = convert_compile_outcome(
                    model_name,
                    ModelCompileOutcome::StrictReport(Box::new(report)),
                );
                attach_compile_metadata(
                    &mut model_result,
                    compile_seconds,
                    compile_perf_profile_file,
                );
                StreamingPreparedEntry::Final(Box::new(model_result))
            }
        }
    }
}

pub(super) fn should_simulate_compilation_result(
    model_name: &str,
    result: &rumoca_compile::compile::CompilationResult,
    ctx: &RenderSimContext<'_>,
) -> bool {
    ctx.run_simulation
        && !result.dae.metadata.is_partial
        && is_selected_sim_target(model_name, ctx)
        && (sim_targets_file_override().is_some()
            || is_root_standalone_msl_example_model(model_name, result))
}

pub(super) fn should_simulate_dae_compilation_result(
    model_name: &str,
    result: &rumoca_compile::compile::DaeCompilationResult,
    ctx: &RenderSimContext<'_>,
) -> bool {
    ctx.run_simulation
        && !result.dae.metadata.is_partial
        && is_selected_sim_target(model_name, ctx)
        && (sim_targets_file_override().is_some()
            || is_root_standalone_msl_example_dae_model(model_name, result))
}

pub(super) fn consume_streaming_prepared_entry(
    entry: StreamingPreparedEntry,
    ctx: &RenderSimContext<'_>,
) -> MslModelResult {
    match entry {
        StreamingPreparedEntry::Final(model_result) => *model_result,
        StreamingPreparedEntry::PendingSimulation {
            model_result,
            prepared_simulation,
            _memory_permit,
            _cpu_permit,
        } => finalize_simulation_into_model_result(
            *model_result,
            run_prepared_simulation(*prepared_simulation),
            ctx,
        ),
    }
}

pub(super) fn record_worker_model_result_progress(
    result: &MslModelResult,
    ctx: &RenderSimContext<'_>,
) {
    let Some(status) = result.sim_status.as_deref() else {
        return;
    };
    ctx.sim_attempted.fetch_add(1, Ordering::Relaxed);
    let done = ctx.sim_completed.fetch_add(1, Ordering::Relaxed) + 1;
    match status {
        "sim_ok" => {
            ctx.sim_ok_live.fetch_add(1, Ordering::Relaxed);
        }
        "sim_nan" => {
            ctx.sim_nan_live.fetch_add(1, Ordering::Relaxed);
        }
        "sim_timeout" => {
            ctx.sim_timeout_live.fetch_add(1, Ordering::Relaxed);
        }
        "sim_balance_fail" => {
            ctx.sim_balance_fail_live.fetch_add(1, Ordering::Relaxed);
        }
        _ => {
            ctx.sim_solver_fail_live.fetch_add(1, Ordering::Relaxed);
        }
    }
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
    eprintln!(
        "  simulation progress: completed={done}/{total} ({progress:.1}%) attempted={attempted} | ok={ok} ({ok_pct:.1}%) fail={fail} ({fail_pct:.1}%) [timeout={timeout}, solver={solver}, nan={nan}, balance={balance}]",
        total = ctx.total_sim_targets,
        progress = pct(done, ctx.total_sim_targets),
        ok_pct = pct(ok, done),
        fail_pct = pct(fail, done),
    );
}

pub(super) fn run_streaming_compile_and_render_chunk(
    source_root: &std::sync::Arc<CompiledSourceRoot>,
    context: &RenderSimContext<'_>,
    plan: StreamingChunkPlan<'_>,
) -> StreamingChunkOutput {
    let StreamingChunkPlan {
        names_chunk,
        source_root_path,
        simulation_threads,
        model_budget_secs,
        chunk_idx,
        chunk_count,
        log_parallelism,
    } = plan;

    if log_parallelism {
        println!("Simulation execution parallelism: {simulation_threads}");
    }
    let cpu_tokens = pipeline_cpu_token_limiter(simulation_threads);

    let pipeline_start = Instant::now();
    let compile_in_flight = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
    let compile_in_flight_flag = std::sync::Arc::clone(&compile_in_flight);
    let chunk_models_for_log = names_chunk.len();
    let compile_progress_logger = std::thread::spawn(move || {
        run_compile_chunk_progress_loop(
            compile_in_flight_flag,
            chunk_idx,
            chunk_count,
            chunk_models_for_log,
        );
    });

    let mut indexed_results = Vec::with_capacity(names_chunk.len());
    let compile_timeout_secs =
        global_queue_compile_timeout_secs(names_chunk.len(), simulation_threads, model_budget_secs);
    let _compile_watchdog = StageAbortWatchdog::new(
        queue_stage_label("compile", chunk_idx, chunk_count),
        compile_timeout_secs,
    );
    let compile_seconds = stream_source_root_compile_with_model_budgets(
        source_root,
        SourceRootCompileQueue {
            source_root_path,
            names_chunk,
            compile_threads: simulation_threads,
            budget_secs: model_budget_secs,
            cpu_tokens: std::sync::Arc::clone(&cpu_tokens),
            run_simulation: context.run_simulation,
            sim_target_names: context.sim_target_names,
        },
        |result_idx, result| {
            record_worker_model_result_progress(&result, context);
            indexed_results.push((result_idx, result));
        },
    );
    compile_in_flight.store(false, Ordering::Relaxed);
    let _ = compile_progress_logger.join();

    let pipeline_seconds = pipeline_start.elapsed().as_secs_f64();
    let drain_seconds = (pipeline_seconds - compile_seconds).max(0.0);
    let mut ordered_results: Vec<Option<MslModelResult>> = std::iter::repeat_with(|| None)
        .take(names_chunk.len())
        .collect();
    for (result_idx, model_result) in indexed_results {
        ordered_results[result_idx] = Some(model_result);
    }

    let model_results = ordered_results
        .into_iter()
        .map(|result| result.expect("every compiled model should produce a final result"))
        .collect();

    StreamingChunkOutput {
        model_results,
        compile_seconds,
        drain_seconds,
    }
}

pub(super) fn run_simulation_chunk(
    source_root: &std::sync::Arc<CompiledSourceRoot>,
    context: &RenderSimContext<'_>,
    plan: StreamingChunkPlan<'_>,
) -> StreamingChunkOutput {
    let StreamingChunkPlan {
        names_chunk,
        source_root_path,
        simulation_threads,
        model_budget_secs,
        chunk_idx,
        chunk_count,
        log_parallelism,
    } = plan;
    let sim_timeout_secs =
        global_queue_sim_timeout_secs(names_chunk.len(), simulation_threads, model_budget_secs);
    let _sim_chunk_watchdog = StageAbortWatchdog::new(
        queue_stage_label("simulate/render", chunk_idx, chunk_count),
        sim_timeout_secs,
    );
    run_streaming_compile_and_render_chunk(
        source_root,
        context,
        StreamingChunkPlan {
            names_chunk,
            source_root_path,
            simulation_threads,
            model_budget_secs,
            chunk_idx,
            chunk_count,
            log_parallelism,
        },
    )
}

pub(super) fn run_compile_only_chunk(
    source_root: &std::sync::Arc<CompiledSourceRoot>,
    _context: &RenderSimContext<'_>,
    plan: StreamingChunkPlan<'_>,
) -> StreamingChunkOutput {
    let StreamingChunkPlan {
        names_chunk,
        source_root_path,
        simulation_threads,
        model_budget_secs,
        chunk_idx,
        chunk_count,
        log_parallelism,
    } = plan;
    let chunk_models_for_log = names_chunk.len();
    let compile_in_flight = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
    let compile_in_flight_flag = std::sync::Arc::clone(&compile_in_flight);
    let compile_progress_logger = std::thread::spawn(move || {
        run_compile_chunk_progress_loop(
            compile_in_flight_flag,
            chunk_idx,
            chunk_count,
            chunk_models_for_log,
        );
    });

    let chunk_compile_start = Instant::now();
    let mut ordered_results: Vec<Option<MslModelResult>> = std::iter::repeat_with(|| None)
        .take(names_chunk.len())
        .collect();
    let compile_seconds = {
        let compile_timeout_secs = global_queue_compile_timeout_secs(
            names_chunk.len(),
            simulation_threads,
            model_budget_secs,
        );
        let _compile_watchdog = StageAbortWatchdog::new(
            queue_stage_label("compile", chunk_idx, chunk_count),
            compile_timeout_secs,
        );
        stream_source_root_compile_with_model_budgets(
            source_root,
            SourceRootCompileQueue {
                source_root_path,
                names_chunk,
                compile_threads: simulation_threads,
                budget_secs: model_budget_secs,
                cpu_tokens: pipeline_cpu_token_limiter(simulation_threads),
                run_simulation: false,
                sim_target_names: None,
            },
            |result_idx, entry| {
                ordered_results[result_idx] = Some(entry);
            },
        )
    };
    compile_in_flight.store(false, Ordering::Relaxed);
    let _ = compile_progress_logger.join();
    let pipeline_seconds = chunk_compile_start.elapsed().as_secs_f64();

    if log_parallelism {
        println!("Compile-only result conversion streamed during compile");
    }
    let model_results = ordered_results
        .into_iter()
        .map(|result| result.expect("every compiled model should produce a final result"))
        .collect();
    let drain_seconds = (pipeline_seconds - compile_seconds).max(0.0);

    StreamingChunkOutput {
        model_results,
        compile_seconds,
        drain_seconds,
    }
}

pub(super) fn run_chunked_compile_and_render(
    source_root: &std::sync::Arc<CompiledSourceRoot>,
    source_root_path: &Path,
    compile_names: &[String],
    run_simulation: bool,
    context: &RenderSimContext<'_>,
    worker_threads: usize,
) -> CompileRenderOutput {
    let compile_count = compile_names.len();
    if compile_count == 0 {
        return CompileRenderOutput {
            model_results: Vec::new(),
            compile_only_seconds: 0.0,
            render_and_write_seconds: 0.0,
            batch_size: 0,
            chunk_count: 0,
            worker_threads: 0,
        };
    }
    let effective_worker_threads = worker_threads_for_model_count(worker_threads, compile_count);

    let batch_size = compile_count;
    let chunk_count = 1;
    println!("  Compile scheduling: global work queue over {compile_count} models");
    if effective_worker_threads < worker_threads.max(1) {
        println!(
            "  Worker parallelism capped at {effective_worker_threads}/{} selected models",
            worker_threads.max(1)
        );
    }
    log_compile_batch_limit(run_simulation, compile_count);

    let model_budget_secs = model_attempt_timeout_secs();

    let mut output = if run_simulation {
        run_simulation_chunk(
            source_root,
            context,
            StreamingChunkPlan {
                source_root_path,
                names_chunk: compile_names,
                simulation_threads: effective_worker_threads,
                model_budget_secs,
                chunk_idx: 1,
                chunk_count,
                log_parallelism: true,
            },
        )
    } else {
        run_compile_only_chunk(
            source_root,
            context,
            StreamingChunkPlan {
                source_root_path,
                names_chunk: compile_names,
                simulation_threads: effective_worker_threads,
                model_budget_secs,
                chunk_idx: 1,
                chunk_count,
                log_parallelism: true,
            },
        )
    };

    println!("    global compile done in {:.2}s", output.compile_seconds);
    if run_simulation {
        println!(
            "    global sim/render done in {:.2}s (sim completed so far: {}/{})",
            output.drain_seconds,
            context.sim_completed.load(Ordering::Relaxed),
            context.total_sim_targets
        );
    }

    CompileRenderOutput {
        model_results: std::mem::take(&mut output.model_results),
        compile_only_seconds: output.compile_seconds,
        render_and_write_seconds: output.drain_seconds,
        batch_size,
        chunk_count,
        worker_threads: effective_worker_threads,
    }
}

pub(super) fn finalize_early_summary(
    mut summary: MslSummary,
    timings: &mut MslPhaseTimings,
    frontend_compile_start: Instant,
    core_start: Instant,
) -> MslSummary {
    timings.frontend_compile_seconds = frontend_compile_start.elapsed().as_secs_f64();
    timings.core_pipeline_seconds = core_start.elapsed().as_secs_f64();
    capture_process_peak_rss(timings);
    summary.timings = timings.clone();
    summary
}

pub(super) fn log_compile_scope(compile_count: usize) {
    println!("Compiling {} models...", compile_count);
    println!(
        "  Compiling {} models with memory-aware batching...",
        compile_count
    );
}

pub(super) fn simulation_threads_for_run(run_simulation: bool) -> usize {
    if run_simulation {
        simulation_parallelism()
    } else {
        compile_stage_parallelism()
    }
}

pub(super) struct PreparedSourceRoot {
    pub(super) source_root: std::sync::Arc<CompiledSourceRoot>,
    pub(super) model_names: Vec<String>,
    pub(super) class_type_counts: HashMap<String, usize>,
}
