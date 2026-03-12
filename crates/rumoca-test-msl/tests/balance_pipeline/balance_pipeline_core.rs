use super::*;

/// Run full MSL compile pipeline using Session for parallel compilation.
///
/// Set `run_simulation=false` for compile+balance-only runs.
const COMPILE_CHUNK_PROGRESS_INTERVAL_SECS: u64 = 15;
const COMPILE_CHUNK_PROGRESS_POLL_MILLIS: u64 = 250;
const MODEL_ATTEMPT_TIMEOUT_ERROR_CODE: &str = "EMSL_TIMEOUT_MODEL_ATTEMPT";
const STREAMING_COMPILE_RESULT_QUEUE_BOUND: usize = 1;
const SLOW_COMPILE_LOG_THRESHOLD_ENV: &str = "RUMOCA_MSL_SLOW_COMPILE_LOG_SECS";

trait FocusedClosureCompiler {
    fn strict_compile_for_focused_model(&self, model_name: &str) -> StrictCompileReport;
}

impl FocusedClosureCompiler for CompiledLibrary {
    fn strict_compile_for_focused_model(&self, model_name: &str) -> StrictCompileReport {
        self.compile_model_strict_reachable_uncached_with_recovery(model_name)
    }
}

pub(super) fn log_simulation_run_configuration(run_simulation: bool) {
    if !run_simulation {
        return;
    }
    println!(
        "Per-model attempt timeout (compile+simulation): {}s",
        model_attempt_timeout_secs()
    );
    if let Some(stop_time_override) = simulation_stop_time_override() {
        println!(
            "Simulation horizon mode: override stopTime={} via RUMOCA_MSL_SIM_STOP_TIME_OVERRIDE",
            stop_time_override
        );
    } else {
        println!("Simulation horizon mode: experiment StopTime when available");
    }
    println!(
        "Simulation experiment settings: applying annotation Tolerance/Interval/StartTime when valid"
    );
    if let Some(timeout_override) = sim_timeout_override_secs() {
        println!(
            "Simulation timeout override: {}s via RUMOCA_MSL_SIM_TIMEOUT_OVERRIDE",
            timeout_override
        );
    }
    if let Some(solver) = simulation_solver_override() {
        println!(
            "Simulation solver override: '{}' (accepts rumoca/OMC/Dymola-style names)",
            solver
        );
    }
}

pub(super) struct CompileSelection {
    compile_scope_count: usize,
    compile_names: Vec<String>,
}

pub(super) fn select_compile_models_for_run(
    model_names: &[String],
    run_simulation: bool,
) -> CompileSelection {
    let compile_scope_names =
        select_compile_targets_for_focused_simulation(model_names, run_simulation)
            .unwrap_or_else(|| model_names.to_vec());
    let compile_scope_count = compile_scope_names.len();
    CompileSelection {
        compile_scope_count,
        compile_names: compile_scope_names,
    }
}

fn compile_batch_size_from_override(run_simulation: bool, override_value: Option<&str>) -> usize {
    override_value
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|size| *size > 0)
        .unwrap_or(if run_simulation { 1 } else { 24 })
}

pub(super) fn compile_batch_size(run_simulation: bool) -> usize {
    compile_batch_size_from_override(
        run_simulation,
        std::env::var("RUMOCA_MSL_COMPILE_BATCH_SIZE")
            .ok()
            .as_deref(),
    )
}

fn slow_compile_log_threshold_secs_from_override(raw: Option<&str>) -> Option<f64> {
    raw.and_then(|value| value.trim().parse::<f64>().ok())
        .filter(|secs| secs.is_finite() && *secs > 0.0)
}

fn slow_compile_log_threshold_secs() -> Option<f64> {
    slow_compile_log_threshold_secs_from_override(
        std::env::var(SLOW_COMPILE_LOG_THRESHOLD_ENV)
            .ok()
            .as_deref(),
    )
}

fn effective_compile_chunk_batch_size(run_simulation: bool, compile_count: usize) -> usize {
    compile_batch_size(run_simulation).min(compile_count.max(1))
}

fn delta_compile_timing_stat(
    before: rumoca_session::compile::CompilePhaseTimingStat,
    after: rumoca_session::compile::CompilePhaseTimingStat,
) -> rumoca_session::compile::CompilePhaseTimingStat {
    rumoca_session::compile::CompilePhaseTimingStat {
        calls: after.calls.saturating_sub(before.calls),
        total_nanos: after.total_nanos.saturating_sub(before.total_nanos),
    }
}

fn delta_compile_phase_timing_snapshot(
    before: rumoca_session::compile::CompilePhaseTimingSnapshot,
    after: rumoca_session::compile::CompilePhaseTimingSnapshot,
) -> rumoca_session::compile::CompilePhaseTimingSnapshot {
    rumoca_session::compile::CompilePhaseTimingSnapshot {
        instantiate: delta_compile_timing_stat(before.instantiate, after.instantiate),
        typecheck: delta_compile_timing_stat(before.typecheck, after.typecheck),
        flatten: delta_compile_timing_stat(before.flatten, after.flatten),
        todae: delta_compile_timing_stat(before.todae, after.todae),
    }
}

fn delta_flatten_timing_stat(
    before: rumoca_phase_flatten::FlattenPhaseTimingStat,
    after: rumoca_phase_flatten::FlattenPhaseTimingStat,
) -> rumoca_phase_flatten::FlattenPhaseTimingStat {
    rumoca_phase_flatten::FlattenPhaseTimingStat {
        calls: after.calls.saturating_sub(before.calls),
        total_nanos: after.total_nanos.saturating_sub(before.total_nanos),
    }
}

fn delta_flatten_phase_timing_snapshot(
    before: rumoca_phase_flatten::FlattenPhaseTimingSnapshot,
    after: rumoca_phase_flatten::FlattenPhaseTimingSnapshot,
) -> rumoca_phase_flatten::FlattenPhaseTimingSnapshot {
    rumoca_phase_flatten::FlattenPhaseTimingSnapshot {
        connections: delta_flatten_timing_stat(before.connections, after.connections),
        eval_fallback: delta_flatten_timing_stat(before.eval_fallback, after.eval_fallback),
    }
}

fn log_slow_model_compile(
    model_name: &str,
    elapsed_secs: f64,
    compile_delta: rumoca_session::compile::CompilePhaseTimingSnapshot,
    flatten_delta: rumoca_phase_flatten::FlattenPhaseTimingSnapshot,
) {
    eprintln!(
        "    slow compile: model={model_name} elapsed={elapsed_secs:.2}s | instantiate={:.2}s/{} typecheck={:.2}s/{} flatten={:.2}s/{} todae={:.2}s/{} | flatten.connections={:.2}s/{} eval_fallback={:.2}s/{}",
        compile_delta.instantiate.total_seconds(),
        compile_delta.instantiate.calls,
        compile_delta.typecheck.total_seconds(),
        compile_delta.typecheck.calls,
        compile_delta.flatten.total_seconds(),
        compile_delta.flatten.calls,
        compile_delta.todae.total_seconds(),
        compile_delta.todae.calls,
        flatten_delta.connections.total_seconds(),
        flatten_delta.connections.calls,
        flatten_delta.eval_fallback.total_seconds(),
        flatten_delta.eval_fallback.calls,
    );
}

struct ChunkedCompileRenderOutput {
    model_results: Vec<MslModelResult>,
    compile_only_seconds: f64,
    render_and_write_seconds: f64,
}

struct ParsedMslBatch {
    total_mo_files: usize,
    parse_errors: usize,
    successes: Vec<(String, rumoca_ir_ast::StoredDefinition)>,
}

fn parse_msl_batch(msl_dir: &Path, timings: &mut MslPhaseTimings) -> ParsedMslBatch {
    let mo_files = find_mo_files(msl_dir);
    let total_mo_files = mo_files.len();
    println!("Parsing {} MSL files in parallel...", total_mo_files);
    let parse_start = Instant::now();
    let _parse_watchdog = StageAbortWatchdog::new(
        "parse_msl_batch",
        "RUMOCA_MSL_STAGE_TIMEOUT_PARSE_SECS",
        600,
    );
    let parse_threads = msl_stage_parallelism();
    let parse_work = || parse_files_parallel_lenient(&mo_files);
    let (successes, failures) = match rayon::ThreadPoolBuilder::new()
        .num_threads(parse_threads.max(1))
        .build()
    {
        Ok(pool) => pool.install(parse_work),
        Err(err) => {
            eprintln!(
                "WARNING: failed to build parse thread pool ({err}); falling back to global rayon pool"
            );
            parse_work()
        }
    };
    timings.parse_seconds = parse_start.elapsed().as_secs_f64();
    let parse_errors = failures.len();
    drop(failures);
    drop(mo_files);
    println!(
        "Parsed {} files successfully, {} failures in {:.2}s",
        successes.len(),
        parse_errors,
        timings.parse_seconds
    );
    ParsedMslBatch {
        total_mo_files,
        parse_errors,
        successes,
    }
}

fn compile_timeout_phase_result(
    model_name: &str,
    elapsed_secs: f64,
    budget_secs: f64,
) -> PhaseResult {
    PhaseResult::Failed {
        phase: FailedPhase::ToDae,
        error: format!(
            "model attempt timeout: compile exceeded {:.3}s budget after {:.3}s ({model_name})",
            budget_secs, elapsed_secs
        ),
        error_code: Some(MODEL_ATTEMPT_TIMEOUT_ERROR_CODE.to_string()),
    }
}

fn finalize_compile_entry(
    model_name: &str,
    compile_outcome: ModelCompileOutcome,
    elapsed_secs: f64,
    budget_secs: f64,
) -> ModelCompileEntry {
    if elapsed_secs > budget_secs {
        return ModelCompileEntry {
            model_name: model_name.to_string(),
            compile_outcome: ModelCompileOutcome::Phase(compile_timeout_phase_result(
                model_name,
                elapsed_secs,
                budget_secs,
            )),
            remaining_budget_secs: None,
        };
    }

    let remaining_budget_secs = if compile_outcome.is_success() {
        Some((budget_secs - elapsed_secs).max(0.0))
    } else {
        None
    };
    ModelCompileEntry {
        model_name: model_name.to_string(),
        compile_outcome,
        remaining_budget_secs,
    }
}

fn compile_model_with_budget_timeout<T: FocusedClosureCompiler + Sync + Send>(
    library: &std::sync::Arc<T>,
    model_name: &str,
    budget_secs: f64,
) -> ModelCompileEntry {
    let slow_log_threshold = slow_compile_log_threshold_secs();
    let compile_timing_before = slow_log_threshold.map(|_| compile_phase_timing_stats());
    let flatten_timing_before = slow_log_threshold.map(|_| flatten_phase_timing_stats());
    let start = Instant::now();
    // Compile synchronously inside the bounded Rayon worker pool. The earlier
    // detached-thread timeout path could not actually cancel compile work, so
    // timed-out models kept consuming memory in the background.
    let compile_outcome =
        ModelCompileOutcome::StrictReport(library.strict_compile_for_focused_model(model_name));
    let elapsed_secs = start.elapsed().as_secs_f64();
    if let (Some(threshold_secs), Some(before_compile), Some(before_flatten)) = (
        slow_log_threshold,
        compile_timing_before,
        flatten_timing_before,
    ) && elapsed_secs >= threshold_secs
    {
        let compile_delta =
            delta_compile_phase_timing_snapshot(before_compile, compile_phase_timing_stats());
        let flatten_delta =
            delta_flatten_phase_timing_snapshot(before_flatten, flatten_phase_timing_stats());
        log_slow_model_compile(model_name, elapsed_secs, compile_delta, flatten_delta);
    }
    finalize_compile_entry(model_name, compile_outcome, elapsed_secs, budget_secs)
}

fn compile_chunk_with_model_budgets<T: FocusedClosureCompiler + Sync + Send>(
    library: &std::sync::Arc<T>,
    names_chunk: &[String],
    compile_threads: usize,
    budget_secs: f64,
) -> Vec<ModelCompileEntry> {
    let compile_worker =
        |name: &String| compile_model_with_budget_timeout(library, name, budget_secs);
    match rayon::ThreadPoolBuilder::new()
        .num_threads(compile_threads.max(1))
        .build()
    {
        Ok(pool) => pool.install(|| names_chunk.par_iter().map(compile_worker).collect()),
        Err(err) => {
            eprintln!(
                "WARNING: failed to build compile thread pool ({err}); falling back to global rayon pool"
            );
            names_chunk.par_iter().map(compile_worker).collect()
        }
    }
}

fn run_compile_chunk_progress_loop(
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
            eprintln!(
                "    chunk {}/{} compile still running after {:.1}s ({} models)",
                chunk_idx,
                chunk_count,
                elapsed.as_secs_f64(),
                chunk_models
            );
            next_log_at += log_interval;
        }
        std::thread::sleep(poll_interval);
    }
}

struct StreamingChunkOutput {
    model_results: Vec<MslModelResult>,
    compile_seconds: f64,
    drain_seconds: f64,
}

enum StreamingPreparedEntry {
    Final(Box<MslModelResult>),
    PendingSimulation {
        model_result: Box<MslModelResult>,
        prepared_simulation: Box<PreparedSimulationRun>,
    },
}

struct StreamingChunkPlan<'a> {
    names_chunk: &'a [String],
    simulation_threads: usize,
    model_budget_secs: f64,
    chunk_idx: usize,
    chunk_count: usize,
    log_parallelism: bool,
}

fn finalize_simulation_into_model_result(
    mut model_result: MslModelResult,
    sim: MslSimModelResult,
    ctx: &RenderSimContext<'_>,
) -> MslModelResult {
    let done = ctx.sim_completed.fetch_add(1, Ordering::Relaxed) + 1;
    update_live_sim_status(&sim, ctx);
    maybe_log_sim_progress(done, ctx);
    model_result.sim_status = Some(sim.status.to_string());
    model_result.sim_error = sim.error;
    model_result.sim_seconds = sim.sim_seconds;
    model_result.sim_wall_seconds = sim.sim_wall_seconds;
    model_result.sim_trace_file = sim.sim_trace_file;
    model_result.sim_trace_error = sim.sim_trace_error;
    model_result
}

fn prepare_successful_streaming_entry(
    model_name: String,
    result: rumoca_session::compile::CompilationResult,
    remaining_budget_secs: Option<f64>,
    ctx: &RenderSimContext<'_>,
) -> StreamingPreparedEntry {
    maybe_dump_model_introspection(&model_name, &result, ctx);
    maybe_render_model_outputs(&model_name, &result, ctx);

    let model_result = summarize_success_result(model_name.clone(), &result);
    let should_simulate = ctx.run_simulation
        && !result.dae.is_partial
        && is_root_standalone_msl_example_model(&model_name, &result)
        && is_selected_sim_target(&model_name, ctx);
    if !should_simulate {
        return StreamingPreparedEntry::Final(Box::new(model_result));
    }

    ctx.sim_attempted.fetch_add(1, Ordering::Relaxed);
    let mut settings = simulation_settings_from_result(&result);
    if let Some(budget_secs) = remaining_budget_secs {
        if budget_secs <= 0.0 {
            let timeout_result = MslSimModelResult {
                name: model_name,
                status: SimStatus::Timeout,
                error: Some(
                    "model attempt timeout exhausted before simulation could start".to_string(),
                ),
                n_states: Some(result.dae.states.len()),
                n_algebraics: Some(result.dae.algebraics.len()),
                sim_seconds: Some(0.0),
                sim_wall_seconds: Some(0.0),
                sim_trace_file: None,
                sim_trace_error: None,
            };
            return StreamingPreparedEntry::Final(Box::new(finalize_simulation_into_model_result(
                model_result,
                timeout_result,
                ctx,
            )));
        }
        settings.timeout_seconds = Some(budget_secs);
    }

    let n_states = result.dae.states.len();
    let n_algebraics = result.dae.algebraics.len();
    let n_state_scalars: usize = result.dae.states.values().map(|value| value.size()).sum();
    let output_samples = output_samples_for_model(n_state_scalars);
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
        },
        Err(sim_result) => StreamingPreparedEntry::Final(Box::new(
            finalize_simulation_into_model_result(model_result, *sim_result, ctx),
        )),
    }
}

fn prepare_streaming_compile_result_entry(
    entry: ModelCompileEntry,
    ctx: &RenderSimContext<'_>,
) -> StreamingPreparedEntry {
    let ModelCompileEntry {
        model_name,
        compile_outcome,
        remaining_budget_secs,
    } = entry;

    match compile_outcome {
        ModelCompileOutcome::Phase(PhaseResult::Success(result)) => {
            prepare_successful_streaming_entry(model_name, *result, remaining_budget_secs, ctx)
        }
        ModelCompileOutcome::Phase(phase_result) => {
            StreamingPreparedEntry::Final(Box::new(convert_phase_result(model_name, phase_result)))
        }
        ModelCompileOutcome::StrictReport(report) => {
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
                prepare_successful_streaming_entry(model_name, *result, remaining_budget_secs, ctx)
            } else {
                StreamingPreparedEntry::Final(Box::new(convert_compile_outcome(
                    model_name,
                    ModelCompileOutcome::StrictReport(report),
                )))
            }
        }
    }
}

fn consume_streaming_prepared_entry(
    entry: StreamingPreparedEntry,
    ctx: &RenderSimContext<'_>,
) -> MslModelResult {
    match entry {
        StreamingPreparedEntry::Final(model_result) => *model_result,
        StreamingPreparedEntry::PendingSimulation {
            model_result,
            prepared_simulation,
        } => finalize_simulation_into_model_result(
            *model_result,
            run_prepared_simulation(*prepared_simulation),
            ctx,
        ),
    }
}

fn streaming_simulation_worker_loop(
    compile_rx: &std::sync::Arc<
        std::sync::Mutex<std::sync::mpsc::Receiver<(usize, StreamingPreparedEntry)>>,
    >,
    result_tx: &std::sync::mpsc::Sender<(usize, MslModelResult)>,
    context: &RenderSimContext<'_>,
) {
    loop {
        let next_entry = {
            let receiver = compile_rx.lock().expect("compile receiver mutex poisoned");
            receiver.recv()
        };
        let Ok((result_idx, entry)) = next_entry else {
            break;
        };
        let model_result = consume_streaming_prepared_entry(entry, context);
        if result_tx.send((result_idx, model_result)).is_err() {
            break;
        }
    }
}

fn run_streaming_compile_and_render_chunk<T: FocusedClosureCompiler + Sync + Send>(
    library: &std::sync::Arc<T>,
    context: &RenderSimContext<'_>,
    plan: StreamingChunkPlan<'_>,
) -> StreamingChunkOutput {
    let StreamingChunkPlan {
        names_chunk,
        simulation_threads,
        model_budget_secs,
        chunk_idx,
        chunk_count,
        log_parallelism,
    } = plan;

    if log_parallelism {
        println!("Simulation execution parallelism: {simulation_threads}");
    }

    let pipeline_start = Instant::now();
    let compile_start = Instant::now();
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

    let (compile_tx, compile_rx) = std::sync::mpsc::sync_channel::<(usize, StreamingPreparedEntry)>(
        STREAMING_COMPILE_RESULT_QUEUE_BOUND,
    );
    let compile_rx = std::sync::Arc::new(std::sync::Mutex::new(compile_rx));
    let (result_tx, result_rx) = std::sync::mpsc::channel::<(usize, MslModelResult)>();
    let mut compile_seconds = 0.0;

    std::thread::scope(|scope| {
        for _ in 0..simulation_threads.max(1) {
            let compile_rx = std::sync::Arc::clone(&compile_rx);
            let result_tx = result_tx.clone();
            scope.spawn(move || {
                streaming_simulation_worker_loop(&compile_rx, &result_tx, context);
            });
        }
        drop(result_tx);

        let _compile_watchdog = StageAbortWatchdog::new(
            format!("compile chunk {chunk_idx}/{chunk_count}"),
            "RUMOCA_MSL_STAGE_TIMEOUT_COMPILE_CHUNK_SECS",
            300,
        );
        for (result_idx, model_name) in names_chunk.iter().enumerate() {
            let entry = prepare_streaming_compile_result_entry(
                compile_model_with_budget_timeout(library, model_name, model_budget_secs),
                context,
            );
            if compile_tx.send((result_idx, entry)).is_err() {
                break;
            }
        }
        compile_seconds = compile_start.elapsed().as_secs_f64();
        compile_in_flight.store(false, Ordering::Relaxed);
        let _ = compile_progress_logger.join();
        drop(compile_tx);
    });

    let pipeline_seconds = pipeline_start.elapsed().as_secs_f64();
    let drain_seconds = (pipeline_seconds - compile_seconds).max(0.0);
    let mut ordered_results: Vec<Option<MslModelResult>> = std::iter::repeat_with(|| None)
        .take(names_chunk.len())
        .collect();
    for (result_idx, model_result) in result_rx {
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

fn run_simulation_chunk<T: FocusedClosureCompiler + Sync + Send>(
    library: &std::sync::Arc<T>,
    context: &RenderSimContext<'_>,
    plan: StreamingChunkPlan<'_>,
) -> StreamingChunkOutput {
    let StreamingChunkPlan {
        names_chunk,
        simulation_threads,
        model_budget_secs,
        chunk_idx,
        chunk_count,
        log_parallelism,
    } = plan;
    let _sim_chunk_watchdog = StageAbortWatchdog::new(
        format!("simulate/render chunk {chunk_idx}/{chunk_count}"),
        "RUMOCA_MSL_STAGE_TIMEOUT_SIM_CHUNK_SECS",
        300,
    );
    run_streaming_compile_and_render_chunk(
        library,
        context,
        StreamingChunkPlan {
            names_chunk,
            simulation_threads,
            model_budget_secs,
            chunk_idx,
            chunk_count,
            log_parallelism,
        },
    )
}

fn run_compile_only_chunk<T: FocusedClosureCompiler + Sync + Send>(
    library: &std::sync::Arc<T>,
    context: &RenderSimContext<'_>,
    plan: StreamingChunkPlan<'_>,
) -> StreamingChunkOutput {
    let StreamingChunkPlan {
        names_chunk,
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
    let chunk_results = {
        let _compile_watchdog = StageAbortWatchdog::new(
            format!("compile chunk {chunk_idx}/{chunk_count}"),
            "RUMOCA_MSL_STAGE_TIMEOUT_COMPILE_CHUNK_SECS",
            300,
        );
        compile_chunk_with_model_budgets(
            library,
            names_chunk,
            simulation_threads,
            model_budget_secs,
        )
    };
    compile_in_flight.store(false, Ordering::Relaxed);
    let _ = compile_progress_logger.join();
    let compile_seconds = chunk_compile_start.elapsed().as_secs_f64();

    let render_start = Instant::now();
    let model_results = collect_render_sim_results(
        chunk_results,
        false,
        context,
        simulation_threads,
        log_parallelism,
    );
    let drain_seconds = render_start.elapsed().as_secs_f64();

    StreamingChunkOutput {
        model_results,
        compile_seconds,
        drain_seconds,
    }
}

fn run_chunked_compile_and_render<T: FocusedClosureCompiler + Sync + Send>(
    library: &std::sync::Arc<T>,
    compile_names: &[String],
    run_simulation: bool,
    context: &RenderSimContext<'_>,
    worker_threads: usize,
) -> ChunkedCompileRenderOutput {
    let compile_count = compile_names.len();
    let batch_size = effective_compile_chunk_batch_size(run_simulation, compile_count);
    let chunk_count = compile_count.div_ceil(batch_size.max(1));
    println!("  Compile batch size: {}", batch_size);

    let mut model_results = Vec::with_capacity(compile_count);
    let mut first_chunk = true;
    let mut compile_only_seconds = 0.0;
    let mut render_and_write_seconds = 0.0;
    let model_budget_secs = model_attempt_timeout_secs();

    for (chunk_idx, names_chunk) in compile_names.chunks(batch_size).enumerate() {
        let chunk_idx = chunk_idx + 1;
        let chunk_models = names_chunk.join(", ");
        println!(
            "  chunk {}/{}: compiling {} models [{}]",
            chunk_idx,
            chunk_count,
            names_chunk.len(),
            chunk_models
        );
        if run_simulation {
            let mut chunk_output = run_simulation_chunk(
                library,
                context,
                StreamingChunkPlan {
                    names_chunk,
                    simulation_threads: worker_threads,
                    model_budget_secs,
                    chunk_idx,
                    chunk_count,
                    log_parallelism: first_chunk,
                },
            );
            first_chunk = false;
            compile_only_seconds += chunk_output.compile_seconds;
            render_and_write_seconds += chunk_output.drain_seconds;
            println!(
                "    chunk {}/{} compile done in {:.2}s",
                chunk_idx, chunk_count, chunk_output.compile_seconds
            );
            model_results.append(&mut chunk_output.model_results);
            println!(
                "    chunk {}/{} sim/render done in {:.2}s (sim completed so far: {}/{})",
                chunk_idx,
                chunk_count,
                chunk_output.drain_seconds,
                context.sim_completed.load(Ordering::Relaxed),
                context.total_sim_targets
            );
            continue;
        }

        let mut chunk_output = run_compile_only_chunk(
            library,
            context,
            StreamingChunkPlan {
                names_chunk,
                simulation_threads: worker_threads,
                model_budget_secs,
                chunk_idx,
                chunk_count,
                log_parallelism: first_chunk,
            },
        );
        first_chunk = false;
        compile_only_seconds += chunk_output.compile_seconds;
        render_and_write_seconds += chunk_output.drain_seconds;
        println!(
            "    chunk {}/{} compile done in {:.2}s",
            chunk_idx, chunk_count, chunk_output.compile_seconds
        );
        model_results.append(&mut chunk_output.model_results);
    }

    ChunkedCompileRenderOutput {
        model_results,
        compile_only_seconds,
        render_and_write_seconds,
    }
}

fn finalize_early_summary(
    mut summary: MslSummary,
    timings: &mut MslPhaseTimings,
    frontend_compile_start: Instant,
    core_start: Instant,
) -> MslSummary {
    timings.frontend_compile_seconds = frontend_compile_start.elapsed().as_secs_f64();
    timings.core_pipeline_seconds = core_start.elapsed().as_secs_f64();
    summary.timings = timings.clone();
    summary
}

fn log_compile_scope(compile_count: usize) {
    println!("Compiling {} models...", compile_count);
    println!("  Compiling {} models in parallel...", compile_count);
}

fn simulation_threads_for_run(run_simulation: bool) -> usize {
    if run_simulation {
        simulation_parallelism()
    } else {
        msl_stage_parallelism()
    }
}

struct PreparedLibrary {
    library: std::sync::Arc<CompiledLibrary>,
    model_names: Vec<String>,
    class_type_counts: HashMap<String, usize>,
}

fn prepare_compiled_library(
    parsed_successes: Vec<(String, rumoca_ir_ast::StoredDefinition)>,
    total_mo_files: usize,
    parse_errors: usize,
    timings: &mut MslPhaseTimings,
    frontend_compile_start: Instant,
    core_start: Instant,
) -> Result<PreparedLibrary, Box<MslSummary>> {
    println!("Building tolerant library index...");
    let session_start = Instant::now();
    let _session_watchdog = StageAbortWatchdog::new(
        "session_build",
        "RUMOCA_MSL_STAGE_TIMEOUT_SESSION_BUILD_SECS",
        300,
    );
    let library = match CompiledLibrary::from_parsed_batch_tolerant(parsed_successes) {
        Ok(library) => std::sync::Arc::new(library),
        Err(error) => {
            println!("Failed to build tolerant library index: {error}");
            let mut summary = empty_summary(total_mo_files, parse_errors);
            summary.resolve_errors = 1;
            timings.session_build_seconds = session_start.elapsed().as_secs_f64();
            return Err(Box::new(finalize_early_summary(
                summary,
                timings,
                frontend_compile_start,
                core_start,
            )));
        }
    };
    let model_names = library.model_names().to_vec();
    let class_type_counts = library.class_type_counts().clone();
    timings.session_build_seconds = session_start.elapsed().as_secs_f64();
    println!(
        "Built tolerant library index + model discovery in {:.2}s",
        timings.session_build_seconds
    );
    Ok(PreparedLibrary {
        library,
        model_names,
        class_type_counts,
    })
}

pub(super) fn run_msl_test(run_simulation: bool) -> MslSummary {
    let core_start = Instant::now();
    let mut timings = MslPhaseTimings::default();
    let frontend_compile_start = Instant::now();
    reset_compile_phase_timing_stats();
    reset_flatten_phase_timing_stats();
    prepare_sim_trace_dirs(run_simulation);

    let msl_dir = ensure_msl_downloaded().expect("Failed to download MSL");
    let parsed = parse_msl_batch(&msl_dir, &mut timings);

    if parsed.successes.is_empty() {
        println!("No files parsed successfully");
        return finalize_early_summary(
            empty_summary(parsed.total_mo_files, parsed.parse_errors),
            &mut timings,
            frontend_compile_start,
            core_start,
        );
    }

    let prepared = match prepare_compiled_library(
        parsed.successes,
        parsed.total_mo_files,
        parsed.parse_errors,
        &mut timings,
        frontend_compile_start,
        core_start,
    ) {
        Ok(prepared) => prepared,
        Err(summary) => return *summary,
    };

    let PreparedLibrary {
        library,
        model_names,
        class_type_counts,
    } = prepared;
    let total_models = model_names.len();
    println!("Found {} simulatable models in MSL", total_models);
    log_simulation_run_configuration(run_simulation);

    let selection = select_compile_models_for_run(&model_names, run_simulation);
    drop(model_names);
    let compile_count = selection.compile_names.len();
    log_compile_scope(compile_count);

    let setup = begin_chunked_render_sim_setup(&selection.compile_names, run_simulation);
    let context = setup.context(run_simulation);
    let simulation_threads = simulation_threads_for_run(run_simulation);

    let chunked_output = run_chunked_compile_and_render(
        &library,
        &selection.compile_names,
        run_simulation,
        &context,
        simulation_threads,
    );

    timings.compile_seconds = chunked_output.compile_only_seconds;
    timings.render_and_write_seconds = chunked_output.render_and_write_seconds;
    update_phase_timing_totals(&mut timings);
    timings.frontend_compile_seconds =
        timings.parse_seconds + timings.session_build_seconds + timings.compile_seconds;
    print_compile_timing_summary(compile_count, &timings);
    setup.print_summary(run_simulation);
    println!(
        "Rendered + wrote per-model artifacts in {:.2}s",
        timings.render_and_write_seconds
    );
    // Free the typed-tree/session memory before render+simulation.
    drop(library);

    let summary_inputs = MslSummaryInputs {
        total_mo_files: parsed.total_mo_files,
        parse_errors: parsed.parse_errors,
        total_models: selection.compile_scope_count,
        class_type_counts,
    };

    finalize_msl_summary_from_results(
        chunked_output.model_results,
        setup.sim_target_models(),
        summary_inputs,
        timings,
        core_start,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_session::compile::CompilationSummary;
    use std::sync::Mutex;

    struct FakeFocusedCompiler {
        uncached_called: Mutex<Vec<String>>,
    }

    impl FocusedClosureCompiler for FakeFocusedCompiler {
        fn strict_compile_for_focused_model(&self, model_name: &str) -> StrictCompileReport {
            self.uncached_called
                .lock()
                .expect("uncached call log should not be poisoned")
                .push(model_name.to_string());
            StrictCompileReport {
                requested_model: model_name.to_string(),
                requested_result: None,
                summary: CompilationSummary::default(),
                failures: Vec::new(),
                source_map: None,
            }
        }
    }

    #[test]
    fn compile_chunk_progress_loop_exits_promptly_after_flag_clears() {
        let compile_in_flight = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
        let compile_in_flight_flag = std::sync::Arc::clone(&compile_in_flight);
        let start = Instant::now();
        let worker = std::thread::spawn(move || {
            run_compile_chunk_progress_loop(compile_in_flight_flag, 1, 1, 1);
        });

        std::thread::sleep(Duration::from_millis(50));
        compile_in_flight.store(false, Ordering::Relaxed);
        worker.join().expect("progress logger thread should exit");

        assert!(
            start.elapsed() < Duration::from_secs(1),
            "progress logger must not add a full log-interval stall after chunk completion"
        );
    }

    #[test]
    fn finalize_compile_entry_converts_over_budget_compile_to_timeout() {
        let entry = finalize_compile_entry(
            "Modelica.Blocks.Examples.PID_Controller",
            ModelCompileOutcome::StrictReport(StrictCompileReport {
                requested_model: "Modelica.Blocks.Examples.PID_Controller".to_string(),
                requested_result: None,
                summary: CompilationSummary::default(),
                failures: Vec::new(),
                source_map: None,
            }),
            12.5,
            10.0,
        );

        assert!(entry.remaining_budget_secs.is_none());
        match entry.compile_outcome {
            ModelCompileOutcome::Phase(PhaseResult::Failed {
                error_code, error, ..
            }) => {
                assert_eq!(
                    error_code.as_deref(),
                    Some(MODEL_ATTEMPT_TIMEOUT_ERROR_CODE)
                );
                assert!(error.contains("compile exceeded 10.000s budget"));
            }
            _ => panic!("expected timeout failure"),
        }
    }

    #[test]
    fn finalize_compile_entry_preserves_under_budget_compile_outcome() {
        let entry = finalize_compile_entry(
            "Modelica.Blocks.Examples.PID_Controller",
            ModelCompileOutcome::StrictReport(StrictCompileReport {
                requested_model: "Modelica.Blocks.Examples.PID_Controller".to_string(),
                requested_result: None,
                summary: CompilationSummary::default(),
                failures: Vec::new(),
                source_map: None,
            }),
            2.5,
            10.0,
        );

        assert!(entry.remaining_budget_secs.is_none());
        match entry.compile_outcome {
            ModelCompileOutcome::StrictReport(report) => {
                assert_eq!(
                    report.requested_model,
                    "Modelica.Blocks.Examples.PID_Controller"
                );
            }
            _ => panic!("expected strict report"),
        }
    }

    #[test]
    fn compile_model_with_budget_timeout_uses_focused_uncached_compile_path() {
        let compiler = std::sync::Arc::new(FakeFocusedCompiler {
            uncached_called: Mutex::new(Vec::new()),
        });

        let entry = compile_model_with_budget_timeout(
            &compiler,
            "Modelica.Electrical.Digital.Examples.DFFREG",
            10.0,
        );

        assert!(entry.remaining_budget_secs.is_none());
        let calls = compiler
            .uncached_called
            .lock()
            .expect("uncached call log should not be poisoned");
        assert_eq!(
            calls.as_slice(),
            &["Modelica.Electrical.Digital.Examples.DFFREG".to_string()]
        );
    }

    #[test]
    fn compile_batch_size_defaults_to_one_for_simulation_runs() {
        assert_eq!(compile_batch_size_from_override(true, None), 1);
        assert_eq!(compile_batch_size_from_override(false, None), 24);
        assert_eq!(compile_batch_size_from_override(true, Some("4")), 4);
        assert_eq!(compile_batch_size_from_override(false, Some("2")), 2);
    }

    #[test]
    fn effective_simulation_compile_batch_size_stays_focused() {
        assert_eq!(effective_compile_chunk_batch_size(true, 8), 1);
        assert_eq!(effective_compile_chunk_batch_size(true, 0), 1);
        assert_eq!(effective_compile_chunk_batch_size(false, 8), 8);
    }

    #[test]
    fn slow_compile_log_threshold_parses_positive_numbers_only() {
        assert_eq!(slow_compile_log_threshold_secs_from_override(None), None);
        assert_eq!(
            slow_compile_log_threshold_secs_from_override(Some("")),
            None
        );
        assert_eq!(
            slow_compile_log_threshold_secs_from_override(Some("0")),
            None
        );
        assert_eq!(
            slow_compile_log_threshold_secs_from_override(Some("-1")),
            None
        );
        assert_eq!(
            slow_compile_log_threshold_secs_from_override(Some("12.5")),
            Some(12.5)
        );
    }
}

/// Test compilation, balance, and simulation of the default MSL 180-model target set.
///
/// This is the main regression test. It compiles the default explicit-example
/// target set, checks structural balance, and simulates those models.
#[test]
#[ignore = "slow-msl-full"] // Run with: cargo test --release --package rumoca-test-msl --test msl_tests test_msl_all -- --ignored --nocapture
pub(super) fn test_msl_all() {
    check_release_mode();
    let summary = run_msl_test(true);

    print_msl_balance_summary(&summary);

    print_simulation_results(&summary);
    print_timing_breakdown(&summary);
    print_failure_details(&summary);
    print_final_stats(&summary);
}
