use super::*;

/// Run full MSL compile pipeline using Session for parallel compilation.
///
/// Set `run_simulation=false` for compile+balance-only runs.
const COMPILE_CHUNK_PROGRESS_INTERVAL_SECS: u64 = 15;
const MODEL_ATTEMPT_TIMEOUT_ERROR_CODE: &str = "EMSL_TIMEOUT_MODEL_ATTEMPT";

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

pub(super) fn compile_batch_size() -> usize {
    std::env::var("RUMOCA_MSL_COMPILE_BATCH_SIZE")
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .filter(|size| *size > 0)
        .unwrap_or(24)
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

fn compile_model_with_budget_timeout(
    library: &std::sync::Arc<CompiledLibrary>,
    model_name: &str,
    budget_secs: f64,
) -> ModelCompileEntry {
    let start = Instant::now();
    let (sender, receiver) = std::sync::mpsc::channel();
    let library_ref = std::sync::Arc::clone(library);
    let model_name_for_worker = model_name.to_string();
    let model_name_for_result = model_name.to_string();
    let _detached_worker = std::thread::spawn(move || {
        let phase_result = library_ref.compile_model_phases(&model_name_for_worker);
        let _ = sender.send(phase_result);
    });

    let timeout = Duration::from_secs_f64(budget_secs);
    let elapsed_secs;
    let phase_result = match receiver.recv_timeout(timeout) {
        Ok(result) => {
            elapsed_secs = start.elapsed().as_secs_f64();
            result
        }
        Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
            elapsed_secs = start.elapsed().as_secs_f64();
            return ModelCompileEntry {
                model_name: model_name_for_result,
                phase_result: compile_timeout_phase_result(model_name, elapsed_secs, budget_secs),
                remaining_budget_secs: None,
            };
        }
        Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
            elapsed_secs = start.elapsed().as_secs_f64();
            PhaseResult::Failed {
                phase: FailedPhase::ToDae,
                error: format!(
                    "model compile worker disconnected after {:.3}s ({model_name})",
                    elapsed_secs
                ),
                error_code: Some("EMSL_COMPILE_WORKER_DISCONNECTED".to_string()),
            }
        }
    };
    let remaining_budget_secs = if phase_result.is_success() {
        Some((budget_secs - elapsed_secs).max(0.0))
    } else {
        None
    };
    ModelCompileEntry {
        model_name: model_name_for_result,
        phase_result,
        remaining_budget_secs,
    }
}

fn compile_chunk_with_model_budgets(
    library: &std::sync::Arc<CompiledLibrary>,
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
    while compile_in_flight_flag.load(Ordering::Relaxed) {
        std::thread::sleep(std::time::Duration::from_secs(
            COMPILE_CHUNK_PROGRESS_INTERVAL_SECS,
        ));
        if !compile_in_flight_flag.load(Ordering::Relaxed) {
            return;
        }
        eprintln!(
            "    chunk {}/{} compile still running after {:.1}s ({} models)",
            chunk_idx,
            chunk_count,
            start.elapsed().as_secs_f64(),
            chunk_models
        );
    }
}

fn run_chunked_compile_and_render(
    library: &std::sync::Arc<CompiledLibrary>,
    compile_names: &[String],
    run_simulation: bool,
    context: &RenderSimContext<'_>,
    worker_threads: usize,
) -> ChunkedCompileRenderOutput {
    let compile_count = compile_names.len();
    let base_batch_size = compile_batch_size().min(compile_count.max(1));
    let batch_size = if run_simulation {
        base_batch_size.max(worker_threads.max(1))
    } else {
        base_batch_size
    }
    .min(compile_count.max(1));
    let chunk_count = compile_count.div_ceil(batch_size.max(1));
    println!("  Compile batch size: {}", batch_size);

    let mut model_results = Vec::with_capacity(compile_count);
    let mut first_chunk = true;
    let mut compile_only_seconds = 0.0;
    let mut render_and_write_seconds = 0.0;
    let model_budget_secs = model_attempt_timeout_secs();

    for (chunk_idx, names_chunk) in compile_names.chunks(batch_size).enumerate() {
        println!(
            "  chunk {}/{}: compiling {} models",
            chunk_idx + 1,
            chunk_count,
            names_chunk.len()
        );
        let compile_in_flight = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(true));
        let compile_in_flight_flag = std::sync::Arc::clone(&compile_in_flight);
        let chunk_idx_for_log = chunk_idx + 1;
        let chunk_count_for_log = chunk_count;
        let chunk_models_for_log = names_chunk.len();
        let compile_progress_logger = std::thread::spawn(move || {
            run_compile_chunk_progress_loop(
                compile_in_flight_flag,
                chunk_idx_for_log,
                chunk_count_for_log,
                chunk_models_for_log,
            );
        });

        let chunk_compile_start = Instant::now();
        let _compile_watchdog = StageAbortWatchdog::new(
            format!("compile chunk {}/{}", chunk_idx + 1, chunk_count),
            "RUMOCA_MSL_STAGE_TIMEOUT_COMPILE_CHUNK_SECS",
            300,
        );
        let chunk_results = compile_chunk_with_model_budgets(
            library,
            names_chunk,
            worker_threads,
            model_budget_secs,
        );
        compile_in_flight.store(false, Ordering::Relaxed);
        let _ = compile_progress_logger.join();
        let chunk_compile_seconds = chunk_compile_start.elapsed().as_secs_f64();
        compile_only_seconds += chunk_compile_seconds;
        println!(
            "    chunk {}/{} compile done in {:.2}s",
            chunk_idx + 1,
            chunk_count,
            chunk_compile_seconds
        );

        let chunk_render_start = Instant::now();
        let _sim_chunk_watchdog = StageAbortWatchdog::new(
            format!("simulate/render chunk {}/{}", chunk_idx + 1, chunk_count),
            "RUMOCA_MSL_STAGE_TIMEOUT_SIM_CHUNK_SECS",
            300,
        );
        let mut chunk_model_results = collect_render_sim_results(
            chunk_results,
            run_simulation,
            context,
            worker_threads,
            first_chunk,
        );
        first_chunk = false;
        let chunk_render_seconds = chunk_render_start.elapsed().as_secs_f64();
        render_and_write_seconds += chunk_render_seconds;
        model_results.append(&mut chunk_model_results);
        if run_simulation {
            println!(
                "    chunk {}/{} sim/render done in {:.2}s (sim completed so far: {}/{})",
                chunk_idx + 1,
                chunk_count,
                chunk_render_seconds,
                context.sim_completed.load(Ordering::Relaxed),
                context.total_sim_targets
            );
        }
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
    println!("Creating session and building typed tree...");
    let session_start = Instant::now();
    let _session_watchdog = StageAbortWatchdog::new(
        "session_build",
        "RUMOCA_MSL_STAGE_TIMEOUT_SESSION_BUILD_SECS",
        300,
    );
    let mut session = Session::new(SessionConfig { parallel: false });
    session.add_parsed_batch(parsed_successes);

    let model_names = match session.model_names() {
        Ok(names) => names.to_vec(),
        Err(error) => {
            println!("Failed to build typed tree: {error}");
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
    let class_type_counts = session.class_type_counts().unwrap_or_default();
    let resolved_tree = match session.resolved() {
        Ok(resolved) => resolved,
        Err(error) => {
            println!("Failed to get resolved tree: {error}");
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
    let library = std::sync::Arc::new(CompiledLibrary::from_resolved_tree(
        resolved_tree,
        model_names.clone(),
    ));
    timings.session_build_seconds = session_start.elapsed().as_secs_f64();
    println!(
        "Built typed tree + model index in {:.2}s",
        timings.session_build_seconds
    );
    drop(session);
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
