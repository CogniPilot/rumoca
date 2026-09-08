use super::*;
use std::collections::HashSet;

pub(super) struct ModelWorkerQueue<'a> {
    pub(super) source_root_path: &'a Path,
    pub(super) names_chunk: &'a [String],
    pub(super) budget_secs: f64,
    pub(super) run_simulation: bool,
    pub(super) sim_target_names: Option<&'a HashSet<String>>,
    pub(super) explicit_sim_target: bool,
    pub(super) cpu_core_id: Option<usize>,
    pub(super) memory_tokens: Option<std::sync::Arc<ResourceTokenLimiter>>,
    pub(super) startup_barrier: std::sync::Arc<std::sync::Barrier>,
    pub(super) scheduler_stats: SchedulerStatsCollector,
    pub(super) next_model: &'a AtomicUsize,
    pub(super) result_tx: std::sync::mpsc::SyncSender<(usize, MslModelResult)>,
}

#[derive(Clone)]
pub(super) struct SchedulerStatsCollector {
    inner: std::sync::Arc<SchedulerStatsInner>,
}

struct SchedulerStatsInner {
    models_started: AtomicUsize,
    active_workers: AtomicUsize,
    max_active_workers: AtomicUsize,
    memory_token_wait_nanos: std::sync::atomic::AtomicU64,
    active_model_wall_nanos: std::sync::atomic::AtomicU64,
}

pub(super) struct SchedulerTimingInputs {
    pub(super) selected_model_count: usize,
    pub(super) requested_worker_threads: usize,
    pub(super) effective_worker_threads: usize,
    pub(super) worker_count: usize,
    pub(super) pinned_worker_count: usize,
    pub(super) compile_memory_token_capacity_mb: Option<usize>,
    pub(super) compile_memory_model_cost_mb: Option<usize>,
    pub(super) elapsed_seconds: f64,
}

struct ActiveModelGuard<'a> {
    stats: &'a SchedulerStatsCollector,
    started: Instant,
}

impl SchedulerStatsCollector {
    pub(super) fn new() -> Self {
        Self {
            inner: std::sync::Arc::new(SchedulerStatsInner {
                models_started: AtomicUsize::new(0),
                active_workers: AtomicUsize::new(0),
                max_active_workers: AtomicUsize::new(0),
                memory_token_wait_nanos: std::sync::atomic::AtomicU64::new(0),
                active_model_wall_nanos: std::sync::atomic::AtomicU64::new(0),
            }),
        }
    }

    fn record_model_started(&self) {
        self.inner.models_started.fetch_add(1, Ordering::Relaxed);
    }

    fn record_memory_wait(&self, elapsed: Duration) {
        self.inner.memory_token_wait_nanos.fetch_add(
            duration_nanos(elapsed),
            std::sync::atomic::Ordering::Relaxed,
        );
    }

    fn enter_active_model(&self) -> ActiveModelGuard<'_> {
        let active = self.inner.active_workers.fetch_add(1, Ordering::Relaxed) + 1;
        update_atomic_max(&self.inner.max_active_workers, active);
        ActiveModelGuard {
            stats: self,
            started: Instant::now(),
        }
    }

    pub(super) fn snapshot(&self, inputs: SchedulerTimingInputs) -> MslSchedulerTimings {
        let worker_slot_wall_seconds = inputs.elapsed_seconds * inputs.worker_count as f64;
        let active_model_wall_seconds =
            nanos_to_seconds(self.inner.active_model_wall_nanos.load(Ordering::Relaxed));
        MslSchedulerTimings {
            selected_model_count: inputs.selected_model_count,
            requested_worker_threads: inputs.requested_worker_threads,
            effective_worker_threads: inputs.effective_worker_threads,
            worker_count: inputs.worker_count,
            pinned_worker_count: inputs.pinned_worker_count,
            cpu_token_capacity: 0,
            compile_memory_token_capacity_mb: inputs.compile_memory_token_capacity_mb,
            compile_memory_model_cost_mb: inputs.compile_memory_model_cost_mb,
            models_started: self.inner.models_started.load(Ordering::Relaxed),
            max_active_workers: self.inner.max_active_workers.load(Ordering::Relaxed),
            cpu_token_wait_seconds: 0.0,
            compile_memory_token_wait_seconds: nanos_to_seconds(
                self.inner.memory_token_wait_nanos.load(Ordering::Relaxed),
            ),
            active_model_wall_seconds,
            worker_slot_wall_seconds,
            worker_slot_idle_seconds: (worker_slot_wall_seconds - active_model_wall_seconds)
                .max(0.0),
        }
    }
}

impl Drop for ActiveModelGuard<'_> {
    fn drop(&mut self) {
        self.stats
            .inner
            .active_workers
            .fetch_sub(1, Ordering::Relaxed);
        self.stats.inner.active_model_wall_nanos.fetch_add(
            duration_nanos(self.started.elapsed()),
            std::sync::atomic::Ordering::Relaxed,
        );
    }
}

fn duration_nanos(duration: Duration) -> u64 {
    duration.as_nanos().min(u64::MAX as u128) as u64
}

fn nanos_to_seconds(nanos: u64) -> f64 {
    Duration::from_nanos(nanos).as_secs_f64()
}

fn update_atomic_max(maximum: &AtomicUsize, value: usize) {
    let mut current = maximum.load(Ordering::Relaxed);
    while value > current {
        match maximum.compare_exchange_weak(current, value, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => return,
            Err(next) => current = next,
        }
    }
}

enum ModelWorkerSpawnOutcome {
    Spawned(ModelWorkerDaemon),
    Failed,
    ReceiverClosed,
}

fn spawn_model_worker(
    queue: &ModelWorkerQueue<'_>,
    idx: usize,
    name: &str,
    startup_timeout_secs: f64,
) -> ModelWorkerSpawnOutcome {
    match ModelWorkerDaemon::spawn(
        queue.source_root_path,
        startup_timeout_secs,
        queue.cpu_core_id,
        model_worker_memory_limit_mb(),
    ) {
        Ok(worker) => ModelWorkerSpawnOutcome::Spawned(worker),
        Err(error) => {
            let entry = model_worker_failure_result(
                name,
                MODEL_WORKER_ERROR_CODE,
                format!("model worker failed to start: {error}"),
            );
            if queue.result_tx.send((idx, entry)).is_ok() {
                ModelWorkerSpawnOutcome::Failed
            } else {
                ModelWorkerSpawnOutcome::ReceiverClosed
            }
        }
    }
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
        queue.scheduler_stats.record_model_started();
        if _worker_memory_permit.is_none() {
            _worker_memory_permit = match queue.memory_tokens.as_ref() {
                Some(tokens) => {
                    let (permit, elapsed) = tokens.acquire_timed(compile_model_memory_mb());
                    queue.scheduler_stats.record_memory_wait(elapsed);
                    Some(permit)
                }
                None => None,
            };
        }
        if worker.is_none() {
            match spawn_model_worker(&queue, idx, name, startup_timeout_secs) {
                ModelWorkerSpawnOutcome::Spawned(spawned) => worker = Some(spawned),
                ModelWorkerSpawnOutcome::Failed => continue,
                ModelWorkerSpawnOutcome::ReceiverClosed => return,
            }
        }
        let _active_model = queue.scheduler_stats.enter_active_model();
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
        if queue.result_tx.send((idx, entry)).is_err() {
            return;
        }
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
    scheduler: MslSchedulerTimings,
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
    let compile_output = stream_source_root_compile_with_model_budgets(
        source_root,
        SourceRootCompileQueue {
            source_root_path,
            names_chunk,
            compile_threads: simulation_threads,
            budget_secs: model_budget_secs,
            run_simulation: context.run_simulation,
            sim_target_names: context.sim_target_names,
        },
        |result_idx, result| {
            record_worker_model_result_progress(&result, context);
            indexed_results.push((result_idx, result));
        },
    );
    compile_in_flight.store(false, Ordering::Relaxed);
    compile_progress_logger
        .join()
        .expect("compile progress logger should not panic");

    let pipeline_seconds = pipeline_start.elapsed().as_secs_f64();
    let compile_seconds = compile_output.elapsed_seconds;
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
        scheduler: compile_output.scheduler,
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
    let compile_output = {
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
                run_simulation: false,
                sim_target_names: None,
            },
            |result_idx, entry| {
                ordered_results[result_idx] = Some(entry);
            },
        )
    };
    compile_in_flight.store(false, Ordering::Relaxed);
    compile_progress_logger
        .join()
        .expect("compile progress logger should not panic");
    let pipeline_seconds = chunk_compile_start.elapsed().as_secs_f64();
    let compile_seconds = compile_output.elapsed_seconds;

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
        scheduler: compile_output.scheduler,
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
            scheduler: MslSchedulerTimings::default(),
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
        scheduler: output.scheduler,
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
    println!(
        "  Model worker memory ceiling: {} MB resident-plus-swap per worker",
        model_worker_memory_limit_mb()
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
