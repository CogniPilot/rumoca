use super::common::MslPaths;
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use rumoca_worker::{
    MODEL_WORKER_PROTOCOL_VERSION, ModelWorkerPhaseMonitor, ModelWorkerRequest,
    read_model_worker_response_file, write_model_worker_request_file,
};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

#[derive(Debug, Clone, ClapArgs)]
pub struct Args {
    /// Exact MSL model name to debug.
    #[arg(long)]
    model: String,
    /// Run simulation after successful DAE/Solve lowering.
    #[arg(long)]
    simulate: bool,
    /// Materialize DAE artifacts even when strict balance validation rejects the model.
    #[arg(long)]
    allow_unbalanced: bool,
    /// Record a perf.data profile around the worker process.
    #[arg(long)]
    perf: bool,
    /// perf sample frequency.
    #[arg(long, default_value_t = 99)]
    perf_frequency: usize,
    /// Worker per-phase timeout in seconds.
    #[arg(long, default_value_t = 10.0)]
    timeout_secs: f64,
    /// Use dev worker build instead of release.
    #[arg(long)]
    dev: bool,
    /// Trace NaN/non-finite runtime values, naming the offending variables
    /// (and, for Jacobians, the differentiated/seeded variables with spans).
    #[arg(long)]
    nan_trace: bool,
    /// Also emit the machine-exact IR JSON (`ir-*.json`) alongside the
    /// human-readable Modelica stage dumps. Use when you need exact op/index or
    /// span detail; the readable `ir-*.mo` files are emitted by default.
    #[arg(long)]
    json: bool,
}

pub fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    ensure_msl_cache_exists(&paths)?;
    let output_dir = paths
        .results_dir
        .join("model_worker")
        .join(model_artifact_dir_name(&args.model));
    std::fs::create_dir_all(&output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;

    let worker_exe = build_worker(&paths.repo_root, args.dev)?;
    let request_json = output_dir.join("request.json");
    let request = ModelWorkerRequest {
        protocol_version: MODEL_WORKER_PROTOCOL_VERSION,
        model_name: args.model.clone(),
        run_simulation: args.simulate,
        selected_for_simulation: args.simulate,
        explicit_sim_target: args.simulate,
        emit_json: args.json,
        allow_unbalanced_for_diagnostics: args.allow_unbalanced,
        nan_trace: args.nan_trace,
        emit_modelica: true,
        source_root_path: paths.msl_dir.clone(),
        output_dir: output_dir.clone(),
    };
    write_model_worker_request_file(&request_json, &request)
        .map_err(anyhow::Error::msg)
        .with_context(|| format!("failed to write {}", request_json.display()))?;

    run_worker_process(&worker_exe, &request_json, &output_dir, &args)?;
    let result_json = output_dir.join("result.json");
    let response = read_model_worker_response_file(&result_json)
        .map_err(anyhow::Error::msg)
        .with_context(|| format!("failed to read {}", result_json.display()))?;
    println!("model: {}", response.result.model_name);
    println!("phase: {}", response.result.phase_reached);
    if let Some(error) = &response.result.error {
        println!("error: {error}");
    }
    println!("elapsed: {:.3}s", response.elapsed_secs);
    println!("artifacts: {}", output_dir.display());
    Ok(())
}

fn ensure_msl_cache_exists(paths: &MslPaths) -> Result<()> {
    let modelica_package = paths.msl_dir.join("Modelica 4.1.0").join("package.mo");
    if modelica_package.is_file() {
        return Ok(());
    }
    bail!(
        "MSL cache not found at {}; run the MSL test once to populate target/msl",
        paths.msl_dir.display()
    )
}

fn build_worker(repo_root: &Path, dev: bool) -> Result<PathBuf> {
    let mut command = Command::new("cargo");
    command
        .arg("build")
        .arg("-p")
        .arg("rumoca-worker")
        .arg("--bin")
        .arg("rumoca-worker")
        .current_dir(repo_root);
    if !dev {
        command.arg("--release");
    }
    let status = command.status().context("failed to build rumoca-worker")?;
    if !status.success() {
        bail!("failed to build rumoca-worker");
    }
    Ok(target_dir(repo_root).join(if dev {
        "debug/rumoca-worker"
    } else {
        "release/rumoca-worker"
    }))
}

fn target_dir(repo_root: &Path) -> PathBuf {
    match std::env::var_os("CARGO_TARGET_DIR") {
        Some(value) => {
            let path = PathBuf::from(value);
            if path.is_absolute() {
                path
            } else {
                repo_root.join(path)
            }
        }
        None => repo_root.join("target"),
    }
}

fn run_worker_process(
    worker_exe: &Path,
    request_json: &Path,
    output_dir: &Path,
    args: &Args,
) -> Result<()> {
    let mut command = if args.perf {
        let mut command = Command::new("perf");
        command
            .arg("record")
            .arg("-F")
            .arg(args.perf_frequency.to_string())
            .arg("-o")
            .arg(output_dir.join("perf.data"))
            .arg("--")
            .arg(worker_exe);
        command
    } else {
        Command::new(worker_exe)
    };
    command
        .env("RAYON_NUM_THREADS", "1")
        .env("MIMALLOC_ARENA_EAGER_COMMIT", "0")
        .env("MIMALLOC_PURGE_DELAY", "0");
    command.arg("--request-json").arg(request_json);
    command.arg("--jobs").arg("1");
    let mut child = command.spawn().context("failed to spawn rumoca-worker")?;
    let progress_jsonl = output_dir.join("progress.jsonl");
    let start = Instant::now();
    let phase_timeout = Duration::from_secs_f64(args.timeout_secs);
    let mut phase_monitor = ModelWorkerPhaseMonitor::new();
    loop {
        let active_phase = phase_monitor.update(&progress_jsonl);
        if let Some(active_phase) = active_phase
            && phase_monitor.phase_elapsed() >= phase_timeout
        {
            let _ = child.kill();
            let _ = child.wait();
            bail!(
                "rumoca-worker timed out after {:.3}s in phase {}",
                args.timeout_secs,
                active_phase
            );
        }
        if active_phase.is_none()
            && !phase_monitor.has_seen_progress()
            && start.elapsed() >= phase_timeout
        {
            let _ = child.kill();
            let _ = child.wait();
            bail!(
                "rumoca-worker timed out after {:.3}s before reporting progress",
                args.timeout_secs
            );
        }
        if let Some(status) = child.try_wait().context("failed to poll rumoca-worker")? {
            if status.success() {
                return Ok(());
            }
            bail!("rumoca-worker failed with status {status}");
        }
        std::thread::sleep(Duration::from_millis(20));
    }
}

fn model_artifact_dir_name(model_name: &str) -> String {
    model_name
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || matches!(ch, '.' | '-' | '_') {
                ch
            } else {
                '_'
            }
        })
        .collect()
}
