use std::path::Path;
use std::time::{Duration, Instant};

use anyhow::{Context, Result, bail};
use clap::Args;
use serde::Serialize;

use rumoca_sim::{SimOptions, SimSolverMode};

use crate::cli::{
    DiagnosticsArgs, ModelInputArgs, ModelOptions, SimulateSolverMode,
    compile_dae_with_inferred_model, init_debug_tracing,
};
#[cfg(feature = "runner")]
use crate::cli::{configured_model_name, resolve_path};

#[derive(Args, Debug)]
#[command(
    arg_required_else_help = true,
    group(clap::ArgGroup::new("bench_source").required(true).multiple(true).args(["MODELICA_FILE", "config"])),
)]
pub struct SimBenchArgs {
    /// Modelica file to benchmark directly, or override the config's `model.file`
    /// with --config.
    #[arg(name = "MODELICA_FILE")]
    pub(crate) model_file: Option<String>,

    /// Run a rumoca-scenario.toml scenario (`rumoca-scenario.toml` /
    /// `rumoca-scenario.<profile>.toml`) instead of a direct benchmark.
    #[arg(short, long)]
    pub(crate) config: Option<String>,

    /// Shared model-selection options (--model / --source-root); with --config
    /// these override the config's `model` / `source_roots`.
    #[command(flatten)]
    pub(crate) model_options: ModelOptions,

    /// Number of timed hot simulation runs after compilation and solver preparation.
    #[arg(long, default_value_t = 5)]
    pub(crate) iterations: usize,

    /// Untimed hot simulation runs before measurement.
    #[arg(long, default_value_t = 1)]
    pub(crate) warmups: usize,

    /// Simulation end time. Direct runs default to 1.0; scenario runs use `sim.t_end`.
    #[arg(long)]
    pub(crate) t_end: Option<f64>,

    /// Optional fixed output interval (dt). If omitted, runtime chooses automatically.
    #[arg(long)]
    pub(crate) dt: Option<f64>,

    /// Solver mode. Prepared hot benchmarking uses the BDF/diffsol path; the
    /// rk-like backend is not benchmarkable here, so it is not offered.
    #[arg(long, value_enum)]
    pub(crate) solver: Option<BenchSolverMode>,

    /// Emit a JSON benchmark report instead of human-readable text.
    #[arg(long)]
    pub(crate) json: bool,

    #[command(flatten)]
    pub(crate) diagnostics: DiagnosticsArgs,
}

/// Solver modes offered by `sim bench`. The reusable prepared hot path measured
/// here is BDF/diffsol-only, so `rk-like` is intentionally not selectable (it
/// would always be rejected); see [`SimBenchArgs::solver`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub(crate) enum BenchSolverMode {
    Auto,
    Bdf,
}

impl From<BenchSolverMode> for SimulateSolverMode {
    fn from(value: BenchSolverMode) -> Self {
        match value {
            BenchSolverMode::Auto => SimulateSolverMode::Auto,
            BenchSolverMode::Bdf => SimulateSolverMode::Bdf,
        }
    }
}

struct BenchInput {
    input: ModelInputArgs,
    t_end: f64,
    dt: Option<f64>,
    solver_mode: SimSolverMode,
    solver_label: String,
}

#[derive(Debug, Serialize)]
struct SimBenchReport {
    model: String,
    solver: String,
    t_end: f64,
    dt: Option<f64>,
    iterations: usize,
    warmups: usize,
    compile_seconds: f64,
    prepare_seconds: f64,
    hot_total_seconds: f64,
    hot_average_seconds: f64,
    hot_best_seconds: f64,
    hot_worst_seconds: f64,
    average_realtime_factor: f64,
    best_realtime_factor: f64,
    last_points: usize,
    last_final_time: Option<f64>,
}

pub(crate) fn run_sim_bench(args: SimBenchArgs) -> Result<()> {
    if args.iterations == 0 {
        bail!("rumoca sim bench requires --iterations greater than zero");
    }

    let bench = resolve_bench_input(&args)?;
    if bench.solver_mode == SimSolverMode::RkLike {
        bail!(
            "rumoca sim bench measures the reusable prepared hot path, which currently uses \
             the BDF/diffsol backend; pass --solver bdf or --solver auto"
        );
    }

    init_debug_tracing(&args.diagnostics)?;

    let compile_start = Instant::now();
    let (result, model) = compile_dae_with_inferred_model(&bench.input, args.diagnostics.verbose)?;
    let compile_elapsed = compile_start.elapsed();

    let opts = SimOptions {
        t_end: bench.t_end,
        dt: bench.dt,
        solver_mode: bench.solver_mode,
        ..SimOptions::default()
    };

    let prepare_start = Instant::now();
    let prepared = rumoca_sim::build_simulation(result.dae.as_ref(), &opts)
        .map_err(|err| anyhow::anyhow!("failed to prepare simulation: {err}"))?;
    let prepare_elapsed = prepare_start.elapsed();

    for _ in 0..args.warmups {
        prepared
            .run()
            .map_err(|err| anyhow::anyhow!("warmup simulation failed: {err}"))?;
    }

    let mut run_seconds = Vec::with_capacity(args.iterations);
    let mut last_points = 0;
    let mut last_final_time = None;
    for _ in 0..args.iterations {
        let run_start = Instant::now();
        let sim = prepared
            .run()
            .map_err(|err| anyhow::anyhow!("hot simulation failed: {err}"))?;
        let elapsed = run_start.elapsed();
        run_seconds.push(duration_secs(elapsed));
        last_points = sim.times.len();
        last_final_time = sim.times.last().copied();
    }

    let hot_total_seconds = run_seconds.iter().sum::<f64>();
    let hot_average_seconds = hot_total_seconds / args.iterations as f64;
    let hot_best_seconds = run_seconds.iter().copied().fold(f64::INFINITY, f64::min);
    let hot_worst_seconds = run_seconds.iter().copied().fold(0.0, f64::max);
    let sim_span = (opts.t_end - opts.t_start).abs();
    let report = SimBenchReport {
        model,
        solver: bench.solver_label,
        t_end: opts.t_end,
        dt: opts.dt,
        iterations: args.iterations,
        warmups: args.warmups,
        compile_seconds: duration_secs(compile_elapsed),
        prepare_seconds: duration_secs(prepare_elapsed),
        hot_total_seconds,
        hot_average_seconds,
        hot_best_seconds,
        hot_worst_seconds,
        average_realtime_factor: realtime_factor(sim_span, hot_average_seconds),
        best_realtime_factor: realtime_factor(sim_span, hot_best_seconds),
        last_points,
        last_final_time,
    };

    if args.json {
        println!("{}", serde_json::to_string_pretty(&report)?);
    } else {
        print_human_report(&report);
    }
    Ok(())
}

fn resolve_bench_input(args: &SimBenchArgs) -> Result<BenchInput> {
    if let Some(config_path) = args.config.as_deref() {
        return resolve_config_bench_input(args, config_path);
    }

    let solver: SimulateSolverMode = args.solver.map_or(SimulateSolverMode::Auto, Into::into);
    let model_file = args
        .model_file
        .clone()
        .ok_or_else(|| anyhow::anyhow!("rumoca sim bench requires MODELICA_FILE or --config"))?;
    Ok(BenchInput {
        input: ModelInputArgs {
            model_file,
            options: args.model_options.clone(),
        },
        t_end: args.t_end.unwrap_or(1.0),
        dt: args.dt,
        solver_mode: solver.into(),
        solver_label: solver.as_label().to_string(),
    })
}

#[cfg(feature = "runner")]
fn resolve_config_bench_input(args: &SimBenchArgs, config_path: &str) -> Result<BenchInput> {
    let config = rumoca_sim::runner::config::SimulationConfig::load(Path::new(config_path))
        .with_context(|| format!("Load simulation config: {config_path}"))?;
    let config_dir = Path::new(config_path).parent().unwrap_or(Path::new("."));
    let model_path_str = args
        .model_file
        .clone()
        .or_else(|| config.model.as_ref().map(|model| model.file.clone()))
        .ok_or_else(|| {
            anyhow::anyhow!(
                "no model file specified: provide MODELICA_FILE or a [model].file in the config"
            )
        })?;
    let model_path = resolve_path(config_dir, &model_path_str);
    let model_name = configured_model_name(
        args.model_options.model.as_deref(),
        config.model.as_ref(),
        &model_path,
    );

    let mut source_roots = config
        .source_roots
        .iter()
        .map(|source_root| {
            resolve_path(config_dir, source_root)
                .to_string_lossy()
                .to_string()
        })
        .collect::<Vec<_>>();
    source_roots.extend(args.model_options.source_roots.iter().cloned());

    let solver_label = args
        .solver
        .map(|solver| SimulateSolverMode::from(solver).as_label().to_string())
        .or_else(|| config.sim.solver.clone())
        .unwrap_or_else(|| "auto".to_string());

    Ok(BenchInput {
        input: ModelInputArgs {
            model_file: model_path.to_string_lossy().to_string(),
            options: ModelOptions {
                model: Some(model_name),
                source_roots,
            },
        },
        t_end: args.t_end.unwrap_or(config.sim.t_end),
        dt: args.dt.or(Some(config.sim.dt)),
        solver_mode: SimSolverMode::from_external_name(&solver_label),
        solver_label,
    })
}

#[cfg(not(feature = "runner"))]
fn resolve_config_bench_input(_args: &SimBenchArgs, _config_path: &str) -> Result<BenchInput> {
    bail!(
        "this rumoca binary was built without simulation config support; \
         rebuild with --features=runner"
    );
}

fn duration_secs(duration: Duration) -> f64 {
    duration.as_secs_f64()
}

fn realtime_factor(sim_seconds: f64, wall_seconds: f64) -> f64 {
    if wall_seconds > 0.0 {
        sim_seconds / wall_seconds
    } else {
        f64::INFINITY
    }
}

fn print_human_report(report: &SimBenchReport) {
    println!("rumoca sim bench");
    println!("  Model: {}", report.model);
    println!("  Solver: {}", report.solver);
    println!(
        "  Horizon: {:.6}s, dt: {}",
        report.t_end,
        report
            .dt
            .map(|dt| format!("{dt:.6}s"))
            .unwrap_or_else(|| "auto".to_string())
    );
    println!(
        "  Runs: {} timed, {} warmup",
        report.iterations, report.warmups
    );
    println!("  Compile: {:.6}s", report.compile_seconds);
    println!("  Prepare: {:.6}s", report.prepare_seconds);
    println!(
        "  Hot run avg: {:.6}s ({:.2}x realtime)",
        report.hot_average_seconds, report.average_realtime_factor
    );
    println!(
        "  Hot run best: {:.6}s ({:.2}x realtime)",
        report.hot_best_seconds, report.best_realtime_factor
    );
    println!("  Hot run worst: {:.6}s", report.hot_worst_seconds);
    println!(
        "  Last run: {} points, final time {}",
        report.last_points,
        report
            .last_final_time
            .map(|time| format!("{time:.6}s"))
            .unwrap_or_else(|| "n/a".to_string())
    );
}
