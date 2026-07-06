use std::path::Path;
use std::time::{Duration, Instant};

use anyhow::{Context, Result, bail};
use clap::Args;
use serde::Serialize;

use rumoca_sim::{BuildSimulationTimings, SimOptions, SimSolverMode};

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

    /// Solver mode. Hot benchmarking uses the prepared BDF/diffsol path or the
    /// reusable rk-like session, depending on this selection.
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
    #[value(name = "rk-like")]
    RkLike,
}

impl From<BenchSolverMode> for SimulateSolverMode {
    fn from(value: BenchSolverMode) -> Self {
        match value {
            BenchSolverMode::Auto => SimulateSolverMode::Auto,
            BenchSolverMode::Bdf => SimulateSolverMode::Bdf,
            BenchSolverMode::RkLike => SimulateSolverMode::RkLike,
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

struct HotRunSummary {
    points: usize,
    final_time: Option<f64>,
}

enum PreparedHotBench {
    Bdf(Box<rumoca_sim::PreparedSimulation>),
    RkLike(Box<RkLikeHotBench>),
}

struct RkLikeHotBench {
    session: rumoca_sim::rk45::SimulationSession,
    sample_times: Vec<f64>,
    t_start: f64,
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
    prepare_ir_solve_structural_dae_seconds: f64,
    prepare_ir_solve_lower_seconds: f64,
    prepare_ir_solve_seconds: f64,
    prepare_override_apply_seconds: f64,
    prepare_backend_build_seconds: f64,
    prepare_accounted_seconds: f64,
    prepare_other_seconds: f64,
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
    let (mut prepared, prepare_timings) = PreparedHotBench::build(result.dae.as_ref(), &opts)?;
    let prepare_elapsed = prepare_start.elapsed();
    let prepare_seconds = duration_secs(prepare_elapsed);

    for _ in 0..args.warmups {
        prepared
            .run_hot()
            .map_err(|err| anyhow::anyhow!("warmup simulation failed: {err}"))?;
    }

    rumoca_sim::row_eval_trace::reset();
    let mut run_seconds = Vec::with_capacity(args.iterations);
    let mut last_points = 0;
    let mut last_final_time = None;
    for _ in 0..args.iterations {
        let run_start = Instant::now();
        let summary = prepared
            .run_hot()
            .map_err(|err| anyhow::anyhow!("hot simulation failed: {err}"))?;
        let elapsed = run_start.elapsed();
        run_seconds.push(duration_secs(elapsed));
        last_points = summary.points;
        last_final_time = summary.final_time;
    }
    rumoca_sim::row_eval_trace::snapshot("sim-bench");

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
        prepare_seconds,
        prepare_ir_solve_structural_dae_seconds: prepare_timings.ir_solve_structural_dae_seconds,
        prepare_ir_solve_lower_seconds: prepare_timings.ir_solve_lower_seconds,
        prepare_ir_solve_seconds: prepare_timings.ir_solve_seconds,
        prepare_override_apply_seconds: prepare_timings.override_apply_seconds,
        prepare_backend_build_seconds: prepare_timings.backend_build_seconds,
        prepare_accounted_seconds: prepare_timings.accounted_seconds(),
        prepare_other_seconds: prepare_other_seconds(prepare_seconds, prepare_timings),
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

impl PreparedHotBench {
    fn build(
        dae: &rumoca_compile::compile::Dae,
        opts: &SimOptions,
    ) -> Result<(Self, BuildSimulationTimings)> {
        match opts.solver_mode {
            SimSolverMode::RkLike => {
                let (bench, timings) = RkLikeHotBench::build(dae, opts)?;
                Ok((Self::RkLike(Box::new(bench)), timings))
            }
            SimSolverMode::Auto | SimSolverMode::Bdf => {
                rumoca_sim::build_simulation_with_stage_timing(dae, opts, ignore_build_stage)
                    .map(|(prepared, timings)| (Self::Bdf(Box::new(prepared)), timings))
                    .map_err(|err| anyhow::anyhow!("failed to prepare simulation: {err}"))
            }
        }
    }

    fn run_hot(&mut self) -> Result<HotRunSummary> {
        match self {
            Self::Bdf(prepared) => {
                let sim = prepared.run()?;
                Ok(HotRunSummary {
                    points: sim.times.len(),
                    final_time: sim.times.last().copied(),
                })
            }
            Self::RkLike(prepared) => prepared.run_hot(),
        }
    }
}

fn ignore_build_stage(_: &'static str) {}

impl RkLikeHotBench {
    fn build(
        dae: &rumoca_compile::compile::Dae,
        opts: &SimOptions,
    ) -> Result<(Self, BuildSimulationTimings)> {
        let (session, timings) =
            rumoca_sim::rk45::SimulationSession::new_with_stage_timing(dae, opts.clone(), |_| {})
                .map_err(|err| anyhow::anyhow!("failed to prepare rk-like simulation: {err}"))?;
        Ok((
            Self {
                session,
                sample_times: build_output_times(opts.t_start, opts.t_end, bench_output_dt(opts)),
                t_start: opts.t_start,
            },
            timings,
        ))
    }

    fn run_hot(&mut self) -> Result<HotRunSummary> {
        self.session
            .reset(self.t_start)
            .map_err(|err| anyhow::anyhow!("failed to reset rk-like simulation: {err}"))?;
        for &target in self.sample_times.iter().skip(1) {
            self.session
                .advance_to(target)
                .map_err(|err| anyhow::anyhow!("failed to step rk-like simulation: {err}"))?;
        }
        Ok(HotRunSummary {
            points: self.sample_times.len(),
            final_time: Some(self.session.time()),
        })
    }
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

fn bench_output_dt(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .unwrap_or_else(|| ((opts.t_end - opts.t_start).abs() / 500.0).max(1.0e-3))
}

fn build_output_times(t_start: f64, t_end: f64, dt: f64) -> Vec<f64> {
    if !dt.is_finite() || dt <= 0.0 {
        return if sample_time_match(t_start, t_end) {
            vec![t_start]
        } else {
            vec![t_start, t_end]
        };
    }

    let mut times = Vec::new();
    let mut k = 0usize;
    loop {
        let t_raw = t_start + (k as f64) * dt;
        if t_raw > t_end && !sample_time_match(t_raw, t_end) {
            break;
        }
        times.push(if sample_time_match(t_raw, t_end) {
            t_end
        } else {
            t_raw
        });
        if times.last().copied().is_some_and(|time| time >= t_end) {
            return times;
        }
        k += 1;
    }
    if times
        .last()
        .copied()
        .is_none_or(|last| !sample_time_match(last, t_end))
    {
        times.push(t_end);
    }
    times
}

fn sample_time_match(a: f64, b: f64) -> bool {
    let tol = 1e-12 * (1.0 + a.abs().max(b.abs()));
    (a - b).abs() <= tol
}

fn realtime_factor(sim_seconds: f64, wall_seconds: f64) -> f64 {
    if wall_seconds > 0.0 {
        sim_seconds / wall_seconds
    } else {
        f64::INFINITY
    }
}

fn prepare_other_seconds(total: f64, timings: BuildSimulationTimings) -> f64 {
    let other = total - timings.accounted_seconds();
    if other > 0.0 { other } else { 0.0 }
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
        "    Solve structural DAE: {:.6}s",
        report.prepare_ir_solve_structural_dae_seconds
    );
    println!(
        "    Solve IR lowering: {:.6}s",
        report.prepare_ir_solve_lower_seconds
    );
    println!(
        "    Apply overrides: {:.6}s",
        report.prepare_override_apply_seconds
    );
    println!(
        "    Backend build: {:.6}s",
        report.prepare_backend_build_seconds
    );
    println!("    Other: {:.6}s", report.prepare_other_seconds);
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
