use std::path::PathBuf;
use std::time::Instant;

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use rumoca_session::compile::{
    CompilationResult, PhaseResult, Session, SessionConfig, SourceRootKind, StrictCompileReport,
    compile_phase_timing_stats, reset_compile_phase_timing_stats,
};
use rumoca_session::runtime::{SimOptions, SimSolverMode, simulate_dae};
use rumoca_session::source_roots::parse_source_root_with_cache;

#[derive(Debug, Clone, Copy, ValueEnum)]
enum ProfileMode {
    Compile,
    Simulate,
}

#[derive(Parser, Debug)]
#[command(name = "rumoca-msl-profile")]
#[command(about = "Profile one focused MSL model through the session API")]
struct Args {
    /// Root directory of the extracted MSL release.
    #[arg(long)]
    source_root: PathBuf,

    /// Fully qualified model name to compile.
    #[arg(long)]
    model: String,

    /// Which focused path to profile.
    #[arg(long, value_enum, default_value_t = ProfileMode::Compile)]
    mode: ProfileMode,

    /// Simulation end time override when mode=simulate.
    #[arg(long)]
    stop_time: Option<f64>,
}

fn compile_report_to_result(report: StrictCompileReport) -> Result<Box<CompilationResult>> {
    match report.requested_result {
        Some(PhaseResult::Success(result)) => Ok(result),
        Some(PhaseResult::NeedsInner { missing_inners }) => bail!(
            "compilation requires inner bindings: {}",
            missing_inners.join(", ")
        ),
        Some(PhaseResult::Failed {
            phase,
            error,
            error_code,
        }) => {
            if let Some(code) = error_code {
                bail!("compilation failed in {phase} [{code}]: {error}");
            }
            bail!("compilation failed in {phase}: {error}");
        }
        None => bail!("{}", report.failure_summary(8)),
    }
}

fn print_compile_phase_snapshot() {
    let timing = compile_phase_timing_stats();
    println!(
        "Compile phase totals: instantiate {:.2}s ({} calls), typecheck {:.2}s ({} calls), flatten {:.2}s ({} calls), todae {:.2}s ({} calls)",
        timing.instantiate.total_seconds(),
        timing.instantiate.calls,
        timing.typecheck.total_seconds(),
        timing.typecheck.calls,
        timing.flatten.total_seconds(),
        timing.flatten.calls,
        timing.todae.total_seconds(),
        timing.todae.calls
    );
}

fn load_profiled_model(
    source_root: &std::path::Path,
    model: &str,
) -> Result<Box<CompilationResult>> {
    let parsed = parse_source_root_with_cache(source_root).with_context(|| {
        format!(
            "failed to parse Modelica source root under {}",
            source_root.display()
        )
    })?;

    let mut session = Session::new(SessionConfig::default());
    let inserted = session.replace_parsed_source_set(
        "profile-msl",
        SourceRootKind::DurableExternal,
        parsed.documents,
        None,
    );
    println!(
        "Loaded {} parsed source-root documents from {} (cache: {:?})",
        inserted,
        source_root.display(),
        parsed.cache_status
    );

    reset_compile_phase_timing_stats();
    let compile_started = Instant::now();
    let report = session.compile_model_strict_reachable_uncached_with_recovery(model);
    let compile_elapsed = compile_started.elapsed();
    let result = compile_report_to_result(report)?;

    println!(
        "Focused compile elapsed: {:.2?} for {}",
        compile_elapsed, model
    );
    print_compile_phase_snapshot();
    println!(
        "Compilation successful: states={} algebraics={} equations={}",
        result.dae.states.len(),
        result.dae.algebraics.len(),
        result.dae.f_x.len()
    );
    Ok(result)
}

fn build_sim_options(result: &CompilationResult, stop_time_override: Option<f64>) -> SimOptions {
    let mut sim_options = SimOptions {
        solver_mode: SimSolverMode::Auto,
        ..SimOptions::default()
    };
    sim_options.t_start = result.experiment_start_time.unwrap_or(0.0);
    sim_options.t_end = stop_time_override
        .or(result.experiment_stop_time)
        .unwrap_or(1.0)
        .max(sim_options.t_start);
    if let Some(tolerance) = result.experiment_tolerance {
        sim_options.rtol = tolerance;
        sim_options.atol = tolerance;
    }
    sim_options.dt = result
        .experiment_interval
        .filter(|value| value.is_finite() && *value > 0.0);
    sim_options.solver_mode = result
        .experiment_solver
        .as_deref()
        .map(SimSolverMode::from_external_name)
        .unwrap_or(SimSolverMode::Auto);
    sim_options
}

fn main() -> Result<()> {
    let args = Args::parse();
    let result = load_profiled_model(&args.source_root, &args.model)?;

    if matches!(args.mode, ProfileMode::Compile) {
        return Ok(());
    }

    let sim_options = build_sim_options(&result, args.stop_time);
    let sim_started = Instant::now();
    let sim_result = simulate_dae(&result.dae, &sim_options)
        .with_context(|| format!("simulation failed for {}", args.model))?;
    let sim_elapsed = sim_started.elapsed();

    println!(
        "Simulation successful: elapsed={:.2?} points={} t_start={} t_end={}",
        sim_elapsed,
        sim_result.times.len(),
        sim_options.t_start,
        sim_options.t_end
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_library(temp: &tempfile::TempDir) -> PathBuf {
        let source_root = temp.path().join("Lib");
        std::fs::create_dir_all(&source_root).expect("mkdir");
        std::fs::write(
            source_root.join("package.mo"),
            r#"
within ;
package Lib
  model M
    Real x(start=1);
  equation
    der(x) = -x;
  annotation(
    experiment(StartTime=0.25, StopTime=1.5, Interval=0.125, Tolerance=1e-4, Solver="dassl")
  );
  end M;
end Lib;
"#,
        )
        .expect("write package");
        source_root
    }

    #[test]
    fn load_profiled_model_compiles_minimal_source_root() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result = load_profiled_model(&source_root, "Lib.M").expect("focused compile");
        assert_eq!(result.dae.states.len(), 1);
        assert_eq!(result.experiment_stop_time, Some(1.5));
    }

    #[test]
    fn build_sim_options_uses_experiment_metadata() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result = load_profiled_model(&source_root, "Lib.M").expect("focused compile");
        let options = build_sim_options(&result, None);
        assert_eq!(options.t_start, 0.25);
        assert_eq!(options.t_end, 1.5);
        assert_eq!(options.dt, Some(0.125));
        assert_eq!(options.rtol, 1e-4);
        assert_eq!(options.atol, 1e-4);
        assert_eq!(options.solver_mode, SimSolverMode::Bdf);
    }

    #[test]
    fn build_sim_options_honors_stop_time_override() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result = load_profiled_model(&source_root, "Lib.M").expect("focused compile");
        let options = build_sim_options(&result, Some(2.0));
        assert_eq!(options.t_end, 2.0);
    }
}
