use std::path::PathBuf;
use std::time::Instant;

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use rumoca_session::compile::{
    CompilationResult, Dae, PhaseResult, Session, SessionConfig, SourceRootKind,
    StrictCompileReport, VarName, Variable, compile_phase_timing_stats,
    reset_compile_phase_timing_stats,
};
use rumoca_session::source_roots::parse_source_root_with_cache;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode};
use rumoca_sim::{compiled_layout_binding_debug, compiled_layout_related_bindings_debug};
use rumoca_solver_diffsol::simulate_dae;

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

    /// Number of simulation repetitions to run after one focused compile.
    #[arg(long, default_value_t = 1)]
    repeat: usize,

    /// Inspect exact compiled DAE bindings for one or more flattened names.
    #[arg(long = "inspect-name")]
    inspect_names: Vec<String>,
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
    debug_log_named_equations(&result);
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

fn run_profiled_simulations(
    result: &CompilationResult,
    sim_options: &SimOptions,
    repeat: usize,
) -> Result<(Vec<std::time::Duration>, SimResult)> {
    let repeat = repeat.max(1);
    let mut elapsed = Vec::with_capacity(repeat);
    let mut last_result = None;
    for _ in 0..repeat {
        let sim_started = Instant::now();
        let sim_result = simulate_dae(&result.dae, sim_options)?;
        elapsed.push(sim_started.elapsed());
        last_result = Some(sim_result);
    }
    Ok((
        elapsed,
        last_result.expect("repeat count must produce at least one simulation result"),
    ))
}

fn format_elapsed_summary(elapsed: &[std::time::Duration]) -> String {
    let total_seconds: f64 = elapsed.iter().map(std::time::Duration::as_secs_f64).sum();
    let repeat = elapsed.len().max(1) as f64;
    let mean_seconds = total_seconds / repeat;
    format!(
        "total={:.2}s mean={:.2}ms repeat={}",
        total_seconds,
        mean_seconds * 1000.0,
        elapsed.len()
    )
}

fn inspect_dae_names(dae: &Dae, names: &[String]) {
    for name in names {
        let key = VarName::new(name);
        println!("Inspect name: {name}");
        inspect_dae_name_category("state", dae.states.get_key_value(&key));
        inspect_dae_name_category("algebraic", dae.algebraics.get_key_value(&key));
        inspect_dae_name_category("input", dae.inputs.get_key_value(&key));
        inspect_dae_name_category("output", dae.outputs.get_key_value(&key));
        inspect_dae_name_category("parameter", dae.parameters.get_key_value(&key));
        inspect_dae_name_category("constant", dae.constants.get_key_value(&key));
        inspect_dae_name_category("discrete_real", dae.discrete_reals.get_key_value(&key));
        inspect_dae_name_category("discrete_valued", dae.discrete_valued.get_key_value(&key));
        inspect_dae_name_category(
            "derivative_alias",
            dae.derivative_aliases.get_key_value(&key),
        );
        inspect_layout_binding(dae, name);
        inspect_dae_name_uses(dae, name);
        inspect_dae_function_matches(dae, name);
    }
}

fn inspect_dae_function_matches(dae: &Dae, needle: &str) {
    for (name, func) in &dae.functions {
        if !name.as_str().contains(needle) {
            continue;
        }
        let outputs = func
            .outputs
            .iter()
            .map(|output| output.name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        println!(
            "  function match: {} (inputs={}, outputs=[{}], body_stmts={})",
            name.as_str(),
            func.inputs.len(),
            outputs,
            func.body.len()
        );
    }
}

fn inspect_dae_name_category(label: &str, entry: Option<(&VarName, &Variable)>) {
    let Some((name, var)) = entry else {
        return;
    };
    println!(
        "  {label}: {} size={} dims={:?} has_start={}",
        name,
        var.size(),
        var.dims,
        var.start.is_some()
    );
    if let Some(start) = &var.start {
        println!("    start={start:?}");
    }
}

fn inspect_dae_name_uses(dae: &Dae, name: &str) {
    inspect_var_collection_uses("state", dae.states.values(), name);
    inspect_var_collection_uses("algebraic", dae.algebraics.values(), name);
    inspect_var_collection_uses("output", dae.outputs.values(), name);
    inspect_var_collection_uses("parameter", dae.parameters.values(), name);
    inspect_var_collection_uses("constant", dae.constants.values(), name);
    inspect_var_collection_uses("input", dae.inputs.values(), name);
    inspect_var_collection_uses("discrete_real", dae.discrete_reals.values(), name);
    inspect_var_collection_uses("discrete_valued", dae.discrete_valued.values(), name);

    for (idx, eq) in dae.f_x.iter().enumerate() {
        let rhs = format!("{:?}", eq.rhs);
        if eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == name) {
            println!("  f_x[{idx}] lhs defines {name}: {rhs}");
        }
        if rhs.contains(name) {
            println!("  f_x[{idx}] rhs uses {name}: {rhs}");
        }
    }
    for (idx, eq) in dae.f_z.iter().enumerate() {
        let rhs = format!("{:?}", eq.rhs);
        if eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == name) {
            println!("  f_z[{idx}] lhs defines {name}: {rhs}");
        }
        if rhs.contains(name) {
            println!("  f_z[{idx}] rhs uses {name}: {rhs}");
        }
    }
    for (idx, eq) in dae.f_m.iter().enumerate() {
        let rhs = format!("{:?}", eq.rhs);
        if eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == name) {
            println!("  f_m[{idx}] lhs defines {name}: {rhs}");
        }
        if rhs.contains(name) {
            println!("  f_m[{idx}] rhs uses {name}: {rhs}");
        }
    }
}

fn inspect_layout_binding(dae: &Dae, name: &str) {
    if let Some(slot) = compiled_layout_binding_debug(dae, name) {
        println!("  compiled_layout binding {name}: {slot:?}");
    }
    for (binding_name, slot) in compiled_layout_related_bindings_debug(dae, name) {
        println!("  compiled_layout related {binding_name}: {slot:?}");
    }
}

fn inspect_var_collection_uses<'a>(
    label: &str,
    vars: impl Iterator<Item = &'a Variable>,
    name: &str,
) {
    for var in vars {
        if let Some(start) = &var.start {
            let start_text = format!("{start:?}");
            if start_text.contains(name) {
                println!("  {label} {} start uses {name}: {start_text}", var.name);
            }
        }
        if let Some(nominal) = &var.nominal {
            let nominal_text = format!("{nominal:?}");
            if nominal_text.contains(name) {
                println!("  {label} {} nominal uses {name}: {nominal_text}", var.name);
            }
        }
    }
}

fn main() -> Result<()> {
    let args = Args::parse();
    let result = load_profiled_model(&args.source_root, &args.model)?;

    if !args.inspect_names.is_empty() {
        inspect_dae_names(&result.dae, &args.inspect_names);
    }

    if matches!(args.mode, ProfileMode::Compile) {
        return Ok(());
    }

    let sim_options = build_sim_options(&result, args.stop_time);
    let (elapsed, sim_result) = run_profiled_simulations(&result, &sim_options, args.repeat)
        .with_context(|| format!("simulation failed for {}", args.model))?;
    let sim_elapsed = *elapsed
        .last()
        .expect("repeat count must produce at least one simulation run");

    println!(
        "Simulation successful: elapsed={:.2?} points={} t_start={} t_end={} ({})",
        sim_elapsed,
        sim_result.times.len(),
        sim_options.t_start,
        sim_options.t_end,
        format_elapsed_summary(&elapsed)
    );
    Ok(())
}

fn debug_log_named_equations(compiled: &CompilationResult) {
    let Ok(patterns) = std::env::var("RUMOCA_MSL_PROFILE_DEBUG_EQ_PATTERNS") else {
        return;
    };
    let patterns: Vec<_> = patterns
        .split(',')
        .map(str::trim)
        .filter(|pattern| !pattern.is_empty())
        .collect();
    if patterns.is_empty() {
        return;
    }

    for (bucket, equations) in [
        ("f_x", &compiled.dae.f_x),
        ("f_z", &compiled.dae.f_z),
        ("f_m", &compiled.dae.f_m),
        ("f_c", &compiled.dae.f_c),
    ] {
        for (idx, eq) in equations.iter().enumerate() {
            let lhs = eq
                .lhs
                .as_ref()
                .map(|v| v.as_str())
                .unwrap_or("<none>")
                .to_string();
            let rhs_dbg = format!("{:?}", eq.rhs);
            let origin = eq.origin.as_str();
            if !patterns.iter().any(|pattern| {
                lhs.contains(pattern) || rhs_dbg.contains(pattern) || origin.contains(pattern)
            }) {
                continue;
            }
            eprintln!(
                "[msl-profile-debug] {bucket}[{idx}] lhs='{lhs}' origin='{}' rhs={rhs_dbg}",
                eq.origin
            );
        }
    }
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

    #[test]
    fn run_profiled_simulations_repeats_simulation_path() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result = load_profiled_model(&source_root, "Lib.M").expect("focused compile");
        let options = build_sim_options(&result, None);
        let (elapsed, sim_result) =
            run_profiled_simulations(&result, &options, 2).expect("repeat simulate succeeds");
        assert_eq!(elapsed.len(), 2);
        assert!(elapsed.iter().all(|duration| duration.as_nanos() > 0));
        assert!(!sim_result.times.is_empty());
    }
}
