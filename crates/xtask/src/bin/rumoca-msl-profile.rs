use std::path::PathBuf;
use std::time::Instant;

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use rumoca_compile::analysis::{balance, balance_detail};
use rumoca_compile::compile::{
    CompilationResult, Dae, PhaseResult, Session, SessionConfig, SourceRootKind,
    StrictCompileReport, VarName, Variable, compile_phase_timing_stats, core as rumoca_core,
    reset_compile_phase_timing_stats,
};
use rumoca_compile::source_roots::parse_source_root_with_cache;
use rumoca_sim::simulate_dae;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode};
use rumoca_sim::{compiled_layout_binding_debug, compiled_layout_related_bindings_debug};
use std::collections::BTreeMap;

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

    /// Print simulated values for one or more result variables.
    #[arg(long = "inspect-sim-name")]
    inspect_sim_names: Vec<String>,

    /// Print inspected simulation values at the nearest output sample to this time.
    #[arg(long = "inspect-sim-time")]
    inspect_sim_times: Vec<f64>,

    /// Materialize DAE even when strict balance validation would reject it.
    #[arg(long)]
    allow_unbalanced: bool,

    /// Directory for focused JSON artifacts.
    #[arg(long)]
    artifact_dir: Option<PathBuf>,
}

fn compile_report_to_result(report: StrictCompileReport) -> Result<Box<CompilationResult>> {
    match report.requested_result {
        Some(PhaseResult::Success(result)) => Ok(result),
        Some(PhaseResult::NeedsInner { missing_inners, .. }) => bail!(
            "compilation requires inner bindings: {}",
            missing_inners.join(", ")
        ),
        Some(PhaseResult::Failed {
            phase,
            error,
            error_code,
            ..
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

fn write_strict_flat_artifact(
    session: &mut Session,
    model: &str,
    artifact_dir: Option<&std::path::Path>,
) -> Result<()> {
    let Some(artifact_dir) = artifact_dir else {
        return Ok(());
    };
    let flat = session
        .compile_model_flat_strict_reachable_uncached_with_recovery(model)
        .map_err(anyhow::Error::msg)?;
    std::fs::create_dir_all(artifact_dir)
        .with_context(|| format!("failed to create {}", artifact_dir.display()))?;
    write_artifact(&artifact_dir.join("ir-flat.json"), &flat)
}

fn load_profiled_model(
    source_root: &std::path::Path,
    model: &str,
    allow_unbalanced: bool,
    artifact_dir: Option<&std::path::Path>,
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
    let result = if allow_unbalanced {
        write_strict_flat_artifact(&mut session, model, artifact_dir)?;
        match session.compile_model_allow_unbalanced_for_diagnostics(model) {
            Ok(result) => Box::new(result),
            Err(error) => {
                return Err(error);
            }
        }
    } else {
        let report = session.compile_model_strict_reachable_uncached_with_recovery(model);
        compile_report_to_result(report)?
    };
    let compile_elapsed = compile_started.elapsed();

    println!(
        "Focused compile elapsed: {:.2?} for {}",
        compile_elapsed, model
    );
    print_compile_phase_snapshot();
    println!(
        "Compilation successful: states={} algebraics={} equations={}",
        result.dae.variables.states.len(),
        result.dae.variables.algebraics.len(),
        result.dae.continuous.equations.len()
    );
    let detail = balance_detail(&result.dae)?;
    println!("Balance detail:\n{detail}");
    println!("Balance result: {}", balance(&result.dae)?);
    debug_log_balance_summary(&result.dae);
    debug_log_unknown_summary(&result.dae);
    if let Some(artifact_dir) = artifact_dir {
        write_artifacts(artifact_dir, &result)?;
    }
    Ok(result)
}

fn write_artifacts(artifact_dir: &std::path::Path, result: &CompilationResult) -> Result<()> {
    std::fs::create_dir_all(artifact_dir)
        .with_context(|| format!("failed to create {}", artifact_dir.display()))?;
    write_artifact(&artifact_dir.join("ir-flat.json"), &result.flat)?;
    write_artifact(&artifact_dir.join("ir-dae.json"), &result.dae)?;
    Ok(())
}

fn write_artifact<T: serde::Serialize>(path: &std::path::Path, value: &T) -> Result<()> {
    let file = std::fs::File::create(path)
        .with_context(|| format!("failed to create {}", path.display()))?;
    serde_json::to_writer_pretty(file, value)
        .with_context(|| format!("failed to write {}", path.display()))?;
    println!("wrote {}", path.display());
    Ok(())
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

fn inspect_dae_names(dae: &Dae, names: &[String]) -> Result<()> {
    for name in names {
        let key = VarName::new(name);
        println!("Inspect name: {name}");
        inspect_dae_name_category("state", dae.variables.states.get_key_value(&key));
        inspect_dae_name_category("algebraic", dae.variables.algebraics.get_key_value(&key));
        inspect_dae_name_category("input", dae.variables.inputs.get_key_value(&key));
        inspect_dae_name_category("output", dae.variables.outputs.get_key_value(&key));
        inspect_dae_name_category("parameter", dae.variables.parameters.get_key_value(&key));
        inspect_dae_name_category("constant", dae.variables.constants.get_key_value(&key));
        inspect_dae_name_category(
            "discrete_real",
            dae.variables.discrete_reals.get_key_value(&key),
        );
        inspect_dae_name_category(
            "discrete_valued",
            dae.variables.discrete_valued.get_key_value(&key),
        );
        inspect_layout_binding(dae, name)?;
        inspect_dae_name_uses(dae, name);
        inspect_dae_function_matches(dae, name);
    }
    Ok(())
}

fn inspect_dae_function_matches(dae: &Dae, needle: &str) {
    for (name, func) in &dae.symbols.functions {
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
    inspect_var_collection_uses("state", dae.variables.states.values(), name);
    inspect_var_collection_uses("algebraic", dae.variables.algebraics.values(), name);
    inspect_var_collection_uses("output", dae.variables.outputs.values(), name);
    inspect_var_collection_uses("parameter", dae.variables.parameters.values(), name);
    inspect_var_collection_uses("constant", dae.variables.constants.values(), name);
    inspect_var_collection_uses("input", dae.variables.inputs.values(), name);
    inspect_var_collection_uses("discrete_real", dae.variables.discrete_reals.values(), name);
    inspect_var_collection_uses(
        "discrete_valued",
        dae.variables.discrete_valued.values(),
        name,
    );

    for (idx, eq) in dae.continuous.equations.iter().enumerate() {
        let rhs = format!("{:?}", eq.rhs);
        if eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == name) {
            println!("  f_x[{idx}] lhs defines {name}: {rhs}");
        }
        if rhs.contains(name) {
            println!("  f_x[{idx}] rhs uses {name}: {rhs}");
        }
    }
    for (idx, eq) in dae.discrete.real_updates.iter().enumerate() {
        let rhs = format!("{:?}", eq.rhs);
        if eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == name) {
            println!("  f_z[{idx}] lhs defines {name}: {rhs}");
        }
        if rhs.contains(name) {
            println!("  f_z[{idx}] rhs uses {name}: {rhs}");
        }
    }
    for (idx, eq) in dae.discrete.valued_updates.iter().enumerate() {
        let rhs = format!("{:?}", eq.rhs);
        if eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == name) {
            println!("  f_m[{idx}] lhs defines {name}: {rhs}");
        }
        if rhs.contains(name) {
            println!("  f_m[{idx}] rhs uses {name}: {rhs}");
        }
    }
}

fn inspect_layout_binding(dae: &Dae, name: &str) -> Result<()> {
    if let Some(slot) = compiled_layout_binding_debug(dae, name)? {
        println!("  compiled_layout binding {name}: {slot:?}");
    }
    for (binding_name, slot) in compiled_layout_related_bindings_debug(dae, name)? {
        println!("  compiled_layout related {binding_name}: {slot:?}");
    }
    Ok(())
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

fn inspect_sim_result(sim: &SimResult, names: &[String], requested_times: &[f64]) {
    if names.is_empty() || sim.times.is_empty() {
        return;
    }

    let sample_indices = simulation_inspection_indices(&sim.times, requested_times);
    for name in names {
        let Some(series_idx) = sim.names.iter().position(|candidate| candidate == name) else {
            println!("Inspect sim name: {name} not found");
            continue;
        };
        println!("Inspect sim name: {name}");
        for sample_idx in &sample_indices {
            if let Some(value) = sim
                .data
                .get(series_idx)
                .and_then(|series| series.get(*sample_idx))
            {
                println!("  t={:.9} value={:.12}", sim.times[*sample_idx], value);
            }
        }
    }
}

fn simulation_inspection_indices(times: &[f64], requested_times: &[f64]) -> Vec<usize> {
    if times.is_empty() {
        return Vec::new();
    }
    if requested_times.is_empty() {
        return vec![times.len() - 1];
    }
    requested_times
        .iter()
        .filter_map(|requested| nearest_time_index(times, *requested))
        .fold(Vec::new(), |mut indices, idx| {
            if !indices.contains(&idx) {
                indices.push(idx);
            }
            indices
        })
}

fn nearest_time_index(times: &[f64], requested: f64) -> Option<usize> {
    if !requested.is_finite() {
        return None;
    }
    times
        .iter()
        .enumerate()
        .filter(|(_, time)| time.is_finite())
        .min_by(|(_, lhs), (_, rhs)| {
            let lhs_dist = (*lhs - requested).abs();
            let rhs_dist = (*rhs - requested).abs();
            lhs_dist.total_cmp(&rhs_dist)
        })
        .map(|(idx, _)| idx)
}

fn main() -> Result<()> {
    let args = Args::parse();
    let result = load_profiled_model(
        &args.source_root,
        &args.model,
        args.allow_unbalanced,
        args.artifact_dir.as_deref(),
    )?;

    if !args.inspect_names.is_empty() {
        inspect_dae_names(&result.dae, &args.inspect_names)?;
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
    inspect_sim_result(
        &sim_result,
        &args.inspect_sim_names,
        &args.inspect_sim_times,
    );
    Ok(())
}

fn debug_log_balance_summary(dae: &Dae) {
    let mut by_origin = BTreeMap::<String, (usize, usize)>::new();
    let mut by_lhs = BTreeMap::<String, (usize, usize)>::new();
    for eq in &dae.continuous.equations {
        let origin_entry = by_origin.entry(eq.origin.clone()).or_default();
        origin_entry.0 += 1;
        origin_entry.1 += eq.scalar_count;

        let lhs = eq
            .lhs
            .as_ref()
            .map(|name| name.as_str().to_string())
            .unwrap_or_else(|| "<none>".to_string());
        let lhs_entry = by_lhs.entry(lhs).or_default();
        lhs_entry.0 += 1;
        lhs_entry.1 += eq.scalar_count;
    }

    println!("Continuous equation origins by scalar count:");
    for (origin, rows, scalars) in sorted_balance_counts(by_origin).into_iter().take(24) {
        println!("  scalars={scalars:>4} rows={rows:>4} origin={origin}");
    }

    let repeated_lhs = sorted_balance_counts(by_lhs)
        .into_iter()
        .filter(|(_, rows, _)| *rows > 1)
        .take(24)
        .collect::<Vec<_>>();
    if repeated_lhs.is_empty() {
        return;
    }
    println!("Repeated continuous equation lhs by scalar count:");
    for (lhs, rows, scalars) in repeated_lhs {
        println!("  scalars={scalars:>4} rows={rows:>4} lhs={lhs}");
    }
}

fn sorted_balance_counts(counts: BTreeMap<String, (usize, usize)>) -> Vec<(String, usize, usize)> {
    let mut entries = counts
        .into_iter()
        .map(|(name, (rows, scalars))| (name, rows, scalars))
        .collect::<Vec<_>>();
    entries.sort_by(|lhs, rhs| {
        rhs.2
            .cmp(&lhs.2)
            .then_with(|| rhs.1.cmp(&lhs.1))
            .then_with(|| lhs.0.cmp(&rhs.0))
    });
    entries
}

fn debug_log_unknown_summary(dae: &Dae) {
    let mut counts = BTreeMap::<String, usize>::new();
    for variable in dae
        .variables
        .states
        .values()
        .chain(dae.variables.algebraics.values())
        .chain(dae.variables.outputs.values())
    {
        let prefix = first_rendered_path_segment(variable.name.as_str())
            .unwrap_or_else(|| "<root>".to_string());
        *counts.entry(prefix).or_default() += variable.size();
    }

    let mut entries = counts.into_iter().collect::<Vec<_>>();
    entries.sort_by(|lhs, rhs| rhs.1.cmp(&lhs.1).then_with(|| lhs.0.cmp(&rhs.0)));
    println!("Continuous unknowns by top-level component:");
    for (prefix, scalars) in entries.into_iter().take(24) {
        println!("  scalars={scalars:>4} component={prefix}");
    }
}

fn first_rendered_path_segment(path: &str) -> Option<String> {
    rumoca_core::split_path_with_indices(path)
        .into_iter()
        .next()
        .map(str::to_string)
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
        let result =
            load_profiled_model(&source_root, "Lib.M", false, None).expect("focused compile");
        assert_eq!(result.dae.variables.states.len(), 1);
        assert_eq!(result.experiment_stop_time, Some(1.5));
    }

    #[test]
    fn build_sim_options_uses_experiment_metadata() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result =
            load_profiled_model(&source_root, "Lib.M", false, None).expect("focused compile");
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
        let result =
            load_profiled_model(&source_root, "Lib.M", false, None).expect("focused compile");
        let options = build_sim_options(&result, Some(2.0));
        assert_eq!(options.t_end, 2.0);
    }

    #[test]
    fn run_profiled_simulations_repeats_simulation_path() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result =
            load_profiled_model(&source_root, "Lib.M", false, None).expect("focused compile");
        let options = build_sim_options(&result, None);
        let (elapsed, sim_result) =
            run_profiled_simulations(&result, &options, 2).expect("repeat simulate succeeds");
        assert_eq!(elapsed.len(), 2);
        assert!(elapsed.iter().all(|duration| duration.as_nanos() > 0));
        assert!(!sim_result.times.is_empty());
    }

    #[test]
    fn simulation_inspection_indices_use_final_sample_by_default() {
        assert_eq!(
            simulation_inspection_indices(&[0.0, 0.5, 1.0], &[]),
            vec![2]
        );
    }

    #[test]
    fn simulation_inspection_indices_pick_nearest_unique_samples() {
        assert_eq!(
            simulation_inspection_indices(&[0.0, 0.5, 1.0], &[0.49, 0.51, 0.9]),
            vec![1, 2]
        );
    }
}
