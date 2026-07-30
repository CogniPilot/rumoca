use std::path::PathBuf;
use std::time::Instant;

use anyhow::{Context, Result, bail};
use clap::{Parser, ValueEnum};
use rumoca_compile::compile::{
    CompilationResult, ContinuousOwnerView, CoordinateView, Dae, ExpressionOperation, PhaseResult,
    Session, SessionConfig, SourceRootKind, StrictCompileReport, VariableId, VariableRole,
    VariableView, compile_phase_timing_stats, core as rumoca_core,
    reset_compile_phase_timing_stats,
};
use rumoca_compile::source_roots::parse_source_root_with_cache;
use rumoca_sim::simulate_dae;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode};
use rumoca_sim::{compiled_layout_binding_debug, compiled_layout_related_bindings_debug};

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

    /// Directory for focused JSON artifacts.
    #[arg(long)]
    artifact_dir: Option<PathBuf>,
}

fn compile_report_to_result(report: StrictCompileReport) -> Result<Box<CompilationResult>> {
    if !report.requested_succeeded() {
        print_strict_failures(&report);
    }
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

fn print_strict_failures(report: &StrictCompileReport) {
    for failure in &report.failures {
        let location = failure
            .primary_label
            .as_ref()
            .and_then(|label| report.source_map.as_ref().map(|map| (map, label.span)))
            .and_then(|(map, span)| {
                let (name, source) = map.get_source(span.source)?;
                let offset = span.start.0.min(source.len());
                let prefix = &source[..offset];
                let line = prefix.bytes().filter(|byte| *byte == b'\n').count() + 1;
                let column = prefix
                    .rsplit_once('\n')
                    .map_or(prefix.len(), |(_, tail)| tail.len())
                    + 1;
                Some(format!("{name}:{line}:{column}"))
            })
            .unwrap_or_else(|| "<unknown>".to_string());
        eprintln!(
            "{location}: {}: {}",
            failure.error_code.as_deref().unwrap_or("error"),
            failure.error
        );
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

fn print_structural_summary(dae: &Dae) {
    let structural = dae.inspect(rumoca_compile::phase_structural::analyze);
    println!(
        "Structural matching: matched={} equations={} unknowns={}",
        structural.matching_size, structural.n_equations, structural.n_unknowns
    );
    const LIMIT: usize = 128;
    print_bounded_names(
        "Structurally unmatched unknowns:",
        &structural.unmatched_unknowns,
        LIMIT,
    );
    print_bounded_names(
        "Structurally unmatched equations:",
        &structural.unmatched_equations,
        LIMIT,
    );
}

fn print_bounded_names(heading: &str, names: &[String], limit: usize) {
    if names.is_empty() {
        return;
    }
    println!("{heading}");
    for name in names.iter().take(limit) {
        println!("  {name}");
    }
    if names.len() > limit {
        println!("  ... {} more", names.len() - limit);
    }
}

fn load_profiled_model(
    source_root: &std::path::Path,
    model: &str,
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
    let report = session.compile_model_strict_reachable_uncached_with_recovery(model);
    let result = compile_report_to_result(report)?;
    let compile_elapsed = compile_started.elapsed();

    println!(
        "Focused compile elapsed: {:.2?} for {}",
        compile_elapsed, model
    );
    print_compile_phase_snapshot();
    let (states, algebraics, equations) = result.dae.inspect(|view| {
        (
            view.variables()
                .filter(|(_, variable)| variable.role() == VariableRole::State)
                .count(),
            view.variables()
                .filter(|(_, variable)| variable.role() == VariableRole::Algebraic)
                .count(),
            view.continuous_owner_count(),
        )
    });
    println!(
        "Compilation successful: states={states} algebraics={algebraics} equations={equations}"
    );
    println!("Balance detail: {:#?}", result.balance_detail);
    println!("Balance result: {}", result.balance_detail.balance());
    if let Some(artifact_dir) = artifact_dir {
        write_artifacts(artifact_dir, &result)?;
    }
    print_structural_summary(&result.dae);
    debug_log_balance_summary(&result.dae);
    debug_log_unknown_summary(&result.dae);
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
        println!("Inspect name: {name}");
        dae.inspect(|view| {
            let matching = view
                .variables()
                .filter(|(_, variable)| variable.name().as_str() == name)
                .collect::<Vec<_>>();
            print_named_variables(view, &matching);
            print_named_coordinate_uses(view, name, &matching);
            print_named_functions(view, name);
        });
        inspect_layout_binding(dae, name)?;
    }
    Ok(())
}

fn print_named_variables<'dae>(
    view: rumoca_compile::compile::DaeView<'dae>,
    matching: &[(VariableId<'dae>, VariableView<'dae>)],
) {
    for (id, variable) in matching {
        println!(
            "  {:?}: {} id={} scalars={} dims={:?} has_start={}",
            variable.role(),
            variable.name(),
            id.index(),
            variable.scalar_count(),
            variable.value_type().dimensions(),
            variable.start().is_some(),
        );
        print_variable_start(view, *variable);
    }
}

fn print_variable_start<'dae>(
    view: rumoca_compile::compile::DaeView<'dae>,
    variable: VariableView<'dae>,
) {
    let Some(start) = variable.start() else {
        return;
    };
    let Some(expression) = view.expression(start) else {
        return;
    };
    println!(
        "    start={}",
        view.source_text(expression.provenance())
            .unwrap_or("<generated expression>")
    );
}

fn print_named_coordinate_uses<'dae>(
    view: rumoca_compile::compile::DaeView<'dae>,
    name: &str,
    matching: &[(VariableId<'dae>, VariableView<'dae>)],
) {
    for (target, _) in matching {
        print_coordinate_uses(view, name, target.index());
    }
}

fn print_coordinate_uses(view: rumoca_compile::compile::DaeView<'_>, name: &str, target: u32) {
    for index in 0..view.expression_count() {
        let expression_id = view
            .expression_id(index)
            .expect("finalized expression has an identity");
        let expression = view
            .expression(expression_id)
            .expect("branded expression resolves");
        if coordinate_variable_index(expression.operation()) == Some(target) {
            println!(
                "  expression[{index}] uses {name}: {}",
                view.source_text(expression.provenance())
                    .unwrap_or("<generated expression>")
            );
        }
    }
}

fn print_named_functions(view: rumoca_compile::compile::DaeView<'_>, name: &str) {
    for index in 0..view.function_count() {
        let function = view
            .function_id(index)
            .and_then(|id| view.function(id))
            .expect("finalized function resolves");
        if function.name().as_str().contains(name) {
            println!(
                "  function match: {} (inputs={}, outputs={})",
                function.name(),
                function.parameter_types().len(),
                function.result_types().len(),
            );
        }
    }
}

fn coordinate_variable_index(operation: ExpressionOperation<'_>) -> Option<u32> {
    match operation {
        ExpressionOperation::Coordinate(CoordinateView::Parameter(id)) => Some(id.index()),
        ExpressionOperation::Coordinate(CoordinateView::Input(id)) => Some(id.index()),
        ExpressionOperation::Coordinate(CoordinateView::State(id))
        | ExpressionOperation::Coordinate(CoordinateView::Derivative(id)) => Some(id.index()),
        ExpressionOperation::Coordinate(CoordinateView::Algebraic(id)) => Some(id.index()),
        ExpressionOperation::Coordinate(CoordinateView::DiscreteReal(id))
        | ExpressionOperation::Coordinate(CoordinateView::PreDiscreteReal(id)) => Some(id.index()),
        ExpressionOperation::Coordinate(CoordinateView::DiscreteValue(id))
        | ExpressionOperation::Coordinate(CoordinateView::PreDiscreteValue(id)) => Some(id.index()),
        ExpressionOperation::Coordinate(
            CoordinateView::Time
            | CoordinateView::ClockInterval(_)
            | CoordinateView::Condition(_)
            | CoordinateView::Delay(_)
            | CoordinateView::Previous(_)
            | CoordinateView::Terminal(_)
            | CoordinateView::Binder(_),
        )
        | ExpressionOperation::Literal(_)
        | ExpressionOperation::Unary { .. }
        | ExpressionOperation::Binary { .. }
        | ExpressionOperation::Conditional(_)
        | ExpressionOperation::Array(_)
        | ExpressionOperation::Record(_)
        | ExpressionOperation::Field { .. }
        | ExpressionOperation::ArrayUpdate { .. }
        | ExpressionOperation::Range(_)
        | ExpressionOperation::Comprehension { .. }
        | ExpressionOperation::Index { .. }
        | ExpressionOperation::Builtin { .. }
        | ExpressionOperation::Call { .. }
        | ExpressionOperation::StringConversion { .. }
        | ExpressionOperation::FunctionValue { .. }
        | ExpressionOperation::FunctionFoldParameter { .. }
        | ExpressionOperation::FunctionFoldOutput { .. }
        | ExpressionOperation::Coordinate(CoordinateView::FunctionParameter(_)) => None,
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
    let result = load_profiled_model(&args.source_root, &args.model, args.artifact_dir.as_deref())?;

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
    dae.inspect(|view| {
        println!("Continuous semantic owners:");
        for (index, owner) in view.continuous_owners().take(24).enumerate() {
            let (scalars, provenance) = match owner {
                ContinuousOwnerView::Residual { equation, .. } => (
                    view.expression(equation.residual())
                        .expect("branded residual resolves")
                        .value_type()
                        .scalar_count()
                        .expect("checked residual has scalar capacity"),
                    equation.provenance(),
                ),
                ContinuousOwnerView::Structured { family, .. } => {
                    (family.scalar_rows() as usize, family.provenance())
                }
            };
            println!(
                "  owner={index:>4} scalars={scalars:>4} source={}",
                view.source_text(provenance).unwrap_or("<generated owner>")
            );
        }
    });
}

fn debug_log_unknown_summary(dae: &Dae) {
    dae.inspect(|view| {
        let mut counts = std::collections::BTreeMap::<String, usize>::new();
        for (_, variable) in view.variables().filter(|(_, variable)| {
            matches!(
                variable.role(),
                VariableRole::State | VariableRole::Algebraic | VariableRole::Output
            )
        }) {
            let prefix = first_rendered_path_segment(variable.name().as_str())
                .unwrap_or_else(|| "<root>".to_string());
            *counts.entry(prefix).or_default() += variable.scalar_count();
        }
        let mut entries = counts.into_iter().collect::<Vec<_>>();
        entries.sort_by(|lhs, rhs| rhs.1.cmp(&lhs.1).then_with(|| lhs.0.cmp(&rhs.0)));
        println!("Continuous unknowns by top-level component:");
        for (prefix, scalars) in entries.into_iter().take(24) {
            println!("  scalars={scalars:>4} component={prefix}");
        }
    });
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
        let result = load_profiled_model(&source_root, "Lib.M", None).expect("focused compile");
        assert_eq!(
            result.dae.inspect(|view| {
                view.variables()
                    .filter(|(_, variable)| variable.role() == VariableRole::State)
                    .count()
            }),
            1
        );
        assert_eq!(result.experiment_stop_time, Some(1.5));
    }

    #[test]
    fn build_sim_options_uses_experiment_metadata() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result = load_profiled_model(&source_root, "Lib.M", None).expect("focused compile");
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
        let result = load_profiled_model(&source_root, "Lib.M", None).expect("focused compile");
        let options = build_sim_options(&result, Some(2.0));
        assert_eq!(options.t_end, 2.0);
    }

    #[test]
    fn run_profiled_simulations_repeats_simulation_path() {
        let temp = tempfile::tempdir().expect("tempdir");
        let source_root = write_library(&temp);
        let result = load_profiled_model(&source_root, "Lib.M", None).expect("focused compile");
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
