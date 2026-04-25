use crate::common::{
    MslPaths, has_fatal_omc_error, msl_load_lines, run_command_with_timeout, summarize_omc_error,
    write_pretty_json,
};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use rumoca_compile::compile::{
    CompilationResult as SessionCompilationResult, Session, SessionConfig,
};
use rumoca_compile::parsing::parse_files_parallel_lenient;
use rumoca_sim::runtime_defined_unknown_names;
use rumoca_sim::sim_trace_compare::{
    ModelDeviationMetric, SimTrace, SimTraceVariableMeta, compare_model_traces, load_trace_json,
};
use rumoca_sim::{SimOptions, SimResult, SimSolverMode};
use rumoca_solver_diffsol::simulate_dae;
use rumoca_viz_web::{UPLOT_CSS, UPLOT_JS};
use serde::Serialize;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

const GRID_DEDUP_EPS: f64 = 1.0e-12;
const SIM_TIMEOUT_SECONDS: f64 = 10.0;
const OMC_SIM_TIMEOUT_SECONDS: u64 = 10;
#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct Args {
    /// Fully qualified model name
    #[arg(long)]
    model: String,
    /// Optional explicit rumoca trace JSON path
    #[arg(long)]
    rumoca_trace: Option<PathBuf>,
    /// Optional explicit OMC trace JSON path
    #[arg(long)]
    omc_trace: Option<PathBuf>,
    /// Optional output HTML path
    #[arg(long)]
    output: Option<PathBuf>,
}

#[derive(Debug, Clone, Serialize)]
struct PlotPayload {
    overlap_start: f64,
    overlap_end: f64,
    times: Vec<f64>,
    variables: Vec<String>,
    omc_data: Vec<Vec<Option<f64>>>,
    rumoca_data: Vec<Vec<Option<f64>>>,
    model_metric: Option<ModelMetricSummary>,
}

#[derive(Debug, Clone, Serialize)]
struct ModelMetricSummary {
    bounded_normalized_l1_score: f64,
    mean_channel_bounded_normalized_l1: f64,
    compared_variables: usize,
    samples_compared: usize,
}

#[derive(Debug, Clone)]
struct AlignedTraceData {
    overlap_start: f64,
    overlap_end: f64,
    times: Vec<f64>,
    variables: Vec<String>,
    omc_data: Vec<Vec<Option<f64>>>,
    rumoca_data: Vec<Vec<Option<f64>>>,
}

pub(crate) fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let rumoca_trace_path = resolve_trace_path(&paths, &args, TraceSource::Rumoca);
    let omc_trace_path = resolve_trace_path(&paths, &args, TraceSource::Omc);
    regenerate_traces_for_model(&paths, &args.model, &rumoca_trace_path, &omc_trace_path)?;

    let rumoca_trace = load_trace_json(&rumoca_trace_path).with_context(|| {
        format!(
            "failed to load rumoca trace '{}'",
            rumoca_trace_path.display()
        )
    })?;
    let omc_trace = load_trace_json(&omc_trace_path)
        .with_context(|| format!("failed to load OMC trace '{}'", omc_trace_path.display()))?;
    let aligned = align_traces(&args.model, &rumoca_trace, &omc_trace)?;
    let metric = match compare_model_traces(&args.model, &rumoca_trace, &omc_trace) {
        Ok(metric) => Some(metric),
        Err(error) => {
            if std::env::var("RUMOCA_PLOT_COMPARE_DEBUG").is_ok() {
                eprintln!("[plot-compare-debug] compare_model_traces error: {error}");
            }
            None
        }
    };
    let payload = build_plot_payload(aligned, metric);
    let html = generate_html(&args.model, &payload)?;
    let output_path = resolve_output_path(&paths, &args);
    write_output_html(&output_path, &html)?;

    println!("Model: {}", args.model);
    println!("Rumoca trace: {}", rumoca_trace_path.display());
    println!("OMC trace: {}", omc_trace_path.display());
    println!("Compared variables: {}", payload.variables.len());
    println!(
        "Overlap: [{:.6}, {:.6}] ({} points)",
        payload.overlap_start,
        payload.overlap_end,
        payload.times.len()
    );
    if let Some(metric) = payload.model_metric.as_ref() {
        println!(
            "Bounded normalized L1 score: {:.3e} (mean channel score {:.3e})",
            metric.bounded_normalized_l1_score, metric.mean_channel_bounded_normalized_l1
        );
        if let Ok(full_metric) = compare_model_traces(&args.model, &rumoca_trace, &omc_trace) {
            for channel in full_metric.worst_variables.iter().take(5) {
                println!(
                    "  worst: {} (bounded L1 {:.3e}, normalized L1 {:.3e})",
                    channel.name, channel.bounded_normalized_l1_error, channel.normalized_l1_error
                );
            }
        }
    }
    println!("Wrote plot: {}", output_path.display());
    Ok(())
}

fn regenerate_traces_for_model(
    paths: &MslPaths,
    model_name: &str,
    rumoca_trace_path: &Path,
    omc_trace_path: &Path,
) -> Result<()> {
    println!("Regenerating traces for model '{model_name}'...");
    ensure_msl_cache_exists(paths)?;
    generate_rumoca_trace(paths, model_name, rumoca_trace_path)?;
    generate_omc_trace(paths, model_name, omc_trace_path)?;
    Ok(())
}

fn ensure_msl_cache_exists(paths: &MslPaths) -> Result<()> {
    if paths.msl_dir.is_dir() {
        return Ok(());
    }
    bail!(
        "MSL cache directory not found at '{}'. Run an MSL test first to populate cache.",
        paths.msl_dir.display()
    );
}

fn generate_rumoca_trace(paths: &MslPaths, model_name: &str, output_path: &Path) -> Result<()> {
    println!("  Rumoca: compiling and simulating...");
    let mut session = load_msl_session(paths)?;
    let compiled = session
        .compile_model(model_name)
        .with_context(|| format!("rumoca compile failed for '{model_name}'"))?;
    maybe_debug_dump_compiled_model(&compiled, model_name);
    let options = sim_options_from_compilation(&compiled);
    let sim = simulate_dae(&compiled.dae, &options)
        .with_context(|| format!("rumoca simulate failed for '{model_name}'"))?;
    let trace = trace_from_sim_result(model_name, &sim);
    write_pretty_json(output_path, &trace).with_context(|| {
        format!(
            "failed writing rumoca trace for '{model_name}' to '{}'",
            output_path.display()
        )
    })?;
    println!("  Rumoca: wrote {}", output_path.display());
    Ok(())
}

fn maybe_debug_dump_compiled_model(compiled: &SessionCompilationResult, model_name: &str) {
    if std::env::var("RUMOCA_PLOT_COMPARE_DEBUG").is_err() {
        return;
    }
    debug_log_compile_summary(compiled, model_name);
    debug_log_runtime_defined(compiled);
    debug_log_enum_ordinals(compiled);
    debug_log_selected_starts(compiled);
    debug_log_named_variables(compiled);
    debug_log_named_equations(compiled);
    debug_log_counter_equations(compiled);
}

fn debug_targets_from_env(var: &str) -> Vec<String> {
    std::env::var(var)
        .ok()
        .map(|raw| {
            raw.split(',')
                .map(str::trim)
                .filter(|s| !s.is_empty())
                .map(ToOwned::to_owned)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default()
}

fn debug_log_compile_summary(compiled: &SessionCompilationResult, model_name: &str) {
    eprintln!("[plot-compare-debug] model={model_name}");
    eprintln!(
        "[plot-compare-debug] discrete_reals={} discrete_valued={} f_z={} f_m={} f_c={} relation={}",
        compiled.dae.discrete_reals.len(),
        compiled.dae.discrete_valued.len(),
        compiled.dae.f_z.len(),
        compiled.dae.f_m.len(),
        compiled.dae.f_c.len(),
        compiled.dae.relation.len(),
    );
    let has_counter_q = compiled
        .dae
        .discrete_valued
        .keys()
        .any(|name| name.as_str().contains("Counter.FF3.q"));
    eprintln!("[plot-compare-debug] has Counter.FF3.q* in discrete_valued={has_counter_q}");
}

fn debug_log_runtime_defined(compiled: &SessionCompilationResult) {
    let runtime_defined = runtime_defined_unknown_names(&compiled.dae);
    for name in [
        "Counter.FF3.RS1.q",
        "Counter.FF3.RS1.qn",
        "Counter.FF3.RS1.Nor1.y",
        "Counter.FF3.q",
        "Counter.FF3.qn",
        "Counter.FF3.And2.x[1]",
        "Counter.FF3.And3.x[1]",
        "Counter.FF3.And4.x[1]",
        "Counter.FF3.Not1.auxiliary",
        "Counter.FF3.Not1.x",
    ] {
        eprintln!(
            "[plot-compare-debug] runtime_defined[{name}]={}",
            runtime_defined.contains(name)
        );
    }
}

fn debug_log_enum_ordinals(compiled: &SessionCompilationResult) {
    for lit in [
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'",
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'",
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'",
    ] {
        eprintln!(
            "[plot-compare-debug] enum_ordinal[{lit}]={:?}",
            compiled.dae.enum_literal_ordinals.get(lit)
        );
    }
}

fn debug_log_selected_starts(compiled: &SessionCompilationResult) {
    for name in [
        "Counter.FF3.Not1.auxiliary",
        "Counter.FF3.And3.auxiliary[1]",
        "Counter.FF3.RS1.Nor1.auxiliary[1]",
        "Counter.FF3.RS1.Nor1.auxiliary_n",
    ] {
        let var = compiled
            .dae
            .states
            .iter()
            .find(|(k, _)| k.as_str() == name)
            .map(|(_, v)| v)
            .or_else(|| {
                compiled
                    .dae
                    .algebraics
                    .iter()
                    .find(|(k, _)| k.as_str() == name)
                    .map(|(_, v)| v)
            })
            .or_else(|| {
                compiled
                    .dae
                    .outputs
                    .iter()
                    .find(|(k, _)| k.as_str() == name)
                    .map(|(_, v)| v)
            })
            .or_else(|| {
                compiled
                    .dae
                    .discrete_reals
                    .iter()
                    .find(|(k, _)| k.as_str() == name)
                    .map(|(_, v)| v)
            })
            .or_else(|| {
                compiled
                    .dae
                    .discrete_valued
                    .iter()
                    .find(|(k, _)| k.as_str() == name)
                    .map(|(_, v)| v)
            });
        eprintln!(
            "[plot-compare-debug] start[{name}]={:?}",
            var.and_then(|v| v.start.as_ref())
        );
    }
}

fn debug_log_named_variables(compiled: &SessionCompilationResult) {
    let targets = debug_targets_from_env("RUMOCA_PLOT_COMPARE_DEBUG_VARS");
    if targets.is_empty() {
        return;
    }

    for target in targets {
        let mut found = false;
        for (bucket, vars) in [
            ("state", &compiled.dae.states),
            ("algebraic", &compiled.dae.algebraics),
            ("output", &compiled.dae.outputs),
            ("input", &compiled.dae.inputs),
            ("discrete_real", &compiled.dae.discrete_reals),
            ("discrete_valued", &compiled.dae.discrete_valued),
            ("parameter", &compiled.dae.parameters),
            ("constant", &compiled.dae.constants),
        ] {
            if let Some((_, var)) = vars.iter().find(|(name, _)| name.as_str() == target) {
                eprintln!(
                    "[plot-compare-debug] var[{target}] bucket={bucket} start={:?} fixed={:?} dims={:?}",
                    var.start, var.fixed, var.dims
                );
                found = true;
            }
        }
        if !found {
            eprintln!("[plot-compare-debug] var[{target}] <not found>");
        }
    }
}

fn debug_log_named_equations(compiled: &SessionCompilationResult) {
    let patterns = debug_targets_from_env("RUMOCA_PLOT_COMPARE_DEBUG_EQ_PATTERNS");
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
            let hit = patterns
                .iter()
                .any(|p| lhs.contains(p) || rhs_dbg.contains(p) || origin.contains(p));
            if !hit {
                continue;
            }
            eprintln!(
                "[plot-compare-debug] {bucket}[{idx}] lhs='{lhs}' origin='{}' rhs={rhs_dbg}",
                eq.origin
            );
        }
    }
}

fn debug_log_counter_equations(compiled: &SessionCompilationResult) {
    for (bucket, equations) in [
        ("f_x", &compiled.dae.f_x),
        ("f_z", &compiled.dae.f_z),
        ("f_m", &compiled.dae.f_m),
    ] {
        for (idx, eq) in equations.iter().enumerate() {
            let rhs_dbg = format!("{:?}", eq.rhs);
            let matches_counter = rhs_dbg.contains("Counter.FF3.Not1.y")
                || rhs_dbg.contains("Counter.FF3.Not1.auxiliary");
            if !matches_counter {
                continue;
            }
            eprintln!(
                "[plot-compare-debug] {bucket}[{idx}] origin='{}' rhs={rhs_dbg}",
                eq.origin
            );
        }
    }
}

fn load_msl_session(paths: &MslPaths) -> Result<Session> {
    let mo_files = find_mo_files(&paths.msl_dir);
    if mo_files.is_empty() {
        bail!("no .mo files found under '{}'", paths.msl_dir.display());
    }
    let (successes, failures) = parse_files_parallel_lenient(&mo_files);
    if successes.is_empty() {
        bail!(
            "failed to parse all MSL files ({} parse failures)",
            failures.len()
        );
    }
    let mut session = Session::new(SessionConfig { parallel: true });
    session.add_parsed_batch(successes);
    Ok(session)
}

fn find_mo_files(msl_dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_mo_files_recursive(msl_dir, &mut files);
    files
}

fn collect_mo_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.filter_map(Result::ok) {
        let path = entry.path();
        if path.is_dir() {
            collect_mo_files_recursive(&path, out);
            continue;
        }
        if path.extension().is_none_or(|ext| ext != "mo") {
            continue;
        }
        let path_text = path.to_string_lossy();
        if path_text.contains("Obsolete") || path_text.contains("ModelicaTestConversion") {
            continue;
        }
        out.push(path);
    }
}

fn sim_options_from_compilation(compiled: &SessionCompilationResult) -> SimOptions {
    let mut options = SimOptions::default();
    options.t_start = compiled.experiment_start_time.unwrap_or(options.t_start);
    options.t_end = compiled.experiment_stop_time.unwrap_or(options.t_end);
    if options.t_end <= options.t_start {
        options.t_end = options.t_start + 1.0;
    }
    if let Some(tolerance) = compiled.experiment_tolerance.filter(|value| *value > 0.0) {
        options.rtol = tolerance;
        options.atol = tolerance;
    }
    options.dt = compiled
        .experiment_interval
        .filter(|value| value.is_finite() && *value > 0.0);
    let solver_override = std::env::var("RUMOCA_MSL_SIM_SOLVER")
        .ok()
        .map(|raw| raw.trim().to_string())
        .filter(|value| !value.is_empty());
    if let Some(solver) = solver_override
        .as_deref()
        .or(compiled.experiment_solver.as_deref())
    {
        options.solver_mode = SimSolverMode::from_external_name(solver);
    }
    options.max_wall_seconds = Some(SIM_TIMEOUT_SECONDS);
    options
}

fn trace_from_sim_result(model_name: &str, sim: &SimResult) -> SimTrace {
    let data = sim
        .data
        .iter()
        .map(|column| column.iter().copied().map(Some).collect())
        .collect();
    let variable_meta = if sim.variable_meta.is_empty() {
        None
    } else {
        Some(
            sim.variable_meta
                .iter()
                .map(|meta| SimTraceVariableMeta {
                    name: meta.name.clone(),
                    role: Some(meta.role.clone()),
                    value_type: meta.value_type.clone(),
                    variability: meta.variability.clone(),
                    time_domain: meta.time_domain.clone(),
                })
                .collect(),
        )
    };
    SimTrace {
        model_name: Some(model_name.to_string()),
        times: sim.times.clone(),
        names: sim.names.clone(),
        data,
        variable_meta,
    }
}

fn generate_omc_trace(paths: &MslPaths, model_name: &str, output_path: &Path) -> Result<()> {
    println!("  OMC: simulating...");
    let work_dir = paths.results_dir.join("plot_compare_omc_work");
    std::fs::create_dir_all(&work_dir)
        .with_context(|| format!("failed to create '{}'", work_dir.display()))?;
    let check_file = work_dir.join("plot_compare_omc_check.txt");
    let mos_file = work_dir.join("plot_compare_omc.mos");
    let script = build_omc_script(paths, model_name, &check_file);
    std::fs::write(&mos_file, script)
        .with_context(|| format!("failed to write '{}'", mos_file.display()))?;

    let mut command = Command::new("omc");
    command.arg(&mos_file).current_dir(&work_dir);
    let run = run_command_with_timeout(&mut command, Duration::from_secs(OMC_SIM_TIMEOUT_SECONDS))
        .with_context(|| "failed running OMC for plot-compare")?;
    if run.timed_out {
        bail!(
            "OMC simulation timed out after {}s for '{}'",
            OMC_SIM_TIMEOUT_SECONDS,
            model_name
        );
    }

    let omc_error = std::fs::read_to_string(&check_file).unwrap_or_default();
    if has_fatal_omc_error(&omc_error) {
        bail!(
            "OMC simulation failed for '{}': {}",
            model_name,
            summarize_omc_error(&omc_error, &(run.stdout + &run.stderr))
        );
    }

    let csv_path = resolve_omc_csv_path(&work_dir, model_name, &(run.stdout + &run.stderr));
    let trace = load_omc_csv_as_trace(model_name, &csv_path)?;
    write_pretty_json(output_path, &trace).with_context(|| {
        format!(
            "failed writing OMC trace for '{model_name}' to '{}'",
            output_path.display()
        )
    })?;
    println!("  OMC: wrote {}", output_path.display());
    Ok(())
}

fn build_omc_script(paths: &MslPaths, model_name: &str, check_file: &Path) -> String {
    let mut lines = msl_load_lines(paths);
    lines.push("getErrorString();".to_string());
    lines.push(format!(
        "simRes := simulate({model_name}, outputFormat=\"csv\", fileNamePrefix=\"{model_name}\");"
    ));
    lines.push("err := getErrorString();".to_string());
    lines.push(format!(
        "writeFile(\"{}\", \"ERROR:\" + err + \"\\n\");",
        check_file.display()
    ));
    lines.join("\n")
}

fn resolve_omc_csv_path(work_dir: &Path, model_name: &str, output: &str) -> PathBuf {
    if let Some(result_file) = parse_omc_result_file(output) {
        let path = PathBuf::from(result_file);
        if path.is_absolute() {
            return path;
        }
        return work_dir.join(path);
    }
    work_dir.join(format!("{model_name}_res.csv"))
}

fn parse_omc_result_file(output: &str) -> Option<String> {
    let marker = "resultFile =";
    let start = output.find(marker)?;
    let tail = output[start + marker.len()..].trim_start();
    let first_quote = tail.find('"')?;
    let rest = &tail[first_quote + 1..];
    let end_quote = rest.find('"')?;
    Some(rest[..end_quote].to_string())
}

fn load_omc_csv_as_trace(model_name: &str, csv_path: &Path) -> Result<SimTrace> {
    if !csv_path.is_file() {
        bail!("OMC result CSV not found: '{}'", csv_path.display());
    }
    let content = std::fs::read_to_string(csv_path)
        .with_context(|| format!("failed to read '{}'", csv_path.display()))?;
    let mut lines = content.lines();
    let Some(header) = lines.next() else {
        bail!("OMC CSV '{}' has empty header", csv_path.display());
    };

    let headers = parse_csv_row(header);
    let Some(time_index) = headers
        .iter()
        .position(|name| name.eq_ignore_ascii_case("time"))
    else {
        bail!("OMC CSV '{}' missing time column", csv_path.display());
    };
    let value_indices = headers
        .iter()
        .enumerate()
        .filter_map(|(index, _)| (index != time_index).then_some(index))
        .collect::<Vec<_>>();
    let names = value_indices
        .iter()
        .map(|index| headers[*index].clone())
        .collect::<Vec<_>>();

    let mut times = Vec::new();
    let mut data = vec![Vec::new(); value_indices.len()];
    for line in lines {
        let row = parse_csv_row(line);
        let Some(time_value) = row.get(time_index).and_then(|raw| raw.parse::<f64>().ok()) else {
            continue;
        };
        if !time_value.is_finite() {
            continue;
        }
        times.push(time_value);
        for (column_index, source_index) in value_indices.iter().enumerate() {
            let value = row
                .get(*source_index)
                .and_then(|raw| raw.parse::<f64>().ok())
                .filter(|v| v.is_finite());
            data[column_index].push(value);
        }
    }
    if times.is_empty() {
        bail!(
            "OMC CSV '{}' contains no numeric time rows",
            csv_path.display()
        );
    }

    Ok(SimTrace {
        model_name: Some(model_name.to_string()),
        times,
        names,
        data,
        variable_meta: None,
    })
}

fn parse_csv_row(row: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut in_quotes = false;
    let mut chars = row.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '"' {
            if in_quotes && chars.peek() == Some(&'"') {
                current.push('"');
                let _ = chars.next();
            } else {
                in_quotes = !in_quotes;
            }
            continue;
        }
        if ch == ',' && !in_quotes {
            fields.push(current.trim().to_string());
            current.clear();
            continue;
        }
        current.push(ch);
    }
    fields.push(current.trim().to_string());
    fields
}

fn build_plot_payload(
    aligned: AlignedTraceData,
    metric: Option<ModelDeviationMetric>,
) -> PlotPayload {
    PlotPayload {
        overlap_start: aligned.overlap_start,
        overlap_end: aligned.overlap_end,
        times: aligned.times,
        variables: aligned.variables,
        omc_data: aligned.omc_data,
        rumoca_data: aligned.rumoca_data,
        model_metric: metric.as_ref().map(model_metric_summary),
    }
}

fn model_metric_summary(metric: &ModelDeviationMetric) -> ModelMetricSummary {
    ModelMetricSummary {
        bounded_normalized_l1_score: metric.bounded_normalized_l1_score,
        mean_channel_bounded_normalized_l1: metric.mean_channel_bounded_normalized_l1,
        compared_variables: metric.compared_variables,
        samples_compared: metric.samples_compared,
    }
}

fn align_traces(model_name: &str, rumoca: &SimTrace, omc: &SimTrace) -> Result<AlignedTraceData> {
    if rumoca.times.is_empty() || omc.times.is_empty() {
        bail!("trace comparison requires non-empty time arrays");
    }
    let overlap_start = rumoca.times[0].max(omc.times[0]);
    let overlap_end = rumoca.times[rumoca.times.len() - 1].min(omc.times[omc.times.len() - 1]);
    if overlap_end <= overlap_start {
        bail!(
            "no overlapping time range for '{}': rumoca [{:.6}, {:.6}] vs omc [{:.6}, {:.6}]",
            model_name,
            rumoca.times[0],
            rumoca.times[rumoca.times.len() - 1],
            omc.times[0],
            omc.times[omc.times.len() - 1]
        );
    }

    let variables = common_variable_names(rumoca, omc);
    if variables.is_empty() {
        bail!("no common variables between rumoca and OMC traces");
    }
    let grid = build_overlap_grid(&rumoca.times, &omc.times, overlap_start, overlap_end);
    if grid.len() < 2 {
        bail!("insufficient overlap samples to plot");
    }
    let rumoca_data = resample_trace_columns(rumoca, &variables, &grid)?;
    let omc_data = resample_trace_columns(omc, &variables, &grid)?;

    Ok(AlignedTraceData {
        overlap_start,
        overlap_end,
        times: grid,
        variables,
        omc_data,
        rumoca_data,
    })
}

fn common_variable_names(rumoca: &SimTrace, omc: &SimTrace) -> Vec<String> {
    let omc_names = omc.names.iter().collect::<std::collections::HashSet<_>>();
    rumoca
        .names
        .iter()
        .filter(|name| omc_names.contains(name))
        .cloned()
        .collect()
}

fn build_overlap_grid(rumoca_times: &[f64], omc_times: &[f64], start: f64, end: f64) -> Vec<f64> {
    let mut grid = Vec::with_capacity(rumoca_times.len() + omc_times.len() + 2);
    grid.push(start);
    grid.push(end);
    grid.extend(
        rumoca_times
            .iter()
            .copied()
            .filter(|time| *time >= start && *time <= end),
    );
    grid.extend(
        omc_times
            .iter()
            .copied()
            .filter(|time| *time >= start && *time <= end),
    );
    grid.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    dedupe_time_grid(&grid)
}

fn dedupe_time_grid(grid: &[f64]) -> Vec<f64> {
    let mut deduped: Vec<f64> = Vec::with_capacity(grid.len());
    for time in grid {
        let is_duplicate = deduped
            .last()
            .is_some_and(|last| (*time - *last).abs() <= GRID_DEDUP_EPS);
        if !is_duplicate {
            deduped.push(*time);
        }
    }
    deduped
}

fn resample_trace_columns(
    trace: &SimTrace,
    variables: &[String],
    grid: &[f64],
) -> Result<Vec<Vec<Option<f64>>>> {
    let mut columns = Vec::with_capacity(variables.len());
    for variable in variables {
        let series = trace_column_by_name(trace, variable)
            .with_context(|| format!("missing variable '{variable}' in trace data"))?;
        columns.push(resample_series(&trace.times, series, grid));
    }
    Ok(columns)
}

fn trace_column_by_name<'a>(trace: &'a SimTrace, name: &str) -> Result<&'a [Option<f64>]> {
    let Some(index) = trace.names.iter().position(|var| var == name) else {
        bail!("trace does not contain '{name}'");
    };
    trace
        .data
        .get(index)
        .map(Vec::as_slice)
        .context("trace column is missing")
}

fn resample_series(times: &[f64], values: &[Option<f64>], grid: &[f64]) -> Vec<Option<f64>> {
    grid.iter()
        .map(|time| value_at_or_nearest(times, values, *time))
        .collect()
}

fn value_at_or_nearest(times: &[f64], values: &[Option<f64>], t: f64) -> Option<f64> {
    interp_linear(times, values, t).or_else(|| nearest_finite_value(times, values, t))
}

fn interp_linear(times: &[f64], values: &[Option<f64>], t: f64) -> Option<f64> {
    if times.len() < 2 || times.len() != values.len() {
        return None;
    }
    if t < times[0] || t > times[times.len() - 1] {
        return None;
    }
    match times.binary_search_by(|probe| probe.partial_cmp(&t).unwrap_or(std::cmp::Ordering::Less))
    {
        Ok(index) => values.get(index).copied().flatten(),
        Err(right) => interp_between_neighbors(times, values, t, right),
    }
}

fn interp_between_neighbors(
    times: &[f64],
    values: &[Option<f64>],
    t: f64,
    right: usize,
) -> Option<f64> {
    if right == 0 {
        return None;
    }
    let left = right - 1;
    if right >= times.len() {
        return values.last().copied().flatten();
    }
    let t0 = times[left];
    let t1 = times[right];
    let (Some(v0), Some(v1)) = (values[left], values[right]) else {
        return None;
    };
    if t1 <= t0 {
        return Some(v0);
    }
    let alpha = (t - t0) / (t1 - t0);
    Some(v0 + alpha * (v1 - v0))
}

fn nearest_finite_value(times: &[f64], values: &[Option<f64>], t: f64) -> Option<f64> {
    if times.is_empty() || times.len() != values.len() {
        return None;
    }
    let right = times.partition_point(|probe| *probe < t);
    let left = right.saturating_sub(1);
    let left_candidate = scan_left(times, values, left);
    let right_candidate = scan_right(times, values, right);
    choose_closest_candidate(t, left_candidate, right_candidate)
}

fn scan_left(times: &[f64], values: &[Option<f64>], start: usize) -> Option<(f64, f64)> {
    let mut index = start;
    while index < values.len() {
        if let Some(value) = values[index] {
            return Some((times[index], value));
        }
        if index == 0 {
            break;
        }
        index -= 1;
    }
    None
}

fn scan_right(times: &[f64], values: &[Option<f64>], start: usize) -> Option<(f64, f64)> {
    let mut index = start.min(values.len());
    while index < values.len() {
        if let Some(value) = values[index] {
            return Some((times[index], value));
        }
        index += 1;
    }
    None
}

fn choose_closest_candidate(
    t: f64,
    left: Option<(f64, f64)>,
    right: Option<(f64, f64)>,
) -> Option<f64> {
    match (left, right) {
        (Some((left_t, left_v)), Some((right_t, right_v))) => {
            if (t - left_t).abs() <= (right_t - t).abs() {
                Some(left_v)
            } else {
                Some(right_v)
            }
        }
        (Some((_, value)), None) | (None, Some((_, value))) => Some(value),
        (None, None) => None,
    }
}

fn resolve_trace_path(paths: &MslPaths, args: &Args, source: TraceSource) -> PathBuf {
    let explicit = match source {
        TraceSource::Rumoca => args.rumoca_trace.clone(),
        TraceSource::Omc => args.omc_trace.clone(),
    };
    let default = default_trace_path(paths, &args.model, source);
    resolve_path(&paths.repo_root, explicit.unwrap_or(default))
}

fn default_trace_path(paths: &MslPaths, model_name: &str, source: TraceSource) -> PathBuf {
    let trace_dir = match source {
        TraceSource::Rumoca => &paths.rumoca_trace_dir,
        TraceSource::Omc => &paths.omc_trace_dir,
    };
    trace_dir.join(format!("{model_name}.json"))
}

fn resolve_output_path(paths: &MslPaths, args: &Args) -> PathBuf {
    let default = paths
        .results_dir
        .join("sim_trace_plots")
        .join(format!("{}.html", args.model));
    resolve_path(&paths.repo_root, args.output.clone().unwrap_or(default))
}

fn resolve_path(repo_root: &Path, path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        path
    } else {
        repo_root.join(path)
    }
}

fn write_output_html(path: &Path, html: &str) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create '{}'", parent.display()))?;
    }
    std::fs::write(path, html).with_context(|| format!("failed to write '{}'", path.display()))
}

fn generate_html(model_name: &str, payload: &PlotPayload) -> Result<String> {
    let payload_json =
        serde_json::to_string(payload).context("failed to serialize plot payload")?;
    let model_json =
        serde_json::to_string(model_name).context("failed to serialize model name for JS")?;
    Ok(format!(
        "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n\
         <meta charset=\"utf-8\">\n\
         <title>{model_name} — OMC vs Rumoca</title>\n\
         <style>{UPLOT_CSS}</style>\n\
         <style>{}</style>\n\
         </head>\n<body>\n\
         <div id=\"sidebar\">\n\
         <h2>{model_name}</h2>\n\
         <div id=\"meta\"></div>\n\
         <h3>Variables</h3>\n\
         <div id=\"checks\"></div>\n\
         </div>\n\
         <div id=\"main\">\n\
         <div id=\"header\">OMC vs Rumoca trace overlay</div>\n\
         <div id=\"plot\"></div>\n\
         </div>\n\
         <script>{UPLOT_JS}</script>\n\
         <script>{}</script>\n\
         </body>\n</html>",
        app_css(),
        app_js(&payload_json, &model_json),
        model_name = model_name,
        UPLOT_CSS = UPLOT_CSS,
        UPLOT_JS = UPLOT_JS,
    ))
}

fn app_css() -> &'static str {
    r#"* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, monospace;
       background: #1e1e1e; color: #d4d4d4; display: flex; height: 100vh; }
#sidebar { width: 280px; min-width: 200px; padding: 12px; overflow-y: auto;
           border-right: 1px solid #333; flex-shrink: 0; }
#sidebar h2 { font-size: 14px; margin-bottom: 8px; color: #569cd6; }
#sidebar h3 { font-size: 12px; margin: 12px 0 6px; color: #888; text-transform: uppercase; }
#meta { font-size: 12px; color: #bdbdbd; margin-bottom: 8px; line-height: 1.4; }
#sidebar label { display: block; padding: 2px 0; font-size: 13px; cursor: pointer; }
#sidebar input[type=checkbox] { margin-right: 6px; }
#main { flex: 1; display: flex; flex-direction: column; padding: 12px; min-width: 0; }
#header { font-size: 16px; font-weight: bold; margin-bottom: 8px; color: #dcdcaa; }
#plot { flex: 1; min-height: 0; }
.u-wrap { background: #1e1e1e !important; }"#
}

fn app_js(payload_json: &str, model_json: &str) -> String {
    format!(
        r##"(function() {{
  var payload = {payload_json};
  var modelName = {model_json};
  var palette = [
    "#4ec9b0","#569cd6","#ce9178","#dcdcaa","#c586c0",
    "#9cdcfe","#d7ba7d","#608b4e","#d16969","#b5cea8"
  ];

  var checksEl = document.getElementById("checks");
  var metaEl = document.getElementById("meta");
  var modelMetric = payload.model_metric;
  var metricText = modelMetric
    ? ("<br>Bounded nL1 score: " + modelMetric.bounded_normalized_l1_score.toExponential(3) +
       " | Mean channel score: " + modelMetric.mean_channel_bounded_normalized_l1.toExponential(3))
    : "";
  metaEl.innerHTML =
    "Overlap: [" + payload.overlap_start.toFixed(6) + ", " + payload.overlap_end.toFixed(6) + "]" +
    "<br>Samples: " + payload.times.length +
    "<br>Common variables: " + payload.variables.length + metricText;

  var checksHtml = "";
  for (var i = 0; i < payload.variables.length; i++) {{
    var checked = i < 1 ? "checked" : "";
    var color = palette[i % palette.length];
    checksHtml += '<label><input type="checkbox" data-idx="' + i + '" ' + checked + '>' +
                  '<span style="color:' + color + '">\u25CF</span> ' + payload.variables[i] + '</label>';
  }}
  checksEl.innerHTML = checksHtml;

  var plot = null;
  function rebuild() {{
    var active = [];
    var selected = checksEl.querySelectorAll("input:checked");
    for (var i = 0; i < selected.length; i++) active.push(parseInt(selected[i].dataset.idx, 10));
    var plotEl = document.getElementById("plot");
    if (plot) plot.destroy();
    if (active.length === 0) {{
      plotEl.innerHTML = "<p style='padding:40px;color:#888'>Select variables to plot</p>";
      return;
    }}

    var data = [payload.times];
    var series = [{{}}];
    for (var j = 0; j < active.length; j++) {{
      var idx = active[j];
      var color = palette[idx % palette.length];
      data.push(payload.omc_data[idx]);
      series.push({{ label: payload.variables[idx] + " (OMC)", stroke: color, width: 1.5 }});
      data.push(payload.rumoca_data[idx]);
      series.push({{ label: payload.variables[idx] + " (Rumoca)", stroke: color, width: 2.0, dash: [8, 4] }});
    }}

    plot = new uPlot({{
      width: plotEl.clientWidth,
      height: plotEl.clientHeight || 400,
      title: modelName + " — OMC vs Rumoca",
      scales: {{ x: {{ time: false }} }},
      axes: [
        {{ stroke: "#888", grid: {{ stroke: "#333" }}, label: "time", font: "11px monospace", labelFont: "12px monospace" }},
        {{ stroke: "#888", grid: {{ stroke: "#333" }}, font: "11px monospace" }}
      ],
      series: series,
      cursor: {{ drag: {{ x: true, y: true }} }},
      legend: {{ show: true }}
    }}, data, plotEl);
  }}

  checksEl.addEventListener("change", rebuild);
  rebuild();
  window.addEventListener("resize", function() {{
    if (!plot) return;
    var el = document.getElementById("plot");
    plot.setSize({{ width: el.clientWidth, height: el.clientHeight || 400 }});
  }});
}})();"##,
        payload_json = payload_json,
        model_json = model_json
    )
}

#[derive(Debug, Clone, Copy)]
enum TraceSource {
    Rumoca,
    Omc,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn trace(model: &str, times: Vec<f64>, names: Vec<&str>, data: Vec<Vec<f64>>) -> SimTrace {
        SimTrace {
            model_name: Some(model.to_string()),
            times,
            names: names.into_iter().map(ToOwned::to_owned).collect(),
            data: data
                .into_iter()
                .map(|column| column.into_iter().map(Some).collect())
                .collect(),
            variable_meta: None,
        }
    }

    #[test]
    fn align_traces_preserves_common_variables_and_grid() {
        let rumoca = trace(
            "M",
            vec![0.0, 0.5, 1.0],
            vec!["x", "y"],
            vec![vec![0.0, 1.0, 2.0], vec![2.0, 2.0, 2.0]],
        );
        let omc = trace(
            "M",
            vec![0.0, 0.25, 0.75, 1.0],
            vec!["x", "z", "y"],
            vec![
                vec![0.0, 1.1, 1.9, 2.0],
                vec![5.0, 5.0, 5.0, 5.0],
                vec![2.0, 2.0, 2.0, 2.0],
            ],
        );
        let aligned = align_traces("M", &rumoca, &omc).expect("aligned traces");
        assert_eq!(aligned.variables, vec!["x".to_string(), "y".to_string()]);
        assert!(aligned.times.len() >= 4);
        assert_eq!(aligned.omc_data.len(), 2);
        assert_eq!(aligned.rumoca_data.len(), 2);
    }

    #[test]
    fn interp_linear_supports_midpoint() {
        let times = vec![0.0, 1.0];
        let values = vec![Some(0.0), Some(2.0)];
        let value = interp_linear(&times, &values, 0.5).expect("interpolated value");
        assert!((value - 1.0).abs() < 1.0e-12);
    }

    #[test]
    fn parse_omc_result_file_from_record_block() {
        let output = r#"
record SimulationResult
  resultFile = "/tmp/Modelica.Blocks.Examples.PID_Controller_res.csv",
  timeSimulation = 0.02
end SimulationResult;
"#;
        let result_file = parse_omc_result_file(output).expect("result file");
        assert_eq!(
            result_file,
            "/tmp/Modelica.Blocks.Examples.PID_Controller_res.csv"
        );
    }
}
