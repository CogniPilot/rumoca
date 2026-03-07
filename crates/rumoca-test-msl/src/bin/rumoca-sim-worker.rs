#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use clap::Parser;
use rumoca_session::compile::Dae;
use rumoca_session::runtime::{SimError, SimOptions, SimResult, SimSolverMode, simulate_dae};
use serde::Deserialize;

#[derive(Debug, Parser)]
#[command(name = "rumoca-sim-worker")]
#[command(about = "Isolated simulation worker for MSL test harness")]
struct Args {
    #[arg(long)]
    dae_json: PathBuf,
    #[arg(long)]
    result_json: PathBuf,
    #[arg(long)]
    model_name: String,
    #[arg(long, default_value_t = 0.0)]
    t_start: f64,
    #[arg(long, default_value_t = 1.0)]
    t_end: f64,
    #[arg(long)]
    dt: Option<f64>,
    #[arg(long)]
    rtol: Option<f64>,
    #[arg(long)]
    atol: Option<f64>,
    /// Solver selection hint. Accepts rumoca (`auto`, `bdf`, `rk`) and
    /// common OpenModelica/Dymola names (`dassl`, `ida`, `rungekutta`, etc).
    #[arg(long, default_value = "auto")]
    solver: String,
    #[arg(long, default_value_t = 100)]
    output_samples: usize,
    #[arg(long, default_value_t = 10.0)]
    timeout_seconds: f64,
    /// Optional path for a per-model simulation trace JSON artifact.
    #[arg(long)]
    trace_json: Option<PathBuf>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SimWorkerResult {
    status: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    error: Option<String>,
    sim_seconds: f64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    trace_file: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    trace_error: Option<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SimTraceArtifact {
    model_name: String,
    n_states: usize,
    times: Vec<f64>,
    names: Vec<String>,
    data: Vec<Vec<f64>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    variable_meta: Option<Vec<SimTraceVariableMetaArtifact>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct SimTraceVariableMetaArtifact {
    name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    role: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    value_type: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    variability: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    time_domain: Option<String>,
}

fn panic_message(panic_info: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = panic_info.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = panic_info.downcast_ref::<String>() {
        msg.clone()
    } else {
        "unknown panic".to_string()
    }
}

fn sample_series_value(result: &SimResult, series_name: &str, time_idx: usize) -> Option<f64> {
    let col_idx = result.names.iter().position(|name| name == series_name)?;
    result
        .data
        .get(col_idx)
        .and_then(|col| col.get(time_idx))
        .copied()
}

fn push_named_value_detail(
    result: &SimResult,
    details: &mut Vec<String>,
    series_name: &str,
    time_idx: usize,
) {
    if let Some(series_value) = sample_series_value(result, series_name, time_idx) {
        details.push(format!("{series_name}={series_value}"));
    }
}

fn sim_worker_result(status: &str, error: Option<String>, sim_seconds: f64) -> SimWorkerResult {
    SimWorkerResult {
        status: status.to_string(),
        error,
        sim_seconds,
        trace_file: None,
        trace_error: None,
    }
}

fn first_non_finite_sample(result: &SimResult) -> Option<(usize, usize, f64)> {
    result.data.iter().enumerate().find_map(|(col_idx, col)| {
        col.iter()
            .enumerate()
            .find_map(|(time_idx, v)| (!v.is_finite()).then_some((col_idx, time_idx, *v)))
    })
}

fn classify_success(result: &SimResult, elapsed: f64) -> SimWorkerResult {
    let first_non_finite = first_non_finite_sample(result);
    if let Some((col_idx, time_idx, value)) = first_non_finite {
        let var_name = result
            .names
            .get(col_idx)
            .cloned()
            .unwrap_or_else(|| format!("col[{col_idx}]"));
        let t = result.times.get(time_idx).copied().unwrap_or(0.0);
        let mut details: Vec<String> = Vec::new();

        if let Some(base) = var_name.strip_suffix(".LossPower") {
            let v_name = format!("{base}.v");
            let i_name = format!("{base}.i");
            push_named_value_detail(result, &mut details, &v_name, time_idx);
            push_named_value_detail(result, &mut details, &i_name, time_idx);
        }

        let mut huge_finite: Vec<(String, f64)> = result
            .names
            .iter()
            .enumerate()
            .filter_map(|(idx, name)| {
                let v = result.data.get(idx)?.get(time_idx).copied()?;
                (v.is_finite() && v.abs() > 1.0e100).then_some((name.clone(), v))
            })
            .collect();
        huge_finite.sort_by(|a, b| {
            b.1.abs()
                .partial_cmp(&a.1.abs())
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        if !huge_finite.is_empty() {
            let sample = huge_finite
                .iter()
                .take(3)
                .map(|(name, value)| format!("{name}={value}"))
                .collect::<Vec<_>>()
                .join(", ");
            details.push(format!("huge_finite=[{sample}]"));
        }

        let detail_suffix = if details.is_empty() {
            String::new()
        } else {
            format!(" ({})", details.join("; "))
        };

        return sim_worker_result(
            "sim_nan",
            Some(format!(
                "NaN/Inf in output at {} (index {}) t={} value={}{}",
                var_name, col_idx, t, value, detail_suffix
            )),
            elapsed,
        );
    }

    sim_worker_result("sim_ok", None, elapsed)
}

fn classify_solver_error(err: SimError, elapsed: f64) -> SimWorkerResult {
    match err {
        SimError::Timeout { seconds } => sim_worker_result(
            "sim_timeout",
            Some(format!("timeout after {:.3}s", seconds)),
            elapsed,
        ),
        other => sim_worker_result("sim_solver_fail", Some(other.to_string()), elapsed),
    }
}

fn write_trace_json(trace_path: &Path, model_name: &str, result: &SimResult) -> Result<(), String> {
    if let Some(parent) = trace_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            format!(
                "failed to create trace directory '{}': {e}",
                parent.display()
            )
        })?;
    }

    let trace = SimTraceArtifact {
        model_name: model_name.to_string(),
        n_states: result.n_states,
        times: result.times.clone(),
        names: result.names.clone(),
        data: result.data.clone(),
        variable_meta: if result.variable_meta.is_empty() {
            None
        } else {
            Some(
                result
                    .variable_meta
                    .iter()
                    .map(|meta| SimTraceVariableMetaArtifact {
                        name: meta.name.clone(),
                        role: Some(meta.role.clone()),
                        value_type: meta.value_type.clone(),
                        variability: meta.variability.clone(),
                        time_domain: meta.time_domain.clone(),
                    })
                    .collect(),
            )
        },
    };

    let mut file = File::create(trace_path).map_err(|e| {
        format!(
            "failed to create trace file '{}': {e}",
            trace_path.display()
        )
    })?;
    serde_json::to_writer(&mut file, &trace).map_err(|e| {
        format!(
            "failed to serialize trace JSON '{}': {e}",
            trace_path.display()
        )
    })?;
    file.write_all(b"\n").map_err(|e| {
        format!(
            "failed to finalize trace JSON '{}': {e}",
            trace_path.display()
        )
    })?;
    Ok(())
}

fn parse_dae_json(mut reader: impl Read, source_label: &str) -> Result<Dae, String> {
    let mut payload = Vec::new();
    reader
        .read_to_end(&mut payload)
        .map_err(|e| format!("failed to read dae_json '{source_label}': {e}"))?;

    let source = source_label.to_string();
    let thread_source = source.clone();
    let parser = std::thread::Builder::new()
        .name("rumoca-sim-worker-json-parse".to_string())
        .stack_size(32 * 1024 * 1024)
        .spawn(move || {
            let mut deserializer = serde_json::Deserializer::from_slice(&payload);
            deserializer.disable_recursion_limit();
            Dae::deserialize(&mut deserializer)
                .map_err(|e| format!("failed to parse dae_json '{thread_source}': {e}"))
        })
        .map_err(|e| format!("failed to spawn dae_json parser thread for '{source}': {e}"))?;

    match parser.join() {
        Ok(result) => result,
        Err(panic_info) => Err(format!(
            "failed to parse dae_json '{source}': parser thread panicked: {}",
            panic_message(panic_info)
        )),
    }
}

fn sample_grid_dt(args: &Args) -> Option<f64> {
    if args.output_samples == 0 {
        return None;
    }
    let span = (args.t_end - args.t_start).abs();
    if !span.is_finite() || span <= 0.0 {
        return None;
    }
    Some((span / args.output_samples as f64).max(1e-6))
}

fn effective_output_dt(args: &Args) -> Option<f64> {
    // Honor explicit experiment Interval from the model/test settings.
    // `output_samples` is only a fallback when no interval is provided.
    args.dt
        .filter(|value| value.is_finite() && *value > 0.0)
        .or_else(|| sample_grid_dt(args))
}

fn run(args: &Args) -> SimWorkerResult {
    let dae = match File::open(&args.dae_json)
        .map_err(|e| format!("failed to open dae_json '{}': {e}", args.dae_json.display()))
        .and_then(|file| parse_dae_json(file, &args.dae_json.display().to_string()))
    {
        Ok(dae) => dae,
        Err(err) => {
            return SimWorkerResult {
                status: "sim_solver_fail".to_string(),
                error: Some(err),
                sim_seconds: 0.0,
                trace_file: None,
                trace_error: None,
            };
        }
    };

    let dt = effective_output_dt(args);
    let solver_mode = SimSolverMode::from_external_name(&args.solver);
    let mut opts = SimOptions {
        t_start: args.t_start,
        t_end: args.t_end,
        dt,
        max_wall_seconds: Some(args.timeout_seconds),
        solver_mode,
        ..SimOptions::default()
    };
    if let Some(rtol) = args.rtol.filter(|v| v.is_finite() && *v > 0.0) {
        opts.rtol = rtol;
    }
    if let Some(atol) = args.atol.filter(|v| v.is_finite() && *v > 0.0) {
        opts.atol = atol;
    }

    let sim_start = Instant::now();
    let outcome =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| simulate_dae(&dae, &opts)));
    let elapsed = sim_start.elapsed().as_secs_f64();
    match outcome {
        Ok(Ok(result)) => {
            let mut worker_result = classify_success(&result, elapsed);
            if worker_result.status == "sim_ok"
                && let Some(trace_path) = args.trace_json.as_deref()
            {
                match write_trace_json(trace_path, &args.model_name, &result) {
                    Ok(()) => {
                        worker_result.trace_file = Some(trace_path.to_string_lossy().to_string());
                    }
                    Err(err) => {
                        worker_result.trace_error = Some(err);
                    }
                }
            }
            worker_result
        }
        Ok(Err(err)) => classify_solver_error(err, elapsed),
        Err(panic_info) => sim_worker_result(
            "sim_solver_fail",
            Some(format!("panic: {}", panic_message(panic_info))),
            elapsed,
        ),
    }
}

fn write_result(path: &PathBuf, result: &SimWorkerResult) -> std::io::Result<()> {
    let mut file = File::create(path)?;
    serde_json::to_writer(&mut file, result).map_err(std::io::Error::other)?;
    file.write_all(b"\n")?;
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let result = run(&args);
    write_result(&args.result_json, &result)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{Args, effective_output_dt, parse_dae_json, sample_grid_dt};
    use rumoca_session::compile::{Session, SessionConfig};
    use serde_json::json;
    use std::path::PathBuf;

    fn deep_add_expr_json(base_add_expr: &serde_json::Value, depth: usize) -> serde_json::Value {
        let mut expr = base_add_expr.clone();
        let add_op = expr
            .get("Binary")
            .and_then(|node| node.get("op"))
            .cloned()
            .expect("base add expression should have Binary.op");

        for _ in 0..depth {
            expr = json!({
                "Binary": {
                    "op": add_op.clone(),
                    "lhs": expr,
                    "rhs": {"Literal": {"Integer": 1}}
                }
            });
        }

        expr
    }

    #[test]
    fn parse_dae_json_handles_deeply_nested_expression_trees() {
        let source =
            "model DeepJson\n  Real x(start = 0);\nequation\n  der(x) = 0 + 1;\nend DeepJson;\n";
        let mut session = Session::new(SessionConfig::default());
        session
            .add_document("deep_json.mo", source)
            .expect("add source file");
        let compiled = session
            .compile_model("DeepJson")
            .expect("compile deep nested expression model");
        let mut dae_json = serde_json::to_value(&compiled.dae).expect("serialize deep DAE json");
        let base_add_expr = dae_json["f_x"][0]["rhs"]["Binary"]["rhs"].clone();
        dae_json["f_x"][0]["rhs"]["Binary"]["rhs"] = deep_add_expr_json(&base_add_expr, 512);
        let payload = serde_json::to_vec(&dae_json).expect("encode deep DAE JSON payload");
        let parsed =
            parse_dae_json(std::io::Cursor::new(payload), "in-memory").expect("parse deep DAE");
        assert_eq!(parsed.f_x.len(), 1);
    }

    fn test_args() -> Args {
        Args {
            dae_json: PathBuf::from("in.json"),
            result_json: PathBuf::from("out.json"),
            model_name: "M".to_string(),
            t_start: 0.0,
            t_end: 1.0,
            dt: None,
            rtol: None,
            atol: None,
            solver: "auto".to_string(),
            output_samples: 100,
            timeout_seconds: 10.0,
            trace_json: None,
        }
    }

    #[test]
    fn test_sample_grid_dt_uses_span_and_output_samples() {
        let args = test_args();
        let dt = sample_grid_dt(&args).expect("sample dt should be present");
        assert!((dt - 0.01).abs() < 1e-12);
    }

    #[test]
    fn test_effective_output_dt_respects_annotation_interval_even_if_finer_than_sample_grid() {
        let mut args = test_args();
        args.dt = Some(1e-6);
        args.output_samples = 100;
        args.t_start = 0.0;
        args.t_end = 1.0;
        let dt = effective_output_dt(&args).expect("effective dt should be present");
        assert!(
            (dt - 1e-6).abs() < 1e-18,
            "annotation dt should be used directly"
        );
    }

    #[test]
    fn test_effective_output_dt_uses_sample_grid_when_annotation_missing() {
        let args = test_args();
        let dt = effective_output_dt(&args).expect("effective dt should be present");
        assert!((dt - 0.01).abs() < 1e-12);
    }

    #[test]
    fn test_effective_output_dt_keeps_annotation_interval() {
        let mut args = test_args();
        args.dt = Some(0.05);
        args.output_samples = 100;
        args.t_start = 0.0;
        args.t_end = 1.0;
        let dt = effective_output_dt(&args).expect("effective dt should be present");
        assert!((dt - 0.05).abs() < 1e-12);
    }
}
