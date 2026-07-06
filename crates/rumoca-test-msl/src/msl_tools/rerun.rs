use super::common::MslPaths;
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use serde_json::Value;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone, ClapArgs)]
pub struct Args {
    /// Exact model name(s) to rerun. Comma-separated values are accepted.
    #[arg(long, value_delimiter = ',')]
    model: Vec<String>,
    /// Rerun models from this reason-code bucket in the triage report.
    #[arg(long)]
    reason: Option<String>,
    /// Triage JSON to read when --reason is used.
    #[arg(long)]
    triage_file: Option<PathBuf>,
    /// Focused MSL results directory.
    #[arg(long)]
    results_dir: Option<PathBuf>,
    /// Maximum models selected from a reason bucket. Use 0 for no limit.
    #[arg(long, default_value_t = 20)]
    limit: usize,
    /// Print the focused rerun command without executing it.
    #[arg(long)]
    dry_run: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RerunCommandSpec {
    program: String,
    args: Vec<String>,
}

pub fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let models = select_models(&paths, &args)?;
    let results_dir = args.results_dir.clone();
    let spec = build_rerun_command_spec(&models, results_dir.as_deref());
    print_rerun_summary(&models, &spec);
    if args.dry_run {
        return Ok(());
    }
    run_rerun_command(&spec, &paths.repo_root)
}

fn select_models(paths: &MslPaths, args: &Args) -> Result<Vec<String>> {
    let explicit_models = normalize_models(&args.model);
    match (&explicit_models[..], args.reason.as_deref()) {
        ([], None) => bail!("provide --model or --reason"),
        ([], Some(reason)) => select_models_for_reason(paths, args, reason),
        (_, None) => Ok(explicit_models),
        (_, Some(_)) => bail!("--model and --reason are mutually exclusive"),
    }
}

fn normalize_models(models: &[String]) -> Vec<String> {
    let mut selected = Vec::new();
    for model in models
        .iter()
        .map(|model| model.trim())
        .filter(|model| !model.is_empty())
    {
        if !selected.iter().any(|existing| existing == model) {
            selected.push(model.to_string());
        }
    }
    selected
}

fn select_models_for_reason(paths: &MslPaths, args: &Args, reason: &str) -> Result<Vec<String>> {
    let triage_file = args
        .triage_file
        .clone()
        .unwrap_or_else(|| paths.results_dir.join("msl_triage.json"));
    let raw = std::fs::read_to_string(&triage_file)
        .with_context(|| format!("failed to read {}", triage_file.display()))?;
    let payload: Value = serde_json::from_str(&raw)
        .with_context(|| format!("invalid JSON in {}", triage_file.display()))?;
    let mut models = collect_reason_models(&payload, reason);
    if args.limit > 0 {
        models.truncate(args.limit);
    }
    if models.is_empty() {
        bail!(
            "no models with reason '{}' found in {}",
            reason,
            triage_file.display()
        );
    }
    Ok(models)
}

fn collect_reason_models(payload: &Value, reason: &str) -> Vec<String> {
    let mut models = Vec::new();
    for section in [
        "compile_failures",
        "balance_failures",
        "simulation_failures",
        "missing_trace_models",
    ] {
        collect_reason_models_from_section(payload, section, reason, &mut models);
    }
    models
}

fn collect_reason_models_from_section(
    payload: &Value,
    section: &str,
    reason: &str,
    models: &mut Vec<String>,
) {
    let Some(records) = payload.get(section).and_then(Value::as_array) else {
        return;
    };
    for record in records {
        if record.get("reason").and_then(Value::as_str) != Some(reason) {
            continue;
        }
        let Some(model_name) = record.get("model_name").and_then(Value::as_str) else {
            continue;
        };
        if !models.iter().any(|existing| existing == model_name) {
            models.push(model_name.to_string());
        }
    }
}

fn build_rerun_command_spec(models: &[String], results_dir: Option<&Path>) -> RerunCommandSpec {
    let mut args = vec![
        "run".to_string(),
        "--bin".to_string(),
        "xtask".to_string(),
        "--".to_string(),
        "verify".to_string(),
        "msl-parity".to_string(),
        "--sim-match-exact".to_string(),
    ];
    for model in models {
        args.push("--sim-match".to_string());
        args.push(model.clone());
    }
    if let Some(results_dir) = results_dir {
        args.push("--results-dir".to_string());
        args.push(results_dir.display().to_string());
    }
    RerunCommandSpec {
        program: "cargo".to_string(),
        args,
    }
}

fn print_rerun_summary(models: &[String], spec: &RerunCommandSpec) {
    println!("Focused MSL rerun:");
    println!("  models: {}", models.len());
    for model in models {
        println!("    {model}");
    }
    println!("  partial marker: enforced by --sim-match-exact");
    println!("  command: {}", render_command(spec));
}

fn render_command(spec: &RerunCommandSpec) -> String {
    let args = spec
        .args
        .iter()
        .map(|arg| shell_quote(arg))
        .collect::<Vec<_>>()
        .join(" ");
    format!("{} {}", spec.program, args)
}

fn shell_quote(value: &str) -> String {
    format!("'{}'", value.replace('\'', "'\\''"))
}

fn run_rerun_command(spec: &RerunCommandSpec, repo_root: &Path) -> Result<()> {
    let mut command = Command::new(&spec.program);
    command.args(&spec.args).current_dir(repo_root);
    let status = command
        .status()
        .context("failed to launch focused MSL rerun")?;
    if !status.success() {
        bail!("focused MSL rerun failed with status {status}");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn normalize_models_deduplicates_and_discards_empty_values() {
        let models = normalize_models(&[
            " Modelica.A ".to_string(),
            "".to_string(),
            "Modelica.A".to_string(),
            "Modelica.B".to_string(),
        ]);
        assert_eq!(models, ["Modelica.A", "Modelica.B"]);
    }

    #[test]
    fn collect_reason_models_preserves_report_order_and_deduplicates() {
        let payload = json!({
            "compile_failures": [
                {"model_name": "Modelica.A", "reason": "compile.instantiate"},
                {"model_name": "Modelica.B", "reason": "compile.flatten"}
            ],
            "simulation_failures": [
                {"model_name": "Modelica.A", "reason": "compile.instantiate"},
                {"model_name": "Modelica.C", "reason": "compile.instantiate"}
            ]
        });
        let models = collect_reason_models(&payload, "compile.instantiate");
        assert_eq!(models, ["Modelica.A", "Modelica.C"]);
    }

    #[test]
    fn rerun_command_sets_exact_focused_partial_flags() {
        let spec = build_rerun_command_spec(
            &["Modelica.A".to_string(), "Modelica.B".to_string()],
            Some(Path::new("target/msl/results-focused")),
        );
        assert_eq!(spec.program, "cargo");
        assert_eq!(
            spec.args,
            [
                "run",
                "--bin",
                "xtask",
                "--",
                "verify",
                "msl-parity",
                "--sim-match-exact",
                "--sim-match",
                "Modelica.A",
                "--sim-match",
                "Modelica.B",
                "--results-dir",
                "target/msl/results-focused",
            ]
        );
    }
}
