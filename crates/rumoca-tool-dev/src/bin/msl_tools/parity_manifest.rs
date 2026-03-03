use crate::common::{MslPaths, get_git_commit, unix_timestamp_seconds, write_pretty_json};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use serde::Serialize;
use serde_json::{Value, json};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct Args {
    /// Rumoca MSL results JSON (default: `CACHE_DIR/results/msl_results.json`)
    #[arg(long)]
    rumoca_results_file: Option<PathBuf>,
    /// OMC simulation reference JSON (default: `CACHE_DIR/results/omc_simulation_reference.json`)
    #[arg(long)]
    omc_simulation_reference_file: Option<PathBuf>,
    /// Output manifest JSON (default: `CACHE_DIR/results/parity_fail_manifest.json`)
    #[arg(long)]
    output_file: Option<PathBuf>,
    /// Optional reason filter(s), comma-separated
    #[arg(long, value_delimiter = ',')]
    reason: Vec<String>,
}

#[derive(Debug, Clone)]
struct RumocaModelStatus {
    phase_reached: Option<String>,
    sim_status: Option<String>,
    sim_error: Option<String>,
}

#[derive(Debug, Clone)]
struct OmcModelStatus {
    status: String,
    error: Option<String>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
struct ParityRecord {
    model_name: String,
    reason: String,
    omc_status: String,
    omc_error: Option<String>,
    rumoca_phase: Option<String>,
    rumoca_sim_status: Option<String>,
    rumoca_sim_error: Option<String>,
}

pub(crate) fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let rumoca_file = resolve_path(
        &paths.repo_root,
        args.rumoca_results_file,
        paths.results_dir.join("msl_results.json"),
    );
    let omc_file = resolve_path(
        &paths.repo_root,
        args.omc_simulation_reference_file,
        paths.results_dir.join("omc_simulation_reference.json"),
    );
    let output_file = resolve_path(
        &paths.repo_root,
        args.output_file,
        paths.results_dir.join("parity_fail_manifest.json"),
    );

    let rumoca_payload = read_json(&rumoca_file)?;
    let omc_payload = read_json(&omc_file)?;
    let rumoca_models = parse_rumoca_models(&rumoca_payload);
    let omc_models = parse_omc_models(&omc_payload);

    let reason_filter: BTreeSet<String> = args
        .reason
        .iter()
        .flat_map(|raw| raw.split(','))
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(|s| s.to_ascii_lowercase())
        .collect();

    let mut records = build_parity_records(&omc_models, &rumoca_models);
    if !reason_filter.is_empty() {
        records.retain(|record| reason_filter.contains(&record.reason));
    }

    let omc_success_models = omc_models
        .values()
        .filter(|model| model.status == "success")
        .count();
    let grouped_by_reason = group_by_reason(&records);
    let reason_counts = grouped_by_reason
        .iter()
        .map(|(reason, models)| (reason.clone(), models.len()))
        .collect::<BTreeMap<_, _>>();
    let model_names = records
        .iter()
        .map(|record| record.model_name.clone())
        .collect::<Vec<_>>();

    let payload = json!({
        "generated_at_unix_seconds": unix_timestamp_seconds(),
        "git_commit": get_git_commit(&paths.repo_root),
        "inputs": {
            "rumoca_results_file": rumoca_file.display().to_string(),
            "omc_simulation_reference_file": omc_file.display().to_string(),
        },
        "summary": {
            "omc_models_total": omc_models.len(),
            "omc_success_models": omc_success_models,
            "parity_fail_models": records.len(),
            "rumoca_success_when_omc_success": omc_success_models.saturating_sub(records.len()),
        },
        "reason_counts": reason_counts,
        "model_names": model_names,
        "grouped_by_reason": grouped_by_reason,
        "records": records,
    });

    write_pretty_json(&output_file, &payload)?;
    print_summary(&output_file, omc_success_models, &payload);
    Ok(())
}

fn print_summary(output_file: &Path, omc_success_models: usize, payload: &Value) {
    let parity_fail_models = payload
        .get("summary")
        .and_then(|summary| summary.get("parity_fail_models"))
        .and_then(Value::as_u64)
        .unwrap_or(0) as usize;
    let reason_counts = payload
        .get("reason_counts")
        .and_then(Value::as_object)
        .cloned()
        .unwrap_or_default();
    println!("Parity manifest written to {}", output_file.display());
    println!(
        "  OMC success models: {} | rumoca failures on those models: {}",
        omc_success_models, parity_fail_models
    );
    if !reason_counts.is_empty() {
        println!("  Failure buckets:");
        for (reason, count) in reason_counts {
            println!("    {reason}: {}", count.as_u64().unwrap_or(0));
        }
    }
}

fn resolve_path(repo_root: &Path, path: Option<PathBuf>, default_path: PathBuf) -> PathBuf {
    let path = path.unwrap_or(default_path);
    if path.is_absolute() {
        path
    } else {
        repo_root.join(path)
    }
}

fn read_json(path: &Path) -> Result<Value> {
    if !path.exists() {
        bail!("missing input file '{}'", path.display());
    }
    let raw = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read '{}'", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("invalid JSON '{}'", path.display()))
}

fn parse_rumoca_models(payload: &Value) -> BTreeMap<String, RumocaModelStatus> {
    payload
        .get("model_results")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|entry| {
            let name = entry.get("model_name").and_then(Value::as_str)?.to_string();
            let phase_reached = entry
                .get("phase_reached")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned);
            let sim_status = entry
                .get("sim_status")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned);
            let sim_error = entry
                .get("sim_error")
                .or_else(|| entry.get("error"))
                .and_then(Value::as_str)
                .map(ToOwned::to_owned);
            Some((
                name,
                RumocaModelStatus {
                    phase_reached,
                    sim_status,
                    sim_error,
                },
            ))
        })
        .collect()
}

fn parse_omc_models(payload: &Value) -> BTreeMap<String, OmcModelStatus> {
    payload
        .get("models")
        .and_then(Value::as_object)
        .into_iter()
        .flatten()
        .map(|(name, entry)| {
            let status = entry
                .get("status")
                .and_then(Value::as_str)
                .unwrap_or("missing")
                .to_string();
            let error = entry
                .get("error")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned);
            (name.clone(), OmcModelStatus { status, error })
        })
        .collect()
}

fn build_parity_records(
    omc_models: &BTreeMap<String, OmcModelStatus>,
    rumoca_models: &BTreeMap<String, RumocaModelStatus>,
) -> Vec<ParityRecord> {
    omc_models
        .iter()
        .filter(|(_, omc)| omc.status == "success")
        .filter_map(|(model_name, omc)| {
            let rumoca = rumoca_models.get(model_name);
            let reason = classify_parity_failure_reason(rumoca)?;
            Some(ParityRecord {
                model_name: model_name.clone(),
                reason,
                omc_status: omc.status.clone(),
                omc_error: omc.error.clone(),
                rumoca_phase: rumoca.and_then(|model| model.phase_reached.clone()),
                rumoca_sim_status: rumoca.and_then(|model| model.sim_status.clone()),
                rumoca_sim_error: rumoca.and_then(|model| model.sim_error.clone()),
            })
        })
        .collect()
}

fn classify_parity_failure_reason(rumoca: Option<&RumocaModelStatus>) -> Option<String> {
    let rumoca = rumoca?;
    match rumoca.sim_status.as_deref() {
        Some("sim_ok") => None,
        Some("sim_timeout") => Some("sim_timeout".to_string()),
        Some("sim_nan") => Some("sim_nan".to_string()),
        Some("sim_balance_fail") => Some("sim_balance_fail".to_string()),
        Some("sim_solver_fail") => Some(classify_solver_fail_reason(
            rumoca.sim_error.as_deref().unwrap_or_default(),
        )),
        Some(status) => Some(format!("rumoca_{status}")),
        None => {
            if rumoca
                .phase_reached
                .as_deref()
                .is_some_and(|phase| phase != "Success")
            {
                Some("rumoca_non_success_phase".to_string())
            } else {
                Some("rumoca_sim_not_attempted".to_string())
            }
        }
    }
}

fn classify_solver_fail_reason(message: &str) -> String {
    let lower = message.to_ascii_lowercase();
    if lower.contains("interpolationtimeoutsidecurrentstep")
        || lower.contains("interpolation time outside current step")
    {
        return "solver_interpolation_outside_step".to_string();
    }
    if lower.contains("step size is too small") {
        return "solver_step_size_too_small".to_string();
    }
    if lower.contains("maximum number of nonlinear solver failures") {
        return "solver_nonlinear_fail_limit".to_string();
    }
    if lower.contains("maximum number of error test failures") {
        return "solver_error_test_fail_limit".to_string();
    }
    if lower.contains("division by zero at initialization") {
        return "init_division_by_zero".to_string();
    }
    if lower.contains("non-finite parameter") || lower.contains("non finite parameter") {
        return "init_non_finite_parameter".to_string();
    }
    if lower.contains("missing state equation") {
        return "missing_state_equation_in_solver".to_string();
    }
    "solver_other".to_string()
}

fn group_by_reason(records: &[ParityRecord]) -> BTreeMap<String, Vec<String>> {
    let mut grouped: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for record in records {
        grouped
            .entry(record.reason.clone())
            .or_default()
            .push(record.model_name.clone());
    }
    for models in grouped.values_mut() {
        models.sort();
    }
    grouped
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_solver_fail_reason_detects_expected_buckets() {
        assert_eq!(
            classify_solver_fail_reason("InterpolationTimeOutsideCurrentStep"),
            "solver_interpolation_outside_step"
        );
        assert_eq!(
            classify_solver_fail_reason("Step size is too small at time = 0.2"),
            "solver_step_size_too_small"
        );
        assert_eq!(
            classify_solver_fail_reason("division by zero at initialization (t=0)"),
            "init_division_by_zero"
        );
        assert_eq!(
            classify_solver_fail_reason("missing state equation in solver"),
            "missing_state_equation_in_solver"
        );
    }

    #[test]
    fn build_parity_records_uses_omc_success_and_rumoca_failures() {
        let omc_models = BTreeMap::from([
            (
                "Modelica.A".to_string(),
                OmcModelStatus {
                    status: "success".to_string(),
                    error: None,
                },
            ),
            (
                "Modelica.B".to_string(),
                OmcModelStatus {
                    status: "error".to_string(),
                    error: Some("omc failed".to_string()),
                },
            ),
            (
                "Modelica.C".to_string(),
                OmcModelStatus {
                    status: "success".to_string(),
                    error: None,
                },
            ),
        ]);
        let rumoca_models = BTreeMap::from([
            (
                "Modelica.A".to_string(),
                RumocaModelStatus {
                    phase_reached: Some("Success".to_string()),
                    sim_status: Some("sim_solver_fail".to_string()),
                    sim_error: Some("Step size is too small at time = 0.1".to_string()),
                },
            ),
            (
                "Modelica.B".to_string(),
                RumocaModelStatus {
                    phase_reached: Some("Success".to_string()),
                    sim_status: Some("sim_timeout".to_string()),
                    sim_error: None,
                },
            ),
            (
                "Modelica.C".to_string(),
                RumocaModelStatus {
                    phase_reached: Some("Success".to_string()),
                    sim_status: Some("sim_ok".to_string()),
                    sim_error: None,
                },
            ),
        ]);

        let records = build_parity_records(&omc_models, &rumoca_models);
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].model_name, "Modelica.A");
        assert_eq!(records[0].reason, "solver_step_size_too_small");
    }
}
