use crate::common::{MslPaths, write_pretty_json};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use serde::Serialize;
use serde_json::{Value, json};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, ClapArgs)]
pub(crate) struct Args {
    /// Optional OMC reference JSON (default: `CACHE_DIR/results/omc_reference.json`)
    #[arg(long)]
    omc_reference_file: Option<PathBuf>,
    /// Optional rumoca balance JSON (default: `CACHE_DIR/results/msl_balance_results.json`)
    #[arg(long)]
    rumoca_balance_file: Option<PathBuf>,
}

#[derive(Debug, Clone, Serialize, Default)]
struct RumocaModelInfo {
    status: String,
    phase: Option<String>,
    balance: Option<i64>,
    equations: Option<usize>,
    variables: Option<usize>,
    initial_balance_ok: Option<bool>,
    initial_balance_deficit_before: Option<i64>,
    initial_balance_deficit_after: Option<i64>,
}

#[derive(Debug, Clone, Serialize)]
struct ModelDetails {
    category: String,
    omc_status: String,
    omc_equations: Option<usize>,
    omc_variables: Option<usize>,
    omc_balanced: Option<bool>,
    rumoca_status: String,
    rumoca_balance: Option<i64>,
    rumoca_phase: Option<String>,
    rumoca_equations: Option<usize>,
    rumoca_variables: Option<usize>,
    rumoca_initial_balance_ok: Option<bool>,
    rumoca_initial_deficit_before: Option<i64>,
    rumoca_initial_deficit_after: Option<i64>,
}

pub(crate) fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let omc_file = resolve_path(
        &paths.repo_root,
        args.omc_reference_file
            .unwrap_or_else(|| paths.results_dir.join("omc_reference.json")),
    );
    let rumoca_file = resolve_path(
        &paths.repo_root,
        args.rumoca_balance_file
            .unwrap_or_else(|| paths.results_dir.join("msl_balance_results.json")),
    );
    let omc_payload = read_json(&omc_file)?;
    let rumoca_payload = read_json(&rumoca_file)?;
    let omc_models = parse_omc_models(&omc_payload);
    let rumoca_models = build_rumoca_model_map(&rumoca_payload);
    let model_details = classify_models(&omc_models, &rumoca_models);
    let categories = category_summary(&model_details);
    print_summary(&omc_payload, &rumoca_payload, &categories);
    let report = build_report(&omc_payload, &rumoca_payload, &model_details, &categories);
    write_outputs(&paths, &report, &model_details)?;
    write_timing_snapshot(&paths, &omc_payload, &rumoca_payload)?;
    Ok(())
}

fn resolve_path(repo_root: &Path, path: PathBuf) -> PathBuf {
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
    let payload = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read '{}'", path.display()))?;
    serde_json::from_str(&payload).with_context(|| format!("invalid JSON '{}'", path.display()))
}

fn parse_omc_models(payload: &Value) -> BTreeMap<String, Value> {
    payload
        .get("models")
        .and_then(Value::as_object)
        .map(|map| {
            map.iter()
                .map(|(name, value)| (name.clone(), value.clone()))
                .collect()
        })
        .unwrap_or_default()
}

fn build_rumoca_model_map(payload: &Value) -> BTreeMap<String, RumocaModelInfo> {
    let mut model_map = BTreeMap::new();
    if let Some(model_results) = payload.get("model_results").and_then(Value::as_array) {
        build_from_model_results(model_results, &mut model_map);
    } else {
        build_from_legacy(payload, &mut model_map);
    }
    model_map
}

fn build_from_model_results(
    model_results: &[Value],
    model_map: &mut BTreeMap<String, RumocaModelInfo>,
) {
    for result in model_results {
        let Some(name) = result.get("model_name").and_then(Value::as_str) else {
            continue;
        };
        let phase = result
            .get("phase_reached")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned)
            .unwrap_or_default();
        let error_code = result.get("error_code").and_then(Value::as_str);
        let scalar_equations = parse_usize(result.get("scalar_equations"));
        let scalar_unknowns = parse_usize(result.get("scalar_unknowns"));
        let fallback_eq = parse_usize(result.get("num_f_x"));
        let fallback_vars = parse_usize(result.get("num_states"))
            .zip(parse_usize(result.get("num_algebraics")))
            .map(|(states, algebraics)| states + algebraics);
        let equations = scalar_equations.or(fallback_eq);
        let variables = scalar_unknowns.or(fallback_vars);
        let status = derive_rumoca_status(result, &phase, error_code);
        model_map.insert(
            name.to_string(),
            RumocaModelInfo {
                status,
                phase: if phase.is_empty() { None } else { Some(phase) },
                balance: parse_i64(result.get("balance")),
                equations,
                variables,
                initial_balance_ok: result.get("initial_balance_ok").and_then(Value::as_bool),
                initial_balance_deficit_before: parse_i64(
                    result.get("initial_balance_deficit_before"),
                ),
                initial_balance_deficit_after: parse_i64(
                    result.get("initial_balance_deficit_after"),
                ),
            },
        );
    }
}

fn derive_rumoca_status(result: &Value, phase: &str, error_code: Option<&str>) -> String {
    if phase == "Success" {
        let is_partial = result
            .get("is_partial")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        if is_partial {
            return "partial".to_string();
        }
        let class_type = result
            .get("class_type")
            .and_then(Value::as_str)
            .unwrap_or("model");
        if !matches!(class_type, "model" | "block" | "class") {
            return "non_sim".to_string();
        }
        let is_balanced = result
            .get("is_balanced")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        return if is_balanced {
            "balanced".to_string()
        } else {
            "unbalanced".to_string()
        };
    }
    if phase == "NonSim" || (phase == "Typecheck" && error_code == Some("ET004")) {
        "non_sim".to_string()
    } else {
        "failed".to_string()
    }
}

fn build_from_legacy(payload: &Value, model_map: &mut BTreeMap<String, RumocaModelInfo>) {
    if let Some(failures) = payload.get("failures_by_phase").and_then(Value::as_object) {
        for (phase, models) in failures {
            let Some(entries) = models.as_array() else {
                continue;
            };
            for name in entries.iter().filter_map(Value::as_str) {
                model_map.insert(
                    name.to_string(),
                    RumocaModelInfo {
                        status: "failed".to_string(),
                        phase: Some(phase.clone()),
                        ..RumocaModelInfo::default()
                    },
                );
            }
        }
    }
    if let Some(unbalanced_list) = payload.get("unbalanced_list").and_then(Value::as_array) {
        for entry in unbalanced_list {
            let Some(entry) = entry.as_str() else {
                continue;
            };
            let Some((name, balance_raw)) = entry.rsplit_once(" (balance=") else {
                continue;
            };
            let balance = balance_raw.trim_end_matches(')').parse::<i64>().ok();
            model_map.insert(
                name.to_string(),
                RumocaModelInfo {
                    status: "unbalanced".to_string(),
                    phase: None,
                    balance,
                    ..RumocaModelInfo::default()
                },
            );
        }
    }
}

fn classify_models(
    omc_models: &BTreeMap<String, Value>,
    rumoca_models: &BTreeMap<String, RumocaModelInfo>,
) -> BTreeMap<String, ModelDetails> {
    let all_models = omc_models
        .keys()
        .chain(rumoca_models.keys())
        .cloned()
        .collect::<BTreeSet<_>>();
    let mut model_details = BTreeMap::new();
    for model_name in all_models {
        let details = classify_one_model(
            &model_name,
            omc_models.get(&model_name),
            rumoca_models.get(&model_name),
        );
        model_details.insert(model_name, details);
    }
    model_details
}

fn classify_one_model(
    model_name: &str,
    omc_info: Option<&Value>,
    rumoca_info: Option<&RumocaModelInfo>,
) -> ModelDetails {
    let omc_success = omc_info
        .and_then(|value| value.get("status"))
        .and_then(Value::as_str)
        .is_some_and(|status| status == "success");
    let omc_equations = omc_info.and_then(|value| parse_usize(value.get("equations")));
    let omc_variables = omc_info.and_then(|value| parse_usize(value.get("variables")));
    let omc_balanced = omc_success.then_some(
        omc_equations
            .zip(omc_variables)
            .is_some_and(|(eq, var)| eq == var),
    );
    let (rumoca_status, rumoca_phase, rumoca_balance, rumoca_equations, rumoca_variables) =
        if let Some(info) = rumoca_info {
            (
                info.status.clone(),
                info.phase.clone(),
                info.balance,
                info.equations,
                info.variables,
            )
        } else {
            ("missing".to_string(), None, None, None, None)
        };
    let category = classify_category(
        &rumoca_status,
        omc_success,
        omc_balanced.unwrap_or(false),
        omc_info.is_some(),
        rumoca_equations,
        omc_equations,
    );
    let _ = model_name;
    ModelDetails {
        category: category.to_string(),
        omc_status: if omc_success {
            "success".to_string()
        } else if omc_info.is_some() {
            "error".to_string()
        } else {
            "missing".to_string()
        },
        omc_equations,
        omc_variables,
        omc_balanced,
        rumoca_status,
        rumoca_balance,
        rumoca_phase,
        rumoca_equations,
        rumoca_variables,
        rumoca_initial_balance_ok: rumoca_info.and_then(|info| info.initial_balance_ok),
        rumoca_initial_deficit_before: rumoca_info
            .and_then(|info| info.initial_balance_deficit_before),
        rumoca_initial_deficit_after: rumoca_info
            .and_then(|info| info.initial_balance_deficit_after),
    }
}

fn classify_category(
    rumoca_status: &str,
    omc_success: bool,
    omc_balanced: bool,
    has_omc_info: bool,
    rumoca_equations: Option<usize>,
    omc_equations: Option<usize>,
) -> &'static str {
    match rumoca_status {
        "missing" => "rumoca_missing",
        "partial" => "partial",
        "non_sim" => {
            if omc_success {
                "rumoca_non_sim_omc_ok"
            } else {
                "non_sim"
            }
        }
        "balanced" => {
            if omc_balanced {
                if rumoca_equations.is_some()
                    && omc_equations.is_some()
                    && rumoca_equations != omc_equations
                {
                    "lucky_balance"
                } else {
                    "both_balanced"
                }
            } else if omc_success {
                "rumoca_balanced_omc_unbalanced"
            } else if has_omc_info {
                "rumoca_balanced_omc_failed"
            } else {
                "rumoca_balanced_omc_missing"
            }
        }
        "unbalanced" => {
            if omc_balanced {
                "rumoca_unbalanced_omc_balanced"
            } else if omc_success {
                "both_unbalanced"
            } else {
                "rumoca_unbalanced_omc_failed"
            }
        }
        "failed" => {
            if omc_success {
                "rumoca_failed_omc_ok"
            } else {
                "both_failed"
            }
        }
        _ => "unknown",
    }
}

fn category_summary(model_details: &BTreeMap<String, ModelDetails>) -> BTreeMap<String, usize> {
    let mut categories = BTreeMap::new();
    for details in model_details.values() {
        *categories.entry(details.category.clone()).or_insert(0) += 1;
    }
    categories
}

fn print_summary(
    omc_payload: &Value,
    rumoca_payload: &Value,
    categories: &BTreeMap<String, usize>,
) {
    println!(
        "OMC version: {}",
        omc_payload
            .get("omc_version")
            .and_then(Value::as_str)
            .unwrap_or("unknown")
    );
    println!(
        "OMC models: {} total, {} successful",
        omc_payload
            .get("total_models")
            .and_then(Value::as_u64)
            .unwrap_or(0),
        omc_payload
            .get("successful")
            .and_then(Value::as_u64)
            .unwrap_or(0)
    );
    println!(
        "Rumoca models: {} total, {} compiled, {} balanced",
        rumoca_payload
            .get("total_models")
            .and_then(Value::as_u64)
            .unwrap_or(0),
        rumoca_payload
            .get("compiled_models")
            .and_then(Value::as_u64)
            .unwrap_or(0),
        rumoca_payload
            .get("balanced_models")
            .and_then(Value::as_u64)
            .unwrap_or(0)
    );
    println!("Comparison summary:");
    for (category, count) in categories {
        println!("  {category}: {count}");
    }
}

fn build_report(
    omc_payload: &Value,
    rumoca_payload: &Value,
    model_details: &BTreeMap<String, ModelDetails>,
    categories: &BTreeMap<String, usize>,
) -> Value {
    let lucky_balance = collect_lucky_balance(model_details);
    let balance_bugs = collect_balance_bugs(model_details);
    let compilation_bugs = collect_compilation_bugs(model_details);
    let rumoca_better = collect_rumoca_better(model_details);
    json!({
        "omc_version": omc_payload.get("omc_version"),
        "omc_git_commit": omc_payload.get("git_commit"),
        "msl_version": omc_payload.get("msl_version"),
        "rumoca_msl_version": rumoca_payload.get("msl_version"),
        "rumoca_git_commit": rumoca_payload.get("git_commit"),
        "has_per_model_counts": rumoca_payload.get("model_results").is_some(),
        "total_models_compared": model_details.len(),
        "summary": categories,
        "rumoca_initial_balance": {
            "ok": rumoca_payload.get("initial_balanced_models"),
            "deficit": rumoca_payload.get("initial_unbalanced_models"),
        },
        "lucky_balance": lucky_balance,
        "balance_bugs": balance_bugs,
        "compilation_bugs": compilation_bugs,
        "rumoca_better": rumoca_better,
        "models": model_details,
    })
}

fn collect_lucky_balance(model_details: &BTreeMap<String, ModelDetails>) -> Vec<Value> {
    let mut lucky = model_details
        .iter()
        .filter(|(_, details)| details.category == "lucky_balance")
        .map(|(model, details)| {
            json!({
                "model": model,
                "rumoca_equations": details.rumoca_equations,
                "rumoca_variables": details.rumoca_variables,
                "omc_equations": details.omc_equations,
                "omc_variables": details.omc_variables,
                "eq_diff": details.rumoca_equations.zip(details.omc_equations).map(|(r, o)| (r as i64) - (o as i64)),
                "var_diff": details.rumoca_variables.zip(details.omc_variables).map(|(r, o)| (r as i64) - (o as i64)),
            })
        })
        .collect::<Vec<_>>();
    lucky.sort_by_key(|entry| {
        entry
            .get("eq_diff")
            .and_then(Value::as_i64)
            .map(i64::abs)
            .unwrap_or(i64::MAX)
    });
    lucky
}

fn collect_balance_bugs(model_details: &BTreeMap<String, ModelDetails>) -> Vec<Value> {
    let mut bugs = model_details
        .iter()
        .filter(|(_, details)| details.category == "rumoca_unbalanced_omc_balanced")
        .map(|(model, details)| {
            json!({
                "model": model,
                "rumoca_balance": details.rumoca_balance,
                "rumoca_equations": details.rumoca_equations,
                "rumoca_variables": details.rumoca_variables,
                "omc_equations": details.omc_equations,
                "omc_variables": details.omc_variables,
                "difficulty": details.rumoca_balance.map(i64::abs),
            })
        })
        .collect::<Vec<_>>();
    bugs.sort_by_key(|entry| {
        (
            entry
                .get("difficulty")
                .and_then(Value::as_i64)
                .unwrap_or(i64::MAX),
            entry
                .get("model")
                .and_then(Value::as_str)
                .unwrap_or("")
                .to_string(),
        )
    });
    bugs
}

fn collect_compilation_bugs(model_details: &BTreeMap<String, ModelDetails>) -> Vec<Value> {
    let mut bugs = model_details
        .iter()
        .filter(|(_, details)| {
            matches!(
                details.category.as_str(),
                "rumoca_failed_omc_ok" | "rumoca_non_sim_omc_ok"
            )
        })
        .map(|(model, details)| {
            json!({
                "model": model,
                "rumoca_phase": details.rumoca_phase,
                "omc_equations": details.omc_equations,
                "omc_variables": details.omc_variables,
            })
        })
        .collect::<Vec<_>>();
    bugs.sort_by_key(|entry| {
        entry
            .get("model")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_string()
    });
    bugs
}

fn collect_rumoca_better(model_details: &BTreeMap<String, ModelDetails>) -> Vec<Value> {
    model_details
        .iter()
        .filter(|(_, details)| details.category == "rumoca_balanced_omc_unbalanced")
        .map(|(model, details)| {
            json!({
                "model": model,
                "omc_equations": details.omc_equations,
                "omc_variables": details.omc_variables,
            })
        })
        .collect()
}

fn write_outputs(
    paths: &MslPaths,
    report: &Value,
    model_details: &BTreeMap<String, ModelDetails>,
) -> Result<()> {
    let report_file = paths.results_dir.join("comparison_report.json");
    write_pretty_json(&report_file, report)?;
    let diagnostic_file = paths.results_dir.join("diagnostic_models.txt");
    write_diagnostic_file(&diagnostic_file, model_details)?;
    println!("Full report written to {}", report_file.display());
    println!("Diagnostic list written to {}", diagnostic_file.display());
    Ok(())
}

fn write_diagnostic_file(
    path: &Path,
    model_details: &BTreeMap<String, ModelDetails>,
) -> Result<()> {
    let mut lines = Vec::new();
    lines
        .push("# Models where rumoca is wrong and OMC is right (sorted easiest-first)".to_string());
    lines.push("#".to_string());
    lines.push("# === LUCKY BALANCE ===".to_string());
    for (model, details) in model_details
        .iter()
        .filter(|(_, details)| details.category == "lucky_balance")
    {
        lines.push(format!(
            "{model}  balance=0  rumoca_eq={:?}  rumoca_var={:?}  omc_eq={:?}  omc_var={:?}",
            details.rumoca_equations,
            details.rumoca_variables,
            details.omc_equations,
            details.omc_variables
        ));
    }
    lines.push("#".to_string());
    lines.push("# === BALANCE BUGS ===".to_string());
    for (model, details) in model_details
        .iter()
        .filter(|(_, details)| details.category == "rumoca_unbalanced_omc_balanced")
    {
        lines.push(format!(
            "{model}  balance={:?}  omc_eq={:?}  omc_var={:?}",
            details.rumoca_balance, details.omc_equations, details.omc_variables
        ));
    }
    lines.push("#".to_string());
    lines.push("# === COMPILATION BUGS ===".to_string());
    for (model, details) in model_details.iter().filter(|(_, details)| {
        matches!(
            details.category.as_str(),
            "rumoca_failed_omc_ok" | "rumoca_non_sim_omc_ok"
        )
    }) {
        lines.push(format!(
            "{model}  phase={:?}  omc_eq={:?}  omc_var={:?}",
            details.rumoca_phase, details.omc_equations, details.omc_variables
        ));
    }
    std::fs::write(path, lines.join("\n"))
        .with_context(|| format!("failed to write '{}'", path.display()))
}

fn write_timing_snapshot(
    paths: &MslPaths,
    omc_payload: &Value,
    rumoca_payload: &Value,
) -> Result<()> {
    let omc_timing = omc_payload
        .get("timing")
        .cloned()
        .unwrap_or_else(|| json!({}));
    let rumoca_timing = rumoca_payload
        .get("timings")
        .cloned()
        .unwrap_or_else(|| json!({}));
    let omc_compile_batches = parse_f64(omc_payload.get("elapsed_seconds"))
        .or_else(|| sum_batch_elapsed_seconds(omc_timing.get("batch_details")));
    let omc_discover = parse_f64(omc_timing.get("discover_seconds"));
    let omc_end_to_end = match (omc_compile_batches, omc_discover) {
        (Some(compile), Some(discover)) => Some(compile + discover),
        (Some(compile), None) => Some(compile),
        _ => None,
    };
    let omc_processed = omc_payload
        .get("processed")
        .or_else(|| omc_payload.get("total_models"))
        .and_then(Value::as_u64)
        .unwrap_or(0);
    let omc_throughput = omc_end_to_end.and_then(|elapsed| {
        if elapsed > 0.0 {
            Some(omc_processed as f64 / elapsed)
        } else {
            None
        }
    });
    let rumoca_total = rumoca_payload
        .get("total_models")
        .and_then(Value::as_u64)
        .unwrap_or(0);
    let rumoca_core = parse_f64(rumoca_timing.get("core_pipeline_seconds"));
    let rumoca_throughput = rumoca_core.and_then(|elapsed| {
        if elapsed > 0.0 {
            Some(rumoca_total as f64 / elapsed)
        } else {
            None
        }
    });
    let speedup = match (rumoca_throughput, omc_throughput) {
        (Some(rumoca), Some(omc)) if omc > 0.0 => Some(rumoca / omc),
        _ => None,
    };
    let snapshot = json!({
        "generated_at_unix_seconds": crate::common::unix_timestamp_seconds(),
        "notes": "Direct timing snapshot from current result artifacts.",
        "omc": {
            "total_models": omc_payload.get("total_models"),
            "processed_models": omc_processed,
            "successful_models": omc_payload.get("successful"),
            "workers_used": omc_timing.get("workers_used"),
            "batch_size_requested": omc_timing.get("batch_size_requested"),
            "batch_size_effective": omc_timing.get("batch_size_effective"),
            "discover_seconds": omc_discover,
            "compile_batches_seconds": omc_compile_batches,
            "end_to_end_seconds": omc_end_to_end,
            "models_per_second_end_to_end": omc_throughput,
        },
        "rumoca": {
            "total_models": rumoca_payload.get("total_models"),
            "compiled_models": rumoca_payload.get("compiled_models"),
            "parse_seconds": rumoca_timing.get("parse_seconds"),
            "session_build_seconds": rumoca_timing.get("session_build_seconds"),
            "frontend_compile_seconds": rumoca_timing.get("frontend_compile_seconds"),
            "compile_seconds": rumoca_timing.get("compile_seconds"),
            "core_pipeline_seconds": rumoca_timing.get("core_pipeline_seconds"),
            "models_per_second_core_pipeline": rumoca_throughput,
        },
        "comparison": {
            "rumoca_core_vs_omc_end_to_end_speedup_x": speedup,
            "model_count_note": "OMC and rumoca model universes can differ; ratio is directional.",
        },
    });
    let json_path = paths.results_dir.join("timing_comparison_latest.json");
    write_pretty_json(&json_path, &snapshot)?;
    let markdown = timing_snapshot_markdown(&snapshot);
    let md_path = paths.results_dir.join("timing_comparison_latest.md");
    std::fs::write(&md_path, markdown)
        .with_context(|| format!("failed to write '{}'", md_path.display()))?;
    println!("Timing snapshot written to {}", json_path.display());
    println!("Timing summary markdown written to {}", md_path.display());
    Ok(())
}

fn sum_batch_elapsed_seconds(batch_details: Option<&Value>) -> Option<f64> {
    let details = batch_details?.as_array()?;
    let mut total = 0.0;
    let mut seen = false;
    for batch in details {
        if batch
            .get("skipped")
            .and_then(Value::as_bool)
            .unwrap_or(false)
        {
            continue;
        }
        let Some(elapsed) = parse_f64(batch.get("elapsed_seconds")) else {
            continue;
        };
        total += elapsed;
        seen = true;
    }
    seen.then_some(total)
}

fn parse_f64(value: Option<&Value>) -> Option<f64> {
    match value {
        Some(Value::Number(number)) => number.as_f64().filter(|value| value.is_finite()),
        Some(Value::String(text)) => text.parse::<f64>().ok().filter(|value| value.is_finite()),
        _ => None,
    }
}

fn parse_usize(value: Option<&Value>) -> Option<usize> {
    value
        .and_then(Value::as_u64)
        .and_then(|value| usize::try_from(value).ok())
}

fn parse_i64(value: Option<&Value>) -> Option<i64> {
    value.and_then(Value::as_i64)
}

fn timing_snapshot_markdown(snapshot: &Value) -> String {
    let mut lines = Vec::new();
    lines.push("# MSL Timing Snapshot".to_string());
    lines.push(String::new());
    lines.push(format!(
        "Generated (unix seconds): {}",
        snapshot
            .get("generated_at_unix_seconds")
            .and_then(Value::as_i64)
            .unwrap_or(0)
    ));
    lines.push(String::new());
    lines.push("## OMC".to_string());
    lines.push(format!(
        "- Models processed: {:?}",
        snapshot
            .get("omc")
            .and_then(|omc| omc.get("processed_models"))
    ));
    lines.push(format!(
        "- End-to-end seconds: {:?}",
        snapshot
            .get("omc")
            .and_then(|omc| omc.get("end_to_end_seconds"))
    ));
    lines.push(String::new());
    lines.push("## Rumoca".to_string());
    lines.push(format!(
        "- Models total: {:?}",
        snapshot
            .get("rumoca")
            .and_then(|rumoca| rumoca.get("total_models"))
    ));
    lines.push(format!(
        "- Core pipeline seconds: {:?}",
        snapshot
            .get("rumoca")
            .and_then(|rumoca| rumoca.get("core_pipeline_seconds"))
    ));
    lines.push(String::new());
    lines.push("## Comparison".to_string());
    lines.push(format!(
        "- Throughput speedup: {:?}",
        snapshot
            .get("comparison")
            .and_then(|comparison| comparison.get("rumoca_core_vs_omc_end_to_end_speedup_x"))
    ));
    lines.push(String::new());
    lines.join("\n")
}
