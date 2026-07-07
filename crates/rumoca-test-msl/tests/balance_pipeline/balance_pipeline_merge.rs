//! Fan-in "merge and gate" entry for sharded MSL parity runs.
//!
//! A sharded run (`verify msl-parity --shard m/n`) produces N partial
//! `msl_results.json` files, one per shard, each of which skips the aggregate
//! quality ratchet. This module loads those partials, concatenates them into the
//! full result set, recomputes the run-wide aggregates by reusing the normal
//! summary builder ([`finalize_msl_summary_from_results`]), and then runs the
//! quality gate ONCE on the merged results — the job the un-sharded gate used to
//! do inline. Almost everything here is reuse; the only new logic is loading and
//! concatenating the shard files.

use super::*;
use serde_json::{Map, Value, json};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tempfile::tempdir;

const SHARD_OMC_REFERENCE_FILE: &str = "omc_simulation_reference.json";
const SHARD_TRACE_COMPARISON_FILE: &str = "sim_trace_comparison.json";

/// Concatenate per-shard summaries into one full-set summary, reusing the normal
/// aggregate builder. Pure (no I/O), so the merge arithmetic is unit-testable.
///
/// The per-model `model_results` and `sim_target_models` are disjoint stripes
/// that tile the full set, so they concatenate. `total_models` is per-shard →
/// summed. `total_mo_files` / `parse_errors` / `class_type_counts` are full-set
/// discovery values (discovery runs before striping) identical across shards →
/// taken from the first shard and asserted equal.
fn merge_shard_summaries(shards: Vec<MslSummary>) -> Result<MslSummary, String> {
    let Some(first) = shards.first() else {
        return Err("merge-shards: no shard summaries provided".to_string());
    };
    let total_mo_files = first.total_mo_files;
    let parse_errors = first.parse_errors;
    let class_type_counts = first.class_type_counts.clone();

    let mut total_models = 0usize;
    for (idx, shard) in shards.iter().enumerate() {
        if shard.total_mo_files != total_mo_files || shard.parse_errors != parse_errors {
            return Err(format!(
                "merge-shards: shard {idx} discovery mismatch \
                 (total_mo_files {} vs {}, parse_errors {} vs {}) — \
                 shards must come from one source root",
                shard.total_mo_files, total_mo_files, shard.parse_errors, parse_errors
            ));
        }
        total_models += shard.total_models;
    }

    let mut model_results = Vec::new();
    let mut sim_target_models = Vec::new();
    for shard in shards {
        model_results.extend(shard.model_results);
        sim_target_models.extend(shard.sim_target_models);
    }

    let inputs = MslSummaryInputs {
        total_mo_files,
        parse_errors,
        total_models,
        class_type_counts,
    };
    Ok(finalize_msl_summary_from_results(
        model_results,
        sim_target_models,
        inputs,
        MslPhaseTimings::default(),
        Instant::now(),
    ))
}

/// Load every `<dir>/shard-*/msl_results.json` into an [`MslSummary`], ordered by
/// shard directory name for determinism.
fn list_shard_dirs(dir: &Path) -> Result<Vec<PathBuf>, String> {
    let mut shard_dirs: Vec<PathBuf> = fs::read_dir(dir)
        .map_err(|e| format!("merge-shards: cannot read {}: {e}", dir.display()))?
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .filter(|path| {
            path.is_dir()
                && path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name.starts_with("shard-"))
        })
        .collect();
    shard_dirs.sort();
    if shard_dirs.is_empty() {
        return Err(format!(
            "merge-shards: no shard-*/ directories under {}",
            dir.display()
        ));
    }
    Ok(shard_dirs)
}

/// Load every `<dir>/shard-*/msl_results.json` into an [`MslSummary`], ordered by
/// shard directory name for determinism.
fn load_shard_summaries(dir: &Path) -> Result<Vec<MslSummary>, String> {
    let shard_dirs = list_shard_dirs(dir)?;
    let mut summaries = Vec::with_capacity(shard_dirs.len());
    for shard_dir in shard_dirs {
        let file = shard_dir.join("msl_results.json");
        let raw = fs::read_to_string(&file)
            .map_err(|e| format!("merge-shards: cannot read {}: {e}", file.display()))?;
        let summary: MslSummary = serde_json::from_str(&raw)
            .map_err(|e| format!("merge-shards: invalid {}: {e}", file.display()))?;
        println!(
            "Loaded {} ({} model results)",
            shard_dir.display(),
            summary.model_results.len()
        );
        summaries.push(summary);
    }
    Ok(summaries)
}

fn read_shard_json(shard_dir: &Path, file_name: &str) -> Result<Value, String> {
    let file = shard_dir.join(file_name);
    let raw = fs::read_to_string(&file)
        .map_err(|e| format!("merge-shards: cannot read {}: {e}", file.display()))?;
    serde_json::from_str(&raw).map_err(|e| format!("merge-shards: invalid {}: {e}", file.display()))
}

fn load_shard_json_payloads(shard_dirs: &[PathBuf], file_name: &str) -> Result<Vec<Value>, String> {
    let mut payloads = Vec::with_capacity(shard_dirs.len());
    for shard_dir in shard_dirs {
        let payload = read_shard_json(shard_dir, file_name)?;
        println!("Loaded {}", shard_dir.join(file_name).display());
        payloads.push(payload);
    }
    Ok(payloads)
}

fn value_at<'a>(value: &'a Value, path: &[&str]) -> Option<&'a Value> {
    let mut current = value;
    for key in path {
        current = current.get(*key)?;
    }
    Some(current)
}

fn object_at<'a>(
    value: &'a Value,
    path: &[&str],
    label: &str,
) -> Result<&'a Map<String, Value>, String> {
    value_at(value, path)
        .and_then(Value::as_object)
        .ok_or_else(|| format!("merge-shards: {label} must be an object"))
}

fn merge_required_object_maps(
    payloads: &[Value],
    path: &[&str],
    label: &str,
) -> Result<Map<String, Value>, String> {
    let mut merged = Map::new();
    for (shard_index, payload) in payloads.iter().enumerate() {
        let object = object_at(payload, path, label)?;
        for (key, value) in object {
            if merged.insert(key.clone(), value.clone()).is_some() {
                return Err(format!(
                    "merge-shards: duplicate {label} entry '{key}' in shard {}",
                    shard_index + 1
                ));
            }
        }
    }
    Ok(merged)
}

fn merge_optional_object_maps(
    payloads: &[Value],
    path: &[&str],
    label: &str,
) -> Result<Map<String, Value>, String> {
    let mut merged = Map::new();
    for (shard_index, payload) in payloads.iter().enumerate() {
        let Some(value) = value_at(payload, path) else {
            continue;
        };
        let Some(object) = value.as_object() else {
            return Err(format!("merge-shards: {label} must be an object"));
        };
        for (key, value) in object {
            if merged.insert(key.clone(), value.clone()).is_some() {
                return Err(format!(
                    "merge-shards: duplicate {label} entry '{key}' in shard {}",
                    shard_index + 1
                ));
            }
        }
    }
    Ok(merged)
}

fn json_usize(value: &Value, path: &[&str]) -> Option<usize> {
    value_at(value, path)?
        .as_u64()
        .and_then(|value| usize::try_from(value).ok())
}

fn json_f64(value: &Value, path: &[&str]) -> Option<f64> {
    value_at(value, path)?
        .as_f64()
        .filter(|value| value.is_finite())
}

fn sum_required_usize(values: &[&Value], path: &[&str], label: &str) -> Result<usize, String> {
    values
        .iter()
        .map(|value| {
            json_usize(value, path).ok_or_else(|| format!("merge-shards: missing numeric {label}"))
        })
        .sum()
}

fn sum_required_f64(values: &[&Value], path: &[&str], label: &str) -> Result<f64, String> {
    values
        .iter()
        .map(|value| {
            json_f64(value, path).ok_or_else(|| format!("merge-shards: missing numeric {label}"))
        })
        .sum()
}

fn sum_optional_usize(values: &[&Value], path: &[&str]) -> Option<usize> {
    let mut found = false;
    let total = values
        .iter()
        .filter_map(|value| {
            let item = json_usize(value, path)?;
            found = true;
            Some(item)
        })
        .sum();
    found.then_some(total)
}

fn sum_optional_f64(values: &[&Value], path: &[&str]) -> Option<f64> {
    let mut found = false;
    let total = values
        .iter()
        .filter_map(|value| {
            let item = json_f64(value, path)?;
            found = true;
            Some(item)
        })
        .sum();
    found.then_some(total)
}

fn percent(count: usize, total: usize) -> f64 {
    if total == 0 {
        return 0.0;
    }
    count as f64 * 100.0 / total as f64
}

fn required_same_string(payloads: &[Value], key: &str) -> Result<String, String> {
    let mut seen: Option<String> = None;
    for payload in payloads {
        let value = payload
            .get(key)
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|value| !value.is_empty())
            .ok_or_else(|| format!("merge-shards: missing {key} in shard parity JSON"))?;
        if let Some(previous) = seen.as_deref() {
            if previous != value {
                return Err(format!(
                    "merge-shards: mismatched {key} across shard parity JSONs ({previous} vs {value})"
                ));
            }
        } else {
            seen = Some(value.to_string());
        }
    }
    seen.ok_or_else(|| format!("merge-shards: no shard parity JSONs for {key}"))
}

fn optional_same_usize(payloads: &[Value], path: &[&str]) -> Option<usize> {
    let mut seen = None;
    for payload in payloads {
        let Some(value) = json_usize(payload, path) else {
            continue;
        };
        if let Some(previous) = seen {
            if previous != value {
                return None;
            }
        } else {
            seen = Some(value);
        }
    }
    seen
}

fn round3(value: f64) -> f64 {
    (value * 1000.0).round() / 1000.0
}

fn finite_positive(value: Option<f64>) -> Option<f64> {
    value.filter(|value| value.is_finite() && *value > 0.0)
}

fn model_status(model: &Value) -> Option<&str> {
    model.get("status").and_then(Value::as_str)
}

fn model_rumoca_status(model: &Value) -> Option<&str> {
    model.get("rumoca_status").and_then(Value::as_str)
}

fn rumoca_runtime_sim_seconds(model: &Value) -> Option<f64> {
    json_f64(model, &["rumoca_sim_run_seconds"])
        .or_else(|| json_f64(model, &["rumoca_sim_seconds"]))
}

fn runtime_pair(rumoca: Option<f64>, omc: Option<f64>) -> Option<(f64, f64)> {
    let rumoca = finite_positive(rumoca)?;
    let omc = finite_positive(omc)?;
    Some((omc, rumoca))
}

fn runtime_ratio_stats(pairs: impl Iterator<Item = (f64, f64)>) -> Option<Value> {
    let mut ratios = Vec::new();
    let mut omc_sum = 0.0_f64;
    let mut rumoca_sum = 0.0_f64;
    for (omc, rumoca) in pairs {
        let ratio = omc / rumoca;
        if !ratio.is_finite() {
            continue;
        }
        ratios.push(ratio);
        omc_sum += omc;
        rumoca_sum += rumoca;
    }
    if ratios.is_empty() || !rumoca_sum.is_finite() || rumoca_sum <= 0.0 {
        return None;
    }
    ratios.sort_by(|a, b| a.total_cmp(b));
    let sample_count = ratios.len();
    let median_ratio = if sample_count.is_multiple_of(2) {
        (ratios[sample_count / 2 - 1] + ratios[sample_count / 2]) / 2.0
    } else {
        ratios[sample_count / 2]
    };
    let mean_ratio = ratios.iter().sum::<f64>() / sample_count as f64;
    Some(json!({
        "sample_count": sample_count,
        "aggregate_ratio": omc_sum / rumoca_sum,
        "min_ratio": ratios[0],
        "median_ratio": median_ratio,
        "mean_ratio": mean_ratio,
        "max_ratio": ratios[sample_count - 1],
    }))
}

fn runtime_ratio_bucket(
    models: &Map<String, Value>,
    wall: bool,
    both_success_only: bool,
) -> Option<Value> {
    runtime_ratio_stats(models.values().filter_map(|model| {
        if both_success_only
            && (model_status(model) != Some("success")
                || model_rumoca_status(model) != Some("sim_ok"))
        {
            return None;
        }
        if wall {
            runtime_pair(
                json_f64(model, &["rumoca_sim_wall_seconds"]),
                json_f64(model, &["omc_wall_seconds"]),
            )
        } else {
            runtime_pair(
                rumoca_runtime_sim_seconds(model),
                json_f64(model, &["sim_system_seconds"]),
            )
        }
    }))
}

fn sum_model_seconds(
    models: &Map<String, Value>,
    field: &str,
    required_status: Option<(&str, &str)>,
) -> f64 {
    models
        .values()
        .filter(|model| {
            required_status.is_none_or(|(key, expected)| {
                model.get(key).and_then(Value::as_str) == Some(expected)
            })
        })
        .filter_map(|model| json_f64(model, &[field]))
        .sum()
}

fn sum_rumoca_runtime_seconds(models: &Map<String, Value>) -> f64 {
    models
        .values()
        .filter(|model| model_rumoca_status(model) == Some("sim_ok"))
        .filter_map(rumoca_runtime_sim_seconds)
        .sum()
}

fn build_runtime_comparison(models: &Map<String, Value>) -> Value {
    json!({
        "ratio_definition": "omc_over_rumoca_higher_is_better (simulation/wall runtime; this is the SIM-time comparison, distinct from the compile-speed `speedup` in msl_speed_comparison.json)",
        "ratio_metric_system": "omc_timeSimulation_over_rumoca_sim_seconds",
        "ratio_metric_wall": "omc_external_wall_over_rumoca_external_wall",
        "omc_wall_metric_note": "omc_wall_seconds is the direct per-model wall time measured around the simulate() request in a warm session (one-time MSL load excluded)",
        "total_omc_sim_system_seconds": round3(sum_model_seconds(models, "sim_system_seconds", Some(("status", "success")))),
        "total_omc_total_system_seconds": round3(sum_model_seconds(models, "total_system_seconds", Some(("status", "success")))),
        "total_omc_wall_seconds": round3(sum_model_seconds(models, "omc_wall_seconds", Some(("status", "success")))),
        "total_rumoca_sim_seconds": round3(sum_rumoca_runtime_seconds(models)),
        "total_rumoca_sim_build_seconds": round3(sum_model_seconds(models, "rumoca_sim_build_seconds", Some(("rumoca_status", "sim_ok")))),
        "total_rumoca_sim_run_seconds": round3(sum_model_seconds(models, "rumoca_sim_run_seconds", Some(("rumoca_status", "sim_ok")))),
        "total_rumoca_sim_wall_seconds": round3(sum_model_seconds(models, "rumoca_sim_wall_seconds", Some(("rumoca_status", "sim_ok")))),
        "ratio_stats": {
            "system_ratio_all_positive": runtime_ratio_bucket(models, false, false),
            "system_ratio_both_success": runtime_ratio_bucket(models, false, true),
            "wall_ratio_all_positive": runtime_ratio_bucket(models, true, false),
            "wall_ratio_both_success": runtime_ratio_bucket(models, true, true),
        },
    })
}

fn merge_initial_condition_summary(trace_values: &[&Value]) -> Result<Value, String> {
    let initial_values = trace_values
        .iter()
        .map(|trace| {
            value_at(trace, &["initial_condition"]).ok_or_else(|| {
                "merge-shards: missing trace_comparison.initial_condition".to_string()
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let models_compared = sum_required_usize(
        &initial_values,
        &["models_compared"],
        "initial_condition.models_compared",
    )?;
    let accurate = sum_required_usize(
        &initial_values,
        &["models_with_accurate_initial_conditions"],
        "initial_condition.models_with_accurate_initial_conditions",
    )?;
    let deviated = sum_required_usize(
        &initial_values,
        &["models_with_initial_condition_deviation"],
        "initial_condition.models_with_initial_condition_deviation",
    )?;
    let total_channels = sum_required_usize(
        &initial_values,
        &["total_channels_compared"],
        "initial_condition.total_channels_compared",
    )?;
    let high = sum_required_usize(
        &initial_values,
        &["high_channels_total"],
        "initial_condition.high_channels_total",
    )?;
    let near = sum_required_usize(
        &initial_values,
        &["near_channels_total"],
        "initial_condition.near_channels_total",
    )?;
    let deviation = sum_required_usize(
        &initial_values,
        &["deviation_channels_total"],
        "initial_condition.deviation_channels_total",
    )?;
    let severe = sum_required_usize(
        &initial_values,
        &["severe_channels_total"],
        "initial_condition.severe_channels_total",
    )?;
    let violation_mass = sum_required_f64(
        &initial_values,
        &["violation_mass_total"],
        "initial_condition.violation_mass_total",
    )?;
    Ok(json!({
        "models_compared": models_compared,
        "models_with_accurate_initial_conditions": accurate,
        "models_with_initial_condition_deviation": deviated,
        "accurate_initial_conditions_percent": percent(accurate, models_compared),
        "models_with_initial_condition_deviation_percent": percent(deviated, models_compared),
        "total_channels_compared": total_channels,
        "high_channels_total": high,
        "near_channels_total": near,
        "deviation_channels_total": deviation,
        "severe_channels_total": severe,
        "high_channels_percent": percent(high, total_channels),
        "near_channels_percent": percent(near, total_channels),
        "deviation_channels_percent": percent(deviation, total_channels),
        "severe_channels_percent": percent(severe, total_channels),
        "violation_mass_total": violation_mass,
        "violation_mass_mean_per_model": if models_compared == 0 { 0.0 } else { violation_mass / models_compared as f64 },
        "violation_mass_mean_per_channel": if total_channels == 0 { 0.0 } else { violation_mass / total_channels as f64 },
        "mean_channel_bounded_normalized_error": weighted_average_optional(
            &initial_values,
            "mean_channel_bounded_normalized_error",
            "total_channels_compared"
        ),
        "max_channel_bounded_normalized_error": max_optional_f64(
            &initial_values,
            &["max_channel_bounded_normalized_error"]
        ),
    }))
}

fn merge_state_selection_summary(trace_values: &[&Value]) -> Result<Value, String> {
    let state_values = trace_values
        .iter()
        .map(|trace| {
            value_at(trace, &["state_selection"])
                .ok_or_else(|| "merge-shards: missing trace_comparison.state_selection".to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;
    let models_compared = sum_required_usize(
        &state_values,
        &["models_compared"],
        "state_selection.models_compared",
    )?;
    let exact = sum_required_usize(
        &state_values,
        &["exact_state_set_match_models"],
        "state_selection.exact_state_set_match_models",
    )?;
    let count_match = sum_required_usize(
        &state_values,
        &["state_count_match_models"],
        "state_selection.state_count_match_models",
    )?;
    Ok(json!({
        "models_compared": models_compared,
        "exact_state_set_match_models": exact,
        "state_count_match_models": count_match,
        "exact_state_set_match_percent": percent(exact, models_compared),
        "state_count_match_percent": percent(count_match, models_compared),
        "total_rumoca_states": sum_required_usize(&state_values, &["total_rumoca_states"], "state_selection.total_rumoca_states")?,
        "total_omc_states": sum_required_usize(&state_values, &["total_omc_states"], "state_selection.total_omc_states")?,
        "total_matching_states": sum_required_usize(&state_values, &["total_matching_states"], "state_selection.total_matching_states")?,
        "total_rumoca_only_states": sum_required_usize(&state_values, &["total_rumoca_only_states"], "state_selection.total_rumoca_only_states")?,
        "total_omc_only_states": sum_required_usize(&state_values, &["total_omc_only_states"], "state_selection.total_omc_only_states")?,
        "max_model_state_set_difference": max_required_usize(&state_values, &["max_model_state_set_difference"], "state_selection.max_model_state_set_difference")?,
    }))
}

fn max_required_usize(values: &[&Value], path: &[&str], label: &str) -> Result<usize, String> {
    values
        .iter()
        .map(|value| {
            json_usize(value, path).ok_or_else(|| format!("merge-shards: missing numeric {label}"))
        })
        .collect::<Result<Vec<_>, _>>()
        .map(|values| values.into_iter().max().unwrap_or(0))
}

fn max_optional_f64(values: &[&Value], path: &[&str]) -> Option<f64> {
    values
        .iter()
        .filter_map(|value| json_f64(value, path))
        .max_by(f64::total_cmp)
}

fn weighted_average_optional(values: &[&Value], value_key: &str, weight_key: &str) -> Option<f64> {
    let mut weighted_sum = 0.0_f64;
    let mut weight_sum = 0usize;
    for value in values {
        let Some(item) = json_f64(value, &[value_key]) else {
            continue;
        };
        let Some(weight) = json_usize(value, &[weight_key]) else {
            continue;
        };
        weighted_sum += item * weight as f64;
        weight_sum += weight;
    }
    (weight_sum > 0).then_some(weighted_sum / weight_sum as f64)
}

fn metric_distribution(values: impl Iterator<Item = f64>) -> Option<Value> {
    let mut values = values.filter(|value| value.is_finite()).collect::<Vec<_>>();
    if values.is_empty() {
        return None;
    }
    values.sort_by(f64::total_cmp);
    let sample_count = values.len();
    let median = if sample_count.is_multiple_of(2) {
        (values[sample_count / 2 - 1] + values[sample_count / 2]) / 2.0
    } else {
        values[sample_count / 2]
    };
    let mean = values.iter().sum::<f64>() / sample_count as f64;
    Some(json!({
        "sample_count": sample_count,
        "min": values[0],
        "median": median,
        "mean": mean,
        "max": values[sample_count - 1],
    }))
}

fn trace_model_distribution(models: &Map<String, Value>, key: &str) -> Option<Value> {
    metric_distribution(models.values().filter_map(|model| json_f64(model, &[key])))
}

fn copy_distribution_fields(
    target: &mut Map<String, Value>,
    distribution: Option<&Value>,
    prefix: &str,
) {
    let Some(distribution) = distribution else {
        return;
    };
    for key in ["min", "median", "mean", "max"] {
        if let Some(value) = distribution.get(key) {
            target.insert(format!("{key}_{prefix}"), value.clone());
        }
    }
}

struct MergedTraceCounts {
    models_compared: usize,
    missing_trace: usize,
    skipped: usize,
    high: usize,
    minor: usize,
    deviation: usize,
    total_channels: usize,
    bad_channels: usize,
    severe_channels: usize,
    violation_mass: f64,
    models_with_any_deviation: usize,
    models_with_bad_channel: usize,
    models_with_severe_channel: usize,
}

fn shard_trace_values(omc_payloads: &[Value]) -> Result<Vec<&Value>, String> {
    omc_payloads
        .iter()
        .map(|payload| {
            value_at(payload, &["trace_comparison"]).ok_or_else(|| {
                "merge-shards: missing trace_comparison in shard OMC reference".to_string()
            })
        })
        .collect()
}

fn collect_flat_trace_counts(trace_values: &[&Value]) -> Result<MergedTraceCounts, String> {
    let models_compared = sum_required_usize(
        trace_values,
        &["models_compared"],
        "trace_comparison.models_compared",
    )?;
    let models_with_any_deviation = sum_required_usize(
        trace_values,
        &["models_with_any_channel_deviation"],
        "trace_comparison.models_with_any_channel_deviation",
    )?;
    Ok(MergedTraceCounts {
        models_compared,
        missing_trace: sum_required_usize(
            trace_values,
            &["missing_trace_models"],
            "trace_comparison.missing_trace_models",
        )?,
        skipped: sum_required_usize(
            trace_values,
            &["skipped_models"],
            "trace_comparison.skipped_models",
        )?,
        high: sum_required_usize(
            trace_values,
            &["agreement_high"],
            "trace_comparison.agreement_high",
        )?,
        minor: trace_minor_count(trace_values)?,
        deviation: sum_required_usize(
            trace_values,
            &["agreement_deviation"],
            "trace_comparison.agreement_deviation",
        )?,
        total_channels: sum_required_usize(
            trace_values,
            &["total_channels_compared"],
            "trace_comparison.total_channels_compared",
        )?,
        bad_channels: sum_required_usize(
            trace_values,
            &["bad_channels_total"],
            "trace_comparison.bad_channels_total",
        )?,
        severe_channels: sum_required_usize(
            trace_values,
            &["severe_channels_total"],
            "trace_comparison.severe_channels_total",
        )?,
        violation_mass: sum_required_f64(
            trace_values,
            &["violation_mass_total"],
            "trace_comparison.violation_mass_total",
        )?,
        models_with_bad_channel: sum_optional_usize(trace_values, &["models_with_bad_channel"])
            .unwrap_or(models_with_any_deviation),
        models_with_severe_channel: sum_required_usize(
            trace_values,
            &["models_with_severe_channel"],
            "trace_comparison.models_with_severe_channel",
        )?,
        models_with_any_deviation,
    })
}

fn trace_minor_count(trace_values: &[&Value]) -> Result<usize, String> {
    sum_required_usize(
        trace_values,
        &["agreement_minor"],
        "trace_comparison.agreement_minor",
    )
    .or_else(|_| {
        sum_required_usize(
            trace_values,
            &["agreement_near"],
            "trace_comparison.agreement_near",
        )
    })
}

fn build_base_flat_trace_summary(
    counts: &MergedTraceCounts,
    trace_values: &[&Value],
) -> Result<Value, String> {
    let no_severe = counts
        .models_compared
        .saturating_sub(counts.models_with_severe_channel);
    Ok(json!({
        "report_file": msl_results_dir().join(SHARD_TRACE_COMPARISON_FILE).display().to_string(),
        "models_compared": counts.models_compared,
        "missing_trace_models": counts.missing_trace,
        "skipped_models": counts.skipped,
        "agreement_high": counts.high,
        "agreement_high_percent": percent(counts.high, counts.models_compared),
        "agreement_near": counts.minor,
        "agreement_minor": counts.minor,
        "agreement_near_percent": percent(counts.minor, counts.models_compared),
        "agreement_minor_percent": percent(counts.minor, counts.models_compared),
        "agreement_deviation": counts.deviation,
        "agreement_deviation_percent": percent(counts.deviation, counts.models_compared),
        "high_plus_near_models": counts.high + counts.minor,
        "high_plus_near_percent": percent(counts.high + counts.minor, counts.models_compared),
        "total_channels_compared": counts.total_channels,
        "bad_channels_total": counts.bad_channels,
        "bad_channels_percent": percent(counts.bad_channels, counts.total_channels),
        "severe_channels_total": counts.severe_channels,
        "severe_channels_percent": percent(counts.severe_channels, counts.total_channels),
        "violation_mass_total": counts.violation_mass,
        "violation_mass_mean_per_model": mean_over_count(counts.violation_mass, counts.models_compared),
        "violation_mass_mean_per_channel": mean_over_count(counts.violation_mass, counts.total_channels),
        "models_with_bad_channel": counts.models_with_bad_channel,
        "models_with_severe_channel": counts.models_with_severe_channel,
        "models_with_no_severe_channel": no_severe,
        "models_with_no_severe_channel_percent": percent(no_severe, counts.models_compared),
        "models_with_any_channel_deviation": counts.models_with_any_deviation,
        "models_with_any_channel_deviation_percent": percent(counts.models_with_any_deviation, counts.models_compared),
        "max_model_channel_deviation_percent": max_optional_f64(trace_values, &["max_model_channel_deviation_percent"]).unwrap_or(0.0),
        "initial_condition": merge_initial_condition_summary(trace_values)?,
        "state_selection": merge_state_selection_summary(trace_values)?,
    }))
}

fn mean_over_count(value: f64, count: usize) -> f64 {
    if count == 0 {
        0.0
    } else {
        value / count as f64
    }
}

fn append_trace_distribution_fields(summary: &mut Value, trace_models: &Map<String, Value>) {
    let score_distribution = trace_model_distribution(trace_models, "bounded_normalized_l1_score");
    let mean_channel_distribution =
        trace_model_distribution(trace_models, "mean_channel_bounded_normalized_l1");
    let max_channel_distribution =
        trace_model_distribution(trace_models, "max_channel_bounded_normalized_l1");
    let root = summary
        .as_object_mut()
        .expect("trace summary payload should be an object");
    copy_distribution_fields(
        root,
        score_distribution.as_ref(),
        "model_bounded_normalized_l1",
    );
    copy_distribution_fields(
        root,
        score_distribution.as_ref(),
        "model_score_bounded_normalized_l1",
    );
    copy_distribution_fields(
        root,
        mean_channel_distribution.as_ref(),
        "model_mean_channel_bounded_normalized_l1",
    );
    copy_distribution_fields(
        root,
        max_channel_distribution.as_ref(),
        "model_max_channel_bounded_normalized_l1",
    );
    append_trace_distribution_aliases(root, mean_channel_distribution, max_channel_distribution);
}

fn append_trace_distribution_aliases(
    root: &mut Map<String, Value>,
    mean_channel_distribution: Option<Value>,
    max_channel_distribution: Option<Value>,
) {
    if let Some(distribution) = mean_channel_distribution.as_ref()
        && let Some(mean) = distribution.get("mean")
    {
        root.insert(
            "mean_model_mean_channel_bounded_normalized_l1".to_string(),
            mean.clone(),
        );
    }
    if let Some(distribution) = max_channel_distribution.as_ref()
        && let Some(max) = distribution.get("max")
    {
        root.insert(
            "max_model_max_channel_bounded_normalized_l1".to_string(),
            max.clone(),
        );
        root.insert(
            "global_max_channel_bounded_normalized_l1".to_string(),
            max.clone(),
        );
    }
}

fn build_flat_trace_summary(
    omc_payloads: &[Value],
    trace_models: &Map<String, Value>,
) -> Result<Value, String> {
    let trace_values = shard_trace_values(omc_payloads)?;
    let counts = collect_flat_trace_counts(&trace_values)?;
    let mut summary = build_base_flat_trace_summary(&counts, &trace_values)?;
    append_trace_distribution_fields(&mut summary, trace_models);
    Ok(summary)
}

fn trace_summary_usize(summary: &Value, key: &str) -> usize {
    json_usize(summary, &[key]).unwrap_or(0)
}

fn trace_summary_f64(summary: &Value, key: &str) -> f64 {
    json_f64(summary, &[key]).unwrap_or(0.0)
}

fn build_trace_report_summary(summary: &Value) -> Value {
    let mut report_summary = Map::new();
    for key in [
        "min_model_bounded_normalized_l1",
        "mean_model_bounded_normalized_l1",
        "median_model_bounded_normalized_l1",
        "max_model_bounded_normalized_l1",
        "worst_model_bounded_normalized_l1",
        "mean_model_mean_channel_bounded_normalized_l1",
        "max_model_max_channel_bounded_normalized_l1",
        "global_max_channel_bounded_normalized_l1",
        "total_channels_compared",
        "bad_channels_total",
        "bad_channels_percent",
        "severe_channels_total",
        "severe_channels_percent",
        "models_with_bad_channel",
        "models_with_severe_channel",
        "models_with_any_channel_deviation",
        "models_with_any_channel_deviation_percent",
        "models_with_no_severe_channel",
        "models_with_no_severe_channel_percent",
        "violation_mass_total",
        "violation_mass_mean_per_model",
        "violation_mass_mean_per_channel",
        "initial_condition",
        "state_selection",
    ] {
        if let Some(value) = summary.get(key) {
            report_summary.insert(key.to_string(), value.clone());
        }
    }
    if let Some(max) = summary.get("max_model_bounded_normalized_l1") {
        report_summary.insert("worst_model_bounded_normalized_l1".to_string(), max.clone());
    }
    Value::Object(report_summary)
}

fn worst_trace_models(models: &Map<String, Value>) -> Vec<Value> {
    let mut values = models.values().cloned().collect::<Vec<_>>();
    values.sort_by(|lhs, rhs| {
        let lhs = json_f64(lhs, &["max_channel_bounded_normalized_l1"]).unwrap_or(0.0);
        let rhs = json_f64(rhs, &["max_channel_bounded_normalized_l1"]).unwrap_or(0.0);
        rhs.total_cmp(&lhs)
    });
    values.truncate(25);
    values
}

fn merge_trace_comparison_payloads(
    trace_payloads: &[Value],
    trace_models: Map<String, Value>,
    summary: &Value,
) -> Result<Value, String> {
    let missing_trace =
        merge_optional_object_maps(trace_payloads, &["missing_trace"], "missing_trace")?;
    let skipped = merge_optional_object_maps(trace_payloads, &["skipped"], "skipped")?;
    let mut payload = trace_payloads
        .first()
        .cloned()
        .ok_or_else(|| "merge-shards: no trace comparison payloads".to_string())?;
    let Some(root) = payload.as_object_mut() else {
        return Err("merge-shards: trace comparison payload must be an object".to_string());
    };
    root.insert("models".to_string(), Value::Object(trace_models.clone()));
    root.insert("missing_trace".to_string(), Value::Object(missing_trace));
    root.insert("skipped".to_string(), Value::Object(skipped));
    root.insert(
        "models_compared".to_string(),
        json!(trace_summary_usize(summary, "models_compared")),
    );
    root.insert(
        "missing_trace_models".to_string(),
        json!(trace_summary_usize(summary, "missing_trace_models")),
    );
    root.insert(
        "skipped_models".to_string(),
        json!(trace_summary_usize(summary, "skipped_models")),
    );
    root.insert(
        "agreement_bands".to_string(),
        json!({
            "high_agreement": trace_summary_usize(summary, "agreement_high"),
            "minor_agreement": trace_summary_usize(summary, "agreement_minor"),
            "deviation": trace_summary_usize(summary, "agreement_deviation"),
        }),
    );
    root.insert(
        "agreement_bands_percent".to_string(),
        json!({
            "high_agreement": trace_summary_f64(summary, "agreement_high_percent"),
            "minor_agreement": trace_summary_f64(summary, "agreement_minor_percent"),
            "deviation": trace_summary_f64(summary, "agreement_deviation_percent"),
        }),
    );
    root.insert("summary".to_string(), build_trace_report_summary(summary));
    root.insert(
        "worst_models".to_string(),
        json!(worst_trace_models(&trace_models)),
    );
    Ok(payload)
}

fn merge_timing_payload(omc_payloads: &[Value]) -> Value {
    let mut timing = omc_payloads
        .first()
        .and_then(|payload| payload.get("timing"))
        .cloned()
        .unwrap_or_else(|| json!({}));
    let Some(root) = timing.as_object_mut() else {
        return json!({});
    };
    for key in ["batches_total", "batches_ran", "batches_skipped"] {
        let values = omc_payloads
            .iter()
            .filter_map(|payload| json_usize(payload, &["timing", key]))
            .sum::<usize>();
        root.insert(key.to_string(), json!(values));
    }
    let workers = omc_payloads
        .iter()
        .filter_map(|payload| json_usize(payload, &["timing", "workers_used"]))
        .sum::<usize>();
    if workers > 0 {
        root.insert("workers_used".to_string(), json!(workers));
    }
    if let Some(omc_threads) = optional_same_usize(omc_payloads, &["timing", "omc_threads"]) {
        root.insert("omc_threads".to_string(), json!(omc_threads));
    }
    let batch_details = omc_payloads
        .iter()
        .filter_map(|payload| value_at(payload, &["timing", "batch_details"])?.as_array())
        .flatten()
        .cloned()
        .collect::<Vec<_>>();
    root.insert("batch_details".to_string(), Value::Array(batch_details));
    timing
}

fn merge_omc_assertion_failures(omc_payloads: &[Value]) -> Value {
    let model_count = omc_payloads
        .iter()
        .filter_map(|payload| json_usize(payload, &["omc_assertion_failures", "model_count"]))
        .sum::<usize>();
    let examples = omc_payloads
        .iter()
        .filter_map(|payload| {
            value_at(payload, &["omc_assertion_failures", "examples"])?.as_array()
        })
        .flatten()
        .cloned()
        .collect::<Vec<_>>();
    json!({
        "model_count": model_count,
        "examples": examples,
    })
}

fn merge_omc_reference_payloads(
    omc_payloads: &[Value],
    omc_models: Map<String, Value>,
    trace_summary: &Value,
) -> Result<Value, String> {
    let omc_version = required_same_string(omc_payloads, "omc_version")?;
    let msl_version = required_same_string(omc_payloads, "msl_version")?;
    let mut payload = omc_payloads
        .first()
        .cloned()
        .ok_or_else(|| "merge-shards: no OMC reference payloads".to_string())?;
    let Some(root) = payload.as_object_mut() else {
        return Err("merge-shards: OMC reference payload must be an object".to_string());
    };
    let total_models = omc_models.len();
    let sim_successful = omc_models
        .values()
        .filter(|model| model_status(model) == Some("success"))
        .count();
    let sim_failed = omc_models
        .values()
        .filter(|model| model_status(model) == Some("error"))
        .count();
    let sim_timed_out = omc_models
        .values()
        .filter(|model| model_status(model) == Some("timeout"))
        .count();
    root.insert("msl_version".to_string(), Value::String(msl_version));
    root.insert("omc_version".to_string(), Value::String(omc_version));
    root.insert("total_models".to_string(), json!(total_models));
    root.insert("processed".to_string(), json!(total_models));
    root.insert("sim_successful".to_string(), json!(sim_successful));
    root.insert("sim_failed".to_string(), json!(sim_failed));
    root.insert("sim_timed_out".to_string(), json!(sim_timed_out));
    root.insert(
        "simulation_success_rate_percent".to_string(),
        json!(percent(sim_successful, total_models)),
    );
    root.insert("timing".to_string(), merge_timing_payload(omc_payloads));
    root.insert(
        "runtime_comparison".to_string(),
        build_runtime_comparison(&omc_models),
    );
    root.insert("trace_comparison".to_string(), trace_summary.clone());
    root.insert(
        "omc_assertion_failures".to_string(),
        merge_omc_assertion_failures(omc_payloads),
    );
    root.insert("models".to_string(), Value::Object(omc_models));
    Ok(payload)
}

fn write_pretty_json_file(path: &Path, payload: &Value) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|e| format!("merge-shards: cannot create {}: {e}", parent.display()))?;
    }
    let json = serde_json::to_string_pretty(payload)
        .map_err(|e| format!("merge-shards: cannot serialize {}: {e}", path.display()))?;
    fs::write(path, json).map_err(|e| format!("merge-shards: cannot write {}: {e}", path.display()))
}

fn merge_shard_parity_artifacts(dir: &Path, results_dir: &Path) -> Result<(), String> {
    let shard_dirs = list_shard_dirs(dir)?;
    let omc_payloads = load_shard_json_payloads(&shard_dirs, SHARD_OMC_REFERENCE_FILE)?;
    let trace_payloads = load_shard_json_payloads(&shard_dirs, SHARD_TRACE_COMPARISON_FILE)?;
    let omc_models = merge_required_object_maps(&omc_payloads, &["models"], "OMC model")?;
    let trace_models = merge_required_object_maps(&trace_payloads, &["models"], "trace model")?;
    let trace_summary = build_flat_trace_summary(&omc_payloads, &trace_models)?;
    let merged_trace =
        merge_trace_comparison_payloads(&trace_payloads, trace_models, &trace_summary)?;
    let merged_omc = merge_omc_reference_payloads(&omc_payloads, omc_models, &trace_summary)?;

    write_pretty_json_file(&results_dir.join(SHARD_OMC_REFERENCE_FILE), &merged_omc)?;
    write_pretty_json_file(
        &results_dir.join(SHARD_TRACE_COMPARISON_FILE),
        &merged_trace,
    )?;
    println!(
        "Merged shard parity artifacts into {}",
        results_dir.display()
    );
    Ok(())
}

/// Fan-in entry: load the shard partials, merge them, and run the real quality
/// gate once on the merged full-set summary. Invoked by
/// `cargo xtask verify msl-parity --merge-shards <dir>` (which is a "full-shaped"
/// run so `should_skip_msl_quality_gate()` is false and the baseline ratchet
/// actually runs).
#[cfg(feature = "msl-full-test")]
#[test]
fn test_msl_merge_and_gate() {
    let dir = merge_shards_dir()
        .expect("test_msl_merge_and_gate requires --merge-shards <dir> in the parity config");
    let shards = load_shard_summaries(&dir).expect("load shard summaries");
    println!("Merging {} shards from {}", shards.len(), dir.display());
    let merged = merge_shard_summaries(shards).expect("merge shard summaries");
    println!(
        "Merged full set: {} models, {} sim targets, total_models {}",
        merged.model_results.len(),
        merged.sim_target_models.len(),
        merged.total_models
    );

    // Reuse the un-sharded write + validate + snapshot + gate sequence.
    write_msl_results(&merged).expect("write merged msl_results.json + reports");
    merge_shard_parity_artifacts(&dir, &msl_results_dir())
        .expect("write merged OMC parity artifacts");
    write_msl_package_trace_accuracy_report(&merged)
        .expect("write merged package trace accuracy report");
    assert_valid_msl_summary(&merged);
    if merged.sim_attempted > 0 {
        write_current_msl_quality_snapshot(&merged).expect("write merged quality snapshot");
    }
    enforce_msl_quality_gate(&merged).expect("merged MSL quality gate");
    println!(
        "Merged MSL quality gate passed ({} sim_ok / {} attempted)",
        merged.sim_ok, merged.sim_attempted
    );
}

#[cfg(test)]
fn model_result(name: &str) -> MslModelResult {
    let mut result = phase_error_result(name.to_string(), "Success", None, None);
    result.is_balanced = Some(true);
    result.initial_balance_ok = Some(true);
    result.ir_solve_file = Some(format!("ir_solve/{name}.json"));
    result.ic_status = Some("ic_ok".to_string());
    result.sim_status = Some("sim_ok".to_string());
    result
}

#[test]
fn merge_shard_summaries_concatenates_and_sums() {
    let mut a = empty_summary(100, 2);
    a.total_models = 3;
    a.sim_target_models = vec!["A".into(), "C".into()];
    a.model_results = vec![model_result("A"), model_result("C")];

    let mut b = empty_summary(100, 2);
    b.total_models = 2;
    b.sim_target_models = vec!["B".into()];
    b.model_results = vec![model_result("B")];

    let merged = merge_shard_summaries(vec![a, b]).expect("merge");
    assert_eq!(merged.total_models, 5, "total_models summed across shards");
    assert_eq!(
        merged.total_mo_files, 100,
        "full-set discovery carried over"
    );
    assert_eq!(merged.parse_errors, 2, "full-set discovery carried over");
    assert_eq!(
        merged.model_results.len(),
        3,
        "model_results concatenated (stripes tile the set)"
    );
    assert_eq!(merged.sim_target_models.len(), 3, "sim targets unioned");
    assert_eq!(merged.sim_attempted, 3, "sim statuses recomputed");
    assert_eq!(merged.sim_ok, 3, "sim successes recomputed");
}

#[test]
fn merge_shard_summaries_rejects_discovery_mismatch() {
    // Shards from different source roots (differing full-set discovery) must not
    // silently merge into a bogus aggregate.
    let a = empty_summary(100, 2);
    let b = empty_summary(999, 2);
    assert!(merge_shard_summaries(vec![a, b]).is_err());
    assert!(
        merge_shard_summaries(vec![]).is_err(),
        "empty input rejected"
    );
}

fn shard_summary(name: &str) -> MslSummary {
    let mut summary = empty_summary(100, 2);
    summary.total_models = 1;
    summary.sim_target_models = vec![name.to_string()];
    summary.model_results = vec![model_result(name)];
    summary
}

fn write_json_fixture(path: &Path, payload: &Value) {
    fs::write(
        path,
        serde_json::to_vec_pretty(payload).expect("serialize fixture JSON"),
    )
    .expect("write fixture JSON");
}

fn shard_timing_fixture() -> Value {
    json!({
        "workers_used": 3,
        "omc_threads": 1,
        "batches_total": 1,
        "batches_ran": 1,
        "batches_skipped": 0,
        "batch_details": []
    })
}

fn shard_ratio_stats_fixture() -> Value {
    json!({
        "sample_count": 1,
        "min_ratio": 2.0,
        "median_ratio": 2.0,
        "mean_ratio": 2.0,
        "max_ratio": 2.0
    })
}

fn shard_trace_initial_condition_fixture() -> Value {
    json!({
        "models_compared": 1,
        "models_with_accurate_initial_conditions": 1,
        "models_with_initial_condition_deviation": 0,
        "total_channels_compared": 1,
        "high_channels_total": 1,
        "near_channels_total": 0,
        "deviation_channels_total": 0,
        "severe_channels_total": 0,
        "violation_mass_total": 0.0,
        "mean_channel_bounded_normalized_error": 0.0,
        "max_channel_bounded_normalized_error": 0.0
    })
}

fn shard_trace_state_selection_fixture() -> Value {
    json!({
        "models_compared": 1,
        "exact_state_set_match_models": 1,
        "state_count_match_models": 1,
        "total_rumoca_states": 1,
        "total_omc_states": 1,
        "total_matching_states": 1,
        "total_rumoca_only_states": 0,
        "total_omc_only_states": 0,
        "max_model_state_set_difference": 0
    })
}

fn shard_flat_trace_summary_fixture() -> Value {
    json!({
        "models_compared": 1,
        "missing_trace_models": 0,
        "skipped_models": 0,
        "agreement_high": 1,
        "agreement_minor": 0,
        "agreement_deviation": 0,
        "total_channels_compared": 2,
        "bad_channels_total": 0,
        "severe_channels_total": 0,
        "violation_mass_total": 0.0,
        "models_with_any_channel_deviation": 0,
        "models_with_bad_channel": 0,
        "models_with_severe_channel": 0,
        "max_model_channel_deviation_percent": 0.0,
        "initial_condition": shard_trace_initial_condition_fixture(),
        "state_selection": shard_trace_state_selection_fixture()
    })
}

fn shard_omc_model_fixture() -> Value {
    json!({
        "status": "success",
        "sim_system_seconds": 2.0,
        "total_system_seconds": 5.0,
        "omc_wall_seconds": 3.0,
        "rumoca_status": "sim_ok",
        "rumoca_sim_seconds": 1.0,
        "rumoca_sim_run_seconds": 1.0,
        "rumoca_sim_build_seconds": 0.5,
        "rumoca_sim_wall_seconds": 1.5
    })
}

fn shard_omc_reference_fixture(model: &str) -> Value {
    json!({
        "msl_version": "v4.1.0",
        "omc_version": "OpenModelica 1.26.1",
        "total_models": 1,
        "timing": shard_timing_fixture(),
        "runtime_comparison": { "ratio_stats": {
            "system_ratio_both_success": shard_ratio_stats_fixture(),
            "wall_ratio_both_success": shard_ratio_stats_fixture()
        }},
        "trace_comparison": shard_flat_trace_summary_fixture(),
        "models": { model: shard_omc_model_fixture() },
        "omc_assertion_failures": {
            "model_count": 0,
            "examples": []
        }
    })
}

fn shard_trace_model_fixture(model: &str) -> Value {
    json!({
        "model_name": model,
        "compared_variables": 2,
        "samples_compared": 10,
        "bounded_normalized_l1_score": 0.0,
        "mean_channel_bounded_normalized_l1": 0.0,
        "max_channel_bounded_normalized_l1": 0.0,
        "channel_high_count": 2,
        "channel_minor_count": 0,
        "channel_deviation_count": 0,
        "channel_severe_count": 0,
        "channel_high_percent": 1.0,
        "channel_minor_percent": 0.0,
        "channel_deviation_percent": 0.0,
        "channel_severe_percent": 0.0,
        "channel_violation_mass": 0.0,
        "initial_condition": {
            "channels_compared": 1,
            "high_count": 1,
            "minor_count": 0,
            "deviation_count": 0,
            "severe_count": 0,
            "violation_mass_total": 0.0,
            "mean_channel_bounded_normalized_error": 0.0,
            "max_channel_bounded_normalized_error": 0.0
        },
        "worst_variables": [],
        "rumoca_sim_wall_seconds": 1.5,
        "rumoca_sim_seconds": 1.0,
        "rumoca_sim_build_seconds": 0.5,
        "rumoca_sim_run_seconds": 1.0,
        "omc_sim_system_seconds": 2.0,
        "omc_total_system_seconds": 5.0,
        "omc_wall_seconds": 3.0
    })
}

fn shard_trace_comparison_fixture(model: &str) -> Value {
    json!({
        "models": { model: shard_trace_model_fixture(model) },
        "missing_trace": {},
        "skipped": {}
    })
}

fn write_shard_fixture(dir: &Path, shard: usize, model: &str) {
    let shard_dir = dir.join(format!("shard-{shard}"));
    fs::create_dir_all(&shard_dir).expect("create shard dir");
    write_json_fixture(
        &shard_dir.join("msl_results.json"),
        &serde_json::to_value(shard_summary(model)).expect("serialize summary"),
    );
    write_json_fixture(
        &shard_dir.join(SHARD_OMC_REFERENCE_FILE),
        &shard_omc_reference_fixture(model),
    );
    write_json_fixture(
        &shard_dir.join(SHARD_TRACE_COMPARISON_FILE),
        &shard_trace_comparison_fixture(model),
    );
}

#[test]
fn merge_shard_parity_artifacts_writes_full_omc_and_trace_inputs() {
    let temp = tempdir().expect("tempdir");
    let shard_root = temp.path().join("shards");
    let results_dir = temp.path().join("results");
    write_shard_fixture(&shard_root, 1, "Modelica.Blocks.Examples.A");
    write_shard_fixture(&shard_root, 2, "Modelica.Blocks.Examples.B");

    merge_shard_parity_artifacts(&shard_root, &results_dir).expect("merge parity artifacts");

    let omc = read_shard_json(&results_dir, SHARD_OMC_REFERENCE_FILE).expect("read merged OMC");
    let trace =
        read_shard_json(&results_dir, SHARD_TRACE_COMPARISON_FILE).expect("read merged trace");
    assert_eq!(json_usize(&omc, &["total_models"]), Some(2));
    assert_eq!(
        omc.get("omc_version").and_then(Value::as_str),
        Some("OpenModelica 1.26.1")
    );
    assert_eq!(
        json_usize(
            &omc,
            &[
                "runtime_comparison",
                "ratio_stats",
                "system_ratio_both_success",
                "sample_count"
            ]
        ),
        Some(2)
    );
    assert_eq!(
        json_usize(&omc, &["trace_comparison", "models_compared"]),
        Some(2)
    );
    assert_eq!(json_usize(&trace, &["models_compared"]), Some(2));
    assert_eq!(
        trace.get("models").and_then(Value::as_object).map(Map::len),
        Some(2)
    );
}
