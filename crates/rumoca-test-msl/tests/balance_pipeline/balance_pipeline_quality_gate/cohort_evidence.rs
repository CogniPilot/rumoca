//! Per-model evidence behind the cohort-dependent quality totals.
//!
//! Initial-condition deviations, state-set disagreements, and runtime ratios
//! accrue on every compared model whatever its band, so a run that compares
//! more models raises those totals without any model getting worse. The
//! snapshot records the per-model values so the baseline ratchet and the
//! baseline resolver compare them over the models both runs measured
//! (`.github/scripts/msl-baseline-cohort.mjs`,
//! `crates/xtask/src/verify_cmd/msl_quality_baseline/cohort.rs`).

use super::*;
use rumoca_test_msl::msl_tools::band_table::BandTable;
use std::collections::BTreeMap;

const TRACE_COMPARISON_FILE: &str = "sim_trace_comparison.json";

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(super) struct TraceModelEvidence {
    ic_deviation_channels: usize,
    ic_severe_channels: usize,
    ic_violation_mass: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    state_set: Option<StateSetEvidence>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(super) struct StateSetEvidence {
    rumoca_only: usize,
    omc_only: usize,
    exact: bool,
}

/// Record `trace_model_evidence` and `runtime_model_ratios` in a snapshot. The
/// evidence is read from the comparator output the band table was derived
/// from, and must name exactly the table's compared models.
pub(super) fn insert_cohort_evidence(
    root: &mut serde_json::Map<String, serde_json::Value>,
    parity: &MslParityGateInput,
    table: &BandTable,
) -> io::Result<()> {
    let path = msl_results_dir().join(TRACE_COMPARISON_FILE);
    let trace: serde_json::Value = serde_json::from_slice(&fs::read(&path)?).map_err(|error| {
        io::Error::other(format!(
            "invalid comparator output {}: {error}",
            path.display()
        ))
    })?;
    let evidence = trace_model_evidence(&trace)?;
    let compared = table
        .compared_rows()
        .map(|row| row.model_name.as_str())
        .collect::<BTreeSet<_>>();
    if !evidence
        .keys()
        .map(String::as_str)
        .eq(compared.iter().copied())
    {
        return Err(io::Error::other(format!(
            "comparator output {} names {} compared models, the band table {}",
            path.display(),
            evidence.len(),
            compared.len()
        )));
    }
    root.insert("trace_model_evidence".to_string(), to_json(&evidence)?);
    root.insert(
        "runtime_model_ratios".to_string(),
        to_json(&runtime_model_ratios(parity))?,
    );
    Ok(())
}

/// The per-model initial-condition and state-set evidence of a comparator
/// output's `models` map.
pub(super) fn trace_model_evidence(
    trace: &serde_json::Value,
) -> io::Result<BTreeMap<String, TraceModelEvidence>> {
    let models = trace
        .get("models")
        .and_then(serde_json::Value::as_object)
        .ok_or_else(|| io::Error::other("comparator output has no models map"))?;
    models
        .iter()
        .map(|(name, model)| {
            model_evidence(model)
                .map(|evidence| (name.clone(), evidence))
                .ok_or_else(|| {
                    io::Error::other(format!(
                        "comparator output has no initial-condition evidence for {name}"
                    ))
                })
        })
        .collect()
}

fn model_evidence(model: &serde_json::Value) -> Option<TraceModelEvidence> {
    let initial = model.get("initial_condition")?;
    let state_set = match model.get("state_selection") {
        Some(state) => Some(StateSetEvidence {
            rumoca_only: json_usize_field(state, "rumoca_only_state_count")?,
            omc_only: json_usize_field(state, "omc_only_state_count")?,
            exact: state.get("exact_state_set_match")?.as_bool()?,
        }),
        None => None,
    };
    Some(TraceModelEvidence {
        ic_deviation_channels: json_usize_field(initial, "deviation_count")?,
        ic_severe_channels: json_usize_field(initial, "severe_count")?,
        ic_violation_mass: json_f64_field(initial, "violation_mass_total")?,
        state_set,
    })
}

fn runtime_model_ratios(parity: &MslParityGateInput) -> BTreeMap<&str, serde_json::Value> {
    parity
        .runtime_model_ratios
        .iter()
        .map(|(name, ratio)| {
            (
                name.as_str(),
                serde_json::json!({ "system": ratio.system, "wall": ratio.wall }),
            )
        })
        .collect()
}

fn to_json(value: &impl Serialize) -> io::Result<serde_json::Value> {
    serde_json::to_value(value)
        .map_err(|error| io::Error::other(format!("failed to serialize cohort evidence: {error}")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn evidence_reads_initial_condition_and_state_set_per_model() {
        let trace = serde_json::json!({
            "models": {
                "A": {
                    "initial_condition": {"deviation_count": 2, "severe_count": 1, "violation_mass_total": 0.5},
                    "state_selection": {"rumoca_only_state_count": 3, "omc_only_state_count": 1, "exact_state_set_match": false}
                },
                "B": {
                    "initial_condition": {"deviation_count": 0, "severe_count": 0, "violation_mass_total": 0.0}
                }
            }
        });
        let evidence = trace_model_evidence(&trace).unwrap();
        assert_eq!(
            serde_json::to_value(&evidence).unwrap(),
            serde_json::json!({
                "A": {"ic_deviation_channels": 2, "ic_severe_channels": 1, "ic_violation_mass": 0.5,
                      "state_set": {"rumoca_only": 3, "omc_only": 1, "exact": false}},
                "B": {"ic_deviation_channels": 0, "ic_severe_channels": 0, "ic_violation_mass": 0.0}
            })
        );
    }

    #[test]
    fn evidence_refuses_a_model_without_initial_condition_counts() {
        let trace = serde_json::json!({"models": {"A": {"state_selection": {}}}});
        let error = trace_model_evidence(&trace).unwrap_err();
        assert!(error.to_string().contains("for A"), "{error}");
        assert!(trace_model_evidence(&serde_json::json!({})).is_err());
    }
}
