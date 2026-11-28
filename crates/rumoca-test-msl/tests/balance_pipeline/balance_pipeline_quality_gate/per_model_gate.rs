//! The gate on per-model transitions (`rumoca_msl_quality::judge`).
//!
//! When the baseline records `model_states`, a run is judged model by model
//! against it, with the rule the promotion ratchet uses: a model worse than its
//! own baseline state fails (unless a crossed boundary reviews the change), a
//! model that must be re-measured (a watchdog kill, a failed OMC reference)
//! fails until it is, and every model whose mean channel error rose is listed
//! and must be stated exactly by the generated `msl_accuracy_changes.json`
//! keyed to the baseline's sha256, so the PR that trades accuracy carries the
//! reviewed list promotion checks. This replaces the stage-count, aggregate
//! trace and certified-cohort reasons; the soundness, tensor, runtime, roster
//! and exception-file checks still apply.

use super::*;
use rumoca_msl_quality::snapshot::{crossed_links, has_old_side, judge_snapshot};
use rumoca_msl_quality::{ACCURACY_STATEMENT_FILE, PerModelVerdict, sha256_hex};

fn read_json(path: &Path, what: &str) -> io::Result<(Vec<u8>, serde_json::Value)> {
    let bytes = fs::read(path)?;
    let value = serde_json::from_slice(&bytes)
        .map_err(|error| io::Error::other(format!("invalid {what} {}: {error}", path.display())))?;
    Ok((bytes, value))
}

/// Judge the current snapshot per model against the raw promoted asset (the
/// baseline promotion judges against), or, offline, against the gate's own
/// baseline. `None` when the run predates `model_states`, or when the baseline
/// predates them and no crossed boundary pins evidence (then the aggregate rule
/// applies).
pub(super) fn per_model_verdict(gate_baseline: &Path) -> io::Result<Option<PerModelVerdict>> {
    let baseline = promoted_baseline_file().unwrap_or_else(|| {
        println!(
            "MSL per-model: no promoted baseline asset; judging against {} (local/offline only)",
            gate_baseline.display()
        );
        gate_baseline.to_path_buf()
    });
    per_model_verdict_at(
        &baseline,
        &msl_quality_current_path(),
        &msl_workspace_root().join(ACCURACY_STATEMENT_FILE),
        &msl_workspace_root(),
    )
}

pub(super) fn per_model_verdict_at(
    baseline_path: &Path,
    current_path: &Path,
    statement_path: &Path,
    evidence_root: &Path,
) -> io::Result<Option<PerModelVerdict>> {
    let (baseline_bytes, baseline) = read_json(baseline_path, "quality baseline")?;
    let (_, current) = read_json(current_path, "quality snapshot")?;
    let crossed = crossed_links(&current, &baseline).map_err(io::Error::other)?;
    if !has_old_side(&baseline, &crossed) {
        return Ok(None);
    }
    if current.get("model_states").is_none() {
        return Err(io::Error::other(format!(
            "{} records no model_states, but the baseline side does",
            current_path.display()
        )));
    }
    let statement = match fs::read(statement_path) {
        Ok(bytes) => Some(bytes),
        Err(error) if error.kind() == io::ErrorKind::NotFound => None,
        Err(error) => return Err(error),
    };
    judge_snapshot(
        &current,
        &baseline,
        &sha256_hex(&baseline_bytes),
        &crossed,
        statement.as_deref(),
        evidence_root,
    )
    .map_err(io::Error::other)
}

/// The gate's failure under the per-model rule, after the parity and context
/// checks every run must pass, and with the checks transitions do not replace.
pub(super) fn per_model_gate_failure_message(
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
    measurement: &MslParityMeasurement,
    verdict: &PerModelVerdict,
) -> Option<String> {
    if let Some(reason) = measurement.gate_failure_reason() {
        return Some(reason);
    }
    let parity_input = measurement.gate_input();
    if let Some(reason) = msl_quality_context_mismatch_reason(gate_input, baseline, parity_input) {
        return Some(format!(
            "MSL quality baseline context mismatch: {reason}. Update {} only with explicit review approval",
            MSL_QUALITY_BASELINE_FILE_REL
        ));
    }
    for line in verdict.report.iter().chain(&verdict.improvements) {
        println!("MSL per-model: {line}");
    }
    let mut reasons = verdict.regressions.clone();
    reasons.extend(
        verdict
            .inconclusive
            .iter()
            .map(|line| format!("{line} (re-measure first)")),
    );
    push_tensor_preservation_regression_reasons(&mut reasons, gate_input, baseline);
    push_trace_soundness_reasons(&mut reasons, gate_input, baseline, parity_input);
    push_runtime_ratio_regression_reasons(&mut reasons, baseline, parity_input);
    (!reasons.is_empty()).then(|| reasons.join("; "))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_msl_quality::{BandLabel, ModelStage, ModelState, StageSeconds, TraceState};
    use serde_json::json;

    fn simulated(error: f64) -> serde_json::Value {
        serde_json::to_value(ModelState {
            stage: ModelStage::Simulated,
            outcome: None,
            trace: TraceState::Scored(BandLabel::High),
            compared_channels: 3,
            trace_violation_mass: Some(0.0),
            mean_channel_bounded_l1: Some(error),
            certified: false,
            roster: false,
            stage_seconds: StageSeconds::default(),
        })
        .unwrap()
    }

    struct Files(PathBuf);

    impl Files {
        fn new(name: &str) -> Self {
            let dir = std::env::temp_dir()
                .join(format!("msl-per-model-gate-{name}-{}", std::process::id()));
            fs::create_dir_all(&dir).unwrap();
            Self(dir)
        }

        fn write(&self, file: &str, value: &serde_json::Value) -> PathBuf {
            let path = self.0.join(file);
            fs::write(&path, serde_json::to_vec_pretty(value).unwrap()).unwrap();
            path
        }

        fn verdict(&self) -> io::Result<Option<PerModelVerdict>> {
            per_model_verdict_at(
                &self.0.join("baseline.json"),
                &self.0.join("current.json"),
                &self.0.join("msl_accuracy_changes.json"),
                &self.0,
            )
        }
    }

    impl Drop for Files {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn snapshot(states: Option<serde_json::Value>) -> serde_json::Value {
        let mut snapshot = json!({ "quality_gate_version": 15, "omc_version": "OMC 1.27" });
        if let Some(states) = states {
            snapshot["model_states"] = states;
        }
        snapshot
    }

    #[test]
    fn a_baseline_without_states_keeps_the_aggregate_rule() {
        let files = Files::new("aggregate");
        files.write("baseline.json", &snapshot(None));
        files.write(
            "current.json",
            &snapshot(Some(json!({ "A": simulated(1.0) }))),
        );
        assert_eq!(files.verdict().unwrap(), None);
    }

    #[test]
    fn a_run_without_states_against_a_baseline_with_them_is_refused() {
        let files = Files::new("refused");
        files.write(
            "baseline.json",
            &snapshot(Some(json!({ "A": simulated(1.0) }))),
        );
        files.write("current.json", &snapshot(None));
        let error = files.verdict().unwrap_err().to_string();
        assert!(error.contains("records no model_states"), "{error}");
    }

    #[test]
    fn the_gate_rules_on_the_statement_keyed_to_its_baseline() {
        let files = Files::new("statement");
        let baseline = files.write(
            "baseline.json",
            &snapshot(Some(json!({ "A": simulated(1.0e-3) }))),
        );
        files.write(
            "current.json",
            &snapshot(Some(json!({ "A": simulated(2.0e-3) }))),
        );
        // Unstated: the rise fails the PR, not main's promotion after merge.
        let unstated = files.verdict().unwrap().unwrap();
        assert_eq!(unstated.regressions.len(), 1, "{:?}", unstated.regressions);
        assert!(unstated.regressions[0].contains("mean channel error rose"));
        let statement = |key: &str| {
            json!({
                "schema_version": 1,
                "baseline_sha256": key,
                "rose": [{ "model": "A", "before": 1.0e-3, "after": 2.0e-3 }],
            })
        };
        let key = sha256_hex(&fs::read(&baseline).unwrap());
        files.write("msl_accuracy_changes.json", &statement(&key));
        let stated = files.verdict().unwrap().unwrap();
        assert!(stated.regressions.is_empty(), "{:?}", stated.regressions);
        // Keyed to another baseline: refused as stale.
        files.write("msl_accuracy_changes.json", &statement(&"0".repeat(64)));
        let error = files.verdict().unwrap_err().to_string();
        assert!(error.starts_with("stale accuracy statement"), "{error}");
    }

    #[test]
    fn the_gate_honors_a_crossed_reviewed_removal_only() {
        let files = Files::new("reviewed");
        let mut baseline = snapshot(Some(json!({ "A": simulated(1.0), "Gone": simulated(1.0) })));
        baseline["quality_gate_version"] = json!(14);
        files.write("baseline.json", &baseline);
        let mut current = snapshot(Some(json!({ "A": simulated(1.0) })));
        current["reference_boundary_migration"] = json!({
            "to_quality_gate_version": 15,
            "reviewed_changes": { "removed_targets": ["Gone"] },
        });
        files.write("current.json", &current);
        let reviewed = files.verdict().unwrap().unwrap();
        assert!(
            reviewed.regressions.is_empty(),
            "{:?}",
            reviewed.regressions
        );
        // The same link, already behind the baseline, is not crossed.
        baseline["quality_gate_version"] = json!(15);
        files.write("baseline.json", &baseline);
        let behind = files.verdict().unwrap().unwrap();
        assert_eq!(
            behind.regressions,
            vec!["Gone: left the sim targets without a reviewed removal".to_string()]
        );
    }

    #[test]
    fn the_first_crossing_judges_against_the_evidence_its_boundary_pins() {
        let files = Files::new("evidence");
        let evidence = json!({
            "schema_version": 1,
            "source_run": "ci-1",
            "source_commit": "abc",
            "caveats": [],
            "model_states": { "A": simulated(1.0), "B": simulated(1.0) },
        });
        let evidence_path = files.write("evidence.json", &evidence);
        let sha = sha256_hex(&fs::read(&evidence_path).unwrap());
        let mut baseline = snapshot(None);
        baseline["quality_gate_version"] = json!(14);
        files.write("baseline.json", &baseline);
        let mut current = snapshot(Some(json!({ "A": simulated(1.0) })));
        current["reference_boundary_migration"] = json!({
            "to_quality_gate_version": 15,
            "evidence_model_states": { "file": "evidence.json", "sha256": sha },
        });
        files.write("current.json", &current);
        // B is in the evidence and gone from the run: judged, and refused.
        let verdict = files.verdict().unwrap().unwrap();
        assert_eq!(
            verdict.regressions,
            vec!["B: left the sim targets without a reviewed removal".to_string()]
        );
    }
}
