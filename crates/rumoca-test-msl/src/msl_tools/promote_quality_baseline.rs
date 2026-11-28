use anyhow::{Context, Result, bail};
use clap::Parser;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use super::common::MslPaths;

const DEFAULT_BASELINE_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json";
const DEFAULT_CURRENT_REL: &str = "msl_quality_current.json";
const EXPECTED_QUALITY_GATE_VERSION: u64 = 15;
const RUN_SCOPE_FULL: &str = "full";
const RUN_SCOPE_PARTIAL: &str = "partial";

#[derive(Debug, Parser, Clone)]
pub struct Args {
    /// Source quality snapshot JSON (defaults to target/msl/results/msl_quality_current.json)
    #[arg(long)]
    pub source: Option<PathBuf>,
    /// Destination committed baseline JSON
    #[arg(long)]
    pub baseline: Option<PathBuf>,
}

pub fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let source = args
        .source
        .unwrap_or_else(|| paths.results_dir.join(DEFAULT_CURRENT_REL));
    let baseline = args
        .baseline
        .unwrap_or_else(|| paths.repo_root.join(DEFAULT_BASELINE_REL));

    if !source.is_file() {
        bail!(
            "current quality snapshot not found: {}. run the MSL test first.",
            source.display()
        );
    }

    let snapshot_text = fs::read_to_string(&source)
        .with_context(|| format!("failed to read {}", source.display()))?;
    let snapshot: serde_json::Value = serde_json::from_str(&snapshot_text)
        .with_context(|| format!("invalid JSON in {}", source.display()))?;
    ensure_promotable_quality_snapshot(&snapshot, &source)?;

    let mut promoted_text = snapshot_text;
    if baseline.exists() {
        let previous_text = fs::read_to_string(&baseline)
            .with_context(|| format!("failed to read {}", baseline.display()))?;
        let previous: serde_json::Value = serde_json::from_str(&previous_text)
            .with_context(|| format!("invalid JSON in {}", baseline.display()))?;
        ensure_certified_roster_preserved(&snapshot, &previous, &baseline)?;
        let mut promoted = snapshot;
        if carry_reviewed_headers(&mut promoted, &previous, &baseline)? {
            promoted_text = serde_json::to_string_pretty(&promoted)?;
        }
    }

    if let Some(parent) = baseline.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&baseline, promoted_text)
        .with_context(|| format!("failed to write {}", baseline.display()))?;

    println!("Promoted MSL quality baseline:");
    println!("  source: {}", source.display());
    println!("  baseline: {}", baseline.display());
    Ok(())
}

/// Header keys a run never measures: reviewed lineage and corrections that a
/// baseline carries from one promotion to the next.
const REVIEWED_HEADER_KEYS: [&str; 4] = [
    "evidence_provenance",
    "budget_metadata_correction",
    "omc_context_migration",
    "promoted_baseline_bridge",
];

/// Carry the previous baseline's reviewed headers into the promoted snapshot.
/// A snapshot that states one of them with a different value is refused: a
/// run cannot rewrite a reviewed record. Returns whether anything was added.
fn carry_reviewed_headers(
    promoted: &mut serde_json::Value,
    previous: &serde_json::Value,
    baseline: &Path,
) -> Result<bool> {
    let mut carried = false;
    for key in REVIEWED_HEADER_KEYS {
        let Some(reviewed) = previous.get(key) else {
            continue;
        };
        match promoted.get(key) {
            Some(current) if current == reviewed => {}
            Some(_) => bail!(
                "cannot promote into {}: the snapshot's {key} differs from the reviewed one",
                baseline.display()
            ),
            None => {
                promoted
                    .as_object_mut()
                    .context("snapshot must be a JSON object")?
                    .insert(key.to_string(), reviewed.clone());
                carried = true;
            }
        }
    }
    Ok(carried)
}

fn ensure_promotable_quality_snapshot(snapshot: &serde_json::Value, source: &Path) -> Result<()> {
    let Some(root) = snapshot.as_object() else {
        bail!("cannot promote {}: expected JSON object", source.display());
    };
    match root
        .get("quality_gate_version")
        .and_then(serde_json::Value::as_u64)
    {
        Some(EXPECTED_QUALITY_GATE_VERSION) => {}
        Some(version) => bail!(
            "cannot promote {}: unsupported quality_gate_version={} (expected {})",
            source.display(),
            version,
            EXPECTED_QUALITY_GATE_VERSION
        ),
        None => bail!(
            "cannot promote {}: missing numeric quality_gate_version",
            source.display()
        ),
    }
    match root.get("run_scope").and_then(serde_json::Value::as_str) {
        Some(RUN_SCOPE_FULL) => {}
        Some(RUN_SCOPE_PARTIAL) => bail!(
            "cannot promote partial MSL quality snapshot {}. rerun the full baseline without focused RUMOCA_MSL_SIM_* filters.",
            source.display()
        ),
        Some(scope) => bail!(
            "cannot promote {}: run_scope must be '{}', got '{}'",
            source.display(),
            RUN_SCOPE_FULL,
            scope
        ),
        None => bail!("cannot promote {}: missing run_scope", source.display()),
    }
    match root.get("omc_version").and_then(serde_json::Value::as_str) {
        Some(version) if !version.trim().is_empty() => {}
        Some(_) => bail!(
            "cannot promote {}: omc_version must be a non-empty string",
            source.display()
        ),
        None => bail!("cannot promote {}: missing omc_version", source.display()),
    }
    certified_roster(snapshot, source)?;
    let Some(partial) = root.get("partial") else {
        return Ok(());
    };
    match partial.as_bool() {
        Some(false) => Ok(()),
        Some(true) => bail!(
            "cannot promote partial MSL quality snapshot {}. rerun the full baseline without focused RUMOCA_MSL_SIM_* filters.",
            source.display()
        ),
        None => bail!(
            "cannot promote {}: field 'partial' must be a boolean when present",
            source.display()
        ),
    }
}

fn ensure_certified_roster_preserved(
    current: &serde_json::Value,
    previous: &serde_json::Value,
    baseline: &Path,
) -> Result<()> {
    let certified = certified_roster(current, baseline)?;
    if let Some(model) = certified_roster(previous, baseline)?
        .difference(&certified)
        .next()
    {
        bail!(
            "cannot promote {}: baseline-certified strict-high model {model} left the certified roster",
            baseline.display()
        );
    }
    Ok(())
}

fn certified_roster<'a>(
    snapshot: &'a serde_json::Value,
    source: &Path,
) -> Result<BTreeSet<&'a str>> {
    let field = "certified_strict_high_models";
    let roster = snapshot
        .get(field)
        .and_then(serde_json::Value::as_array)
        .with_context(|| {
            format!(
                "cannot promote {}: {field} must be an array",
                source.display()
            )
        })?;
    let mut models = BTreeSet::new();
    for value in roster {
        let model = value
            .as_str()
            .filter(|model| !model.trim().is_empty())
            .with_context(|| {
                format!(
                    "cannot promote {}: {field} has an invalid identity",
                    source.display()
                )
            })?;
        if !models.insert(model) {
            bail!(
                "cannot promote {}: {field} repeats {model}",
                source.display()
            );
        }
    }
    Ok(models)
}

#[cfg(test)]
mod tests {
    use super::{Args, EXPECTED_QUALITY_GATE_VERSION, ensure_promotable_quality_snapshot, run};
    use serde_json::json;
    use std::path::PathBuf;

    fn full_snapshot() -> serde_json::Value {
        json!({
            "quality_gate_version": EXPECTED_QUALITY_GATE_VERSION,
            "run_scope": "full",
            "omc_version": "OpenModelica 1.26.1",
            "sim_ok": 120,
            "certified_strict_high_models": ["High0", "High1"],
            "timing_margin_models": {"Slow": {"phase": "Sim", "budget_share": 0.6}}
        })
    }

    #[test]
    fn manual_promotion_rejects_dropped_and_equal_count_swapped_certificates_atomically() {
        let previous = full_snapshot();
        for roster in [json!(["High0"]), json!(["High0", "Slow"])] {
            let dir = tempfile::tempdir().expect("tempdir");
            let baseline = dir.path().join("baseline.json");
            let source = dir.path().join("current.json");
            let original = serde_json::to_string(&previous).expect("baseline JSON");
            std::fs::write(&baseline, &original).expect("write baseline");
            let mut current = previous.clone();
            current["certified_strict_high_models"] = roster;
            current["timing_margin_models"] =
                json!({"High1": {"phase": "Sim", "budget_share": 0.6}});
            std::fs::write(&source, current.to_string()).expect("write current");
            let err = run(Args {
                source: Some(source),
                baseline: Some(baseline.clone()),
            })
            .expect_err("certification loss must refuse promotion");
            assert!(err.to_string().contains("High1"), "{err}");
            assert_eq!(
                std::fs::read_to_string(baseline).expect("retained baseline"),
                original
            );
        }
    }

    #[test]
    fn manual_promotion_preserves_certificates_across_schema_and_context_changes() {
        let dir = tempfile::tempdir().expect("tempdir");
        let baseline = dir.path().join("baseline.json");
        let source = dir.path().join("current.json");
        let mut previous = full_snapshot();
        previous["quality_gate_version"] = json!(EXPECTED_QUALITY_GATE_VERSION - 1);
        previous["omc_version"] = json!("OpenModelica 1.25.0");
        let original = previous.to_string();
        std::fs::write(&baseline, &original).expect("write baseline");
        let mut current = full_snapshot();
        current["certified_strict_high_models"] = json!(["High0"]);
        current["compiler_contract_migration"] = json!({
            "from_contract": "permissive-dae-v1",
            "to_contract": "checked-dae-v1",
            "evidence_git_commit": "reviewed-contract-evidence",
            "sim_target_models": 566
        });
        std::fs::write(&source, current.to_string()).expect("write current");
        run(Args {
            source: Some(source.clone()),
            baseline: Some(baseline.clone()),
        })
        .expect_err("a version or context change cannot waive certificate loss");
        assert_eq!(
            std::fs::read_to_string(&baseline).expect("retained baseline"),
            original
        );
        current["certified_strict_high_models"] = json!(["High0", "High1", "NewHigh"]);
        std::fs::write(&source, current.to_string()).expect("write current");
        run(Args {
            source: Some(source),
            baseline: Some(baseline.clone()),
        })
        .expect("preserved certificates permit promotion");
        assert_eq!(
            std::fs::read_to_string(baseline).expect("promoted baseline"),
            current.to_string()
        );
    }

    #[test]
    fn manual_promotion_rejects_missing_or_malformed_certification_rosters() {
        for bad in [
            None,
            Some(json!(null)),
            Some(json!("High0")),
            Some(json!([""])),
            Some(json!(["High0", "High0"])),
        ] {
            for corrupt_previous in [false, true] {
                assert_invalid_certification_refused(bad.as_ref(), corrupt_previous);
            }
        }
    }

    fn assert_invalid_certification_refused(
        bad: Option<&serde_json::Value>,
        corrupt_previous: bool,
    ) {
        let dir = tempfile::tempdir().expect("tempdir");
        let baseline = dir.path().join("baseline.json");
        let source = dir.path().join("current.json");
        let mut previous = full_snapshot();
        let mut current = full_snapshot();
        let damaged = if corrupt_previous {
            &mut previous
        } else {
            &mut current
        };
        match bad {
            Some(value) => damaged["certified_strict_high_models"] = value.clone(),
            None => {
                damaged
                    .as_object_mut()
                    .expect("snapshot object")
                    .remove("certified_strict_high_models");
            }
        };
        std::fs::write(&baseline, previous.to_string()).expect("write baseline");
        std::fs::write(&source, current.to_string()).expect("write current");
        run(Args {
            source: Some(source),
            baseline: Some(baseline),
        })
        .expect_err("invalid certification metadata cannot become a baseline");
    }

    fn reviewed_headers() -> serde_json::Value {
        json!({
            "evidence_provenance": {"oracle": "reviewed"},
            "budget_metadata_correction": {"asset_sha256": "abc", "reason": "reviewed"},
            "omc_context_migration": {"from_omc_version": "a", "to_omc_version": "b"},
            "promoted_baseline_bridge": {"document_sha256": "def"}
        })
    }

    fn promote(
        previous: &serde_json::Value,
        current: &serde_json::Value,
    ) -> anyhow::Result<serde_json::Value> {
        let dir = tempfile::tempdir().expect("tempdir");
        let baseline = dir.path().join("baseline.json");
        let source = dir.path().join("current.json");
        std::fs::write(&baseline, previous.to_string()).expect("write baseline");
        std::fs::write(&source, current.to_string()).expect("write current");
        run(Args {
            source: Some(source),
            baseline: Some(baseline.clone()),
        })?;
        let text = std::fs::read_to_string(baseline).expect("promoted baseline");
        Ok(serde_json::from_str(&text).expect("promoted JSON"))
    }

    #[test]
    fn manual_promotion_carries_every_reviewed_header_forward() {
        let mut previous = full_snapshot();
        let headers = reviewed_headers();
        for (key, value) in headers.as_object().expect("headers") {
            previous[key] = value.clone();
        }
        let current = full_snapshot();
        let promoted = promote(&previous, &current).expect("promotion");
        for (key, value) in headers.as_object().expect("headers") {
            assert_eq!(
                promoted.get(key),
                Some(value),
                "{key} must be carried forward"
            );
        }
        assert_eq!(
            promoted["sim_ok"], current["sim_ok"],
            "measurements come from the run"
        );
    }

    #[test]
    fn manual_promotion_refuses_a_snapshot_that_rewrites_a_reviewed_header() {
        let mut previous = full_snapshot();
        previous["budget_metadata_correction"] = json!({"asset_sha256": "abc"});
        let mut current = full_snapshot();
        current["budget_metadata_correction"] = json!({"asset_sha256": "other"});
        let error =
            promote(&previous, &current).expect_err("a run cannot rewrite a reviewed record");
        assert!(
            error.to_string().contains("budget_metadata_correction"),
            "{error}"
        );
        current["budget_metadata_correction"] = json!({"asset_sha256": "abc"});
        let promoted = promote(&previous, &current).expect("an equal value is not a rewrite");
        assert_eq!(
            promoted["budget_metadata_correction"],
            json!({"asset_sha256": "abc"})
        );
    }

    #[test]
    fn manual_promotion_creates_the_first_baseline_from_a_valid_full_snapshot() {
        let dir = tempfile::tempdir().expect("tempdir");
        let baseline = dir.path().join("baseline.json");
        let source = dir.path().join("current.json");
        let text = full_snapshot().to_string();
        std::fs::write(&source, &text).expect("write current");
        run(Args {
            source: Some(source),
            baseline: Some(baseline.clone()),
        })
        .expect("first full baseline has no earlier certificates to retain");
        assert_eq!(
            std::fs::read_to_string(baseline).expect("new baseline"),
            text
        );
    }

    #[test]
    fn promotable_quality_snapshot_allows_missing_or_false_partial_marker_for_full_run() {
        let source = PathBuf::from("msl_quality_current.json");
        ensure_promotable_quality_snapshot(&full_snapshot(), &source)
            .expect("full scope snapshot is promotable");
        let mut explicit_false = full_snapshot();
        explicit_false["partial"] = json!(false);
        ensure_promotable_quality_snapshot(&explicit_false, &source)
            .expect("explicit false marker means full baseline snapshot");
    }

    #[test]
    fn promotable_quality_snapshot_rejects_partial_scope() {
        let source = PathBuf::from("msl_quality_current.json");
        let err = ensure_promotable_quality_snapshot(
            &json!({
                "quality_gate_version": EXPECTED_QUALITY_GATE_VERSION,
                "run_scope": "partial"
            }),
            &source,
        )
        .expect_err("partial snapshots must not be promotable");
        assert!(
            err.to_string().contains("cannot promote partial"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn promotable_quality_snapshot_rejects_non_boolean_partial_marker() {
        let source = PathBuf::from("msl_quality_current.json");
        let mut snapshot = full_snapshot();
        snapshot["partial"] = json!("yes");
        let err = ensure_promotable_quality_snapshot(&snapshot, &source)
            .expect_err("partial marker must be boolean");
        assert!(
            err.to_string().contains("must be a boolean"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn promotable_quality_snapshot_rejects_missing_gate_metadata() {
        let source = PathBuf::from("msl_quality_current.json");
        let err = ensure_promotable_quality_snapshot(&json!({ "run_scope": "full" }), &source)
            .expect_err("gate version is required");
        assert!(
            err.to_string().contains("quality_gate_version"),
            "unexpected error: {err}"
        );

        let err = ensure_promotable_quality_snapshot(
            &json!({ "quality_gate_version": EXPECTED_QUALITY_GATE_VERSION }),
            &source,
        )
        .expect_err("run_scope is required");
        assert!(
            err.to_string().contains("run_scope"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn promotable_quality_snapshot_rejects_missing_omc_version() {
        let source = PathBuf::from("msl_quality_current.json");
        let err = ensure_promotable_quality_snapshot(
            &json!({
                "quality_gate_version": EXPECTED_QUALITY_GATE_VERSION,
                "run_scope": "full"
            }),
            &source,
        )
        .expect_err("OMC version is required for promoted baselines");
        assert!(
            err.to_string().contains("omc_version"),
            "unexpected error: {err}"
        );

        let mut blank = full_snapshot();
        blank["omc_version"] = json!(" ");
        let err = ensure_promotable_quality_snapshot(&blank, &source)
            .expect_err("OMC version must be non-empty");
        assert!(
            err.to_string().contains("non-empty"),
            "unexpected error: {err}"
        );
    }
}
