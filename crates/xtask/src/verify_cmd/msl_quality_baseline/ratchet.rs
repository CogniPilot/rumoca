//! The MSL baseline ratchet: promote a full CI quality snapshot over the
//! published release baseline when it improves some ratchet metric and
//! regresses none.
//!
//! This is the decision `.github/scripts/msl-baseline-ratchet.mjs` makes, read
//! from the same JSON with the same checks and messages, so the two can run side
//! by side on one CI run before the script is retired. The cohort-relative
//! metrics are compared by [`super::cohort`], which the script's cohort module
//! is already held to by the shared cases.

use super::cohort::{
    CohortEvidence, CohortSnapshot, cohort_comparison, to_exponential_6, validate_runtime_ratios,
};
use super::{RuntimeRatioStats, TraceAccuracyStats};
use anyhow::{Context, Result, bail, ensure};
use serde::Deserialize;
use serde_json::Value;
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

/// A count every strict-high completion enters; the ones the certification
/// timing margin holds back (SPEC_0050) are left out of it when both snapshots
/// apply the margin rule.
const CERTIFIED: bool = true;

type IntMetric = (&'static str, &'static [&'static str], bool);

const CONTEXT_INDEPENDENT_HIGHER_IS_BETTER: &[IntMetric] = &[
    ("parse models", &["parse_models"], false),
    ("flatten models", &["flatten_models"], false),
    ("DAE models", &["dae_models"], false),
    ("compiled models", &["compiled_models"], false),
    ("solve models", &["solve_models"], false),
    ("balanced models", &["balanced_models"], false),
    ("balance denominator", &["balance_denominator"], false),
    (
        "initial balanced models",
        &["initial_balanced_models"],
        false,
    ),
    ("simulation attempts", &["sim_attempted"], false),
    ("initial-condition attempts", &["ic_attempted"], false),
    ("initial-condition solves", &["ic_ok"], false),
    ("successful simulations", &["sim_ok"], CERTIFIED),
];

const OMC_DEPENDENT_HIGHER_IS_BETTER: &[IntMetric] = &[
    (
        "trace models compared",
        &["trace_accuracy_stats", "models_compared"],
        CERTIFIED,
    ),
    (
        "high trace agreement",
        &["trace_accuracy_stats", "agreement_high"],
        CERTIFIED,
    ),
    (
        "state-set exact matches",
        &[
            "trace_accuracy_stats",
            "state_selection",
            "exact_state_set_match_models",
        ],
        false,
    ),
];

const CONTEXT_INDEPENDENT_LOWER_IS_BETTER: &[IntMetric] = &[
    ("partial models", &["partial_models"], false),
    ("unbalanced models", &["unbalanced_models"], false),
    (
        "initial unbalanced models",
        &["initial_unbalanced_models"],
        false,
    ),
    (
        "initial-condition solver failures",
        &["ic_solver_fail"],
        false,
    ),
];

const OMC_DEPENDENT_LOWER_IS_BETTER: &[IntMetric] = &[
    (
        "trace deviation models",
        &["trace_accuracy_stats", "agreement_deviation"],
        false,
    ),
    (
        "trace bad channels",
        &["trace_accuracy_stats", "bad_channels_total"],
        false,
    ),
    (
        "trace severe channels",
        &["trace_accuracy_stats", "severe_channels_total"],
        false,
    ),
    (
        "trace models with bad channels",
        &["trace_accuracy_stats", "models_with_any_channel_deviation"],
        false,
    ),
];

const LOWER_FLOAT_IS_BETTER: &[(&str, &[&str])] = &[(
    "trace violation mass",
    &["trace_accuracy_stats", "violation_mass_total"],
)];

/// Whether to promote the current snapshot, and why.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RatchetDecision {
    pub(crate) promote: bool,
    pub(crate) reason: Option<String>,
    pub(crate) improvements: Vec<String>,
    pub(crate) regressions: Vec<String>,
}

/// Overwrite `baseline_path` with the snapshot at `source_path`, byte for
/// byte, when [`ratchet_decision`] promotes it against the checked-in baseline.
pub(crate) fn promote_baseline_if_improved(
    source_path: &Path,
    baseline_path: &Path,
    checked_in_baseline_path: &Path,
    log: &mut dyn FnMut(&str),
) -> Result<RatchetDecision> {
    let source_text = fs::read_to_string(source_path)
        .with_context(|| format!("read {}", source_path.display()))?;
    let source = parse_json(&source_text, source_path)?;
    let baseline = parse_json(&read(baseline_path)?, baseline_path)?;
    let checked_in = parse_json(&read(checked_in_baseline_path)?, checked_in_baseline_path)?;

    let decision = ratchet_decision(&source, &baseline, &checked_in)?;
    if !decision.promote {
        log(&format!(
            "MSL quality baseline not promoted: {}",
            decision.reason.as_deref().unwrap_or_default()
        ));
        return Ok(decision);
    }
    log("MSL quality baseline ratchet improvements:");
    for line in &decision.improvements {
        log(&format!("  - {line}"));
    }
    fs::write(baseline_path, source_text)
        .with_context(|| format!("write {}", baseline_path.display()))?;
    Ok(decision)
}

fn read(path: &Path) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("read {}", path.display()))
}

fn parse_json(text: &str, path: &Path) -> Result<Value> {
    serde_json::from_str(text).with_context(|| format!("failed to parse {}", path.display()))
}

/// The quality schema a snapshot must carry to be promoted is the one of the
/// reviewed checked-in baseline of the same commit; the quality gate pins that
/// baseline to its own schema version, so the ratchet keeps no copy of it.
pub(crate) fn ensure_promotable_snapshot(
    snapshot: &Value,
    expected_version: i64,
    name: &str,
) -> Result<()> {
    ensure_runtime_ratios(snapshot, name)?;
    ensure!(
        integer_at(snapshot, &["quality_gate_version"], name)? == expected_version,
        "{name}: quality_gate_version differs from the reviewed checked-in baseline"
    );
    ensure!(
        string_at(snapshot, &["run_scope"], name)? == "full",
        "{name}: only full MSL quality snapshots can be promoted"
    );
    ensure!(
        !string_at(snapshot, &["omc_version"], name)?
            .trim()
            .is_empty(),
        "{name}: omc_version must be non-empty"
    );
    if let Some(partial) = snapshot.get("partial") {
        ensure!(
            partial == &Value::Bool(false),
            "{name}: partial snapshots cannot be promoted"
        );
    }
    Ok(())
}

/// One snapshot as the cohort comparison reads it.
#[derive(Deserialize)]
struct CohortInput {
    #[serde(default)]
    quality_gate_version: Option<u64>,
    trace_accuracy_stats: TraceAccuracyStats,
    runtime_ratio_stats: RuntimeRatioStats,
    #[serde(flatten)]
    evidence: CohortEvidence,
}

impl CohortInput {
    fn read(snapshot: &Value, name: &str) -> Result<Self> {
        serde_json::from_value(snapshot.clone())
            .with_context(|| format!("{name}: read the cohort evidence"))
    }

    fn view(&self) -> CohortSnapshot<'_> {
        CohortSnapshot {
            quality_gate_version: self.quality_gate_version,
            evidence: &self.evidence,
            trace: &self.trace_accuracy_stats,
            runtime: &self.runtime_ratio_stats,
        }
    }
}

/// The script's `snapshotRuntimeRatios`: a snapshot without per-model ratios
/// passes unless it is of a schema that records them; one with them must bind
/// them to its runtime cohort and medians. Reads the runtime fields only.
fn ensure_runtime_ratios(snapshot: &Value, name: &str) -> Result<()> {
    let version = snapshot.get("quality_gate_version").and_then(Value::as_f64);
    let modern = version.is_some_and(|version| version >= 14.0);
    let stats = snapshot
        .get("runtime_ratio_stats")
        .filter(|stats| !stats.is_null());
    if snapshot.get("runtime_model_ratios").is_none() {
        ensure!(
            !(modern && stats.is_some()),
            "{name}: current-schema runtime_model_ratios are missing"
        );
        return Ok(());
    }
    let evidence = ["runtime_ratio_cohort_models", "runtime_model_ratios"]
        .into_iter()
        .filter_map(|key| Some((key.to_string(), snapshot.get(key)?.clone())))
        .collect::<serde_json::Map<_, _>>();
    let evidence: CohortEvidence = serde_json::from_value(Value::Object(evidence))
        .with_context(|| format!("{name}: runtime_model_ratios must be an object"))?;
    let runtime: RuntimeRatioStats = serde_json::from_value(stats.cloned().unwrap_or_default())
        .with_context(|| format!("{name}: read runtime_ratio_stats"))?;
    validate_runtime_ratios(
        version.map(|version| version as u64),
        &evidence,
        &runtime,
        name,
    )
}

pub(crate) fn ratchet_decision(
    current: &Value,
    baseline: &Value,
    checked_in: &Value,
) -> Result<RatchetDecision> {
    ensure!(
        checked_in.is_object(),
        "cannot ratchet baseline: the reviewed checked-in baseline is required"
    );
    let schema_version = integer_at(checked_in, &["quality_gate_version"], "checked-in baseline")?;
    ensure_promotable_snapshot(current, schema_version, "current snapshot")?;
    ensure_same_context(current, baseline, &["simulatable_attempted"])?;
    ensure_same_context(current, baseline, &["sim_target_models"])?;
    let crossed = crossed_reference_boundaries(current, baseline, checked_in)?;
    let current_omc = non_empty_string_at(current, &["omc_version"], "current snapshot")?;
    let baseline_omc = non_empty_string_at(baseline, &["omc_version"], "baseline snapshot")?;
    let omc_context_changed = current_omc != baseline_omc;
    if omc_context_changed {
        ensure!(
            crossed.is_empty(),
            "cannot ratchet baseline: a reference boundary cannot change the OMC context"
        );
        validate_omc_context_migration(current, baseline, checked_in)?;
    }

    let mut verdict = Verdict::default();
    compare_certified_roster(current, baseline, &mut verdict)?;
    let floored = applies_timing_margin(current) && applies_timing_margin(baseline);
    verdict.integers(
        CONTEXT_INDEPENDENT_HIGHER_IS_BETTER,
        current,
        baseline,
        true,
        floored,
    )?;
    verdict.integers(
        CONTEXT_INDEPENDENT_LOWER_IS_BETTER,
        current,
        baseline,
        false,
        floored,
    )?;
    if !omc_context_changed {
        compare_omc_dependent(current, baseline, floored, &mut verdict)?;
    } else {
        verdict
            .improvements
            .push(format!("OMC context: {baseline_omc} -> {current_omc}"));
    }
    for boundary in &crossed {
        verdict.improvements.push(format!(
            "reference boundary: {} -> {} ({})",
            integer_at(
                boundary,
                &["from_quality_gate_version"],
                "reference boundary"
            )?,
            integer_at(boundary, &["to_quality_gate_version"], "reference boundary")?,
            string_at(boundary, &["change"], "reference boundary")?,
        ));
    }

    let Verdict {
        improvements,
        regressions,
    } = verdict;
    let reason = if !regressions.is_empty() {
        Some(format!(
            "ratchet metric regression(s): {}",
            regressions.join("; ")
        ))
    } else if improvements.is_empty() {
        Some("source snapshot is equivalent to the committed baseline".to_string())
    } else {
        None
    };
    Ok(RatchetDecision {
        promote: reason.is_none(),
        reason,
        improvements,
        regressions,
    })
}

/// The metrics only comparable under one OMC context: trace agreement, the
/// rosters, the trace exception file and the cohort-relative totals.
fn compare_omc_dependent(
    current: &Value,
    baseline: &Value,
    floored: bool,
    verdict: &mut Verdict,
) -> Result<()> {
    verdict.integers(
        OMC_DEPENDENT_HIGHER_IS_BETTER,
        current,
        baseline,
        true,
        floored,
    )?;
    verdict.integers(
        OMC_DEPENDENT_LOWER_IS_BETTER,
        current,
        baseline,
        false,
        floored,
    )?;
    for (label, path) in LOWER_FLOAT_IS_BETTER {
        verdict.float(
            label,
            number_at(current, path, "current snapshot")?,
            number_at(baseline, path, "baseline snapshot")?,
        );
    }
    compare_derived_metrics(current, baseline, floored, verdict)?;
    compare_unexcepted_roster(current, baseline, verdict)?;
    compare_trace_exceptions(current, baseline, verdict)?;
    let cohort = cohort_comparison(
        CohortInput::read(baseline, "baseline snapshot")?.view(),
        CohortInput::read(current, "current snapshot")?.view(),
    )?;
    verdict.improvements.extend(cohort.improvements);
    verdict.regressions.extend(cohort.regressions);
    Ok(())
}

/// A promoted baseline at an older quality schema is crossed only through the
/// reviewed reference boundary chain (SPEC_0033, SPEC_0050), the same chain the
/// quality gate resolves the promoted asset through: the current snapshot
/// declares the checked-in chain, the chain steps down one reviewed boundary at
/// a time to exactly the promoted version, and the promoted asset declares the
/// boundary that chain names for its version. A boundary changes no ratchet
/// floor, so the caller still compares every metric against the promoted asset.
/// Returns the crossed boundaries, oldest first.
fn crossed_reference_boundaries<'a>(
    current: &'a Value,
    baseline: &Value,
    checked_in: &Value,
) -> Result<Vec<&'a Value>> {
    let current_version = integer_at(current, &["quality_gate_version"], "current snapshot")?;
    let baseline_version = integer_at(baseline, &["quality_gate_version"], "baseline snapshot")?;
    if current_version == baseline_version {
        return Ok(Vec::new());
    }
    ensure!(
        baseline_version < current_version,
        "cannot ratchet baseline: promoted quality schema {baseline_version} is newer than current {current_version}"
    );
    let chain = value_at(current, &["reference_boundary_migration"])?;
    ensure!(
        chain == value_at(checked_in, &["reference_boundary_migration"])?,
        "cannot ratchet baseline: current reference boundary chain differs from the reviewed checked-in chain"
    );
    let mut crossed = Vec::new();
    let mut boundary = Some(chain);
    let mut target = current_version;
    while target > baseline_version {
        let Some(link) = boundary.filter(|link| link.is_object()) else {
            bail!(
                "cannot ratchet baseline: no reviewed reference boundary reaches quality schema {baseline_version}"
            );
        };
        let to = integer_at(link, &["to_quality_gate_version"], "reference boundary")?;
        let from = integer_at(link, &["from_quality_gate_version"], "reference boundary")?;
        ensure!(
            to == target && from < target,
            "cannot ratchet baseline: reference boundary {from} -> {to} does not continue the chain at {target}"
        );
        crossed.push(link);
        target = from;
        boundary = link.get("previous");
    }
    ensure!(
        target == baseline_version,
        "cannot ratchet baseline: reference boundary chain skips quality schema {baseline_version}"
    );
    ensure!(
        baseline.get("reference_boundary_migration") == boundary,
        "cannot ratchet baseline: promoted reference boundary differs from the reviewed chain at quality schema {baseline_version}"
    );
    crossed.reverse();
    Ok(crossed)
}

fn validate_omc_context_migration(
    current: &Value,
    baseline: &Value,
    checked_in: &Value,
) -> Result<()> {
    ensure_promotable_snapshot(
        checked_in,
        integer_at(current, &["quality_gate_version"], "current snapshot")?,
        "checked-in migration baseline",
    )?;
    let current_omc = non_empty_string_at(current, &["omc_version"], "current snapshot")?;
    let baseline_omc = non_empty_string_at(baseline, &["omc_version"], "baseline snapshot")?;
    ensure!(
        non_empty_string_at(
            checked_in,
            &["omc_version"],
            "checked-in migration baseline"
        )? == current_omc,
        "cannot ratchet baseline: checked-in OMC context does not match current snapshot"
    );
    let Some(migration) = checked_in.get("omc_context_migration") else {
        bail!(
            "cannot ratchet baseline: changed OMC context requires the reviewed checked-in migration"
        );
    };
    ensure!(
        migration.is_object() || migration.is_null(),
        "cannot ratchet baseline: checked-in omc_context_migration must be an object"
    );
    ensure!(
        non_empty_string_at(migration, &["from_omc_version"], "OMC context migration")?
            == baseline_omc,
        "cannot ratchet baseline: OMC migration source does not match promoted baseline"
    );
    ensure!(
        non_empty_string_at(migration, &["to_omc_version"], "OMC context migration")?
            == current_omc,
        "cannot ratchet baseline: OMC migration target does not match current snapshot"
    );
    let current_target_count = integer_at(current, &["sim_target_models"], "current snapshot")?;
    ensure!(
        integer_at(migration, &["sim_target_models"], "OMC context migration")?
            == current_target_count,
        "cannot ratchet baseline: OMC migration target count does not match current snapshot"
    );
    ensure!(
        integer_at(
            checked_in,
            &["sim_target_models"],
            "checked-in migration baseline"
        )? == current_target_count,
        "cannot ratchet baseline: checked-in OMC target count does not match current snapshot"
    );
    Ok(())
}

fn ensure_same_context(current: &Value, baseline: &Value, path: &[&str]) -> Result<()> {
    let current_value = integer_at(current, path, "current snapshot")?;
    let baseline_value = integer_at(baseline, path, "baseline snapshot")?;
    ensure!(
        current_value == baseline_value,
        "cannot ratchet baseline: {} changed from {baseline_value} to {current_value}",
        path.join(".")
    );
    Ok(())
}

#[derive(Default)]
struct Verdict {
    improvements: Vec<String>,
    regressions: Vec<String>,
}

impl Verdict {
    fn integers(
        &mut self,
        metrics: &[IntMetric],
        current: &Value,
        baseline: &Value,
        higher_is_better: bool,
        floored: bool,
    ) -> Result<()> {
        for &(label, path, certified) in metrics {
            let floored = certified && floored;
            self.metric(
                label,
                metric_value(current, path, "current snapshot", floored)?,
                metric_value(baseline, path, "baseline snapshot", floored)?,
                higher_is_better,
            );
        }
        Ok(())
    }

    fn metric(&mut self, label: &str, current: i64, baseline: i64, higher_is_better: bool) {
        let (improved, regressed) = if higher_is_better {
            (current > baseline, current < baseline)
        } else {
            (current < baseline, current > baseline)
        };
        if improved {
            self.improvements
                .push(format!("{label}: {baseline} -> {current}"));
        } else if regressed {
            self.regressions
                .push(format!("{label}: {baseline} -> {current}"));
        }
    }

    fn float(&mut self, label: &str, current: f64, baseline: f64) {
        const EPSILON: f64 = 1.0e-9;
        let line = || {
            format!(
                "{label}: {} -> {}",
                to_exponential_6(baseline),
                to_exponential_6(current)
            )
        };
        if current < baseline - EPSILON {
            self.improvements.push(line());
        } else if current > baseline + EPSILON {
            self.regressions.push(line());
        }
    }
}

/// A count as recorded, or its certified floor when `floored`.
fn metric_value(snapshot: &Value, path: &[&str], name: &str, floored: bool) -> Result<i64> {
    let measured = integer_at(snapshot, path, name)?;
    if floored {
        certified_floor(snapshot, measured)
    } else {
        Ok(measured)
    }
}

fn compare_certified_roster(
    current: &Value,
    baseline: &Value,
    verdict: &mut Verdict,
) -> Result<()> {
    let certified = certified_roster_at(current, "current snapshot")?;
    for model in certified_roster_at(baseline, "baseline snapshot")? {
        if !certified.contains(&model) {
            verdict.regressions.push(format!(
                "baseline-certified strict-high model {model} left the certified roster"
            ));
        }
    }
    Ok(())
}

/// The certified roster in recorded order, each model once.
fn certified_roster_at<'a>(snapshot: &'a Value, name: &str) -> Result<Vec<&'a str>> {
    let field = "certified_strict_high_models";
    let Some(roster) = value_at(snapshot, &[field])?.as_array() else {
        bail!("{name}: {field} must be an array");
    };
    let mut seen = BTreeSet::new();
    let mut models = Vec::with_capacity(roster.len());
    for model in roster {
        let Some(model) = model.as_str().filter(|model| !model.trim().is_empty()) else {
            bail!("{name}: {field} has an invalid identity");
        };
        ensure!(seen.insert(model), "{name}: {field} repeats {model}");
        models.push(model);
    }
    Ok(models)
}

fn compare_derived_metrics(
    current: &Value,
    baseline: &Value,
    floored: bool,
    verdict: &mut Verdict,
) -> Result<()> {
    let certified = |snapshot: &Value, measured: i64| {
        if floored {
            certified_floor(snapshot, measured)
        } else {
            Ok(measured)
        }
    };
    verdict.metric(
        "high+near trace agreement",
        certified(current, trace_high_near_count(current)?)?,
        certified(baseline, trace_high_near_count(baseline)?)?,
        true,
    );
    verdict.metric(
        "trace models without severe channels",
        certified(current, trace_no_severe_count(current)?)?,
        certified(baseline, trace_no_severe_count(baseline)?)?,
        true,
    );
    Ok(())
}

/// The typed trace exception file only changes through a reviewed boundary: a
/// snapshot read under a different file than its baseline is a regression
/// unless its own reference boundary pins that file (SPEC_0050).
fn compare_trace_exceptions(
    current: &Value,
    baseline: &Value,
    verdict: &mut Verdict,
) -> Result<()> {
    let read = non_empty_string_at(current, &["trace_exceptions_sha256"], "current snapshot")?;
    let previous = if baseline.get("trace_exceptions_sha256").is_some() {
        non_empty_string_at(baseline, &["trace_exceptions_sha256"], "baseline snapshot")?
    } else {
        non_empty_string_at(
            baseline,
            &["reference_boundary_migration", "exclusions_sha256"],
            "baseline snapshot",
        )?
    };
    if read == previous {
        return Ok(());
    }
    let pinned = non_empty_string_at(
        current,
        &["reference_boundary_migration", "exclusions_sha256"],
        "current snapshot",
    )?;
    if pinned == read {
        verdict.improvements.push(format!(
            "trace exceptions: reviewed boundary {previous} -> {read}"
        ));
    } else {
        verdict.regressions.push(format!(
            "trace exceptions changed without a reviewed boundary: {previous} -> {read}"
        ));
    }
    Ok(())
}

/// The roster of completions without strict-high parity or a typed trace
/// exception only shrinks: a model outside the baseline roster is a regression
/// even when the count holds (SPEC_0033 simulation soundness). A baseline from
/// before the roster existed is compared by the reviewed boundary that
/// introduces it, and a model joins the roster only through the
/// `roster_additions` of a reviewed boundary the comparison crosses, naming the
/// defect (SPEC_0050).
fn compare_unexcepted_roster(
    current: &Value,
    baseline: &Value,
    verdict: &mut Verdict,
) -> Result<()> {
    let current_roster = model_roster_at(current, "current snapshot")?;
    if baseline.get("unexcepted_non_high_models").is_none() {
        verdict.improvements.push(format!(
            "unexcepted non-high roster introduced: {}",
            current_roster.len()
        ));
        return Ok(());
    }
    let baseline_roster = model_roster_at(baseline, "baseline snapshot")?
        .iter()
        .map(roster_key)
        .collect::<BTreeSet<_>>();
    let reviewed = crossed_roster_additions(current, baseline)?;
    for model in current_roster {
        let key = roster_key(model);
        if baseline_roster.contains(&key) {
            continue;
        }
        if reviewed.contains(&key) {
            verdict.improvements.push(format!(
                "unexcepted non-high roster: reviewed addition {key}"
            ));
        } else {
            verdict
                .regressions
                .push(format!("unexcepted non-high roster gained {key}"));
        }
    }
    if reviewed.is_empty() {
        verdict.metric(
            "unexcepted non-high models",
            current_roster.len() as i64,
            baseline_roster.len() as i64,
            false,
        );
    }
    Ok(())
}

/// A roster entry as JavaScript's `Set` and template strings see it: a string
/// by its text, anything else by its JSON form.
fn roster_key(model: &Value) -> String {
    model
        .as_str()
        .map_or_else(|| model.to_string(), str::to_string)
}

/// The roster additions of the current snapshot's reference boundaries that
/// lie above the baseline's quality gate version.
fn crossed_roster_additions(current: &Value, baseline: &Value) -> Result<BTreeSet<String>> {
    let baseline_version = integer_at(baseline, &["quality_gate_version"], "baseline snapshot")?;
    let mut models = BTreeSet::new();
    let mut boundary = current
        .get("reference_boundary_migration")
        .filter(|link| truthy(link));
    while let Some(link) = boundary {
        let to = link.get("to_quality_gate_version").and_then(Value::as_f64);
        if !to.is_some_and(|to| to > baseline_version as f64) {
            break;
        }
        if let Some(additions) = link.get("roster_additions").and_then(Value::as_array) {
            for addition in additions {
                models.insert(roster_key(
                    addition.get("model_name").unwrap_or(&Value::Null),
                ));
            }
        }
        boundary = link.get("previous").filter(|link| truthy(link));
    }
    Ok(models)
}

fn truthy(value: &Value) -> bool {
    !matches!(value, Value::Null | Value::Bool(false))
}

fn model_roster_at<'a>(snapshot: &'a Value, name: &str) -> Result<&'a Vec<Value>> {
    value_at(snapshot, &["unexcepted_non_high_models"])?
        .as_array()
        .with_context(|| format!("{name}: unexcepted_non_high_models must be an array"))
}

/// Whether a snapshot records the certification timing margin (SPEC_0050); a
/// snapshot from before the rule counts every strict-high completion.
fn applies_timing_margin(snapshot: &Value) -> bool {
    snapshot.get("timing_margin_models").is_some()
}

/// A count every strict-high completion enters, less the completions the
/// snapshot's timing margin leaves uncertified.
fn certified_floor(snapshot: &Value, measured: i64) -> Result<i64> {
    let Some(held) = snapshot
        .get("timing_margin_models")
        .and_then(Value::as_object)
    else {
        bail!("timing_margin_models must be an object");
    };
    let count = held.len() as i64;
    ensure!(
        count <= measured,
        "timing-margin models exceed the count they leave"
    );
    Ok(measured - count)
}

fn trace_high_near_count(snapshot: &Value) -> Result<i64> {
    Ok(integer_at(
        snapshot,
        &["trace_accuracy_stats", "agreement_high"],
        "snapshot",
    )? + integer_at(
        snapshot,
        &["trace_accuracy_stats", "agreement_minor"],
        "snapshot",
    )?)
}

fn trace_no_severe_count(snapshot: &Value) -> Result<i64> {
    let compared = integer_at(
        snapshot,
        &["trace_accuracy_stats", "models_compared"],
        "snapshot",
    )?;
    let severe = integer_at(
        snapshot,
        &["trace_accuracy_stats", "models_with_severe_channel"],
        "snapshot",
    )?;
    Ok((compared - severe).max(0))
}

fn integer_at(snapshot: &Value, path: &[&str], name: &str) -> Result<i64> {
    let value = number_at(snapshot, path, name)?;
    ensure!(
        value.fract() == 0.0 && value.abs() <= 9_007_199_254_740_991.0,
        "{name}: {} must be an integer",
        path.join(".")
    );
    Ok(value as i64)
}

fn number_at(snapshot: &Value, path: &[&str], name: &str) -> Result<f64> {
    let Some(value) = value_at(snapshot, path)?.as_f64() else {
        bail!("{name}: {} must be numeric", path.join("."));
    };
    ensure!(
        value.is_finite(),
        "{name}: {} must be finite",
        path.join(".")
    );
    Ok(value)
}

fn string_at<'a>(snapshot: &'a Value, path: &[&str], name: &str) -> Result<&'a str> {
    value_at(snapshot, path)?
        .as_str()
        .with_context(|| format!("{name}: {} must be a string", path.join(".")))
}

fn non_empty_string_at<'a>(snapshot: &'a Value, path: &[&str], name: &str) -> Result<&'a str> {
    let value = string_at(snapshot, path, name)?.trim();
    ensure!(
        !value.is_empty(),
        "{name}: {} must be non-empty",
        path.join(".")
    );
    Ok(value)
}

fn value_at<'a>(snapshot: &'a Value, path: &[&str]) -> Result<&'a Value> {
    let mut value = snapshot;
    for key in path {
        let Some(next) = value.as_object().and_then(|object| object.get(*key)) else {
            bail!("missing quality metric {}", path.join("."));
        };
        value = next;
    }
    Ok(value)
}

#[cfg(test)]
mod tests;
