//! Baseline-owned roster of completions without strict-high parity or a typed
//! trace exception (SPEC_0033 simulation soundness).
//!
//! A model that compiles must simulate correctly. The target roster is empty;
//! until then the resolved baseline names every model still in it, and a
//! current model outside the roster fails the gate, so the roster only
//! shrinks. An aggregate count alone would let one model leave while another
//! entered.

use super::*;

pub(super) fn validate_unexcepted_non_high_roster(baseline: &MslQualityBaseline) -> io::Result<()> {
    if let Some(model) = baseline
        .unexcepted_non_high_models
        .iter()
        .find(|model| model.trim().is_empty())
    {
        return Err(io::Error::other(format!(
            "MSL quality baseline contains an empty unexcepted non-high model identity: {model:?}"
        )));
    }
    if let Some(model) = baseline
        .unexcepted_non_high_models
        .iter()
        .find(|model| baseline.certified_strict_high_models.contains(*model))
    {
        return Err(io::Error::other(format!(
            "MSL quality baseline lists {model} as both certified strict-high and unexcepted non-high"
        )));
    }
    roster_additions_are_reviewed(baseline)
}

/// A model joins the roster only through a reviewed boundary that names its
/// open defect: the cause, the facts, and the owner (SPEC_0050).
fn roster_additions_are_reviewed(baseline: &MslQualityBaseline) -> io::Result<()> {
    let mut boundary = baseline.reference_boundary_migration.as_ref();
    while let Some(migration) = boundary {
        for addition in &migration.roster_additions {
            if addition.cause.trim().is_empty()
                || addition.owner.trim().is_empty()
                || addition.facts.iter().all(|fact| fact.trim().is_empty())
            {
                return Err(io::Error::other(format!(
                    "MSL roster addition {} must name its cause, facts, and owner",
                    addition.model_name
                )));
            }
        }
        boundary = migration.previous.as_deref();
    }
    let Some(head) = baseline.reference_boundary_migration.as_ref() else {
        return Ok(());
    };
    match head.roster_additions.iter().find(|addition| {
        !baseline
            .unexcepted_non_high_models
            .contains(&addition.model_name)
    }) {
        Some(addition) => Err(io::Error::other(format!(
            "MSL roster addition {} is not in the baseline roster",
            addition.model_name
        ))),
        None => Ok(()),
    }
}

pub(super) fn unexcepted_roster_growth_reasons(
    baseline: &MslQualityBaseline,
    measurement: &MslParityMeasurement,
) -> Vec<String> {
    let Some(cohort) = measurement.cohort() else {
        return vec![
            "current certification has no full-cohort band table to compare with the unexcepted non-high roster"
                .to_string(),
        ];
    };
    cohort
        .table
        .unexcepted_non_high_rows()
        .filter(|row| !baseline.unexcepted_non_high_models.contains(&row.model_name))
        .map(|row| {
            format!(
                "{} simulated without strict-high parity or a typed trace exception and is not in the baseline roster ({})",
                row.model_name,
                row.describe()
            )
        })
        .collect()
}

/// Fail closed when more completed simulations lack both strict-high OMC
/// parity and a typed trace exception than the baseline roster names. A typed
/// exception row is the only reviewed boundary: a pointwise non-identifiable
/// completion without one is unclassified (SPEC_0033 simulation soundness).
pub(super) fn push_trace_soundness_reasons(
    reasons: &mut Vec<String>,
    gate_input: MslQualityGateInput<'_>,
    baseline: &MslQualityBaseline,
    parity_input: Option<&MslParityGateInput>,
) {
    let Some(trace) = parity_input.and_then(|parity| parity.trace_accuracy_stats.as_ref()) else {
        return;
    };
    let classified = trace.agreement_high + trace.policy_excluded_models;
    let unclassified = gate_input.sim_ok.saturating_sub(classified);
    let overclassified = classified.saturating_sub(gate_input.sim_ok);
    let allowed = baseline.unexcepted_non_high_models.len();
    if unclassified > allowed || overclassified > 0 {
        reasons.push(format!(
            "simulation soundness requires every sim_ok model to be strict-high or carry a typed trace exception, beyond the baseline roster of {allowed}: sim_ok={} strict_high={} typed_exceptions={} unclassified={unclassified} overclassified={overclassified}",
            gate_input.sim_ok, trace.agreement_high, trace.policy_excluded_models,
        ));
    }
}

/// The typed trace exception file the run read must be the reviewed one: a
/// changed or added row needs a new reviewed boundary with evidence, so an
/// unreviewed row cannot retire a roster member (SPEC_0050).
pub(super) fn exception_file_reasons(measurement: &MslParityMeasurement) -> Vec<String> {
    let Some(cohort) = measurement.cohort() else {
        return Vec::new();
    };
    let read = &cohort.table.source.exclusions_sha256;
    let reviewed = reviewed_reference_boundary_migration()
        .metric
        .exclusions_sha256;
    if *read == reviewed {
        return Vec::new();
    }
    vec![format!(
        "the run read trace exceptions with SHA-256 `{read}`, not the reviewed boundary's `{reviewed}`; a changed exception row needs a new reviewed boundary with evidence"
    )]
}

/// Write the run's unexcepted non-high roster and the SHA-256 of the exception
/// file it read into a quality snapshot.
pub(super) fn insert_soundness_roster(
    root: &mut serde_json::Map<String, serde_json::Value>,
    table: &rumoca_test_msl::msl_tools::band_table::BandTable,
) -> io::Result<IndexSet<String>> {
    let unexcepted = table
        .unexcepted_non_high_rows()
        .map(|row| row.model_name.clone())
        .collect::<IndexSet<_>>();
    let roster = serde_json::to_value(&unexcepted).map_err(|error| {
        io::Error::other(format!(
            "failed to serialize unexcepted non-high roster: {error}"
        ))
    })?;
    root.insert("unexcepted_non_high_models".to_string(), roster);
    root.insert(
        "trace_exceptions_sha256".to_string(),
        serde_json::Value::String(table.source.exclusions_sha256.clone()),
    );
    Ok(unexcepted)
}
