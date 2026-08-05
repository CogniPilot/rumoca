//! Typed outcome of the Tier 2 OMC trace-comparator stage.
//!
//! # Why this type exists
//!
//! `sim_ok` is completion, never parity (SPEC 0033, `38464fd8`). A parity claim
//! may only come from the OMC comparator's agreement bands. Before this module
//! the harness carried the comparator reading as a bare
//! `Option<MslParityGateInput>`: `None` meant "no comparator output", and every
//! consumer silently defaulted — the trace gate printed nothing, the ratchet
//! skipped its trace reasons, and the run's only surviving number was `sim_ok`.
//! That is exactly how the `results-wave3` sweep published `sim_ok 49/566` with
//! no comparison behind it.
//!
//! [`MslParityMeasurement`] makes the absent state *unrepresentable without a
//! reason*: a run either carries a checked [`MslParityGateInput`] or it carries
//! a named [`MslParityUnmeasuredReason`]. There is no third state and no
//! default, so "the comparator did not run" cannot be spelled the same way as
//! "the comparator ran and found nothing wrong".
//!
//! # Cohort pinning
//!
//! Aggregate band counts do not pin the population behind them. A model can
//! stop simulating, disappear from the comparator's `models` map, and leave the
//! headline `agreement_high` unchanged — which is how `DCPM_Start` regressed
//! from strict-high to `sim_solver_fail` with no run naming it. So the strict-
//! high count this module quotes comes from the **persisted per-model band
//! table** ([`rumoca_test_msl::msl_tools::band_table`]), not from the
//! reference's summary field, and the two must agree.
//!
//! The table is derived from this run's comparator output once per process
//! ([`cohort_reading`]) and written to the results directory, rotating an
//! *earlier run's* table to `msl_band_table_previous.json`. That single seam
//! covers both the un-sharded run and the shard fan-in, which merges comparator
//! artifacts without running the comparator stage itself.
//!
//! The run's scope travels with the table, and it is the same predicate the gate
//! skips on: a focused, subset, or sharded run writes a `partial` table. A CI
//! shard still writes one — its results directory is its own — while a focused
//! run over a directory that already holds the cohort's `full` table declines to
//! displace it and states that it did.
//!
//! # Acceptance contract (SPEC 0008)
//!
//! [`MslParityMeasurement::measured`] is the only constructor of the `Measured`
//! variant, and it **accepts** exactly a reading that carries:
//!
//! * `omc_version` metadata (so the reading names the OMC it came from),
//! * `trace_accuracy_stats` with `models_compared > 0`,
//! * a strict-high band count that is representable as a fraction of
//!   `models_compared`,
//! * a comparable per-model band table (see
//!   [`rumoca_test_msl::msl_tools::band_table::ensure_comparable`]) whose
//!   compared, strict-high, near, and deviation counts all equal the
//!   reference's.
//!
//! It **rejects** — by demoting to `Unmeasured(...)`, never by silently keeping
//! a partial reading — a reference missing trace statistics, a reference whose
//! comparator compared zero models, a reference with no OMC version, a run
//! whose band table could not be built or persisted, and a band table that
//! disagrees with the reference on any band. Owner: this module; the on-disk
//! shapes it reads are written by `rumoca-msl-tools omc-simulation-reference`
//! (the reference) and by
//! [`rumoca_test_msl::msl_tools::band_table::persist_band_table`] (the table).
//!
//! A demoted reading is not a soft failure: on a baseline-relative Tier 2 run
//! [`MslParityMeasurement::gate_failure_reason`] turns it into a gate reason, so
//! an incomplete reference fails the run rather than shrinking it to `sim_ok`.
//!
//! The table makes two further gate readings possible, and both are failures
//! rather than notes:
//!
//! * [`MslParityMeasurement::departed_strict_high_reason`] — a model that held
//!   the strict-high band and is no longer compared, which no aggregate can show
//!   because one model leaving and another entering leaves them unchanged.
//! * [`MslParityMeasurement::cohort_movement_unverified_reason`] — a full-cohort
//!   run with no predecessor table to diff against, which would leave the rule
//!   above silently inert. CI restores the predecessor before the gate runs.

use super::*;
use rumoca_test_msl::msl_tools::band_table::{
    self, BandTable, BandTableRunScope, BandTransitions, ExitReason,
};
use std::sync::OnceLock;

/// Fixed prefix every "no parity reading" line starts with. Operators and the
/// CI summary grep for this exact text, so it is a constant rather than an
/// inline literal.
pub(crate) const PARITY_UNMEASURED_HEADLINE: &str = "parity unmeasured: comparator did not run";

/// Named reason a Tier 2 run carries no comparator reading.
///
/// Every variant names the boundary that failed to produce a comparison. A
/// reason is mandatory: there is no `Unknown`, because "we do not know why the
/// comparator produced nothing" is precisely the state that let `sim_ok`
/// masquerade as parity.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MslParityUnmeasuredReason {
    /// `omc --version` failed, so no reference could be generated.
    OmcUnavailable { detail: String },
    /// The comparator stage ran and failed.
    ComparatorStageFailed { detail: String },
    /// The comparator stage never executed in this process.
    StageNotExecuted { detail: String },
    /// No `omc_simulation_reference.json` exists at the expected path.
    ReferenceAbsent { path: String },
    /// A reference exists but does not carry a readable set of bands.
    ReferenceIncomplete { detail: String },
    /// No comparable per-model band table exists, so the compared set is not
    /// pinned and a model could leave it unnoticed.
    BandTableAbsent { detail: String },
    /// The band table and the reference disagree about the bands. One of the
    /// two is stale; neither may be quoted.
    BandTableInconsistent { detail: String },
    /// The run attempted no simulations, so there is nothing to compare.
    NoSimulationsAttempted,
}

impl MslParityUnmeasuredReason {
    /// Operator-facing detail, appended to [`PARITY_UNMEASURED_HEADLINE`].
    pub(crate) fn detail(&self) -> String {
        match self {
            Self::OmcUnavailable { detail } => {
                format!("omc is not available on PATH: {detail}")
            }
            Self::ComparatorStageFailed { detail } => {
                format!("the comparator stage failed: {detail}")
            }
            Self::StageNotExecuted { detail } => {
                format!("the comparator stage never executed: {detail}")
            }
            Self::ReferenceAbsent { path } => {
                format!("no OMC reference at {path}")
            }
            Self::ReferenceIncomplete { detail } => {
                format!("the OMC reference carries no readable bands: {detail}")
            }
            Self::BandTableAbsent { detail } => {
                format!("the run persisted no comparable per-model band table: {detail}")
            }
            Self::BandTableInconsistent { detail } => {
                format!("the per-model band table disagrees with the OMC reference: {detail}")
            }
            Self::NoSimulationsAttempted => "the run attempted no simulations".to_string(),
        }
    }
}

/// What the comparator stage did in this process.
///
/// The gate combines this with what it finds on disk, so a run that merges
/// another process's comparator output (the sharded fan-in) is measured, while
/// a run whose own stage was skipped names the skip.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MslParityStageOutcome {
    /// The comparator stage ran to completion in this process.
    Ran,
    /// The stage did not run; the reason must reach the summary.
    DidNotRun(MslParityUnmeasuredReason),
    /// This run consumes comparator artifacts merged from shard partials.
    MergedShardArtifacts,
}

impl MslParityStageOutcome {
    fn skip_reason(&self) -> Option<&MslParityUnmeasuredReason> {
        match self {
            Self::DidNotRun(reason) => Some(reason),
            Self::Ran | Self::MergedShardArtifacts => None,
        }
    }
}

/// The run's cohort evidence: its band table plus, when the results directory
/// carried one, the previous run's table and the diff against it.
#[derive(Debug, Clone)]
pub(crate) struct MslCohortReading {
    pub(crate) table: BandTable,
    pub(crate) transitions: Option<BandTransitions>,
    /// Set when the previous slot held something that could not be diffed
    /// against — unreadable, or a different run scope. It is stated in the
    /// summary rather than rendered as "no previous table".
    pub(crate) previous_not_diffable: Option<String>,
    /// Whether this run wrote its table into the results directory. A focused
    /// (Tier 1) run declines when the directory already holds the cohort's
    /// table: rotating that aside would destroy the baseline the next Tier 2
    /// diff needs.
    pub(crate) persisted: bool,
}

impl MslCohortReading {
    /// The cohort clause every measured summary line carries. When a previous
    /// table exists this states entered/left/band-changed; when none does it
    /// says so rather than printing zeros that would read as "nothing moved".
    pub(crate) fn summary_clause(&self) -> String {
        if let Some(transitions) = self.transitions.as_ref() {
            return transitions.summary_line();
        }
        if !self.persisted {
            return format!(
                "cohort: {} compared, focused run declined to displace the cohort table so \
                 nothing was written or diffed",
                self.table.models_compared()
            );
        }
        match self.previous_not_diffable.as_deref() {
            Some(detail) => format!(
                "cohort: {} compared, previous band table not diffable so no movement ({detail})",
                self.table.models_compared()
            ),
            None => format!(
                "cohort: {} compared, no previous band table to diff against",
                self.table.models_compared()
            ),
        }
    }

    /// Strict-high models that are no longer compared. Empty when there is no
    /// previous table to diff against.
    pub(crate) fn departed_strict_high(&self) -> Vec<&band_table::LeftModel> {
        self.transitions
            .as_ref()
            .map(|transitions| transitions.departed_strict_high().collect())
            .unwrap_or_default()
    }

    /// Models still compared over fewer channels than before, rendered one per
    /// line.
    pub(crate) fn coverage_drop_lines(&self) -> Vec<String> {
        let Some(transitions) = self.transitions.as_ref() else {
            return Vec::new();
        };
        transitions
            .coverage_dropped
            .iter()
            .map(|drop| {
                format!(
                    "  COVERAGE-DROPPED: {} is still {} but over {} channels, was {}",
                    drop.model_name,
                    drop.band.as_str(),
                    drop.after_compared_variables,
                    drop.before_compared_variables
                )
            })
            .collect()
    }

    /// Models that left the compared set since the previous run, rendered one
    /// per line. Empty when there is no previous table or nothing left.
    pub(crate) fn departure_lines(&self) -> Vec<String> {
        let Some(transitions) = self.transitions.as_ref() else {
            return Vec::new();
        };
        transitions
            .left
            .iter()
            .map(|left| {
                format!(
                    "  LEFT the compared set: {} (was {}) — {}: {}",
                    left.model_name,
                    left.before_band.as_str(),
                    left.exit_reason.as_str(),
                    left.exit_detail.as_deref().unwrap_or("no detail recorded")
                )
            })
            .collect()
    }
}

/// A Tier 2 run's parity reading: either checked bands, or a named absence.
#[derive(Debug, Clone)]
pub(crate) enum MslParityMeasurement {
    Measured {
        input: Box<MslParityGateInput>,
        cohort: Box<MslCohortReading>,
    },
    Unmeasured(MslParityUnmeasuredReason),
}

impl MslParityMeasurement {
    /// The only constructor of `Measured`. Demotes any reading that cannot
    /// support a pinned band population to `Unmeasured(...)`; see the module
    /// acceptance contract.
    pub(crate) fn measured(input: MslParityGateInput, cohort: MslCohortReading) -> Self {
        if input.omc_version.is_none() {
            return Self::Unmeasured(MslParityUnmeasuredReason::ReferenceIncomplete {
                detail: "reference carries no omc_version".to_string(),
            });
        }
        let Some(stats) = input.trace_accuracy_stats.as_ref() else {
            return Self::Unmeasured(MslParityUnmeasuredReason::ReferenceIncomplete {
                detail: "reference carries no trace_accuracy_stats".to_string(),
            });
        };
        if stats.models_compared == 0 {
            return Self::Unmeasured(MslParityUnmeasuredReason::ReferenceIncomplete {
                detail: "reference compared 0 models".to_string(),
            });
        }
        if let Some(reason) = band_table_disagreement(stats, &cohort.table) {
            return Self::Unmeasured(reason);
        }
        Self::Measured {
            input: Box::new(input),
            cohort: Box::new(cohort),
        }
    }

    pub(crate) fn unmeasured(reason: MslParityUnmeasuredReason) -> Self {
        Self::Unmeasured(reason)
    }

    /// The checked reading, or `None` when this run has no parity number. Every
    /// consumer that reaches for band statistics goes through here, so the
    /// `Unmeasured` case has to be handled explicitly at each call site.
    pub(crate) fn gate_input(&self) -> Option<&MslParityGateInput> {
        match self {
            Self::Measured { input, .. } => Some(input),
            Self::Unmeasured(_) => None,
        }
    }

    /// The run's cohort evidence, or `None` when the run has no reading.
    pub(crate) fn cohort(&self) -> Option<&MslCohortReading> {
        match self {
            Self::Measured { cohort, .. } => Some(cohort),
            Self::Unmeasured(_) => None,
        }
    }

    pub(crate) fn unmeasured_reason(&self) -> Option<&MslParityUnmeasuredReason> {
        match self {
            Self::Measured { .. } => None,
            Self::Unmeasured(reason) => Some(reason),
        }
    }

    pub(crate) fn is_measured(&self) -> bool {
        matches!(self, Self::Measured { .. })
    }

    /// Models in the strict-high agreement band — the only quotable parity
    /// count. Read from the persisted per-model band table, which names the
    /// population; `measured` already refused to build if that table and the
    /// reference summary disagreed. `None` when the run has no reading at all.
    pub(crate) fn strict_high_models(&self) -> Option<usize> {
        self.cohort()
            .map(|cohort| cohort.table.strict_high_models())
    }

    pub(crate) fn models_compared(&self) -> Option<usize> {
        self.cohort().map(|cohort| cohort.table.models_compared())
    }

    /// The single line every Tier 2 run prints for parity. A run without a
    /// comparator reading prints [`PARITY_UNMEASURED_HEADLINE`] plus the reason;
    /// it never prints a number.
    ///
    /// `models_compared` never appears alone: SPEC 0033 requires the skipped and
    /// missing counts beside it, because the same compared count means something
    /// different depending on how many candidates were dropped to reach it.
    /// Every band count is read off the rows, not off the table's own `counts`,
    /// so the line cannot quote a number the per-model rows do not support.
    pub(crate) fn summary_line(&self, sim_target_models: usize) -> String {
        match self {
            Self::Measured { input, cohort } => {
                let table = &cohort.table;
                format!(
                    "MSL parity: strict-high {}/{} sim targets ({:.2}% of the cohort); \
                     bands near {}, deviation {}; models_compared {} (skipped {}, missing-trace \
                     {}, not-attempted {}) (omc={}); {}",
                    table.strict_high_models(),
                    sim_target_models,
                    percent_of(table.strict_high_models(), sim_target_models),
                    table.near_models(),
                    table.deviation_models(),
                    table.models_compared(),
                    table.absent_for(band_table::ExitReason::Excluded)
                        + table.absent_for(band_table::ExitReason::ComparatorFailed)
                        + table.absent_for(band_table::ExitReason::NoComparableSamples),
                    table.absent_for(band_table::ExitReason::ReferenceMissing)
                        + table.absent_for(band_table::ExitReason::RumocaTraceMissing)
                        + table.absent_for(band_table::ExitReason::TraceMissingSideUnrecorded),
                    table.absent_for(band_table::ExitReason::NotAttempted),
                    input.omc_version.as_deref().unwrap_or("unknown"),
                    cohort.summary_clause(),
                )
            }
            Self::Unmeasured(reason) => {
                format!(
                    "MSL {PARITY_UNMEASURED_HEADLINE} ({}); sim_ok is completion, never parity",
                    reason.detail()
                )
            }
        }
    }

    /// Gate reason for a baseline-relative Tier 2 run. `Some` exactly when the
    /// run has no comparator reading: SPEC 0033 requires an unmeasured Tier 2
    /// run to fail rather than report a number.
    pub(crate) fn gate_failure_reason(&self) -> Option<String> {
        let reason = self.unmeasured_reason()?;
        Some(format!(
            "{PARITY_UNMEASURED_HEADLINE} over every sim_ok trace ({}); a Tier 2 run \
             without comparator bands reports no parity number",
            reason.detail()
        ))
    }

    /// Gate reason for a cohort certification that could not compare itself to
    /// its predecessor.
    ///
    /// Cohort movement is the whole point of the artifact, and every rule that
    /// reads it — the departed-strict-high gate above, the LEFT list, the
    /// coverage-drop list — is inert without a predecessor to diff against. A
    /// certification that cannot state its movement must say so and fail, not
    /// pass with the rules silently switched off. SPEC 0033 puts the predecessor
    /// in the acceptance contract for exactly this reason; CI supplies it by
    /// restoring the last certification's table before the gate runs.
    ///
    /// Only full-scope runs are held to it: a focused run makes no cohort claim.
    pub(crate) fn cohort_movement_unverified_reason(&self) -> Option<String> {
        let cohort = self.cohort()?;
        if cohort.table.run_scope != BandTableRunScope::Full || cohort.transitions.is_some() {
            return None;
        }
        let detail = cohort.previous_not_diffable.as_deref().unwrap_or(
            "no previous certification band table was present in the results directory; CI \
             restores it as msl_band_table_previous.json before the gate",
        );
        Some(format!(
            "cohort movement is unverified: this certification has no predecessor band table to \
             diff against, so no model can be reported as having left the strict-high band \
             ({detail})"
        ))
    }

    /// Gate reason for models that held the strict-high band and are no longer
    /// compared.
    ///
    /// The aggregate band count cannot catch this: one model leaving and another
    /// entering leaves `agreement_high` flat while the population behind the
    /// number changes. A strict-high model that stops being compared is a
    /// regression unless it moved to a reviewed pointwise-oracle boundary, so
    /// actionable departures fail the run by name rather than being visible
    /// only to a reader diffing two artifacts.
    pub(crate) fn departed_strict_high_reason(&self) -> Option<String> {
        let departed = self
            .cohort()?
            .departed_strict_high()
            .into_iter()
            .filter(|left| {
                !matches!(
                    left.exit_reason,
                    ExitReason::Excluded | ExitReason::TraceNonidentifiable
                )
            })
            .collect::<Vec<_>>();
        if departed.is_empty() {
            return None;
        }
        Some(format!(
            "{} model(s) left the strict-high band's compared set since the previous \
             certification: {}",
            departed.len(),
            departed
                .iter()
                .map(|left| format!(
                    "{} ({}: {})",
                    left.model_name,
                    left.exit_reason.as_str(),
                    left.exit_detail.as_deref().unwrap_or("no detail recorded")
                ))
                .collect::<Vec<_>>()
                .join("; ")
        ))
    }
}

fn percent_of(count: usize, total: usize) -> f64 {
    if total == 0 {
        return 0.0;
    }
    count as f64 * 100.0 / total as f64
}

/// Cross-check the persisted table against the reference's own band summary.
///
/// Both are derived from the same comparator output, so any difference means
/// one artifact is stale — e.g. a table left over from a previous results
/// directory. Quoting either would publish a number whose population is
/// unknown, so the reading is demoted instead.
///
/// Every band is checked, not just the quoted one: a table and a reference that
/// agree on `models_compared` and `agreement_high` while disagreeing on the
/// split between `near` and `deviation` are still two readings of two different
/// comparator outputs, and the summary quotes those two counts as well.
fn band_table_disagreement(
    stats: &MslTraceAccuracyStatsBaseline,
    table: &BandTable,
) -> Option<MslParityUnmeasuredReason> {
    let checks = [
        (
            "compared models",
            table.models_compared(),
            stats.models_compared,
        ),
        (
            "strict-high models",
            table.strict_high_models(),
            stats.agreement_high,
        ),
        ("near models", table.near_models(), stats.agreement_minor),
        (
            "deviation models",
            table.deviation_models(),
            stats.agreement_deviation,
        ),
    ];
    checks
        .into_iter()
        .find(|(_, table_count, reference_count)| table_count != reference_count)
        .map(|(band, table_count, reference_count)| {
            MslParityUnmeasuredReason::BandTableInconsistent {
                detail: format!(
                    "band table has {table_count} {band}, reference reports {reference_count}"
                ),
            }
        })
}

/// This run's cohort reading, computed **once** per process.
///
/// The gate reads the measurement more than once (the snapshot writer and the
/// gate itself). The `OnceLock` makes "one run, one reading" a property of the
/// type rather than a call-order convention — rotation itself is idempotent, but
/// deriving the table twice would still cost the run a second full read of the
/// comparator output.
fn cohort_reading() -> Result<MslCohortReading, MslParityUnmeasuredReason> {
    static COHORT: OnceLock<Result<MslCohortReading, MslParityUnmeasuredReason>> = OnceLock::new();
    COHORT.get_or_init(read_cohort).clone()
}

/// Derive the run's band table from the comparator output in the results
/// directory, persist it, and diff it against the previous run's table.
///
/// The table is derived here rather than trusted from disk so it always
/// describes *this* run's comparator output: a stale table left in a reused
/// results directory would otherwise be read as the run's own population. The
/// shard fan-in, which merges comparator artifacts without running the
/// comparator stage itself, is covered by exactly the same path.
///
/// The run's **scope** travels with the table, and it is the same predicate the
/// gate skips on: a focused, subset, or sharded run compares a slice of the
/// model set, so its table is `partial`. A CI shard still writes one (its
/// results directory is its own, and the fan-in checks that every shard produced
/// it), while a focused run over a directory that already holds the cohort table
/// declines to displace it and says so.
fn read_cohort() -> Result<MslCohortReading, MslParityUnmeasuredReason> {
    let run_scope = if should_skip_msl_quality_gate() {
        BandTableRunScope::Partial
    } else {
        BandTableRunScope::Full
    };
    let persisted =
        band_table::persist_band_table(&msl_results_dir(), run_scope).map_err(|error| {
            MslParityUnmeasuredReason::BandTableAbsent {
                detail: format!("{error:#}"),
            }
        })?;
    if let Some(reason) = persisted.not_persisted_reason.as_deref() {
        println!("MSL band table: NOT WRITTEN — {reason}");
    }
    Ok(MslCohortReading {
        transitions: persisted
            .previous
            .as_ref()
            .map(|previous| band_table::diff_band_tables(previous, &persisted.table)),
        previous_not_diffable: persisted.previous_not_diffable,
        table: persisted.table,
        persisted: persisted.persisted,
    })
}

/// Read the comparator's on-disk reference and turn it into a typed reading.
///
/// This is the single boundary where "there is a file" becomes "there is a
/// parity number". A stage that already knows why it produced nothing wins over
/// the on-disk state, so an `omc`-less run reports `omc is not available` rather
/// than the downstream symptom `no OMC reference at ...`.
pub(crate) fn measure_msl_parity(
    stage: &MslParityStageOutcome,
    expected_sim_target_models: usize,
) -> MslParityMeasurement {
    if let Some(reason) = stage.skip_reason() {
        return MslParityMeasurement::unmeasured(reason.clone());
    }
    let path = omc_simulation_reference_path();
    if !path.is_file() {
        return MslParityMeasurement::unmeasured(MslParityUnmeasuredReason::ReferenceAbsent {
            path: path.display().to_string(),
        });
    }
    let input = match load_current_msl_parity_gate_input_required(expected_sim_target_models) {
        Ok(input) => input,
        Err(error) => {
            return MslParityMeasurement::unmeasured(
                MslParityUnmeasuredReason::ReferenceIncomplete {
                    detail: error.to_string(),
                },
            );
        }
    };
    match cohort_reading() {
        Ok(cohort) => MslParityMeasurement::measured(input, cohort),
        Err(reason) => MslParityMeasurement::unmeasured(reason),
    }
}

/// Band-table fixtures shared by the parity-measurement and quality-gate
/// tests. Every fixture goes through the real derivation, so a gate test can
/// never quote a strict-high count the per-model table would not support.
#[cfg(test)]
pub(crate) mod fixtures {
    use super::*;

    /// A band table with `strict_high` strict-high models and `near` near-band
    /// models, built through the real derivation so the fixture cannot drift
    /// from the classifier the comparator uses.
    pub(crate) fn cohort_table(strict_high: usize, near: usize) -> BandTable {
        cohort_table_tagged("fixture", strict_high, near)
    }

    /// As [`cohort_table`], with an explicit run identity. Two fixtures with
    /// different tags describe different comparator outputs, which is what the
    /// rotation and binding rules key on.
    pub(crate) fn cohort_table_tagged(tag: &str, strict_high: usize, near: usize) -> BandTable {
        cohort_table_bands(tag, strict_high, near, 0)
    }

    /// A fixture carrying every band. The gate cross-checks all four counts
    /// against the reference, so a fixture that could only produce high and near
    /// models could not stand in for a real cohort.
    pub(crate) fn cohort_table_bands(
        tag: &str,
        strict_high: usize,
        near: usize,
        deviation: usize,
    ) -> BandTable {
        let mut models = serde_json::Map::new();
        for index in 0..strict_high {
            let name = format!("High{index}");
            models.insert(name.clone(), band_metric(&name, 10, 0));
        }
        for index in 0..near {
            let name = format!("Near{index}");
            models.insert(name.clone(), band_metric(&name, 7, 3));
        }
        for index in 0..deviation {
            let name = format!("Deviation{index}");
            models.insert(name.clone(), deviation_metric(&name));
        }
        band_table::derive_band_table(
            &serde_json::json!({ "models": models, "missing_trace": {}, "skipped": {} }),
            None,
            &std::collections::BTreeMap::new(),
            fixture_meta(tag),
        )
        .expect("derive band table fixture")
    }

    /// Fixture provenance. The comparator-output digest is what binds a table to
    /// one run, so a fixture that omitted it would not be a comparable artifact.
    pub(crate) fn fixture_meta(tag: &str) -> band_table::BandTableMeta {
        band_table::BandTableMeta {
            run_scope: BandTableRunScope::Full,
            git_commit: "cert1234".to_string(),
            // A fixture stands for a clean-tree certification.
            working_tree_digest: None,
            omc_version: Some("OpenModelica 1.25.0".to_string()),
            source: band_table::BandTableSource {
                trace_comparison_digest: format!("digest-{tag}"),
                ..band_table::BandTableSource::default()
            },
        }
    }

    pub(crate) fn band_metric(model_name: &str, high: usize, minor: usize) -> serde_json::Value {
        serde_json::json!({
            "model_name": model_name,
            "compared_variables": high + minor,
            "samples_compared": 100,
            "bounded_normalized_l1_score": 0.0,
            "mean_channel_bounded_normalized_l1": 0.0,
            "max_channel_bounded_normalized_l1": 0.0,
            "channel_high_count": high,
            "channel_minor_count": minor,
            "channel_deviation_count": 0,
            "channel_severe_count": 0,
            "worst_variables": []
        })
    }

    /// A model the classifier places in the deviation band: most of its channels
    /// deviate.
    fn deviation_metric(model_name: &str) -> serde_json::Value {
        serde_json::json!({
            "model_name": model_name,
            "compared_variables": 10,
            "samples_compared": 100,
            "bounded_normalized_l1_score": 0.5,
            "mean_channel_bounded_normalized_l1": 0.5,
            "max_channel_bounded_normalized_l1": 0.9,
            "channel_high_count": 1,
            "channel_minor_count": 0,
            "channel_deviation_count": 9,
            "channel_severe_count": 0,
            "worst_variables": []
        })
    }

    /// A cohort reading whose table carries exactly the bands the reference
    /// reports, so a test using it exercises the check it means to and not the
    /// cross-check.
    pub(crate) fn cohort_for(stats: &MslTraceAccuracyStatsBaseline) -> MslCohortReading {
        MslCohortReading {
            table: cohort_table_bands(
                "fixture",
                stats.agreement_high,
                stats.agreement_minor,
                stats.agreement_deviation,
            ),
            transitions: None,
            previous_not_diffable: None,
            persisted: true,
        }
    }

    pub(crate) fn cohort_or_empty(
        stats: Option<&MslTraceAccuracyStatsBaseline>,
    ) -> MslCohortReading {
        stats.map_or_else(
            || MslCohortReading {
                table: cohort_table(1, 0),
                transitions: None,
                previous_not_diffable: None,
                persisted: true,
            },
            cohort_for,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::{band_metric, cohort_for, cohort_or_empty, cohort_table, cohort_table_tagged};

    fn stats_with(models_compared: usize, agreement_high: usize) -> MslTraceAccuracyStatsBaseline {
        MslTraceAccuracyStatsBaseline {
            models_compared,
            missing_trace_models: 0,
            skipped_models: 0,
            policy_excluded_models: 0,
            trace_nonidentifiable_models: 0,
            agreement_high,
            agreement_high_percent: None,
            agreement_minor: models_compared - agreement_high,
            agreement_minor_percent: None,
            agreement_deviation: 0,
            agreement_deviation_percent: None,
            total_channels_compared: None,
            bad_channels_total: None,
            severe_channels_total: None,
            bad_channels_percent: None,
            severe_channels_percent: None,
            violation_mass_total: None,
            violation_mass_mean_per_model: None,
            violation_mass_mean_per_channel: None,
            models_with_bad_channel: None,
            models_with_severe_channel: None,
            models_with_any_channel_deviation: None,
            models_with_any_channel_deviation_percent: None,
            max_model_channel_deviation_percent: None,
            bounded_normalized_l1: None,
            mean_model_mean_channel_bounded_normalized_l1: None,
            max_model_max_channel_bounded_normalized_l1: None,
            model_mean_channel_bounded_normalized_l1: None,
            model_max_channel_bounded_normalized_l1: None,
            initial_condition: None,
            state_selection: None,
        }
    }

    fn gate_input_with(
        omc_version: Option<&str>,
        trace: Option<MslTraceAccuracyStatsBaseline>,
    ) -> MslParityGateInput {
        MslParityGateInput {
            total_models: Some(10),
            omc_version: omc_version.map(str::to_string),
            runtime_context: None,
            runtime_ratio_stats: None,
            runtime_model_ratios: IndexMap::new(),
            trace_accuracy_stats: trace,
            omc_assertion_failure_models: 0,
            omc_assertion_failure_examples: Vec::new(),
        }
    }

    /// Build a measurement whose band table matches the reference's own band
    /// counts, so these cases exercise the reference checks rather than the
    /// cohort cross-check.
    fn measured_with(input: MslParityGateInput) -> MslParityMeasurement {
        let cohort = cohort_or_empty(input.trace_accuracy_stats.as_ref());
        MslParityMeasurement::measured(input, cohort)
    }

    #[test]
    fn measured_requires_trace_statistics() {
        let measurement = measured_with(gate_input_with(Some("omc 1.0"), None));
        assert!(!measurement.is_measured());
        assert_eq!(
            measurement.unmeasured_reason(),
            Some(&MslParityUnmeasuredReason::ReferenceIncomplete {
                detail: "reference carries no trace_accuracy_stats".to_string()
            })
        );
    }

    #[test]
    fn measured_rejects_a_reference_that_compared_no_models() {
        let measurement = measured_with(gate_input_with(Some("omc 1.0"), Some(stats_with(0, 0))));
        assert!(!measurement.is_measured());
        assert!(
            measurement
                .unmeasured_reason()
                .expect("zero compared models must demote to unmeasured")
                .detail()
                .contains("compared 0 models")
        );
    }

    #[test]
    fn measured_rejects_a_reference_without_an_omc_version() {
        let measurement = measured_with(gate_input_with(None, Some(stats_with(5, 4))));
        assert!(!measurement.is_measured());
        assert!(
            measurement
                .unmeasured_reason()
                .expect("missing omc_version must demote to unmeasured")
                .detail()
                .contains("no omc_version")
        );
    }

    #[test]
    fn measured_accepts_a_complete_reference_and_exposes_the_strict_high_band() {
        let measurement = measured_with(gate_input_with(
            Some("OpenModelica 1.25.0"),
            Some(stats_with(48, 38)),
        ));
        assert!(measurement.is_measured());
        assert_eq!(measurement.strict_high_models(), Some(38));
        assert_eq!(measurement.models_compared(), Some(48));
        assert!(measurement.gate_failure_reason().is_none());
    }

    #[test]
    fn unmeasured_summary_line_names_the_headline_and_the_reason() {
        let measurement =
            MslParityMeasurement::unmeasured(MslParityUnmeasuredReason::OmcUnavailable {
                detail: "No such file or directory (os error 2)".to_string(),
            });
        let line = measurement.summary_line(566);
        assert!(
            line.contains(PARITY_UNMEASURED_HEADLINE),
            "unmeasured summary must carry the fixed headline, got: {line}"
        );
        assert!(
            line.contains("omc is not available on PATH"),
            "unmeasured summary must name the reason, got: {line}"
        );
        assert!(
            !line.contains('%'),
            "an unmeasured run must never print a parity percentage, got: {line}"
        );
    }

    #[test]
    fn unmeasured_is_a_gate_failure_reason() {
        let measurement =
            MslParityMeasurement::unmeasured(MslParityUnmeasuredReason::ReferenceAbsent {
                path: "/tmp/omc_simulation_reference.json".to_string(),
            });
        let reason = measurement
            .gate_failure_reason()
            .expect("an unmeasured Tier 2 run must fail the gate");
        assert!(reason.contains(PARITY_UNMEASURED_HEADLINE));
        assert!(reason.contains("/tmp/omc_simulation_reference.json"));
    }

    #[test]
    fn measured_summary_line_quotes_the_strict_high_band_over_the_cohort() {
        let measurement = measured_with(gate_input_with(
            Some("OpenModelica 1.25.0"),
            Some(stats_with(48, 38)),
        ));
        let line = measurement.summary_line(566);
        assert!(line.contains("strict-high 38/566"), "got: {line}");
        assert!(line.contains("models_compared 48"), "got: {line}");
    }

    #[test]
    fn a_stage_skip_reason_wins_over_the_on_disk_state() {
        let stage = MslParityStageOutcome::DidNotRun(MslParityUnmeasuredReason::OmcUnavailable {
            detail: "omc not found".to_string(),
        });
        let measurement = measure_msl_parity(&stage, 566);
        assert_eq!(
            measurement.unmeasured_reason(),
            Some(&MslParityUnmeasuredReason::OmcUnavailable {
                detail: "omc not found".to_string()
            }),
            "the stage's own reason must survive to the summary"
        );
    }

    #[test]
    fn the_quoted_strict_high_count_comes_from_the_band_table() {
        let stats = stats_with(48, 38);
        let cohort = cohort_for(&stats);
        assert_eq!(cohort.table.strict_high_models(), 38);
        assert_eq!(cohort.table.models_compared(), 48);

        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats)),
            cohort,
        );

        assert_eq!(measurement.strict_high_models(), Some(38));
        assert_eq!(measurement.models_compared(), Some(48));
    }

    #[test]
    fn a_band_table_that_disagrees_with_the_reference_is_not_measured() {
        // The reference claims 38 strict-high over 48 compared; the table on
        // disk holds a different population. One artifact is stale, so neither
        // may be quoted.
        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats_with(48, 38))),
            MslCohortReading {
                table: cohort_table(37, 11),
                transitions: None,
                previous_not_diffable: None,
                persisted: true,
            },
        );

        assert!(!measurement.is_measured());
        let detail = measurement
            .unmeasured_reason()
            .expect("a stale band table must demote the reading")
            .detail();
        assert!(detail.contains("37 strict-high"), "got: {detail}");
        assert!(detail.contains("reference reports 38"), "got: {detail}");
        assert!(measurement.gate_failure_reason().is_some());
    }

    #[test]
    fn a_band_table_over_a_different_compared_set_is_not_measured() {
        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats_with(48, 38))),
            MslCohortReading {
                table: cohort_table(38, 9),
                transitions: None,
                previous_not_diffable: None,
                persisted: true,
            },
        );

        let detail = measurement
            .unmeasured_reason()
            .expect("a table over a different cohort must demote the reading")
            .detail();
        assert!(detail.contains("has 47 compared models"), "got: {detail}");
    }

    #[test]
    fn a_measured_summary_states_the_cohort_movement_against_the_previous_run() {
        let stats = stats_with(48, 38);
        let previous = cohort_table(38, 10);
        let current = cohort_table(38, 10);
        let mut cohort = cohort_for(&stats);
        cohort.transitions = Some(band_table::diff_band_tables(&previous, &current));

        let line = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats)),
            cohort,
        )
        .summary_line(566);

        assert!(
            line.contains("cohort: entered 0, left 0, band-changed 0"),
            "a measured run must always state cohort movement, got: {line}"
        );
    }

    #[test]
    fn a_run_with_no_previous_table_says_so_rather_than_printing_zero_movement() {
        let line = measured_with(gate_input_with(
            Some("OpenModelica 1.25.0"),
            Some(stats_with(48, 38)),
        ))
        .summary_line(566);

        assert!(
            line.contains("no previous band table to diff against"),
            "a first certification must say it has nothing to diff, got: {line}"
        );
        assert!(
            !line.contains("left 0"),
            "an absent previous table must not be rendered as zero movement, got: {line}"
        );
    }

    #[test]
    fn departures_since_the_previous_run_are_listed_with_their_exit_reason() {
        let previous = cohort_table(2, 0);
        let current = band_table::derive_band_table(
            &serde_json::json!({
                "models": { "High0": band_metric("High0", 10, 0) },
                "missing_trace": {},
                "skipped": {}
            }),
            Some(&serde_json::json!({
                "git_commit": "cert1234",
                "sim_target_models": ["High0", "High1"],
                "model_results": [
                    {"model_name": "High0", "phase_reached": "Success", "sim_status": "sim_ok"},
                    {"model_name": "High1", "phase_reached": "Success", "sim_status": "sim_solver_fail"}
                ]
            })),
            &std::collections::BTreeMap::new(),
            fixtures::fixture_meta("current"),
        )
        .expect("derive current table");
        let cohort = MslCohortReading {
            transitions: Some(band_table::diff_band_tables(&previous, &current)),
            table: current,
            previous_not_diffable: None,
            persisted: true,
        };

        let lines = cohort.departure_lines();

        assert_eq!(lines.len(), 1, "got: {lines:?}");
        assert!(
            lines[0].contains("LEFT the compared set: High1"),
            "got: {lines:?}"
        );
        assert!(lines[0].contains("was high"), "got: {lines:?}");
        assert!(lines[0].contains("sim_failed"), "got: {lines:?}");
        assert!(cohort.summary_clause().contains("left 1"));
    }

    /// The consistency check used to read only `models_compared` and
    /// `agreement_high`. Two artifacts that agree on those and disagree on the
    /// near/deviation split are still two different comparator outputs — and the
    /// summary quotes those two counts too.
    #[test]
    fn a_band_table_that_splits_near_and_deviation_differently_is_not_measured() {
        let mut stats = stats_with(48, 38);
        stats.agreement_minor = 4;
        stats.agreement_deviation = 6;

        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats)),
            // 38 strict-high over 48 compared, as the reference says — but every
            // remaining model is near, none deviating.
            MslCohortReading {
                table: cohort_table(38, 10),
                transitions: None,
                previous_not_diffable: None,
                persisted: true,
            },
        );

        assert!(!measurement.is_measured());
        let detail = measurement
            .unmeasured_reason()
            .expect("a mismatched band split must demote the reading")
            .detail();
        assert!(detail.contains("near models"), "got: {detail}");
        assert!(detail.contains("reference reports 4"), "got: {detail}");
    }

    /// A model that held the strict-high band and is no longer compared is a
    /// regression in the only band the project quotes. It cannot be seen in any
    /// aggregate — one leaving and one entering holds `agreement_high` flat — so
    /// the gate reads the per-model table for it.
    #[test]
    fn a_strict_high_model_that_left_the_compared_set_fails_the_gate() {
        let previous = cohort_table_tagged("previous", 2, 0);
        let current = band_table::derive_band_table(
            &serde_json::json!({
                "models": {
                    "High0": band_metric("High0", 10, 0),
                    "High2": band_metric("High2", 10, 0)
                },
                "missing_trace": {},
                "skipped": {}
            }),
            Some(&serde_json::json!({
                "git_commit": "cert1234",
                "sim_target_models": ["High0", "High1", "High2"],
                "model_results": [
                    {"model_name": "High0", "phase_reached": "Success", "sim_status": "sim_ok"},
                    {"model_name": "High1", "phase_reached": "Success", "sim_status": "sim_solver_fail"},
                    {"model_name": "High2", "phase_reached": "Success", "sim_status": "sim_ok"}
                ]
            })),
            &std::collections::BTreeMap::new(),
            fixtures::fixture_meta("current"),
        )
        .expect("derive current table");
        let transitions = band_table::diff_band_tables(&previous, &current);
        // The headline is flat: two strict-high models before, two after.
        assert_eq!(previous.strict_high_models(), current.strict_high_models());

        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats_with(2, 2))),
            MslCohortReading {
                table: current,
                transitions: Some(transitions),
                previous_not_diffable: None,
                persisted: true,
            },
        );

        let reason = measurement
            .departed_strict_high_reason()
            .expect("a strict-high departure must fail the run");
        assert!(reason.contains("High1"), "got: {reason}");
        assert!(reason.contains("sim_failed"), "got: {reason}");
    }

    /// The departed-strict-high rule reads `transitions`, so a run with no
    /// predecessor has it switched off. A certification cannot pass with its
    /// movement rules inert: the absence is itself a gate reason.
    #[test]
    fn a_cohort_run_with_no_predecessor_table_fails_rather_than_skipping_the_movement_rules() {
        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats_with(2, 2))),
            MslCohortReading {
                table: cohort_table(2, 0),
                transitions: None,
                previous_not_diffable: None,
                persisted: true,
            },
        );

        let reason = measurement
            .cohort_movement_unverified_reason()
            .expect("a cohort run that cannot diff must fail, not pass quietly");
        assert!(
            reason.contains("no predecessor band table"),
            "got: {reason}"
        );
        assert!(
            reason.contains("left the strict-high band"),
            "the reason must name the rule that is inert, got: {reason}"
        );
        assert!(
            measurement.departed_strict_high_reason().is_none(),
            "the departure rule genuinely cannot fire here; that is the point"
        );
    }

    /// A focused run makes no cohort claim, so it is not held to the predecessor
    /// requirement.
    #[test]
    fn a_focused_run_is_not_required_to_have_a_predecessor() {
        let mut table = cohort_table(2, 0);
        table.run_scope = BandTableRunScope::Partial;
        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats_with(2, 2))),
            MslCohortReading {
                table,
                transitions: None,
                previous_not_diffable: None,
                persisted: false,
            },
        );

        assert!(measurement.cohort_movement_unverified_reason().is_none());
    }

    /// A predecessor that could not be diffed against is not the same as none,
    /// and the gate reason carries the stated reason.
    #[test]
    fn a_predecessor_that_could_not_be_diffed_names_why_in_the_gate_reason() {
        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats_with(2, 2))),
            MslCohortReading {
                table: cohort_table(2, 0),
                transitions: None,
                previous_not_diffable: Some(
                    "run_scope_mismatch: the previous table is 'partial' over 20 models"
                        .to_string(),
                ),
                persisted: true,
            },
        );

        let reason = measurement
            .cohort_movement_unverified_reason()
            .expect("a stripe predecessor is still no predecessor");
        assert!(reason.contains("run_scope_mismatch"), "got: {reason}");
    }

    /// SPEC 0033 requires the skipped and missing counts beside `models_compared`:
    /// the same compared count means something different depending on how many
    /// candidates were dropped to reach it.
    #[test]
    fn the_summary_states_the_skipped_and_missing_counts_beside_models_compared() {
        let stats = stats_with(48, 38);
        let line = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats.clone())),
            cohort_for(&stats),
        )
        .summary_line(566);

        assert!(line.contains("models_compared 48"), "got: {line}");
        assert!(line.contains("skipped "), "got: {line}");
        assert!(line.contains("missing-trace "), "got: {line}");
        assert!(line.contains("not-attempted "), "got: {line}");
    }

    #[test]
    fn a_run_with_no_strict_high_departure_has_no_gate_reason() {
        let table = cohort_table(2, 0);
        let transitions = band_table::diff_band_tables(&table, &table);

        let measurement = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats_with(2, 2))),
            MslCohortReading {
                table,
                transitions: Some(transitions),
                previous_not_diffable: None,
                persisted: true,
            },
        );

        assert!(measurement.departed_strict_high_reason().is_none());
    }

    /// A focused (Tier 1) run that would displace the cohort table writes
    /// nothing, and the summary says so rather than implying the run produced a
    /// cohort table.
    #[test]
    fn a_focused_run_states_that_it_wrote_no_cohort_table() {
        let stats = stats_with(48, 38);
        let cohort = MslCohortReading {
            table: cohort_for(&stats).table,
            transitions: None,
            previous_not_diffable: None,
            persisted: false,
        };

        let line = MslParityMeasurement::measured(
            gate_input_with(Some("OpenModelica 1.25.0"), Some(stats)),
            cohort,
        )
        .summary_line(566);

        assert!(
            line.contains("focused run declined to displace the cohort table"),
            "got: {line}"
        );
        assert!(
            !line.contains("no previous band table to diff against"),
            "a focused run must not read as a first certification, got: {line}"
        );
    }

    /// A model that stays compared over a fraction of its channels keeps its
    /// band. The summary lists it beside the departures.
    #[test]
    fn a_coverage_drop_is_listed_beside_the_departures() {
        let wide = band_table::derive_band_table(
            &serde_json::json!({
                "models": { "Wide": band_metric("Wide", 165, 0) },
                "missing_trace": {},
                "skipped": {}
            }),
            None,
            &std::collections::BTreeMap::new(),
            fixtures::fixture_meta("wide"),
        )
        .expect("derive wide table");
        let narrow = band_table::derive_band_table(
            &serde_json::json!({
                "models": { "Wide": band_metric("Wide", 3, 0) },
                "missing_trace": {},
                "skipped": {}
            }),
            None,
            &std::collections::BTreeMap::new(),
            fixtures::fixture_meta("narrow"),
        )
        .expect("derive narrow table");
        let cohort = MslCohortReading {
            transitions: Some(band_table::diff_band_tables(&wide, &narrow)),
            table: narrow,
            previous_not_diffable: None,
            persisted: true,
        };

        let lines = cohort.coverage_drop_lines();

        assert_eq!(lines.len(), 1, "got: {lines:?}");
        assert!(
            lines[0].contains("COVERAGE-DROPPED: Wide"),
            "got: {lines:?}"
        );
        assert!(lines[0].contains("over 3 channels"), "got: {lines:?}");
        assert!(lines[0].contains("was 165"), "got: {lines:?}");
        assert!(cohort.departed_strict_high().is_empty());
    }
}
