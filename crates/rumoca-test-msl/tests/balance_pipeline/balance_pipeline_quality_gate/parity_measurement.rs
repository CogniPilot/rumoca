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
//! # Acceptance contract (SPEC 0008)
//!
//! [`MslParityMeasurement::measured`] is the only constructor of the `Measured`
//! variant, and it **accepts** exactly a reference that carries:
//!
//! * `omc_version` metadata (so the reading names the OMC it came from),
//! * `trace_accuracy_stats` with `models_compared > 0`,
//! * a strict-high band count that is representable as a fraction of
//!   `models_compared`.
//!
//! It **rejects** — by demoting to `Unmeasured(ReferenceIncomplete)`, never by
//! silently keeping a partial reading — a reference missing trace statistics, a
//! reference whose comparator compared zero models, and a reference with no OMC
//! version. Owner: this module; the on-disk shape it reads is written by
//! `rumoca-msl-tools omc-simulation-reference`.
//!
//! A demoted reading is not a soft failure: on a baseline-relative Tier 2 run
//! [`MslParityMeasurement::gate_failure_reason`] turns it into a gate reason, so
//! an incomplete reference fails the run rather than shrinking it to `sim_ok`.

use super::*;

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

/// A Tier 2 run's parity reading: either checked bands, or a named absence.
#[derive(Debug, Clone)]
pub(crate) enum MslParityMeasurement {
    Measured(Box<MslParityGateInput>),
    Unmeasured(MslParityUnmeasuredReason),
}

impl MslParityMeasurement {
    /// The only constructor of `Measured`. Demotes any reference that cannot
    /// support a band reading to `Unmeasured(ReferenceIncomplete)`; see the
    /// module acceptance contract.
    pub(crate) fn measured(input: MslParityGateInput) -> Self {
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
        Self::Measured(Box::new(input))
    }

    pub(crate) fn unmeasured(reason: MslParityUnmeasuredReason) -> Self {
        Self::Unmeasured(reason)
    }

    /// The checked reading, or `None` when this run has no parity number. Every
    /// consumer that reaches for band statistics goes through here, so the
    /// `Unmeasured` case has to be handled explicitly at each call site.
    pub(crate) fn gate_input(&self) -> Option<&MslParityGateInput> {
        match self {
            Self::Measured(input) => Some(input),
            Self::Unmeasured(_) => None,
        }
    }

    pub(crate) fn unmeasured_reason(&self) -> Option<&MslParityUnmeasuredReason> {
        match self {
            Self::Measured(_) => None,
            Self::Unmeasured(reason) => Some(reason),
        }
    }

    pub(crate) fn is_measured(&self) -> bool {
        matches!(self, Self::Measured(_))
    }

    /// Models in the strict-high agreement band — the only quotable parity
    /// count. `None` when the run has no reading at all.
    pub(crate) fn strict_high_models(&self) -> Option<usize> {
        self.gate_input()
            .and_then(|input| input.trace_accuracy_stats.as_ref())
            .map(|stats| stats.agreement_high)
    }

    pub(crate) fn models_compared(&self) -> Option<usize> {
        self.gate_input()
            .and_then(|input| input.trace_accuracy_stats.as_ref())
            .map(|stats| stats.models_compared)
    }

    /// The single line every Tier 2 run prints for parity. A run without a
    /// comparator reading prints [`PARITY_UNMEASURED_HEADLINE`] plus the reason;
    /// it never prints a number.
    pub(crate) fn summary_line(&self, sim_target_models: usize) -> String {
        match self {
            Self::Measured(input) => {
                let stats = input
                    .trace_accuracy_stats
                    .as_ref()
                    .expect("Measured is only constructed with trace_accuracy_stats");
                format!(
                    "MSL parity: strict-high {}/{} sim targets ({:.2}% of the cohort); \
                     bands minor {}, deviation {}; models_compared {} (omc={})",
                    stats.agreement_high,
                    sim_target_models,
                    percent_of(stats.agreement_high, sim_target_models),
                    stats.agreement_minor,
                    stats.agreement_deviation,
                    stats.models_compared,
                    input.omc_version.as_deref().unwrap_or("unknown"),
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
}

fn percent_of(count: usize, total: usize) -> f64 {
    if total == 0 {
        return 0.0;
    }
    count as f64 * 100.0 / total as f64
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
    match load_current_msl_parity_gate_input_required(expected_sim_target_models) {
        Ok(input) => MslParityMeasurement::measured(input),
        Err(error) => {
            MslParityMeasurement::unmeasured(MslParityUnmeasuredReason::ReferenceIncomplete {
                detail: error.to_string(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stats_with(models_compared: usize, agreement_high: usize) -> MslTraceAccuracyStatsBaseline {
        MslTraceAccuracyStatsBaseline {
            models_compared,
            missing_trace_models: 0,
            skipped_models: 0,
            agreement_high,
            agreement_high_percent: None,
            agreement_minor: 0,
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

    #[test]
    fn measured_requires_trace_statistics() {
        let measurement = MslParityMeasurement::measured(gate_input_with(Some("omc 1.0"), None));
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
        let measurement = MslParityMeasurement::measured(gate_input_with(
            Some("omc 1.0"),
            Some(stats_with(0, 0)),
        ));
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
        let measurement =
            MslParityMeasurement::measured(gate_input_with(None, Some(stats_with(5, 4))));
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
        let measurement = MslParityMeasurement::measured(gate_input_with(
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
        let measurement = MslParityMeasurement::measured(gate_input_with(
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
}
