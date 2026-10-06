//! Certification timing margin (SPEC_0050): a strict-high completion enters
//! the certified roster and the ratcheted counts only when its evidence run
//! used at most [`CERTIFICATION_TIMING_MARGIN`] of every phase budget it ran
//! under. A strict-high model above the margin stays simulated and reported,
//! uncertified with the typed reason `timing_margin`.

use super::*;

/// The largest share of a phase budget a certified completion may use.
///
/// Two CI runs of one tree measured 68 completions on both; the per-model
/// ratio of their worst phase-budget shares reached 1.73 (90th percentile
/// 1.36). A completion at half of its budget therefore survives a 2x slower
/// runner, beyond every observed spread.
pub(super) const CERTIFICATION_TIMING_MARGIN: f64 = 0.5;

/// Why a strict-high completion is not certified: the phase whose budget it
/// used the largest share of, and that share.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct TimingMarginEntry {
    pub(super) phase: String,
    pub(super) budget_share: f64,
}

/// The phase budgets of the evidence run: every worker phase has the model
/// attempt budget except the simulation run, which the solver bounds.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct PhaseBudgets {
    pub(super) model_attempt_seconds: f64,
    pub(super) simulation_seconds: f64,
}

impl PhaseBudgets {
    pub(super) fn configured() -> Self {
        Self {
            model_attempt_seconds: model_attempt_timeout_secs(),
            simulation_seconds: sim_timeout_secs(),
        }
    }

    /// The phase of `result` that used the largest share of its budget.
    pub(super) fn worst_share(self, result: &MslModelResult) -> Option<TimingMarginEntry> {
        let attempt = self.model_attempt_seconds;
        [
            ("Instantiate", result.instantiate_seconds, attempt),
            ("Typecheck", result.typecheck_seconds, attempt),
            ("Flatten", result.flatten_seconds, attempt),
            ("ToDae", result.dae_seconds, attempt),
            ("Solve", result.ir_solve_seconds, attempt),
            ("SimBuild", result.sim_backend_build_seconds, attempt),
            ("IC", result.ic_seconds, attempt),
            ("Sim", result.sim_run_seconds, self.simulation_seconds),
        ]
        .into_iter()
        .filter_map(|(phase, seconds, budget)| {
            Some(TimingMarginEntry {
                phase: phase.to_string(),
                budget_share: seconds? / budget,
            })
        })
        .max_by(|lhs, rhs| lhs.budget_share.total_cmp(&rhs.budget_share))
    }
}

/// The strict-high models of `strict_high` whose evidence run in `summary`
/// used more than the margin of a phase budget, with their worst phase.
pub(super) fn timing_margin_models<'a>(
    summary: &MslSummary,
    strict_high: impl IntoIterator<Item = &'a str>,
    budgets: PhaseBudgets,
) -> IndexMap<String, TimingMarginEntry> {
    let results = summary
        .model_results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect::<HashMap<_, _>>();
    strict_high
        .into_iter()
        .filter_map(|model| {
            let worst = budgets.worst_share(results.get(model)?)?;
            (worst.budget_share > CERTIFICATION_TIMING_MARGIN).then(|| (model.to_string(), worst))
        })
        .collect()
}

/// The ratcheted value of a count a baseline records: its measured value less
/// the strict-high completions the timing margin leaves uncertified, each of
/// which the count includes.
pub(super) fn certified_floor(baseline: &MslQualityBaseline, measured: usize) -> usize {
    measured.saturating_sub(baseline.timing_margin_models.len())
}
