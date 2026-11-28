//! Per-model states of a quality snapshot.
//!
//! The ratchet judges a run by what happened to each model, not by aggregate
//! counts: an aggregate cannot tell a model that got worse from one that newly
//! compiles and then fails, or from one that is newly scored. So a snapshot
//! records, for every sim target, the furthest stage it reached, how its trace
//! was judged (a band, or why it was not scored), how many channels were
//! compared, whether it is certified or on the unexcepted roster, and each
//! watchdog stage's elapsed time. The ratchet compares these per model against
//! the promoted baseline's.

use super::*;
use rumoca_msl_quality::{
    BandLabel, ModelStage, ModelState, ModelStates, StageSeconds, TraceState, UnscoredCause,
    failed_compile_phase,
};
use rumoca_test_msl::msl_tools::band_table::{BandRow, ExitReason};
use rumoca_test_msl::msl_tools::common::typed_exception_kind;

/// The furthest stage a model reached, ranked by the one compile-phase order;
/// an unknown phase is refused rather than ranked.
fn stage_of(result: &MslModelResult) -> Result<(ModelStage, Option<String>), String> {
    let phase = failed_compile_phase(&result.phase_reached)
        .map_err(|error| format!("{}: {error}", result.model_name))?;
    if let Some(phase) = phase {
        return Ok((ModelStage::FailedAt(phase), result.error_code.clone()));
    }
    Ok(
        match (result.ic_status.as_deref(), result.sim_status.as_deref()) {
            (_, Some("sim_ok")) => (ModelStage::Simulated, None),
            (Some("ic_ok"), status) => (ModelStage::Initialized, status.map(str::to_string)),
            (ic, sim) => (ModelStage::Compiled, ic.or(sim).map(str::to_string)),
        },
    )
}

fn trace_of(row: Option<&BandRow>) -> Result<TraceState, String> {
    let Some(row) = row else {
        return Ok(TraceState::Unscored(UnscoredCause::NotSimulated));
    };
    if row.band.is_compared() {
        return Ok(TraceState::Scored(row.band));
    }
    Ok(TraceState::Unscored(match row.exit_reason {
        // The exclusion's kind is part of its state: a row that moves between
        // kinds is a change only a reviewed boundary makes.
        Some(ExitReason::Excluded) => UnscoredCause::PolicyExcluded(
            row.exit_detail
                .as_deref()
                .and_then(typed_exception_kind)
                .ok_or_else(|| format!("{}: exclusion without a typed kind", row.model_name))?,
        ),
        Some(ExitReason::ReferenceMissing) => UnscoredCause::OmcFailed,
        Some(ExitReason::RumocaTraceMissing) => UnscoredCause::RumocaTraceMissing,
        Some(ExitReason::TraceNonidentifiable) => UnscoredCause::TraceNonidentifiable,
        Some(ExitReason::SimFailed | ExitReason::NotAttempted) => UnscoredCause::NotSimulated,
        // Everything else left a simulated model unscored without naming the
        // OMC side; it counts as rumoca's until the record says otherwise.
        Some(
            ExitReason::ComparatorFailed
            | ExitReason::NoComparableSamples
            | ExitReason::TraceMissingSideUnrecorded
            | ExitReason::NotCompared,
        )
        | None => UnscoredCause::RumocaTraceError,
    }))
}

/// The worker's name for the stage a watchdog killed, refused when the
/// worker's phase does not serialize as a name.
///
/// A kill is recorded in one of two places: the worker's `timeout_phase` when
/// the harness stopped the worker, or `sim_status: sim_timeout` when the run
/// stage's own budget stopped the simulation inside it. Both are the Sim
/// stage's watchdog.
fn timeout_phase_of(result: &MslModelResult) -> Result<Option<String>, String> {
    let phase = match result.timeout_phase {
        Some(phase) => phase,
        None if result.sim_status.as_deref() == Some("sim_timeout") => {
            rumoca_worker::WorkerProgressPhase::Sim
        }
        None => return Ok(None),
    };
    let phase = &phase;
    let value = serde_json::to_value(phase).map_err(|error| error.to_string())?;
    value
        .as_str()
        .map(|name| Some(name.to_string()))
        .ok_or_else(|| {
            format!(
                "{}: timeout phase is not a name: {value}",
                result.model_name
            )
        })
}

/// The state of every sim target, keyed by model name in target order.
pub(super) fn model_states(
    targets: &[String],
    results: &[MslModelResult],
    rows: &[BandRow],
    certified: &IndexSet<String>,
    roster: &IndexSet<String>,
    masses: &BTreeMap<String, f64>,
) -> Result<ModelStates, String> {
    let results = results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect::<HashMap<_, _>>();
    let rows = rows
        .iter()
        .map(|row| (row.model_name.as_str(), row))
        .collect::<HashMap<_, _>>();
    targets
        .iter()
        .map(|model| {
            let result = results
                .get(model.as_str())
                .ok_or_else(|| format!("sim target {model} has no model result"))?;
            let (stage, outcome) = stage_of(result)?;
            let row = rows.get(model.as_str()).copied();
            let trace = trace_of(row)?;
            let (trace_violation_mass, mean_channel_bounded_l1) = match trace {
                TraceState::Scored(_) => (
                    Some(
                        masses
                            .get(model.as_str())
                            .copied()
                            .ok_or_else(|| format!("{model}: scored without a violation mass"))?,
                    ),
                    Some(
                        row.and_then(|row| row.mean_channel_bounded_normalized_l1)
                            .ok_or_else(|| {
                                format!("{model}: scored without a mean channel error")
                            })?,
                    ),
                ),
                TraceState::Unscored(_) => (None, None),
            };
            let state = ModelState {
                stage,
                outcome,
                trace,
                compared_channels: row.map_or(0, |row| row.compared_variables),
                trace_violation_mass,
                mean_channel_bounded_l1,
                certified: certified.contains(model),
                roster: roster.contains(model),
                stage_seconds: StageSeconds {
                    build: result.sim_build_seconds,
                    initialization: result.ic_seconds,
                    run: result.sim_run_seconds,
                    timeout_phase: timeout_phase_of(result)?,
                    // Rule B fills these once the run's OMC timings are read.
                    ..StageSeconds::default()
                },
            };
            Ok((model.clone(), state))
        })
        .collect()
}

/// Insert the strict-high models that timing holds back (`timing_margin_models`)
/// and the rest, the certified roster (`certified_strict_high_models`), and
/// return the certified roster.
pub(super) fn insert_certified_rosters(
    root: &mut serde_json::Map<String, serde_json::Value>,
    summary: &MslSummary,
    rows: &[BandRow],
    phase_budgets: PhaseBudgets,
) -> io::Result<IndexSet<String>> {
    let strict_high = rows
        .iter()
        .filter(|row| row.band == BandLabel::High)
        .map(|row| row.model_name.as_str());
    let timing_margin = timing_margin_models(summary, strict_high.clone(), phase_budgets);
    let certified = strict_high
        .filter(|model| !timing_margin.contains_key(*model))
        .map(str::to_string)
        .collect::<IndexSet<_>>();
    root.insert(
        "timing_margin_models".to_string(),
        serde_json::to_value(timing_margin).map_err(|error| {
            io::Error::other(format!("failed to serialize timing-margin roster: {error}"))
        })?,
    );
    root.insert(
        "certified_strict_high_models".to_string(),
        serde_json::to_value(&certified).map_err(|error| {
            io::Error::other(format!(
                "failed to serialize certified strict-high roster: {error}"
            ))
        })?,
    );
    Ok(certified)
}

/// Insert `model_states`, given the certified and unexcepted rosters the
/// snapshot was just given.
pub(super) fn insert_model_states(
    root: &mut serde_json::Map<String, serde_json::Value>,
    summary: &MslSummary,
    rows: &[BandRow],
    (certified, roster): (&IndexSet<String>, &IndexSet<String>),
    masses: &BTreeMap<String, f64>,
) -> io::Result<()> {
    let states = model_states(
        &summary.sim_target_models,
        &summary.model_results,
        rows,
        certified,
        roster,
        masses,
    )
    .map_err(io::Error::other)?;
    let value = serde_json::to_value(states)
        .map_err(|error| io::Error::other(format!("failed to serialize model states: {error}")))?;
    root.insert("model_states".to_string(), value);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn row(model: &str, band: &str, exit: Option<&str>, compared: usize) -> BandRow {
        let mut row = serde_json::json!({
            "model_name": model, "band": band, "compared_variables": compared,
        });
        if band != "absent" {
            row["mean_channel_bounded_normalized_l1"] = serde_json::json!(1.0e-3);
        }
        if let Some(exit) = exit {
            row["exit_reason"] = serde_json::json!(exit);
        }
        serde_json::from_value(row).expect("band row")
    }

    fn excluded_row(model: &str, detail: &str) -> BandRow {
        let mut row = row(model, "absent", Some("excluded"), 0);
        row.exit_detail = Some(detail.to_string());
        row
    }

    fn result(model: &str, phase: &str, ic: Option<&str>, sim: Option<&str>) -> MslModelResult {
        let mut result = phase_error_result(model.to_string(), phase, None, None);
        result.ic_status = ic.map(str::to_string);
        result.sim_status = sim.map(str::to_string);
        result
    }

    fn states(results: Vec<MslModelResult>, rows: Vec<BandRow>) -> ModelStates {
        let targets = results
            .iter()
            .map(|r| r.model_name.clone())
            .collect::<Vec<_>>();
        let certified = IndexSet::from(["High".to_string()]);
        let roster = IndexSet::from(["Dev".to_string()]);
        // Every scored model carries a mass; the value only matters to the floors.
        let masses = rows
            .iter()
            .map(|row| (row.model_name.clone(), 0.5))
            .collect::<BTreeMap<_, _>>();
        model_states(&targets, &results, &rows, &certified, &roster, &masses).expect("states")
    }

    #[test]
    fn a_model_s_stage_is_the_furthest_it_got() {
        let states = states(
            vec![
                result("Flat", "Flatten", None, None),
                result("IcFail", "Success", Some("ic_solver_fail"), None),
                result("Timeout", "Success", Some("ic_ok"), Some("sim_timeout")),
                result("High", "Success", Some("ic_ok"), Some("sim_ok")),
            ],
            vec![row("High", "high", None, 12)],
        );
        assert_eq!(
            states["Flat"].stage,
            ModelStage::FailedAt(rumoca_msl_quality::CompilePhase::Flatten)
        );
        assert_eq!(states["IcFail"].stage, ModelStage::Compiled);
        assert_eq!(states["IcFail"].outcome.as_deref(), Some("ic_solver_fail"));
        assert_eq!(states["Timeout"].stage, ModelStage::Initialized);
        assert_eq!(states["Timeout"].outcome.as_deref(), Some("sim_timeout"));
        assert_eq!(states["High"].stage, ModelStage::Simulated);
        assert!(states["Flat"].stage < ModelStage::Compiled);
        assert!(ModelStage::Initialized < ModelStage::Simulated);
        assert_eq!(states["High"].trace, TraceState::Scored(BandLabel::High));
        assert_eq!(states["High"].compared_channels, 12);
        assert!(states["High"].certified);
    }

    #[test]
    fn an_unscored_model_names_who_left_it_unscored() {
        let simulated = |model: &str| result(model, "Success", Some("ic_ok"), Some("sim_ok"));
        let states = states(
            vec![
                simulated("Excluded"),
                simulated("OmcGone"),
                simulated("NoTrace"),
                simulated("Nonident"),
                simulated("Broken"),
                simulated("Dev"),
            ],
            vec![
                excluded_row("Excluded", "model_issue: chaotic"),
                row("OmcGone", "absent", Some("reference_missing"), 0),
                row("NoTrace", "absent", Some("rumoca_trace_missing"), 0),
                row("Nonident", "absent", Some("trace_nonidentifiable"), 0),
                row("Broken", "absent", Some("comparator_failed"), 0),
                row("Dev", "deviation", None, 85),
            ],
        );
        let cause = |model: &str| states[model].trace;
        assert_eq!(
            cause("Excluded"),
            TraceState::Unscored(UnscoredCause::PolicyExcluded(
                rumoca_msl_quality::TraceExceptionKind::ModelIssue
            ))
        );
        assert_eq!(
            cause("OmcGone"),
            TraceState::Unscored(UnscoredCause::OmcFailed)
        );
        assert_eq!(
            cause("NoTrace"),
            TraceState::Unscored(UnscoredCause::RumocaTraceMissing)
        );
        assert_eq!(
            cause("Nonident"),
            TraceState::Unscored(UnscoredCause::TraceNonidentifiable)
        );
        assert_eq!(
            cause("Broken"),
            TraceState::Unscored(UnscoredCause::RumocaTraceError)
        );
        assert_eq!(cause("Dev"), TraceState::Scored(BandLabel::Deviation));
        assert!(states["Dev"].roster);
    }

    #[test]
    fn an_unknown_phase_or_an_untyped_exclusion_is_refused() {
        let targets = ["M".to_string()];
        let refused = |result: MslModelResult, rows: Vec<BandRow>| {
            model_states(
                &targets,
                &[result],
                &rows,
                &IndexSet::new(),
                &IndexSet::new(),
                &BTreeMap::new(),
            )
            .unwrap_err()
        };
        assert!(refused(result("M", "Lowering", None, None), vec![]).contains("Lowering"));
        let simulated = result("M", "Success", Some("ic_ok"), Some("sim_ok"));
        let untyped = excluded_row("M", "no kind here");
        assert!(refused(simulated, vec![untyped]).contains("typed kind"));
    }

    #[test]
    fn a_run_stage_timeout_is_recorded_as_a_sim_kill() {
        // The run stage's own budget stops the simulation without a worker
        // timeout phase; it is still the Sim stage's watchdog.
        let states = states(
            vec![result(
                "Slow",
                "Success",
                Some("ic_ok"),
                Some("sim_timeout"),
            )],
            vec![],
        );
        assert_eq!(
            states["Slow"].stage_seconds.timeout_phase.as_deref(),
            Some("Sim")
        );
    }

    #[test]
    fn a_scored_model_without_a_violation_mass_is_refused() {
        let targets = ["M".to_string()];
        let simulated = result("M", "Success", Some("ic_ok"), Some("sim_ok"));
        let error = model_states(
            &targets,
            &[simulated],
            &[row("M", "high", None, 4)],
            &IndexSet::new(),
            &IndexSet::new(),
            &BTreeMap::new(),
        )
        .unwrap_err();
        assert!(error.contains("violation mass"), "{error}");
    }

    #[test]
    fn a_parse_failure_ranks_below_a_resolve_failure() {
        let states = states(
            vec![
                result("P", "Parse", None, None),
                result("R", "Resolve", None, None),
            ],
            vec![],
        );
        assert!(states["P"].stage < states["R"].stage);
    }

    #[test]
    fn every_sim_target_needs_a_model_result() {
        let error = model_states(
            &["Missing".to_string()],
            &[],
            &[],
            &IndexSet::new(),
            &IndexSet::new(),
            &BTreeMap::new(),
        )
        .unwrap_err();
        assert!(error.contains("Missing"), "{error}");
    }
}
