//! Two-run fixtures for the band table.
//!
//! The load-bearing case is [`a_model_that_silently_leaves_the_compared_set_is_reported`]:
//! a model that is strict-high in run A and absent from run B's
//! comparator output must appear in the diff as a LEFT event with its exit
//! reason. That is the regression `DCPM_Start` slipped through.
//!
//! The rest of this file pins the properties an artifact needs before it can
//! *be* that evidence: every cohort target has a row, a table is bound to the
//! comparator output it describes, re-persisting one certification does not
//! diff it against itself, a focused run cannot consume the cohort baseline,
//! and each recorded non-comparison keeps the boundary that produced it.

use super::*;
use serde_json::json;
use std::collections::BTreeMap;

fn metric(model_name: &str, high: usize, minor: usize, deviation: usize, max_dev: f64) -> Value {
    metric_over(model_name, high, minor, deviation, max_dev)
}

fn metric_over(
    model_name: &str,
    high: usize,
    minor: usize,
    deviation: usize,
    max_dev: f64,
) -> Value {
    json!({
        "model_name": model_name,
        "compared_variables": high + minor + deviation,
        "samples_compared": 100,
        "bounded_normalized_l1_score": max_dev / 2.0,
        "mean_channel_bounded_normalized_l1": max_dev / 2.0,
        "max_channel_bounded_normalized_l1": max_dev,
        "channel_high_count": high,
        "channel_minor_count": minor,
        "channel_deviation_count": deviation,
        "channel_severe_count": 0,
        "worst_variables": []
    })
}

fn trace_payload(models: Value, missing: Value, skipped: Value) -> Value {
    json!({
        "models": models,
        "missing_trace": missing,
        "skipped": skipped
    })
}

/// A comparator-recorded non-comparison in the shape a current run writes.
fn exit(kind: &str, detail: &str) -> Value {
    json!({ "kind": kind, "detail": detail })
}

/// `msl_results.json` for a run whose cohort roster is exactly `entries`.
///
/// `None` as the status is a target the run never simulated — the 467-of-566
/// case on the real sweep.
fn results_payload(entries: &[(&str, Option<&str>)]) -> Value {
    let targets = entries
        .iter()
        .map(|(model_name, _)| *model_name)
        .collect::<Vec<_>>();
    results_payload_with_targets(entries, &targets)
}

fn results_payload_with_targets(entries: &[(&str, Option<&str>)], targets: &[&str]) -> Value {
    json!({
        "git_commit": "cert1234",
        "sim_target_models": targets,
        "model_results": entries
            .iter()
            .map(|(model_name, sim_status)| match sim_status {
                Some(status) => json!({
                    "model_name": model_name,
                    "phase_reached": "Success",
                    "sim_status": status,
                    "sim_error_code": "ER900",
                    "sim_error": "solver diverged\nmore detail"
                }),
                None => json!({
                    "model_name": model_name,
                    "phase_reached": "ToDae",
                    "error": "unsupported operator\nsecond line"
                }),
            })
            .collect::<Vec<_>>()
    })
}

/// Provenance for an in-memory fixture. The digest is what binds a table to one
/// comparator output, so fixtures carry a distinct one per run.
fn meta(tag: &str) -> BandTableMeta {
    BandTableMeta {
        run_scope: BandTableRunScope::Full,
        git_commit: "cert1234".to_string(),
        // A fixture stands for a clean-tree certification.
        working_tree_digest: None,
        omc_version: Some("OpenModelica 1.25.0".to_string()),
        source: BandTableSource {
            trace_comparison_file: format!("{tag}/sim_trace_comparison.json"),
            trace_comparison_digest: format!("digest-{tag}"),
            results_file: format!("{tag}/msl_results.json"),
            results_digest: format!("results-digest-{tag}"),
            exclusions_file: "msl_trace_compare_exclusions.json".to_string(),
            exclusions_digest: "exclusions-digest".to_string(),
        },
    }
}

fn derive_tagged(tag: &str, trace: &Value, results: &Value) -> BandTable {
    derive_band_table(trace, Some(results), &BTreeMap::new(), meta(tag)).expect("derive band table")
}

fn derive(trace: &Value, results: &Value) -> BandTable {
    derive_tagged("fixture", trace, results)
}

/// Run A: `Alpha` is strict-high, `Beta` deviates, `Gamma` is excluded.
fn run_a() -> BandTable {
    let artifacts = run_a_artifacts();
    derive_tagged("run-a", &artifacts.trace, &artifacts.results)
}

/// Run B: `Alpha` regressed to `sim_solver_fail` and vanished from the
/// comparator's `models` map; `Delta` joined; `Beta` improved to strict-high.
fn run_b() -> BandTable {
    let artifacts = run_b_artifacts();
    derive_tagged("run-b", &artifacts.trace, &artifacts.results)
}

#[test]
fn compared_models_carry_their_band_channel_counts_and_max_deviation() {
    let table = run_a();

    let alpha = table.row("Alpha").expect("Alpha must have a row");
    assert_eq!(alpha.band, BandLabel::High);
    assert_eq!(alpha.channel_high_count, 10);
    assert_eq!(alpha.compared_variables, 10);
    assert_eq!(alpha.max_channel_bounded_normalized_l1, Some(1.0e-9));
    assert_eq!(alpha.bounded_normalized_l1_score, Some(0.5e-9));
    assert!(alpha.exit_reason.is_none());

    assert_eq!(
        table.row("Beta").map(|row| row.band),
        Some(BandLabel::Deviation)
    );
    assert_eq!(table.strict_high_models(), 1);
    assert_eq!(table.models_compared(), 2);
}

#[test]
fn every_cohort_model_outside_the_compared_set_carries_a_named_exit_reason() {
    let table = run_b();

    let alpha = table
        .row("Alpha")
        .expect("a departed model still gets a row");
    assert_eq!(alpha.band, BandLabel::Absent);
    assert_eq!(alpha.exit_reason, Some(ExitReason::SimFailed));
    let detail = alpha.exit_detail.as_deref().expect("exit detail");
    assert!(detail.contains("sim_solver_fail"), "got: {detail}");
    assert!(detail.contains("ER900"), "got: {detail}");

    let gamma = table.row("Gamma").expect("an excluded model gets a row");
    assert_eq!(gamma.exit_reason, Some(ExitReason::Excluded));

    assert_eq!(table.counts.absent, 2);
    assert_eq!(table.counts.absent_by_reason["sim_failed"], 1);
    assert_eq!(table.counts.absent_by_reason["excluded"], 1);
}

/// The review's F3: on the real 566-model sweep the table carried 99 rows —
/// only the models the comparator or the simulator happened to mention — so 467
/// cohort targets had no row at all and their absence was unaccounted for. The
/// cohort is the run's `sim_target_models` roster, and "never attempted" is an
/// exit reason like any other.
#[test]
fn every_cohort_target_gets_a_row_even_when_the_run_never_simulated_it() {
    let table = derive(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({}),
            json!({}),
        ),
        &results_payload_with_targets(
            &[
                ("Compared", Some("sim_ok")),
                ("NeverSimulated", None),
                ("Failed", Some("sim_solver_fail")),
            ],
            &["Compared", "NeverSimulated", "Failed", "NoResultAtAll"],
        ),
    );

    assert_eq!(
        table.counts.cohort_models, 4,
        "the cohort is the run's sim_target_models roster, not the comparator's map"
    );
    let never = table
        .row("NeverSimulated")
        .expect("a target the run never simulated still gets a row");
    assert_eq!(never.exit_reason, Some(ExitReason::NotAttempted));
    let detail = never.exit_detail.as_deref().expect("exit detail");
    assert!(detail.contains("ToDae"), "got: {detail}");
    assert_eq!(
        table.row("NoResultAtAll").and_then(|row| row.exit_reason),
        Some(ExitReason::NotAttempted),
        "a roster target with no result at all is still accounted for"
    );
    assert_eq!(table.counts.absent_by_reason["not_attempted"], 2);
    assert_eq!(table.counts.absent_by_reason["sim_failed"], 1);
}

/// A comparator that bands a model the run never targeted means the two
/// artifacts describe different populations; the table would not be this run's.
#[test]
fn a_comparator_row_outside_the_cohort_roster_is_rejected() {
    let error = derive_band_table(
        &trace_payload(
            json!({
                "InRoster": metric("InRoster", 10, 0, 0, 1.0e-9),
                "Stranger": metric("Stranger", 10, 0, 0, 1.0e-9),
            }),
            json!({}),
            json!({}),
        ),
        Some(&results_payload_with_targets(
            &[("InRoster", Some("sim_ok"))],
            &["InRoster"],
        )),
        &BTreeMap::new(),
        meta("mismatch"),
    )
    .expect_err("a comparator row outside the roster must be refused");

    assert!(format!("{error:#}").contains("Stranger"), "got: {error:#}");
    assert!(
        format!("{error:#}").contains("outside the run's sim_target_models roster"),
        "got: {error:#}"
    );
}

/// The review's F4: `skipped` carries both policy exclusions and comparator
/// failures. Recording a comparator crash as `excluded` files a defect as a
/// policy decision, and it stops looking like something to fix.
#[test]
fn a_comparator_failure_is_not_recorded_as_a_policy_exclusion() {
    let table = derive(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({}),
            json!({
                "Policy": exit("policy_excluded", "stochastic random-input model"),
                "Crashed": exit("comparator_failed", "trace compare failed: shape mismatch"),
            }),
        ),
        &results_payload(&[
            ("Compared", Some("sim_ok")),
            ("Policy", Some("sim_ok")),
            ("Crashed", Some("sim_ok")),
        ]),
    );

    assert_eq!(
        table.row("Policy").and_then(|row| row.exit_reason),
        Some(ExitReason::Excluded)
    );
    assert_eq!(
        table.row("Crashed").and_then(|row| row.exit_reason),
        Some(ExitReason::ComparatorFailed),
        "a comparator failure must not be filed as a policy exclusion"
    );
    assert_eq!(table.counts.absent_by_reason["comparator_failed"], 1);
    assert_eq!(table.counts.absent_by_reason["excluded"], 1);
}

/// The other half of F4: `missing_trace` carries both "rumoca produced no
/// trace" and "OMC has no reference". Filing our own gap as a missing OMC
/// reference points the investigation at the wrong tool.
#[test]
fn a_rumoca_trace_gap_is_not_recorded_as_a_missing_omc_reference() {
    let table = derive(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({
                "OursMissing": exit(
                    "rumoca_trace_missing",
                    "successful Rumoca attempt did not declare a trace file"
                ),
                "TheirsMissing": exit("omc_trace_missing", "declared OMC trace file does not exist"),
            }),
            json!({}),
        ),
        &results_payload(&[
            ("Compared", Some("sim_ok")),
            ("OursMissing", Some("sim_ok")),
            ("TheirsMissing", Some("sim_ok")),
        ]),
    );

    assert_eq!(
        table.row("OursMissing").and_then(|row| row.exit_reason),
        Some(ExitReason::RumocaTraceMissing),
        "our own trace gap must not be reported as a missing OMC reference"
    );
    assert_eq!(
        table.row("TheirsMissing").and_then(|row| row.exit_reason),
        Some(ExitReason::ReferenceMissing)
    );
}

/// A certification written before the comparator recorded kinds cannot say which
/// side a missing trace came from. The table says exactly that rather than
/// picking one.
#[test]
fn an_untyped_missing_trace_entry_does_not_claim_a_side() {
    let table = derive(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({ "Untyped": "failed to load omc trace: no such file" }),
            json!({}),
        ),
        &results_payload(&[("Compared", Some("sim_ok")), ("Untyped", Some("sim_ok"))]),
    );

    let untyped = table
        .row("Untyped")
        .expect("an untyped entry still gets a row");
    assert_eq!(
        untyped.exit_reason,
        Some(ExitReason::TraceMissingSideUnrecorded)
    );
    assert_eq!(
        untyped.exit_detail.as_deref(),
        Some("failed to load omc trace: no such file"),
        "the recorded text must survive even when the side does not"
    );
}

/// The exclusions argument used to be dead, so an excluded model that the
/// comparator never mentioned had no row and no reason.
#[test]
fn a_tracked_exclusion_supplies_the_reason_the_row_records() {
    let mut exclusions = BTreeMap::new();
    exclusions.insert(
        "Excluded".to_string(),
        "wall-clock dependent; excluded until a clock shim exists".to_string(),
    );
    let table = derive_band_table(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({}),
            json!({}),
        ),
        Some(&results_payload(&[
            ("Compared", Some("sim_ok")),
            ("Excluded", Some("sim_ok")),
        ])),
        &exclusions,
        meta("exclusions"),
    )
    .expect("derive with exclusions");

    let row = table.row("Excluded").expect("an excluded model gets a row");
    assert_eq!(row.exit_reason, Some(ExitReason::Excluded));
    assert_eq!(
        row.exit_detail.as_deref(),
        Some("wall-clock dependent; excluded until a clock shim exists"),
        "the row must carry that entry's own reason, not a shared constant"
    );
}

/// A certification written before the comparator recorded kinds cannot say
/// whether a `skipped` model was policy or a comparator crash — but the tracked
/// exclusion list can, and membership in it is a fact about the run's
/// configuration rather than a reading of the reason text.
#[test]
fn an_untyped_skip_is_attributed_by_the_tracked_exclusion_list_not_by_its_wording() {
    let mut exclusions = BTreeMap::new();
    exclusions.insert(
        "OnTheList".to_string(),
        "stochastic random-input model".to_string(),
    );
    let table = derive_band_table(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({}),
            json!({
                "OnTheList": "stochastic random-input model; skipped until seed parity",
                "NotOnTheList": "trace compare failed: shape mismatch",
            }),
        ),
        Some(&results_payload(&[
            ("Compared", Some("sim_ok")),
            ("OnTheList", Some("sim_ok")),
            ("NotOnTheList", Some("sim_ok")),
        ])),
        &exclusions,
        meta("untyped-skips"),
    )
    .expect("derive with untyped skips");

    assert_eq!(
        table.row("OnTheList").and_then(|row| row.exit_reason),
        Some(ExitReason::Excluded),
        "a model on the tracked list is a policy exclusion"
    );
    assert_eq!(
        table.row("NotOnTheList").and_then(|row| row.exit_reason),
        Some(ExitReason::ComparatorFailed),
        "the only other producer of `skipped` is a comparator failure, and a defect \
         must not be filed as policy"
    );
}

/// The comparator only reaches its exclusion check for models that simulated. A
/// model on the exclusion list that failed to simulate is recorded by that
/// failure — otherwise excluding a model would also hide its solver regression.
#[test]
fn an_excluded_model_that_never_simulated_is_recorded_by_its_sim_outcome() {
    let mut exclusions = BTreeMap::new();
    exclusions.insert("Broken".to_string(), "stochastic".to_string());
    exclusions.insert("NeverRan".to_string(), "stochastic".to_string());
    exclusions.insert("Simulated".to_string(), "stochastic".to_string());
    let table = derive_band_table(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({}),
            json!({}),
        ),
        Some(&results_payload(&[
            ("Compared", Some("sim_ok")),
            ("Broken", Some("sim_solver_fail")),
            ("NeverRan", None),
            ("Simulated", Some("sim_ok")),
        ])),
        &exclusions,
        meta("excluded-outcomes"),
    )
    .expect("derive with exclusions over failing models");

    assert_eq!(
        table.row("Broken").and_then(|row| row.exit_reason),
        Some(ExitReason::SimFailed),
        "an exclusion must not hide a solver failure"
    );
    assert_eq!(
        table.row("NeverRan").and_then(|row| row.exit_reason),
        Some(ExitReason::NotAttempted)
    );
    assert_eq!(
        table.row("Simulated").and_then(|row| row.exit_reason),
        Some(ExitReason::Excluded),
        "a model that simulated and was excluded from comparison is a policy exclusion"
    );
}

#[test]
fn a_model_that_silently_leaves_the_compared_set_is_reported() {
    let before = run_a();
    let after = run_b();

    let transitions = diff_band_tables(&before, &after);

    let left = transitions
        .left
        .iter()
        .find(|left| left.model_name == "Alpha")
        .expect("a model that left the compared set must be listed");
    assert_eq!(left.before_band, BandLabel::High);
    assert_eq!(left.exit_reason, ExitReason::SimFailed);
    assert!(
        left.exit_detail
            .as_deref()
            .is_some_and(|detail| detail.contains("sim_solver_fail")),
        "the LEFT row must carry the exit reason detail, got: {:?}",
        left.exit_detail
    );

    assert_eq!(transitions.counts.left, 1);
    assert_eq!(transitions.counts.left_by_reason["sim_failed"], 1);
    assert_eq!(
        transitions
            .entered
            .iter()
            .map(|entered| entered.model_name.as_str())
            .collect::<Vec<_>>(),
        vec!["Delta"]
    );
    assert_eq!(transitions.counts.entered, 1);
    assert_eq!(
        transitions.band_changed,
        vec![BandChangedModel {
            model_name: "Beta".to_string(),
            before_band: BandLabel::Deviation,
            after_band: BandLabel::High,
        }]
    );
    assert_eq!(transitions.counts.common_compared, 1);
    assert_eq!(
        transitions.departed_strict_high().count(),
        1,
        "a strict-high departure must be reachable on its own"
    );
}

#[test]
fn strict_high_totals_alone_hide_the_departure_the_diff_reports() {
    let before = run_a();
    let after = run_b();

    // Both runs report exactly one strict-high model, so the aggregate count is
    // flat. The population behind it is not: Alpha left, Delta entered.
    assert_eq!(before.strict_high_models(), after.strict_high_models());
    let transitions = diff_band_tables(&before, &after);
    assert!(
        transitions.counts.left > 0,
        "a flat strict-high total must not hide a departure"
    );
    assert!(transitions.summary_line().contains("left 1"));
}

/// The review's F5: a band is a *share* of the compared channels, so a model can
/// hold `high` while the evidence behind it collapses. `DCPM_Start` was compared
/// over 165 channels; a run comparing three of them and calling it the same band
/// is not the same claim.
#[test]
fn a_model_that_keeps_its_band_over_fewer_channels_is_reported_as_a_coverage_drop() {
    let before = derive_tagged(
        "wide",
        &trace_payload(
            json!({ "Wide": metric_over("Wide", 165, 0, 0, 1.0e-9) }),
            json!({}),
            json!({}),
        ),
        &results_payload(&[("Wide", Some("sim_ok"))]),
    );
    let after = derive_tagged(
        "narrow",
        &trace_payload(
            json!({ "Wide": metric_over("Wide", 3, 0, 0, 1.0e-9) }),
            json!({}),
            json!({}),
        ),
        &results_payload(&[("Wide", Some("sim_ok"))]),
    );

    let transitions = diff_band_tables(&before, &after);

    assert!(
        transitions.band_changed.is_empty(),
        "the band did not move; the evidence behind it did"
    );
    assert_eq!(transitions.counts.coverage_dropped, 1);
    assert_eq!(transitions.counts.compared_variables_lost, 162);
    assert_eq!(
        transitions.coverage_dropped[0],
        CoverageDroppedModel {
            model_name: "Wide".to_string(),
            before_compared_variables: 165,
            after_compared_variables: 3,
            band: BandLabel::High,
        }
    );
    assert!(
        transitions.summary_line().contains("coverage-dropped 1"),
        "got: {}",
        transitions.summary_line()
    );
}

#[test]
fn a_departure_with_no_row_in_the_candidate_table_is_still_reported() {
    let before = run_a();
    let after = derive_band_table(
        &trace_payload(
            json!({ "Beta": metric("Beta", 10, 0, 0, 1.0e-9) }),
            json!({}),
            json!({}),
        ),
        None,
        &BTreeMap::new(),
        meta("no-results"),
    )
    .expect("derive without a results file");

    let transitions = diff_band_tables(&before, &after);

    let left = transitions
        .left
        .iter()
        .find(|left| left.model_name == "Alpha")
        .expect("an unexplained disappearance must still be listed");
    assert_eq!(left.exit_reason, ExitReason::NotCompared);
    assert!(
        left.exit_detail
            .as_deref()
            .is_some_and(|detail| detail.contains("no row")),
        "an unexplained departure must say the run recorded no reason, got: {:?}",
        left.exit_detail
    );
}

#[test]
fn a_reference_missing_departure_is_distinguished_from_a_solver_regression() {
    let before = run_a();
    let after = derive_tagged(
        "omc-gap",
        &trace_payload(
            json!({ "Beta": metric("Beta", 10, 0, 0, 1.0e-9) }),
            json!({ "Alpha": exit("omc_trace_missing", "omc produced no trace") }),
            json!({}),
        ),
        &results_payload(&[("Alpha", Some("sim_ok")), ("Beta", Some("sim_ok"))]),
    );

    let transitions = diff_band_tables(&before, &after);

    assert_eq!(transitions.counts.left_by_reason["reference_missing"], 1);
    assert_eq!(
        transitions.left[0].exit_detail.as_deref(),
        Some("omc produced no trace")
    );
}

#[test]
fn ensure_comparable_rejects_tables_that_cannot_witness_a_departure() {
    let mut empty = run_a();
    empty.rows.clear();
    empty.counts = count_rows(&empty.rows);
    assert!(
        ensure_comparable(&empty)
            .unwrap_err()
            .to_string()
            .contains("no rows")
    );

    let mut foreign = run_a();
    foreign.schema = "sim_trace_comparison".to_string();
    assert!(
        ensure_comparable(&foreign)
            .unwrap_err()
            .to_string()
            .contains("schema")
    );

    let mut future = run_a();
    future.schema_version = BAND_TABLE_SCHEMA_VERSION + 1;
    assert!(
        ensure_comparable(&future)
            .unwrap_err()
            .to_string()
            .contains("schema_version")
    );

    let mut duplicated = run_a();
    let first = duplicated.rows[0].clone();
    duplicated.rows.push(first);
    assert!(
        ensure_comparable(&duplicated)
            .unwrap_err()
            .to_string()
            .contains("more than once")
    );

    let mut unbound = run_a();
    unbound.source.trace_comparison_digest.clear();
    assert!(
        ensure_comparable(&unbound)
            .unwrap_err()
            .to_string()
            .contains("no comparator-output digest"),
        "a table with nothing binding it to a run is not evidence"
    );
}

#[test]
fn ensure_comparable_rejects_an_absent_row_without_a_reason() {
    let mut table = run_b();
    let row = table
        .rows
        .iter_mut()
        .find(|row| row.band == BandLabel::Absent)
        .expect("run B has absent rows");
    row.exit_reason = None;
    assert!(
        ensure_comparable(&table)
            .unwrap_err()
            .to_string()
            .contains("no exit reason")
    );
}

#[test]
fn ensure_comparable_rejects_a_run_with_no_compared_models() {
    let table = derive(
        &trace_payload(json!({}), json!({}), json!({})),
        &results_payload(&[("Alpha", Some("sim_solver_fail"))]),
    );
    assert!(
        ensure_comparable(&table)
            .unwrap_err()
            .to_string()
            .contains("no compared models")
    );
}

#[test]
fn a_persisted_table_round_trips_and_rotates_the_previous_run_aside() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());

    let first = persist_band_table(dir, BandTableRunScope::Full).expect("persist run A");
    assert!(
        first.previous.is_none(),
        "the first run has no previous table"
    );
    assert!(first.previous_not_diffable.is_none());
    assert!(!first.rewrote_same_run);
    assert!(band_table_path(dir).is_file());
    assert!(!previous_band_table_path(dir).is_file());

    write_run(dir, &run_b_artifacts());
    let second = persist_band_table(dir, BandTableRunScope::Full).expect("persist run B");
    let previous = second
        .previous
        .clone()
        .expect("the second run rotates run A aside");

    assert_eq!(previous.rows, first.table.rows);
    assert_eq!(
        load_band_table(&previous_band_table_path(dir))
            .expect("read rotated table")
            .rows,
        first.table.rows
    );
    assert_eq!(
        load_band_table(&band_table_path(dir))
            .expect("read current table")
            .rows,
        second.table.rows
    );

    let transitions = diff_band_tables(&previous, &second.table);
    assert_eq!(transitions.counts.left, 1);
    assert_eq!(transitions.left[0].model_name, "Alpha");
}

/// The other half of F9: a crash between the rotation and the write leaves the
/// run before it sitting in `_previous` with no current table. The next persist
/// must read it back — "no previous table" and "the last run's table is one
/// filename over" must not be spelled the same way.
#[test]
fn a_persist_after_an_interrupted_rotation_still_finds_the_previous_run() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());
    let run_a = persist_band_table(dir, BandTableRunScope::Full).expect("persist run A");

    // The crash window: run A's table was rotated aside, run B's write never
    // landed.
    write_run(dir, &run_b_artifacts());
    fs::rename(band_table_path(dir), previous_band_table_path(dir)).expect("simulate the crash");

    let retried = persist_band_table(dir, BandTableRunScope::Full).expect("persist run B again");

    let previous = retried
        .previous
        .as_ref()
        .expect("the rotated-aside run A must be read back, not reported as absent");
    assert_eq!(previous.rows, run_a.table.rows);
    assert_eq!(
        diff_band_tables(previous, &retried.table).counts.left,
        1,
        "the diff the crash would have cost the run must still be produced"
    );
}

/// The review's F1: rotation used to fire on every call, so persisting one
/// certification twice pushed that run's own table into the previous slot,
/// destroyed the real previous table, and then diffed the run against itself —
/// reporting a flat "nothing moved" over a cohort that had in fact moved.
#[test]
fn re_persisting_one_certification_does_not_rotate_its_own_table_aside() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());
    persist_band_table(dir, BandTableRunScope::Full).expect("persist run A");
    write_run(dir, &run_b_artifacts());
    let run_b_persist = persist_band_table(dir, BandTableRunScope::Full).expect("persist run B");
    assert!(!run_b_persist.rewrote_same_run);

    // Same directory, same comparator output: this is the second reader of one
    // certification, not a new run.
    let again = persist_band_table(dir, BandTableRunScope::Full).expect("persist run B again");

    assert!(
        again.rewrote_same_run,
        "a re-persist of one comparator output must be recognised as such"
    );
    let previous = again
        .previous
        .as_ref()
        .expect("the real previous run's table must survive a re-persist");
    assert!(
        previous
            .row("Alpha")
            .is_some_and(|row| row.band == BandLabel::High),
        "the previous slot must still hold run A, not run B's own table"
    );
    assert_eq!(
        diff_band_tables(previous, &again.table).counts.left,
        1,
        "the diff must still see Alpha leave; a self-diff would report zero"
    );
    assert_eq!(
        load_band_table(&previous_band_table_path(dir))
            .expect("previous table on disk")
            .rows,
        previous.rows,
        "the on-disk previous table must not have been overwritten"
    );
}

/// `target/msl/task4445-after` and `target/msl/task65-canary` both carry
/// `models_compared: 0` with all-zero agreement bands. Every band check such an
/// artifact meets, it meets trivially — 0 deviations out of 0 comparisons — so
/// it reads as a clean run unless the zero is rejected at the boundary.
#[test]
fn a_comparison_that_compared_nothing_is_rejected_as_vacuous() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    fs::create_dir_all(dir).expect("create results dir");
    write_pretty_json(
        &dir.join(TRACE_COMPARISON_FILE),
        &json!({
            "models_candidate": 0,
            "models_compared": 0,
            "missing_trace_models": 0,
            "skipped_models": 0,
            "agreement_bands": { "high_agreement": 0, "minor_agreement": 0, "deviation": 0 },
            "missing_trace": {},
            "skipped": {},
            "models": {}
        }),
    )
    .expect("write vacuous comparison");
    write_pretty_json(
        &dir.join(MSL_RESULTS_FILE),
        &results_payload(&[("Alpha", Some("sim_ok"))]),
    )
    .expect("write results");

    let error = format!(
        "{:#}",
        load_or_derive_band_table(dir).expect_err("a vacuous comparison is not evidence")
    );

    assert!(error.contains("vacuous comparison"), "got: {error}");
    assert!(error.contains("compared 0 models"), "got: {error}");
    assert!(
        format!(
            "{:#}",
            persist_band_table(dir, BandTableRunScope::Full)
                .expect_err("nor may it be persisted as a table")
        )
        .contains("vacuous comparison"),
        "the same rejection must apply on the write path"
    );
}

/// An artifact whose header claims comparisons its `models` map does not carry
/// does not describe its own contents.
#[test]
fn a_comparison_whose_header_disagrees_with_its_models_map_is_rejected() {
    let trace = json!({
        "models_compared": 48,
        "models": { "Only": metric("Only", 10, 0, 0, 1.0e-9) },
        "missing_trace": {},
        "skipped": {}
    });

    let error = format!(
        "{:#}",
        ensure_comparison_not_vacuous(&trace, Path::new("sim_trace_comparison.json"))
            .expect_err("a header that overstates the map must be refused")
    );

    assert!(
        error.contains("declares models_compared=48"),
        "got: {error}"
    );
    assert!(error.contains("1 model entries"), "got: {error}");
}

/// `target/msl/task65-canary-parity` carries a full results directory and no
/// `sim_trace_comparison.json` at all. A missing comparator output is a loud
/// failure, not a skip: the run compared nothing, and nothing in the directory
/// says so on its own.
#[test]
fn a_results_dir_with_no_comparator_output_is_named_not_skipped() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    fs::create_dir_all(dir).expect("create results dir");
    write_pretty_json(
        &dir.join(MSL_RESULTS_FILE),
        &results_payload(&[("Alpha", Some("sim_ok"))]),
    )
    .expect("write results");

    let error = format!(
        "{:#}",
        load_or_derive_band_table(dir)
            .expect_err("a directory with no comparator output carries no parity evidence")
    );

    assert!(error.contains("no comparator output"), "got: {error}");
    assert!(
        error.contains("sim_ok is completion, never parity"),
        "the failure must say what the directory does not prove, got: {error}"
    );
}

/// The review's F2: a well-formed table copied in from another run passed every
/// check and was quoted as this directory's band population.
#[test]
fn a_table_planted_from_another_run_is_refused() {
    let temp = tempfile::tempdir().expect("tempdir");
    let donor = temp.path().join("donor");
    let target = temp.path().join("target");
    write_run(&donor, &run_a_artifacts());
    write_run(&target, &run_b_artifacts());
    persist_band_table(&donor, BandTableRunScope::Full).expect("persist donor");

    fs::copy(band_table_path(&donor), band_table_path(&target)).expect("plant the donor's table");

    let error = load_or_derive_band_table(&target)
        .expect_err("a table derived from another run's comparator output is not this run's");
    let error = format!("{error:#}");
    assert!(
        error.contains("different comparator output"),
        "got: {error}"
    );

    // And the honest path still works: the directory's own artifacts derive a
    // table that is bound to them.
    let persisted = persist_band_table(&target, BandTableRunScope::Full).expect("persist target");
    ensure_bound_to_dir(&persisted.table, &target).expect("a freshly persisted table is bound");
    assert!(
        load_or_derive_band_table(&target).is_ok(),
        "the run's own table must be accepted"
    );
}

/// A stale table left behind when the comparator re-ran is the same defect as a
/// planted one: it describes a comparator output that no longer exists.
#[test]
fn a_table_left_over_from_an_earlier_comparator_run_is_refused() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());
    persist_band_table(dir, BandTableRunScope::Full).expect("persist run A");

    // The comparator re-runs; the table on disk still describes run A.
    write_run(dir, &run_b_artifacts());

    let error = format!(
        "{:#}",
        load_bound_band_table(dir).expect_err("a stale table must not read as this run's")
    );
    assert!(
        error.contains("different comparator output"),
        "got: {error}"
    );
}

/// The review's F7: the harness persists on every path, including a Tier 1
/// focused run over a handful of models. Letting that rotate the cohort table
/// aside destroys the baseline the next Tier 2 diff needs.
#[test]
fn a_partial_run_refuses_to_rotate_the_full_cohort_table_aside() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());
    let cohort = persist_band_table(dir, BandTableRunScope::Full).expect("persist the cohort run");
    assert_eq!(cohort.table.run_scope, BandTableRunScope::Full);

    write_run(dir, &run_b_artifacts());
    let focused =
        persist_band_table(dir, BandTableRunScope::Partial).expect("the focused run still reads");

    assert!(
        !focused.persisted,
        "a focused run must not consume the cohort baseline"
    );
    let reason = focused
        .not_persisted_reason
        .as_deref()
        .expect("declining to write must be stated, never silent");
    assert!(reason.contains("partial run"), "got: {reason}");
    assert_eq!(
        load_band_table(&band_table_path(dir))
            .expect("the cohort table must still be there")
            .rows,
        cohort.table.rows
    );
    assert!(
        !previous_band_table_path(dir).is_file(),
        "nothing may have rotated"
    );
}

/// The scope guard runs both ways. A full run may rotate a shard's stripe table
/// aside — it is a new run — but it must not then diff the cohort against that
/// stripe: the difference between two model sets would be published as cohort
/// movement, and every model the stripe never contained would read as departed.
#[test]
fn a_full_run_does_not_diff_the_cohort_against_a_shard_stripe() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());
    let shard = persist_band_table(dir, BandTableRunScope::Partial).expect("persist the shard");
    assert!(shard.persisted);

    write_run(dir, &run_b_artifacts());
    let cohort = persist_band_table(dir, BandTableRunScope::Full).expect("persist the cohort run");

    assert!(
        cohort.persisted,
        "a full run is still written; only the diff is refused"
    );
    assert!(
        cohort.previous.is_none(),
        "a stripe is not a predecessor certification"
    );
    let reason = cohort
        .previous_not_diffable
        .as_deref()
        .expect("declining to diff must be stated");
    assert!(reason.contains("run_scope_mismatch"), "got: {reason}");
    assert!(
        previous_band_table_path(dir).is_file(),
        "the stripe is still rotated aside for inspection"
    );
}

/// The same guard on the two-directory diff the campaign quotes.
#[test]
fn diffing_a_cohort_against_a_stripe_is_refused() {
    let cohort = run_a();
    let mut stripe = run_b();
    stripe.run_scope = BandTableRunScope::Partial;

    let error = format!(
        "{:#}",
        ensure_diffable_pair(&cohort, &stripe).expect_err("a cohort and a stripe are not a diff")
    );

    assert!(error.contains("run_scope_mismatch"), "got: {error}");
    assert!(ensure_diffable_pair(&cohort, &run_b()).is_ok());
}

/// The comparator distinguishes "the two traces shared nothing comparable" from
/// a comparator defect, so a consumer reads the kind rather than the wording of
/// the message.
#[test]
fn a_trace_with_nothing_comparable_is_its_own_kind() {
    let entry = exit(
        "no_comparable_samples",
        "trace compare failed: trace has no comparable variable samples",
    );

    assert_eq!(
        trace_exit_kind(&entry),
        Some(TraceExitKind::NoComparableSamples)
    );
    assert_eq!(
        trace_exit_kind(&exit(
            "comparator_failed",
            "trace compare failed: shape mismatch"
        )),
        Some(TraceExitKind::ComparatorFailed)
    );
    assert_eq!(
        trace_exit_kind(&json!(
            "trace compare failed: trace has no comparable variable samples"
        )),
        None,
        "an untyped entry does not say which boundary stopped the comparison, and a reader must \
         not infer one from the text"
    );

    let table = derive(
        &trace_payload(
            json!({ "Compared": metric("Compared", 10, 0, 0, 1.0e-9) }),
            json!({}),
            json!({ "Empty": entry }),
        ),
        &results_payload(&[("Compared", Some("sim_ok")), ("Empty", Some("sim_ok"))]),
    );
    assert_eq!(
        table.row("Empty").and_then(|row| row.exit_reason),
        Some(ExitReason::NoComparableSamples)
    );
}

/// A CI shard is a partial run with a results directory of its own. It must
/// still write its table: the fan-in checks that every shard produced one, and
/// there is no cohort baseline in a fresh runner's directory to displace.
#[test]
fn a_partial_run_in_a_fresh_directory_still_writes_its_table() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());

    let shard = persist_band_table(dir, BandTableRunScope::Partial).expect("persist the shard");

    assert!(shard.persisted, "got: {:?}", shard.not_persisted_reason);
    assert_eq!(shard.table.run_scope, BandTableRunScope::Partial);
    assert_eq!(
        load_band_table(&band_table_path(dir))
            .expect("the shard's table must be on disk for the fan-in to collect")
            .run_scope,
        BandTableRunScope::Partial,
        "the scope travels with the table, so a merged reader can tell what it is"
    );
}

/// The review's F9: rotation renames the current table aside before writing the
/// new one. A crash in that window used to leave the run with no table and a
/// `_previous` nobody read, so the next run reported "no previous table" over a
/// directory that had one.
#[test]
fn a_rotation_interrupted_before_the_write_is_recovered_from_the_previous_slot() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_b_artifacts());
    let persisted = persist_band_table(dir, BandTableRunScope::Full).expect("persist run B");

    // Reproduce the crash window: the table was renamed aside, the write never
    // happened.
    fs::rename(band_table_path(dir), previous_band_table_path(dir)).expect("simulate the crash");
    assert!(!band_table_path(dir).is_file());

    let recovered = load_or_derive_band_table(dir).expect("the interrupted table must be found");

    assert_eq!(
        recovered.rows, persisted.table.rows,
        "the run's own table was in the previous slot; it must be read back rather than lost"
    );
}

/// A `_previous` that belongs to an *earlier* comparator output is not a
/// recovery candidate: it is the previous run, and reading it as this run's
/// table would republish stale bands.
#[test]
fn an_earlier_runs_previous_table_is_not_mistaken_for_an_interrupted_write() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());
    persist_band_table(dir, BandTableRunScope::Full).expect("persist run A");
    write_run(dir, &run_b_artifacts());
    persist_band_table(dir, BandTableRunScope::Full).expect("persist run B");
    fs::remove_file(band_table_path(dir)).expect("drop the current table");

    let derived = load_or_derive_band_table(dir).expect("derive from the directory's artifacts");

    assert!(
        derived.row("Delta").is_some(),
        "the table must describe run B's comparator output, not run A's"
    );
    assert!(
        derived
            .row("Alpha")
            .is_some_and(|row| row.band == BandLabel::Absent),
        "run A's bands must not come back through the previous slot"
    );
}

#[test]
fn an_unreadable_previous_table_is_rotated_aside_and_named_never_silently_dropped() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());
    fs::write(band_table_path(dir), "{\"schema\":\"something-else\"}").expect("write junk table");

    let persisted =
        persist_band_table(dir, BandTableRunScope::Full).expect("persist over an unreadable table");

    assert!(persisted.previous.is_none());
    let detail = persisted
        .previous_not_diffable
        .as_deref()
        .expect("an unreadable previous table must be named");
    assert!(detail.contains("band table"), "got: {detail}");
    assert_eq!(
        fs::read_to_string(previous_band_table_path(dir)).expect("rotated file"),
        "{\"schema\":\"something-else\"}",
        "the unreadable table must be preserved byte-for-byte for inspection"
    );
    assert!(load_band_table(&band_table_path(dir)).is_ok());
}

/// The review's F8: provenance used to be `git rev-parse HEAD` in the reading
/// process, so re-deriving a table for an old certification stamped it with
/// today's commit and attributed those numbers to this checkout.
#[test]
fn the_table_records_the_certification_commit_not_the_readers_head() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());

    let table = derive_band_table_from_dir(dir, BandTableRunScope::Full).expect("derive from dir");

    assert_eq!(
        table.git_commit, "cert1234",
        "the commit must come from the certification's own msl_results.json"
    );
    assert!(!table.source.trace_comparison_digest.is_empty());
}

/// A directory holding only comparator output has provenance — the comparator
/// stamps its own commit — but it is still **not a cohort table**: with no
/// `msl_results.json` there is no roster, so "one row per cohort target" is
/// unverifiable and the row count is just however many models the comparator
/// happened to mention.
#[test]
fn a_table_derived_without_a_results_roster_is_not_comparable() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    fs::create_dir_all(dir).expect("create results dir");
    // Only the comparator's own output — no `skipped` entries, so any excluded
    // row in the result would have been invented from the tracked list.
    let mut trace = trace_payload(
        json!({ "Alpha": metric("Alpha", 10, 0, 0, 1.0e-9) }),
        json!({}),
        json!({}),
    );
    trace["git_commit"] = json!("comparatorstamp");
    trace["git_worktree_dirty"] = json!(false);
    write_pretty_json(&dir.join(TRACE_COMPARISON_FILE), &trace).expect("write trace");

    let table = derive_band_table_from_dir(dir, BandTableRunScope::Full).expect("derive from dir");

    assert_eq!(
        table.git_commit, "comparatorstamp",
        "the comparator's own stamp is the provenance when no results file exists"
    );
    assert_eq!(table.cohort_roster_models, 0);
    assert_eq!(
        table.rows.len(),
        1,
        "with no roster, the table is the comparator's output and nothing else; injecting a row \
         per tracked exclusion would invent membership for models the run never considered: {:?}",
        table
            .rows
            .iter()
            .map(|row| row.model_name.as_str())
            .collect::<Vec<_>>()
    );
    let error = format!(
        "{:#}",
        ensure_comparable(&table)
            .expect_err("a table with no cohort roster cannot claim one row per cohort target")
    );
    assert!(error.contains("no cohort roster"), "got: {error}");

    // And the whole evidence path refuses it, not just the predicate.
    let error = format!(
        "{:#}",
        load_or_derive_band_table(dir).expect_err("the evidence path must refuse it too")
    );
    assert!(error.contains("no cohort roster"), "got: {error}");
}

/// The row set is the cohort, checkable from the table alone.
#[test]
fn a_row_set_that_is_not_the_cohort_roster_is_rejected() {
    let mut table = run_a();
    table.cohort_roster_models += 1;

    let error = format!(
        "{:#}",
        ensure_comparable(&table).expect_err("row set mismatch")
    );

    assert!(
        error.contains("rows for a cohort roster of"),
        "got: {error}"
    );
}

/// Numbers with no traceable origin are not evidence.
#[test]
fn a_table_with_no_commit_is_rejected() {
    let mut table = run_a();
    table.git_commit.clear();

    let error = format!(
        "{:#}",
        ensure_comparable(&table).expect_err("no provenance")
    );

    assert!(error.contains("no git_commit"), "got: {error}");
}

/// The source digests bind a table to the artifacts it came from and say nothing
/// about the rows afterwards. A band relabelled by hand keeps every binding
/// intact while changing what the table claims.
#[test]
fn a_table_whose_rows_were_edited_after_derivation_is_rejected() {
    let mut table = run_a();
    let row = table
        .rows
        .iter_mut()
        .find(|row| row.band == BandLabel::Deviation)
        .expect("run A has a deviating model");
    row.band = BandLabel::High;
    table.counts = count_rows(&table.rows);

    let error = format!(
        "{:#}",
        ensure_comparable(&table).expect_err("a relabelled band must not pass as evidence")
    );

    assert!(
        error.contains("edited after the table was derived"),
        "got: {error}"
    );
    assert!(error.contains("row digest"), "got: {error}");
}

/// A metric's `f64` does not always survive JSON: the comparator produced
/// `5.723148252362699e-9` for `InvertingAmp`, and reading that text back yields a
/// neighbouring double. The digest has to describe the artifact a reader parses,
/// or every table this tool writes fails its own integrity check on reload.
#[test]
fn a_metric_that_does_not_survive_json_still_round_trips_the_integrity_check() {
    let table = derive(
        &trace_payload(
            json!({
                "Fragile": {
                    "model_name": "Fragile",
                    "compared_variables": 49,
                    "samples_compared": 100,
                    "bounded_normalized_l1_score": 0.0,
                    "mean_channel_bounded_normalized_l1": 1.4521673119841147e-9,
                    "max_channel_bounded_normalized_l1": 5.723148252362699e-9,
                    "channel_high_count": 49,
                    "channel_minor_count": 0,
                    "channel_deviation_count": 0,
                    "channel_severe_count": 0,
                    "worst_variables": []
                }
            }),
            json!({}),
            json!({}),
        ),
        &results_payload(&[("Fragile", Some("sim_ok"))]),
    );
    ensure_comparable(&table).expect("a freshly derived table must be comparable");

    let encoded = serde_json::to_string_pretty(&table).expect("encode");
    let reloaded: BandTable = serde_json::from_str(&encoded).expect("decode");

    assert_eq!(
        reloaded.rows_digest, table.rows_digest,
        "the digest must survive the artifact's own wire format"
    );
    ensure_comparable(&reloaded).expect("a table must still be comparable after a round trip");
}

/// Counts are what consumers quote; they must be recomputable from the rows.
#[test]
fn a_table_whose_counts_were_edited_is_rejected() {
    let mut table = run_a();
    table.counts.cohort_models = 999;

    let error = format!(
        "{:#}",
        ensure_comparable(&table).expect_err("an inflated count must not pass as evidence")
    );

    assert!(
        error.contains("counts do not match its rows"),
        "got: {error}"
    );
}

/// The exclusion list decides policy-vs-defect attribution, so which list was
/// used has to be on the artifact. Two readings of one certification against
/// different lists are different readings, and the table says which it is.
#[test]
fn the_table_records_the_exclusion_list_that_attributed_it() {
    let temp = tempfile::tempdir().expect("tempdir");
    let dir = temp.path();
    write_run(dir, &run_a_artifacts());

    let table = derive_band_table_from_dir(dir, BandTableRunScope::Full).expect("derive from dir");

    assert!(
        table
            .source
            .exclusions_file
            .ends_with("msl_trace_compare_exclusions.json"),
        "got: {}",
        table.source.exclusions_file
    );
    assert!(
        !table.source.exclusions_digest.is_empty(),
        "the list's digest must travel with the table"
    );
}

/// An unreadable exclusion list used to yield an empty map, which silently
/// reclassified every policy skip as a comparator defect — and did so as a
/// function of the working directory, since the path is resolved by walking up
/// from the CWD. Defaulting on error is prohibited (SPEC 0008); the read is loud.
#[test]
fn an_unreadable_exclusion_list_is_an_error_not_an_empty_map() {
    let temp = tempfile::tempdir().expect("tempdir");

    let error = format!(
        "{:#}",
        exclusions_from(&temp.path().join("does-not-exist.json"))
            .expect_err("a missing exclusion list must not read as `nothing is excluded`")
    );

    assert!(
        error.contains("cannot attribute policy exclusions"),
        "got: {error}"
    );
    assert!(
        error.contains("comparator defect"),
        "the failure must name what silence would have cost, got: {error}"
    );

    let malformed = temp.path().join("bare.json");
    fs::write(&malformed, r#"["A.Model"]"#).expect("write bare list");
    assert!(
        exclusions_from(&malformed).is_err(),
        "a list without per-entry reasons must not be read as usable policy"
    );
}

#[test]
fn a_results_dir_written_before_the_table_existed_is_still_diffable() {
    let temp = tempfile::tempdir().expect("tempdir");
    let before_dir = temp.path().join("before");
    let after_dir = temp.path().join("after");
    write_run(&before_dir, &run_a_artifacts());
    write_run(&after_dir, &run_b_artifacts());

    // Neither directory carries `msl_band_table.json`; both must still yield a
    // comparable table so a historical certification is not silently dropped.
    let before = load_or_derive_band_table(&before_dir).expect("derive before");
    let after = load_or_derive_band_table(&after_dir).expect("derive after");

    let transitions = diff_band_tables(&before, &after);
    assert_eq!(transitions.counts.left, 1);
    assert_eq!(transitions.left[0].exit_reason, ExitReason::SimFailed);
}

struct RunArtifacts {
    trace: Value,
    results: Value,
}

fn run_a_artifacts() -> RunArtifacts {
    RunArtifacts {
        trace: trace_payload(
            json!({
                "Alpha": metric("Alpha", 10, 0, 0, 1.0e-9),
                "Beta": metric("Beta", 1, 0, 9, 0.9),
            }),
            json!({}),
            json!({ "Gamma": exit("policy_excluded", "stochastic random-input model") }),
        ),
        results: results_payload_with_targets(
            &[
                ("Alpha", Some("sim_ok")),
                ("Beta", Some("sim_ok")),
                ("Gamma", Some("sim_ok")),
            ],
            &["Alpha", "Beta", "Gamma", "Delta"],
        ),
    }
}

fn run_b_artifacts() -> RunArtifacts {
    RunArtifacts {
        trace: trace_payload(
            json!({
                "Beta": metric("Beta", 10, 0, 0, 1.0e-9),
                "Delta": metric("Delta", 7, 3, 0, 0.02),
            }),
            json!({}),
            json!({ "Gamma": exit("policy_excluded", "stochastic random-input model") }),
        ),
        results: results_payload_with_targets(
            &[
                ("Alpha", Some("sim_solver_fail")),
                ("Beta", Some("sim_ok")),
                ("Gamma", Some("sim_ok")),
                ("Delta", Some("sim_ok")),
            ],
            &["Alpha", "Beta", "Gamma", "Delta"],
        ),
    }
}

fn write_run(dir: &Path, artifacts: &RunArtifacts) {
    fs::create_dir_all(dir).expect("create results dir");
    write_pretty_json(&dir.join(TRACE_COMPARISON_FILE), &artifacts.trace).expect("write trace");
    write_pretty_json(&dir.join(MSL_RESULTS_FILE), &artifacts.results).expect("write results");
}
