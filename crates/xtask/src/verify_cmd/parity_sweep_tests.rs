use super::parity_sweep::{
    MslParityAction, diff_report, find_band_table, isolated_rerun_args, merge_reruns,
    rerun_results_dir, strict_high, timed_out_models,
};
use super::{VerifyCommand, VerifyMslParityArgs};
use clap::Parser;
use serde_json::{Value, json};

#[derive(Debug, Parser)]
struct Harness {
    #[command(subcommand)]
    command: VerifyCommand,
}

fn parity(args: &[&str]) -> Result<VerifyMslParityArgs, clap::Error> {
    let argv = ["verify", "msl-parity"]
        .into_iter()
        .chain(args.iter().copied());
    match Harness::try_parse_from(argv)?.command {
        VerifyCommand::MslParity(args) => Ok(*args),
        other => panic!("msl-parity parses as {other:?}"),
    }
}

/// A band table fixture: a strict high, a deviating high, a solver timeout, a
/// lowering budget overrun, and an ordinary failure.
fn fixture() -> Value {
    json!({ "rows": [
        { "model_name": "A.High", "band": "high", "channel_deviation_count": 0 },
        { "model_name": "B.Deviating", "band": "high", "channel_deviation_count": 2 },
        { "model_name": "C.Slow", "band": "failed", "exit_reason": "sim_timeout" },
        { "model_name": "D.Lowering", "band": "failed", "exit_detail": "exceeded the 20 s lowering budget" },
        { "model_name": "E.Wrong", "band": "low", "exit_reason": "channel_mismatch" }
    ]})
}

#[test]
fn msl_parity_parses_the_sweep_flags_and_the_diff_action() {
    let args = parity(&["--serialize", "--rerun-timeouts-alone", "90"]).unwrap();
    assert!(args.serialize);
    assert_eq!(args.rerun_timeouts_alone, Some(90));
    let args = parity(&["diff", "run.json", "ref.json"]).unwrap();
    let Some(MslParityAction::Diff(diff)) = args.action else {
        panic!("diff action");
    };
    assert_eq!(diff.table.to_str(), Some("run.json"));
    assert_eq!(diff.reference.to_str(), Some("ref.json"));
    assert!(
        parity(&["--serialize", "diff", "a.json", "b.json"]).is_err(),
        "sweep flags do not combine with the diff action"
    );
}

#[test]
fn reruns_select_only_models_that_ran_out_of_a_budget() {
    assert_eq!(timed_out_models(&fixture()), ["C.Slow", "D.Lowering"]);
}

#[test]
fn an_isolated_rerun_runs_one_model_alone_with_the_given_budget() {
    let base = parity(&[
        "--serialize",
        "--rerun-timeouts-alone",
        "90",
        "--sim-match",
        "Mechanics",
    ])
    .unwrap();
    let results = std::path::Path::new("/results");
    let rerun = isolated_rerun_args(&base, results, "C.Slow", 90);
    assert_eq!(rerun.sim_match, ["C.Slow"]);
    assert!(rerun.sim_match_exact && rerun.clean_results);
    assert_eq!(
        (rerun.stage_parallelism, rerun.sim_parallelism),
        (Some(1), Some(1))
    );
    assert_eq!(
        rerun.results_dir,
        Some(rerun_results_dir(results, "C.Slow"))
    );
    assert!(!rerun.serialize && rerun.rerun_timeouts_alone.is_none());
    let config = rerun.to_parity_config_json().to_string();
    assert!(config.contains("\"sim_timeout_secs\":90.0"), "{config}");
    assert!(
        config.contains("\"model_attempt_timeout_secs\":90.0"),
        "{config}"
    );
}

#[test]
fn a_rerun_row_replaces_its_sweep_row() {
    let rerun = json!({ "rows": [{ "model_name": "C.Slow", "band": "high", "channel_deviation_count": 0 }] });
    let merged = merge_reruns(&fixture(), &[rerun]);
    let rows = merged["rows"].as_array().unwrap();
    assert_eq!(rows.len(), 5);
    let slow = rows
        .iter()
        .find(|row| row["model_name"] == "C.Slow")
        .unwrap();
    assert!(strict_high(slow) && slow["rerun_alone"] == true);
}

#[test]
fn the_diff_reports_lost_gained_and_changed_rows() {
    let reference = fixture();
    let table = json!({ "rows": [
        { "model_name": "A.High", "band": "failed", "exit_reason": "sim_timeout", "exit_detail": "wall budget" },
        { "model_name": "B.Deviating", "band": "high", "channel_deviation_count": 0 },
        { "model_name": "C.Slow", "band": "low", "exit_reason": "channel_mismatch" },
        { "model_name": "E.Wrong", "band": "low", "exit_reason": "channel_mismatch" },
        { "model_name": "F.New", "band": "high", "channel_deviation_count": 0 }
    ]});
    let report = diff_report(&table, &reference);
    assert_eq!(
        report,
        "strict-high 2 / 5 rows\n\
         reference strict-high 1 / 5\n\
         lost 1\n  A.High: failed/sim_timeout wall budget\n\
         gained 1: B.Deviating\n\
         other changes 3\n  C.Slow: failed/sim_timeout -> low/channel_mismatch\n  D.Lowering: failed -> MISSING\n  F.New: MISSING -> high\n"
    );
}

#[test]
fn the_sweep_table_is_found_outside_the_rerun_directory() {
    let dir = tempfile::tempdir().unwrap();
    let rerun = rerun_results_dir(dir.path(), "C.Slow");
    std::fs::create_dir_all(&rerun).unwrap();
    std::fs::write(rerun.join("msl_band_table.json"), "{}").unwrap();
    assert_eq!(find_band_table(dir.path()), None);
    let run = dir.path().join("run");
    std::fs::create_dir_all(&run).unwrap();
    std::fs::write(run.join("msl_band_table.json"), "{}").unwrap();
    assert_eq!(
        find_band_table(dir.path()),
        Some(run.join("msl_band_table.json"))
    );
}

/// The sweep runs under the host lock, reruns each timed-out model alone, and
/// writes the merged table; the diff action reads two tables.
#[test]
fn a_sweep_reruns_its_timeouts_alone_and_merges_them() {
    let dir = tempfile::tempdir().unwrap();
    let results = dir.path().join("sweep");
    let mut args = parity(&["--serialize", "--rerun-timeouts-alone", "90"]).unwrap();
    args.results_dir = Some(results.clone());
    let mut runs = Vec::new();
    let mut gate = |_: &std::path::Path, run: &VerifyMslParityArgs| -> anyhow::Result<()> {
        let dir = run.results_dir.clone().unwrap();
        let table = if run.sim_match.is_empty() {
            fixture()
        } else {
            json!({ "rows": [{ "model_name": run.sim_match[0], "band": "high", "channel_deviation_count": 0 }] })
        };
        std::fs::create_dir_all(dir.join("run")).unwrap();
        std::fs::write(dir.join("run/msl_band_table.json"), table.to_string()).unwrap();
        runs.push(run.sim_match.clone());
        Ok(())
    };
    super::parity_sweep::run(dir.path(), &args, &mut gate).unwrap();
    // The diff action reads two tables and runs no sweep.
    let diff = parity(&[
        "diff",
        results.join("merged_band_table.json").to_str().unwrap(),
        results.join("run/msl_band_table.json").to_str().unwrap(),
    ])
    .unwrap();
    super::parity_sweep::run(dir.path(), &diff, &mut gate).unwrap();
    assert_eq!(
        runs,
        [
            vec![],
            vec!["C.Slow".to_string()],
            vec!["D.Lowering".to_string()]
        ]
    );
    let merged = super::parity_sweep::load(&results.join("merged_band_table.json")).unwrap();
    assert_eq!(
        merged["rows"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|row| strict_high(row))
            .count(),
        3
    );
}
