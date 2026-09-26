use super::*;
use crate::{Cli, Commands};
use clap::Parser;

#[test]
fn watch_parses_the_commit_and_poll_options() {
    let cli = Cli::try_parse_from(["xtask", "ci", "watch", "abc123", "--interval-secs", "5"])
        .expect("watch parses");
    let Commands::Ci(CiArgs {
        command: CiCommand::Watch(watch),
    }) = cli.command
    else {
        panic!("ci watch");
    };
    assert_eq!(
        watch,
        CiWatchArgs {
            sha: "abc123".into(),
            interval_secs: 5,
            once: false,
        }
    );
    assert!(
        Cli::try_parse_from(["xtask", "ci", "watch"]).is_err(),
        "a commit is required"
    );
}

#[test]
fn runs_and_failed_jobs_are_read_from_gh_json() {
    let runs = parse_runs(
        r#"[{"databaseId":7,"workflowName":"CI","status":"completed","conclusion":"failure"},
            {"databaseId":8,"workflowName":"Nightly","status":"in_progress","conclusion":""}]"#,
    )
    .unwrap();
    assert_eq!(runs.len(), 2);
    assert!(runs[0].completed() && !runs[0].succeeded());
    assert!(!runs[1].completed());
    let jobs = failed_jobs(
        r#"{"jobs":[{"name":"lint","conclusion":"success"},
                    {"name":"coverage","conclusion":"failure"},
                    {"name":"msl","conclusion":"timed_out"}]}"#,
    )
    .unwrap();
    assert_eq!(jobs, ["coverage", "msl"]);
}

/// A fake `gh`: pending runs first, then a completed failing run whose jobs
/// are listed.
fn fake_gh(calls: &mut Vec<String>) -> impl FnMut(&[&str]) -> Result<String> + '_ {
    move |args: &[&str]| {
        calls.push(args.join(" "));
        Ok(match (args[1], calls.len()) {
            ("list", 1) => r#"[{"databaseId":1,"workflowName":"CI","status":"in_progress","conclusion":""}]"#.into(),
            ("list", _) => r#"[{"databaseId":1,"workflowName":"CI","status":"completed","conclusion":"failure"}]"#.into(),
            _ => r#"{"jobs":[{"name":"coverage","conclusion":"failure"}]}"#.into(),
        })
    }
}

#[test]
fn watch_waits_for_completion_and_reports_the_failed_jobs() {
    let mut calls = Vec::new();
    let args = CiArgs {
        command: CiCommand::Watch(CiWatchArgs {
            sha: "abc123".into(),
            interval_secs: 0,
            once: false,
        }),
    };
    let error = run(args, &mut fake_gh(&mut calls)).expect_err("the run failed");
    assert!(error.to_string().contains("did not succeed"), "{error}");
    assert_eq!(calls.len(), 3, "two polls and one job listing: {calls:?}");
    assert!(calls[0].contains("--commit abc123 --branch main"));
    assert!(calls[2].starts_with("run view 1"));
}

#[test]
fn a_single_look_reports_the_current_state() {
    let mut empty = |_: &[&str]| -> Result<String> { Ok("[]".into()) };
    let once = |sha: &str| CiArgs {
        command: CiCommand::Watch(CiWatchArgs {
            sha: sha.into(),
            interval_secs: 0,
            once: true,
        }),
    };
    assert!(run(once("abc"), &mut empty).is_err(), "no run on main");
    let mut green = |_: &[&str]| -> Result<String> {
        Ok(
            r#"[{"databaseId":2,"workflowName":"CI","status":"completed","conclusion":"success"}]"#
                .into(),
        )
    };
    run(once("abc"), &mut green).unwrap();
}
