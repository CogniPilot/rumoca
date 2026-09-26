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
