use super::*;

#[test]
fn a_tree_passes_only_with_every_required_node_and_no_forbidden_one() {
    let tree = "rumoca-runtime-fmi-ls v0.10.4\n├── rumoca-solver v0.10.4\n│   └── rumoca-solver-rk45 v0.10.4\n└── rumoca-eval-solve feature \"linked-guest\"\n    └── rumoca-eval-solve v0.10.4\n";
    let report = check_tree(
        tree,
        &[
            "rumoca-solver",
            "rumoca-eval-solve",
            "feature \"linked-guest\"",
        ],
        &["feature \"reference-eval\"", "rumoca-phase-solve"],
    )
    .expect("a production tree passes");
    assert!(report.contains("require rumoca-solver: 1"), "{report}");
    let forbidden = check_tree(tree, &[], &["rumoca-solver-rk45"]).unwrap_err();
    assert!(forbidden.to_string().contains("forbidden"), "{forbidden}");
    let missing = check_tree(tree, &["rumoca-phase-solve"], &[]).unwrap_err();
    assert!(missing.to_string().contains("missing"), "{missing}");
    assert!(check_tree(" \n", &[], &[]).is_err(), "an empty tree fails");
}

#[test]
fn a_crate_name_does_not_match_a_longer_crate_name() {
    let tree = "└── rumoca-solver-rk45 v0.10.4\n";
    assert_eq!(tree_mentions(tree, "rumoca-solver"), 0);
    assert_eq!(tree_mentions(tree, "rumoca-solver-rk45"), 1);
}

#[test]
fn a_test_stage_requires_exactly_its_named_tests_and_nothing_ignored() {
    let ok = "running 2 tests\ntest a::b::first ... ok\ntest a::second ... ok\n\ntest result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 9 filtered out; finished in 0.01s\n";
    assert!(check_tests(ok, &["a::b::first", "second"]).is_ok());
    assert!(
        check_tests(ok, &["first"]).is_err(),
        "a count other than the named set fails"
    );
    let ignored = ok.replace("0 ignored", "1 ignored");
    assert!(check_tests(&ignored, &["a::b::first", "second"]).is_err());
    let none = "running 0 tests\n\ntest result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 11 filtered out\n";
    assert!(check_tests(none, &["first"]).is_err(), "no test ran");
    assert!(check_tests("", &[]).is_err(), "no summary at all");
}

#[test]
fn the_artifact_is_the_wasm_the_messages_name_for_the_guest() {
    let dir = tempfile::tempdir().unwrap();
    let wasm = dir.path().join("rumoca_runtime_fmi_ls.wasm");
    std::fs::write(&wasm, b"\0asm").unwrap();
    let message = json!({
        "reason": "compiler-artifact",
        "target": { "name": "rumoca_runtime_fmi_ls" },
        "filenames": [wasm],
    });
    let report = check_artifact(&format!("{{\"reason\":\"build-finished\"}}\n{message}\n"))
        .expect("the artifact is found");
    assert!(report.contains("sha256 "), "{report}");
    assert!(check_artifact("{\"reason\":\"build-finished\"}\n").is_err());
}

#[test]
fn every_stage_is_unique_and_the_guest_builds_use_the_packager_flags() {
    let all = stages(Path::new("/sysroot/bin/rust-lld"), 4, true);
    let mut names: Vec<_> = all.iter().map(|stage| stage.name).collect();
    names.sort_unstable();
    names.dedup();
    assert_eq!(names.len(), all.len());
    let build = all.iter().find(|stage| stage.name == "BUILD-P").unwrap();
    assert!(build.args.contains(&"--offline".to_string()));
    assert!(build.args.contains(&"-j4".to_string()));
    assert!(build.rustflags.contains("-Clinker=/sysroot/bin/rust-lld"));
    assert_eq!(build.rustflags.split('\u{1f}').count(), 10);
    let check = all.iter().find(|stage| stage.name == "CHECK-C").unwrap();
    assert!(
        check
            .args
            .windows(2)
            .any(|pair| pair == ["--features", CERTIFY])
    );
}

#[test]
fn check_matrix_parses_its_stage_filter() {
    use crate::{Cli, Commands};
    use clap::Parser;
    let cli = Cli::try_parse_from([
        "xtask",
        "ci",
        "check-matrix",
        "--stage",
        "CHECK-P",
        "--stage",
        "R1",
        "--keep-going",
    ])
    .expect("check-matrix parses");
    let Commands::Ci(super::super::CiArgs {
        command: super::super::CiCommand::CheckMatrix(args),
    }) = cli.command
    else {
        panic!("ci check-matrix");
    };
    assert_eq!(args.stages, ["CHECK-P", "R1"]);
    assert!(args.keep_going && !args.offline && !args.list);
}
