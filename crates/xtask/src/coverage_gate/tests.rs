use super::*;
use clap::Parser;

#[derive(Debug, Parser)]
struct Harness {
    #[command(flatten)]
    args: CoverageGateArgs,
}

fn parse(flags: &[&str]) -> Result<CoverageGateArgs, clap::Error> {
    Harness::try_parse_from(std::iter::once("gate").chain(flags.iter().copied()))
        .map(|harness| harness.args)
}

fn function(file: &str, line: u64, name: &str) -> ZeroExecutionFunction {
    ZeroExecutionFunction {
        file: file.to_string(),
        line,
        name: name.to_string(),
        package: "alpha".to_string(),
    }
}

const TWO_FILE_DIFF: &str = "\
diff --git a/crates/alpha/src/lib.rs b/crates/alpha/src/lib.rs
index 1111111..2222222 100644
--- a/crates/alpha/src/lib.rs
+++ b/crates/alpha/src/lib.rs
@@ -3,0 +4,3 @@ fn kept() {
+fn added() {
+++ b/not/a/header.rs
+}
@@ -20,2 +22,0 @@ fn other() {
@@ -40 +40 @@ fn edited() {
-    old();
+    new();
diff --git a/crates/alpha/src/gone.rs b/crates/alpha/src/gone.rs
deleted file mode 100644
--- a/crates/alpha/src/gone.rs
+++ /dev/null
@@ -1,2 +0,0 @@
-#[cfg_attr(coverage_nightly, coverage(off))]
-fn gone() {}
diff --git a/crates/beta/src/new.rs b/crates/beta/src/new.rs
new file mode 100644
--- /dev/null
+++ b/crates/beta/src/new.rs
@@ -0,0 +1,5 @@
+fn fresh() {}
+#[cfg_attr(coverage_nightly, coverage(off))]
+fn exempt() {}
";

#[test]
fn added_lines_come_from_new_side_hunks_only() {
    let added = parse_changed_code(TWO_FILE_DIFF).added;
    assert_eq!(
        added,
        AddedLines::from([
            (
                "crates/alpha/src/lib.rs".to_string(),
                vec![(4, 6), (40, 40)]
            ),
            ("crates/beta/src/new.rs".to_string(), vec![(1, 5)]),
        ]),
        "a deletion-only hunk, a deleted file, and an added line reading `++ b/...` add nothing"
    );
    assert_eq!(
        parse_changed_code(TWO_FILE_DIFF).exemptions,
        ["crates/beta/src/new.rs:2"],
        "an added exemption is located by its new-side line; a deleted one is not listed"
    );
    assert_eq!(parse_changed_code(""), ChangedCode::default());
    // Mentions in documentation, string literals, and fixture text are not
    // the attribute; only a line that starts with it is.
    let mentions = "diff --git a/x b/x\n+++ b/crates/alpha/src/lib.rs\n@@ -0,0 +1,6 @@\n\
        +/// The exemption, `#[cfg_attr(coverage_nightly, coverage(off))]`.\n\
        +const MARKER: &str = \"coverage(off)\";\n\
        ++#[cfg_attr(coverage_nightly, coverage(off))]\n\
        +    let text = \"#[cfg_attr(coverage_nightly, coverage(off))]\";\n\
        +    #[cfg_attr( coverage_nightly , coverage(off) )]\n\
        +#![cfg_attr(coverage_nightly, feature(coverage_attribute))]\n";
    assert_eq!(
        parse_changed_code(mentions).exemptions,
        ["crates/alpha/src/lib.rs:5"]
    );
    assert_eq!(added_range("-1,2 +7,0 @@"), None);
    assert_eq!(
        added_range("-1,2 +0,0 @@"),
        None,
        "emptying a file adds nothing"
    );
    assert_eq!(added_range("-1 +9 @@ fn x"), Some((9, 9)));
    assert_eq!(
        added_range("-1 +x,2 @@"),
        None,
        "a malformed header adds nothing"
    );
    assert_eq!(added_range("-1 @@"), None);
}

#[test]
fn closures_are_recognized_in_both_demangling_schemes() {
    assert!(is_closure("alpha::run::{closure#0}"));
    assert!(is_closure("alpha::run::{{closure}}"));
    assert!(!is_closure("alpha::run"));
    assert!(!is_closure("<alpha::Thing as core::fmt::Display>::fmt"));
}

#[test]
fn only_new_named_functions_without_executions_fail() {
    let functions = vec![
        function("crates/alpha/src/lib.rs", 50, "alpha::untouched"),
        function("crates/alpha/src/lib.rs", 5, "alpha::added::{closure#0}"),
        function("crates/beta/src/new.rs", 1, "beta::fresh"),
        function("crates/alpha/src/lib.rs", 4, "alpha::added"),
        function("crates/alpha/src/lib.rs", 4, "alpha::added"),
        function("crates/gamma/src/lib.rs", 4, "gamma::elsewhere"),
    ];
    let untested =
        new_functions_without_executions(&functions, &parse_changed_code(TWO_FILE_DIFF).added);
    assert_eq!(
        untested,
        [&functions[3], &functions[2]],
        "sorted by file and line, deduplicated, closures and untouched code excluded"
    );
}

#[test]
fn the_line_coverage_check_allows_only_the_configured_drop() {
    let baseline = WorkspaceLineCoverage {
        percent: 80.0,
        covered: 80,
        total: 100,
    };
    let at = |percent| {
        Some(WorkspaceLineCoverage {
            percent,
            covered: 0,
            total: 100,
        })
    };
    assert_eq!(
        compare_workspace_line_coverage(baseline, at(79.8), 0.25),
        None
    );
    let failure = compare_workspace_line_coverage(baseline, at(79.7), 0.25).expect("fails");
    assert!(failure.contains("current=79.70%"), "{failure}");
    assert!(compare_workspace_line_coverage(baseline, None, 0.25).is_some());
    assert!(render_line_coverage(baseline, at(81.0), 0.25).contains("| 80.00 | 81.00 | +1.00 |"));
    let missing = render_line_coverage(baseline, None, 0.25);
    assert!(missing.contains("| missing | n/a |") && missing.contains("FAIL"));
}

#[test]
fn the_cli_requires_a_change_source_and_rejects_the_retired_ratchets() {
    let since = parse(&["--changed-since", "origin/main"]).expect("parses");
    assert_eq!(since.changed_since.as_deref(), Some("origin/main"));
    assert!(!since.promote_baseline);
    assert_eq!(since.allowed_workspace_line_coverage_drop, 0.25);
    let diff = parse(&["--changed-diff", "change.diff", "--report-file", "r.md"]).expect("parses");
    assert_eq!(diff.changed_diff, Some(PathBuf::from("change.diff")));
    assert_eq!(diff.report_file, Some(PathBuf::from("r.md")));
    assert!(
        parse(&["--promote-baseline"])
            .expect("parses")
            .promote_baseline
    );

    assert!(parse(&[]).is_err(), "the gate needs the change it judges");
    assert!(parse(&["--changed-since", "a", "--changed-diff", "b"]).is_err());
    assert!(parse(&["--promote-baseline", "--changed-since", "a"]).is_err());
    for retired in [
        "--enforce-trim-regressions",
        "--allowed-zero-count-growth=2",
        "--allowed-dead-likely-growth=2",
        "--allowed-total-candidate-growth=2",
        "--allowed-needs-targeted-test-growth=2",
        "--package=alpha",
    ] {
        assert!(
            parse(&["--changed-since", "a", retired]).is_err(),
            "{retired} is retired"
        );
    }
}

/// A coverage output directory holding trim candidates, a line summary at
/// `percent`, and a baseline at 80%.
fn coverage_fixture(dir: &Path, candidates: &str, percent: f64) {
    let output = dir.join("target/llvm-cov");
    fs::create_dir_all(&output).unwrap();
    fs::write(output.join("trim-candidates.json"), candidates).unwrap();
    let summary = serde_json::json!({
        "data": [{"totals": {"lines": {"covered": 8, "count": 10, "percent": percent}}}]
    });
    fs::write(output.join(DEFAULT_SUMMARY_FILE_NAME), summary.to_string()).unwrap();
    let baseline = serde_json::json!({
        "generated_by": GENERATED_BY,
        "generated_at_unix_secs": 0,
        "source_candidates_file": "target/llvm-cov/trim-candidates.json",
        "workspace_line_coverage_percent": 80.0,
        "workspace_lines_covered": 8,
        "workspace_lines_total": 10
    });
    let baseline_path = dir.join(DEFAULT_BASELINE_FILE_REL);
    fs::create_dir_all(baseline_path.parent().unwrap()).unwrap();
    fs::write(baseline_path, baseline.to_string()).unwrap();
}

const CANDIDATES: &str = r#"{"candidates": [
  {"package": "alpha", "file": "crates/alpha/src/lib.rs", "line": 4, "demangled_function": "alpha::added"},
  {"package": "alpha", "file": "crates/alpha/src/lib.rs", "line": 5, "demangled_function": "alpha::added::{closure#0}"},
  {"package": "alpha", "file": "crates/alpha/src/lib.rs", "line": 90, "demangled_function": "alpha::old"}
]}"#;

fn run_with_diff(dir: &Path, diff: &str) -> Result<()> {
    fs::write(dir.join("change.diff"), diff).unwrap();
    run(dir, &parse(&["--changed-diff", "change.diff"]).unwrap())
}

#[test]
fn the_gate_fails_on_a_new_untested_function_and_reports_it() {
    let dir = tempfile::tempdir().unwrap();
    coverage_fixture(dir.path(), CANDIDATES, 80.0);
    let error = run_with_diff(dir.path(), TWO_FILE_DIFF).expect_err("new untested function");
    let message = error.to_string();
    assert!(message.contains("1 new function(s)"), "{message}");
    assert!(
        message.contains("crates/alpha/src/lib.rs:4 alpha::added"),
        "{message}"
    );
    assert!(!message.contains("line coverage"), "{message}");
    let report = fs::read_to_string(dir.path().join(DEFAULT_REPORT_FILE_REL)).unwrap();
    assert!(report.contains("| `crates/alpha/src/lib.rs:4` | `alpha::added` |"));
    assert!(
        report.contains("- `crates/beta/src/new.rs:2`\n"),
        "the reviewer sees every exemption the change adds: {report}"
    );
    assert!(
        report.contains("| `alpha` | 2 | 1 |"),
        "informational counts: {report}"
    );
}

#[test]
fn the_gate_passes_when_the_change_adds_no_untested_function() {
    let dir = tempfile::tempdir().unwrap();
    coverage_fixture(dir.path(), CANDIDATES, 80.0);
    // The change touches only line 5, which holds a closure.
    let diff = "diff --git a/x b/x\n+++ b/crates/alpha/src/lib.rs\n@@ -5 +5 @@\n";
    run_with_diff(dir.path(), diff).expect("closures and old code do not fail the gate");
    let report = fs::read_to_string(dir.path().join(DEFAULT_REPORT_FILE_REL)).unwrap();
    assert!(report.contains("| _none_ | - |"));
    assert!(report.contains("## Coverage exemptions the change adds\n\n"));
    assert!(report.contains("- _none_\n"));
}

#[test]
fn the_gate_fails_on_a_line_coverage_drop_and_on_bad_inputs() {
    let dir = tempfile::tempdir().unwrap();
    coverage_fixture(dir.path(), CANDIDATES, 70.0);
    let error = run_with_diff(dir.path(), "").expect_err("line coverage dropped");
    assert!(
        error
            .to_string()
            .contains("workspace line coverage regressed")
    );

    coverage_fixture(dir.path(), r#"{"candidates": [{"file": "x.rs"}]}"#, 80.0);
    let error = run_with_diff(dir.path(), "").expect_err("malformed candidate");
    assert!(
        error.to_string().contains("has a candidate without"),
        "{error}"
    );
    coverage_fixture(dir.path(), "{}", 80.0);
    let error = run_with_diff(dir.path(), "").expect_err("no candidates array");
    assert!(error.to_string().contains("no candidates array"), "{error}");

    coverage_fixture(dir.path(), CANDIDATES, 80.0);
    let error = run(
        dir.path(),
        &parse(&["--changed-diff", "absent.diff"]).unwrap(),
    )
    .expect_err("missing diff");
    assert!(error.to_string().contains("absent.diff"), "{error}");
    let mut no_change = parse(&["--promote-baseline"]).unwrap();
    no_change.promote_baseline = false;
    let error = run(dir.path(), &no_change).expect_err("no change source");
    assert!(error.to_string().contains("--changed-since"), "{error}");

    fs::remove_file(dir.path().join(DEFAULT_BASELINE_FILE_REL)).unwrap();
    let error = run_with_diff(dir.path(), "").expect_err("missing baseline");
    assert!(error.to_string().contains("--promote-baseline"), "{error}");
    let empty = tempfile::tempdir().unwrap();
    let error = run_with_diff(empty.path(), "").expect_err("missing candidates");
    assert!(error.to_string().contains("coverage report"), "{error}");
}

#[test]
fn promotion_records_only_the_workspace_line_coverage() {
    let dir = tempfile::tempdir().unwrap();
    coverage_fixture(dir.path(), CANDIDATES, 82.5);
    run(dir.path(), &parse(&["--promote-baseline"]).unwrap()).expect("promotes");
    let baseline = load_baseline(&dir.path().join(DEFAULT_BASELINE_FILE_REL)).unwrap();
    assert_eq!(baseline.workspace_line_coverage_percent, 82.5);
    assert_eq!(
        (
            baseline.workspace_lines_covered,
            baseline.workspace_lines_total
        ),
        (8, 10)
    );
    let raw = fs::read_to_string(dir.path().join(DEFAULT_BASELINE_FILE_REL)).unwrap();
    assert!(!raw.contains("packages"), "{raw}");

    fs::remove_file(
        dir.path()
            .join("target/llvm-cov")
            .join(DEFAULT_SUMMARY_FILE_NAME),
    )
    .unwrap();
    let error =
        run(dir.path(), &parse(&["--promote-baseline"]).unwrap()).expect_err("nothing to promote");
    assert!(
        error.to_string().contains("no workspace line coverage"),
        "{error}"
    );
}

fn git(repo: &Path, args: &[&str]) {
    let status = Command::new("git")
        .args([
            "-c",
            "user.name=Gate",
            "-c",
            "user.email=gate@fixture.invalid",
        ])
        .arg("-C")
        .arg(repo)
        .args(args)
        .status()
        .expect("git runs");
    assert!(status.success(), "git {args:?}");
}

#[test]
fn changed_since_diffs_the_rust_sources_from_the_merge_base() {
    let dir = tempfile::tempdir().unwrap();
    let repo = dir.path();
    git(repo, &["init", "-q", "-b", "main"]);
    fs::create_dir_all(repo.join("crates/alpha/src")).unwrap();
    fs::write(repo.join("crates/alpha/src/lib.rs"), "fn kept() {}\n").unwrap();
    git(repo, &["add", "-A"]);
    git(repo, &["commit", "-q", "-m", "base"]);
    git(repo, &["branch", "base"]);
    fs::write(
        repo.join("crates/alpha/src/lib.rs"),
        "fn kept() {}\n\nfn added() {}\n",
    )
    .unwrap();
    fs::write(repo.join("notes.txt"), "not Rust\n").unwrap();
    git(repo, &["add", "-A"]);
    git(repo, &["commit", "-q", "-m", "change"]);

    let diff = changed_code_diff(repo, "base", "HEAD").unwrap();
    assert!(!diff.contains("notes.txt"), "{diff}");
    assert_eq!(
        parse_changed_code(&diff).added,
        AddedLines::from([("crates/alpha/src/lib.rs".to_string(), vec![(2, 3)])])
    );
    let candidates = r#"{"candidates": [
      {"package": "alpha", "file": "crates/alpha/src/lib.rs", "line": 3, "demangled_function": "alpha::added"}
    ]}"#;
    coverage_fixture(repo, candidates, 80.0);
    let error = run(repo, &parse(&["--changed-since", "base"]).unwrap()).expect_err("untested");
    assert!(
        error.to_string().contains("lib.rs:3 alpha::added"),
        "{error}"
    );
    let report = fs::read_to_string(repo.join(DEFAULT_REPORT_FILE_REL)).unwrap();
    assert!(report.contains("- change: `base...HEAD`"), "{report}");

    let error = changed_code_diff(repo, "no-such-rev", "HEAD").expect_err("bad base");
    assert!(error.to_string().contains("no-such-rev...HEAD"), "{error}");
}

#[test]
fn resolve_path_keeps_absolute_paths_and_roots_relative_ones() {
    let root = Path::new("/repo");
    let absolute = PathBuf::from("/elsewhere/candidates.json");
    assert_eq!(
        resolve_path(root, Some(&absolute), "default.json"),
        absolute
    );
    assert_eq!(
        resolve_path(root, Some(&PathBuf::from("target/x.json")), "default.json"),
        Path::new("/repo/target/x.json")
    );
    assert_eq!(
        resolve_path(root, None, "target/llvm-cov/trim-candidates.json"),
        Path::new("/repo/target/llvm-cov/trim-candidates.json")
    );
}

#[test]
fn metadata_paths_are_relative_to_the_working_directory_when_inside_it() {
    let cwd = std::env::current_dir().unwrap();
    assert_eq!(path_metadata_string(&cwd.join("a/b.json")), "a/b.json");
    assert_eq!(
        path_metadata_string(Path::new("/elsewhere/b.json")),
        "/elsewhere/b.json"
    );
    assert!(unix_timestamp_seconds() > 0);
}
