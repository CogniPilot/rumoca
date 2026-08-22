//! Unit coverage for the parts of the corpus gate that decide things.
//!
//! The corpus run itself needs a compiler and two checkouts, so what is tested
//! here is everything that turns a measurement into a verdict: reading a value
//! out of a trace, recognizing a diagnostic code, refusing an inadmissible
//! manifest, and judging a run against its pin in both directions. Plus one
//! test that the checked-in manifest is the roster it claims to be.

use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

use super::execution::{ModelRun, artifact_stem, first_diagnostic_code, strip_ansi};
use super::manifest::{
    Check, Corpus, CorpusEntry, CorpusManifest, Expectation, PinnedObservation, validate,
};
use super::record::propose_entry;
use super::selected_manifest;
use super::trace::{Trace, split_csv_record};
use super::verdict::judge;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("workspace root")
}

const SAMPLE_TRACE: &str = "time,x,\"a[1,2]\"\n0,1,10\n0.01,2,20\n0.02,3,30\n";

fn simulate_check() -> Check {
    Check::Simulate {
        t_end: 0.02,
        dt: 0.01,
        solver: "rk-like".to_string(),
    }
}

fn entry(check: Check, expect: Expectation) -> CorpusEntry {
    CorpusEntry {
        id: "row".to_string(),
        corpus: Corpus::Msl,
        model: "Some.Model".to_string(),
        entry_point: "Modelica 4.1.0/package.mo".to_string(),
        why: "coverage".to_string(),
        check,
        expect,
    }
}

fn run(succeeded: bool, output: &str, trace_path: Option<PathBuf>) -> ModelRun {
    ModelRun {
        succeeded,
        output: output.to_string(),
        trace_path,
        elapsed: Duration::from_millis(1),
        command_line: "rumoca ...".to_string(),
    }
}

fn write_trace(dir: &Path) -> PathBuf {
    let path = dir.join("trace.csv");
    fs::write(&path, SAMPLE_TRACE).expect("write trace");
    path
}

#[test]
fn quoted_variable_names_keep_their_commas() {
    assert_eq!(
        split_csv_record("time,x,\"a[1,2]\",y"),
        vec!["time", "x", "a[1,2]", "y"]
    );
    assert_eq!(
        split_csv_record("\"he said \"\"hi\"\"\""),
        vec!["he said \"hi\""]
    );
}

#[test]
fn a_probe_reads_the_last_sample_at_or_before_it() {
    let trace = Trace::parse(SAMPLE_TRACE).expect("parse");
    assert_eq!(trace.value_at("x", 0.0).expect("t0"), 1.0);
    // Between samples: right-continuous hold, not interpolation.
    assert_eq!(trace.value_at("x", 0.015).expect("mid"), 2.0);
    assert_eq!(trace.value_at("x", 0.02).expect("end"), 3.0);
    // A stamp that lands a whisker below the probe still counts.
    assert_eq!(
        trace.value_at("a[1,2]", 0.009_999_999_999).expect("eps"),
        20.0
    );
    assert_eq!(trace.stop_time(), 0.02);
    assert_eq!(
        trace.variable_names(),
        ["x".to_string(), "a[1,2]".to_string()]
    );
}

#[test]
fn a_missing_variable_is_an_error_rather_than_a_default() {
    let trace = Trace::parse(SAMPLE_TRACE).expect("parse");
    let error = trace.value_at("nope", 0.0).expect_err("unknown variable");
    assert!(
        format!("{error:#}").contains("does not carry `nope`"),
        "{error:#}"
    );
}

#[test]
fn a_short_row_is_refused_rather_than_padded() {
    let error = Trace::parse("time,x\n0,1\n0.01\n").expect_err("ragged");
    assert!(format!("{error:#}").contains("fields"), "{error:#}");
}

#[test]
fn diagnostic_codes_are_found_bracketed_bare_and_colored() {
    assert_eq!(
        first_diagnostic_code("  [EX002] input `reset` has no default"),
        Some("EX002".to_string())
    );
    assert_eq!(
        first_diagnostic_code(&strip_ansi("\u{1b}[31mEGT017\u{1b}[0m dynamic index")),
        Some("EGT017".to_string())
    );
    assert_eq!(first_diagnostic_code("Error: everything is fine"), None);
    assert_eq!(strip_ansi("\u{1b}[31mred\u{1b}[0m"), "red");
}

#[test]
fn a_row_id_becomes_a_safe_artifact_stem() {
    assert_eq!(
        artifact_stem("msl/blocks-examples-x"),
        "msl_blocks-examples-x"
    );
}

#[test]
fn the_checked_in_manifest_is_admissible_and_covers_the_canary_roster() {
    let root = workspace_root();
    let manifest = super::manifest::load(&super::manifest::manifest_path(&root))
        .expect("the checked-in corpus manifest must load");
    let roster: Vec<String> = serde_json::from_str(
        &fs::read_to_string(root.join("infra/verification/msl-canary-20.json"))
            .expect("canary roster"),
    )
    .expect("canary roster is a list of model names");
    for model in &roster {
        assert!(
            manifest
                .entries
                .iter()
                .any(|entry| entry.corpus == Corpus::Msl && &entry.model == model),
            "the corpus manifest does not gate canary model {model}"
        );
    }
    let flight: Vec<&str> = manifest
        .entries
        .iter()
        .filter(|entry| entry.corpus == Corpus::FlightModels)
        .map(|entry| entry.model.as_str())
        .collect();
    for model in [
        "Vehicles.Rdd2.NavigationEstimator",
        "Vehicles.Rdd2.Controller",
        "Vehicles.Cubs2.OuterLoop",
        "Estimation.StrapdownINS.UKF.Estimator",
    ] {
        assert!(
            flight.contains(&model),
            "the corpus manifest does not gate flight model {model}"
        );
    }
}

#[test]
fn an_inadmissible_manifest_is_refused_at_load_time() {
    let base = CorpusManifest {
        schema_version: 1,
        runtime_budget_seconds: 180.0,
        entries: vec![entry(
            simulate_check(),
            Expectation::Succeeds {
                observations: vec![PinnedObservation {
                    variable: "x".to_string(),
                    time: 0.0,
                    value: 1.0,
                    tolerance: 1.0e-9,
                }],
            },
        )],
    };
    validate(&base).expect("the baseline manifest is admissible");

    let mut duplicated = base.clone();
    duplicated.entries.push(duplicated.entries[0].clone());
    assert_refused(&duplicated, "duplicate corpus entry id");

    let mut bad_solver = base.clone();
    bad_solver.entries[0].check = Check::Simulate {
        t_end: 0.02,
        dt: 0.01,
        solver: "euler".to_string(),
    };
    assert_refused(&bad_solver, "unknown solver");

    let mut late_probe = base.clone();
    if let Expectation::Succeeds { observations } = &mut late_probe.entries[0].expect {
        observations[0].time = 5.0;
    }
    assert_refused(&late_probe, "outside the run");

    let mut zero_tolerance = base.clone();
    if let Expectation::Succeeds { observations } = &mut zero_tolerance.entries[0].expect {
        observations[0].tolerance = 0.0;
    }
    assert_refused(&zero_tolerance, "positive tolerance");

    let mut observed_compile = base.clone();
    observed_compile.entries[0].check = Check::Compile {
        target: "galec-production".to_string(),
    };
    assert_refused(&observed_compile, "compile check produces no trace");

    let mut wrong_version = base;
    wrong_version.schema_version = 99;
    assert_refused(&wrong_version, "schema_version");
}

fn assert_refused(manifest: &CorpusManifest, needle: &str) {
    let error = validate(manifest).expect_err("manifest should be refused");
    assert!(
        format!("{error:#}").contains(needle),
        "expected `{needle}` in: {error:#}"
    );
}

#[test]
fn a_row_pinned_to_compile_that_stops_compiling_is_red() {
    let entry = entry(
        Check::Compile {
            target: "galec-production".to_string(),
        },
        Expectation::Succeeds {
            observations: Vec::new(),
        },
    );
    let verdict = judge(&entry, &run(false, "EGT017 dynamic index", None));
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("EGT017"),
        "{:?}",
        verdict.findings
    );
    assert!(judge(&entry, &run(true, "", None)).passed());
}

#[test]
fn a_row_pinned_to_be_refused_that_starts_compiling_is_also_red() {
    let entry = entry(
        Check::Compile {
            target: "galec-production".to_string(),
        },
        Expectation::Refused {
            diagnostic: "EGT017".to_string(),
        },
    );
    let improved = judge(&entry, &run(true, "", None));
    assert!(!improved.passed());
    assert!(
        improved.findings[0].contains("the run succeeded"),
        "{:?}",
        improved.findings
    );
    assert!(judge(&entry, &run(false, "[EGT017] nope", None)).passed());
}

#[test]
fn a_changed_refusal_reason_is_red() {
    let entry = entry(
        Check::Compile {
            target: "galec-production".to_string(),
        },
        Expectation::Refused {
            diagnostic: "EGT017".to_string(),
        },
    );
    let verdict = judge(&entry, &run(false, "[EGT001] different", None));
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("refused with EGT001"),
        "{:?}",
        verdict.findings
    );
    let unnamed = judge(&entry, &run(false, "it just died", None));
    assert!(!unnamed.passed());
    assert!(
        unnamed.findings[0].contains("without any diagnostic code"),
        "{:?}",
        unnamed.findings
    );
}

#[test]
fn a_pinned_reading_is_compared_within_its_tolerance() {
    let directory = tempfile::tempdir().expect("tempdir");
    let trace_path = write_trace(directory.path());
    let pinned = |value: f64, tolerance: f64| {
        entry(
            simulate_check(),
            Expectation::Succeeds {
                observations: vec![PinnedObservation {
                    variable: "x".to_string(),
                    time: 0.02,
                    value,
                    tolerance,
                }],
            },
        )
    };
    let measured = run(true, "", Some(trace_path));
    assert!(judge(&pinned(3.0, 1.0e-9), &measured).passed());
    let drifted = judge(&pinned(2.0, 1.0e-9), &measured);
    assert!(!drifted.passed());
    assert!(
        drifted.findings[0].contains("is 3 but Some.Model is pinned to 2"),
        "{:?}",
        drifted.findings
    );
    // Inside a wider tolerance the same reading is accepted.
    assert!(judge(&pinned(2.0, 2.0), &measured).passed());
}

#[test]
fn a_simulate_row_that_pins_nothing_is_red() {
    let entry = entry(
        simulate_check(),
        Expectation::Succeeds {
            observations: Vec::new(),
        },
    );
    let verdict = judge(&entry, &run(true, "", None));
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("pins no reading"),
        "{:?}",
        verdict.findings
    );
}

#[test]
fn a_proposal_records_what_the_run_actually_did() {
    let directory = tempfile::tempdir().expect("tempdir");
    let trace_path = write_trace(directory.path());
    let unpinned = entry(
        simulate_check(),
        Expectation::Succeeds {
            observations: Vec::new(),
        },
    );
    let proposed = propose_entry(&unpinned, &run(true, "", Some(trace_path.clone())));
    let Expectation::Succeeds { observations } = &proposed.expect else {
        panic!("a successful run proposes a success");
    };
    assert!(
        observations
            .iter()
            .any(|observation| observation.variable == "x" && observation.time == 0.02),
        "{observations:?}"
    );

    let refused = propose_entry(&unpinned, &run(false, "[ED019] unsupported", None));
    assert_eq!(
        refused.expect,
        Expectation::Refused {
            diagnostic: "ED019".to_string()
        }
    );
}

#[test]
fn selecting_an_unknown_row_is_an_error() {
    let manifest = CorpusManifest {
        schema_version: 1,
        runtime_budget_seconds: 180.0,
        entries: vec![entry(
            Check::Compile {
                target: "galec-production".to_string(),
            },
            Expectation::Succeeds {
                observations: Vec::new(),
            },
        )],
    };
    assert_eq!(
        selected_manifest(&manifest, &[])
            .expect("all rows")
            .entries
            .len(),
        1
    );
    assert_eq!(
        selected_manifest(&manifest, &["row".to_string()])
            .expect("named row")
            .entries
            .len(),
        1
    );
    let error = selected_manifest(&manifest, &["missing".to_string()]).expect_err("unknown row");
    assert!(
        format!("{error:#}").contains("no corpus row has id"),
        "{error:#}"
    );
}

/// An explicitly named models root is authoritative: when it is unusable the
/// candidate list holds only it, so resolution falls through to "corpus
/// unmeasured" instead of silently measuring a usable fallback.
#[test]
fn an_explicit_models_root_is_never_replaced_by_a_fallback() {
    let scratch = tempfile::tempdir().expect("temp dir for the probe");
    let missing = scratch.path().join("not-a-corpus");
    let config = super::roots::CorpusConfig {
        models_root: Some(scratch.path().join("some-other-corpus")),
        msl_root: None,
    };
    let candidates =
        super::roots::flight_model_candidates(scratch.path(), Some(&missing), Some(&config));
    assert_eq!(
        candidates.len(),
        1,
        "an explicit root must be the only candidate, got: {candidates:?}"
    );
    assert_eq!(candidates[0].0, "--models-root");
    assert_eq!(candidates[0].1, missing);
}
