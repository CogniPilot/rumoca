//! Unit coverage for the parts of the corpus gate that decide things.
//!
//! The corpus run itself needs a compiler and two checkouts, so what is tested
//! here is everything that turns a measurement into a verdict: reading a value
//! out of a trace, telling a run that moved from one that stood still,
//! recognizing a diagnostic code, reading a compile row's artifacts back off
//! disk, killing a row that never finishes, refusing an inadmissible manifest,
//! and judging a run against its pin in both directions. Plus one test that the
//! checked-in manifest is the roster it claims to be.

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Duration;

use super::artifacts;
use super::execution::{ModelRun, artifact_stem, first_diagnostic_code, strip_ansi};
use super::manifest::{
    ArtifactKind, Check, Corpus, CorpusEntry, CorpusManifest, Expectation, ExpectedArtifact,
    PinnedObservation, validate,
};
use super::observability::{self, Unobserved};
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

/// `x` and `a[1,2]` move over the run; `held` never does.
const SAMPLE_TRACE: &str = "time,x,\"a[1,2]\",held\n0,1,10,7\n0.01,2,20,7\n0.02,3,30,7\n";

/// The same run truncated to its first sample: what a simulator that stopped
/// after initialization, or a truncated file, leaves behind.
const TRUNCATED_TRACE: &str = "time,x,\"a[1,2]\",held\n0,1,10,7\n";

fn simulate_check() -> Check {
    Check::Simulate {
        t_end: 0.02,
        dt: 0.01,
        solver: "rk-like".to_string(),
    }
}

fn compile_check() -> Check {
    Check::Compile {
        target: "galec-production".to_string(),
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

fn succeeds(observations: Vec<PinnedObservation>) -> Expectation {
    Expectation::Succeeds {
        observations,
        artifacts: Vec::new(),
    }
}

fn emits(artifacts: Vec<ExpectedArtifact>) -> Expectation {
    Expectation::Succeeds {
        observations: Vec::new(),
        artifacts,
    }
}

fn pinned(variable: &str, time: f64, value: f64, tolerance: f64) -> PinnedObservation {
    PinnedObservation {
        variable: variable.to_string(),
        time,
        value,
        tolerance,
    }
}

fn artifact(path: &str, kind: ArtifactKind, min_bytes: u64) -> ExpectedArtifact {
    ExpectedArtifact {
        path: path.to_string(),
        kind,
        min_bytes,
    }
}

fn run(succeeded: bool, output: &str, trace_path: Option<PathBuf>) -> ModelRun {
    ModelRun {
        succeeded,
        output: output.to_string(),
        trace_path,
        output_dir: None,
        elapsed: Duration::from_millis(1),
        timed_out: false,
        deadline: Duration::from_secs(60),
        command_line: "rumoca ...".to_string(),
    }
}

fn compile_run(output_dir: &Path) -> ModelRun {
    let mut measured = run(true, "", None);
    measured.output_dir = Some(output_dir.to_path_buf());
    measured
}

fn write_trace(dir: &Path, contents: &str) -> PathBuf {
    let path = dir.join("trace.csv");
    fs::write(&path, contents).expect("write trace");
    path
}

/// A zip holding exactly `entries`, so a test can hand the container check a
/// container that does or does not carry the `__content.xml` marker the eFMU
/// packaging step writes at the archive root.
fn write_container(path: &Path, entries: &[(&str, &str)]) {
    let file = fs::File::create(path).expect("create container");
    let mut archive = zip::ZipWriter::new(file);
    let options =
        zip::write::SimpleFileOptions::default().compression_method(zip::CompressionMethod::Stored);
    for (name, contents) in entries {
        archive.start_file(*name, options).expect("start entry");
        archive.write_all(contents.as_bytes()).expect("write entry");
    }
    archive.finish().expect("finish container");
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
        ["x".to_string(), "a[1,2]".to_string(), "held".to_string()]
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
fn a_spread_spans_every_sample_inside_the_run() {
    let trace = Trace::parse(SAMPLE_TRACE).expect("parse");
    assert_eq!(trace.spread_until("x", 0.02), Some(2.0));
    // Only the samples inside the window count.
    assert_eq!(trace.spread_until("x", 0.01), Some(1.0));
    // A variable that never changes spans nothing, however many samples it has.
    assert_eq!(trace.spread_until("held", 0.02), Some(0.0));
    assert_eq!(trace.spread_until("nope", 0.02), None);
    // One sample is one value, so nothing in a truncated trace has moved.
    let truncated = Trace::parse(TRUNCATED_TRACE).expect("parse");
    assert_eq!(truncated.spread_until("x", 0.02), Some(0.0));
}

#[test]
fn movement_the_tolerance_swallows_does_not_count_as_movement() {
    let trace = Trace::parse("time,x\n0,1\n0.02,1.0000001\n").expect("parse");
    // 1e-7 of travel is real, but a pin carrying a 1e-6 tolerance reads the
    // same verdict whether the run moved or stood still.
    assert!(!observability::moves(&trace, "x", 0.02, 1.0e-6));
    assert!(observability::moves(&trace, "x", 0.02, 1.0e-9));
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

/// Every checked-in row that is pinned to succeed must state what it is judged
/// on. The gate reports the omission at run time, but a row that reaches the
/// corpus without one has to be caught here rather than in a three-minute run
/// against two checkouts.
#[test]
fn every_checked_in_success_row_states_what_it_is_judged_on() {
    let root = workspace_root();
    let manifest = super::manifest::load(&super::manifest::manifest_path(&root))
        .expect("the checked-in corpus manifest must load");
    for entry in &manifest.entries {
        let Expectation::Succeeds {
            observations,
            artifacts,
        } = &entry.expect
        else {
            continue;
        };
        match &entry.check {
            Check::Compile { target: _ } => assert!(
                !artifacts.is_empty(),
                "compile row `{}` declares no output artifact",
                entry.id
            ),
            Check::Simulate {
                t_end: _,
                dt: _,
                solver: _,
            } => assert!(
                !observations.is_empty(),
                "simulate row `{}` pins no reading",
                entry.id
            ),
        }
    }
}

#[test]
fn an_inadmissible_manifest_is_refused_at_load_time() {
    let base = CorpusManifest {
        schema_version: 2,
        runtime_budget_seconds: 180.0,
        entries: vec![entry(
            simulate_check(),
            succeeds(vec![pinned("x", 0.0, 1.0, 1.0e-9)]),
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
    if let Expectation::Succeeds {
        observations,
        artifacts: _,
    } = &mut late_probe.entries[0].expect
    {
        observations[0].time = 5.0;
    }
    assert_refused(&late_probe, "outside the run");

    let mut zero_tolerance = base.clone();
    if let Expectation::Succeeds {
        observations,
        artifacts: _,
    } = &mut zero_tolerance.entries[0].expect
    {
        observations[0].tolerance = 0.0;
    }
    assert_refused(&zero_tolerance, "positive tolerance");

    let mut observed_compile = base.clone();
    observed_compile.entries[0].check = compile_check();
    assert_refused(&observed_compile, "compile check produces no trace");

    let mut wrong_version = base;
    wrong_version.schema_version = 99;
    assert_refused(&wrong_version, "schema_version");
}

#[test]
fn an_inadmissible_artifact_expectation_is_refused_at_load_time() {
    let base = CorpusManifest {
        schema_version: 2,
        runtime_budget_seconds: 180.0,
        entries: vec![entry(
            compile_check(),
            emits(vec![artifact("Model/__content.xml", ArtifactKind::File, 1)]),
        )],
    };
    validate(&base).expect("the baseline manifest is admissible");

    let mut on_a_simulate_row = base.clone();
    on_a_simulate_row.entries[0].check = simulate_check();
    assert_refused(&on_a_simulate_row, "cannot carry expected artifacts");

    let mut escaping = base.clone();
    replace_artifact(
        &mut escaping,
        artifact("../elsewhere", ArtifactKind::File, 1),
    );
    assert_refused(&escaping, "must not escape");

    let mut absolute = base.clone();
    replace_artifact(
        &mut absolute,
        artifact("/etc/passwd", ArtifactKind::File, 1),
    );
    assert_refused(&absolute, "must be relative");

    let mut allowed_to_be_empty = base;
    replace_artifact(
        &mut allowed_to_be_empty,
        artifact("Model/__content.xml", ArtifactKind::File, 0),
    );
    assert_refused(&allowed_to_be_empty, "positive min_bytes");
}

/// Distinct ids are not enough on their own: the artifact stem is what names a
/// row's output directory and trace file, so two ids that flatten to one stem
/// would each be judged on whatever the other one wrote.
#[test]
fn two_ids_that_flatten_to_one_artifact_stem_are_refused() {
    let row = entry(
        compile_check(),
        emits(vec![artifact("Model.c", ArtifactKind::File, 1)]),
    );
    let mut manifest = CorpusManifest {
        schema_version: 2,
        runtime_budget_seconds: 180.0,
        entries: vec![row.clone(), row],
    };
    manifest.entries[0].id = "flight/one".to_string();
    manifest.entries[1].id = "flight_one".to_string();
    assert_eq!(
        artifact_stem(&manifest.entries[0].id),
        artifact_stem(&manifest.entries[1].id)
    );
    assert_refused(&manifest, "shares its artifact stem");
}

fn replace_artifact(manifest: &mut CorpusManifest, replacement: ExpectedArtifact) {
    if let Expectation::Succeeds {
        observations: _,
        artifacts,
    } = &mut manifest.entries[0].expect
    {
        *artifacts = vec![replacement];
    }
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
    let directory = tempfile::tempdir().expect("tempdir");
    fs::write(directory.path().join("Model.c"), "int main(void);\n").expect("write artifact");
    let entry = entry(
        compile_check(),
        emits(vec![artifact("Model.c", ArtifactKind::File, 1)]),
    );
    let verdict = judge(&entry, &run(false, "EGT017 dynamic index", None));
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("EGT017"),
        "{:?}",
        verdict.findings
    );
    assert!(judge(&entry, &compile_run(directory.path())).passed());
}

#[test]
fn a_row_pinned_to_be_refused_that_starts_compiling_is_also_red() {
    let entry = entry(
        compile_check(),
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
        compile_check(),
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
    let trace_path = write_trace(directory.path(), SAMPLE_TRACE);
    let row = |value: f64, tolerance: f64| {
        entry(
            simulate_check(),
            succeeds(vec![
                pinned("x", 0.0, 1.0, 1.0e-9),
                pinned("x", 0.02, value, tolerance),
            ]),
        )
    };
    let measured = run(true, "", Some(trace_path));
    assert!(judge(&row(3.0, 1.0e-9), &measured).passed());
    let drifted = judge(&row(2.0, 1.0e-9), &measured);
    assert!(!drifted.passed());
    assert!(
        drifted.findings[0].contains("is 3 but Some.Model is pinned to 2"),
        "{:?}",
        drifted.findings
    );
    // Inside a wider tolerance the same reading is accepted.
    assert!(judge(&row(2.0, 1.5), &measured).passed());
    // A tolerance as wide as the whole travel reads the same for a run that
    // moved and one that stood still, so the row stops being evidence.
    let blind = judge(&row(2.0, 2.0), &measured);
    assert!(!blind.passed());
    assert!(
        blind.findings[0].contains("hold one value across the whole run"),
        "{:?}",
        blind.findings
    );
}

#[test]
fn a_simulate_row_that_pins_nothing_is_red() {
    let entry = entry(simulate_check(), succeeds(Vec::new()));
    let verdict = judge(&entry, &run(true, "", None));
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("pins no reading"),
        "{:?}",
        verdict.findings
    );
}

/// The weakness this closes: every probe on a variable the run never moves
/// reports the initial value, so the row reads the same for a completed run and
/// for one that stopped after initialization.
#[test]
fn a_simulate_row_whose_pins_never_move_is_red_as_unobserved() {
    let directory = tempfile::tempdir().expect("tempdir");
    let trace_path = write_trace(directory.path(), SAMPLE_TRACE);
    let frozen = entry(
        simulate_check(),
        succeeds(vec![
            pinned("held", 0.0, 7.0, 1.0e-9),
            pinned("held", 0.02, 7.0, 1.0e-6),
        ]),
    );
    let verdict = judge(&frozen, &run(true, "", Some(trace_path.clone())));
    assert!(!verdict.passed(), "a constant pin must not certify a run");
    assert!(
        verdict.findings[0].contains("hold one value across the whole run"),
        "{:?}",
        verdict.findings
    );
    // One variable that moves is enough to make the row evidence again.
    let moving = entry(
        simulate_check(),
        succeeds(vec![
            pinned("held", 0.0, 7.0, 1.0e-9),
            pinned("x", 0.02, 3.0, 1.0e-6),
        ]),
    );
    assert!(judge(&moving, &run(true, "", Some(trace_path))).passed());
}

/// The negative control for the re-recorded rows: a run truncated to one sample
/// must be red as unobserved, not green because every probe held.
#[test]
fn a_truncated_single_sample_trace_fails_as_unobserved() {
    let directory = tempfile::tempdir().expect("tempdir");
    let truncated = write_trace(directory.path(), TRUNCATED_TRACE);
    let row = entry(
        simulate_check(),
        succeeds(vec![
            pinned("x", 0.0, 1.0, 1.0e-9),
            pinned("x", 0.02, 3.0, 1.0e-6),
        ]),
    );
    let verdict = judge(&row, &run(true, "", Some(truncated)));
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("truncated to its first sample"),
        "{:?}",
        verdict.findings
    );
}

/// A pinned variable the trace does not carry must be named as the finding.
/// The unobserved verdict fires first structurally, but its advice (pin
/// something that moves) misdirects when the real defect is a vanished name,
/// so the comparison's precise message wins for exactly those pins.
#[test]
fn a_vanished_pin_is_reported_as_missing_not_as_unobserved() {
    let directory = tempfile::tempdir().expect("tempdir");
    let trace_path = write_trace(directory.path(), SAMPLE_TRACE);
    let renamed = entry(
        simulate_check(),
        succeeds(vec![
            pinned("renamed_away", 0.0, 1.0, 1.0e-9),
            pinned("renamed_away", 0.02, 3.0, 1.0e-6),
        ]),
    );
    let verdict = judge(&renamed, &run(true, "", Some(trace_path)));
    assert!(!verdict.passed(), "a vanished pin must stay red");
    assert!(
        verdict.findings[0].contains("renamed_away"),
        "the finding must name the vanished variable: {:?}",
        verdict.findings
    );
    assert!(
        !verdict.findings[0].contains("hold one value across the whole run"),
        "the unobserved advice misdirects here: {:?}",
        verdict.findings
    );
}

#[test]
fn both_unobserved_reasons_say_the_row_proves_only_that_the_process_exited_zero() {
    for reason in [Unobserved::NoReadings, Unobserved::EveryVariableConstant] {
        assert!(
            reason.finding().contains("process exited zero"),
            "{}",
            reason.finding()
        );
    }
}

#[test]
fn a_compile_row_that_declares_no_artifact_is_red() {
    let directory = tempfile::tempdir().expect("tempdir");
    let row = entry(compile_check(), emits(Vec::new()));
    let verdict = judge(&row, &compile_run(directory.path()));
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("declares no output artifact"),
        "{:?}",
        verdict.findings
    );
}

#[test]
fn a_declared_artifact_must_be_a_non_empty_file() {
    let directory = tempfile::tempdir().expect("tempdir");
    let present = artifact("Model.c", ArtifactKind::File, 1);
    let missing = artifacts::judge(directory.path(), std::slice::from_ref(&present));
    assert_eq!(missing.len(), 1);
    assert!(missing[0].contains("is missing"), "{missing:?}");

    fs::write(directory.path().join("Model.c"), "").expect("write empty");
    let empty = artifacts::judge(directory.path(), std::slice::from_ref(&present));
    assert_eq!(empty.len(), 1);
    assert!(empty[0].contains("0 bytes"), "{empty:?}");

    fs::write(directory.path().join("Model.c"), "int main(void);\n").expect("write artifact");
    assert!(artifacts::judge(directory.path(), std::slice::from_ref(&present)).is_empty());

    // A file that shrank below its pinned floor is a behavior change too.
    let floored = artifact("Model.c", ArtifactKind::File, 4096);
    let shrunk = artifacts::judge(directory.path(), std::slice::from_ref(&floored));
    assert_eq!(shrunk.len(), 1);
    assert!(shrunk[0].contains("pinned floor"), "{shrunk:?}");
}

#[test]
fn a_declared_efmu_container_must_open_as_a_zip_holding_its_content_marker() {
    let directory = tempfile::tempdir().expect("tempdir");
    let declared = artifact("Model.efmu", ArtifactKind::EfmuContainer, 1);
    let path = directory.path().join("Model.efmu");

    fs::write(&path, "not a zip at all").expect("write plain file");
    let plain = artifacts::judge(directory.path(), std::slice::from_ref(&declared));
    assert_eq!(plain.len(), 1);
    assert!(plain[0].contains("does not open as a zip"), "{plain:?}");

    write_container(&path, &[("AlgorithmCode/Model.alg", "block Model")]);
    let unmarked = artifacts::judge(directory.path(), std::slice::from_ref(&declared));
    assert_eq!(unmarked.len(), 1);
    assert!(
        unmarked[0].contains("carries no `__content.xml`"),
        "{unmarked:?}"
    );

    write_container(&path, &[("__content.xml", "")]);
    let hollow = artifacts::judge(directory.path(), std::slice::from_ref(&declared));
    assert_eq!(hollow.len(), 1);
    assert!(hollow[0].contains("empty `__content.xml`"), "{hollow:?}");

    write_container(&path, &[("__content.xml", "<Content/>")]);
    assert!(artifacts::judge(directory.path(), std::slice::from_ref(&declared)).is_empty());
}

#[test]
fn a_timed_out_row_is_red_whichever_way_its_pin_points() {
    let mut hung = run(true, "", None);
    hung.timed_out = true;
    hung.deadline = Duration::from_secs(64);
    for expect in [
        emits(vec![artifact("Model.c", ArtifactKind::File, 1)]),
        Expectation::Refused {
            diagnostic: "EGT017".to_string(),
        },
    ] {
        let verdict = judge(&entry(compile_check(), expect), &hung);
        assert!(!verdict.passed());
        assert!(
            verdict.findings[0].contains("timed out after 64s")
                && verdict.findings[0].contains("rumoca ..."),
            "{:?}",
            verdict.findings
        );
    }
}

#[test]
fn the_row_deadline_is_a_share_of_the_reviewed_budget_with_a_floor() {
    let row = entry(compile_check(), emits(Vec::new()));
    let manifest = |rows: usize, budget: f64| CorpusManifest {
        schema_version: 2,
        runtime_budget_seconds: budget,
        entries: (0..rows)
            .map(|index| {
                let mut clone = row.clone();
                clone.id = format!("row-{index}");
                clone
            })
            .collect(),
    };
    // Thirty times an even share of the budget.
    assert_eq!(manifest(20, 200.0).row_deadline(), Duration::from_secs(300));
    // Never below the floor, however many rows share the budget.
    assert_eq!(
        manifest(1000, 200.0).row_deadline(),
        Duration::from_secs(30)
    );
}

/// The gate is wired into `verify quick`, so a model that stops terminating has
/// to cost one row rather than the developer loop.
///
/// The stub sleeps in a child of its own rather than replacing itself with one,
/// which is the harder case: killing the process the gate spawned leaves that
/// grandchild alive, and a gate that read the child's streams to end-of-file
/// would wait on it anyway. Streams captured to files cannot be held open
/// against the gate that way.
#[cfg(unix)]
#[test]
fn a_row_that_never_finishes_is_killed_at_its_deadline() {
    use std::os::unix::fs::PermissionsExt;

    let directory = tempfile::tempdir().expect("tempdir");
    let stub = directory.path().join("hangs");
    fs::write(&stub, "#!/bin/sh\nsleep 60\n").expect("write stub");
    fs::set_permissions(&stub, fs::Permissions::from_mode(0o755)).expect("chmod stub");

    let context = super::execution::RunContext {
        rumoca: &stub,
        corpus_root: directory.path(),
        artifact_dir: directory.path(),
        cache_dir: directory.path(),
        deadline: Duration::from_millis(250),
    };
    let row = entry(compile_check(), emits(Vec::new()));
    let measured = super::execution::run_entry(&row, &context).expect("the row runs");
    assert!(measured.timed_out, "the stub never exits on its own");
    assert!(!measured.succeeded);
    assert!(
        measured.elapsed < Duration::from_secs(10),
        "the row was killed at its deadline, not waited out: {:?}",
        measured.elapsed
    );
    let verdict = judge(&row, &measured);
    assert!(!verdict.passed());
    assert!(
        verdict.findings[0].contains("timed out"),
        "{:?}",
        verdict.findings
    );
}

#[test]
fn a_proposal_records_what_the_run_actually_did() {
    let directory = tempfile::tempdir().expect("tempdir");
    let trace_path = write_trace(directory.path(), SAMPLE_TRACE);
    let unpinned = entry(simulate_check(), succeeds(Vec::new()));
    let proposed = propose_entry(&unpinned, &run(true, "", Some(trace_path.clone())));
    assert!(proposed.note.is_none(), "{:?}", proposed.note);
    let Expectation::Succeeds {
        observations,
        artifacts: _,
    } = &proposed.entry.expect
    else {
        panic!("a successful run proposes a success");
    };
    assert!(
        observations
            .iter()
            .any(|observation| observation.variable == "x" && observation.time == 0.02),
        "{observations:?}"
    );
    // The variable the run never moves is passed over, not kept as a fallback.
    assert!(
        observations
            .iter()
            .all(|observation| observation.variable != "held"),
        "{observations:?}"
    );

    let refused = propose_entry(&unpinned, &run(false, "[ED019] unsupported", None));
    assert_eq!(
        refused.entry.expect,
        Expectation::Refused {
            diagnostic: "ED019".to_string()
        }
    );
}

/// The recorder proposes only pins the gate would call evidence, so a proposal
/// cannot be pasted in and go green while certifying nothing.
#[test]
fn a_proposal_refuses_to_pin_a_run_that_moves_nothing() {
    let directory = tempfile::tempdir().expect("tempdir");
    let trace_path = write_trace(directory.path(), "time,held\n0,7\n0.01,7\n0.02,7\n");
    let unpinned = entry(simulate_check(), succeeds(Vec::new()));
    let proposed = propose_entry(&unpinned, &run(true, "", Some(trace_path.clone())));
    let Expectation::Succeeds {
        observations,
        artifacts: _,
    } = &proposed.entry.expect
    else {
        panic!("a successful run proposes a success");
    };
    assert!(observations.is_empty(), "{observations:?}");
    let note = proposed.note.expect("the operator has to be told");
    assert!(note.contains("raise t_end"), "{note}");

    // Re-recording an already vacuous row drops its pins rather than
    // refreshing them into another vacuous row.
    let vacuous = entry(
        simulate_check(),
        succeeds(vec![
            pinned("held", 0.0, 7.0, 1.0e-9),
            pinned("held", 0.02, 7.0, 1.0e-6),
        ]),
    );
    let rerecorded = propose_entry(&vacuous, &run(true, "", Some(trace_path)));
    let Expectation::Succeeds {
        observations,
        artifacts: _,
    } = &rerecorded.entry.expect
    else {
        panic!("a successful run proposes a success");
    };
    assert!(observations.is_empty(), "{observations:?}");
    assert!(rerecorded.note.is_some());
}

#[test]
fn a_proposal_carries_reviewed_artifact_expectations_across_untouched() {
    let directory = tempfile::tempdir().expect("tempdir");
    let declared = artifact("Model/Model.efmu", ArtifactKind::EfmuContainer, 512);
    let row = entry(compile_check(), emits(vec![declared.clone()]));
    let proposed = propose_entry(&row, &compile_run(directory.path()));
    assert_eq!(proposed.entry.expect, emits(vec![declared]));
    assert!(proposed.note.is_none());

    // A compile row with nothing declared is reported rather than filled in
    // from whatever happens to be on disk.
    let bare = entry(compile_check(), emits(Vec::new()));
    let proposed = propose_entry(&bare, &compile_run(directory.path()));
    let note = proposed.note.expect("the operator has to be told");
    assert!(note.contains("expect.artifacts"), "{note}");
}

#[test]
fn a_timed_out_row_proposes_nothing() {
    let mut hung = run(true, "", None);
    hung.timed_out = true;
    let pinned_row = entry(
        simulate_check(),
        succeeds(vec![pinned("x", 0.0, 1.0, 1.0e-9)]),
    );
    let proposed = propose_entry(&pinned_row, &hung);
    assert_eq!(proposed.entry.expect, pinned_row.expect);
    let note = proposed.note.expect("the operator has to be told");
    assert!(note.contains("measured nothing"), "{note}");
}

#[test]
fn selecting_an_unknown_row_is_an_error() {
    let manifest = CorpusManifest {
        schema_version: 2,
        runtime_budget_seconds: 180.0,
        entries: vec![entry(compile_check(), emits(Vec::new()))],
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
