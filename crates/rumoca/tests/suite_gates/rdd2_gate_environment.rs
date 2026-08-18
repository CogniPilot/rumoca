//! Shared environment resolution for the two RDD2 metric gates: where the
//! model corpus is, which binary measures it, and which corpus revision the
//! numbers came from.
//!
//! This module exists because the two gates' docs claim they "can never
//! disagree about which checkout they are measuring". That was previously
//! enforced by six verbatim-duplicated helpers in two files, which is exactly
//! the kind of claim that stops being true the first time someone edits one
//! copy. Now it is true structurally.
//!
//! Gated with the gates themselves: unlike `text_metrics.rs`, nothing here is
//! meaningful without a corpus and a release build, so compiling it
//! unconditionally would only produce dead code.

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// The baseline document, relative to the crate root. It carries the perf
/// numbers AND the corpus location/revision that both gates share.
pub(super) const BASELINE_RELATIVE_PATH: &str = "tests/rdd2_perf_baseline.json";

pub(super) fn crate_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

pub(super) fn workspace_root() -> PathBuf {
    crate_dir()
        .parent()
        .and_then(Path::parent)
        .expect("workspace root is two levels above crates/rumoca")
        .to_path_buf()
}

pub(super) fn baseline_path() -> PathBuf {
    crate_dir().join(BASELINE_RELATIVE_PATH)
}

/// Read and parse the checked-in baseline.
pub(super) fn read_baseline() -> serde_json::Value {
    let path = baseline_path();
    let text = fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read baseline {}: {error}", path.display()));
    serde_json::from_str(&text)
        .unwrap_or_else(|error| panic!("parse baseline {}: {error}", path.display()))
}

pub(super) fn baseline_str<'a>(baseline: &'a serde_json::Value, key: &str) -> &'a str {
    baseline[key]
        .as_str()
        .unwrap_or_else(|| panic!("baseline key `{key}` must be a string"))
}

pub(super) fn baseline_f64(baseline: &serde_json::Value, key: &str) -> f64 {
    baseline[key]
        .as_f64()
        .unwrap_or_else(|| panic!("baseline key `{key}` must be a number"))
}

/// First entry of `MODELICAPATH` that contains the RDD2 stack, if that variable
/// is set. `MODELICAPATH` is the Modelica-standard override and is already how
/// `msl_sim_regression.rs` locates out-of-tree libraries, so these gates do not
/// invent a knob of their own -- and SPEC_0018 bans `RUMOCA_*` variables.
pub(super) fn modelicapath_entry() -> Option<PathBuf> {
    let raw = env::var_os("MODELICAPATH")?;
    env::split_paths(&raw).find(|path| path.join("Vehicles").is_dir())
}

/// The `modelica_models` checkout holding the RDD2 stack.
///
/// Absence is a HARD FAILURE, not a skip. These gates only compile under an
/// explicitly requested feature, so whoever enabled it asked for a
/// measurement; quietly reporting "ok" because the corpus was missing is the
/// silent-pass failure mode both gates exist to prevent, and libtest swallows
/// a skip message unless `--nocapture` happens to be on.
pub(super) fn require_model_library(baseline: &serde_json::Value) -> PathBuf {
    if let Some(from_env) = modelicapath_entry() {
        return from_env;
    }
    let recorded = PathBuf::from(baseline_str(baseline, "source_root"));
    assert!(
        recorded.is_dir(),
        "the RDD2 metric gates need the out-of-tree modelica_models corpus, which is not at \
         {}.\nPoint them at your checkout in one of two ways:\n  \
         MODELICAPATH=/path/to/modelica_models cargo test -p rumoca --features \
         rdd2-metric-gates --test suite_gates\nor edit `source_root` in {}.\n\
         This is a hard failure rather than a skip because the gates only build when their \
         feature is explicitly requested: a silent pass would mean nothing was measured.",
        recorded.display(),
        baseline_path().display(),
    );
    recorded
}

/// The release binary the gates measure with. Absence is a hard failure with
/// instructions, for the same reason.
pub(super) fn release_binary() -> PathBuf {
    let path = workspace_root().join("target/release/rumoca");
    assert!(
        path.is_file(),
        "the RDD2 metric gates measure the RELEASE binary, which is missing at {}.\n\
         Build it first:\n    cargo build --release -p rumoca",
        path.display()
    );
    path
}

/// The corpus revision the numbers came from: the short commit, and whether
/// the checkout has uncommitted changes.
///
/// Both gates print this. The ceilings and the perf baseline are measured
/// against an out-of-tree corpus that nothing in this repo pins, so without it
/// a ratchet failure is ambiguous: a reader cannot tell a compiler regression
/// from someone having edited the model.
pub(super) fn corpus_revision(source_root: &Path) -> (String, bool) {
    let commit = git(source_root, &["rev-parse", "--short", "HEAD"])
        .unwrap_or_else(|| "unknown".to_string());
    let dirty = git(source_root, &["status", "--porcelain"]).is_some_and(|s| !s.trim().is_empty());
    (commit, dirty)
}

/// Run a git command in `dir`, returning trimmed stdout on success.
fn git(dir: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(args)
        .output()
        .ok()?;
    output
        .status
        .success()
        .then(|| String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// One line naming the corpus and revision the numbers were taken from,
/// warning when it is not the revision the baseline recorded.
///
/// Only the commit is compared. A dirty working tree is reported but not
/// warned about, because this corpus routinely carries unrelated local edits
/// (READMEs, flake files) and a warning that fires constantly is one nobody
/// reads. The dirty flag is still printed, so a reader chasing an unexplained
/// number knows the tree was not pristine.
pub(super) fn corpus_provenance(baseline: &serde_json::Value, source_root: &Path) -> String {
    let (commit, dirty) = corpus_revision(source_root);
    let recorded = baseline["corpus_revision"].as_str().unwrap_or("unrecorded");
    let dirty_note = if dirty { ", working tree dirty" } else { "" };
    let mismatch = if commit == recorded {
        String::new()
    } else {
        format!(
            "\n  NOTE: the ceilings and the perf baseline were calibrated against corpus \
             {recorded}; this is {commit}. A failure may reflect a corpus change rather than a \
             compiler change -- reconcile before treating it as a regression."
        )
    };
    format!(
        "corpus {} @ {commit}{dirty_note} (baseline recorded {recorded}){mismatch}",
        source_root.display()
    )
}
