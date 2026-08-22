//! The checked-in corpus pin: which real models are gated, how, and what each
//! one is expected to do.
//!
//! Every expectation lives in `infra/verification/corpus-pin.json` rather than
//! in Rust, so changing what the corpus is allowed to do is a reviewed diff on
//! a data file instead of an edit buried in gate logic. The loader is strict on
//! purpose: `deny_unknown_fields` everywhere, an explicit schema version, and a
//! validation pass that rejects a manifest which cannot be measured (a probe
//! past the end of the run, a zero tolerance, a duplicate id). A manifest the
//! gate silently half-understood would be worse than no manifest at all.

use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

/// Location of the pinned corpus manifest, relative to the workspace root.
pub(crate) const MANIFEST_PATH: &str = "infra/verification/corpus-pin.json";

/// Bumped whenever a field changes meaning. A manifest written for another
/// version is refused rather than reinterpreted.
pub(crate) const MANIFEST_SCHEMA_VERSION: u32 = 1;

/// Solver names the gate is willing to ask for. Mirrors `rumoca --solver`; a
/// manifest naming anything else is refused at load time instead of producing a
/// clap error from a subprocess halfway through the run.
const KNOWN_SOLVERS: [&str; 3] = ["auto", "bdf", "rk-like"];

/// The whole pinned corpus.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct CorpusManifest {
    pub(crate) schema_version: u32,
    /// Wall-clock budget the corpus check is designed to fit in, in seconds.
    ///
    /// Reported next to the measured wall time, never asserted. The sibling
    /// RDD2 performance guard records at length why a wall-clock assertion on a
    /// shared developer machine measures machine load rather than the compiler;
    /// this gate does not repeat that mistake. The number is here so a corpus
    /// addition that doubles the runtime is visible in review.
    pub(crate) runtime_budget_seconds: f64,
    pub(crate) entries: Vec<CorpusEntry>,
}

/// One gated (model, check) pair. A model checked two ways (two solvers, two
/// targets) appears twice, each with its own id, so a per-row failure names
/// exactly one thing.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct CorpusEntry {
    /// Stable key for this row. Unique across the manifest; used in the report
    /// and in the summary JSON.
    pub(crate) id: String,
    pub(crate) corpus: Corpus,
    /// Fully qualified Modelica class name.
    pub(crate) model: String,
    /// Package entry point passed to the compiler, relative to the corpus root.
    pub(crate) entry_point: String,
    /// Why this row is in the gate. Prose for the reviewer, not parsed.
    pub(crate) why: String,
    pub(crate) check: Check,
    pub(crate) expect: Expectation,
}

/// Which root a row's sources come from.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum Corpus {
    /// The out-of-tree `modelica_models` checkout carrying the RDD2/Cubs2
    /// flight stack. Never located by a path baked into this repository.
    FlightModels,
    /// The cached Modelica Standard Library release the MSL campaign already
    /// provisions.
    Msl,
}

impl Corpus {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::FlightModels => "flight-models",
            Self::Msl => "msl",
        }
    }
}

/// What the gate asks the compiler to do with the model.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(tag = "kind", rename_all = "kebab-case", deny_unknown_fields)]
pub(crate) enum Check {
    /// `rumoca compile --target <target>`: the model's established target.
    Compile { target: String },
    /// `rumoca sim --t-end <t_end> --dt <dt> --solver <solver>`, writing a CSV
    /// trace the pinned observations are read back from.
    Simulate { t_end: f64, dt: f64, solver: String },
}

impl Check {
    pub(crate) fn label(&self) -> String {
        match self {
            Self::Compile { target } => format!("compile --target {target}"),
            Self::Simulate { t_end, dt, solver } => {
                format!("sim --solver {solver} --t-end {t_end} --dt {dt}")
            }
        }
    }

    /// The run's stop time, for validating that a probe lands inside it.
    fn stop_time(&self) -> Option<f64> {
        match self {
            Self::Compile { target: _ } => None,
            Self::Simulate {
                t_end,
                dt: _,
                solver: _,
            } => Some(*t_end),
        }
    }
}

/// What the row is pinned to do. Both directions are enforced: a row pinned to
/// be refused that starts succeeding is red too, because an unreviewed change
/// in what the compiler accepts is exactly the drift this manifest exists to
/// make visible.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(tag = "outcome", rename_all = "kebab-case", deny_unknown_fields)]
pub(crate) enum Expectation {
    /// The command exits zero. For a simulate check, every pinned observation
    /// must also be reproduced within its tolerance.
    Succeeds {
        observations: Vec<PinnedObservation>,
    },
    /// The command exits nonzero and reports this diagnostic code. Refusal is
    /// free under the top-level theorem, but a *different* refusal is a
    /// behavior change and gets reviewed like any other.
    Refused { diagnostic: String },
}

/// One pinned reading: a variable, an instant, and the value the corpus is
/// pinned to produce there.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct PinnedObservation {
    pub(crate) variable: String,
    pub(crate) time: f64,
    pub(crate) value: f64,
    /// Absolute tolerance. Sized per row: initialization readings agree
    /// bit-for-bit across solvers, a reading after one step does not.
    pub(crate) tolerance: f64,
}

pub(crate) fn manifest_path(root: &Path) -> PathBuf {
    root.join(MANIFEST_PATH)
}

/// Read and validate the manifest.
pub(crate) fn load(path: &Path) -> Result<CorpusManifest> {
    let raw = fs::read_to_string(path)
        .with_context(|| format!("failed to read corpus manifest {}", path.display()))?;
    let manifest: CorpusManifest = serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse corpus manifest {}", path.display()))?;
    validate(&manifest)
        .with_context(|| format!("corpus manifest {} is not admissible", path.display()))?;
    Ok(manifest)
}

pub(crate) fn validate(manifest: &CorpusManifest) -> Result<()> {
    ensure!(
        manifest.schema_version == MANIFEST_SCHEMA_VERSION,
        "corpus manifest schema_version {} is not the supported version {}",
        manifest.schema_version,
        MANIFEST_SCHEMA_VERSION
    );
    ensure!(
        manifest.runtime_budget_seconds > 0.0,
        "runtime_budget_seconds must be positive"
    );
    ensure!(
        !manifest.entries.is_empty(),
        "a corpus manifest with no entries would certify nothing"
    );
    let mut seen = BTreeSet::new();
    for entry in &manifest.entries {
        ensure!(
            seen.insert(entry.id.as_str()),
            "duplicate corpus entry id `{}`",
            entry.id
        );
        validate_entry(entry).with_context(|| format!("corpus entry `{}`", entry.id))?;
    }
    Ok(())
}

fn validate_entry(entry: &CorpusEntry) -> Result<()> {
    ensure!(!entry.id.trim().is_empty(), "id must not be empty");
    ensure!(!entry.model.trim().is_empty(), "model must not be empty");
    ensure!(
        !entry.entry_point.trim().is_empty(),
        "entry_point must not be empty"
    );
    ensure!(
        !entry.why.trim().is_empty(),
        "why must say what this row is for"
    );
    ensure!(
        !Path::new(&entry.entry_point).is_absolute(),
        "entry_point `{}` must be relative to the corpus root",
        entry.entry_point
    );
    validate_check(&entry.check)?;
    validate_expectation(&entry.expect, &entry.check)
}

fn validate_check(check: &Check) -> Result<()> {
    match check {
        Check::Compile { target } => {
            ensure!(
                !target.trim().is_empty(),
                "compile target must not be empty"
            );
            Ok(())
        }
        Check::Simulate { t_end, dt, solver } => {
            ensure!(
                t_end.is_finite() && *t_end > 0.0,
                "t_end must be finite and positive, found {t_end}"
            );
            ensure!(
                dt.is_finite() && *dt > 0.0 && dt <= t_end,
                "dt must be finite, positive, and no larger than t_end, found {dt}"
            );
            ensure!(
                KNOWN_SOLVERS.contains(&solver.as_str()),
                "unknown solver `{solver}`; valid solvers are {}",
                KNOWN_SOLVERS.join(", ")
            );
            Ok(())
        }
    }
}

fn validate_expectation(expect: &Expectation, check: &Check) -> Result<()> {
    match expect {
        Expectation::Succeeds { observations } => {
            let Some(stop_time) = check.stop_time() else {
                ensure!(
                    observations.is_empty(),
                    "a compile check produces no trace, so it cannot carry pinned observations"
                );
                return Ok(());
            };
            // A simulate row that pins no reading is admissible but not green:
            // the gate reports it as unobserved at run time. Keeping that out of
            // load-time validation is what lets `--record` bootstrap a new row
            // from a manifest that does not yet know the numbers.
            for observation in observations {
                validate_observation(observation, stop_time)?;
            }
            Ok(())
        }
        Expectation::Refused { diagnostic } => {
            ensure!(
                !diagnostic.trim().is_empty(),
                "a refusal must pin the diagnostic code it is refused with"
            );
            Ok(())
        }
    }
}

fn validate_observation(observation: &PinnedObservation, stop_time: f64) -> Result<()> {
    let PinnedObservation {
        variable,
        time,
        value,
        tolerance,
    } = observation;
    ensure!(!variable.trim().is_empty(), "observation needs a variable");
    ensure!(
        time.is_finite() && *time >= 0.0 && *time <= stop_time,
        "observation of `{variable}` probes t={time}, outside the run [0, {stop_time}]"
    );
    ensure!(
        value.is_finite(),
        "observation of `{variable}` pins a non-finite value"
    );
    if !(tolerance.is_finite() && *tolerance > 0.0) {
        bail!("observation of `{variable}` needs a finite positive tolerance, found {tolerance}");
    }
    Ok(())
}
