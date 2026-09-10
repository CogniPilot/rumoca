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
use std::path::{Component, Path, PathBuf};
use std::time::Duration;

use super::execution::artifact_stem;

/// Location of the pinned corpus manifest, relative to the workspace root.
pub(crate) const MANIFEST_PATH: &str = "infra/verification/corpus-pin.json";

/// Bumped whenever a field changes meaning. A manifest written for another
/// version is refused rather than reinterpreted.
///
/// Version 2 added the per-row output-artifact expectations a compile row is
/// judged on, and made `runtime_budget_seconds` load-bearing: it now also
/// derives the per-row deadline in [`CorpusManifest::row_deadline`].
pub(crate) const MANIFEST_SCHEMA_VERSION: u32 = 2;

/// Solver names the gate is willing to ask for. Mirrors `rumoca --solver`; a
/// manifest naming anything else is refused at load time instead of producing a
/// clap error from a subprocess halfway through the run.
const KNOWN_SOLVERS: [&str; 3] = ["auto", "bdf", "rk-like"];

/// How much longer than its share of the budget a single row may run before it
/// is killed as hung.
///
/// The deadline is a hang catcher, never a performance assertion: the sibling
/// RDD2 performance guard records at length why a wall-clock assertion on a
/// shared developer machine measures machine load rather than the compiler. So
/// the multiplier sits far above the spread a loaded machine produces. At the
/// checked-in budget a row's share is about six seconds and its deadline a
/// little over three minutes. The corpus's slowest row is the implicit-session
/// waypoint mission, measured at forty-three seconds on an idle machine and
/// fifty-eight on a busy one, so the deadline keeps better than three times its
/// worst measurement and lands near the budget for the whole corpus, which is
/// the point at which a row has stopped being slow.
const DEADLINE_HEADROOM: f64 = 30.0;

/// Floor under [`CorpusManifest::row_deadline`], so a small manifest still
/// leaves each row room to parse the standard library.
const DEADLINE_FLOOR: Duration = Duration::from_secs(30);

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
    /// addition that doubles the runtime is visible in review, and it is the
    /// one place [`CorpusManifest::row_deadline`] reads from, so the per-row
    /// deadline moves with the reviewed budget instead of with an environment
    /// variable.
    pub(crate) runtime_budget_seconds: f64,
    pub(crate) entries: Vec<CorpusEntry>,
}

impl CorpusManifest {
    /// How long one row may run before it is killed and reported as hung.
    ///
    /// Derived from the reviewed budget: a row's even share of it, times
    /// [`DEADLINE_HEADROOM`], never below [`DEADLINE_FLOOR`]. Computed from the
    /// whole manifest rather than from a `--only` selection, so focusing on one
    /// row does not hand that row the entire corpus budget.
    pub(crate) fn row_deadline(&self) -> Duration {
        let rows = self.entries.len().max(1);
        let share = self.runtime_budget_seconds / rows as f64;
        Duration::try_from_secs_f64(share * DEADLINE_HEADROOM)
            .unwrap_or(DEADLINE_FLOOR)
            .max(DEADLINE_FLOOR)
    }
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
    pub(crate) fn stop_time(&self) -> Option<f64> {
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
    /// must also be reproduced within its tolerance; for a compile check, every
    /// declared artifact must be on disk and hold what its kind requires.
    Succeeds {
        observations: Vec<PinnedObservation>,
        artifacts: Vec<ExpectedArtifact>,
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

/// One file a compile row's target must emit.
///
/// A compile row judged on the exit status alone stays green while its target
/// emits nothing at all, so every compile row states the files it is pinned to
/// produce and the gate reads them back off disk.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub(crate) struct ExpectedArtifact {
    /// Path relative to the row's `-o` output directory.
    pub(crate) path: String,
    pub(crate) kind: ArtifactKind,
    /// Floor on the artifact's size in bytes. `1` states only "non-empty"; a
    /// larger floor is how a row pins that a generated source file is still a
    /// generated source file rather than a stub.
    pub(crate) min_bytes: u64,
}

/// How deeply the gate looks into one declared artifact.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum ArtifactKind {
    /// A regular file, judged on existence and size only.
    File,
    /// The zip form of an eFMI eFMU container. Judged additionally on the
    /// cheapest structural property the container has: it opens as a zip and
    /// carries a non-empty `__content.xml` at the archive root, which is the
    /// marker the packaging step itself uses to recognize one of its own
    /// products.
    EfmuContainer,
}

impl ArtifactKind {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::File => "file",
            Self::EfmuContainer => "eFMU container",
        }
    }
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
        manifest.runtime_budget_seconds.is_finite() && manifest.runtime_budget_seconds > 0.0,
        "runtime_budget_seconds must be finite and positive"
    );
    ensure!(
        !manifest.entries.is_empty(),
        "a corpus manifest with no entries would certify nothing"
    );
    let mut seen = BTreeSet::new();
    let mut stems = BTreeSet::new();
    for entry in &manifest.entries {
        ensure!(
            seen.insert(entry.id.as_str()),
            "duplicate corpus entry id `{}`",
            entry.id
        );
        // Two ids that flatten to one artifact stem would share an output
        // directory and a trace path, so each row could be judged on what the
        // other one wrote. Distinct ids are not enough on their own: the stem
        // maps every character outside `[A-Za-z0-9._-]` to `_`.
        ensure!(
            stems.insert(artifact_stem(&entry.id)),
            "corpus entry id `{}` shares its artifact stem `{}` with another row, so the two \
             would overwrite each other's output",
            entry.id,
            artifact_stem(&entry.id)
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
        !crate::verify_cmd::manifest_path_is_rooted(&entry.entry_point),
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
        Expectation::Succeeds {
            observations,
            artifacts,
        } => validate_success(observations, artifacts, check),
        Expectation::Refused { diagnostic } => {
            ensure!(
                !diagnostic.trim().is_empty(),
                "a refusal must pin the diagnostic code it is refused with"
            );
            Ok(())
        }
    }
}

fn validate_success(
    observations: &[PinnedObservation],
    artifacts: &[ExpectedArtifact],
    check: &Check,
) -> Result<()> {
    for artifact in artifacts {
        validate_artifact(artifact)?;
    }
    let Some(stop_time) = check.stop_time() else {
        // A compile row that declares no artifact is admissible but not green:
        // the gate reports it as unverified at run time, exactly the treatment
        // a simulate row with no pinned reading gets. Keeping both out of
        // load-time validation is what lets a new row be bootstrapped from a
        // manifest that does not yet know what the target emits.
        ensure!(
            observations.is_empty(),
            "a compile check produces no trace, so it cannot carry pinned observations"
        );
        return Ok(());
    };
    ensure!(
        artifacts.is_empty(),
        "a simulate check is judged on its trace, so it cannot carry expected artifacts"
    );
    // A simulate row that pins no reading is admissible but not green: the gate
    // reports it as unobserved at run time. Keeping that out of load-time
    // validation is what lets `--record` bootstrap a new row from a manifest
    // that does not yet know the numbers.
    for observation in observations {
        validate_observation(observation, stop_time)?;
    }
    Ok(())
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

fn validate_artifact(artifact: &ExpectedArtifact) -> Result<()> {
    let ExpectedArtifact {
        path,
        kind: _,
        min_bytes,
    } = artifact;
    ensure!(!path.trim().is_empty(), "an artifact needs a path");
    let relative = Path::new(path);
    ensure!(
        !crate::verify_cmd::manifest_path_is_rooted(path),
        "artifact `{path}` must be relative to the row's output directory"
    );
    ensure!(
        relative
            .components()
            .all(|component| matches!(component, Component::Normal(_))),
        "artifact `{path}` must not escape the row's output directory"
    );
    ensure!(
        *min_bytes >= 1,
        "artifact `{path}` needs a positive min_bytes; an artifact allowed to be empty proves \
         nothing"
    );
    Ok(())
}
