//! Proposing pins from a measured run.
//!
//! `--record` never touches the checked-in manifest. It writes a *proposal*
//! next to the run's other artifacts, for a human to diff and copy across. That
//! is the whole safeguard: a gate that could rewrite its own expectations would
//! turn every regression into a green run and a quiet manifest edit.
//!
//! The second safeguard is that the recorder proposes only pins the gate would
//! call evidence. It picks variables through the same predicate
//! [`super::observability`] judges them with, and when nothing in the run
//! qualifies it proposes no reading at all and says so, rather than pinning a
//! variable that never moves. A proposal that could be pasted in and go green
//! while certifying nothing would defeat the diff it exists to feed.

use anyhow::Result;
use std::fs;
use std::path::Path;

use super::execution::{ModelRun, first_diagnostic_code};
use super::manifest::{
    Check, CorpusEntry, CorpusManifest, Expectation, ExpectedArtifact, PinnedObservation,
};
use super::observability;
use super::trace::Trace;

/// How many variables a proposal pins for a model that has no pins yet.
const PROPOSED_OBSERVATION_VARIABLES: usize = 3;

/// Absolute tolerance for an initialization reading. Initialization is solved,
/// not integrated, so both solver sessions land on the same bits.
const INITIAL_TOLERANCE: f64 = 1.0e-9;

/// Tolerance for a reading after the run's steps, as an absolute floor plus a
/// relative part. Sized from the measured spread between the explicit and the
/// implicit session over one step of the pinned `t_end`, which is around four
/// parts in ten million.
const STEPPED_ABSOLUTE_TOLERANCE: f64 = 1.0e-6;
const STEPPED_RELATIVE_TOLERANCE: f64 = 1.0e-5;

/// The placeholder recorded when a run failed without naming a code. It is not
/// a valid diagnostic code, so a proposal carrying one cannot be pasted into
/// the manifest and quietly pass validation.
const UNNAMED_REFUSAL: &str = "UNNAMED-REFUSAL";

/// One row's proposal, plus whatever the recorder could not propose.
pub(crate) struct Proposal {
    pub(crate) entry: CorpusEntry,
    /// Left for the operator to act on. A row with a note is a row the
    /// proposal deliberately did not fill in.
    pub(crate) note: Option<String>,
}

pub(crate) fn propose_entry(entry: &CorpusEntry, run: &ModelRun) -> Proposal {
    let mut proposed = entry.clone();
    if run.timed_out {
        return Proposal {
            entry: proposed,
            note: Some(
                "the run was killed at its deadline, so it measured nothing; this row's pin \
                 is left exactly as it stands"
                    .to_string(),
            ),
        };
    }
    if !run.succeeded {
        proposed.expect = Expectation::Refused {
            diagnostic: first_diagnostic_code(&run.output)
                .unwrap_or_else(|| UNNAMED_REFUSAL.to_string()),
        };
        return Proposal {
            entry: proposed,
            note: None,
        };
    }
    let (expect, note) = propose_success(entry, run);
    proposed.expect = expect;
    Proposal {
        entry: proposed,
        note,
    }
}

fn propose_success(entry: &CorpusEntry, run: &ModelRun) -> (Expectation, Option<String>) {
    // Artifact expectations are reviewed choices about what a target must
    // emit, so a re-record carries them across untouched. Discovering them
    // from the output tree would let a target that stopped emitting a file
    // propose its own smaller expectation.
    let artifacts = reviewed_artifacts(entry);
    match &entry.check {
        Check::Compile { target: _ } => {
            let note = artifacts.is_empty().then(|| {
                "the row declares no output artifact, so it would prove only that the process \
                 exited zero; list the files this target must emit under `expect.artifacts`"
                    .to_string()
            });
            (
                Expectation::Succeeds {
                    observations: Vec::new(),
                    artifacts,
                },
                note,
            )
        }
        Check::Simulate {
            t_end,
            dt: _,
            solver: _,
        } => {
            let (observations, note) = propose_observations(entry, run, *t_end);
            (
                Expectation::Succeeds {
                    observations,
                    artifacts: Vec::new(),
                },
                note,
            )
        }
    }
}

fn reviewed_artifacts(entry: &CorpusEntry) -> Vec<ExpectedArtifact> {
    match &entry.expect {
        Expectation::Succeeds {
            observations: _,
            artifacts,
        } => artifacts.clone(),
        Expectation::Refused { diagnostic: _ } => Vec::new(),
    }
}

fn propose_observations(
    entry: &CorpusEntry,
    run: &ModelRun,
    t_end: f64,
) -> (Vec<PinnedObservation>, Option<String>) {
    let Ok(trace) = run.trace() else {
        return (
            Vec::new(),
            Some(
                "the run exited zero but wrote no readable trace, so there was nothing to \
                 record"
                    .to_string(),
            ),
        );
    };
    let proposed = match &entry.expect {
        Expectation::Succeeds {
            observations,
            artifacts: _,
        } if !observations.is_empty() => remeasure(&trace, observations),
        Expectation::Succeeds {
            observations: _,
            artifacts: _,
        }
        | Expectation::Refused { diagnostic: _ } => propose_fresh(&trace, t_end),
    };
    adjudicate(proposed, &trace, t_end)
}

/// Refuse to propose a row whose pins would observe nothing.
///
/// This is the gate's own admissibility test, applied to the proposal before it
/// is written: whatever the two proposal paths produced, a set of pins the
/// checker would call unobserved is dropped and reported instead of written.
fn adjudicate(
    observations: Vec<PinnedObservation>,
    trace: &Trace,
    t_end: f64,
) -> (Vec<PinnedObservation>, Option<String>) {
    if observability::assess(&observations, trace, t_end).is_none() {
        return (observations, None);
    }
    (
        Vec::new(),
        Some(format!(
            "nothing this run reports moves over [0, {t_end}] by more than the tolerance its \
             pin would carry, so every candidate pin would be vacuous and none was written. \
             Choose observables this model moves, or raise t_end past the first thing that \
             happens in it, and record again."
        )),
    )
}

/// Keep the reviewed variable/instant/tolerance choices and refresh only the
/// values, so re-recording after an adjudicated change is a numbers-only diff.
fn remeasure(trace: &Trace, observations: &[PinnedObservation]) -> Vec<PinnedObservation> {
    observations
        .iter()
        .map(|observation| {
            let mut refreshed = observation.clone();
            if let Ok(measured) = trace.value_at(&observation.variable, observation.time) {
                refreshed.value = measured;
            }
            refreshed
        })
        .collect()
}

/// Pick variables the run moves further than their own pin's tolerance, probed
/// at initialization and at the stop time.
///
/// A variable that holds its start value through the whole run pins nothing
/// about integration: every probe on it reads the initial value, so a run
/// truncated to one sample reproduces the pin exactly. Such a variable is
/// passed over rather than kept as a fallback, which is why a run that moves
/// nothing yields no proposal at all.
fn propose_fresh(trace: &Trace, t_end: f64) -> Vec<PinnedObservation> {
    let stop = trace.stop_time().min(t_end);
    trace
        .variable_names()
        .iter()
        .filter_map(|name| readings(trace, name, stop))
        .filter(|(name, _, final_value)| {
            observability::moves(trace, name, stop, stepped_tolerance(*final_value))
        })
        .take(PROPOSED_OBSERVATION_VARIABLES)
        .flat_map(|(name, initial, final_value)| {
            [
                PinnedObservation {
                    variable: name.clone(),
                    time: 0.0,
                    value: initial,
                    tolerance: INITIAL_TOLERANCE,
                },
                PinnedObservation {
                    variable: name,
                    time: stop,
                    value: final_value,
                    tolerance: stepped_tolerance(final_value),
                },
            ]
        })
        .collect()
}

fn readings(trace: &Trace, name: &str, stop: f64) -> Option<(String, f64, f64)> {
    let (Ok(initial), Ok(final_value)) = (trace.value_at(name, 0.0), trace.value_at(name, stop))
    else {
        return None;
    };
    (initial.is_finite() && final_value.is_finite())
        .then(|| (name.to_string(), initial, final_value))
}

fn stepped_tolerance(value: f64) -> f64 {
    STEPPED_ABSOLUTE_TOLERANCE + STEPPED_RELATIVE_TOLERANCE * value.abs()
}

pub(crate) fn write_proposal(path: &Path, manifest: &CorpusManifest) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(
        path,
        format!("{}\n", serde_json::to_string_pretty(manifest)?),
    )?;
    Ok(())
}
