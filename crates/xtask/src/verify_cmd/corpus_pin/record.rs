//! Proposing pins from a measured run.
//!
//! `--record` never touches the checked-in manifest. It writes a *proposal*
//! next to the run's other artifacts, for a human to diff and copy across. That
//! is the whole safeguard: a gate that could rewrite its own expectations would
//! turn every regression into a green run and a quiet manifest edit.

use anyhow::Result;
use std::fs;
use std::path::Path;

use super::execution::{ModelRun, first_diagnostic_code};
use super::manifest::{Check, CorpusEntry, CorpusManifest, Expectation, PinnedObservation};
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

pub(crate) fn propose_entry(entry: &CorpusEntry, run: &ModelRun) -> CorpusEntry {
    let mut proposed = entry.clone();
    proposed.expect = if run.succeeded {
        Expectation::Succeeds {
            observations: propose_observations(entry, run),
        }
    } else {
        Expectation::Refused {
            diagnostic: first_diagnostic_code(&run.output)
                .unwrap_or_else(|| UNNAMED_REFUSAL.to_string()),
        }
    };
    proposed
}

fn propose_observations(entry: &CorpusEntry, run: &ModelRun) -> Vec<PinnedObservation> {
    let Check::Simulate {
        t_end,
        dt: _,
        solver: _,
    } = &entry.check
    else {
        return Vec::new();
    };
    let Ok(trace) = run.trace() else {
        return Vec::new();
    };
    match &entry.expect {
        Expectation::Succeeds { observations } if !observations.is_empty() => {
            remeasure(&trace, observations)
        }
        Expectation::Succeeds { observations: _ } | Expectation::Refused { diagnostic: _ } => {
            propose_fresh(&trace, *t_end)
        }
    }
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

/// Pick variables the run actually moves, probed at initialization and at the
/// stop time. A variable that holds its start value through the whole run pins
/// nothing about integration, so it is passed over when a moving one exists.
fn propose_fresh(trace: &Trace, t_end: f64) -> Vec<PinnedObservation> {
    let stop = trace.stop_time().min(t_end);
    let mut moving = Vec::new();
    let mut steady = Vec::new();
    for name in trace.variable_names() {
        let (Ok(initial), Ok(final_value)) =
            (trace.value_at(name, 0.0), trace.value_at(name, stop))
        else {
            continue;
        };
        if !initial.is_finite() || !final_value.is_finite() {
            continue;
        }
        let bucket = if initial == final_value {
            &mut steady
        } else {
            &mut moving
        };
        bucket.push((name.clone(), initial, final_value));
    }
    moving
        .into_iter()
        .chain(steady)
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
