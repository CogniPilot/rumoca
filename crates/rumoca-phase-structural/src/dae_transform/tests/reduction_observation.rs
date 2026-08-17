//! Permanent focused regressions for the observation-only reduction seam.
//!
//! Both fixtures already exist for other reasons — the accumulated-holonomic
//! and accumulated-direct-demotion batteries — and are reused here rather
//! than duplicated, so a wave landing that changes an unrelated part of the
//! reducer cannot desynchronize two copies of the same model. Assertions
//! avoid exact residue/round counts and name matching for the same reason:
//! they check the shape the design proof guarantees (non-increasing residue,
//! a strict decrease somewhere, a terminal `Sorted`), not incidental current
//! numbers.

use super::equalities::independent_holonomic_model;
use super::*;

fn selected_residues(report: &ReductionReport, lane: ReductionLane) -> Vec<(usize, Option<usize>)> {
    report
        .records
        .iter()
        .filter_map(|record| match record {
            ReductionRecord::Selected {
                lane: record_lane,
                residue_before,
                residue_after,
                ..
            } if *record_lane == lane => Some((*residue_before, *residue_after)),
            _ => None,
        })
        .collect()
}

fn assert_monotonic_chain_to_sorted(residues: &[(usize, Option<usize>)]) {
    assert!(!residues.is_empty(), "at least one round must be accepted");
    let mut saw_strict_decrease = false;
    for window in residues.windows(2) {
        let (_, previous_after) = window[0];
        let (next_before, _) = window[1];
        assert_eq!(
            previous_after,
            Some(next_before),
            "one round's residue must hand off exactly to the next round's"
        );
    }
    for &(before, after) in residues {
        match after {
            Some(after) => {
                assert!(
                    after <= before,
                    "an accepted round must never raise the residue"
                );
                saw_strict_decrease |= after < before;
            }
            None => saw_strict_decrease = true,
        }
    }
    assert!(
        saw_strict_decrease,
        "the accumulation must strictly shrink the residue somewhere"
    );
    let (_, last_after) = *residues.last().expect("checked non-empty above");
    assert_eq!(
        last_after, None,
        "the last accepted round in a lane that alone resolves the system must fully match"
    );
}

#[test]
fn accepted_rounds_shrink_the_residue_monotonically() {
    let holonomic_model = independent_holonomic_model();
    let (result, report) = inspect_prepare_for_solve(&holonomic_model);
    assert!(
        result.is_ok(),
        "the two independent holonomic constraints must still accumulate to a regular system"
    );
    let holonomic_residues = selected_residues(&report, ReductionLane::Holonomic);
    assert_monotonic_chain_to_sorted(&holonomic_residues);
    assert!(
        holonomic_residues.len() >= 2,
        "each independent constraint contributes its own accepted holonomic round"
    );
    assert!(
        report.records.iter().any(|record| matches!(
            record,
            ReductionRecord::Candidates {
                lane: ReductionLane::Holonomic,
                group: ReductionCandidateGroup::Holonomic,
                discovered,
            } if *discovered >= 1
        )),
        "an accepted holonomic round must have discovered at least one candidate"
    );
    assert!(
        matches!(
            report.records.last(),
            Some(ReductionRecord::Stopped {
                outcome: ReductionStop::Sorted
            })
        ),
        "the report's last event must be the terminal disposition"
    );

    let direct_model = independent_constraint_model(false);
    let (result, report) = inspect_prepare_for_solve(&direct_model);
    assert!(
        result.is_ok(),
        "the two independently direct-defined states must still accumulate to a regular system"
    );
    let direct_residues = selected_residues(&report, ReductionLane::Direct);
    assert_monotonic_chain_to_sorted(&direct_residues);
    assert!(
        direct_residues.len() >= 2,
        "each independently direct-defined state contributes its own accepted round"
    );
    assert!(
        matches!(
            report.records.last(),
            Some(ReductionRecord::Stopped {
                outcome: ReductionStop::Sorted
            })
        ),
        "the report's last event must be the terminal disposition"
    );
}

fn assert_prepared_dae_matches(plain: &PreparedDae<'_>, traced: &PreparedDae<'_>) {
    let PreparedDae::Transformed {
        dae: plain_dae,
        manifold: plain_manifold,
        pins: plain_pins,
    } = plain
    else {
        panic!("fixture requires index reduction");
    };
    let PreparedDae::Transformed {
        dae: traced_dae,
        manifold: traced_manifold,
        pins: traced_pins,
    } = traced
    else {
        panic!("fixture requires index reduction");
    };
    assert_eq!(
        serde_json::to_vec(&**plain_dae).expect("plain replacement serializes"),
        serde_json::to_vec(&**traced_dae).expect("traced replacement serializes"),
        "recording an observer must not change the prepared DAE"
    );
    assert_eq!(
        plain_manifold, traced_manifold,
        "recording an observer must not change the manifold"
    );
    assert_eq!(plain_pins.len(), traced_pins.len());
    for (plain, traced) in plain_pins.iter().zip(traced_pins.iter()) {
        assert_eq!(plain.coordinate, traced.coordinate);
        assert_eq!(plain.value, traced.value);
        assert_eq!(plain.role, traced.role);
        assert_eq!(plain.source, traced.source);
        assert_eq!(plain.provenance, traced.provenance);
    }
}

#[test]
fn recording_observer_is_inert() {
    let holonomic_model = independent_holonomic_model();
    let plain = prepare_for_solve(&holonomic_model).expect("the plain seam reduces the fixture");
    let (traced, _report) = inspect_prepare_for_solve(&holonomic_model);
    let traced = traced.expect("the traced seam reduces the same fixture the same way");
    assert_prepared_dae_matches(&plain, &traced);

    let direct_model = independent_constraint_model(false);
    let plain = prepare_for_solve(&direct_model).expect("the plain seam reduces the fixture");
    let (traced, _report) = inspect_prepare_for_solve(&direct_model);
    let traced = traced.expect("the traced seam reduces the same fixture the same way");
    assert_prepared_dae_matches(&plain, &traced);
}

#[test]
fn regular_inspection_records_a_borrowed_terminal() {
    let source = independent_constraint_model(false);
    let prepared = prepare_for_solve(&source).expect("fixture reduces to a regular system");
    let (result, report) = inspect_prepare_for_solve(prepared.as_dae());
    assert!(matches!(result, Ok(PreparedDae::Borrowed { .. })));
    assert!(matches!(
        report.records.last(),
        Some(ReductionRecord::Stopped {
            outcome: ReductionStop::Borrowed
        })
    ));
}

#[test]
fn source_backed_reducer_records_held_and_pristine_retry_end_to_end() {
    let source = constrained_state_model(
        false,
        FixtureFeatures {
            holonomic: true,
            ..FixtureFeatures::default()
        },
    )
    .0;
    let (result, report) = inspect_prepare_for_solve(&source);
    assert!(matches!(result, Err(StructuralError::Singular { .. })));

    let direct_held = report
        .records
        .iter()
        .position(|record| {
            matches!(
                record,
                ReductionRecord::Attempt {
                    lane: ReductionLane::Direct,
                    outcome: ReductionOutcome::Held { .. },
                    ..
                }
            )
        })
        .expect("the source-shaped direct demotion must hold its residue");
    let retry = report
        .records
        .iter()
        .position(|record| {
            matches!(
                record,
                ReductionRecord::RetriedPristine {
                    lane: ReductionLane::Holonomic
                }
            )
        })
        .expect("a stalled demoted holonomic pass must retry the pristine DAE");
    let pristine_held = report
        .records
        .iter()
        .enumerate()
        .skip(retry + 1)
        .find_map(|(index, record)| {
            matches!(
                record,
                ReductionRecord::Attempt {
                    lane: ReductionLane::Holonomic,
                    outcome: ReductionOutcome::Held { .. },
                    ..
                }
            )
            .then_some(index)
        })
        .expect("the pristine holonomic candidate must also hold its residue");
    assert!(direct_held < retry && retry < pristine_held);
    assert!(matches!(
        report.records.last(),
        Some(ReductionRecord::Stopped {
            outcome: ReductionStop::Singular { .. }
        })
    ));
}

#[test]
fn rare_observation_outcomes_keep_their_exact_owned_labels() {
    assert!(matches!(
        ReductionOutcome::from(refused_holonomic_outcome(4, 4)),
        ReductionOutcome::Held { residue: 4 }
    ));
    assert!(matches!(
        ReductionOutcome::from(refused_holonomic_outcome(6, 4)),
        ReductionOutcome::Raised { residue: 6 }
    ));

    let mut recorder = ReductionRecorder::default();
    recorder.observe(ReductionEvent::RetriedPristine {
        lane: Lane::Holonomic,
    });
    let error = StructuralError::EmptySystem;
    recorder.observe(ReductionEvent::Stopped {
        outcome: StoppedOutcome::Failure { error: &error },
    });
    assert!(matches!(
        recorder.finish().records.as_slice(),
        [
            ReductionRecord::RetriedPristine {
                lane: ReductionLane::Holonomic
            },
            ReductionRecord::Stopped {
                outcome: ReductionStop::Failure {
                    error: StructuralError::EmptySystem
                }
            }
        ]
    ));
}
