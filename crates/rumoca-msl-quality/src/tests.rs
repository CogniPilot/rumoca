use super::*;

fn simulated(band: BandLabel, channels: usize) -> ModelState {
    ModelState {
        stage: ModelStage::Simulated,
        outcome: None,
        trace: TraceState::Scored(band),
        compared_channels: channels,
        trace_violation_mass: Some(0.0),
        mean_channel_bounded_l1: Some(0.0),
        certified: band == BandLabel::High,
        roster: false,
        stage_seconds: StageSeconds::default(),
    }
}

fn failed(stage: ModelStage, outcome: &str) -> ModelState {
    ModelState {
        stage,
        outcome: Some(outcome.to_string()),
        trace: TraceState::Unscored(UnscoredCause::NotSimulated),
        compared_channels: 0,
        trace_violation_mass: None,
        mean_channel_bounded_l1: None,
        certified: false,
        roster: false,
        stage_seconds: StageSeconds::default(),
    }
}

fn unscored(cause: UnscoredCause) -> ModelState {
    ModelState {
        trace: TraceState::Unscored(cause),
        compared_channels: 0,
        trace_violation_mass: None,
        mean_channel_bounded_l1: None,
        certified: false,
        ..simulated(BandLabel::High, 0)
    }
}

fn one(before: ModelState, after: ModelState) -> Transition {
    one_reviewed(before, after, &ReviewedChanges::default())
}

fn one_reviewed(before: ModelState, after: ModelState, reviewed: &ReviewedChanges) -> Transition {
    let before = ModelStates::from([("M".to_string(), before)]);
    let after = ModelStates::from([("M".to_string(), after)]);
    transitions(&before, &after, reviewed).remove("M").unwrap()
}

#[test]
fn a_model_that_newly_compiles_and_fails_later_is_progress() {
    // CascodeCircuit: it used to fail lowering, and now fails initialization.
    let transition = one(
        failed(ModelStage::FailedAt(CompilePhase::ToDae), "EC004"),
        failed(ModelStage::Compiled, "ic_solver_fail"),
    );
    assert_eq!(transition, Transition::Progressed);
}

#[test]
fn a_stage_drop_is_a_regression_unless_a_watchdog_killed_it() {
    let dropped = one(
        simulated(BandLabel::High, 10),
        failed(ModelStage::Initialized, "sim_solver_fail"),
    );
    assert!(matches!(
        dropped,
        Transition::Regressed(Regression::StageDropped { .. })
    ));
    let mut killed = failed(ModelStage::Initialized, "sim_timeout");
    killed.stage_seconds.timeout_phase = Some("Sim".to_string());
    assert!(matches!(
        one(simulated(BandLabel::High, 10), killed),
        Transition::Inconclusive(_)
    ));
}

#[test]
fn a_lower_band_or_fewer_channels_while_scored_is_a_regression() {
    assert!(matches!(
        one(
            simulated(BandLabel::High, 10),
            simulated(BandLabel::Deviation, 10)
        ),
        Transition::Regressed(Regression::BandDropped { .. })
    ));
    assert!(matches!(
        one(
            simulated(BandLabel::Near, 10),
            simulated(BandLabel::Near, 9)
        ),
        Transition::Regressed(Regression::ChannelsDropped { from: 10, to: 9 })
    ));
    // A compiler rename lands with a reviewed boundary that maps the set.
    let renamed = ReviewedChanges {
        channel_set_changes: BTreeSet::from(["M".to_string()]),
        ..ReviewedChanges::default()
    };
    assert_eq!(
        one_reviewed(
            simulated(BandLabel::Near, 10),
            simulated(BandLabel::Near, 9),
            &renamed
        ),
        Transition::Unchanged
    );
}

#[test]
fn leaving_scoring_is_judged_by_who_caused_it() {
    for cause in [
        UnscoredCause::RumocaTraceMissing,
        UnscoredCause::RumocaTraceError,
        UnscoredCause::TraceNonidentifiable,
        UnscoredCause::PolicyExcluded(TraceExceptionKind::ModelIssue),
    ] {
        assert_eq!(
            one(simulated(BandLabel::High, 10), unscored(cause)),
            Transition::Regressed(Regression::LeftScoring(cause)),
            "{cause:?}"
        );
    }
    assert!(matches!(
        one(
            simulated(BandLabel::High, 10),
            unscored(UnscoredCause::OmcFailed)
        ),
        Transition::Inconclusive(_)
    ));
    let excluded = ReviewedChanges {
        newly_excluded: BTreeSet::from(["M".to_string()]),
        ..ReviewedChanges::default()
    };
    assert_eq!(
        one_reviewed(
            simulated(BandLabel::High, 10),
            unscored(UnscoredCause::PolicyExcluded(
                TraceExceptionKind::ModelIssue
            )),
            &excluded
        ),
        Transition::Reviewed
    );
}

#[test]
fn a_newly_scored_model_is_progress_whatever_its_band() {
    // A previously excluded model that lands in deviation exposes a defect;
    // it is recorded, not counted against the old floor.
    assert_eq!(
        one(
            unscored(UnscoredCause::PolicyExcluded(
                TraceExceptionKind::ModelIssue
            )),
            simulated(BandLabel::Deviation, 85)
        ),
        Transition::Progressed
    );
}

#[test]
fn losing_certification_is_a_regression() {
    let mut held = simulated(BandLabel::High, 10);
    held.certified = false;
    assert_eq!(
        one(simulated(BandLabel::High, 10), held),
        Transition::Regressed(Regression::Uncertified)
    );
}

#[test]
fn a_target_that_disappears_needs_a_reviewed_removal() {
    let before = ModelStates::from([("Gone".to_string(), simulated(BandLabel::High, 3))]);
    let after = ModelStates::from([("New".to_string(), simulated(BandLabel::High, 3))]);
    let found = transitions(&before, &after, &ReviewedChanges::default());
    assert_eq!(
        found["Gone"],
        Transition::Regressed(Regression::LeftTargets)
    );
    assert_eq!(found["New"], Transition::New);
    assert_eq!(verdict(&found), Verdict::Fail);
    let removed = ReviewedChanges {
        removed_targets: BTreeSet::from(["Gone".to_string()]),
        ..ReviewedChanges::default()
    };
    assert_eq!(
        verdict(&transitions(&before, &after, &removed)),
        Verdict::Pass
    );
}

#[test]
fn the_verdict_fails_on_a_regression_and_holds_on_inconclusive() {
    let inconclusive = BTreeMap::from([
        ("A".to_string(), Transition::Progressed),
        ("B".to_string(), Transition::Inconclusive("watchdog kill")),
    ]);
    assert_eq!(verdict(&inconclusive), Verdict::Inconclusive);
    let mut failing = inconclusive.clone();
    failing.insert(
        "C".to_string(),
        Transition::Regressed(Regression::Uncertified),
    );
    assert_eq!(verdict(&failing), Verdict::Fail);
}

#[test]
fn model_states_keep_their_wire_form() {
    let state = simulated(BandLabel::High, 12);
    let json = serde_json::to_value(&state).unwrap();
    assert_eq!(json["stage"], "simulated");
    assert_eq!(
        serde_json::to_value(ModelStage::FailedAt(CompilePhase::ToDae)).unwrap(),
        serde_json::json!({ "failed_at": "to_dae" })
    );
    assert_eq!(json["trace"], serde_json::json!({ "scored": "high" }));
    assert_eq!(serde_json::from_value::<ModelState>(json).unwrap(), state);
    assert_eq!(
        serde_json::to_value(TraceState::Unscored(UnscoredCause::OmcFailed)).unwrap(),
        serde_json::json!({ "unscored": "omc_failed" })
    );
}

#[test]
fn the_stage_rank_follows_the_compile_phase_order() {
    // A parse failure is the earliest failure, not a lowering one.
    assert!(
        ModelStage::FailedAt(CompilePhase::Parse) < ModelStage::FailedAt(CompilePhase::Resolve)
    );
    assert!(ModelStage::FailedAt(CompilePhase::ToDae) < ModelStage::Compiled);
    for (rank, phase) in CompilePhase::ALL.into_iter().enumerate() {
        assert_eq!(compile_phase_rank(phase.as_str()), Some(rank));
        assert_eq!(failed_compile_phase(phase.as_str()), Ok(Some(phase)));
    }
    assert_eq!(
        compile_phase_rank(COMPILE_SUCCESS),
        Some(CompilePhase::ALL.len())
    );
    assert_eq!(failed_compile_phase(COMPILE_SUCCESS), Ok(None));
}

#[test]
fn an_unknown_compile_phase_is_refused() {
    for name in ["", "Lowering", "todae"] {
        assert!(failed_compile_phase(name).is_err(), "{name:?}");
        assert_eq!(compile_phase_rank(name), None, "{name:?}");
    }
}

#[test]
fn an_exclusion_that_changes_kind_needs_a_boundary() {
    let excluded = |kind| unscored(UnscoredCause::PolicyExcluded(kind));
    assert_eq!(
        one(
            excluded(TraceExceptionKind::ReferenceFailure),
            excluded(TraceExceptionKind::ComparatorLimitation)
        ),
        Transition::Regressed(Regression::ExclusionKindChanged {
            from: TraceExceptionKind::ReferenceFailure,
            to: TraceExceptionKind::ComparatorLimitation,
        })
    );
    assert_eq!(
        TraceExceptionKind::parse("comparator_limitation"),
        Some(TraceExceptionKind::ComparatorLimitation)
    );
    assert_eq!(TraceExceptionKind::parse("near_zero"), None);
}

#[test]
fn an_evidence_file_of_another_schema_is_refused() {
    let file = |version: u32| {
        serde_json::json!({
            "schema_version": version,
            "source_run": "ci-1",
            "source_commit": "abc",
            "caveats": [],
            "model_states": { "M": simulated(BandLabel::High, 3) },
        })
        .to_string()
    };
    let evidence = parse_evidence_states(&file(EVIDENCE_STATES_SCHEMA_VERSION)).unwrap();
    assert_eq!(evidence.model_states.len(), 1);
    let error = parse_evidence_states(&file(EVIDENCE_STATES_SCHEMA_VERSION + 1)).unwrap_err();
    assert!(error.contains("is not the supported"), "{error}");
}

#[test]
fn a_regression_reads_as_a_sentence() {
    assert_eq!(
        Regression::BandDropped {
            from: BandLabel::High,
            to: BandLabel::Deviation
        }
        .to_string(),
        "band dropped: high -> deviation"
    );
}

#[test]
fn the_continuous_floors_count_only_models_scored_on_both_sides() {
    let scored = |mass: f64, band| ModelState {
        trace_violation_mass: Some(mass),
        ..simulated(band, 10)
    };
    let before = ModelStates::from([
        ("Kept".to_string(), scored(1.0, BandLabel::High)),
        ("Hidden".to_string(), unscored(UnscoredCause::NotSimulated)),
    ]);
    // A newly scored model with a large mass does not move the floor.
    let after = ModelStates::from([
        ("Kept".to_string(), scored(1.0, BandLabel::High)),
        ("Hidden".to_string(), scored(7.5, BandLabel::Deviation)),
    ]);
    let floors = shared_floors(&before, &after);
    assert_eq!(floors.models, 1);
    assert!(
        floors.regressions().is_empty(),
        "{:?}",
        floors.regressions()
    );
    // An accuracy loss that stays inside the band is caught.
    let worse = ModelStates::from([("Kept".to_string(), scored(2.0, BandLabel::High))]);
    let regressions = shared_floors(&before, &worse).regressions();
    assert_eq!(regressions.len(), 1, "{regressions:?}");
    assert!(regressions[0].starts_with("trace violation mass over 1 models"));
}

#[test]
fn an_accuracy_loss_inside_the_band_shows_in_the_mean_channel_error() {
    let scored = |error: f64| ModelState {
        mean_channel_bounded_l1: Some(error),
        ..simulated(BandLabel::High, 10)
    };
    let before = ModelStates::from([("Tank".to_string(), scored(1.0e-3))]);
    let after = ModelStates::from([("Tank".to_string(), scored(2.3e-3))]);
    let floors = shared_floors(&before, &after);
    // Same band, zero violation mass: only the continuous measure moves.
    assert!(floors.regressions().is_empty());
    assert!(floors.accuracy_change().unwrap().contains("over 1 models"));
    assert_eq!(shared_floors(&before, &before).accuracy_change(), None);
}

#[test]
fn every_model_whose_error_rose_is_listed_even_when_the_sum_falls() {
    let scored = |error: f64| ModelState {
        mean_channel_bounded_l1: Some(error),
        ..simulated(BandLabel::High, 10)
    };
    let before = ModelStates::from([
        ("A".to_string(), scored(1.0e-3)),
        ("B".to_string(), scored(5.0e-3)),
    ]);
    // A gets worse, B gets much better: the sum falls, A must still be named.
    let after = ModelStates::from([
        ("A".to_string(), scored(1.5e-3)),
        ("B".to_string(), scored(1.0e-3)),
    ]);
    let floors = shared_floors(&before, &after);
    assert!(floors.mean_channel_bounded_l1.1 < floors.mean_channel_bounded_l1.0);
    let change = accuracy_change(&before, &after).expect("A rose");
    assert_eq!(
        change.rose,
        vec![ModelAccuracy {
            model: "A".to_string(),
            before: 1.0e-3,
            after: 1.5e-3
        }]
    );
    // A statement must be the measured list exactly.
    let mut stated = change.clone();
    stated.rose[0].after += f64::EPSILON;
    assert_ne!(stated, change);
    let unchanged = ModelStates::from([("A".to_string(), scored(1.0e-3))]);
    assert_eq!(accuracy_change(&unchanged, &unchanged), None);
}
