use super::*;

fn simulated(band: BandLabel, channels: usize) -> ModelState {
    ModelState {
        stage: ModelStage::Simulated,
        outcome: None,
        trace: TraceState::Scored(band),
        compared_channels: channels,
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
        certified: false,
        roster: false,
        stage_seconds: StageSeconds::default(),
    }
}

fn unscored(cause: UnscoredCause) -> ModelState {
    ModelState {
        trace: TraceState::Unscored(cause),
        compared_channels: 0,
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
