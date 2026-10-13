use super::*;
use rumoca_ir_solve::{
    ContinuousRefreshOwners, ContinuousStructuralArtifacts, RetainedAssertionRoot,
    SolvePureCallTable,
};
use serde::de::IntoDeserializer;

fn source(count: usize) -> String {
    let original = "Real y = RecordGate.Correct(position,referencePosition,requested,accepted);";
    assert_eq!(SOURCE.matches(original).count(), 1);
    SOURCE.replace(original, &format!(
        "Real y[{count}] = fill(RecordGate.Correct(position,referencePosition,requested,accepted),{count});"
    ))
}

fn assert_compact_guarded_source(root: &RetainedAssertionRoot, count: usize) {
    let model = root.model();
    let problem = &model.problem;
    assert_eq!(problem.events.actions.len(), 4);
    assert!(problem.events.root_conditions.is_empty());
    assert!(
        problem
            .continuous
            .refresh_owners
            .has_compact_source_recipe()
    );
    assert!(
        problem
            .continuous
            .refresh_owners
            .algebraic()
            .rows
            .is_empty()
    );
    assert!(
        problem
            .continuous
            .refresh_owners
            .algebraic()
            .static_causal_seed_rows
            .is_empty()
    );
    assert!(
        problem
            .continuous
            .refresh_owners
            .algebraic()
            .dynamic_causal_seed_rows
            .is_empty()
    );
    let assignment = model
        .artifacts
        .continuous
        .structural
        .checked_assignment()
        .unwrap();
    assignment.check_source(problem).unwrap();
    assert_eq!(assignment.projection().len(), count);
    assert_eq!(assignment.projection().stages().len(), 1);
    assert!(model.artifacts.continuous.implicit_jacobian_v.is_empty());
    assert!(
        model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .is_empty()
    );
    let preparation = root.native_preparation().unwrap();
    assert_eq!(preparation.schedule().stages().len(), 1);
    assert_eq!(preparation.schedule().stages()[0].target_count(), count);
}

#[test]
fn assigned_record_source_pair_precedes_compact_refresh_and_artifact_preparation() {
    let _lock = session_test_guard();
    for count in [8, 65_536] {
        with_source_component(&source(count), "RecordGatedFlow", |component| {
            let root = component.runtime_view().retain_assertion_root();
            assert_compact_guarded_source(&root, count);
            Ok(String::new())
        })
        .unwrap();
    }
}

#[test]
fn assigned_record_source_pair_keeps_strict_no_call_and_foreign_table_refusals() {
    let _lock = session_test_guard();
    with_source_component(&source(8), "RecordGatedFlow", |component| {
        let root = component.runtime_view().retain_assertion_root();
        assert_compact_guarded_source(&root, 8);
        let model = root.model();
        let mut strict = model.problem.clone();
        rumoca_ir_solve::issue_native_assignment_schedule(&mut strict);
        assert_eq!(
            strict.continuous.refresh_owners.native_assignment_refusal(),
            Some(&rumoca_ir_solve::NativeScheduleRefusal::Evaluation(
                rumoca_ir_solve::NativeEvaluationRefusal::EventActions
            ))
        );
        assert!(
            ContinuousStructuralArtifacts::checked_assignment_for_problem(&strict)
                .unwrap()
                .is_none()
        );

        let no_calls = SolvePureCallTable::default();
        let refused = ContinuousRefreshOwners::try_compact_with_calls(&model.problem, &no_calls)
            .unwrap()
            .unwrap();
        let reason = refused.native_assignment_refusal().unwrap();
        assert!(matches!(
            reason,
            rumoca_ir_solve::NativeScheduleRefusal::Construction(_)
        ));
        assert_eq!(
            reason.to_string(),
            "native source calls do not belong to the actual table"
        );
        let replayed_calls: SolvePureCallTable =
            serde_json::from_value(serde_json::to_value(&model.pure_calls).unwrap()).unwrap();
        assert_eq!(replayed_calls, model.pure_calls);
        assert!(!replayed_calls.shares_table_owner(&model.pure_calls));
        assert_eq!(
            ContinuousStructuralArtifacts::checked_assignment_with_calls(
                &model.problem,
                &replayed_calls
            )
            .unwrap_err(),
            "assignment artifacts belong to another model call table"
        );
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn assigned_record_canonical_replay_issues_fresh_source_pair_before_artifacts() {
    let _lock = session_test_guard();
    with_source_component(&source(8), "RecordGatedFlow", |component| {
        let root = component.runtime_view().retain_assertion_root();
        assert_compact_guarded_source(&root, 8);
        let preparation = root.native_preparation().unwrap();
        let wire =
            serde_json::to_value(rumoca_sim::retained_fmi_component_wire(&root).unwrap()).unwrap();
        let replay =
            rumoca_sim::deserialize_fmi_component(wire.clone().into_deserializer()).unwrap();
        let replayed = replay.runtime_view().retain_assertion_root();
        assert_compact_guarded_source(&replayed, 8);
        assert!(!replayed.shares_root_owner(&root));
        assert!(
            !replayed
                .model()
                .pure_calls
                .shares_table_owner(&root.model().pure_calls)
        );
        assert!(matches!(
            preparation.bind_resolved(replayed.resolved_periodic_schedules_at(2.0).unwrap()),
            Err(rumoca_ir_solve::RetainedNativeScheduleError::ForeignPreparation)
        ));

        let mut missing_table = wire;
        missing_table["solve_model"]["pure_calls"] =
            serde_json::to_value(SolvePureCallTable::default()).unwrap();
        let failure = rumoca_sim::deserialize_fmi_component(missing_table.into_deserializer());
        let Err(error) = failure else {
            panic!("a call-bearing model cannot replay without its table")
        };
        // SOLVE-C51: a site resolves only against the table read before it, so a site
        // naming an owner of the removed table is refused while the wire is read.
        assert_eq!(error.to_string(), "pure-call site names unknown owner 0");
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn call_membership_does_not_issue_a_malformed_final_model_or_root() {
    let _lock = session_test_guard();
    with_source_component(&source(8), "RecordGatedFlow", |component| {
        let root = component.runtime_view().retain_assertion_root();
        assert_compact_guarded_source(&root, 8);
        let mut malformed = root.model().clone();
        assert!(
            malformed
                .problem
                .solve_layout
                .variable_declarations
                .pop()
                .is_some()
        );
        rumoca_ir_solve::validate_problem_call_references(
            &malformed.problem,
            &malformed.pure_calls,
        )
        .unwrap();
        let expected = rumoca_ir_solve::SolveProblemShapeContractError::DiscreteCertificate {
            context: "solve_layout.variable_storage_runs",
            row: 0,
            detail: "storage and declaration catalogs have different lengths",
            span: None,
        };
        assert_eq!(malformed.validate().unwrap_err(), expected);
        assert_eq!(
            rumoca_ir_solve::validate_problem_pure_call_sites(
                &malformed.problem,
                &malformed.pure_calls,
            )
            .unwrap_err(),
            expected
        );
        let result = rumoca_ir_solve::fmi::FmiComponent::construct(
            malformed,
            root.fmi_declaration_inputs().collect(),
        );
        let Err(error) = result else {
            panic!("membership alone cannot issue a finalized FMI root")
        };
        assert_eq!(
            error,
            rumoca_ir_solve::fmi::FmiComponentError::InvalidSolve(expected.to_string())
        );
        assert_compact_guarded_source(&root, 8);
        Ok(String::new())
    })
    .unwrap();
}
