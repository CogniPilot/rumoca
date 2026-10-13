//! Actual emitted native execution of later valid checks over a guarded record producer.

use super::*;
use rumoca_exec_wasm::NativeModelLibrary;
use rumoca_ir_solve::{SolveValueKind, fmi::FmiDeploymentCapabilities};

mod compact_construction;

const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/RecordGatedFlow.mo"
);

#[test]
fn assigned_record_reads_keep_checked_effects_without_model_crossings() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "RecordGatedFlow", |component| {
        let root = component.runtime_view().retain_assertion_root();
        assert!(std::ptr::eq(root.metadata(), component.metadata()));
        let problem = &root.model().problem;
        assert!(problem.events.root_conditions.is_empty());
        assert!(problem.events.condition_memory_parameter_indices.is_empty());
        assert!(
            problem
                .solve_layout
                .relation_memory_parameter_indices
                .is_empty()
        );
        // The executed checks and their source error actions are still present.
        assert_eq!(problem.events.actions.len(), 4);
        let owner = &root.model().pure_calls.owners()[0];
        assert!(owner.assertion_flow().is_some());
        assert_eq!(
            owner
                .outputs()
                .iter()
                .filter(|output| output.assertion_level().is_some())
                .count(),
            4
        );
        let mut detached_problem = problem.clone();
        rumoca_ir_solve::issue_native_assignment_schedule(&mut detached_problem);
        assert_eq!(
            detached_problem
                .continuous
                .refresh_owners
                .native_assignment_refusal(),
            Some(&rumoca_ir_solve::NativeScheduleRefusal::Evaluation(
                rumoca_ir_solve::NativeEvaluationRefusal::EventActions
            ))
        );
        let preparation = root.native_preparation().unwrap();
        assert!(preparation.root().shares_root_owner(&root));
        assert!(std::ptr::eq(preparation.root().metadata(), root.metadata()));
        assert!(!preparation.schedule().stages().is_empty());
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn authored_function_error_assertion_raises_at_the_publication_boundary() {
    let _lock = session_test_guard();
    let source = r#"
function CheckedPositive
  input Real x;
  output Real y;
algorithm
  assert(x > 0, "x must be positive");
  y := x;
end CheckedPositive;
model AuthoredCallAssertion
  input Real x = 1;
  Real y = CheckedPositive(x);
end AuthoredCallAssertion;
"#;
    with_source_component(source, "AuthoredCallAssertion", |component| {
        let root = component.runtime_view().retain_assertion_root();
        assert!(std::ptr::eq(root.metadata(), component.metadata()));
        // The stateless native product raises a call assertion at the publication boundary:
        // the event action owns the authored message and no relation root exists.
        assert_eq!(
            root.model().problem.events.root_conditions.output_count(),
            0
        );
        assert_eq!(root.model().problem.events.actions.len(), 1);
        // SOLVE-C67: the retained event actions still keep the value schedule refused.
        assert_eq!(
            root.native_preparation().err(),
            Some(rumoca_ir_solve::RetainedNativeScheduleError::Refused(
                rumoca_ir_solve::NativeScheduleRefusal::Evaluation(
                    rumoca_ir_solve::NativeEvaluationRefusal::EventActions
                )
            ))
        );
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn mixed_record_checks_keep_every_event_action_without_a_relation_root() {
    let _lock = session_test_guard();
    let source = SOURCE.replace(
        "y := sum(candidate.referencePosition);",
        "assert(accepted, \"accepted record required\"); y := sum(candidate.referencePosition);",
    );
    with_source_component(&source, "RecordGatedFlow", |component| {
        let root = component.runtime_view().retain_assertion_root();
        assert_eq!(
            root.model().problem.events.root_conditions.output_count(),
            0
        );
        assert_eq!(root.model().problem.events.actions.len(), 5);
        // SOLVE-C67: the retained event actions still keep the value schedule refused.
        assert_eq!(
            root.native_preparation().err(),
            Some(rumoca_ir_solve::RetainedNativeScheduleError::Refused(
                rumoca_ir_solve::NativeScheduleRefusal::Evaluation(
                    rumoca_ir_solve::NativeEvaluationRefusal::EventActions
                )
            ))
        );
        Ok(String::new())
    })
    .unwrap();
}

#[test]
fn later_valid_checks_execute_actual_retained_native_record_wasm() {
    let _lock = session_test_guard();
    with_source_component(SOURCE, "RecordGatedFlow", |component| {
        let original = component.runtime_view().retain_assertion_root();
        assert!(!original.model().pure_calls.owners().is_empty());
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
            original.native_preparation().unwrap(),
        )
        .unwrap();
        let deployment = component
            .into_codegen_view()
            .try_deployment(FmiDeploymentCapabilities::default())
            .unwrap();
        let library = NativeModelLibrary::from_preparation(code.clone(), deployment).unwrap();
        assert!(library.inputs().root().shares_root_owner(&original));
        assert!(std::ptr::eq(library.compiled(), code.compiled()));
        let root = original.resolved_periodic_schedules_at(0.0).unwrap();
        let host = code.bind_resolved(root.clone()).unwrap().into_host();
        let artifact = execution_description(host.product());
        let mut execution = CallExecution::uninitialized(&artifact);
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        let mut transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        for (requested, accepted, values, expected) in [
            (true, true, [1.0_f64, 2.0, 3.0], 12.0_f64),
            (false, true, [1.0, 2.0, 3.0], 0.0),
            (true, false, [1.0, 2.0, 3.0], 0.0),
            (true, true, [2.0, 3.0, 4.0], 15.0),
            (true, true, [1.0, 2.0, 3.0], 12.0),
        ] {
            for (name, value) in [("requested", requested), ("accepted", accepted)] {
                transport
                    .set_typed_input(slot(&artifact, name, "P"), NativeTypedInput::Boolean(value))
                    .unwrap();
            }
            write_position_vectors(&mut transport, &artifact, &values);
            assert_position_vectors(transport.coordinates().1, &artifact, &values);
            let typed = transport
                .input_values()
                .map(|(input, value)| (input.p_index(), value.unwrap()))
                .collect::<Vec<_>>();
            assert_eq!(typed.len(), 2);
            for (name, value) in [
                ("requested", SolveValueKind::Boolean(requested)),
                ("accepted", SolveValueKind::Boolean(accepted)),
            ] {
                assert!(typed.contains(&(slot(&artifact, name, "P"), value)));
            }
            let lease = transport.enter().unwrap();
            let (status, actual) = execute_transport(&mut execution, &lease);
            assert_eq!(status, 0);
            assert_eq!(
                actual[slot(&artifact, "y", "Y")].to_bits(),
                expected.to_bits()
            );
        }
        Ok(String::new())
    })
    .unwrap();
}

fn write_position_vectors(
    transport: &mut NativeTransport,
    artifact: &serde_json::Value,
    values: &[f64; 3],
) {
    for name in ["position", "referencePosition"] {
        for (index, value) in values.iter().enumerate() {
            transport
                .set_float64_input_batches(&[(
                    slot(artifact, &format!("{name}[{}]", index + 1), "P"),
                    std::slice::from_ref(value),
                )])
                .unwrap();
        }
    }
}

fn assert_position_vectors(p: &[f64], artifact: &serde_json::Value, values: &[f64; 3]) {
    for name in ["position", "referencePosition"] {
        for (index, value) in values.iter().enumerate() {
            assert_eq!(
                p[slot(artifact, &format!("{name}[{}]", index + 1), "P")].to_bits(),
                value.to_bits()
            );
        }
    }
}
