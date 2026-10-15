use super::*;
use crate::{
    FunctionConditionalProgram, SolveArithmeticProfile, SolveAssertionActionProjection,
    SolveAssertionLevel, SolveEventMessage, SolveIntegerDomain, SolvePureCallIdentity,
    SolvePureCallOutput, SolveRealFormat, SolveValue,
};
use rumoca_core::{SourceId, Span};
use std::num::NonZeroU64;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("retained-assertion.mo"), 1, 2)
}

fn model() -> SolveModel {
    let profile =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let mut builder = SolvePureCallTable::builder(profile);
    builder
        .add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![],
            vec![SolvePureCallOutput::assertion_predicate()],
            span(),
            |body, _, outputs| {
                let condition = body.constant(SolveValue::boolean(false), span())?;
                let assertion = body.assertion_output(0, span())?;
                body.check_assertion(assertion, condition, &[], span(), |_, _, _| Ok(()))?;
                body.store(outputs[0], condition, span())
            },
        )
        .unwrap();
    let table = builder.finish();
    let site = table.owners()[0].call_site();
    let observation = super::super::SolveAssertionObservationSite::new(site.clone()).unwrap();
    let program = vec![
        LinearOp::PureCallObservation {
            dst_start: 0,
            input_starts: vec![].into(),
            site: observation,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let mut model = SolveModel {
        pure_calls: table,
        ..SolveModel::default()
    };
    model.problem.events.action_conditions =
        ScalarProgramBlock::with_program_spans(vec![program], vec![span()]).unwrap();
    model.problem.events.actions = vec![SolveEventAction {
        kind: SolveEventActionKind::Assert,
        message: SolveEventMessage {
            parts: vec![SolveEventMessagePart::Text("original text".into())],
        },
        span: span(),
        origin: "authored assertion".into(),
        clock_owner: None,
        assertion_projection: SolveAssertionActionProjection::new(site, 0),
    }];
    model
}

#[test]
fn retained_inputs_preserve_original_sources_and_private_binding_identity() {
    let component = crate::fmi::FmiComponent::construct(model(), vec![]).unwrap();
    let original = component.runtime_view().model();
    let retained = component.runtime_view().retain_assertion_root();
    assert!(std::ptr::eq(retained.model(), original));
    assert!(std::ptr::eq(retained.metadata(), component.metadata()));
    assert!(matches!(
        retained.checked_invocation(RetainedAssertionSource::ActionConditions, 0, 0),
        Err(RetainedAssertionInputError::UnresolvedStartTime)
    ));
    let root = retained.resolved_periodic_schedules_at(2.0).unwrap();
    assert_eq!(retained.resolved_start_time(), None);
    assert_eq!(root.resolved_start_time(), Some(2.0));
    assert!(root.shares_root_owner(&retained));
    assert!(std::ptr::eq(root.metadata(), retained.metadata()));
    let invocation = root
        .checked_invocation(RetainedAssertionSource::ActionConditions, 0, 0)
        .unwrap();
    assert!(std::ptr::eq(invocation.table(), &original.pure_calls));
    assert_eq!(invocation.operation(), 0);
    assert!(std::ptr::eq(
        invocation.program(),
        original.problem.events.action_conditions.programs()[0].as_slice()
    ));
    assert!(std::ptr::eq(
        invocation.actions(),
        original.problem.events.actions.as_slice()
    ));
    assert!(std::ptr::eq(
        invocation.owner(),
        &original.pure_calls.owners()[0]
    ));
    let projection = invocation.projections().next().unwrap();
    let second_view = invocation.projections().next().unwrap();
    assert!(invocation.owns_projection(&projection));
    assert!(invocation.owns_projection(&second_view));
    let (
        RetainedActionView::Retained { binding: first, .. },
        RetainedActionView::Retained {
            binding: second, ..
        },
    ) = (&projection.view, &second_view.view)
    else {
        panic!("retained projections keep their private retained binding");
    };
    assert!(std::ptr::eq(*first, *second));
    assert!(std::ptr::eq(
        projection.action(),
        &original.problem.events.actions[0]
    ));
    assert_eq!(projection.action_index(), 0);
    assert_eq!(projection.predicate_output(), 0);
    let message: Vec<_> = projection.message().collect();
    let [RetainedAssertionMessagePart::Text(text)] = message.as_slice() else {
        panic!("the original literal message remains borrowed");
    };
    let SolveEventMessagePart::Text(original_text) =
        &original.problem.events.actions[0].message.parts[0]
    else {
        panic!("literal source fixture");
    };
    assert!(std::ptr::eq(*text, original_text.as_str()));
    let separately_checked = root
        .checked_invocation(RetainedAssertionSource::ActionConditions, 0, 0)
        .unwrap();
    assert!(!separately_checked.owns_projection(&projection));
    drop(component);
    assert_eq!(
        invocation.projections().next().unwrap().action().span,
        span()
    );
}

#[test]
fn canonical_start_resolution_issues_a_different_root_only_when_needed() {
    let mut source = model();
    let lattice = rumoca_core::ClockLattice::from_seconds(0.25, 0.25).unwrap();
    let schedule = rumoca_core::PeriodicClockSchedule::simulation_start_relative(lattice).unwrap();
    source.problem.clocks.periodic_event_schedules =
        vec![crate::PeriodicEventSchedule::from_schedule(schedule).unwrap()];
    source.problem.clocks.activation_parameter_indices = vec![0];
    source.problem.layout = crate::VarLayout::from_parts(Default::default(), 0, 1);
    source.parameters = crate::SolveInitialValues::repeat(0.0, 1).unwrap();
    let component = crate::fmi::FmiComponent::construct(source, vec![]).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let resolved = root.resolved_periodic_schedules_at(2.0).unwrap();
    assert_eq!(root.resolved_start_time(), None);
    assert_eq!(resolved.resolved_start_time(), Some(2.0));
    assert!(!resolved.shares_root_owner(&root));
    assert_eq!(
        root.model().problem.clocks.periodic_event_schedules[0].anchor(),
        rumoca_core::ClockPhaseAnchor::SimulationStart
    );
    let schedule = &resolved.model().problem.clocks.periodic_event_schedules[0];
    assert_eq!(schedule.anchor(), rumoca_core::ClockPhaseAnchor::Absolute);
    assert_eq!(schedule.phase_seconds(), 2.25);
    for start_time in [2.0, 5.0] {
        assert_eq!(
            resolved
                .resolved_periodic_schedules_at(start_time)
                .unwrap_err(),
            RetainedAssertionResolutionError::AlreadyResolved
        );
    }
    let next_instance = component
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(5.0)
        .unwrap();
    assert!(!next_instance.shares_root_owner(&resolved));
    assert_eq!(
        next_instance
            .model()
            .problem
            .clocks
            .periodic_event_schedules[0]
            .phase_seconds(),
        5.25
    );
    assert_eq!(
        root.resolved_periodic_schedules_at(f64::NAN).unwrap_err(),
        RetainedAssertionResolutionError::Clock(
            rumoca_core::ClockLatticeErrorKind::NonFiniteSeconds
        )
    );
    let invocation = resolved
        .checked_invocation(RetainedAssertionSource::ActionConditions, 0, 0)
        .unwrap();
    assert!(invocation.root().shares_root_owner(&resolved));
    assert!(!invocation.root().shares_root_owner(&root));
}

#[test]
fn finalized_empty_component_retains_its_constructor_issued_native_refusal() {
    let component =
        crate::fmi::FmiComponent::construct(crate::SolveModel::default(), vec![]).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let expected = root
        .model()
        .problem
        .continuous
        .refresh_owners
        .native_assignment_refusal()
        .expect("finalized component records native admission")
        .clone();
    let crate::NativeScheduleRefusal::Construction(cause) = &expected else {
        panic!("an empty inventory has a construction refusal");
    };
    assert_eq!(cause.to_string(), "native assignment inventory is empty");
    assert!(matches!(root.native_preparation(),
        Err(RetainedNativeScheduleError::Refused(actual)) if actual == expected));
    assert!(matches!(
        root.native_schedule(),
        Err(RetainedNativeScheduleError::UnresolvedStartTime)
    ));
    let root = root.resolved_periodic_schedules_at(0.0).unwrap();
    assert_native_refusal(&root, &expected);
}

fn assert_native_refusal(root: &RetainedAssertionRoot, expected: &crate::NativeScheduleRefusal) {
    assert!(matches!(root.native_preparation(),
        Err(RetainedNativeScheduleError::Refused(actual)) if &actual == expected));
    assert!(matches!(root.native_schedule(),
        Err(RetainedNativeScheduleError::Refused(actual)) if &actual == expected));
}

#[test]
fn finalized_authored_assertion_retains_its_call_aware_native_refusal() {
    let component = crate::fmi::FmiComponent::construct(model(), vec![]).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let expected =
        crate::NativeScheduleRefusal::Evaluation(crate::NativeEvaluationRefusal::EventActions);
    assert_eq!(
        root.model()
            .problem
            .continuous
            .refresh_owners
            .native_assignment_refusal(),
        Some(&expected)
    );
    assert!(matches!(root.native_preparation(),
        Err(RetainedNativeScheduleError::Refused(actual)) if actual == expected));
    assert!(matches!(
        root.native_schedule(),
        Err(RetainedNativeScheduleError::UnresolvedStartTime)
    ));
    let root = root.resolved_periodic_schedules_at(0.0).unwrap();
    assert_native_refusal(&root, &expected);
}

#[test]
fn retained_selection_refuses_wrong_source_operation_and_nested_scalar_region() {
    let component = crate::fmi::FmiComponent::construct(model(), vec![]).unwrap();
    let root = component
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(0.0)
        .unwrap();
    for (source, program, operation, expected) in [
        (
            RetainedAssertionSource::RootConditions,
            0,
            0,
            RetainedAssertionInputError::MissingProgram,
        ),
        (
            RetainedAssertionSource::ActionConditions,
            1,
            0,
            RetainedAssertionInputError::MissingProgram,
        ),
        (
            RetainedAssertionSource::ActionConditions,
            0,
            2,
            RetainedAssertionInputError::MissingOperation,
        ),
        (
            RetainedAssertionSource::ActionConditions,
            0,
            1,
            RetainedAssertionInputError::UncheckedInvocation,
        ),
    ] {
        assert!(
            matches!(root.checked_invocation(source, program, operation), Err(error) if error == expected)
        );
    }
    let mut nested = model();
    let scalar = nested.problem.events.action_conditions.programs()[0].clone();
    let conditional = FunctionConditionalProgram::checked(
        0,
        vec![1],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            scalar.clone(),
        )],
        scalar,
    )
    .unwrap();
    nested.problem.events.action_conditions = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![span()],
    )
    .unwrap();
    let component = crate::fmi::FmiComponent::construct(nested, vec![]).unwrap();
    let root = component
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(0.0)
        .unwrap();
    assert!(matches!(
        root.checked_invocation(RetainedAssertionSource::ActionConditions, 0, 0),
        Err(RetainedAssertionInputError::NestedScalarRegionUnsupported)
    ));
    assert_eq!(
        root.model().pure_calls.owners()[0].outputs()[0].assertion_level(),
        Some(SolveAssertionLevel::Error)
    );
}

#[test]
fn scheduleless_resolution_rejects_nonfinite_anchors_without_issuing_authority() {
    let component = crate::fmi::FmiComponent::construct(model(), vec![]).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    for coordinate in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert_eq!(
            root.resolved_periodic_schedules_at(coordinate).unwrap_err(),
            RetainedAssertionResolutionError::Clock(
                rumoca_core::ClockLatticeErrorKind::NonFiniteSeconds
            )
        );
        assert_eq!(root.resolved_start_time(), None);
    }
    let resolved = root.resolved_periodic_schedules_at(-2.0).unwrap();
    assert_eq!(resolved.resolved_start_time(), Some(-2.0));
    assert_eq!(
        resolved.resolved_periodic_schedules_at(-2.0).unwrap_err(),
        RetainedAssertionResolutionError::AlreadyResolved
    );
    assert_eq!(resolved.resolved_start_time(), Some(-2.0));
}

#[test]
fn checked_assertion_program_reuses_only_its_exact_slice() {
    let model = model();
    let original = &model.problem.events.action_conditions.programs()[0];
    let program = vec![
        original[0].clone(),
        original[0].clone(),
        original[1].clone(),
    ];
    let checked = super::super::CheckedAssertionProgram::new(&program).unwrap();
    for operation in [0, 1] {
        let reused = checked
            .invocation(&model.pure_calls, operation, &model.problem.events.actions)
            .unwrap();
        let standalone = super::super::CheckedAssertionInvocation::new(
            &model.pure_calls,
            &program,
            operation,
            &model.problem.events.actions,
        )
        .unwrap();
        assert!(std::ptr::eq(reused.program(), program.as_slice()));
        assert!(std::ptr::eq(reused.owner(), standalone.owner()));
        assert_eq!(reused.operation(), standalone.operation());
        assert_eq!(reused.projections().len(), standalone.projections().len());
    }
    for operation in [2, program.len(), usize::MAX] {
        assert!(
            checked
                .invocation(&model.pure_calls, operation, &model.problem.events.actions)
                .is_none()
        );
    }
    assert!(checked.invocation(&model.pure_calls, 0, &[]).is_none());
    let mut wrong_actions = model.problem.events.actions.clone();
    wrong_actions[0].kind = SolveEventActionKind::Warning;
    assert!(
        checked
            .invocation(&model.pure_calls, 0, &wrong_actions)
            .is_none()
    );
}

#[test]
fn checked_assertion_program_refuses_undefined_sources_and_malformed_ranges() {
    let undefined = [LinearOp::StoreOutput { src: 0 }];
    assert!(matches!(
        crate::ScalarProgramRegisterFlow::derive(&undefined),
        Err(crate::ScalarProgramRegisterError::UndefinedRegister { .. })
    ));
    assert!(super::super::CheckedAssertionProgram::new(&undefined).is_none());
    let malformed = [
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count: 0,
            lanes: 1,
        },
    ];
    assert!(matches!(
        crate::ScalarProgramRegisterFlow::derive(&malformed),
        Err(crate::ScalarProgramRegisterError::InvalidTensorProjection {
            reason: "tensor fill has an invalid extent or lane count",
            ..
        })
    ));
    assert!(super::super::CheckedAssertionProgram::new(&malformed).is_none());
}
