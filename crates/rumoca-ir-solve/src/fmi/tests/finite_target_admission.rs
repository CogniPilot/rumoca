//! Unsupported typed operations fail at Solve IR target admission, not print.

use super::*;
use crate::{
    ProgramSlot, SolveFiniteCheck, SolveProgramConstructionError, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallTable, SolveScalarType, SolveValue, SolveValueType,
    TypedProgramBuilder,
};
use std::num::NonZeroU64;

#[derive(Clone, Copy, PartialEq)]
enum Placement {
    None,
    Root,
    Then,
    Else,
}

fn at() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("FiniteTargetAdmission.mo"),
        1,
        2,
    )
}

fn branch<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    guarded: bool,
) -> Result<(), SolveProgramConstructionError> {
    let value = builder.load(inputs[0], at())?;
    if guarded {
        builder.check_finite(value, SolveFiniteCheck::Derivative, at())?;
    }
    builder.store(outputs[0], value, at())
}

fn emit_owner<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    placement: Placement,
    real: SolveValueType,
) -> Result<(), SolveProgramConstructionError> {
    let value = builder.load(inputs[0], at())?;
    let result = match placement {
        Placement::None => value,
        Placement::Root => {
            builder.check_finite(value, SolveFiniteCheck::Derivative, at())?;
            value
        }
        Placement::Then | Placement::Else => {
            let condition = builder.constant(SolveValue::boolean(true), at())?;
            builder.conditional(
                condition,
                &[value],
                vec![real],
                at(),
                |b, i, o| branch(b, i, o, placement == Placement::Then),
                |b, i, o| branch(b, i, o, placement == Placement::Else),
            )?[0]
        }
    };
    builder.store(outputs[0], result, at())
}

fn component(placement: Placement) -> FmiComponent {
    let profile = SolvePureCallTable::default().arithmetic();
    let real = SolveValueType::scalar(SolveScalarType::real(profile));
    let table = SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1180).unwrap()),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            at(),
            |builder, inputs, outputs| {
                emit_owner(builder, inputs, outputs, placement, real.clone())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let model = SolveModel {
        pure_calls: table,
        ..SolveModel::default()
    };
    FmiComponent::construct(model, Vec::new()).expect("complete checked component")
}

fn assert_refused(error: FmiCCodegenError) {
    assert!(error.to_string().contains("CheckFinite"), "{error}");
}

#[test]
fn c_profile_refuses_finite_checks_before_any_c_view_is_issued() {
    assert_refused(
        component(Placement::Root)
            .into_codegen_view()
            .try_c()
            .unwrap_err(),
    );
    let event_free = component(Placement::Root)
        .into_codegen_view()
        .try_event_free()
        .unwrap();
    assert_refused(FmiCCodegenView::try_from(event_free).unwrap_err());
    let deployment = component(Placement::Root)
        .into_codegen_view()
        .try_deployment(FmiDeploymentCapabilities::default())
        .unwrap();
    assert_refused(deployment.try_c().unwrap_err());
}

#[test]
fn c_profile_refuses_both_nested_arms_without_sampling_the_condition() {
    for placement in [Placement::Then, Placement::Else] {
        assert_refused(
            component(placement)
                .into_codegen_view()
                .try_c()
                .unwrap_err(),
        );
    }
}

#[test]
fn c_profile_keeps_ordinary_unguarded_typed_owners_admitted() {
    component(Placement::None)
        .into_codegen_view()
        .try_c()
        .unwrap();
    let event_free = component(Placement::None)
        .into_codegen_view()
        .try_event_free()
        .unwrap();
    FmiCCodegenView::try_from(event_free).unwrap();
}
