use super::*;
use crate::{FaultKind, SolvePureCallTable};
use rumoca_core::SourceId;

fn at() -> Span {
    Span::from_offsets(SourceId::from_source_name("parameter-stage.alg"), 1, 2)
}

fn real(profile: SolveArithmeticProfile) -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile))
}

fn input<'p>(
    b: &mut TypedProgramBuilder<'p>,
    value_type: SolveValueType,
) -> Result<ProgramRegister<'p>, SolveProgramConstructionError> {
    let slot = b.declare_slot(
        value_type,
        SolveStorageClass::Input,
        SolveSlotAccess::ReadOnly,
        at(),
    )?;
    b.load(slot, at())
}

fn kept(candidate: &SolveParameterStageCandidate<'_>, index: usize) -> bool {
    candidate
        .call_steps()
        .contains(&SolveParameterCallStep::Keep { operation: index })
}

#[test]
fn candidate_keeps_all_checks_and_publication_at_original_ordinals() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let p = input(b, real(profile))?;
        let x = input(b, real(profile))?;
        b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, at())?;
        b.check_finite(p, SolveFiniteCheck::Input { coordinate: 1 }, at())?;
        let square = b.binary(SolveBinaryOperator::Multiply, p, p, at())?;
        b.check_finite(
            square,
            SolveFiniteCheck::Primal {
                program: 1,
                operation: 1,
            },
            at(),
        )?;
        let sum = b.binary(SolveBinaryOperator::Add, x, square, at())?;
        b.check_finite(sum, SolveFiniteCheck::Derivative, at())?;
        let out = b.declare_slot(
            real(profile),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            at(),
        )?;
        b.store(out, sum, at())
    })
    .unwrap();
    let before = program.clone();
    let candidate = program
        .parameter_stage_candidate(&[program.slots()[0].id()])
        .unwrap();
    assert!(std::ptr::eq(candidate.source(), &program));
    assert_eq!(candidate.stage_operation_ordinals(), [0, 4]);
    assert_eq!(candidate.required_slots(), [program.slots()[0].id()]);
    assert_eq!(candidate.values().len(), 2);
    for (index, operation) in program.operations().iter().enumerate() {
        if matches!(
            operation.operation(),
            SolveOperation::CheckFinite { .. } | SolveOperation::Store { .. }
        ) {
            assert!(kept(&candidate, index));
        }
    }
    assert_eq!(program, before);
}

#[test]
fn storage_labels_do_not_automatically_certify_stable_parameters() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let slot = b.declare_slot(
            real(profile),
            SolveStorageClass::TunableParameter,
            SolveSlotAccess::ReadOnly,
            at(),
        )?;
        let p = b.load(slot, at())?;
        b.unary(SolveUnaryOperator::Sin, p, at())?;
        Ok(())
    })
    .unwrap();
    let candidate = program.parameter_stage_candidate(&[]).unwrap();
    assert!(candidate.stage_operation_ordinals().is_empty());
    assert_eq!(candidate.call_steps().len(), program.operations().len());
    let proposed = program
        .parameter_stage_candidate(&[program.slots()[0].id()])
        .unwrap();
    assert_eq!(proposed.required_slots(), [program.slots()[0].id()]);
}

#[test]
fn invalid_or_mutable_slot_proposals_refuse_before_partitioning() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        input(b, real(profile))?;
        b.declare_slot(
            real(profile),
            SolveStorageClass::PersistentState,
            SolveSlotAccess::ReadWrite,
            at(),
        )?;
        Ok(())
    })
    .unwrap();
    let read_only = program.slots()[0].id();
    let mutable = program.slots()[1].id();
    assert_eq!(
        program.parameter_stage_candidate(&[mutable]).unwrap_err(),
        SolveParameterStageError::MutableSlot(mutable)
    );
    assert_eq!(
        program
            .parameter_stage_candidate(&[read_only, read_only])
            .unwrap_err(),
        SolveParameterStageError::DuplicateSlot(read_only)
    );
    assert_eq!(
        program
            .parameter_stage_candidate(&[SolveSlotId(99)])
            .unwrap_err(),
        SolveParameterStageError::UnknownSlot(SolveSlotId(99))
    );
}

#[test]
fn unknown_integer_arithmetic_keeps_its_existing_fault_site() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let p = input(b, SolveValueType::scalar(SolveScalarType::integer(profile)))?;
        let one = b.constant(SolveValue::integer(profile, 1).unwrap(), at())?;
        b.binary(SolveBinaryOperator::Add, p, one, at())?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        program.fault_class(2),
        FaultClass::MayFault(FaultKind::IntegerArithmetic)
    );
    let candidate = program
        .parameter_stage_candidate(&[program.slots()[0].id()])
        .unwrap();
    assert!(candidate.stage_operation_ordinals().is_empty());
    assert!(kept(&candidate, 2));
}

#[test]
fn nonfinite_real_results_never_discharge_call_time_checks() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let p = input(b, real(profile))?;
        let zero = b.constant(SolveValue::real(profile, 0.0), at())?;
        let quotient = b.binary(SolveBinaryOperator::Divide, p, zero, at())?;
        b.check_finite(
            quotient,
            SolveFiniteCheck::Primal {
                program: 3,
                operation: 9,
            },
            at(),
        )
    })
    .unwrap();
    let candidate = program
        .parameter_stage_candidate(&[program.slots()[0].id()])
        .unwrap();
    assert_eq!(candidate.stage_operation_ordinals(), [0, 1, 2]);
    assert!(kept(&candidate, 3));
    assert_eq!(candidate.values().len(), 1);
    assert_eq!(candidate.values()[0].operation(), 2);
}

#[test]
fn tensor_stage_values_remain_compact_and_keep_their_full_shape() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let p = input(b, real(profile))?;
        let values = b.fill(p, vec![1000, 1000], at())?;
        b.check_finite(values, SolveFiniteCheck::Derivative, at())
    })
    .unwrap();
    let candidate = program
        .parameter_stage_candidate(&[program.slots()[0].id()])
        .unwrap();
    assert_eq!(candidate.stage_operations().count(), 2);
    assert_eq!(candidate.values().len(), 1);
    let value = candidate.values()[0].register();
    assert_eq!(
        program.register_types()[value.index()].dimensions(),
        [1000, 1000]
    );
    assert_eq!(candidate.retained_scalar_cells(), 1_000_000);
    assert!(kept(&candidate, 2));
}

#[test]
fn structured_regions_stay_whole_without_speculative_body_extraction() {
    let profile = SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let p = input(b, real(profile))?;
        let condition = b.constant(SolveValue::boolean(true), at())?;
        b.conditional(
            condition,
            &[p],
            vec![real(profile)],
            at(),
            logarithm_arm,
            logarithm_arm,
        )?;
        Ok(())
    })
    .unwrap();
    let candidate = program
        .parameter_stage_candidate(&[program.slots()[0].id()])
        .unwrap();
    assert!(candidate.stage_operation_ordinals().is_empty());
    assert!(kept(&candidate, 2));
}

fn logarithm_arm<'p>(
    child: &mut TypedProgramBuilder<'p>,
    inputs: &[ProgramSlot<'p>],
    outputs: &[ProgramSlot<'p>],
) -> Result<(), SolveProgramConstructionError> {
    let value = child.load(inputs[0], at())?;
    let result = child.unary(SolveUnaryOperator::Log, value, at())?;
    child.store(outputs[0], result, at())
}
