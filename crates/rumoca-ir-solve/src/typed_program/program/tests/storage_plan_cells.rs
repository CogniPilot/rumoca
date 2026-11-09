use super::*;
use crate::SolveRegisterHome;

fn real() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile()))
}

fn scalar_slot<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    at: usize,
) -> Result<ProgramSlot<'p>, SolveProgramConstructionError> {
    builder.declare_slot(real(), storage, access, span(at))
}

/// A register defined after another's last read reuses its range; one live
/// at the same time gets its own. Slot-homed registers have no range.
#[test]
fn a_dead_registers_range_is_reused() {
    let program = TypedProgram::construct(profile(), |builder| {
        let input = scalar_slot(
            builder,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            0,
        )?;
        let output = scalar_slot(
            builder,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let local = scalar_slot(
            builder,
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let x = builder.load(input, span(2))?;
        let a = builder.binary(SolveBinaryOperator::Add, x, x, span(3))?;
        builder.store(local, a, span(3))?;
        let b = builder.binary(SolveBinaryOperator::Multiply, a, a, span(4))?;
        builder.store(local, b, span(4))?;
        let c = builder.binary(SolveBinaryOperator::Add, b, x, span(5))?;
        builder.store(local, c, span(5))?;
        let d = builder.binary(SolveBinaryOperator::Add, c, x, span(6))?;
        builder.store(output, d, span(7))
    })
    .unwrap();
    let cells = program.storage_plan().cells();
    assert_eq!(
        cells.offsets(),
        [None, Some(0), Some(1), Some(0), None],
        "x views the input, a and c share a range, d lives in the output"
    );
    assert_eq!(cells.extent(), 2);
}

/// Freed adjacent ranges merge, so a wider value later fits where two
/// narrower ones were.
#[test]
fn adjacent_free_ranges_merge() {
    let program = TypedProgram::construct(profile(), |builder| {
        let input = scalar_slot(
            builder,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            0,
        )?;
        let output = scalar_slot(
            builder,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let local = scalar_slot(
            builder,
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let x = builder.load(input, span(2))?;
        let a = builder.binary(SolveBinaryOperator::Add, x, x, span(3))?;
        builder.store(local, a, span(3))?;
        let b = builder.binary(SolveBinaryOperator::Multiply, x, x, span(4))?;
        builder.store(local, b, span(4))?;
        let c = builder.binary(SolveBinaryOperator::Add, a, b, span(5))?;
        let pair = builder.fill(c, vec![2], span(6))?;
        let sum = builder.reduce(SolveReductionOperator::Sum, pair, span(7))?;
        builder.store(output, sum, span(8))
    })
    .unwrap();
    let cells = program.storage_plan().cells();
    assert_eq!(
        cells.offsets(),
        [None, Some(0), Some(1), Some(2), Some(0), None]
    );
    assert_eq!(cells.extent(), 3);
}

/// A register homed in another register's cells shares its root's range.
#[test]
fn a_register_sharing_cells_shares_its_roots_range() {
    let vector = SolveValueType::tensor(SolveScalarType::real(profile()), vec![3]).unwrap();
    let program = TypedProgram::construct(profile(), |builder| {
        let input = builder.declare_slot(
            vector.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            vector.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let loaded = builder.load(input, span(2))?;
        let negated = builder.unary(SolveUnaryOperator::Negate, loaded, span(3))?;
        let index = builder.constant(SolveValue::integer(profile(), 2).unwrap(), span(4))?;
        let value = builder.constant(SolveValue::real(profile(), 1.0), span(5))?;
        let updated = builder.update_element(negated, value, &[index], span(6))?;
        builder.store(output, updated, span(7))
    })
    .unwrap();
    let freed = SolveAliasPlan::of(
        program.slots(),
        program.register_types(),
        program.operations(),
        program.storage_plan().fusion(),
        &[1],
    );
    assert_eq!(
        freed.registers()[4],
        SolveRegisterHome::Register { register: 1 }
    );
    let cells = SolveFrameCells::of(
        program.register_types(),
        program.operations(),
        program.storage_plan().fusion().last_reads(),
        freed.registers(),
    );
    assert_eq!(cells.offsets()[0], None);
    assert_eq!(cells.offsets()[4], cells.offsets()[1]);
    assert!(cells.offsets()[1].is_some());
}
