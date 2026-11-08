use super::*;
use crate::SolveRegisterHome;

fn vector(extent: u32) -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::real(profile()), vec![extent]).unwrap()
}

fn slot<'p>(
    builder: &mut TypedProgramBuilder<'p>,
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    at: usize,
) -> Result<ProgramSlot<'p>, SolveProgramConstructionError> {
    builder.declare_slot(vector(3), storage, access, span(at))
}

/// Every `Register` home names a register whose own home is `Own`: applying
/// the rules in issue order resolves each chain to its root, with no cycle.
fn assert_resolved(homes: &[SolveRegisterHome]) {
    for (register, home) in homes.iter().enumerate() {
        if let SolveRegisterHome::Register { register: root } = home {
            assert_ne!(*root, register, "register {register} aliases itself");
            assert_eq!(
                homes[*root],
                SolveRegisterHome::Own,
                "register {register}'s root {root} is not its own home"
            );
        }
    }
}

/// `input` (read-only Input) → negate → `update` at its last read → store
/// into `output`; operation 5 stores.
fn update_chain(storage: SolveStorageClass) -> TypedProgram {
    TypedProgram::construct(profile(), |builder| {
        let input = slot(builder, storage, SolveSlotAccess::ReadOnly, 0)?;
        let output = slot(
            builder,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let loaded = builder.load(input, span(2))?;
        let negated = builder.unary(SolveUnaryOperator::Negate, loaded, span(3))?;
        let index = builder.constant(SolveValue::integer(profile(), 2).unwrap(), span(4))?;
        let value = builder.constant(SolveValue::real(profile(), 1.0), span(5))?;
        let updated = builder.update_element(negated, value, &[index], span(6))?;
        builder.store(output, updated, span(7))
    })
    .unwrap()
}

/// A read-only input load views the input; a value whose final store is its
/// last read lives in the output slot; a consumed update takes the
/// aggregate's cells.
#[test]
fn the_alias_rules_resolve_each_register_home() {
    let program = update_chain(SolveStorageClass::Input);
    let homes = program.storage_plan().aliases().registers();
    assert_eq!(homes[0], SolveRegisterHome::InputSlot { slot: 0 });
    assert_eq!(homes[1], SolveRegisterHome::Own);
    // The update's destination is stored last, so it lives in the output
    // slot; the consumed-update rule then refuses it, because the
    // destination no longer lives in the body's own cells.
    assert_eq!(homes[4], SolveRegisterHome::OutputSlot { slot: 1 });
    assert_resolved(homes);
}

/// A constant slot is read-only but lives in the body's own cells: a load of
/// it is never an input view.
#[test]
fn a_read_only_constant_slot_is_never_aliased() {
    let program = update_chain(SolveStorageClass::Constant);
    let homes = program.storage_plan().aliases().registers();
    assert_eq!(homes[0], SolveRegisterHome::Own);
    assert!(
        !homes
            .iter()
            .any(|home| matches!(home, SolveRegisterHome::InputSlot { .. }))
    );
    assert_resolved(homes);
}

/// With the output slot unaliased, the stored update keeps the body's own
/// cells, so the consumed-update rule now gives it the aggregate's cells.
#[test]
fn an_unaliased_output_slot_frees_its_store_for_a_consumed_update() {
    let program = update_chain(SolveStorageClass::Input);
    let freed = SolveAliasPlan::of(
        program.slots(),
        program.register_types(),
        program.operations(),
        program.storage_plan().fusion().last_reads(),
        &[1],
    );
    let homes = freed.registers();
    assert!(
        !homes.contains(&SolveRegisterHome::OutputSlot { slot: 1 }),
        "no register may take an unaliased slot"
    );
    assert_eq!(homes[4], SolveRegisterHome::Register { register: 1 });
    assert_resolved(homes);
    let ordinary = program.storage_plan().aliases().registers();
    assert_eq!(observation_cascades(ordinary, homes, &[1]), Ok(vec![4]));

    // A change that no freed register explains is refused, as is a home in
    // the unaliased slot.
    let mut drifted = homes.to_vec();
    drifted[0] = SolveRegisterHome::Register { register: 1 };
    assert!(observation_cascades(ordinary, &drifted, &[1]).is_err());
    let mut kept = homes.to_vec();
    kept[4] = SolveRegisterHome::OutputSlot { slot: 1 };
    assert!(observation_cascades(ordinary, &kept, &[1]).is_err());
}

/// A value read after its store, or a slot loaded between the value's
/// definition and its store, keeps its own cells.
#[test]
fn an_observed_or_later_read_store_is_not_aliased() {
    let read_after = TypedProgram::construct(profile(), |builder| {
        let input = slot(
            builder,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            0,
        )?;
        let output = slot(
            builder,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let loaded = builder.load(input, span(2))?;
        let negated = builder.unary(SolveUnaryOperator::Negate, loaded, span(3))?;
        builder.store(output, negated, span(4))?;
        // Not a second negation, which the builder folds back to `loaded`.
        let again = builder.binary(SolveBinaryOperator::Add, negated, loaded, span(5))?;
        builder.store(output, again, span(6))
    })
    .unwrap();
    let homes = read_after.storage_plan().aliases().registers();
    assert_eq!(homes[1], SolveRegisterHome::Own, "read after its store");
    // The last value is defined after the first store and is the slot's
    // final store at its last read.
    assert_eq!(homes[2], SolveRegisterHome::OutputSlot { slot: 1 });

    let observed = TypedProgram::construct(profile(), |builder| {
        let input = slot(
            builder,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            0,
        )?;
        let output = slot(
            builder,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let loaded = builder.load(input, span(2))?;
        let negated = builder.unary(SolveUnaryOperator::Negate, loaded, span(3))?;
        builder.store(output, loaded, span(4))?;
        builder.store(output, negated, span(5))
    })
    .unwrap();
    let homes = observed.storage_plan().aliases().registers();
    assert_eq!(
        homes[1],
        SolveRegisterHome::Own,
        "the slot is stored between definition and store"
    );
}

fn one_pass() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

/// A fold whose entry value is read once, by the fold, at its last read,
/// leaves its result in that value's cells; an entry the fold also captures
/// is read twice and keeps its own.
fn fold_program(capture_entry: bool) -> TypedProgram {
    TypedProgram::construct(profile(), |builder| {
        let input = slot(
            builder,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            0,
        )?;
        let output = slot(
            builder,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            1,
        )?;
        let loaded = builder.load(input, span(2))?;
        let entry = builder.unary(SolveUnaryOperator::Negate, loaded, span(3))?;
        let captures = if capture_entry { vec![entry] } else { vec![] };
        let result = builder.fold(
            one_pass(),
            &[entry],
            &captures,
            span(4),
            |region, carried, _, _, outputs| {
                let value = region.load(carried[0], span(5))?;
                let next = region.unary(SolveUnaryOperator::Negate, value, span(6))?;
                region.store(outputs[0], next, span(7))
            },
        )?[0];
        // Read after the fold, so the result is not the output's final store.
        let doubled = builder.binary(SolveBinaryOperator::Add, result, result, span(8))?;
        builder.store(output, doubled, span(9))
    })
    .unwrap()
}

#[test]
fn a_consumed_fold_entry_lends_its_cells_to_the_result() {
    let consumed = fold_program(false);
    let homes = consumed.storage_plan().aliases().registers();
    assert_eq!(homes[2], SolveRegisterHome::Register { register: 1 });
    assert_resolved(homes);
    let captured = fold_program(true);
    let homes = captured.storage_plan().aliases().registers();
    assert_eq!(homes[2], SolveRegisterHome::Own, "the entry is read twice");
    assert_resolved(homes);
}
