use super::*;

fn real_tensor(extent: u32) -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::real(profile()), vec![extent]).unwrap()
}

/// An input the body never stores is the caller's array, and the register
/// its load defines views it; an owned value is a frame array.
#[test]
fn an_unstored_input_and_its_load_are_views_of_the_caller_array() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            real_tensor(4),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            real_tensor(4),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let value = builder.load(input, span(2))?;
        let negated = builder.unary(SolveUnaryOperator::Negate, value, span(3))?;
        builder.store(output, negated, span(4))
    })
    .unwrap();
    let plan = program.storage_plan();
    assert_eq!(
        plan.slots(),
        [
            SolveSlotPlacement::Argument { argument: 0 },
            SolveSlotPlacement::Frame {
                extent: 4,
                argument: None
            },
        ]
    );
    assert_eq!(
        plan.registers(),
        [
            SolveRegisterPlacement::View { slot: 0 },
            SolveRegisterPlacement::Frame { extent: 4 },
        ]
    );
}

/// An input the body stores is copied into an owned frame array; argument
/// numbers count inputs in slot order, past outputs.
#[test]
fn a_stored_input_is_an_owned_frame_copy() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let output = builder.declare_slot(
            real_tensor(2),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(0),
        )?;
        let scratch = builder.declare_slot(
            real_tensor(2),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        builder.declare_slot(
            real_tensor(3),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(2),
        )?;
        let value = builder.load(scratch, span(3))?;
        let negated = builder.unary(SolveUnaryOperator::Negate, value, span(4))?;
        builder.store(scratch, negated, span(5))?;
        let again = builder.load(scratch, span(6))?;
        builder.store(output, again, span(7))
    })
    .unwrap();
    let plan = program.storage_plan();
    assert_eq!(
        plan.slots(),
        [
            SolveSlotPlacement::Frame {
                extent: 2,
                argument: None
            },
            SolveSlotPlacement::Frame {
                extent: 2,
                argument: Some(0)
            },
            SolveSlotPlacement::Argument { argument: 1 },
        ]
    );
    assert!(
        plan.registers()
            .iter()
            .all(|placement| matches!(placement, SolveRegisterPlacement::Frame { extent: 2 }))
    );
}

/// The plan is part of the program's assembly: a program replayed from its
/// wire form issues the same plan, and the serialized plan is not read back.
#[test]
fn a_replayed_program_assembles_the_same_plan() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            real_tensor(3),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            real_tensor(3),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let value = builder.load(input, span(2))?;
        builder.store(output, value, span(3))
    })
    .unwrap();
    let mut wire = serde_json::to_value(&program).unwrap();
    assert_eq!(
        wire["storage"]["slots"][0],
        serde_json::json!({"placement": "argument", "argument": 0})
    );
    wire["storage"] = serde_json::json!({"slots": [], "registers": []});
    let replayed: TypedProgram = serde_json::from_value(wire).unwrap();
    assert_eq!(replayed.storage_plan(), program.storage_plan());
}

/// An element read or update whose one-based indices are Integer constants
/// within the extents has its row-major offset issued; an index outside its
/// extent or known only at run time leaves the bounds check in place.
#[test]
fn constant_in_range_element_accesses_have_static_offsets() {
    let arithmetic = profile();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 3]).unwrap();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            matrix.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let at = builder.declare_slot(
            integer.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(1),
        )?;
        let output = builder.declare_slot(
            matrix.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let value = builder.load(input, span(3))?;
        let runtime = builder.load(at, span(4))?;
        let [one, two, three, four] = [1, 2, 3, 4].map(|index| {
            builder.constant(SolveValue::integer(arithmetic, index).unwrap(), span(5))
        });
        let (one, two, three, four) = (one?, two?, three?, four?);
        let read = builder.project_element_dynamic(value, &[two, three], span(6))?;
        builder.project_element_dynamic(value, &[two, four], span(7))?;
        builder.project_element_dynamic(value, &[runtime, one], span(8))?;
        let updated = builder.update_element(value, read, &[one, one], span(9))?;
        builder.store(output, updated, span(10))
    })
    .unwrap();
    assert_eq!(
        program.storage_plan().element_offsets(),
        [
            None,
            None,
            None,
            None,
            None,
            None,
            Some(5),
            None,
            None,
            Some(0),
            None,
        ]
    );
}
