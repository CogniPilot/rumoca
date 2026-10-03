use super::*;
use rumoca_core::native_body::NativeBody;

fn binary64() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, crate::SolveIntegerDomain::FULL)
}

/// One xorshift64* program over an Integer state of the given extent.
fn native_program(
    arithmetic: SolveArithmeticProfile,
    extent: u32,
) -> Result<TypedProgram, SolveProgramConstructionError> {
    TypedProgram::construct(arithmetic, |builder| {
        let state_type =
            SolveValueType::tensor(SolveScalarType::integer(arithmetic), vec![extent]).unwrap();
        let input = builder.declare_slot(
            state_type,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            SolveValueType::scalar(SolveScalarType::real(arithmetic)),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let state = builder.load(input, span(2))?;
        let values = builder.native(NativeBody::Xorshift64Star, &[state], span(3))?;
        builder.store(output, values[1], span(4))
    })
}

#[test]
fn native_body_is_typed_by_its_catalog_interface_and_replays() {
    let program = native_program(binary64(), 2).unwrap();
    let [_, native, _] = program.operations() else {
        panic!("load, native, store");
    };
    let SolveOperation::Native {
        body,
        operands,
        destinations,
    } = native.operation()
    else {
        panic!("a native operation");
    };
    assert_eq!(*body, NativeBody::Xorshift64Star);
    assert_eq!(operands.len(), 1);
    assert_eq!(destinations.len(), 2);
    assert_eq!(program.register_types()[1].dimensions(), [2]);
    assert!(program.register_types()[2].dimensions().is_empty());
    let mut inputs = Vec::new();
    native
        .operation()
        .visit_input_registers(|register| inputs.push(register));
    assert_eq!(inputs, operands.to_vec());
    let encoded = serde_json::to_value(&program).unwrap();
    let replayed: TypedProgram = serde_json::from_value(encoded.clone()).unwrap();
    assert_eq!(replayed, program);
    let mut forged = encoded;
    forged["operations"][1]["operation"]["body"] = serde_json::json!("xorshift128_plus");
    assert!(serde_json::from_value::<TypedProgram>(forged).is_err());
}

/// The operands must be the catalog inputs, the C `int` results must fit the
/// Integer domain, and the C `double` result must be binary64.
#[test]
fn native_body_outside_its_interface_is_refused() {
    let refused = |result: Result<TypedProgram, SolveProgramConstructionError>| {
        matches!(
            result,
            Err(SolveProgramConstructionError::InvalidCallInterface { .. })
        )
    };
    assert!(refused(native_program(binary64(), 3)));
    assert!(refused(native_program(
        SolveArithmeticProfile::construct(
            SolveRealFormat::Binary32,
            crate::SolveIntegerDomain::FULL
        ),
        2
    )));
    assert!(refused(native_program(
        SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            crate::SolveIntegerDomain::construct(-1000, 1000).unwrap()
        ),
        2
    )));
}
