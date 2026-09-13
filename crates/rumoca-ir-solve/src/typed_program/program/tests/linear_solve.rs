use super::*;

fn arithmetic() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, crate::SolveIntegerDomain::FULL)
}

fn solve_program(extent: u32) -> TypedProgram {
    let arithmetic = arithmetic();
    TypedProgram::construct(arithmetic, |builder| {
        let element = SolveScalarType::real(arithmetic);
        let matrix_type = SolveValueType::tensor(element, vec![extent, extent]).unwrap();
        let vector_type = SolveValueType::tensor(element, vec![extent]).unwrap();
        let matrix = builder.declare_slot(
            matrix_type,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let rhs = builder.declare_slot(
            vector_type.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(1),
        )?;
        let output = builder.declare_slot(
            vector_type,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let matrix = builder.load(matrix, span(3))?;
        let rhs = builder.load(rhs, span(4))?;
        let solution = builder.linear_solve(matrix, rhs, span(5))?;
        builder.store(output, solution, span(6))
    })
    .unwrap()
}

#[test]
fn linear_solve_keeps_one_operation_at_every_extent() {
    for extent in [1, 2, 4096] {
        let program = solve_program(extent);
        assert_eq!(program.operations().len(), 4);
        assert_eq!(program.register_types().len(), 3);
        assert_eq!(program.register_types()[2].dimensions(), [extent]);
    }
}

#[test]
fn linear_solve_replay_rejects_a_forged_result_shape() {
    let program = solve_program(2);
    let encoded = serde_json::to_value(&program).unwrap();
    let replayed: TypedProgram = serde_json::from_value(encoded.clone()).unwrap();
    assert_eq!(replayed, program);
    let mut forged = encoded;
    forged["register_types"][2]["dimensions"] = serde_json::json!([1, 2]);
    assert!(serde_json::from_value::<TypedProgram>(forged).is_err());
}

#[test]
fn linear_solve_rejects_non_square_and_mismatched_shapes() {
    for (matrix_shape, rhs_shape) in [
        (vec![2, 3], vec![2]),
        (vec![2, 2], vec![3]),
        (vec![2, 2], vec![2, 1]),
        (vec![4], vec![2]),
    ] {
        let error = invalid_signature(arithmetic(), true, matrix_shape, rhs_shape);
        assert_eq!(
            error,
            SolveProgramConstructionError::InvalidTensorAlgebra {
                provenance: span(4)
            }
        );
    }
}

#[test]
fn linear_solve_rejects_integer_and_binary32_profiles() {
    for (arithmetic, real) in [(arithmetic(), false), (profile(), true)] {
        let error = invalid_signature(arithmetic, real, vec![2, 2], vec![2]);
        assert_eq!(
            error,
            SolveProgramConstructionError::InvalidTensorAlgebra {
                provenance: span(4)
            }
        );
    }
}

fn invalid_signature(
    arithmetic: SolveArithmeticProfile,
    real: bool,
    matrix_shape: Vec<u32>,
    rhs_shape: Vec<u32>,
) -> SolveProgramConstructionError {
    TypedProgram::construct(arithmetic, |builder| {
        let element = if real {
            SolveScalarType::real(arithmetic)
        } else {
            SolveScalarType::integer(arithmetic)
        };
        let matrix = builder.declare_slot(
            SolveValueType::tensor(element, matrix_shape).unwrap(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let rhs = builder.declare_slot(
            SolveValueType::tensor(element, rhs_shape).unwrap(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(1),
        )?;
        let matrix = builder.load(matrix, span(2))?;
        let rhs = builder.load(rhs, span(3))?;
        builder.linear_solve(matrix, rhs, span(4))?;
        Ok(())
    })
    .unwrap_err()
}
