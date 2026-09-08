use super::*;
use crate::{
    SolveMatrixMultiplyArithmetic, SolveMatrixMultiplyContraction, SolveMatrixMultiplyInfinity,
    SolveMatrixMultiplyNan, SolveMatrixMultiplyOrder, SolveMatrixMultiplyRounding,
    SolveMatrixMultiplySignedZero, SolveMatrixMultiplyStatus, SolveMatrixMultiplySubnormal,
    SolveMatrixOperandLayout, SolveMatrixResultLayout, SolveRealFormat, SolveTypeConstructionError,
    SolveValueKind,
};
use rumoca_core::{
    RealMatrixMultiplySemantics, SourceId, StructuredIndexBinder, StructuredIndexDomain,
};

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("typed_program.alg"),
        start,
        start + 1,
    )
}

fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        crate::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    )
}

#[test]
fn aggregate_copy_stays_one_typed_load_and_store() {
    let arithmetic = profile();
    let tensor = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4, 4]).unwrap();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            tensor.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            tensor.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let value = builder.load(input, span(2))?;
        builder.store(output, value, span(3))
    })
    .unwrap();
    assert_eq!(program.operations().len(), 2);
    assert_eq!(program.register_types(), &[tensor]);
}

#[test]
fn profile_rounds_real_constants_at_construction() {
    let value = SolveValue::real(profile(), 1.0 + f64::from(f32::EPSILON) / 4.0);
    assert_eq!(value.kind(), SolveValueKind::Real32(1.0_f32.to_bits()));
}

#[test]
fn boolean_and_integer_never_enter_real_registers() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let boolean = builder.constant(SolveValue::boolean(true), span(0))?;
        let integer = builder.constant(
            SolveValue::integer(arithmetic, 2).map_err(|error| match error {
                SolveTypeConstructionError::IntegerOutsideDomain { .. } => {
                    SolveProgramConstructionError::ProfileMismatch {
                        provenance: span(1),
                    }
                }
                _ => SolveProgramConstructionError::TypeMismatch {
                    provenance: span(1),
                },
            })?,
            span(1),
        )?;
        assert!(
            builder
                .unary(SolveUnaryOperator::Not, boolean, span(2))
                .is_ok()
        );
        assert!(
            builder
                .unary(SolveUnaryOperator::Sign, integer, span(3))
                .is_ok()
        );
        assert!(
            builder
                .binary(SolveBinaryOperator::Min, integer, integer, span(4))
                .is_ok()
        );
        Ok(())
    })
    .unwrap();
    assert!(matches!(
        program.register_types()[0].element_type(),
        SolveScalarType::Boolean
    ));
    assert!(matches!(
        program.register_types()[1].element_type(),
        SolveScalarType::Integer(_)
    ));
}

#[test]
fn integer_operations_without_result_range_evidence_refuse_before_commit() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let integer = builder.constant(
            SolveValue::integer(arithmetic, 2).expect("fixture lies in the selected domain"),
            span(10),
        )?;
        let real = builder.constant(SolveValue::real(arithmetic, 2.0), span(11))?;
        for (operator, provenance) in [
            (SolveUnaryOperator::Negate, span(12)),
            (SolveUnaryOperator::Abs, span(13)),
        ] {
            let register_count = builder.register_types.len();
            let operation_count = builder.operations.len();
            assert_eq!(
                builder.unary(operator, integer, provenance),
                Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
            );
            assert_eq!(builder.register_types.len(), register_count);
            assert_eq!(builder.operations.len(), operation_count);
        }
        for (operator, provenance) in [
            (SolveBinaryOperator::Add, span(14)),
            (SolveBinaryOperator::Subtract, span(15)),
            (SolveBinaryOperator::Multiply, span(16)),
        ] {
            let register_count = builder.register_types.len();
            let operation_count = builder.operations.len();
            assert_eq!(
                builder.binary(operator, integer, integer, provenance),
                Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
            );
            assert_eq!(builder.register_types.len(), register_count);
            assert_eq!(builder.operations.len(), operation_count);
        }
        for (operator, provenance) in [
            (SolveConversionOperator::RealToIntegerTowardZero, span(17)),
            (
                SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
                span(18),
            ),
        ] {
            let register_count = builder.register_types.len();
            let operation_count = builder.operations.len();
            assert_eq!(
                builder.convert(operator, real, provenance),
                Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
            );
            assert_eq!(builder.register_types.len(), register_count);
            assert_eq!(builder.operations.len(), operation_count);
        }
        Ok(())
    })
    .expect("refusals leave the valid prefix constructible");
    assert_eq!(program.operations().len(), 2);
}

#[test]
fn integer_sign_constructs_only_when_its_complete_image_fits_the_domain() {
    for (minimum, maximum, operand) in [(1, 10, 2), (-10, -1, -2), (0, 0, 0), (-1, 1, 0)] {
        let arithmetic = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary32,
            crate::SolveIntegerDomain::construct(minimum, maximum).unwrap(),
            RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        );
        let program = TypedProgram::construct(arithmetic, |builder| {
            let operand = builder.constant(
                SolveValue::integer(arithmetic, operand)
                    .expect("fixture belongs to the retained domain"),
                span(20),
            )?;
            builder.unary(SolveUnaryOperator::Sign, operand, span(21))?;
            Ok(())
        })
        .expect("the entire attainable Sign image belongs to the retained domain");
        assert_eq!(program.operations().len(), 2);
    }

    for (minimum, maximum, operand) in [(2, 10, 2), (-10, -2, -2)] {
        let arithmetic = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary32,
            crate::SolveIntegerDomain::construct(minimum, maximum).unwrap(),
            RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        );
        let provenance = span(22);
        let program = TypedProgram::construct(arithmetic, |builder| {
            let operand = builder.constant(
                SolveValue::integer(arithmetic, operand)
                    .expect("fixture belongs to the retained domain"),
                span(20),
            )?;
            let register_count = builder.register_types.len();
            let operation_count = builder.operations.len();
            assert_eq!(
                builder.unary(SolveUnaryOperator::Sign, operand, provenance),
                Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
            );
            assert_eq!(builder.register_types.len(), register_count);
            assert_eq!(builder.operations.len(), operation_count);
            Ok(())
        })
        .expect("a refused Sign leaves its valid constant prefix constructible");
        assert_eq!(program.operations().len(), 1);
    }
}

#[test]
fn aggregate_integer_arithmetic_cannot_bypass_result_range_evidence() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let scalar = builder.constant(
            SolveValue::integer(arithmetic, 2).expect("fixture lies in the retained domain"),
            span(30),
        )?;
        let aggregate = builder.fill(scalar, vec![2], span(31))?;
        for (operator, provenance) in [
            (SolveBinaryOperator::Add, span(32)),
            (SolveBinaryOperator::Subtract, span(33)),
            (SolveBinaryOperator::Multiply, span(34)),
        ] {
            let register_count = builder.register_types.len();
            let operation_count = builder.operations.len();
            assert_eq!(
                builder.broadcast_binary(operator, aggregate, scalar, false, provenance),
                Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
            );
            assert_eq!(builder.register_types.len(), register_count);
            assert_eq!(builder.operations.len(), operation_count);
        }
        let provenance = span(35);
        let register_count = builder.register_types.len();
        let operation_count = builder.operations.len();
        assert_eq!(
            builder.scale(aggregate, scalar, provenance),
            Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
        );
        assert_eq!(builder.register_types.len(), register_count);
        assert_eq!(builder.operations.len(), operation_count);
        Ok(())
    })
    .expect("aggregate refusals leave their valid prefix constructible");
    assert_eq!(program.operations().len(), 2);
}

#[test]
fn integer_diagonal_proves_synthesized_zero_before_commit_and_wire_replay() {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        crate::SolveIntegerDomain::construct(1, 10).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let program = TypedProgram::construct(arithmetic, |builder| {
        let scalar = builder.constant(
            SolveValue::integer(arithmetic, 2).expect("fixture lies in the retained domain"),
            span(40),
        )?;
        let vector = builder.fill(scalar, vec![2], span(41))?;
        let register_count = builder.register_types.len();
        let operation_count = builder.operations.len();
        let provenance = span(42);
        assert_eq!(
            builder.diagonal(vector, provenance),
            Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
        );
        assert_eq!(builder.register_types.len(), register_count);
        assert_eq!(builder.operations.len(), operation_count);

        let singleton = builder.fill(scalar, vec![1], span(43))?;
        builder.diagonal(singleton, span(44))?;
        Ok(())
    })
    .expect("a singleton diagonal synthesizes no observable zero");

    let replayed: TypedProgram = serde_json::from_value(serde_json::to_value(&program).unwrap())
        .expect("the proved singleton diagonal replays");
    assert_eq!(replayed, program);

    let mut wire = serde_json::to_value(&program).unwrap();
    wire["register_types"][2]["dimensions"] = serde_json::json!([2]);
    wire["register_types"][2]["scalar_count"] = serde_json::json!(2);
    wire["register_types"][3]["dimensions"] = serde_json::json!([2, 2]);
    wire["register_types"][3]["scalar_count"] = serde_json::json!(4);
    let error = serde_json::from_value::<TypedProgram>(wire)
        .expect_err("wire replay must reissue the diagonal zero proof");
    assert!(
        error
            .to_string()
            .contains("integer operation has no construction-issued result-range proof"),
        "unexpected diagonal replay error: {error}"
    );
}

#[test]
fn integer_identity_proves_only_the_literals_observable_at_each_extent() {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        crate::SolveIntegerDomain::construct(1, 10).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let program = TypedProgram::construct(arithmetic, |builder| {
        builder.identity(SolveScalarType::integer(arithmetic), 0, span(45))?;
        builder.identity(SolveScalarType::integer(arithmetic), 1, span(46))?;
        let register_count = builder.register_types.len();
        let operation_count = builder.operations.len();
        let provenance = span(47);
        assert_eq!(
            builder.identity(SolveScalarType::integer(arithmetic), 2, provenance),
            Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
        );
        assert_eq!(builder.register_types.len(), register_count);
        assert_eq!(builder.operations.len(), operation_count);
        Ok(())
    })
    .expect("empty and singleton identities require no observable zero");

    let mut wire = serde_json::to_value(&program).unwrap();
    wire["register_types"][1]["dimensions"] = serde_json::json!([2, 2]);
    wire["register_types"][1]["scalar_count"] = serde_json::json!(4);
    let error = serde_json::from_value::<TypedProgram>(wire)
        .expect_err("wire replay must reissue the identity literal proof");
    assert!(
        error
            .to_string()
            .contains("integer operation has no construction-issued result-range proof"),
        "unexpected identity replay error: {error}"
    );

    let no_one = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        crate::SolveIntegerDomain::construct(2, 10).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    TypedProgram::construct(no_one, |builder| {
        builder.identity(SolveScalarType::integer(no_one), 0, span(48))?;
        let provenance = span(49);
        assert_eq!(
            builder.identity(SolveScalarType::integer(no_one), 1, provenance),
            Err(SolveProgramConstructionError::UnprovedIntegerRange { provenance })
        );
        Ok(())
    })
    .expect("an empty identity observes neither zero nor one");

    let complete = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        crate::SolveIntegerDomain::construct(0, 10).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    TypedProgram::construct(complete, |builder| {
        builder.identity(SolveScalarType::integer(complete), 2, span(50))?;
        Ok(())
    })
    .expect("a nontrivial identity constructs when zero and one are retained");
}

#[test]
fn mismatched_store_fails_before_the_operation_is_committed() {
    let arithmetic = profile();
    let error = TypedProgram::construct(arithmetic, |builder| {
        let output = builder.declare_slot(
            SolveValueType::scalar(SolveScalarType::real(arithmetic)),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(0),
        )?;
        let value = builder.constant(SolveValue::boolean(true), span(1))?;
        builder.store(output, value, span(2))
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::TypeMismatch {
            provenance: span(2)
        }
    );
}

#[test]
fn method_local_load_requires_a_dominating_definition() {
    let arithmetic = profile();
    let error = TypedProgram::construct(arithmetic, |builder| {
        let local = builder.declare_slot(
            SolveValueType::scalar(SolveScalarType::real(arithmetic)),
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            span(0),
        )?;
        builder.load(local, span(1))?;
        Ok(())
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::UninitializedSlot {
            provenance: span(1)
        }
    );
}

#[test]
fn aggregate_operations_retain_compact_shape_and_exact_types() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let two = builder.constant(SolveValue::real(arithmetic, 2.0), span(1))?;
        let filled = builder.fill(one, vec![2, 2], span(2))?;
        let aggregate = builder.construct_aggregate(&[one, two, two, one], vec![2, 2], span(3))?;
        let element = builder.project_element(aggregate, vec![1, 0], span(4))?;
        let slice = builder.project_slice(filled, vec![0, 1], vec![2, 1], span(5))?;
        let index = builder.constant(
            SolveValue::integer(arithmetic, 1).expect("index fits the checked domain"),
            span(6),
        )?;
        let axes = [
            ProgramTensorViewAxis::Span {
                origin: 0,
                extent: 2,
            },
            ProgramTensorViewAxis::Index(index),
        ];
        let view = builder.project_view(aggregate, &axes, span(7))?;
        let with_view = builder.update_view(aggregate, view, &axes, span(8))?;
        let selected = builder.select_element(with_view, &[index, index], element, span(9))?;
        let updated_element =
            builder.update_element(with_view, selected, &[index, index], span(10))?;
        let updated_slice = builder.update_slice(updated_element, slice, vec![0, 1], span(11))?;
        assert_eq!(builder.register_type(slice, span(8))?.dimensions(), &[2, 1]);
        assert_eq!(builder.register_type(view, span(8))?.dimensions(), &[2]);
        assert!(
            builder
                .register_type(selected, span(9))?
                .dimensions()
                .is_empty()
        );
        assert_eq!(
            builder.register_type(updated_slice, span(10))?.dimensions(),
            &[2, 2]
        );
        Ok(())
    })
    .expect("checked aggregate operations construct");
    assert_eq!(program.operations().len(), 12);
    assert!(matches!(
        program.operations()[2].operation(),
        SolveOperation::Fill { .. }
    ));
    assert!(matches!(
        program.operations()[3].operation(),
        SolveOperation::ConstructAggregate { elements, .. } if elements.len() == 4
    ));
    assert!(matches!(
        program.operations()[5].operation(),
        SolveOperation::ProjectSlice { origin, .. } if origin.as_ref() == [0, 1]
    ));
    assert!(matches!(
        program.operations()[7].operation(),
        SolveOperation::ProjectView { .. }
    ));
    assert!(matches!(
        program.operations()[8].operation(),
        SolveOperation::UpdateView { .. }
    ));
    assert!(matches!(
        program.operations()[10].operation(),
        SolveOperation::UpdateElement { .. }
    ));
    assert!(matches!(
        program.operations()[11].operation(),
        SolveOperation::UpdateSlice { origin, .. } if origin.as_ref() == [0, 1]
    ));
    let replayed: TypedProgram = serde_json::from_str(
        &serde_json::to_string(&program).expect("aggregate program serializes"),
    )
    .expect("aggregate operations replay through checked constructors");
    assert_eq!(replayed, program);
}

#[test]
fn tensor_algebra_derives_shapes_without_coordinate_operations() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let matrix = builder.fill(one, vec![2, 3], span(1))?;
        let vector = builder.fill(one, vec![3], span(2))?;
        let scaled = builder.scale(matrix, one, span(3))?;
        let transposed = builder.transpose(scaled, span(4))?;
        let product = builder.matrix_multiply(matrix, vector, span(5))?;
        let identity = builder.identity(SolveScalarType::real(arithmetic), 3, span(8))?;
        let diagonal = builder.diagonal(vector, span(9))?;
        assert_eq!(
            builder.register_type(transposed, span(8))?.dimensions(),
            &[3, 2]
        );
        assert_eq!(builder.register_type(product, span(9))?.dimensions(), &[2]);
        assert_eq!(
            builder.register_type(identity, span(11))?.dimensions(),
            &[3, 3]
        );
        assert_eq!(
            builder.register_type(diagonal, span(12))?.dimensions(),
            &[3, 3]
        );
        Ok(())
    })
    .expect("checked tensor algebra constructs");
    assert_eq!(program.operations().len(), 8);
    assert!(matches!(
        program.operations()[3].operation(),
        SolveOperation::Scale { .. }
    ));
    assert!(matches!(
        program.operations()[4].operation(),
        SolveOperation::Transpose { .. }
    ));
    assert!(matches!(
        program.operations()[5].operation(),
        SolveOperation::MatrixMultiply {
            plan,
            ..
        } if matches!(plan.arithmetic(), SolveMatrixMultiplyArithmetic::Real {
            accumulator: SolveRealFormat::Binary32,
            semantics: RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
            ..
        })
    ));
    assert!(matches!(
        program.operations()[6].operation(),
        SolveOperation::Identity { .. }
    ));
    assert!(matches!(
        program.operations()[7].operation(),
        SolveOperation::Diagonal { .. }
    ));
    let round_trip: TypedProgram = serde_json::from_str(&serde_json::to_string(&program).unwrap())
        .expect("wire replay retains compact tensor algebra");
    assert_eq!(round_trip, program);
}

#[test]
fn matrix_reduction_profile_is_issued_once_without_changing_graph_granularity() {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        crate::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
    );
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 3]).unwrap();
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let lhs = builder.declare_slot(
            matrix.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(12),
        )?;
        let rhs = builder.declare_slot(
            vector.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(13),
        )?;
        let lhs = builder.load(lhs, span(14))?;
        let rhs = builder.load(rhs, span(15))?;
        builder.matrix_multiply(lhs, rhs, span(16))?;
        Ok(())
    })
    .unwrap();

    assert_eq!(program.operations().len(), 3);
    assert!(matches!(
        program.operations()[2].operation(),
        SolveOperation::MatrixMultiply {
            plan,
            ..
        } if matches!(plan.arithmetic(), SolveMatrixMultiplyArithmetic::Real {
            accumulator: SolveRealFormat::Binary32,
            semantics: RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ..
        })
    ));
    assert!(program.operations().iter().all(|operation| !matches!(
        operation.operation(),
        SolveOperation::Binary {
            operator: SolveBinaryOperator::Add | SolveBinaryOperator::Multiply,
            ..
        } | SolveOperation::ProjectElement { .. }
            | SolveOperation::Reduce { .. }
    )));

    let mut forged = serde_json::to_value(&program).unwrap();
    forged["operations"][2]["operation"]["plan"] = serde_json::json!({
        "rows": 2,
        "inner": 3,
        "columns": 1
    });
    let error = serde_json::from_value::<TypedProgram>(forged)
        .expect_err("wire cannot replace an occurrence contract issued by its root profile");
    assert!(error.to_string().contains("unknown field `plan`"));
}

#[test]
fn first_product_matrix_reduction_rejects_every_zero_inner_rank_pair_before_commit() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        assert_first_product_zero_inner_refusals(format);
    }
}

fn assert_first_product_zero_inner_refusals(format: SolveRealFormat) {
    let arithmetic = SolveArithmeticProfile::construct(
        format,
        crate::SolveIntegerDomain::FULL,
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real = SolveScalarType::real(arithmetic);
    let types = [
        (vec![0], vec![0]),
        (vec![2, 0], vec![0]),
        (vec![0], vec![0, 4]),
        (vec![2, 0], vec![0, 4]),
        (vec![0, 0], vec![0, 4]),
    ];
    let program = TypedProgram::construct(arithmetic, |builder| {
        for (case, (lhs_dimensions, rhs_dimensions)) in types.into_iter().enumerate() {
            let lhs_type = SolveValueType::tensor(real, lhs_dimensions).unwrap();
            let rhs_type = SolveValueType::tensor(real, rhs_dimensions).unwrap();
            let lhs = builder.declare_slot(
                lhs_type,
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(200 + case * 5),
            )?;
            let rhs = builder.declare_slot(
                rhs_type,
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(201 + case * 5),
            )?;
            let lhs = builder.load(lhs, span(202 + case * 5))?;
            let rhs = builder.load(rhs, span(203 + case * 5))?;
            let prefix_registers = builder.register_types.len();
            let prefix_operations = builder.operations.len();
            let provenance = span(204 + case * 5);
            let error = builder
                .matrix_multiply(lhs, rhs, provenance)
                .expect_err("FirstProduct cannot issue a plan over an empty inner domain");
            assert_eq!(
                error,
                SolveProgramConstructionError::EmptyFirstProductDomain { provenance }
            );
            assert_eq!(builder.register_types.len(), prefix_registers);
            assert_eq!(builder.operations.len(), prefix_operations);
        }
        Ok(())
    })
    .expect("each failed reduction leaves its valid input prefix intact");
    assert_eq!(program.operations().len(), 10);
}

#[test]
fn positive_zero_matrix_reduction_issues_a_zero_inner_plan_and_replay_rechecks_the_seed() {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        crate::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
    );
    let real = SolveScalarType::real(arithmetic);
    let empty = SolveValueType::tensor(real, vec![0]).unwrap();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let lhs = builder.declare_slot(
            empty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(230),
        )?;
        let rhs = builder.declare_slot(
            empty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(231),
        )?;
        let lhs = builder.load(lhs, span(232))?;
        let rhs = builder.load(rhs, span(233))?;
        builder.matrix_multiply(lhs, rhs, span(234))?;
        Ok(())
    })
    .expect("PositiveZero owns the identity for an empty inner domain");
    assert!(matches!(
        program.operations()[2].operation(),
        SolveOperation::MatrixMultiply { plan, .. }
            if plan.inner() == 0
                && matches!(plan.arithmetic(), SolveMatrixMultiplyArithmetic::Real {
                    accumulator: SolveRealFormat::Binary32,
                    semantics: RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
                    ..
                })
    ));

    let mut wire = serde_json::to_value(&program).unwrap();
    wire["arithmetic"]["real_matrix_multiply"] =
        serde_json::json!("separate_mul_add_ascending_first_product");
    let error = serde_json::from_value::<TypedProgram>(wire)
        .expect_err("wire replay must reissue the nonempty-domain proof for FirstProduct");
    assert!(
        error
            .to_string()
            .contains("first-product matrix reduction requires a nonempty inner domain"),
        "unexpected zero-inner replay error: {error}"
    );
}

#[test]
fn matrix_multiply_plan_owns_all_four_rank_layouts() {
    let arithmetic = profile();
    let real = SolveScalarType::real(arithmetic);
    let types = [
        (
            SolveValueType::tensor(real, vec![3]).unwrap(),
            SolveValueType::tensor(real, vec![3]).unwrap(),
        ),
        (
            SolveValueType::tensor(real, vec![2, 3]).unwrap(),
            SolveValueType::tensor(real, vec![3]).unwrap(),
        ),
        (
            SolveValueType::tensor(real, vec![3]).unwrap(),
            SolveValueType::tensor(real, vec![3, 4]).unwrap(),
        ),
        (
            SolveValueType::tensor(real, vec![2, 3]).unwrap(),
            SolveValueType::tensor(real, vec![3, 4]).unwrap(),
        ),
    ];
    let program = TypedProgram::construct(arithmetic, |builder| {
        for (case, (lhs, rhs)) in types.iter().enumerate() {
            let lhs = builder.declare_slot(
                lhs.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(30 + case * 4),
            )?;
            let rhs = builder.declare_slot(
                rhs.clone(),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(31 + case * 4),
            )?;
            let lhs = builder.load(lhs, span(32 + case * 4))?;
            let rhs = builder.load(rhs, span(33 + case * 4))?;
            builder.matrix_multiply(lhs, rhs, span(34 + case * 4))?;
        }
        Ok(())
    })
    .unwrap();

    let plans = program
        .operations()
        .iter()
        .filter_map(|operation| match operation.operation() {
            SolveOperation::MatrixMultiply { plan, .. } => Some(*plan),
            _ => None,
        })
        .collect::<Vec<_>>();
    let expected = [
        (
            (1, 3, 1),
            SolveMatrixOperandLayout::Vector,
            SolveMatrixOperandLayout::Vector,
            SolveMatrixResultLayout::Scalar,
        ),
        (
            (2, 3, 1),
            SolveMatrixOperandLayout::RowMajorMatrix,
            SolveMatrixOperandLayout::Vector,
            SolveMatrixResultLayout::Vector,
        ),
        (
            (1, 3, 4),
            SolveMatrixOperandLayout::Vector,
            SolveMatrixOperandLayout::RowMajorMatrix,
            SolveMatrixResultLayout::Vector,
        ),
        (
            (2, 3, 4),
            SolveMatrixOperandLayout::RowMajorMatrix,
            SolveMatrixOperandLayout::RowMajorMatrix,
            SolveMatrixResultLayout::RowMajorMatrix,
        ),
    ];
    assert_eq!(plans.len(), expected.len());
    for (plan, (dimensions, lhs, rhs, result)) in plans.iter().zip(expected) {
        assert_eq!((plan.rows(), plan.inner(), plan.columns()), dimensions);
        assert_eq!(plan.lhs_layout(), lhs);
        assert_eq!(plan.rhs_layout(), rhs);
        assert_eq!(plan.result_layout(), result);
    }

    let matrix = plans[3];
    assert_eq!(matrix.lhs_row_stride(), 3);
    assert_eq!(matrix.lhs_inner_stride(), 1);
    assert_eq!(matrix.rhs_inner_stride(), 4);
    assert_eq!(matrix.rhs_column_stride(), 1);
    assert_eq!(matrix.result_row_stride(), 4);
    assert_eq!(matrix.result_column_stride(), 1);
    assert_matrix_plan_arithmetic_contract(matrix);
}

fn assert_matrix_plan_arithmetic_contract(matrix: SolveMatrixMultiplyPlan) {
    assert!(matches!(
        matrix.arithmetic(),
        SolveMatrixMultiplyArithmetic::Real {
            accumulator: SolveRealFormat::Binary32,
            semantics: RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
            order: SolveMatrixMultiplyOrder::AscendingSharedAxis,
            primitive_rounding: SolveMatrixMultiplyRounding::RoundToNearestTiesToEven,
            contraction: SolveMatrixMultiplyContraction::SeparateMultiplyAdd,
            intermediate_precision:
                crate::SolveMatrixMultiplyIntermediatePrecision::AccumulatorFormatOnly,
            final_rounding: crate::SolveMatrixMultiplyFinalRounding::None,
            signed_zero: SolveMatrixMultiplySignedZero::IeeePrimitiveResult,
            nan: SolveMatrixMultiplyNan::QuietPayloadAndSignQuotient,
            infinity: SolveMatrixMultiplyInfinity::IeeePrimitiveResult,
            subnormal: SolveMatrixMultiplySubnormal::GradualUnderflow,
            status: SolveMatrixMultiplyStatus::NoObservableFloatingStatus,
        }
    ));
}

#[test]
fn million_row_matrix_product_keeps_constant_operation_and_wire_cardinality() {
    let arithmetic = profile();
    let real = SolveScalarType::real(arithmetic);
    let lhs = SolveValueType::tensor(real, vec![1_000_000, 3]).unwrap();
    let rhs = SolveValueType::tensor(real, vec![3]).unwrap();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let lhs = builder.declare_slot(
            lhs,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(60),
        )?;
        let rhs = builder.declare_slot(
            rhs,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(61),
        )?;
        let lhs = builder.load(lhs, span(62))?;
        let rhs = builder.load(rhs, span(63))?;
        builder.matrix_multiply(lhs, rhs, span(64))?;
        Ok(())
    })
    .unwrap();
    assert_eq!(program.operations().len(), 3);
    assert_eq!(
        program
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), SolveOperation::Load { .. }))
            .count(),
        2
    );
    assert!(matches!(
        program.operations()[2].operation(),
        SolveOperation::MatrixMultiply { plan, .. }
            if (plan.rows(), plan.inner(), plan.columns(), plan.output_count())
                == (1_000_000, 3, 1, 1_000_000)
    ));
    let wire = serde_json::to_string(&program).unwrap();
    assert!(
        wire.len() < 2_000,
        "wire metadata grew with tensor extent: {} bytes",
        wire.len()
    );
}

#[test]
fn unsupported_reductions_reject_before_operation_or_register_insertion() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let vector = builder.fill(one, vec![3], span(1))?;
        let prefix_registers = builder.register_types.len();
        let prefix_operations = builder.operations.len();

        let result = builder.cross(vector, vector, span(2));
        assert!(matches!(
            result,
            Err(SolveProgramConstructionError::MissingReductionContract { .. })
        ));
        assert_eq!(builder.register_types.len(), prefix_registers);
        assert_eq!(builder.operations.len(), prefix_operations);
        for (index, operator) in [
            SolveReductionOperator::Sum,
            SolveReductionOperator::Product,
            SolveReductionOperator::Minimum,
            SolveReductionOperator::Maximum,
        ]
        .into_iter()
        .enumerate()
        {
            let result = builder.reduce(operator, vector, span(3 + index));
            assert!(matches!(
                result,
                Err(SolveProgramConstructionError::MissingReductionContract { .. })
            ));
            assert_eq!(builder.register_types.len(), prefix_registers);
            assert_eq!(builder.operations.len(), prefix_operations);
        }
        let integer = builder.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(4))?;
        let integer = builder.fill(integer, vec![1], span(5))?;
        let prefix_registers = builder.register_types.len();
        let prefix_operations = builder.operations.len();
        assert_eq!(
            builder.matrix_multiply(integer, integer, span(6)),
            Err(SolveProgramConstructionError::MissingReductionContract {
                provenance: span(6)
            })
        );
        assert_eq!(builder.register_types.len(), prefix_registers);
        assert_eq!(builder.operations.len(), prefix_operations);
        Ok(())
    })
    .expect("unsupported reductions leave the valid compact prefix intact");
    assert_eq!(program.operations().len(), 4);
}

#[test]
fn sev_162_rank_zero_and_rank_n_values_share_one_binary_operation_leaf() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let scalar_lhs = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let scalar_rhs = builder.constant(SolveValue::real(arithmetic, 2.0), span(1))?;
        let scalar = builder.binary(SolveBinaryOperator::Add, scalar_lhs, scalar_rhs, span(2))?;
        let tensor_lhs = builder.fill(scalar_lhs, vec![2, 3], span(3))?;
        let tensor_rhs = builder.fill(scalar_rhs, vec![2, 3], span(4))?;
        let tensor = builder.binary(SolveBinaryOperator::Add, tensor_lhs, tensor_rhs, span(5))?;
        assert!(
            builder
                .register_type(scalar, span(6))?
                .dimensions()
                .is_empty()
        );
        assert_eq!(
            builder.register_type(tensor, span(7))?.dimensions(),
            &[2, 3]
        );
        Ok(())
    })
    .expect("one shape-polymorphic binary leaf admits rank zero and rank N");

    let binary_shapes = program
        .operations()
        .iter()
        .filter_map(|operation| match operation.operation() {
            SolveOperation::Binary { destination, .. } => {
                Some(program.register_types()[destination.index()].dimensions())
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(binary_shapes, [vec![].as_slice(), [2, 3].as_slice()]);
}

#[test]
fn compact_map_derives_tensor_shape_and_replays_its_checked_region() {
    let arithmetic = profile();
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "i".to_owned(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    let program = TypedProgram::construct(arithmetic, |builder| {
        let two = builder.constant(SolveValue::real(arithmetic, 2.0), span(20))?;
        let mapped = builder.map(
            domain,
            &[two],
            SolveValueType::scalar(SolveScalarType::real(arithmetic)),
            span(21),
            |builder, captures, binders, output| {
                let capture = builder.load(captures[0], span(22))?;
                let binder = builder.load(binders[0], span(23))?;
                let binder =
                    builder.convert(SolveConversionOperator::IntegerToReal, binder, span(24))?;
                let value =
                    builder.binary(SolveBinaryOperator::Multiply, capture, binder, span(25))?;
                builder.store(output, value, span(26))
            },
        )?;
        assert_eq!(builder.register_type(mapped, span(27))?.dimensions(), &[3]);
        Ok(())
    })
    .expect("checked compact map constructs");
    assert_eq!(program.operations().len(), 2);
    assert!(matches!(
        program.operations()[1].operation(),
        SolveOperation::Map { domain, body, .. }
            if domain.binders.len() == 1 && body.outputs().len() == 1
    ));
    let replayed: TypedProgram = serde_json::from_str(&serde_json::to_string(&program).unwrap())
        .expect("wire replay reconstructs the compact map");
    assert_eq!(replayed, program);

    let mut forged_region = serde_json::to_value(&program).unwrap();
    forged_region["operations"][1]["operation"]["body"]["future_policy"] = serde_json::json!(true);
    let error = serde_json::from_value::<TypedProgram>(forged_region)
        .expect_err("a structured region cannot ignore unknown wire authority");
    assert!(error.to_string().contains("unknown field `future_policy`"));
}

#[test]
fn invalid_map_domain_commits_no_destination_or_operation() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(30))?;
        let invalid = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_owned(),
                lower: 1,
                upper: 3,
                step: 0,
            }],
        };
        assert_eq!(
            builder.map(
                invalid,
                &[one],
                SolveValueType::scalar(SolveScalarType::real(arithmetic)),
                span(31),
                |_, _, _, _| Ok(()),
            ),
            Err(SolveProgramConstructionError::InvalidMap {
                provenance: span(31)
            })
        );
        assert_eq!(builder.register_types.len(), 1);
        assert_eq!(builder.operations.len(), 1);
        Ok(())
    })
    .expect("invalid map leaves its valid prefix intact");
    assert_eq!(program.operations().len(), 1);
}

#[test]
fn scalar_broadcast_binary_retains_one_tensor_operation_and_replays() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(40))?;
        let two = builder.constant(SolveValue::real(arithmetic, 2.0), span(41))?;
        let vector = builder.fill(one, vec![3], span(42))?;
        let powered =
            builder.broadcast_binary(SolveBinaryOperator::Power, vector, two, false, span(43))?;
        assert_eq!(builder.register_type(powered, span(44))?.dimensions(), &[3]);
        Ok(())
    })
    .expect("checked scalar broadcast constructs");
    assert_eq!(program.operations().len(), 4);
    assert!(matches!(
        program.operations()[3].operation(),
        SolveOperation::BroadcastBinary {
            operator: SolveBinaryOperator::Power,
            scalar_on_lhs: false,
            ..
        }
    ));
    let replayed: TypedProgram = serde_json::from_str(&serde_json::to_string(&program).unwrap())
        .expect("wire replay reconstructs scalar broadcasting");
    assert_eq!(replayed, program);
}

#[test]
fn tensor_algebra_rejects_rank_and_inner_extent_mismatches_before_commit() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let lhs = builder.fill(one, vec![2, 3], span(1))?;
        let rhs = builder.fill(one, vec![4, 2], span(2))?;
        assert_eq!(
            builder.matrix_multiply(lhs, rhs, span(3)),
            Err(SolveProgramConstructionError::InvalidTensorAlgebra {
                provenance: span(3)
            })
        );
        assert_eq!(builder.register_types.len(), 3);
        assert_eq!(builder.operations.len(), 3);
        Ok(())
    })
    .expect("failed tensor algebra leaves its valid prefix intact");
    assert_eq!(program.operations().len(), 3);
}

#[test]
fn single_operand_concatenation_preserves_its_promoted_matrix_shape() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let matrix = builder.concatenate(1, &[one], span(1))?;
        assert_eq!(
            builder.register_type(matrix, span(2))?.dimensions(),
            &[1, 1]
        );
        Ok(())
    })
    .expect("one nonempty concatenation operand constructs a promoted matrix");
    assert!(matches!(
        program.operations()[1].operation(),
        SolveOperation::Concatenate { axis: 1, operands, .. } if operands.len() == 1
    ));
    let replayed: TypedProgram = serde_json::from_str(&serde_json::to_string(&program).unwrap())
        .expect("wire replay reconstructs single-operand concatenation");
    assert_eq!(replayed, program);
}

#[test]
fn vector_concatenation_appends_unit_extents_during_promotion() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let lhs = builder.fill(one, vec![2], span(1))?;
        let rhs = builder.fill(one, vec![2], span(2))?;
        let columns = builder.concatenate(1, &[lhs, rhs], span(3))?;
        assert_eq!(
            builder.register_type(columns, span(4))?.dimensions(),
            &[2, 2]
        );

        let longer = builder.fill(one, vec![3], span(5))?;
        let rows = builder.concatenate(0, &[lhs, longer], span(6))?;
        assert_eq!(builder.register_type(rows, span(7))?.dimensions(), &[5, 1]);
        Ok(())
    })
    .expect("vector operands promote by appending unit extents");

    let replayed: TypedProgram = serde_json::from_str(&serde_json::to_string(&program).unwrap())
        .expect("wire replay reconstructs promoted vector concatenation");
    assert_eq!(replayed, program);
}

#[test]
fn invalid_aggregate_projection_fails_before_register_or_operation_commit() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let value = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let aggregate = builder.fill(value, vec![2, 2], span(1))?;
        assert_eq!(
            builder.project_element(aggregate, vec![2, 0], span(2)),
            Err(SolveProgramConstructionError::InvalidProjection {
                provenance: span(2)
            })
        );
        assert_eq!(
            builder.project_slice(aggregate, vec![1, 1], vec![2, 1], span(3)),
            Err(SolveProgramConstructionError::InvalidProjection {
                provenance: span(3)
            })
        );
        assert_eq!(builder.register_types.len(), 2);
        assert_eq!(builder.operations.len(), 2);
        Ok(())
    })
    .expect("failed projections leave the valid prefix intact");
    assert_eq!(program.operations().len(), 2);
}

#[test]
fn conversions_preserve_shape_and_reject_same_kind_coercion() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let integer = builder.constant(
            SolveValue::integer(arithmetic, 2).expect("value fits the checked domain"),
            span(0),
        )?;
        let integers = builder.fill(integer, vec![3], span(1))?;
        let reals = builder.convert(SolveConversionOperator::IntegerToReal, integers, span(2))?;
        assert_eq!(builder.register_type(reals, span(3))?.dimensions(), &[3]);
        assert_eq!(
            builder.convert(SolveConversionOperator::IntegerToReal, reals, span(4),),
            Err(SolveProgramConstructionError::TypeMismatch {
                provenance: span(4)
            })
        );
        Ok(())
    })
    .expect("explicit integer-to-real conversion constructs");
    assert!(matches!(
        program.register_types()[2].element_type(),
        SolveScalarType::Real { .. }
    ));
}

#[test]
fn aggregate_elements_are_explicitly_widened_to_the_checked_result_type() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let integer = builder.constant(
            SolveValue::integer(arithmetic, 0).expect("zero fits the checked domain"),
            span(0),
        )?;
        let real = builder.constant(SolveValue::real(arithmetic, 1.0), span(1))?;
        let real_type = SolveValueType::scalar(SolveScalarType::real(arithmetic));
        let widened = builder.coerce_to(integer, &real_type, span(2))?;
        let vector = builder.construct_aggregate(&[widened, real], vec![2], span(3))?;
        assert_eq!(builder.register_type(vector, span(4))?.dimensions(), [2]);
        Ok(())
    })
    .expect("checked integer-to-real array promotion emits an explicit conversion");

    assert!(matches!(
        program.operations()[2].operation(),
        SolveOperation::Convert {
            operator: SolveConversionOperator::IntegerToReal,
            ..
        }
    ));
}

#[test]
fn conditional_requires_every_region_to_define_the_complete_result_tuple() {
    let arithmetic = profile();
    let error = TypedProgram::construct(arithmetic, |builder| {
        let condition = builder.constant(SolveValue::boolean(true), span(0))?;
        let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
        builder.conditional(
            condition,
            &[],
            vec![real],
            span(1),
            |region, _inputs, outputs| {
                let value = region.constant(SolveValue::real(arithmetic, 1.0), span(2))?;
                region.store(outputs[0], value, span(3))
            },
            |_region, _inputs, _outputs| Ok(()),
        )?;
        Ok(())
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::InvalidRegion {
            provenance: span(1)
        }
    );
}

#[test]
fn fold_rejects_an_invalid_domain_before_building_its_transition() {
    let arithmetic = profile();
    let error = TypedProgram::construct(arithmetic, |builder| {
        let initial = builder.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(0))?;
        builder.fold(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".into(),
                    lower: 1,
                    upper: 3,
                    step: 0,
                }],
            },
            &[initial],
            &[],
            span(1),
            |_region, _carried, _captures, _binders, _outputs| {
                panic!("an invalid domain must fail before transition construction")
            },
        )?;
        Ok(())
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::InvalidFold {
            provenance: span(1)
        }
    );
}

#[test]
fn real_intrinsics_preserve_one_aggregate_owner_and_wire_replay() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let value = builder.constant(SolveValue::real(arithmetic, 0.5), span(0))?;
        let vector = builder.fill(value, vec![16], span(1))?;
        let sine = builder.unary(SolveUnaryOperator::Sin, vector, span(2))?;
        let power = builder.binary(SolveBinaryOperator::Power, sine, vector, span(3))?;
        assert_eq!(builder.register_type(power, span(4))?.dimensions(), &[16]);
        Ok(())
    })
    .expect("shape-preserving Real intrinsics construct");
    assert_eq!(program.operations().len(), 4);
    assert!(matches!(
        program.operations()[2].operation(),
        SolveOperation::Unary {
            operator: SolveUnaryOperator::Sin,
            ..
        }
    ));
    let json = serde_json::to_string(&program).expect("intrinsic program serializes");
    let replayed: TypedProgram =
        serde_json::from_str(&json).expect("intrinsic program replays through checked builders");
    assert_eq!(replayed, program);
}

#[test]
fn integer_divide_cannot_bypass_an_explicit_real_conversion() {
    let arithmetic = profile();
    TypedProgram::construct(arithmetic, |builder| {
        let integer = builder.constant(
            SolveValue::integer(arithmetic, 2).expect("integer belongs to profile"),
            span(0),
        )?;
        assert_eq!(
            builder.binary(SolveBinaryOperator::Divide, integer, integer, span(1)),
            Err(SolveProgramConstructionError::TypeMismatch {
                provenance: span(1)
            })
        );
        assert_eq!(builder.operations.len(), 1);
        Ok(())
    })
    .expect("rejected division leaves the valid typed prefix intact");
}

#[test]
fn dummy_provenance_never_commits_a_typed_owner() {
    let arithmetic = profile();
    let error = TypedProgram::construct(arithmetic, |builder| {
        builder.constant(SolveValue::real(arithmetic, 1.0), Span::DUMMY)?;
        Ok(())
    })
    .expect_err("dummy provenance must fail checked construction");
    assert_eq!(error, SolveProgramConstructionError::MissingProvenance);
}

#[test]
fn wire_roundtrip_replays_checked_aggregate_operations() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let value = builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        let aggregate = builder.fill(value, vec![2, 2], span(1))?;
        builder.project_element(aggregate, vec![1, 1], span(2))?;
        Ok(())
    })
    .expect("fixture constructs");
    let json = serde_json::to_string(&program).expect("typed program serializes");
    let decoded: TypedProgram = serde_json::from_str(&json).expect("checked wire replays");
    assert_eq!(decoded, program);
}

#[test]
fn wire_rejects_forged_register_dominance() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        builder.constant(SolveValue::real(arithmetic, 1.0), span(0))?;
        Ok(())
    })
    .expect("fixture constructs");
    let mut json = serde_json::to_value(&program).expect("typed program serializes");
    json["operations"][0]["operation"]["destination"] = serde_json::json!(1);
    let error = serde_json::from_value::<TypedProgram>(json)
        .expect_err("wire cannot forge a forward register definition");
    assert!(error.to_string().contains("does not replay"), "{error}");
}
