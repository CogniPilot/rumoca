use super::*;
use crate::{SolveRealFormat, SolveRoundingMode, SolveTypeConstructionError};
use rumoca_core::{SourceId, StructuredIndexBinder, StructuredIndexDomain};

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
        SolveRoundingMode::NearestTiesToEven,
        crate::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
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
    assert_eq!(value.real_as_f64(), Some(1.0));
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
                .binary(SolveBinaryOperator::Add, integer, integer, span(3))
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
        let cross = builder.cross(vector, vector, span(6))?;
        let sum = builder.reduce(SolveReductionOperator::Sum, product, span(7))?;
        let identity = builder.identity(SolveScalarType::real(arithmetic), 3, span(8))?;
        let diagonal = builder.diagonal(vector, span(9))?;
        assert_eq!(
            builder.register_type(transposed, span(8))?.dimensions(),
            &[3, 2]
        );
        assert_eq!(builder.register_type(product, span(9))?.dimensions(), &[2]);
        assert_eq!(builder.register_type(cross, span(9))?.dimensions(), &[3]);
        assert!(
            builder
                .register_type(sum, span(10))?
                .dimensions()
                .is_empty()
        );
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
    assert_eq!(program.operations().len(), 10);
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
        SolveOperation::MatrixMultiply { .. }
    ));
    assert!(matches!(
        program.operations()[6].operation(),
        SolveOperation::Cross { .. }
    ));
    assert!(matches!(
        program.operations()[7].operation(),
        SolveOperation::Reduce { .. }
    ));
    assert!(matches!(
        program.operations()[8].operation(),
        SolveOperation::Identity { .. }
    ));
    assert!(matches!(
        program.operations()[9].operation(),
        SolveOperation::Diagonal { .. }
    ));
    let round_trip: TypedProgram = serde_json::from_str(&serde_json::to_string(&program).unwrap())
        .expect("wire replay retains compact tensor algebra");
    assert_eq!(round_trip, program);
}

#[test]
fn compact_map_derives_tensor_shape_and_replays_its_checked_region() {
    let arithmetic = profile();
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
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
}

#[test]
fn invalid_map_domain_commits_no_destination_or_operation() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(30))?;
        let invalid = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
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
                    id: 0,
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
