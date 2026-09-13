use super::*;
use crate::{BinaryOp, LinearOp, SolveUnaryOperator, TargetAssignmentShape, TensorInputKind};

fn real_shape(dimensions: Vec<u32>) -> SolveValueType {
    if dimensions.is_empty() {
        SolveValueType::scalar(SolveScalarType::real(profile()))
    } else {
        SolveValueType::tensor(SolveScalarType::real(profile()), dimensions).unwrap()
    }
}

fn nested_copy(width: u32) -> SolvePureCallTable {
    let vector = real_shape(vec![width]);
    SolvePureCallTable::construct(profile(), |table| {
        let inner = add_passthrough_owner(table, identity(1), &vector, span(0))?;
        table.add_owner(
            identity(2),
            vec![vector.clone(), vector.clone()],
            vec![SolvePureCallOutput::result(vector)],
            span(1),
            |builder, inputs, outputs| {
                let unused = builder.load(inputs[0], span(2))?;
                let selected = builder.load(inputs[1], span(3))?;
                let result = builder.call(inner, &[selected], span(4))?;
                let _ = unused;
                builder.store(outputs[0], result[0], span(5))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn nested_copies_preserve_input_and_tangent_coordinate_identity() {
    let table = nested_copy(3);
    let site = table.owners()[1].call_site();
    for index in 0..3 {
        assert_eq!(site.projected_input_coordinate(index), Some((1, index)));
        let directional = site.directional().unwrap();
        assert_eq!(
            directional.projected_input_coordinate(index),
            Some((2, index))
        );
        assert_eq!(
            directional.projected_input_coordinate(index + 3),
            Some((3, index))
        );
    }
    assert_eq!(site.projected_input_coordinate(3), None);
    let large = nested_copy(1_000_000);
    let large = large.owners()[1].call_site();
    assert_eq!(site.projections, large.projections);
    assert_eq!(
        site.directional().unwrap().projections,
        large.directional().unwrap().projections
    );
}

#[test]
fn static_tensor_projections_compose_through_nested_calls() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        let inner = table.add_owner(
            identity(1),
            vec![real_shape(vec![3, 2])],
            vec![SolvePureCallOutput::result(real_shape(vec![2, 3]))],
            span(0),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1))?;
                let value = builder.transpose(input, span(2))?;
                builder.store(outputs[0], value, span(3))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![real_shape(vec![2, 3])],
            vec![SolvePureCallOutput::result(real_shape(vec![2, 2]))],
            span(4),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(5))?;
                let transposed = builder.transpose(input, span(6))?;
                let result = builder.call(inner, &[transposed], span(7))?;
                let value = builder.project_slice(result[0], vec![0, 1], vec![2, 2], span(8))?;
                builder.store(outputs[0], value, span(9))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[1].call_site();
    for (output, input) in [1, 2, 4, 5].into_iter().enumerate() {
        assert_eq!(site.projected_input_coordinate(output), Some((0, input)));
        assert_eq!(
            site.directional()
                .unwrap()
                .projected_input_coordinate(output + 4),
            Some((1, input))
        );
    }
}

#[test]
fn filling_a_projected_element_preserves_the_selected_coordinate() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            vec![real_shape(vec![2, 3])],
            vec![SolvePureCallOutput::result(real_shape(vec![2, 2]))],
            span(0),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1))?;
                let element = builder.project_element(input, vec![1, 2], span(2))?;
                let value = builder.fill(element, vec![2, 2], span(3))?;
                builder.store(outputs[0], value, span(4))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    for output in 0..4 {
        assert_eq!(site.projected_input_coordinate(output), Some((0, 5)));
    }
}

fn copy_with_additional_operation(case: u8) -> SolvePureCallTable {
    let scalar = real_shape(vec![]);
    let mut outputs = vec![SolvePureCallOutput::result(scalar.clone())];
    if case == 3 {
        outputs.push(SolvePureCallOutput::assertion_predicate());
    }
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            vec![scalar],
            outputs,
            span(0),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1))?;
                match case {
                    0 => {
                        builder.constant(SolveValue::real(profile(), 2.0), span(2))?;
                    }
                    1 => {
                        builder.unary(SolveUnaryOperator::Log, input, span(2))?;
                    }
                    2 => {
                        builder.constant(SolveValue::real(profile(), f64::NAN), span(2))?;
                    }
                    3 => {
                        let predicate = builder.constant(SolveValue::boolean(false), span(2))?;
                        builder.store(outputs[1], predicate, span(3))?;
                    }
                    _ => unreachable!(),
                }
                builder.store(outputs[0], input, span(4))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn an_input_dependency_is_not_a_value_projection_proof() {
    for case in 0..4 {
        let table = copy_with_additional_operation(case);
        let site = table.owners()[0].call_site();
        assert_eq!(site.output_dependencies()[0][0].input_index(), 0);
        assert_eq!(
            site.projected_input_coordinate(0),
            (case == 0).then_some((0, 0))
        );
    }
}

#[test]
fn value_projection_wire_claims_are_bound_to_reconstructed_owners() {
    let table = nested_copy(3);
    let site = table.owners()[1].call_site();
    let original = serde_json::to_value(&site).unwrap();
    let replayed: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&table).unwrap()).unwrap();
    assert!(replayed.matches_site(&site));
    for path in ["/projections", "/directional/projections"] {
        let mut absent = original.clone();
        *absent.pointer_mut(path).unwrap() = serde_json::Value::Null;
        let mut wrong_input = original.clone();
        wrong_input.pointer_mut(path).unwrap()[0]["input"] = serde_json::json!(99);
        let mut wrong_map = original.clone();
        wrong_map.pointer_mut(path).unwrap()[0]["coordinates"]["subscripts"][0]["coeffs"][0] =
            serde_json::json!(0);
        for wire in [absent, wrong_input, wrong_map] {
            let forged: SolvePureCallSite = serde_json::from_value(wire).unwrap();
            assert!(!replayed.matches_site(&forged));
            if path.starts_with("/directional") {
                assert!(!replayed.matches_directional_site(forged.directional().unwrap()));
            }
        }
    }
}

#[test]
fn directional_site_identity_cannot_be_replaced_by_an_identical_owner() {
    let vector = vector_type();
    let table = SolvePureCallTable::construct(profile(), |table| {
        add_passthrough_owner(table, identity(1), &vector, span(0))?;
        add_passthrough_owner(table, identity(2), &vector, span(1))?;
        Ok(())
    })
    .unwrap();
    let mut wire = serde_json::to_value(table.owners()[0].call_site()).unwrap();
    wire["directional"]["owner"] = serde_json::json!(1);
    let forged: SolvePureCallSite = serde_json::from_value(wire).unwrap();
    assert!(!table.matches_site(&forged));
}

fn inverse_copy_program() -> Vec<LinearOp> {
    let table = nested_copy(3);
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 3,
            input: TensorInputKind::Y,
            input_start: 3,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::PureCall {
            dst_start: 6,
            input_starts: vec![0, 3].into(),
            site: table.owners()[1].call_site(),
        },
        LinearOp::TensorBinary {
            dst_start: 9,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: 6,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 9,
            count: 3,
            stride: 1,
        },
    ]
}

#[test]
fn isolating_a_forwarded_target_retains_the_call_evaluation_prefix() {
    let program = inverse_copy_program();
    for output in 0..3 {
        let shape = crate::derive_target_assignment_shape_for_output(&program, output, output + 3);
        assert!(matches!(shape, Some(TargetAssignmentShape::Direct {
            target_y_index, expr_reg, expr_eval_len: 3, ..
        }) if target_y_index == output + 3 && expr_reg as usize == output));
    }
}

#[test]
fn zero_residual_through_total_copy_retains_exact_target_and_call_prefix() {
    let mut program = inverse_copy_program();
    program.truncate(3);
    program.push(LinearOp::StoreOutputRange {
        start: 6,
        count: 3,
        stride: 1,
    });
    for output in 0..3 {
        let shape = crate::derive_target_assignment_shape_for_output(&program, output, output + 3);
        assert!(matches!(shape, Some(TargetAssignmentShape::Zero {
            target_y_index, expr_eval_len: 3,
        }) if target_y_index == output + 3));
        assert!(
            crate::derive_target_assignment_shape_for_output(&program, output, output).is_none()
        );
    }
}

#[test]
fn affine_forwarding_requires_an_independent_coefficient() {
    for coefficient in [1, 6] {
        let mut program = inverse_copy_program();
        program.truncate(3);
        program.extend([
            LinearOp::Binary {
                dst: 9,
                op: BinaryOp::Mul,
                lhs: 6,
                rhs: coefficient,
            },
            LinearOp::Binary {
                dst: 10,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 9,
            },
            LinearOp::StoreOutput { src: 10 },
        ]);
        let shape = crate::derive_target_assignment_shape_for_output(&program, 0, 3);
        if coefficient == 1 {
            assert!(matches!(
                shape,
                Some(TargetAssignmentShape::Affine {
                    target_y_index: 3,
                    coefficient_reg: Some(1),
                    expr_eval_len: 3,
                    ..
                })
            ));
        } else {
            assert!(shape.is_none(), "a squared input has no affine isolator");
        }
    }
}
