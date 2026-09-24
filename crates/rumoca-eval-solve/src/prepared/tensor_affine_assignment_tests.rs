use super::*;
use rumoca_ir_solve::TensorInputKind;

fn force_moment_program() -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::P,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 3,
            input: TensorInputKind::Y,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorCross {
            dst_start: 6,
            lhs_start: 0,
            rhs_start: 3,
            lanes: 1,
        },
        LinearOp::LoadY { dst: 9, index: 3 },
        LinearOp::Binary {
            dst: 10,
            op: BinaryOp::Sub,
            lhs: 9,
            rhs: 8,
        },
        LinearOp::StoreOutput { src: 10 },
    ]
}

fn prepare(program: Vec<LinearOp>) -> PreparedScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("force_moment.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(
            vec![program],
            span.require_provenance("force moment equation").unwrap(),
        )
        .unwrap(),
    )
    .unwrap()
}

#[test]
fn tensor_cross_force_assignment_preserves_independent_offset() {
    // t[3] - cross(r, f)[3] = 0 gives f[2] = (t[3] + r[2]*f[1])/r[1].
    let prepared = prepare(force_moment_program());
    for guess in [1e30, -1e30, 0.0] {
        assert_eq!(
            prepared
                .eval_target_assignment_row_with_context(
                    0,
                    1,
                    &[4.0, guess, 7.0, 18.0],
                    &[2.0, 3.0, 5.0],
                    0.0,
                    RowEvalContext::default(),
                )
                .unwrap(),
            Some(15.0),
        );
    }
}

#[test]
fn tensor_affine_materialization_preserves_output_identity_and_prefix_errors() {
    let mut program = force_moment_program();
    program.insert(1, LinearOp::StoreOutput { src: 0 });
    program.insert(0, LinearOp::LoadP { dst: 11, index: 3 });
    let prepared = prepare(program.clone());
    let shape = prepared.assignment_shape_for_output(0, 1, 1).unwrap();
    assert!(matches!(shape, TargetAssignmentShape::TensorAffine { .. }));
    assert_eq!(shape.expr_eval_len(), program.len() - 1);
    assert!(!assignment_shape_reads_y_index(&program, shape, 1));
    assert!(!assignment_shape_reads_y_index(&program, shape, 2));
    assert!(
        rumoca_ir_solve::ScalarProgramYDependency::new(&[]).assignment_depends_on(shape, 2),
        "unknown dependency must remain conservative"
    );
    assert!(assignment_shape_reads_y_index(&program, shape, 0));
    assert!(assignment_shape_reads_y_index(&program, shape, 3));
    let materialized = prepare(
        prepared
            .exact_target_assignment_output_program(0, 1, 1)
            .unwrap(),
    );
    let y = [4.0, 1e30, 7.0, 18.0];
    for p in [&[2.0, 3.0, 5.0][..], &[2.0, 3.0, 5.0, 9.0][..]] {
        let explicit = materialized.eval_row_with_context(0, &y, p, 0.0, RowEvalContext::default());
        if p.len() == 3 {
            assert!(explicit.is_err());
            continue;
        }
        let implicit = prepared.eval_target_assignment_output_unchecked_with_context(
            TargetAssignmentOutputRequest {
                row_idx: 0,
                output_offset: 1,
                target_y_index: 1,
                y: &y,
                p,
                t: 0.0,
                context: RowEvalContext::default(),
            },
        );
        assert_eq!(explicit.unwrap(), 15.0);
        assert_eq!(implicit.unwrap(), Some(15.0));
    }
    let mut single_output = force_moment_program();
    single_output.insert(0, LinearOp::LoadP { dst: 11, index: 3 });
    assert!(
        prepare(single_output)
            .eval_target_assignment_row_with_context(
                0,
                1,
                &y,
                &[2.0, 3.0, 5.0],
                0.0,
                RowEvalContext::default(),
            )
            .is_err()
    );
}

#[test]
fn tensor_affine_zero_and_nonfinite_coefficients_decline_isolation() {
    for input in [TensorInputKind::P, TensorInputKind::Y] {
        let mut program = force_moment_program();
        if let LinearOp::TensorLoad {
            input: kind,
            input_start,
            ..
        } = &mut program[0]
        {
            *kind = input;
            *input_start = if input == TensorInputKind::Y { 4 } else { 0 };
        }
        let prepared = prepare(program);
        for coefficient in [0.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
            let result = prepared.eval_target_assignment_row_with_context(
                0,
                1,
                &[4.0, 1.0, 7.0, 18.0, coefficient, 3.0, 5.0],
                &[coefficient, 3.0, 5.0],
                0.0,
                RowEvalContext::default(),
            );
            assert!(
                matches!(
                    result,
                    Err(EvalSolveError::SingularTargetAssignment {
                        target_y_index: 1,
                        ..
                    })
                ),
                "{result:?}"
            );
        }
    }
}

#[test]
fn tensor_affine_refuses_products_with_two_target_dependent_operands() {
    let mut program = force_moment_program();
    program[0] = LinearOp::TensorLoad {
        dst_start: 0,
        input: TensorInputKind::Y,
        input_start: 0,
        seed_start: None,
        count: 3,
        lanes: 1,
    };
    assert!(rumoca_ir_solve::derive_target_assignment_shape_for_output(&program, 0, 1).is_none());
}

#[test]
fn rectangular_matrix_assignments_use_the_selected_output_and_input_lanes() {
    let program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::P,
            input_start: 0,
            seed_start: None,
            count: 6,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 6,
            input: TensorInputKind::Y,
            input_start: 0,
            seed_start: None,
            count: 6,
            lanes: 1,
        },
        LinearOp::MatrixMultiply {
            dst_start: 12,
            lhs_start: 0,
            rhs_start: 6,
            rows: 2,
            inner: 3,
            columns: 2,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 16,
            input: TensorInputKind::Y,
            input_start: 6,
            seed_start: None,
            count: 4,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 20,
            op: BinaryOp::Sub,
            lhs_start: 16,
            rhs_start: 12,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 20,
            count: 4,
            stride: 1,
        },
    ];
    let prepared = prepare(program);
    let p = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0];
    for (output, target) in [
        (0, 0),
        (0, 2),
        (0, 4),
        (1, 1),
        (1, 3),
        (1, 5),
        (2, 2),
        (3, 3),
    ] {
        let explicit = prepare(
            prepared
                .exact_target_assignment_output_program(0, output, target)
                .unwrap(),
        );
        let mut y = [4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 40.0, 46.0, 94.0, 109.0];
        let expected = y[target];
        y[target] = 1e30;
        let result = explicit
            .eval_row_with_context(0, &y, &p, 0.0, RowEvalContext::default())
            .unwrap();
        assert_eq!(result, expected, "output {output}, target {target}");
        let result = prepared
            .eval_target_assignment_output_unchecked_with_context(TargetAssignmentOutputRequest {
                row_idx: 0,
                output_offset: output,
                target_y_index: target,
                y: &y,
                p: &p,
                t: 0.0,
                context: RowEvalContext::default(),
            })
            .unwrap();
        assert_eq!(result, Some(expected));
    }
}

#[test]
fn tensor_cross_left_operand_preserves_cross_product_sign() {
    let mut program = force_moment_program();
    program[2] = LinearOp::TensorCross {
        dst_start: 6,
        lhs_start: 3,
        rhs_start: 0,
        lanes: 1,
    };
    let value = prepare(program)
        .eval_target_assignment_row_with_context(
            0,
            1,
            &[4.0, 1e30, 7.0, -18.0],
            &[2.0, 3.0, 5.0],
            0.0,
            RowEvalContext::default(),
        )
        .unwrap();
    assert_eq!(value, Some(15.0));
}

#[test]
fn affine_projection_handles_nested_scalar_products_and_independent_division() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::LoadP { dst: 3, index: 0 },
        LinearOp::LoadP { dst: 4, index: 1 },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 3,
            rhs: 5,
        },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Div,
            lhs: 6,
            rhs: 4,
        },
        LinearOp::Binary {
            dst: 8,
            op: BinaryOp::Sub,
            lhs: 7,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 8 },
    ];
    let prepared = prepare(program.clone());
    let y = [1e30, 4.0, 6.5];
    assert_eq!(
        prepared
            .eval_target_assignment_row_with_context(
                0,
                0,
                &y,
                &[2.0, 4.0],
                0.0,
                RowEvalContext::default(),
            )
            .unwrap(),
        Some(9.0)
    );
    let mut nonlinear = program;
    nonlinear[4] = LinearOp::LoadY { dst: 4, index: 0 };
    assert!(rumoca_ir_solve::derive_target_assignment_shape_for_output(&nonlinear, 0, 0).is_none());
}

#[test]
fn tensor_affine_certificate_and_materialization_remain_compact_as_extents_grow() {
    let mut sizes = Vec::new();
    for count in [16, 16_384] {
        let n = count as u32;
        let program = vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::P,
                input_start: 0,
                seed_start: None,
                count,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: n,
                input: TensorInputKind::Y,
                input_start: 0,
                seed_start: None,
                count,
                lanes: 1,
            },
            LinearOp::MatrixMultiply {
                dst_start: 2 * n,
                lhs_start: 0,
                rhs_start: n,
                rows: 1,
                inner: count,
                columns: 1,
                lanes: 1,
            },
            LinearOp::LoadY {
                dst: 2 * n + 1,
                index: count,
            },
            LinearOp::Binary {
                dst: 2 * n + 2,
                op: BinaryOp::Sub,
                lhs: 2 * n + 1,
                rhs: 2 * n,
            },
            LinearOp::StoreOutput { src: 2 * n + 2 },
        ];
        let shape =
            rumoca_ir_solve::derive_target_assignment_shape_for_output(&program, 0, count / 2)
                .unwrap();
        assert!(matches!(shape, TargetAssignmentShape::TensorAffine { .. }));
        let wire = serde_json::to_string(&shape).unwrap();
        assert!(wire.len() < 600, "{wire}");
        let mut materialized = program[..program.len() - 1].to_vec();
        rumoca_ir_solve::materialize_target_assignment(&shape, &mut materialized).unwrap();
        sizes.push(materialized.len());
    }
    assert_eq!(sizes[0], sizes[1]);
}

#[test]
fn materialized_affine_projection_declines_an_overflowed_coefficient() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadP { dst: 2, index: 0 },
        LinearOp::LoadP { dst: 3, index: 1 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Add,
            lhs: 4,
            rhs: 5,
        },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Add,
            lhs: 6,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 7 },
    ];
    let prepared = prepare(program);
    let y = [1.0, 4.0];
    let p = [1e308, 1e308];
    assert!(matches!(
        prepared.eval_target_assignment_row_with_context(
            0,
            0,
            &y,
            &p,
            0.0,
            RowEvalContext::default(),
        ),
        Err(EvalSolveError::SingularTargetAssignment { .. })
    ));
    let explicit = prepare(
        prepared
            .exact_target_assignment_output_program(0, 0, 0)
            .unwrap(),
    );
    let result = explicit
        .eval_row_with_context(0, &y, &p, 0.0, RowEvalContext::default())
        .unwrap();
    assert!(
        !result.is_finite(),
        "overflowed coefficient must decline, got {result}"
    );
}

#[test]
fn tensor_lane_dependency_retains_coefficient_and_branch_guard() {
    let program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::LoadY { dst: 3, index: 3 },
        LinearOp::Const { dst: 4, value: 0.0 },
        LinearOp::Compare {
            dst: 5,
            op: rumoca_ir_solve::CompareOp::Gt,
            lhs: 3,
            rhs: 4,
        },
        LinearOp::LoadY { dst: 6, index: 4 },
        LinearOp::LoadY { dst: 7, index: 5 },
        LinearOp::Select {
            dst: 8,
            cond: 5,
            if_true: 6,
            if_false: 7,
        },
        LinearOp::TensorBinary {
            dst_start: 9,
            op: BinaryOp::Mul,
            lhs_start: 0,
            rhs_start: 8,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 1,
        },
        LinearOp::Const {
            dst: 12,
            value: 1.0,
        },
        LinearOp::TensorBinary {
            dst_start: 13,
            op: BinaryOp::Sub,
            lhs_start: 9,
            rhs_start: 12,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 13,
            count: 3,
            stride: 1,
        },
    ];
    let prepared = prepare(program.clone());
    let shape = prepared.assignment_shape_for_output(0, 1, 1).unwrap();
    assert!(matches!(shape, TargetAssignmentShape::TensorAffine { .. }));
    for (index, expected) in [
        (0, false),
        (1, false),
        (2, false),
        (3, true),
        (4, true),
        (5, true),
    ] {
        assert_eq!(
            assignment_shape_reads_y_index(&program, shape, index),
            expected,
            "Y{index}"
        );
    }
    for (branch, expected) in [(1.0, 0.5), (-1.0, 0.25)] {
        let actual = prepared
            .eval_target_assignment_output_unchecked_with_context(TargetAssignmentOutputRequest {
                row_idx: 0,
                output_offset: 1,
                target_y_index: 1,
                y: &[f64::NAN, 99.0, f64::NAN, branch, 2.0, 4.0],
                p: &[],
                t: 0.0,
                context: RowEvalContext::default(),
            })
            .unwrap();
        assert_eq!(actual, Some(expected));
    }
}
