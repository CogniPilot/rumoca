use super::*;
use rumoca_ir_solve::{BinaryOp, LinearOp};

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("eval_solve_assignment_shape_source_47.mo"),
        0,
        1,
    )
}

// Regression: `reg_depends_on_y_index` used to recurse over the register DAG
// without memoization, so a row whose affine coefficient/offset is a deeply
// shared sub-expression (typical of inlined matrix products) took O(2^depth)
// and hung `PreparedScalarProgramBlock::new`. A 40-deep doubling chain has
// 2^40 distinct root-to-leaf paths; the memoized walk must still finish
// instantly and classify the row correctly.
#[test]
fn affine_shape_with_deep_shared_dag_terminates() {
    let depth: u32 = 40;
    let mut ops = vec![LinearOp::Const { dst: 0, value: 1.0 }];
    // reg i = reg(i-1) + reg(i-1): a register reused twice at every level.
    for i in 1..=depth {
        ops.push(LinearOp::Binary {
            dst: i,
            op: BinaryOp::Add,
            lhs: i - 1,
            rhs: i - 1,
        });
    }
    let deep = depth; // root of the shared DAG (no LoadY inside -> full traversal)
    let y_reg = depth + 1;
    let mul_reg = depth + 2;
    let out_reg = depth + 3;
    // out = (y[7] * deep) + deep  -> affine: coefficient `deep`, offset `deep`.
    ops.push(LinearOp::LoadY {
        dst: y_reg,
        index: 7,
    });
    ops.push(LinearOp::Binary {
        dst: mul_reg,
        op: BinaryOp::Mul,
        lhs: y_reg,
        rhs: deep,
    });
    ops.push(LinearOp::Binary {
        dst: out_reg,
        op: BinaryOp::Add,
        lhs: mul_reg,
        rhs: deep,
    });
    ops.push(LinearOp::StoreOutput { src: out_reg });

    // Would hang pre-fix; must return promptly now.
    let shape = target_assignment_shape(&ops).expect("shape recognizer should not fail");
    match shape {
        Some(TargetAssignmentShape::Affine { target_y_index, .. }) => {
            assert_eq!(target_y_index, 7);
        }
        _ => panic!("expected Affine shape for y[7]"),
    }

    // And the public preparation path must also complete.
    let _ = PreparedScalarProgramBlock::new(
        rumoca_ir_solve::ScalarProgramBlock::with_source_span(
            vec![ops],
            fixture_span()
                .require_provenance("prepared assignment fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("scalar fixture is computable"),
    )
    .expect("valid scalar block should prepare");
}

#[test]
fn target_assignment_shape_rejects_expr_eval_len_overflow() {
    let err = checked_expr_eval_len(usize::MAX)
        .expect_err("target assignment expression length overflow should fail");

    assert!(matches!(err, EvalSolveError::InvalidRow { .. }));
}

#[test]
fn direct_assignment_shape_rejects_target_dependent_expression() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 7 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];

    assert_eq!(target_assignment_shape(&row).unwrap(), None);
}

#[test]
fn affine_shape_isolates_either_factor_of_two_solver_coordinates() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(
        vec![row],
        fixture_span()
            .require_provenance("two-coordinate affine fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("scalar fixture is computable");
    let prepared = PreparedScalarProgramBlock::new(block).expect("affine row should prepare");
    let y = [6.0, 3.0, 2.0];

    let first = prepared
        .eval_target_assignment_row_with_context(0, 1, &y, &[], 0.0, RowEvalContext::default())
        .expect("first factor is exactly isolatable");
    let second = prepared
        .eval_target_assignment_row_with_context(0, 2, &y, &[], 0.0, RowEvalContext::default())
        .expect("second factor is exactly isolatable");

    assert_eq!(first, Some(3.0));
    assert_eq!(second, Some(2.0));

    let error = prepared
        .eval_target_assignment_row_with_context(
            0,
            1,
            &[6.0, 3.0, 0.0],
            &[],
            0.0,
            RowEvalContext::default(),
        )
        .expect_err("a zero solver-coordinate coefficient remains singular");
    assert!(matches!(
        error,
        EvalSolveError::SingularTargetAssignment {
            target_y_index: 1,
            coefficient: 0.0,
            ..
        }
    ));
}

#[test]
fn batched_refresh_consumes_the_selected_target_isolator_certificate() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(
        vec![row],
        fixture_span()
            .require_provenance("selected isolator fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("scalar fixture is computable");
    let prepared = PreparedScalarProgramBlock::new(block).expect("affine row should prepare");
    let selected = prepared
        .assignment_shape_for_output(0, 0, 1)
        .expect("the first factor has an isolator");
    let refresh = crate::refresh_plan::AlgebraicRefreshRow {
        equation_index: 0,
        row_idx: 0,
        output_offset: 0,
        target_index: 1,
        assignment_target: Some(1),
        assignment_shape: Some(selected),
        direct_assignment_certified: false,
        exact_assignment_certified: true,
    };
    let mut y = vec![6.0, 99.0, 2.0];

    prepared
        .apply_target_assignment_rows_unchecked_with_context(
            std::slice::from_ref(&refresh),
            &mut y,
            &[],
            0.0,
            RowEvalContext::default(),
        )
        .expect("the selected target certificate should execute directly");
    assert_eq!(y, vec![6.0, 3.0, 2.0]);

    let mismatched = crate::refresh_plan::AlgebraicRefreshRow {
        target_index: 2,
        assignment_target: Some(2),
        ..refresh
    };
    let error = prepared
        .apply_target_assignment_rows_unchecked_with_context(
            &[mismatched],
            &mut y,
            &[],
            0.0,
            RowEvalContext::default(),
        )
        .expect_err("a selected isolator cannot be reused for another target");
    assert!(
        error
            .to_string()
            .contains("shape does not match its refresh target")
    );
}

#[test]
fn affine_shape_isolates_a_coordinate_from_negated_zero_sum() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::LoadY { dst: 2, index: 1 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(
        vec![row],
        fixture_span()
            .require_provenance("negated zero-sum fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("scalar fixture is computable");
    let prepared = PreparedScalarProgramBlock::new(block).expect("affine row should prepare");

    let value = prepared
        .eval_target_assignment_row_with_context(
            0,
            1,
            &[4.0, 0.0],
            &[],
            0.0,
            RowEvalContext::default(),
        )
        .expect("current-balance factor is exactly isolatable");

    assert_eq!(value, Some(-4.0));
}

#[test]
fn affine_residual_shape_isolates_nested_connection_difference() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadY { dst: 1, index: 4 },
        LinearOp::LoadY { dst: 2, index: 6 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(
        vec![row],
        fixture_span()
            .require_provenance("nested connection-difference fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("scalar fixture is computable");
    let prepared = PreparedScalarProgramBlock::new(block).expect("affine row should prepare");
    let mut y = [0.0; 7];
    y[3] = 1.0;
    y[4] = 99.0;
    y[6] = 2.0;

    assert!(!prepared.certifies_direct_target_assignment(0, 0, 4));
    assert!(prepared.certifies_exact_target_assignment(0, 4));
    let value = prepared
        .eval_target_assignment_row_with_context(0, 4, &y, &[], 0.0, RowEvalContext::default())
        .expect("nested connection difference is exactly isolatable");

    assert_eq!(value, Some(3.0));
}

#[test]
fn tensor_load_lanes_are_isolated_without_scalar_load_reconstruction() {
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 10,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::Const { dst: 2, value: 5.0 },
        LinearOp::Const { dst: 3, value: 6.0 },
        LinearOp::TensorBinary {
            dst_start: 4,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: 2,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 2,
            stride: 1,
        },
    ];
    let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(
        vec![row],
        fixture_span()
            .require_provenance("tensor-load isolator fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("tensor fixture is computable");
    let prepared = PreparedScalarProgramBlock::new(block).expect("tensor row should prepare");
    let y = [0.0; 12];

    assert!(prepared.certifies_exact_target_assignment_output(0, 0, 10));
    assert!(prepared.certifies_exact_target_assignment_output(0, 1, 11));
    let grouped = prepared
        .exact_target_assignment_group_program(0, &[(0, 10), (1, 11)])
        .expect("one compact result range materializes two exact assignments");
    assert_eq!(
        rumoca_ir_solve::ScalarProgramBlock::program_output_count(&grouped),
        2
    );
    assert!(
        grouped
            .iter()
            .all(|op| !matches!(op, LinearOp::StoreOutputRange { .. })),
        "the source result range is consumed before final assignment outputs are appended"
    );
    assert_eq!(
        prepared
            .eval_target_assignment_output_unchecked_with_context(
                0,
                0,
                10,
                &y,
                &[],
                0.0,
                RowEvalContext::default(),
            )
            .expect("first tensor lane is isolatable"),
        Some(5.0),
    );
    assert_eq!(
        prepared
            .eval_target_assignment_output_unchecked_with_context(
                0,
                1,
                11,
                &y,
                &[],
                0.0,
                RowEvalContext::default(),
            )
            .expect("second tensor lane is isolatable"),
        Some(6.0),
    );
}

#[test]
fn tensor_difference_keeps_both_compact_y_ranges_as_isolators() {
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 10,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 2,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 20,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 4,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: 2,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::StoreOutput { src: 5 },
    ];
    let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(
        vec![row],
        fixture_span()
            .require_provenance("bidirectional tensor isolator fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("tensor fixture is computable");
    let prepared = PreparedScalarProgramBlock::new(block).expect("tensor row should prepare");
    let mut y = [0.0; 22];
    y[10] = 3.0;
    y[11] = 4.0;
    y[20] = 30.0;
    y[21] = 40.0;

    for (output, lhs, rhs) in [(0, 10, 20), (1, 11, 21)] {
        assert!(prepared.certifies_exact_target_assignment_output(0, output, lhs));
        assert!(prepared.certifies_exact_target_assignment_output(0, output, rhs));
        assert_eq!(
            prepared
                .eval_target_assignment_output_unchecked_with_context(
                    0,
                    output,
                    rhs,
                    &y,
                    &[],
                    0.0,
                    RowEvalContext::default(),
                )
                .expect("the right-hand tensor lane is directly isolatable"),
            Some(y[lhs]),
        );
    }
}
