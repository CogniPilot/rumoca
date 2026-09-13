use super::*;

fn prepare(program: Vec<LinearOp>) -> PreparedScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("additive.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(
            vec![program],
            span.require_provenance("additive assignment").unwrap(),
        )
        .unwrap(),
    )
    .unwrap()
}

fn offset_program() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 3,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 4 },
    ]
}

#[test]
fn additive_offsets_preserve_prefix_errors_and_materialized_values() {
    let mut program = offset_program();
    program.insert(0, LinearOp::LoadP { dst: 5, index: 0 });
    let prepared = prepare(program.clone());
    let shape = prepared.assignment_shape_for_output(0, 0, 0).unwrap();
    assert_eq!(shape.expr_eval_len(), program.len() - 1);
    assert!(!assignment_shape_reads_y_index(&program, shape, 0));
    assert!(assignment_shape_reads_y_index(&program, shape, 1));
    assert!(assignment_shape_reads_y_index(&program, shape, 2));
    let materialized = prepared
        .exact_target_assignment_output_program(0, 0, 0)
        .unwrap();
    assert_eq!(
        &materialized[..program.len() - 1],
        &program[..program.len() - 1]
    );
    let explicit = prepare(materialized);
    for guess in [1e30, -1e30, 0.0] {
        let y = [guess, 4.0, 13.0];
        assert!(
            prepared
                .eval_target_assignment_row_with_context(
                    0,
                    0,
                    &y,
                    &[],
                    0.0,
                    RowEvalContext::default()
                )
                .is_err()
        );
        assert!(
            explicit
                .eval_row_with_context(0, &y, &[], 0.0, RowEvalContext::default())
                .is_err()
        );
        assert_eq!(
            explicit
                .eval_row_with_context(0, &y, &[0.0], 0.0, RowEvalContext::default())
                .unwrap(),
            9.0
        );
        assert_eq!(
            prepared
                .eval_target_assignment_row_with_context(
                    0,
                    0,
                    &y,
                    &[0.0],
                    0.0,
                    RowEvalContext::default()
                )
                .unwrap(),
            Some(9.0)
        );
    }
}

#[test]
fn additive_shared_dag_keeps_two_offset_terms_after_forty_doublings() {
    let mut program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
    ];
    for dst in 3..43 {
        program.push(LinearOp::Binary {
            dst,
            op: BinaryOp::Add,
            lhs: dst - 1,
            rhs: dst - 1,
        });
    }
    program.extend([
        LinearOp::LoadY { dst: 43, index: 2 },
        LinearOp::Binary {
            dst: 44,
            op: BinaryOp::Sub,
            lhs: 42,
            rhs: 43,
        },
        LinearOp::StoreOutput { src: 44 },
    ]);
    let length = program.len();
    let prepared = prepare(program);
    let TargetAssignmentShape::Additive {
        offset_terms,
        coefficient,
        ..
    } = prepared.assignment_shape_for_output(0, 0, 0).unwrap()
    else {
        panic!("requires additive isolation");
    };
    let scale = 2_f64.powi(40);
    assert_eq!(*coefficient, scale);
    assert_eq!(offset_terms.as_ref(), &[(1, scale), (43, -1.0)]);
    let materialized = prepared
        .exact_target_assignment_output_program(0, 0, 0)
        .unwrap();
    assert!(materialized.len() < length + 16);
    let y = [1e30, 4.0, 13.0 * scale];
    assert_eq!(
        prepare(materialized)
            .eval_row_with_context(0, &y, &[], 0.0, RowEvalContext::default())
            .unwrap(),
        9.0
    );
    assert_eq!(
        prepared
            .eval_target_assignment_row_with_context(0, 0, &y, &[], 0.0, RowEvalContext::default())
            .unwrap(),
        Some(9.0)
    );
}

#[test]
fn additive_isolation_refuses_cancelled_and_nonlinear_target_coefficients() {
    for op in [BinaryOp::Sub, BinaryOp::Mul] {
        let program = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::LoadY { dst: 2, index: 1 },
            LinearOp::Binary {
                dst: 3,
                op,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 3,
                rhs: 2,
            },
            LinearOp::StoreOutput { src: 4 },
        ];
        assert!(
            rumoca_ir_solve::derive_target_assignment_shape_for_output(&program, 0, 0).is_none()
        );
    }
}
