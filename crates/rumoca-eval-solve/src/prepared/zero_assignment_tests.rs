use super::*;
use rumoca_ir_solve::UnaryOp;

fn prepare(program: Vec<LinearOp>) -> PreparedScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("zero_assignment.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(
            vec![program],
            span.require_provenance("zero assignment").unwrap(),
        )
        .unwrap(),
    )
    .unwrap()
}

#[test]
fn zero_tensor_residual_supplies_each_declared_target_seed() {
    let prepared = prepare(vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 1,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 3,
            stride: 1,
        },
    ]);
    for output in 0..3 {
        let target = output + 1;
        assert!(
            prepared.can_evaluate_declared_target_assignment(0, output, target),
            "an exact zero residual must supply a causal seed for coordinate {target}"
        );
        assert!(prepared.certifies_direct_target_assignment(0, output, target));
        assert!(!prepared.can_evaluate_target_assignment_output(0, output, 0));
    }
    let program = prepared
        .exact_target_assignment_group_program(0, &[(0, 1), (1, 2), (2, 3)])
        .expect("independent zero assignments share their tensor source");
    let mut actual = [99.0; 3];
    prepare(program)
        .eval_with_context(
            &[13.0, 17.0, -23.0, 31.0],
            &[],
            0.0,
            RowEvalContext::default(),
            &mut actual,
        )
        .unwrap();
    assert_eq!(actual, [0.0; 3]);
}

#[test]
fn zero_scalar_assignment_keeps_load_copy_and_negation_prefixes() {
    for wrapper in [
        None,
        Some(LinearOp::Move { dst: 1, src: 0 }),
        Some(LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        }),
    ] {
        let mut program = vec![LinearOp::LoadY { dst: 0, index: 2 }];
        let output = u32::from(wrapper.is_some());
        program.extend(wrapper);
        program.push(LinearOp::LoadP { dst: 2, index: 0 });
        let prefix = program.clone();
        program.push(LinearOp::StoreOutput { src: output });
        let prepared = prepare(program);
        assert!(prepared.certifies_direct_target_assignment(0, 0, 2));
        assert_eq!(
            prepared
                .eval_target_assignment_row_with_context(
                    0,
                    2,
                    &[1.0, 2.0, 37.0],
                    &[5.0],
                    0.0,
                    RowEvalContext::default(),
                )
                .unwrap(),
            Some(0.0)
        );
        let isolated = prepared
            .exact_target_assignment_output_program(0, 0, 2)
            .unwrap();
        assert_eq!(&isolated[..prefix.len()], prefix);
        let mut actual = [1.0];
        assert!(
            prepare(isolated)
                .eval_with_context(
                    &[1.0, 2.0, 37.0],
                    &[],
                    0.0,
                    RowEvalContext::default(),
                    &mut actual,
                )
                .is_err(),
            "isolating zero must retain the missing-parameter error"
        );
    }
}

#[test]
fn zero_assignment_does_not_certify_singular_or_nonlinear_products_as_direct() {
    for factor in [
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::Const { dst: 1, value: 0.0 },
    ] {
        let program = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            factor,
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ];
        let dynamic_coefficient = matches!(program[1], LinearOp::LoadP { .. });
        let prepared = prepare(program);
        assert_eq!(
            prepared.can_evaluate_declared_target_assignment(0, 0, 0),
            dynamic_coefficient
        );
        assert!(!prepared.certifies_direct_target_assignment(0, 0, 0));
        if dynamic_coefficient {
            assert_eq!(
                prepared
                    .eval_target_assignment_row_with_context(
                        0,
                        0,
                        &[1e30],
                        &[2.0],
                        0.0,
                        RowEvalContext::default(),
                    )
                    .unwrap(),
                Some(0.0)
            );
            assert!(matches!(
                prepared.eval_target_assignment_row_with_context(
                    0,
                    0,
                    &[1e30],
                    &[0.0],
                    0.0,
                    RowEvalContext::default(),
                ),
                Err(EvalSolveError::SingularTargetAssignment { .. })
            ));
        }
    }
}

#[test]
fn zero_assignment_rejects_overwritten_target_registers() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    assert!(rumoca_ir_solve::derive_target_assignment_shape_for_output(&program, 0, 0).is_none());
}
