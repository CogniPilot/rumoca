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
    let _ = PreparedScalarProgramBlock::new(rumoca_ir_solve::ScalarProgramBlock::with_source_span(
        vec![ops],
        fixture_span(),
    ))
    .expect("valid scalar block should prepare");
}

#[test]
fn target_assignment_shape_rejects_expr_eval_len_overflow() {
    let err = checked_expr_eval_len(usize::MAX)
        .expect_err("target assignment expression length overflow should fail");

    assert!(matches!(err, EvalSolveError::InvalidRow { .. }));
}
