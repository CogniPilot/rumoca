//! Checked compact dense-solve operands cannot bypass final scalar expansion cost.

use super::*;
use rumoca_ir_solve::ScalarProgramBlock;

fn checked_identity_solve(size: usize) -> ScalarProgramBlock {
    let matrix = 1;
    let rhs = Reg::try_from(1 + size * size).unwrap();
    let output = rhs + Reg::try_from(size).unwrap();
    let mut ops = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::TensorIdentity {
            dst_start: matrix,
            size,
            lanes: 1,
        },
        LinearOp::TensorFill {
            dst_start: rhs,
            value_start: 0,
            count: size,
            lanes: 1,
        },
    ];
    ops.extend((0..size).map(|component| LinearOp::LinearSolveComponent {
        dst: output + Reg::try_from(component).unwrap(),
        matrix_start: matrix,
        rhs_start: rhs,
        n: size,
        component,
    }));
    ops.push(LinearOp::StoreOutputRange {
        start: output,
        count: size,
        stride: 1,
    });
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dense_solve_projection.mo"),
        0,
        1,
    );
    ScalarProgramBlock::with_output_indices(vec![ops], vec![span], (0..size).collect())
        .expect("canonical constructor admits the original compact register ranges")
}

#[test]
fn scalar_dense_solve_small_control_matches_checked_native_and_independent_solution() {
    let original = checked_identity_solve(2);
    let lowered = lower_scalar_ops(&original.programs()[0], None).expect("bounded solve");
    let projected = ScalarProgramBlock::with_output_indices(
        vec![lowered],
        original.program_spans().to_vec(),
        original.output_indices().to_vec(),
    )
    .expect("canonical constructor checks projected register flow");
    for block in [original, projected] {
        let mut actual = [f64::NAN; 2];
        crate::eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut actual).unwrap();
        assert_eq!(
            actual,
            [2.0, 2.0],
            "I*x=[2,2] has independent solution [2,2]"
        );
    }
}

#[test]
fn scalar_dense_solve_final_operand_expansion_refuses_before_large_template_output() {
    let original = checked_identity_solve(200);
    // Every component's emitted solve call lists all n*n matrix and n RHS
    // references. Count that template obligation independently; never render it.
    let references = original.programs()[0]
        .iter()
        .map(|op| match op {
            LinearOp::LinearSolveComponent { n, .. } => n * n + n,
            _ => 0,
        })
        .sum::<usize>();
    assert_eq!(references, 8_040_000);
    assert!(references > EXPANSION_LIMIT);
    assert!(
        lower_scalar_ops(&original.programs()[0], None).is_none(),
        "final scalar solve operands exceed the existing projection budget"
    );
}

#[test]
fn scalar_dense_solve_operand_budget_also_applies_to_scalar_only_programs() {
    let op = LinearOp::LinearSolveComponent {
        dst: 0,
        matrix_start: 0,
        rhs_start: 0,
        n: 2000,
        component: 0,
    };
    assert!(lower_scalar_ops(&[op], None).is_none());
}

#[test]
fn scalar_dense_solve_operand_cost_refuses_overflow_and_preserves_exact_limit() {
    let solve = |n| LinearOp::LinearSolveComponent {
        dst: 0,
        matrix_start: 0,
        rhs_start: 0,
        n,
        component: 0,
    };
    assert_eq!(expansion_cost(&solve(2)), Some(7));
    assert_eq!(expansion_cost(&solve(usize::MAX)), None);
    assert!(lower_scalar_ops(&[solve(usize::MAX)], None).is_none());
    let mut budget = ExpansionBudget { used: 0 };
    assert!(budget.reserve(EXPANSION_LIMIT).is_some());
    assert!(budget.reserve(1).is_none());
    assert_eq!(budget.used, EXPANSION_LIMIT);
}
