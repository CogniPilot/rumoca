//! Explicit-time directions are separate from solver Y and parameter seeds.

use super::*;
use rumoca_ir_solve::TensorInputKind;

fn source(ops: Vec<LinearOp>) -> ScalarProgramBlock {
    let id = rumoca_core::SourceId::from_source_name("time_seed_fixture.mo");
    let span = rumoca_core::Span::from_offsets(id, 0, 1);
    let count = ScalarProgramBlock::program_output_count(&ops);
    ScalarProgramBlock::with_output_indices(vec![ops], vec![span], (0..count).collect()).unwrap()
}

fn evaluate(
    program: &ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    time: f64,
    seed: &[f64],
) -> Vec<f64> {
    let mut values = vec![0.0; program.output_count()];
    crate::eval_scalar_program_block(program, y, p, time, Some(seed), &mut values).unwrap();
    values
}

fn time_p() -> ScalarProgramBlock {
    source(vec![
        LinearOp::LoadTime { dst: 0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::LoadP { dst: 2, index: 0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
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
        LinearOp::Const { dst: 5, value: 3.0 },
        LinearOp::Const { dst: 6, value: 2.0 },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Mul,
            lhs: 5,
            rhs: 0,
        },
        LinearOp::Binary {
            dst: 8,
            op: BinaryOp::Mul,
            lhs: 6,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 9,
            op: BinaryOp::Add,
            lhs: 7,
            rhs: 8,
        },
        LinearOp::StoreOutput { src: 9 },
    ])
}

#[test]
fn explicit_time_seed_has_mixed_basis_and_transpose_products() {
    let primal = time_p();
    let derived = lower_scalar_program_block_full_time_jvp(&primal, 1, 1).unwrap();
    // Internal storage is [x | u | time], public fixture order is [time,x,u].
    assert_eq!(
        evaluate(&derived, &[1.0], &[0.0], 2.0, &[7.0, 11.0, 5.0]),
        [30.0, 29.0]
    );
    let columns = [[0.0, 0.0, 1.0], [1.0, 0.0, 0.0], [0.0, 1.0, 0.0]]
        .map(|seed| evaluate(&derived, &[1.0], &[0.0], 2.0, &seed));
    assert_eq!(columns, [vec![1.0, 3.0], vec![2.0, 2.0], vec![1.0, 0.0]]);
    let adjoint = columns.map(|column| 2.0 * column[0] + 3.0 * column[1]);
    assert_eq!(adjoint, [11.0, 10.0, 2.0]);
    assert_eq!(
        5.0 * adjoint[0] + 7.0 * adjoint[1] + 11.0 * adjoint[2],
        147.0
    );
}

#[test]
fn original_solver_and_parameter_entrypoints_keep_time_held() {
    let primal = time_p();
    let held = lower_scalar_program_block_full_jvp(&primal, 1).unwrap();
    assert_eq!(
        evaluate(&held, &[1.0], &[0.0], 2.0, &[7.0, 11.0]),
        [25.0, 14.0]
    );
    for mode in [
        SeedMode::SolverYOnly,
        SeedMode::SolverYSubset { active: &[0] },
    ] {
        let rows = lower_scalar_program_rows_ad(
            primal.programs(),
            primal.program_spans(),
            mode,
            "held-time regression",
        )
        .unwrap();
        let program = ScalarProgramBlock::with_output_indices(
            rows,
            primal.program_spans().to_vec(),
            primal.output_indices().to_vec(),
        )
        .unwrap();
        assert_eq!(
            evaluate(&program, &[1.0], &[0.0], 2.0, &[7.0]),
            [14.0, 14.0]
        );
    }
}

#[test]
fn time_domain_refuses_storage_aliases_and_extent_overflow() {
    for op in [
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::LoadP { dst: 0, index: 1 },
    ] {
        let block = source(vec![op, LinearOp::StoreOutput { src: 0 }]);
        assert!(lower_scalar_program_block_full_time_jvp(&block, 1, 1).is_err());
    }
    let indexed = source(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::LoadIndexedP {
            dst: 1,
            base: 0,
            count: 2,
            index: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ]);
    assert!(lower_scalar_program_block_full_time_jvp(&indexed, 1, 1).is_err());
    for input in [TensorInputKind::Y, TensorInputKind::P] {
        let tensor = source(vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ]);
        assert!(lower_scalar_program_block_full_time_jvp(&tensor, 1, 1).is_err());
    }
    assert!(
        lower_scalar_program_block_full_time_jvp(&ScalarProgramBlock::default(), usize::MAX, 0)
            .is_err()
    );
    assert!(lower_scalar_program_block_full_time_jvp(&ScalarProgramBlock::default(), 0, 0).is_ok());
}

#[test]
fn compact_tensor_product_retains_its_explicit_time_direction() {
    let primal = source(vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::LoadTime { dst: 2 },
        LinearOp::TensorFill {
            dst_start: 3,
            value_start: 2,
            count: 2,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 5,
            op: BinaryOp::Mul,
            lhs_start: 0,
            rhs_start: 3,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 5,
            count: 2,
            stride: 1,
        },
    ]);
    let derived = lower_scalar_program_block_full_time_jvp(&primal, 2, 0).unwrap();
    assert_eq!(
        evaluate(&derived, &[1.0, 4.0], &[], 2.0, &[7.0, 11.0, 5.0]),
        [19.0, 42.0]
    );
}
