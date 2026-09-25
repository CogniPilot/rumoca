use super::*;
use crate::BinaryOp;

/// JVP of `y0 * y1`: the primal product and the tangent `v0*y1 + y0*v1`.
fn product_jvp() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::LoadSeed { dst: 3, index: 0 },
        LinearOp::LoadSeed { dst: 4, index: 1 },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 3,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 4,
        },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Add,
            lhs: 5,
            rhs: 6,
        },
        LinearOp::StoreOutput { src: 7 },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn count(program: &TangentLaneProgram, kind: &str) -> usize {
    program
        .ops()
        .iter()
        .filter(|op| op.kind_name() == kind)
        .count()
}

#[test]
fn seed_independent_operations_run_once_and_tangent_operations_once_per_lane() {
    let lanes = 3;
    let program = TangentLaneProgram::replicate(&product_jvp(), lanes).unwrap();
    assert_eq!(count(&program, "LoadY"), 2);
    assert_eq!(count(&program, "LoadSeed"), 2 * lanes);
    // One primal product, three tangent operations per lane.
    assert_eq!(count(&program, "Binary"), 1 + 3 * lanes);
    assert_eq!(program.lane_outputs(), 2);
    let seeds = program
        .ops()
        .iter()
        .filter_map(|op| match op {
            LinearOp::LoadSeed { index, .. } => Some(*index),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(seeds, [0, 1, 2, 3, 4, 5], "seeds are element-major");
    assert_eq!(
        ScalarProgramBlock::program_output_count(program.ops()),
        lanes * 2
    );
}

/// A dual tensor product whose operands are packed from scalars: the primal
/// packing runs once, the tangent packing once per lane, and the product is
/// one widened aggregate.
#[test]
fn a_dual_aggregate_widens_to_one_primal_and_one_lane_per_tangent() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadSeed { dst: 1, index: 0 },
        LinearOp::Move { dst: 2, src: 0 },
        LinearOp::Move { dst: 3, src: 1 },
        LinearOp::TensorBinary {
            dst_start: 4,
            op: BinaryOp::Mul,
            lhs_start: 2,
            rhs_start: 2,
            count: 1,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 2,
        },
        LinearOp::StoreOutputRange {
            start: 5,
            count: 1,
            stride: 1,
        },
    ];
    let lanes = 4;
    let widened = TangentLaneProgram::replicate(&program, lanes).unwrap();
    let aggregates = widened
        .ops()
        .iter()
        .filter_map(|op| match op {
            LinearOp::TensorBinary { lanes, .. } => Some(*lanes),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(aggregates, [lanes + 1]);
    assert_eq!(count(&widened, "LoadY"), 1);
    // One primal packing move, one tangent packing move and one output move
    // per lane.
    assert_eq!(count(&widened, "Move"), 1 + 2 * lanes);
}

#[test]
fn a_seeded_runtime_index_and_an_empty_lane_set_are_refused() {
    let seeded_index = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::LoadIndexedSeed {
            dst: 1,
            base: 0,
            count: 2,
            index: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    assert!(matches!(
        TangentLaneProgram::replicate(&seeded_index, 2),
        Err(TangentLaneError::Unsupported { op_index: 1, .. })
    ));
    assert!(matches!(
        TangentLaneProgram::replicate(&product_jvp(), 0),
        Err(TangentLaneError::LaneCount { lanes: 0 })
    ));
}
