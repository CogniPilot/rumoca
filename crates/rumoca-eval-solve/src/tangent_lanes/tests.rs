use rumoca_ir_solve::{
    BinaryOp, LinearOp, ScalarProgramBlock, TangentLaneProgram, TensorInputKind,
};

use crate::{PreparedScalarProgramBlock, PreparedTangentLaneProgram, RowEvalContext};

/// JVP of `(cross(a, b) .* a) * b` and `cross(a, b)` for `a = y[0..3]`,
/// `b = y[3..6]`: dual loads, a dual cross product, a dual elementwise
/// product, and a dual matrix product.
fn dual_program() -> Vec<LinearOp> {
    let load = |dst_start, start| LinearOp::TensorLoad {
        dst_start,
        input: TensorInputKind::Y,
        input_start: start,
        count: 3,
        seed_start: Some(start),
        lanes: 2,
    };
    vec![
        load(0, 0),
        load(6, 3),
        LinearOp::TensorCross {
            dst_start: 12,
            lhs_start: 0,
            rhs_start: 6,
            lanes: 2,
        },
        LinearOp::TensorBinary {
            dst_start: 18,
            op: BinaryOp::Mul,
            lhs_start: 12,
            rhs_start: 0,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 2,
        },
        LinearOp::MatrixMultiply {
            dst_start: 24,
            lhs_start: 18,
            rhs_start: 6,
            rows: 1,
            inner: 3,
            columns: 1,
            lanes: 2,
        },
        LinearOp::StoreOutputRange {
            start: 25,
            count: 1,
            stride: 2,
        },
        LinearOp::StoreOutputRange {
            start: 13,
            count: 3,
            stride: 2,
        },
    ]
}

#[test]
fn every_lane_of_a_widened_program_equals_the_one_direction_program() {
    let program = dual_program();
    let lanes = 5;
    let widened = PreparedTangentLaneProgram::new(
        TangentLaneProgram::replicate(&program, lanes).expect("the program widens"),
    );
    let single = PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_program_spans(
            vec![program],
            vec![rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("tangent_lanes_tests.mo"),
                0,
                1,
            )],
        )
        .expect("a checked block"),
    )
    .expect("a prepared block");
    let y = [0.3, -1.2, 0.7, 2.0, 0.4, -0.9];
    let directions = (0..lanes)
        .map(|lane| {
            (0..6)
                .map(|index| ((lane * 7 + index * 3) % 11) as f64 / 5.0 - 1.0)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut seed = vec![0.0; 6 * lanes];
    for (lane, direction) in directions.iter().enumerate() {
        for (index, value) in direction.iter().enumerate() {
            seed[index * lanes + lane] = *value;
        }
    }
    let outputs = widened.program().lane_outputs();
    let mut out = vec![0.0; lanes * outputs];
    widened
        .eval(
            &y,
            &[],
            0.0,
            RowEvalContext {
                seed: Some(&seed),
                ..RowEvalContext::default()
            },
            &mut out,
        )
        .expect("the widened program evaluates");
    for (lane, direction) in directions.iter().enumerate() {
        let mut expected = Vec::new();
        single
            .eval_row_outputs_unchecked_with_context(
                0,
                &y,
                &[],
                0.0,
                RowEvalContext {
                    seed: Some(direction),
                    ..RowEvalContext::default()
                },
                &mut expected,
            )
            .expect("the one-direction program evaluates");
        let lane_values = &out[lane * outputs..(lane + 1) * outputs];
        assert_eq!(
            lane_values
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
            expected
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
            "lane {lane}"
        );
    }
}
