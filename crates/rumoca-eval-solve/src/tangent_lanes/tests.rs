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

/// JVP of `if y[1] > 0 then y[0]^2 else y[2]` as a checked function
/// conditional: the selected arm reads the dual pair of `y[0]` through its
/// captures and the fallback loads `y[2]` and its seed itself.
fn conditional_program() -> Vec<LinearOp> {
    let condition = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Compare {
            dst: 2,
            op: rumoca_ir_solve::CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let square = vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
        LinearOp::LoadFunctionConditionalCapture { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
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
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 2 },
        LinearOp::StoreOutput { src: 4 },
    ];
    let fallback = vec![
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::LoadSeed { dst: 1, index: 2 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked(
        2,
        [2],
        [(condition, square)],
        fallback,
    )
    .expect("a checked conditional");
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadSeed { dst: 1, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 2,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 3 },
        LinearOp::StoreOutput { src: 2 },
    ]
}

/// A checked function conditional widens to one selected-arm program per
/// lane, and each lane equals the one-direction program for either arm.
#[test]
fn every_lane_of_a_widened_function_conditional_equals_the_one_direction_program() {
    let program = conditional_program();
    let lanes = 4;
    let widened = PreparedTangentLaneProgram::new(
        TangentLaneProgram::replicate(&program, lanes).expect("the conditional widens"),
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
    let directions = (0..lanes)
        .map(|lane| {
            (0..3)
                .map(|index| (lane * 3 + index) as f64 - 4.5)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut seed = vec![0.0; 3 * lanes];
    for (lane, direction) in directions.iter().enumerate() {
        for (index, value) in direction.iter().enumerate() {
            seed[index * lanes + lane] = *value;
        }
    }
    for y in [[0.7, 1.0, -2.5], [0.7, -1.0, -2.5]] {
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
            assert_eq!(
                out[lane * outputs..(lane + 1) * outputs]
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>(),
                expected
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>(),
                "lane {lane} at {y:?}"
            );
        }
    }
}

/// A causal coefficient proven nonzero at construction admits its division
/// unless run-time values made it non-finite, which declines the torn solve.
#[test]
fn a_causal_coefficient_declines_only_when_not_finite() {
    assert!(super::causal_coefficient_is_finite(-2.5));
    assert!(super::causal_coefficient_is_finite(f64::MIN_POSITIVE));
    assert!(!super::causal_coefficient_is_finite(f64::NAN));
    assert!(!super::causal_coefficient_is_finite(f64::INFINITY));
}

/// A zero coefficient contradicts the construction proof, which a debug
/// build asserts instead of declining.
#[cfg(debug_assertions)]
#[test]
#[should_panic(expected = "proven nonzero at construction")]
fn a_zero_causal_coefficient_is_a_construction_defect() {
    let _ = super::causal_coefficient_is_finite(0.0);
}
