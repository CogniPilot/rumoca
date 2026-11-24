//! A native `TensorIntegerPower` against the `TensorBinary` chain of its plan
//! and against the canonical evaluator, bit for bit, on primal rows and on
//! their forward-mode JVP, on direct registers (9 elements) and on the
//! register tape (225 elements, past the 128 direct registers).

use super::*;
use rumoca_ir_solve::{BinaryOp, IntegerPower, Reg, TensorInputKind};

const BASES: [f64; 9] = [
    0.0,
    -0.0,
    0.5,
    -1.75,
    1.000_000_000_000_000_2,
    3.0e-5,
    -7.25e3,
    1.0e300,
    f64::INFINITY,
];

fn reg(index: usize) -> Reg {
    Reg::try_from(index).expect("register fits")
}

fn load(count: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: 0,
        input: TensorInputKind::Y,
        input_start: 0,
        count,
        seed_start: None,
        lanes: 1,
    }
}

fn store(start: Reg, count: usize) -> LinearOp {
    LinearOp::StoreOutputRange {
        start,
        count,
        stride: 1,
    }
}

fn chain(power: IntegerPower, count: usize) -> Vec<LinearOp> {
    let mut ops = vec![load(count)];
    let mut next = count;
    let mut values = vec![0];
    if power.reciprocal() {
        ops.push(LinearOp::Const {
            dst: reg(next),
            value: 1.0,
        });
        ops.push(LinearOp::TensorBinary {
            dst_start: reg(next + 1),
            op: BinaryOp::Div,
            lhs_start: reg(next),
            rhs_start: 0,
            count,
            lhs_stride: 0,
            rhs_stride: 1,
            lanes: 1,
        });
        values[0] = reg(next + 1);
        next += 1 + count;
    }
    for (lhs, rhs) in power.products() {
        ops.push(LinearOp::TensorBinary {
            dst_start: reg(next),
            op: BinaryOp::Mul,
            lhs_start: values[lhs],
            rhs_start: values[rhs],
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        });
        values.push(reg(next));
        next += count;
    }
    ops.push(store(*values.last().expect("a value"), count));
    ops
}

fn fused(power: IntegerPower, count: usize) -> Vec<LinearOp> {
    vec![
        load(count),
        LinearOp::TensorIntegerPower {
            dst_start: reg(count),
            src_start: 0,
            count,
            power,
            lanes: 1,
        },
        store(reg(count), count),
    ]
}

fn block(row: Vec<LinearOp>) -> ScalarProgramBlock {
    ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()]).expect("block")
}

fn assert_bits(what: &str, exponent: i64, actual: &[f64], expected: &[f64]) {
    for (index, (a, b)) in actual.iter().zip(expected).enumerate() {
        assert!(
            a.to_bits() == b.to_bits() || (a.is_nan() && b.is_nan()),
            "{what}, x ^ {exponent} at {index}: {a:e} != {b:e}"
        );
    }
}

/// `count` inputs cycling through `BASES`.
fn inputs(count: usize) -> Vec<f64> {
    BASES.iter().copied().cycle().take(count).collect()
}

fn native_primal(row: Vec<LinearOp>, count: usize) -> Vec<f64> {
    let compiled = compile_expression_scalar_program_block(&block(row)).expect("compiles");
    let mut out = vec![0.0; count];
    compiled
        .call(&inputs(count), &[], 0.0, &mut out)
        .expect("runs");
    out
}

fn native_jvp(row: Vec<LinearOp>, seed: &[f64]) -> (Vec<f64>, Vec<f64>) {
    let count = seed.len();
    let y = inputs(count);
    let mut rows =
        rumoca_eval_solve::ad::lower_scalar_program_block_ad(&[row]).expect("forward AD lowers");
    let jvp = block(rows.pop().expect("one row"));
    let compiled = compile_jacobian_scalar_program_block(&jvp).expect("compiles");
    let mut out = vec![0.0; count];
    compiled.call(&y, &[], 0.0, seed, &mut out).expect("runs");
    let mut oracle = vec![0.0; count];
    rumoca_eval_solve::eval_scalar_program_block(&jvp, &y, &[], 0.0, Some(seed), &mut oracle)
        .expect("the evaluator runs");
    (out, oracle)
}

#[test]
fn native_power_matches_the_chain_and_the_evaluator() {
    for count in [BASES.len(), 225] {
        let seed: Vec<f64> = (0..count)
            .map(|i| ((i * 7) % 11) as f64 / 5.0 - 1.0)
            .collect();
        for exponent in [-17, -3, -2, 2, 3, 4, 7, 16, 31, 63, i64::MAX, i64::MIN] {
            let power = IntegerPower::new(exponent).expect("exponent has a product");
            let primal = native_primal(fused(power, count), count);
            let chain_primal = native_primal(chain(power, count), count);
            assert_bits("native primal", exponent, &primal, &chain_primal);
            let expected: Vec<f64> = inputs(count)
                .iter()
                .map(|&x| rumoca_core::modelica_integer_power(x, power.exponent()))
                .collect();
            assert_bits(
                "native primal against the canonical rule",
                exponent,
                &primal,
                &expected,
            );
            let (tangent, oracle) = native_jvp(fused(power, count), &seed);
            let (chain_tangent, _) = native_jvp(chain(power, count), &seed);
            assert_bits("native tangent", exponent, &tangent, &chain_tangent);
            assert_bits(
                "native tangent against the evaluator",
                exponent,
                &tangent,
                &oracle,
            );
        }
    }
}
