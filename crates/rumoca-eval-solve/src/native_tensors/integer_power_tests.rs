//! `TensorIntegerPower` against the `TensorBinary` chain its plan stands for:
//! the quotient `1 / x` over a broadcast constant for a negative exponent,
//! then one packed product per step. Every evaluator path gives the chain's
//! bits: the canonical and prepared evaluators, the scalar expansion, forward
//! AD, widened tangent lanes, and the reverse sweep, including an unselected
//! arm whose zero adjoint must not meet an infinite partial.

use rumoca_ir_solve::{
    BinaryOp, IntegerPower, LinearOp, Reg, ScalarProgramBlock, TangentLaneProgram, TensorInputKind,
};

use crate::reverse::{
    ReverseCotangents, ReverseInputs, ReverseScratch, ScalarVjpProgram, reverse_scalar_block_vjp,
};
use crate::{PreparedScalarProgramBlock, PreparedTangentLaneProgram, RowEvalContext};

/// Bases of every sign and scale, with the IEEE edge values.
const BASES: [f64; 14] = [
    0.0,
    -0.0,
    1.0,
    -1.0,
    0.5,
    -1.75,
    1.000_000_000_000_000_2,
    3.0e-5,
    -7.25e3,
    f64::MIN_POSITIVE,
    1.0e300,
    -2.5,
    f64::INFINITY,
    f64::NEG_INFINITY,
];
const COUNT: usize = BASES.len();

const EXPONENTS: [i64; 15] = [
    -17,
    -4,
    -3,
    -2,
    2,
    3,
    4,
    5,
    7,
    16,
    17,
    31,
    63,
    i64::MAX,
    i64::MIN,
];

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn block(row: Vec<LinearOp>) -> ScalarProgramBlock {
    ScalarProgramBlock::with_program_spans(vec![row], vec![span()]).expect("a checked block")
}

fn load(input: TensorInputKind) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: 0,
        input,
        input_start: 0,
        count: COUNT,
        seed_start: None,
        lanes: 1,
    }
}

fn reg(index: usize) -> Reg {
    Reg::try_from(index).expect("register fits")
}

/// The packed chain `1 / x` then one `TensorBinary` product per step, and the
/// register holding the result.
fn chain(power: IntegerPower, input: TensorInputKind) -> (Vec<LinearOp>, Reg) {
    let mut ops = vec![load(input)];
    let mut next = COUNT;
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
            count: COUNT,
            lhs_stride: 0,
            rhs_stride: 1,
            lanes: 1,
        });
        values[0] = reg(next + 1);
        next += 1 + COUNT;
    }
    for (lhs, rhs) in power.products() {
        ops.push(LinearOp::TensorBinary {
            dst_start: reg(next),
            op: BinaryOp::Mul,
            lhs_start: values[lhs],
            rhs_start: values[rhs],
            count: COUNT,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        });
        values.push(reg(next));
        next += COUNT;
    }
    (ops, *values.last().expect("a base value"))
}

fn fused(power: IntegerPower, input: TensorInputKind) -> (Vec<LinearOp>, Reg) {
    let ops = vec![
        load(input),
        LinearOp::TensorIntegerPower {
            dst_start: reg(COUNT),
            src_start: 0,
            count: COUNT,
            power,
            lanes: 1,
        },
    ];
    (ops, reg(COUNT))
}

fn stored((mut ops, result): (Vec<LinearOp>, Reg)) -> Vec<LinearOp> {
    ops.push(LinearOp::StoreOutputRange {
        start: result,
        count: COUNT,
        stride: 1,
    });
    ops
}

fn powers() -> impl Iterator<Item = IntegerPower> {
    EXPONENTS
        .iter()
        .map(|&exponent| IntegerPower::new(exponent).expect("exponent has a product"))
}

fn direction() -> Vec<f64> {
    (0..COUNT)
        .map(|index| ((index * 7) % 11) as f64 / 5.0 - 1.0)
        .collect()
}

fn assert_bits(what: &str, power: IntegerPower, fused: &[f64], chain: &[f64]) {
    assert_eq!(fused.len(), chain.len(), "{what} ^ {}", power.exponent());
    for (index, (a, b)) in fused.iter().zip(chain).enumerate() {
        assert!(
            a.to_bits() == b.to_bits() || (a.is_nan() && b.is_nan()),
            "{what}, x ^ {} at {index}: fused {a:e} != chain {b:e}",
            power.exponent()
        );
    }
}

fn eval(row: Vec<LinearOp>, y: &[f64], p: &[f64], seed: Option<&[f64]>, width: usize) -> Vec<f64> {
    let mut out = vec![0.0; width];
    crate::eval_scalar_program_block(&block(row), y, p, 0.0, seed, &mut out)
        .expect("the program evaluates");
    out
}

fn prepared(row: Vec<LinearOp>, y: &[f64], seed: Option<&[f64]>) -> Vec<f64> {
    let prepared = PreparedScalarProgramBlock::new(block(row)).expect("a prepared block");
    let mut out = Vec::new();
    prepared
        .eval_row_outputs_unchecked_with_context(
            0,
            y,
            &[],
            0.0,
            RowEvalContext {
                seed,
                ..RowEvalContext::default()
            },
            &mut out,
        )
        .expect("the prepared program evaluates");
    out
}

#[test]
fn primal_values_match_the_chain_on_every_evaluator() {
    for power in powers() {
        let (fused, chain) = (
            stored(fused(power, TensorInputKind::Y)),
            stored(chain(power, TensorInputKind::Y)),
        );
        let canonical = eval(fused.clone(), &BASES, &[], None, COUNT);
        assert_bits(
            "canonical",
            power,
            &canonical,
            &eval(chain.clone(), &BASES, &[], None, COUNT),
        );
        let expected: Vec<f64> = BASES
            .iter()
            .map(|&x| rumoca_core::modelica_integer_power(x, power.exponent()))
            .collect();
        assert_bits("the canonical rule", power, &canonical, &expected);
        assert_bits(
            "prepared",
            power,
            &prepared(fused.clone(), &BASES, None),
            &prepared(chain.clone(), &BASES, None),
        );
        let scalar = |row: &[LinearOp]| {
            crate::lower_scalar_ops(row, None).expect("the program expands to scalar ops")
        };
        assert_bits(
            "scalar expansion",
            power,
            &eval(scalar(&fused), &BASES, &[], None, COUNT),
            &eval(scalar(&chain), &BASES, &[], None, COUNT),
        );
    }
}

fn jvp(row: Vec<LinearOp>) -> Vec<LinearOp> {
    let mut rows = crate::ad::lower_scalar_program_block_ad(&[row]).expect("forward AD lowers");
    rows.pop().expect("one JVP row")
}

#[test]
fn forward_tangents_match_the_chain_and_widen_lane_by_lane() {
    let seed = direction();
    for power in powers() {
        let fused_jvp = jvp(stored(fused(power, TensorInputKind::Y)));
        let chain_jvp = jvp(stored(chain(power, TensorInputKind::Y)));
        assert!(
            fused_jvp
                .iter()
                .any(|op| matches!(op, LinearOp::TensorIntegerPower { lanes: 2, .. })),
            "the dual program keeps one power operation"
        );
        let width = COUNT;
        assert_bits(
            "forward tangent",
            power,
            &eval(fused_jvp.clone(), &BASES, &[], Some(&seed), width),
            &eval(chain_jvp.clone(), &BASES, &[], Some(&seed), width),
        );
        assert_bits(
            "prepared forward tangent",
            power,
            &prepared(fused_jvp.clone(), &BASES, Some(&seed)),
            &prepared(chain_jvp.clone(), &BASES, Some(&seed)),
        );
        let lanes = 3;
        let mut widened_seed = vec![0.0; COUNT * lanes];
        for (index, value) in widened_seed.iter_mut().enumerate() {
            *value = ((index * 5) % 13) as f64 / 4.0 - 1.5;
        }
        let widened = |row: &[LinearOp]| {
            let program = PreparedTangentLaneProgram::new(
                TangentLaneProgram::replicate(row, lanes).expect("the program widens"),
            );
            let mut out = vec![0.0; lanes * program.program().lane_outputs()];
            program
                .eval(
                    &BASES,
                    &[],
                    0.0,
                    RowEvalContext {
                        seed: Some(&widened_seed),
                        ..RowEvalContext::default()
                    },
                    &mut out,
                )
                .expect("the widened program evaluates");
            out
        };
        assert_bits(
            "tangent lanes",
            power,
            &widened(&fused_jvp),
            &widened(&chain_jvp),
        );
    }
}

#[test]
fn an_inactive_base_keeps_a_primal_power_with_zero_tangents() {
    for power in powers() {
        let fused_jvp = jvp(stored(fused(power, TensorInputKind::P)));
        assert!(
            !fused_jvp
                .iter()
                .any(|op| matches!(op, LinearOp::TensorIntegerPower { lanes: 2, .. })),
            "a parameter base needs no dual power"
        );
        let chain_jvp = jvp(stored(chain(power, TensorInputKind::P)));
        assert_bits(
            "inactive tangent",
            power,
            &eval(fused_jvp, &[], &BASES, Some(&[]), COUNT),
            &eval(chain_jvp, &[], &BASES, Some(&[]), COUNT),
        );
    }
}

fn gradient(row: Vec<LinearOp>, y: &[f64], cotangents: &[f64]) -> Vec<f64> {
    let block = block(row);
    let row_registers: Vec<usize> = block
        .programs()
        .iter()
        .map(|row| crate::required_registers(row).expect("register count"))
        .collect();
    let requirements = crate::scalar_program_block_input_requirements(&block).expect("inputs");
    let mut gy = vec![0.0; y.len()];
    reverse_scalar_block_vjp(
        &ScalarVjpProgram {
            block: &block,
            row_registers: &row_registers,
            requirements,
        },
        &ReverseInputs {
            y,
            p: &[],
            t: 0.0,
            context: RowEvalContext::default(),
        },
        cotangents,
        &mut ReverseCotangents {
            y: &mut gy,
            p: &mut [],
            seed: &mut [],
        },
        &mut ReverseScratch::default(),
    )
    .expect("reverse sweep");
    gy
}

#[test]
fn reverse_adjoints_match_the_chain() {
    let cotangents = direction();
    for power in powers() {
        assert_bits(
            "reverse adjoint",
            power,
            &gradient(
                stored(fused(power, TensorInputKind::Y)),
                &BASES,
                &cotangents,
            ),
            &gradient(
                stored(chain(power, TensorInputKind::Y)),
                &BASES,
                &cotangents,
            ),
        );
    }
}

/// `select(y[0] > 2, x .^ n, y)` per element, with `y[0] = 0`: every power
/// element is the unselected arm, its adjoint is zero, and a zero base makes
/// the partials of a negative exponent infinite. The chain passes nothing
/// back from such an arm, and neither may the operation.
fn selected((mut ops, result): (Vec<LinearOp>, Reg)) -> Vec<LinearOp> {
    let base = ops
        .iter()
        .filter_map(LinearOp::dst_register)
        .zip(ops.iter().map(LinearOp::dst_register_count))
        .map(|(start, count)| start as usize + count)
        .max()
        .expect("registers");
    let (two, condition) = (reg(base), reg(base + 1));
    ops.push(LinearOp::Const {
        dst: two,
        value: 2.0,
    });
    ops.push(LinearOp::Compare {
        dst: condition,
        op: rumoca_ir_solve::CompareOp::Gt,
        lhs: 0,
        rhs: two,
    });
    for element in 0..COUNT {
        ops.push(LinearOp::Select {
            dst: reg(base + 2 + element),
            cond: condition,
            if_true: result + reg(element),
            if_false: reg(element),
        });
    }
    ops.push(LinearOp::StoreOutputRange {
        start: reg(base + 2),
        count: COUNT,
        stride: 1,
    });
    ops
}

#[test]
fn an_unselected_power_passes_no_adjoint_back() {
    let cotangents = direction();
    let mut y = BASES;
    y[0] = 0.0;
    for power in powers() {
        let fused = gradient(selected(fused(power, TensorInputKind::Y)), &y, &cotangents);
        let chain = gradient(selected(chain(power, TensorInputKind::Y)), &y, &cotangents);
        assert_bits("unselected reverse adjoint", power, &fused, &chain);
        assert!(
            fused.iter().all(|value| !value.is_nan()),
            "x ^ {}: an unselected arm leaks no NaN",
            power.exponent()
        );
    }
}
