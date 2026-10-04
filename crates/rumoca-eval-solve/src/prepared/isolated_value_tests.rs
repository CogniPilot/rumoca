//! The materialized isolator, the evaluator's per-row isolation, and the
//! folded form agree bit for bit.

use std::sync::Arc;

use rumoca_ir_solve::{
    IsolatedValue, LinearOp, Reg, TargetAssignmentShape, materialize_target_assignment,
};

use super::isolated_value::{eval_isolated_parts, eval_isolated_value, register_coefficient};
use crate::ops::{eval_binary, eval_unary};

/// Registers 0..4 hold loaded values; materialization appends after them.
fn prefix() -> Vec<LinearOp> {
    (0..4)
        .map(|index| LinearOp::LoadY {
            dst: index,
            index: index as usize,
        })
        .collect()
}

fn additive(terms: &[(Reg, f64)], coefficient: f64) -> TargetAssignmentShape {
    TargetAssignmentShape::Additive {
        target_y_index: 9,
        offset_terms: Arc::from(terms),
        coefficient,
        expr_eval_len: 4,
    }
}

fn affine(offset_scale: f64, coefficient: Option<Reg>, scale: f64) -> TargetAssignmentShape {
    TargetAssignmentShape::Affine {
        target_y_index: 9,
        offset_reg: 0,
        coefficient_reg: coefficient,
        offset_scale,
        coefficient_scale: scale,
        expr_eval_len: 4,
    }
}

fn reciprocal(numerator_scale: f64, divisor_scale: f64) -> TargetAssignmentShape {
    TargetAssignmentShape::Reciprocal {
        target_y_index: 9,
        numerator_reg: 0,
        numerator_scale,
        divisor_reg: 3,
        divisor_scale,
        expr_eval_len: 4,
    }
}

/// The appended operations and the result register.
fn materialized(shape: &TargetAssignmentShape) -> (Vec<LinearOp>, u32) {
    let mut operations = prefix();
    let (result, _) = materialize_target_assignment(shape, &mut operations).expect("materializes");
    (operations.split_off(4), result)
}

/// Run the appended isolator operations with the evaluator's arithmetic.
fn interpret(operations: &[LinearOp], registers: &mut Vec<f64>) {
    for operation in operations {
        let (dst, value) = match *operation {
            LinearOp::Const { dst, value } => (dst, value),
            LinearOp::Unary { dst, op, arg } => (dst, eval_unary(op, registers[arg as usize])),
            LinearOp::Binary { dst, op, lhs, rhs } => (
                dst,
                eval_binary(op, registers[lhs as usize], registers[rhs as usize]),
            ),
            ref other => unreachable!("unexpected isolator operation {other:?}"),
        };
        if registers.len() <= dst as usize {
            registers.resize(dst as usize + 1, 0.0);
        }
        registers[dst as usize] = value;
    }
}

/// Deterministic sample values, signed zeros and exact extremes included.
fn sample(state: &mut u64) -> f64 {
    *state = state
        .wrapping_mul(6_364_136_223_846_793_005)
        .wrapping_add(1_442_695_040_888_963_407);
    match *state >> 60 {
        0 => 0.0,
        1 => -0.0,
        2 => 1.0,
        3 => -1.0,
        4 => 4.0,
        5 => -0.5,
        _ => {
            f64::from_bits(0x3FF0_0000_0000_0000 | (*state >> 12))
                * (*state as i64).signum() as f64
                * 3.7
        }
    }
}

/// The emitted program, the evaluator's form, and its allocation-free
/// evaluation agree bit for bit; the plain `-(0 + sum) / c` form agrees up
/// to the sign of zero.
#[test]
fn materialized_and_evaluated_values_agree_bit_for_bit() {
    let mut state = 7_u64;
    for case in 0..4000 {
        let scales = [sample(&mut state), sample(&mut state), sample(&mut state)];
        let coefficient = sample(&mut state);
        let values = [
            sample(&mut state),
            sample(&mut state),
            sample(&mut state),
            sample(&mut state),
        ];
        let count = 1 + case % 3;
        let terms: Vec<(Reg, f64)> = (0..count)
            .map(|index| (index as Reg, scales[index]))
            .collect();
        let shapes = [
            additive(&terms, coefficient),
            affine(scales[0], None, coefficient),
            affine(scales[0], Some(3), scales[1]),
            reciprocal(scales[0], scales[1]),
        ];
        for shape in shapes {
            let (operations, result) = materialized(&shape);
            let mut registers = values.to_vec();
            interpret(&operations, &mut registers);
            let emitted = registers[result as usize];
            let read = |register: Reg| Ok::<f64, ()>(values[register as usize]);
            let evaluated = eval_isolated_value(&shape, read).unwrap().unwrap();
            let form = IsolatedValue::of(&shape).unwrap();
            let formed =
                eval_isolated_parts(form.terms.iter().copied(), form.divisor, read).unwrap();
            let guarded = match &shape {
                TargetAssignmentShape::Affine {
                    coefficient_reg: Some(register),
                    coefficient_scale,
                    ..
                } => !register_coefficient(values[*register as usize], *coefficient_scale)
                    .is_finite(),
                TargetAssignmentShape::Affine {
                    coefficient_scale, ..
                } => !coefficient_scale.is_finite(),
                TargetAssignmentShape::Reciprocal {
                    divisor_reg,
                    divisor_scale,
                    ..
                } => {
                    !register_coefficient(values[*divisor_reg as usize], *divisor_scale).is_finite()
                }
                _ => false,
            };
            if guarded {
                assert!(emitted.is_nan(), "{shape:?}: the guard poisons");
                continue;
            }
            assert_eq!(
                emitted.to_bits(),
                evaluated.to_bits(),
                "{shape:?} {values:?}"
            );
            assert_eq!(
                formed.to_bits(),
                evaluated.to_bits(),
                "{shape:?} {values:?}"
            );
            let plain = plain_value(&shape, &values);
            assert!(
                plain.to_bits() == emitted.to_bits()
                    || (plain == 0.0 && emitted == 0.0)
                    || (plain.is_nan() && emitted.is_nan()),
                "{shape:?} {values:?}: plain {plain} emitted {emitted}"
            );
        }
    }
}

/// `-(0 + sum of scale * r) / c`, the form before the exact identities.
fn plain_value(shape: &TargetAssignmentShape, values: &[f64]) -> f64 {
    match shape {
        TargetAssignmentShape::Additive {
            offset_terms,
            coefficient,
            ..
        } => {
            let mut offset = 0.0;
            for &(register, scale) in offset_terms.iter() {
                offset += scale * values[register as usize];
            }
            -offset / coefficient
        }
        TargetAssignmentShape::Affine {
            offset_reg,
            coefficient_reg,
            offset_scale,
            coefficient_scale,
            ..
        } => {
            let coefficient = coefficient_scale
                * coefficient_reg.map_or(1.0, |register| values[register as usize]);
            -(offset_scale * values[*offset_reg as usize]) / coefficient
        }
        TargetAssignmentShape::Reciprocal {
            numerator_reg,
            numerator_scale,
            divisor_reg,
            divisor_scale,
            ..
        } => {
            -(numerator_scale * values[*numerator_reg as usize])
                / (divisor_scale * values[*divisor_reg as usize])
        }
        _ => unreachable!("affine, additive, and reciprocal shapes only"),
    }
}

#[test]
fn a_reciprocal_shape_solves_its_literal_quotient() {
    // rest = 4 solves 4 - 2 / y = 0 at y = 0.5: numerator -r1, divisor r0.
    let shape = TargetAssignmentShape::Reciprocal {
        target_y_index: 0,
        numerator_reg: 1,
        numerator_scale: -1.0,
        divisor_reg: 0,
        divisor_scale: 1.0,
        expr_eval_len: 2,
    };
    let registers = [4.0, 2.0];
    let value = eval_isolated_value(&shape, |register| Ok::<_, ()>(registers[register as usize]));
    assert_eq!(value, Some(Ok(0.5)));
    let zero = TargetAssignmentShape::Zero {
        target_y_index: 0,
        expr_eval_len: 0,
    };
    assert!(eval_isolated_value(&zero, |_| Ok::<_, ()>(0.0)).is_none());
}
