use std::sync::Arc;

use super::*;
use crate::refresh::materialize_target_assignment;
use crate::{BinaryOp, LinearOp, UnaryOp};

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

/// The appended operations and the result register.
fn materialized(shape: &TargetAssignmentShape) -> (Vec<LinearOp>, u32) {
    let mut operations = prefix();
    let (result, _) = materialize_target_assignment(shape, &mut operations).expect("materializes");
    (operations.split_off(4), result)
}

#[test]
fn each_shape_emits_its_minimal_operation_count() {
    let cases: [(TargetAssignmentShape, usize); 10] = [
        // -(0 + (-1)*r) / 1 is r itself.
        (additive(&[(1, -1.0)], 1.0), 0),
        (additive(&[(1, 1.0)], -1.0), 0),
        (additive(&[(1, 1.0)], 1.0), 1),
        (additive(&[(1, 2.5)], 1.0), 2),
        (additive(&[(1, 1.0), (2, -1.0)], -1.0), 2),
        // A power-of-two coefficient multiplies by its exact reciprocal.
        (additive(&[(1, 3.0)], 4.0), 4),
        (additive(&[(1, 1.0)], 3.0), 3),
        (affine(1.0, None, 1.0), 1),
        // A register coefficient keeps the divide and the poison guard.
        (affine(1.0, Some(2), 1.0), 4),
        // A singular constant coefficient keeps the guard as well.
        (affine(1.0, None, 0.0), 5),
    ];
    for (shape, count) in cases {
        let (operations, result) = materialized(&shape);
        assert_eq!(operations.len(), count, "{shape:?}: {operations:?}");
        if count == 0 {
            assert_eq!(result, 1, "{shape:?} returns its register");
        }
    }
}

fn interpret(operations: &[LinearOp], registers: &mut Vec<f64>) {
    for operation in operations {
        let (dst, value) = match *operation {
            LinearOp::LoadY { dst, .. } => (dst, registers[dst as usize]),
            LinearOp::Const { dst, value } => (dst, value),
            LinearOp::Unary {
                dst,
                op: UnaryOp::Neg,
                arg,
            } => (dst, -registers[arg as usize]),
            LinearOp::Binary { dst, op, lhs, rhs } => {
                let (a, b) = (registers[lhs as usize], registers[rhs as usize]);
                let value = match op {
                    BinaryOp::Add => a + b,
                    BinaryOp::Sub => a - b,
                    BinaryOp::Mul => a * b,
                    BinaryOp::Div => a / b,
                    _ => unreachable!("isolators use the four arithmetic operations"),
                };
                (dst, value)
            }
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
        ];
        for shape in shapes {
            let (operations, result) = materialized(&shape);
            let mut registers = values.to_vec();
            interpret(&operations, &mut registers);
            let emitted = registers[result as usize];
            let read = |register: Reg| Ok::<f64, ()>(values[register as usize]);
            let evaluated = eval_isolated_value(&shape, read).unwrap().unwrap();
            let formed = IsolatedValue::of(&shape).unwrap().eval(read).unwrap();
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
        _ => unreachable!("affine and additive shapes only"),
    }
}
