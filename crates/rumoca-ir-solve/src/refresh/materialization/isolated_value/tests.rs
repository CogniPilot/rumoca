use std::sync::Arc;

use super::*;
use crate::LinearOp;
use crate::refresh::materialize_target_assignment;

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

#[test]
fn each_shape_emits_its_minimal_operation_count() {
    let cases: [(TargetAssignmentShape, usize); 11] = [
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
        // A reciprocal divides by its register and keeps the guard.
        (reciprocal(-1.0, 1.0), 5),
    ];
    for (shape, count) in cases {
        let (operations, result) = materialized(&shape);
        assert_eq!(operations.len(), count, "{shape:?}: {operations:?}");
        if count == 0 {
            assert_eq!(result, 1, "{shape:?} returns its register");
        }
    }
}

/// The form folds the sign of a unit divisor into a lone term and keeps the
/// parts of several terms or another divisor as they are.
#[test]
fn the_form_folds_a_unit_divisor_into_a_lone_term() {
    let lone = additive(&[(1, 2.5)], 1.0);
    let (terms, divisor) = isolated_parts(&lone).expect("additive parts");
    assert_eq!(terms.collect::<Vec<_>>(), [IsolatedTerm::Scaled(1, 2.5)]);
    assert_eq!(divisor, IsolatedDivisor::Negate);
    assert_eq!(
        IsolatedValue::of(&lone),
        Some(IsolatedValue {
            terms: vec![IsolatedTerm::Scaled(1, -2.5)],
            divisor: IsolatedDivisor::Keep,
        })
    );
    let pair = additive(&[(1, 1.0), (2, -1.0)], 4.0);
    assert_eq!(
        IsolatedValue::of(&pair),
        Some(IsolatedValue {
            terms: vec![IsolatedTerm::Register(1), IsolatedTerm::Negated(2)],
            divisor: IsolatedDivisor::Multiply(-0.25),
        })
    );
    let quotient = reciprocal(-1.0, 3.0);
    let (terms, divisor) = isolated_parts(&quotient).expect("reciprocal parts");
    assert_eq!(terms.collect::<Vec<_>>(), [IsolatedTerm::Negated(0)]);
    assert_eq!(
        divisor,
        IsolatedDivisor::DivideRegister {
            register: 3,
            scale: 3.0
        }
    );
    let zero = TargetAssignmentShape::Zero {
        target_y_index: 0,
        expr_eval_len: 0,
    };
    assert!(isolated_parts(&zero).is_none() && IsolatedValue::of(&zero).is_none());
}
