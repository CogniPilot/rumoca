//! Isolation of a target that enters its residual only as the denominator of
//! a nonzero literal quotient, `rest + s * (c / target)`.
//!
//! With `c` a nonzero finite literal and `rest` independent of the target, the
//! residual vanishes exactly when `target = -(s * c) / rest`; a zero or
//! nonfinite `rest` admits no finite solution. The literal is proven nonzero
//! when the certificate is issued, so the only singularity left to an
//! evaluation is its divisor, which every evaluator guards.

use super::{
    ProgramPrefix, ScalarProgramYDependency, binary_operands, producer, producer_position,
    strip_affine_output_wrappers, target_load,
};
use crate::{BinaryOp, LinearOp, Reg, TargetAssignmentShape};

pub(super) fn derive(
    program: ProgramPrefix<'_>,
    output: Reg,
    target: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<TargetAssignmentShape> {
    let (output, output_scale) = strip_affine_output_wrappers(program, output);
    let (op, lhs, rhs) = binary_operands(program, output)?;
    let rhs_scale = match op {
        BinaryOp::Add => output_scale,
        BinaryOp::Sub => -output_scale,
        _ => return None,
    };
    [
        ((lhs, output_scale), (rhs, rhs_scale)),
        ((rhs, rhs_scale), (lhs, output_scale)),
    ]
    .into_iter()
    .find_map(|(quotient, rest)| reciprocal_shape(program, quotient, rest, target, dependencies))
}

fn reciprocal_shape(
    program: ProgramPrefix<'_>,
    (quotient, quotient_scale): (Reg, f64),
    (rest, rest_scale): (Reg, f64),
    target: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<TargetAssignmentShape> {
    let (BinaryOp::Div, numerator, denominator) = binary_operands(program, quotient)? else {
        return None;
    };
    let load = target_load(program, denominator)?;
    if load.index != target
        || !nonzero_literal(program, numerator)
        || dependencies.depends_on(rest, target)
        || quotient_scale == 0.0
        || rest_scale == 0.0
    {
        return None;
    }
    let value_end = producer_position(program, numerator)?
        .max(producer_position(program, rest)?)
        .checked_add(1)?;
    Some(TargetAssignmentShape::Reciprocal {
        target_y_index: target,
        numerator_reg: numerator,
        numerator_scale: quotient_scale,
        divisor_reg: rest,
        divisor_scale: rest_scale,
        expr_eval_len: value_end.max(load.required_eval_len),
    })
}

fn nonzero_literal(program: ProgramPrefix<'_>, register: Reg) -> bool {
    matches!(
        producer(program, register),
        Some(LinearOp::Const { value, .. }) if *value != 0.0 && value.is_finite()
    )
}

#[cfg(test)]
mod tests {
    use crate::{BinaryOp, LinearOp, TargetAssignmentShape};

    use super::super::canonical_assignment_shape_for_output;

    /// `rest - c / y0` over a parameter `rest` and literal `c`.
    fn program(numerator: f64, rest_reads_target: bool) -> Vec<LinearOp> {
        vec![
            if rest_reads_target {
                LinearOp::LoadY { dst: 0, index: 0 }
            } else {
                LinearOp::LoadP { dst: 0, index: 0 }
            },
            LinearOp::Const {
                dst: 1,
                value: numerator,
            },
            LinearOp::LoadY { dst: 2, index: 0 },
            LinearOp::Binary {
                dst: 3,
                op: BinaryOp::Div,
                lhs: 1,
                rhs: 2,
            },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ]
    }

    #[test]
    fn a_literal_quotient_of_the_target_isolates_its_reciprocal() {
        let shape = canonical_assignment_shape_for_output(&program(2.0, false), 0, 0);
        assert_eq!(
            shape,
            Some(TargetAssignmentShape::Reciprocal {
                target_y_index: 0,
                numerator_reg: 1,
                numerator_scale: -1.0,
                divisor_reg: 0,
                divisor_scale: 1.0,
                expr_eval_len: 2,
            })
        );
        let shape = shape.unwrap();
        // y = -(-r1) / r0, so rest = 4 solves 4 - 2 / y = 0 at y = 0.5.
        assert_eq!(
            crate::IsolatedValue::of(&shape),
            Some(crate::IsolatedValue {
                terms: vec![crate::IsolatedTerm::Negated(1)],
                divisor: crate::IsolatedDivisor::DivideRegister {
                    register: 0,
                    scale: 1.0
                },
            })
        );
        assert!(!shape.constant_coefficient());
        assert_eq!(shape.value_registers().collect::<Vec<_>>(), [1, 0]);
    }

    #[test]
    fn a_zero_numerator_or_a_target_dependent_rest_issues_no_reciprocal() {
        assert_eq!(
            canonical_assignment_shape_for_output(&program(0.0, false), 0, 0),
            None
        );
        assert_eq!(
            canonical_assignment_shape_for_output(&program(2.0, true), 0, 0),
            None
        );
    }

    #[test]
    fn the_quotient_may_stand_on_either_side_of_a_sum() {
        let mut operations = program(3.0, false);
        operations[4] = LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 3,
            rhs: 0,
        };
        let Some(TargetAssignmentShape::Reciprocal {
            numerator_scale,
            divisor_scale,
            ..
        }) = canonical_assignment_shape_for_output(&operations, 0, 0)
        else {
            panic!("a quotient on the left isolates its reciprocal");
        };
        assert_eq!((numerator_scale, divisor_scale), (1.0, 1.0));
    }
}
