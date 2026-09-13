use std::collections::BTreeMap;

use super::{ScalarProgramYDependency, binary_operands, producer_position, target_load_index};
use crate::{BinaryOp, LinearOp, Reg, TargetAssignmentShape, UnaryOp};

pub(super) fn derive(
    program: &[LinearOp],
    output: Reg,
    target: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<TargetAssignmentShape> {
    let position = producer_position(program, output)?;
    let mut pending = BTreeMap::from([((position, output), 1.0)]);
    let mut offsets = BTreeMap::new();
    let mut coefficient = 0.0;
    // Descending producer order combines all parents before visiting a shared
    // child. Repeated subexpressions therefore never expand into repeated trees.
    while let Some(((position, register), weight)) = pending.pop_last() {
        if weight == 0.0 {
            continue;
        }
        if !dependencies.depends_on(register, target) {
            accumulate(&mut offsets, register, weight)?;
        } else if target_load_index(program, register) == Some(target) {
            coefficient += weight;
            if !coefficient.is_finite() {
                return None;
            }
        } else {
            for (operand, scale) in additive_operands(program, register)?.into_iter().flatten() {
                let producer = producer_position(program.get(..position)?, operand)?;
                accumulate(&mut pending, (producer, operand), weight * scale)?;
            }
        }
    }
    if coefficient == 0.0 || !coefficient.is_finite() {
        return None;
    }
    Some(TargetAssignmentShape::Additive {
        target_y_index: target,
        offset_terms: offsets
            .into_iter()
            .filter(|(_, scale)| *scale != 0.0)
            .collect(),
        coefficient,
        expr_eval_len: program.len(),
    })
}

fn accumulate<K: Ord>(weights: &mut BTreeMap<K, f64>, key: K, weight: f64) -> Option<()> {
    if !weight.is_finite() {
        return None;
    }
    let value = weights.entry(key).or_default();
    *value += weight;
    value.is_finite().then_some(())
}

fn additive_operands(program: &[LinearOp], register: Reg) -> Option<[Option<(Reg, f64)>; 2]> {
    if let Some((op @ (BinaryOp::Add | BinaryOp::Sub), lhs, rhs)) =
        binary_operands(program, register)
    {
        return Some([
            Some((lhs, 1.0)),
            Some((rhs, if op == BinaryOp::Add { 1.0 } else { -1.0 })),
        ]);
    }
    match program.get(producer_position(program, register)?)? {
        LinearOp::Move { src, .. } => Some([Some((*src, 1.0)), None]),
        LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        } => Some([Some((*arg, -1.0)), None]),
        _ => None,
    }
}
