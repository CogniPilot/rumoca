//! Programs that are equal up to the indices of their state and parameter
//! loads.
//!
//! The rows of one array equation slice to programs that run the same
//! operations on registers of the same numbers and differ only in which
//! element of `y` or `p` they load. Such programs form a class: one program
//! with its load indices taken from a per-member table computes, for each
//! member, exactly that member's program.

use super::*;
use std::collections::BTreeMap;

/// Programs equal up to their load indices: `members` index the programs
/// given to [`load_index_classes`], in order, and `indices[k]` lists the
/// `LoadY` and `LoadP` indices of member `k` in operation order. The first
/// member is the class's representative.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoadIndexClass {
    pub members: Vec<usize>,
    pub indices: Vec<Vec<usize>>,
}

/// The classes of at least two of `programs` that differ only in the
/// indices of their `LoadY` and `LoadP` operations, in order of their first
/// member. A program takes part only when every operation is one whose
/// fields are registers, integers and operator kinds, with constants
/// compared by their bits; and a member joins a class only when the
/// representative with its indices put back is that member exactly.
#[must_use]
pub fn load_index_classes(programs: &[Vec<LinearOp>]) -> Vec<LoadIndexClass> {
    // Candidate classes by the sequence of operation kinds, then by shape.
    let mut buckets = BTreeMap::<Vec<&'static str>, Vec<(Vec<LinearOp>, LoadIndexClass)>>::new();
    for (member, program) in programs.iter().enumerate() {
        let Some((shape, indices)) = index_shape(program) else {
            continue;
        };
        let key = program.iter().map(LinearOp::kind_name).collect();
        let classes = buckets.entry(key).or_default();
        match classes
            .iter_mut()
            .find(|(known, _)| same_ops(known, &shape))
        {
            // The representative's shape with this member's indices must be
            // this member's program, bit for bit.
            Some((known, class)) => {
                let rebuilt = instantiate(known, &indices);
                if rebuilt.is_some_and(|rebuilt| same_ops(&rebuilt, program)) {
                    class.members.push(member);
                    class.indices.push(indices);
                }
            }
            None if instantiate(&shape, &indices)
                .is_some_and(|rebuilt| same_ops(&rebuilt, program)) =>
            {
                let class = LoadIndexClass {
                    members: vec![member],
                    indices: vec![indices],
                };
                classes.push((shape, class));
            }
            None => {}
        }
    }
    let mut classes: Vec<LoadIndexClass> = buckets
        .into_values()
        .flatten()
        .map(|(_, class)| class)
        .filter(|class| class.members.len() > 1)
        .collect();
    classes.sort_by_key(|class| class.members[0]);
    classes
}

/// `program` with every load index replaced by its ordinal, and the indices
/// it replaced, or `None` when an operation is not one the classes model.
fn index_shape(program: &[LinearOp]) -> Option<(Vec<LinearOp>, Vec<usize>)> {
    let mut indices = Vec::new();
    let mut shape = Vec::with_capacity(program.len());
    for op in program {
        let mut op = op.clone();
        match &mut op {
            LinearOp::LoadY { index, .. } | LinearOp::LoadP { index, .. } => {
                indices.push(std::mem::replace(index, indices.len()));
            }
            LinearOp::Const { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::TensorLoad { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorIntegerPower { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. } => {}
            _ => return None,
        }
        shape.push(op);
    }
    Some((shape, indices))
}

/// `shape` with its load ordinals replaced by `indices`, or `None` when
/// they do not match.
fn instantiate(shape: &[LinearOp], indices: &[usize]) -> Option<Vec<LinearOp>> {
    let mut program = shape.to_vec();
    let mut used = 0usize;
    for op in &mut program {
        if let LinearOp::LoadY { index, .. } | LinearOp::LoadP { index, .. } = op {
            if *index != used {
                return None;
            }
            *index = *indices.get(used)?;
            used += 1;
        }
    }
    (used == indices.len()).then_some(program)
}

/// Operation-by-operation equality, constants by their bits, so `0.0` and
/// `-0.0` are different programs.
fn same_ops(lhs: &[LinearOp], rhs: &[LinearOp]) -> bool {
    lhs.len() == rhs.len()
        && lhs.iter().zip(rhs).all(|pair| match pair {
            (
                LinearOp::Const { dst, value },
                LinearOp::Const {
                    dst: other,
                    value: bits,
                },
            ) => dst == other && value.to_bits() == bits.to_bits(),
            (lhs, rhs) => lhs == rhs,
        })
}

#[cfg(test)]
mod tests;
