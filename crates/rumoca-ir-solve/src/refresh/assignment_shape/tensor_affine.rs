//! Source-bound affine rules; tensor coordinates are not stored as a new graph.

pub(in crate::refresh) mod operation;

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use super::{ScalarProgramYDependency, producer_position};
use crate::{LinearOp, Reg, TensorInputKind};
use operation::{OperandRange, ProjectionOperation};

/// Compact offset/coefficient relation for one selected residual output.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AffineTensorProjection {
    pub(in crate::refresh) output: Reg,
    pub(in crate::refresh) steps: Arc<[(Reg, ProjectionRule)]>,
    independent_ranges: Arc<[(Reg, usize)]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(in crate::refresh) enum ProjectionRule {
    Independent,
    Target { offset: usize },
    Linear,
    LeftProduct,
    RightProduct,
}

impl AffineTensorProjection {
    pub const fn output_register(&self) -> Reg {
        self.output
    }

    pub fn value_registers(&self) -> impl Iterator<Item = Reg> + '_ {
        self.independent_ranges.iter().flat_map(|&(start, count)| {
            (0..count).filter_map(move |offset| start.checked_add(u32::try_from(offset).ok()?))
        })
    }
}

pub(super) fn derive(
    program: &[LinearOp],
    output: Reg,
    target: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<AffineTensorProjection> {
    let mut pending = BTreeSet::from([producer_position(program, output)?]);
    let mut steps = BTreeMap::new();
    let mut independent_ranges = BTreeSet::new();
    while let Some(position) = pending.pop_last() {
        if steps.contains_key(&position) {
            continue;
        }
        let operation = program.get(position)?;
        let start = operation.dst_register()?;
        let count = operation.dst_register_count();
        let rule = projection_rule(operation, target, dependencies)?;
        reject_zero_scalar_factor(program.get(..position)?, operation, rule)?;
        match rule {
            ProjectionRule::Independent => {
                independent_ranges.insert((start, count));
            }
            ProjectionRule::Target { offset } => {
                independent_ranges.insert((start, offset));
                independent_ranges.insert((
                    start.checked_add(u32::try_from(offset.checked_add(1)?).ok()?)?,
                    count.checked_sub(offset.checked_add(1)?)?,
                ));
            }
            _ => {
                for operand in ProjectionOperation::new(operation)?
                    .operands()
                    .into_iter()
                    .flatten()
                {
                    require_producers(program.get(..position)?, operand, &mut pending)?;
                }
            }
        }
        steps.insert(position, rule);
    }
    // Constant additive coefficients already have their canonical weighted
    // selection. This relation extends that vocabulary to bilinear operations.
    if !steps.values().any(|rule| {
        matches!(
            rule,
            ProjectionRule::LeftProduct | ProjectionRule::RightProduct
        )
    }) {
        return None;
    }
    Some(AffineTensorProjection {
        output,
        steps: steps
            .into_iter()
            .map(|(position, rule)| Some((program.get(position)?.dst_register()?, rule)))
            .collect::<Option<Vec<_>>>()?
            .into(),
        independent_ranges: independent_ranges
            .into_iter()
            .filter(|(_, count)| *count != 0)
            .collect(),
    })
}

fn reject_zero_scalar_factor(
    program: &[LinearOp],
    operation: &LinearOp,
    rule: ProjectionRule,
) -> Option<()> {
    let LinearOp::Binary {
        op: crate::BinaryOp::Mul,
        lhs,
        rhs,
        ..
    } = operation
    else {
        return Some(());
    };
    let factor = match rule {
        ProjectionRule::LeftProduct => *rhs,
        ProjectionRule::RightProduct => *lhs,
        _ => return Some(()),
    };
    match program.get(producer_position(program, factor)?)? {
        LinearOp::Const { value, .. } if *value == 0.0 || !value.is_finite() => None,
        _ => Some(()),
    }
}

fn projection_rule(
    operation: &LinearOp,
    target: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<ProjectionRule> {
    let range = OperandRange::new(operation.dst_register()?, operation.dst_register_count());
    if range.independent_of(target, dependencies) {
        return Some(ProjectionRule::Independent);
    }
    match *operation {
        LinearOp::LoadY { index, .. } if index == target => {
            return Some(ProjectionRule::Target { offset: 0 });
        }
        LinearOp::TensorLoad {
            input: TensorInputKind::Y,
            input_start,
            count,
            lanes: 1,
            ..
        } => {
            let offset = target.checked_sub(input_start)?;
            return (offset < count).then_some(ProjectionRule::Target { offset });
        }
        _ => {}
    }
    ProjectionOperation::new(operation)?.rule(target, dependencies)
}

fn require_producers(
    program: &[LinearOp],
    range: OperandRange,
    pending: &mut BTreeSet<usize>,
) -> Option<()> {
    let mut cursor = range.start;
    let end = range.end()?;
    while cursor < end {
        let position = producer_position(program, cursor)?;
        let operation = &program[position];
        let operation_end = operation
            .dst_register()?
            .checked_add(u32::try_from(operation.dst_register_count()).ok()?)?;
        pending.insert(position);
        cursor = operation_end.min(end);
    }
    Some(())
}
