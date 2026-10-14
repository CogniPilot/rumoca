//! C admission of the compiler's relation-only initialization mode.

#[cfg(test)]
mod tests;
mod typed_relation;

use crate::parameter_reads::scalar_parameter_output as parameter;
use crate::{LinearOp, LinearOpSliceKind, SolveModel, SolveVisitor};

pub(super) fn validate(model: &SolveModel) -> Result<(), &'static str> {
    let Some(flag) = model.problem.solve_layout.initial_event_parameter_index else {
        return Ok(());
    };
    let mut reads = ModeReads {
        flag,
        memory: &model.problem.solve_layout.relation_memory_parameter_indices,
        calls: &model.pure_calls,
        seed_offset: model.problem.layout.y_scalars(),
        total: 0,
        guards: 0,
        unknown: false,
    };
    match reads.visit_solve_model(model) {
        Ok(()) => {}
        Err(never) => match never {},
    }
    if reads.unknown {
        return Err("the C profile cannot prove compact reads independent of initialization mode");
    }
    if reads.total != reads.guards {
        return Err(
            "the C profile cannot execute mode reads outside checked relation regions (including authored initial()) yet",
        );
    }
    Ok(())
}

struct ModeReads<'a> {
    flag: usize,
    memory: &'a [usize],
    calls: &'a crate::SolvePureCallTable,
    seed_offset: usize,
    total: usize,
    guards: usize,
    unknown: bool,
}

impl SolveVisitor for ModeReads<'_> {
    type Error = std::convert::Infallible;

    fn visit_compute_node(
        &mut self,
        index: usize,
        node: &crate::ComputeNode,
    ) -> Result<(), Self::Error> {
        self.unknown |= crate::parameter_reads::affine_parameter_read_limit(node)
            .map_or(true, |limit| limit.is_some_and(|end| self.flag < end));
        crate::visitor::walk_compute_node(self, index, node)
    }

    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        self.total += usize::from(
            crate::parameter_reads::parameter_read_range(op)
                .is_some_and(|run| run.contains(&self.flag)),
        );
        if let LinearOp::FunctionConditional { program, .. } = op
            && let [arm] = program.arms.as_ref()
            && mode_parameter(&arm.condition) == Some(self.flag)
            && parameter_seed(&arm.condition, self.seed_offset)
            && memory_output(&program.fallback, self.seed_offset)
                .is_some_and(|p| self.memory.contains(&p))
            && comparison_output(&arm.result, self.calls)
        {
            self.guards += 1;
        }
        Ok(())
    }
}

fn mode_parameter(ops: &[LinearOp]) -> Option<usize> {
    parameter(ops).or_else(|| match ops {
        [
            LinearOp::LoadP { dst, index },
            LinearOp::Const {
                dst: tangent,
                value,
            },
            LinearOp::StoreOutput { src },
        ] if dst == src && tangent != dst && value.to_bits() == 0 => Some(*index),
        _ => None,
    })
}

fn parameter_seed(ops: &[LinearOp], offset: usize) -> bool {
    match ops {
        [
            LinearOp::LoadP { index, .. },
            LinearOp::LoadSeed { index: seed, .. },
            ..,
        ] => offset.checked_add(*index) == Some(*seed),
        _ => true,
    }
}

fn memory_output(ops: &[LinearOp], offset: usize) -> Option<usize> {
    if let Some(index) = mode_parameter(ops) {
        return parameter_seed(ops, offset).then_some(index);
    }
    match ops {
        [
            LinearOp::LoadP { dst, index },
            LinearOp::LoadSeed {
                dst: tangent,
                index: seed,
            },
            LinearOp::StoreOutput { src },
            LinearOp::StoreOutput { src: seed_src },
        ] if dst == src
            && tangent == seed_src
            && dst != tangent
            && offset.checked_add(*index) == Some(*seed) =>
        {
            Some(*index)
        }
        [
            LinearOp::LoadP { dst, index },
            LinearOp::Const {
                dst: tangent,
                value,
            },
            LinearOp::StoreOutput { src },
            LinearOp::StoreOutput { src: tangent_src },
        ] if dst == src && tangent == tangent_src && dst != tangent && value.to_bits() == 0 => {
            Some(*index)
        }
        _ => None,
    }
}

fn comparison_output(ops: &[LinearOp], calls: &crate::SolvePureCallTable) -> bool {
    let (primal, tangent) = match ops {
        [
            ..,
            LinearOp::StoreOutput { src: primal },
            LinearOp::StoreOutput { src: tangent },
        ] => (*primal, Some(*tangent)),
        [.., LinearOp::StoreOutput { src }] => (*src, None),
        _ => return false,
    };
    last_writer(ops, primal).is_some_and(|op| {
        matches!(op, LinearOp::Compare { .. }) || typed_relation::comparison(op, primal, calls)
    }) && tangent.is_none_or(|register| {
        matches!(last_writer(ops, register),
            Some(LinearOp::Const { value, .. }) if value.to_bits() == 0.0f64.to_bits())
    })
}

fn last_writer(ops: &[LinearOp], register: crate::Reg) -> Option<&LinearOp> {
    ops.iter().rev().find(|op| {
        op.dst_register().is_some_and(|start| {
            register.checked_sub(start).is_some_and(|offset| {
                usize::try_from(offset).is_ok_and(|offset| offset < op.dst_register_count())
            })
        })
    })
}
