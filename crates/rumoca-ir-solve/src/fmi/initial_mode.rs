//! C admission of the compiler's initialization-mode reads.

#[cfg(test)]
mod tests;
mod typed_relation;

use crate::parameter_reads::scalar_parameter_output as parameter;
use crate::{
    LinearOp, LinearOpSliceKind, SolveArtifacts, SolveModel, SolveProblem, SolvePureCallTable,
    SolveVisitor,
};

pub(super) fn validate(model: &SolveModel) -> Result<(), &'static str> {
    admit(
        &model.problem,
        &model.artifacts,
        &model.pure_calls,
        |reads| reads.visit_solve_model(model),
    )
}

/// `validate` for a scalar target that renders a problem and its artifacts
/// without a retained model: every read of the initialization-mode flag must
/// sit in a checked relation region.
pub fn admit_initial_mode(
    problem: &SolveProblem,
    artifacts: &SolveArtifacts,
    calls: &SolvePureCallTable,
) -> Result<(), &'static str> {
    admit(problem, artifacts, calls, |reads| {
        reads.visit_solve_problem(problem)?;
        reads.visit_solve_artifacts(artifacts)
    })
}

fn admit(
    problem: &SolveProblem,
    artifacts: &SolveArtifacts,
    calls: &SolvePureCallTable,
    visit: impl FnOnce(&mut ModeReads<'_>) -> Result<(), std::convert::Infallible>,
) -> Result<(), &'static str> {
    let Some(flag) = problem.solve_layout.initial_event_parameter_index else {
        return Ok(());
    };
    let mut reads = ModeReads {
        model: Some((problem, artifacts)),
        in_sample: false,
        flag,
        memory: &problem.solve_layout.relation_memory_parameter_indices,
        calls,
        seed_offset: problem.layout.y_scalars(),
        total: 0,
        guards: 0,
        unknown: false,
    };
    match visit(&mut reads) {
        Ok(()) => {}
        Err(never) => match never {},
    }
    if reads.unknown {
        return Err("the C profile cannot prove compact reads independent of initialization mode");
    }
    if reads.total != reads.guards {
        return Err(
            "the C profile cannot execute mode reads outside checked relation or sampled assignment regions (including authored initial()) yet",
        );
    }
    Ok(())
}

struct ModeReads<'a> {
    /// The problem and artifacts the visited blocks belong to.
    model: Option<(&'a SolveProblem, &'a SolveArtifacts)>,
    in_sample: bool,
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

    fn visit_scalar_program_block(
        &mut self,
        block: &crate::ScalarProgramBlock,
    ) -> Result<(), Self::Error> {
        let Some((problem, artifacts)) = self.model else {
            return crate::visitor::walk_scalar_program_block(self, block);
        };
        let discrete = &problem.discrete;
        let artifacts = &artifacts.discrete;
        let guarded_jvp = artifacts
            .guarded_jacobian_v
            .as_ref()
            .is_some_and(|b| std::ptr::eq(b, block));
        let condition_rows = std::ptr::eq(block, &discrete.rhs)
            || artifacts
                .rhs_jacobian_v
                .as_ref()
                .is_some_and(|b| std::ptr::eq(b, block));
        for (i, ops) in block.programs().iter().enumerate() {
            // Both primal and directional programs use the same current mode
            // during the initial clock event. Its isolated edge-memory copy
            // is seeded through the existing condition-memory lifecycle.
            self.in_sample = (guarded_jvp
                && discrete
                    .guarded_assignments
                    .get(i)
                    .is_some_and(|g| g.clock_owner().is_some()))
                || (condition_rows
                    && discrete
                        .rhs
                        .programs()
                        .get(i)
                        .is_some_and(|primal| mode_parameter(primal) == Some(self.flag))
                    && block
                        .program_contiguous_output_range(i)
                        .is_some_and(|rows| {
                            rows.len() == 1
                                && discrete.row_roles.get(rows.start)
                                    == Some(&crate::DiscreteRowRole::ConditionMemory)
                        }));
            self.visit_scalar_program(i, block.program_span(i), ops)?;
        }
        self.in_sample = false;
        Ok(())
    }

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
        kind: LinearOpSliceKind,
        _index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        let sampled = self.in_sample
            || matches!(kind, LinearOpSliceKind::GuardedAssignmentProgram { program_index, .. } if self.model.is_some_and(|(problem, _)| problem.discrete.guarded_assignments[program_index].clock_owner().is_some()));
        if sampled && matches!(op, LinearOp::LoadP { index, .. } if *index == self.flag) {
            return Ok(());
        }
        self.total += usize::from(
            crate::parameter_reads::parameter_read_range(op)
                .is_some_and(|run| run.contains(&self.flag)),
        );
        if !sampled
            && let LinearOp::FunctionConditional { program, .. } = op
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
