use crate::{LinearOp, LinearOpSliceKind, SolveVisitor, TensorInputKind};

pub(super) fn validate(model: &crate::SolveModel) -> Result<(), &'static str> {
    let mut memory = model
        .problem
        .events
        .condition_memory_parameter_indices
        .clone();
    // A parameter-determined discrete equation is settled with the parameter
    // bindings, so the continuous kernel may read it; a condition memory is
    // event data it never reads.
    let discrete = &model.problem.discrete;
    memory.extend(
        discrete
            .update_targets
            .iter()
            .zip(&discrete.row_roles)
            .filter(|(_, role)| **role != crate::DiscreteRowRole::Equation)
            .filter_map(|(slot, _)| match slot {
                crate::ScalarSlot::P { index, .. } => Some(*index),
                _ => None,
            }),
    );
    memory.extend(
        model
            .problem
            .solve_layout
            .pre_param_bindings
            .iter()
            .map(|binding| binding.dest_p_index),
    );
    MemoryReads(&memory).visit_continuous_system(&model.problem.continuous)
}

struct MemoryReads<'a>(&'a [usize]);

impl SolveVisitor for MemoryReads<'_> {
    type Error = &'static str;
    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        let reads = match op {
            LinearOp::LoadP { index, .. } => self.0.contains(index),
            LinearOp::LoadIndexedP { base, count, .. } => self
                .0
                .iter()
                .any(|i| *i >= *base && i.saturating_sub(*base) < *count),
            LinearOp::TensorLoad {
                input: TensorInputKind::P,
                input_start,
                count,
                ..
            } => self
                .0
                .iter()
                .any(|i| *i >= *input_start && i.saturating_sub(*input_start) < *count),
            _ => false,
        };
        if reads {
            Err(
                "the continuous kernel reads a condition memory or a pre() value the C profile updates only at events",
            )
        } else {
            Ok(())
        }
    }
}
