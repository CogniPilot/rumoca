use crate::{LinearOp, LinearOpSliceKind, SolveVisitor, TensorInputKind};

pub(super) fn validate(model: &crate::SolveModel) -> Result<(), &'static str> {
    let mut memory = model
        .problem
        .events
        .condition_memory_parameter_indices
        .clone();
    memory.extend(
        model
            .problem
            .discrete
            .update_targets
            .iter()
            .filter_map(|slot| match slot {
                crate::ScalarSlot::P { index, .. } => Some(*index),
                _ => None,
            }),
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
            Err("continuous kernel reads assertion condition memory")
        } else {
            Ok(())
        }
    }
}
