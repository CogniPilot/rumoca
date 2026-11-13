//! Canonical complete source-program parameter reads, shared by preparation.
use crate::LinearOp;
use std::collections::BTreeSet;

/// Exact ordered parameter coordinates read anywhere in this original program.
pub fn program_parameter_indices(row: &[LinearOp]) -> Vec<usize> {
    let mut indices = BTreeSet::new();
    collect_parameter_indices(row, &mut indices);
    indices.into_iter().collect()
}

fn collect_parameter_indices(row: &[LinearOp], indices: &mut BTreeSet<usize>) {
    for op in row {
        match op {
            LinearOp::LoadP { index, .. } => {
                indices.insert(*index);
            }
            LinearOp::LoadIndexedP { base, count, .. } => {
                indices.extend(*base..base.saturating_add(*count));
            }
            LinearOp::TensorLoad {
                input: crate::TensorInputKind::P,
                input_start,
                count,
                ..
            } => {
                indices.extend(*input_start..input_start.saturating_add(*count));
            }
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                for region in program.regions() {
                    collect_parameter_indices(region, indices);
                }
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    collect_parameter_indices(&arm.condition, indices);
                    collect_parameter_indices(&arm.result, indices);
                }
                collect_parameter_indices(&program.fallback, indices);
            }
            _ => {}
        }
    }
}
