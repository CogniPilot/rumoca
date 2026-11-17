//! Exact source relation for one spatial articulated joint reduction, not a tree admission.
mod outer;
mod pattern;
use super::{SolvePureCallOutput, SolvePureCallOutputKind};
use crate::{SolveRealFormat, SolveScalarType, SolveValueType, TypedProgram};

/// The checked source computes U=I*S, D=(S*U+armature)+increment,
/// u=effort-S*p, Ia=I-U*U'/D, pa=(p+Ia*c)+U*(u/D).
/// This proves neither positive inertia nor the increment's h*d provenance,
/// complete physical topology, state basis, or numerical/runtime admission.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ArticulatedJointReduction(());

pub(super) fn derive(
    body: &TypedProgram,
    inputs: &[SolveValueType],
    outputs: &[SolvePureCallOutput],
) -> Option<ArticulatedJointReduction> {
    let input_shapes: &[&[u32]] = &[&[6, 6], &[6], &[], &[], &[], &[6], &[6]];
    let output_shapes: &[&[u32]] = &[&[6], &[], &[], &[6, 6], &[6]];
    if inputs.len() != input_shapes.len() || outputs.len() != output_shapes.len() {
        return None;
    }
    if !inputs
        .iter()
        .zip(input_shapes)
        .all(|(t, s)| real_shape(t, s))
        || !outputs.iter().zip(output_shapes).all(|(o, s)| {
            o.kind() == SolvePureCallOutputKind::Result && real_shape(o.value_type(), s)
        })
    {
        return None;
    }
    pattern::matches(body).then_some(ArticulatedJointReduction(()))
}
fn real_shape(ty: &SolveValueType, shape: &[u32]) -> bool {
    ty.element_type()
        == SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
        }
        && ty.dimensions() == shape
}
