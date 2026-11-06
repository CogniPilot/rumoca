//! Final materialization of a model state Jacobian for the selected layout.

use super::{SolveTargetLayout, SolveTargetSelection};
use crate::StoredStateJacobianOutputOrder;
use crate::typed_program::compact_placement::{
    CompactJacobianPlacement, materialize_dense_model_jacobian,
};
use crate::typed_program::{ProgramRegister, SolveProgramConstructionError, TypedProgramBuilder};
use rumoca_core::Span;

impl SolveTargetSelection<'_> {
    /// The selected target's declared scalar expansion bound. A product whose
    /// compact nodes would expand past it is not issued for this target.
    #[must_use]
    pub const fn max_unrolled_operations(&self) -> usize {
        self.profile.max_unrolled_operations as usize
    }

    /// Place compact node results and scalar cells into the selected layout.
    /// `zero` is the constructor's existing zero, reused for absent cells.
    pub fn materialize_model_jacobian<'program>(
        &self,
        builder: &mut TypedProgramBuilder<'program>,
        compact: &[(&CompactJacobianPlacement, ProgramRegister<'program>)],
        scalar: &[Option<ProgramRegister<'program>>],
        zero: ProgramRegister<'program>,
        order: &StoredStateJacobianOutputOrder<'_>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        match self.layout() {
            SolveTargetLayout::DenseHostV1 => {
                materialize_dense_model_jacobian(builder, compact, scalar, zero, order, provenance)
            }
        }
    }
}
