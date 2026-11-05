//! C execution capability admission over the retained Solve typed owners.

use super::FmiCCodegenError;
use crate::{SolveFunctionTarget, SolvePureCallTable};

/// Every emitted owner, including directional bodies and nested regions, must
/// fit the C profile before a checked C view can reach any renderer.
pub(super) fn admit(table: &SolvePureCallTable) -> Result<(), FmiCCodegenError> {
    for owner in table.owners() {
        SolveFunctionTarget::CFmi
            .admit_finite_domain(owner.body())
            .map_err(|error| FmiCCodegenError(error.message()))?;
        if let Some(directional) = owner.directional() {
            SolveFunctionTarget::CFmi
                .admit_finite_domain(directional.body())
                .map_err(|error| FmiCCodegenError(error.message()))?;
        }
    }
    Ok(())
}
