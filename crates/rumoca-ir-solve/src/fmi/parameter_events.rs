//! Borrowed event admission for hosts with no continuous event locator.

use super::FmiRuntimeView;

/// The model cannot be driven by a parameter-only event host.
#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct FmiParameterEventError(&'static str);

impl FmiRuntimeView<'_> {
    /// Check the parameter-determined event partition (ME-EVENT-003).
    ///
    /// Uses the same proof as the C component. Unlike that component, a host
    /// using this stricter admission may not support changing error assertions
    /// at accepted points, so those assertions are refused as well. Successful
    /// admission does not waive initialization, parameter-change evaluation,
    /// or the reporting of warning-level actions at accepted points.
    pub fn validate_parameter_only_events(&self) -> Result<(), FmiParameterEventError> {
        let order = super::static_assertions::validate(self.model)
            .map_err(|refusal| FmiParameterEventError(refusal.message()))?;
        if order.accepted_actions.iter().any(|&accepted| accepted) {
            return Err(FmiParameterEventError(
                "a changing error assertion needs accepted-point checks",
            ));
        }
        Ok(())
    }
}
