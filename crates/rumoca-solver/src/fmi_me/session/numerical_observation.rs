//! Observe existing lifecycle calls without changing their activation or restoration.

use super::*;
use crate::fmi_me::numerical_observation::{FailureCategory, LifecycleCall};

impl MeSimulationSession<'_, '_> {
    pub(super) fn run_observed_lifecycle<T>(
        &mut self,
        kind: LifecycleCall,
        body: impl FnOnce(&mut dyn MeIntegratorBackend) -> Result<T, MeIntegrationError>,
    ) -> Result<T, MeSessionError> {
        let observation = self
            .numerical_observation
            .as_ref()
            .and_then(|writer| writer.begin(kind));
        let result = self.run_with_derivatives(body);
        if let Some(observation) = observation {
            match &result {
                Ok(_) => observation.finish(true),
                Err(error) => observation.finish_failure(session_failure_category(error)),
            }
        }
        result
    }

    pub(super) fn record_current_numerical_policy(&self) {
        let Some(writer) = &self.numerical_observation else {
            return;
        };
        match self.host.policy.as_ref() {
            Some(policy) => writer.current_policy(
                self.host.options.relative_tolerance(),
                self.host.options.absolute_tolerance(),
                policy.nominals(),
            ),
            None => writer.unavailable(),
        }
    }
}

fn session_failure_category(error: &MeSessionError) -> FailureCategory {
    match error {
        MeSessionError::Component(_) => FailureCategory::Component,
        MeSessionError::Integration(error) => integration_failure_category(error),
        MeSessionError::Timeout { .. } => FailureCategory::Timeout,
        MeSessionError::Allocation { .. } => FailureCategory::Allocation,
        MeSessionError::Options { .. }
        | MeSessionError::Contract { .. }
        | MeSessionError::PluginArity { .. } => FailureCategory::Contract,
        MeSessionError::RootApplicationUnavailable { .. }
        | MeSessionError::RootScanUnrepresentable { .. }
        | MeSessionError::EventIterationDiverged { .. }
        | MeSessionError::AcceptedPointLost { .. }
        | MeSessionError::SessionNotReusable { .. } => FailureCategory::HostLifecycle,
    }
}

fn integration_failure_category(error: &MeIntegrationError) -> FailureCategory {
    match error {
        MeIntegrationError::Component(_) => FailureCategory::Component,
        MeIntegrationError::ComponentDiscard { .. } => FailureCategory::ComponentDiscard,
        MeIntegrationError::StepSizeUnderflow { .. } => FailureCategory::StepSizeUnderflow,
        MeIntegrationError::Numerical { category, .. } => FailureCategory::Numerical(*category),
        MeIntegrationError::Contract { .. } => FailureCategory::Contract,
        MeIntegrationError::DerivativeCapabilityInactive { .. }
        | MeIntegrationError::DerivativeRefused => FailureCategory::DerivativeCapability,
        MeIntegrationError::Allocation { .. } => FailureCategory::Allocation,
    }
}
