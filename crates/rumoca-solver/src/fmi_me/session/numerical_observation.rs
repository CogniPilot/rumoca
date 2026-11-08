//! Observe existing lifecycle calls without changing their activation or restoration.

use super::*;
use crate::fmi_me::numerical_observation::{FailureCategory, HostWriter, LifecycleCall};

impl MeComponentHost<'_> {
    /// Attach an optional write-only numerical-settings endpoint before initialization.
    /// The endpoint supplies no numerical capability and cannot affect the result.
    pub fn into_session_with_numerical_observation<'lease, 'plugin>(
        self,
        backend: Option<Box<dyn MeIntegratorBackend + 'plugin>>,
        observation: Option<HostWriter>,
    ) -> Result<MeSimulationSession<'lease, 'plugin>, MeSessionError>
    where
        Self: 'lease,
    {
        let state_count = self.host.state_count;
        // ME-ZERO-001's arity rule, decided from the linked component's own
        // width and the presence of a plugin. It asks the backend nothing: the
        // thin ME-INT-001 surface has no identity operation to ask
        let backend: Box<dyn MeIntegratorBackend + 'plugin> = match backend {
            Some(_) if state_count == 0 => {
                return Err(MeSessionError::PluginArity {
                    state_count,
                    mismatch: MePluginArity::RejectsANumericalPlugin,
                });
            }
            Some(backend) => backend,
            None if state_count == 0 => Box::new(TimeOnlyIntegrator::new()),
            None => {
                return Err(MeSessionError::PluginArity {
                    state_count,
                    mismatch: MePluginArity::RequiresANumericalPlugin,
                });
            }
        };
        let mut session = MeSimulationSession {
            host: self.host,
            owned_component: self.owned_component,
            backend,
            numerical_observation: observation,
            scan_workspace: RefCell::new(RootScanWorkspace::default()),
            deferred: None,
            snapped_yield: None,
            lease: PhantomData,
        };
        session.start_plugin()?;
        Ok(session)
    }
}

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
