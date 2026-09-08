//! Exhaustive finite-domain tests for the production FMI ME operation relation.

use crate::fmi_me::lifecycle::{
    MeConfigurationCapability, MeLifecycle, MeLifecycleOperation, MeState,
};
use crate::fmi_me::{MeError, MeStage, resolve_me_stage};

const ME_STAGES: [MeStage; 5] = [
    MeStage::Instantiate,
    MeStage::Initialization,
    MeStage::EventIteration,
    MeStage::ManifoldProjection,
    MeStage::Integration,
];

fn expected_relation(
    capability: MeConfigurationCapability,
    state: MeState,
    operation: MeLifecycleOperation,
) -> Result<Option<MeState>, ()> {
    use MeLifecycleOperation as Operation;
    use MeState as State;

    let target = match (state, operation) {
        (State::Instantiated, Operation::EnterConfigurationMode)
            if capability != MeConfigurationCapability::Absent =>
        {
            Some(State::ConfigurationMode)
        }
        (State::ConfigurationMode, Operation::ExitConfigurationMode) => Some(State::Instantiated),
        (State::Instantiated, Operation::EnterInitializationMode) => {
            Some(State::InitializationMode)
        }
        (State::InitializationMode, Operation::ExitInitializationMode) => Some(State::EventMode),
        (State::EventMode, Operation::UpdateDiscreteStates) => Some(State::EventMode),
        (State::EventMode, Operation::EnterConfigurationMode)
            if capability == MeConfigurationCapability::TunableStructuralParameter =>
        {
            Some(State::ReconfigurationMode)
        }
        (State::ReconfigurationMode, Operation::ExitConfigurationMode) => Some(State::EventMode),
        (State::EventMode, Operation::EnterContinuousTimeMode) => Some(State::ContinuousTimeMode),
        (State::ContinuousTimeMode, Operation::EnterEventMode) => Some(State::EventMode),
        (
            State::EventMode | State::ReconfigurationMode | State::ContinuousTimeMode,
            Operation::Terminate,
        ) => Some(State::Terminated),
        (State::ContinuousTimeMode, Operation::SetTime)
        | (State::ContinuousTimeMode, Operation::SetContinuousStates)
        | (State::ContinuousTimeMode, Operation::CompletedIntegratorStep)
        | (
            State::InitializationMode
            | State::EventMode
            | State::ContinuousTimeMode
            | State::Terminated,
            Operation::GetContinuousStates
            | Operation::GetNominalsOfContinuousStates
            | Operation::GetContinuousStateDerivatives
            | Operation::GetDirectionalDerivative
            | Operation::GetEventIndicators,
        )
        | (
            State::Instantiated
            | State::InitializationMode
            | State::EventMode
            | State::ContinuousTimeMode
            | State::Terminated,
            Operation::GetFloat64,
        )
        | (
            State::Instantiated
            | State::ConfigurationMode
            | State::InitializationMode
            | State::EventMode
            | State::ReconfigurationMode
            | State::ContinuousTimeMode,
            Operation::SetFloat64,
        )
        | (_, Operation::GetFmuState | Operation::SetFmuState) => None,
        _ => return Err(()),
    };
    Ok(target)
}

fn property_operation_relation_is_exact(
    capability: MeConfigurationCapability,
    state: MeState,
    operation: MeLifecycleOperation,
) {
    let actual = MeLifecycle::relation_for_verification(capability, state, operation);
    match expected_relation(capability, state, operation) {
        Ok(expected) => assert_eq!(
            actual.expect("specified operation must be admitted"),
            expected
        ),
        Err(()) => {
            let violation = actual.expect_err("unspecified operation must be refused");
            assert_eq!(violation.state, state);
            assert_eq!(violation.operation, operation);
        }
    }
}

fn property_stage_resolution_is_idempotent_and_innermost_wins(
    recorded: Option<MeStage>,
    incoming: MeStage,
) {
    let resolved = resolve_me_stage(recorded, incoming);
    assert_eq!(resolved, recorded.unwrap_or(incoming));
    assert_eq!(resolve_me_stage(Some(resolved), incoming), resolved);

    let error = MeError::Contract {
        reason: "bounded stage proof".to_owned(),
    };
    let error = match recorded {
        Some(stage) => error.at_stage(stage),
        None => error,
    };
    let rendered = error.to_string();
    let annotated = error.at_stage(incoming);
    assert_eq!(annotated.stage(), Some(resolved));
    assert_eq!(annotated.to_string(), rendered);
}

#[cfg(test)]
mod tests {
    fn assert_capability_relation(capability: super::MeConfigurationCapability) {
        for state in super::MeState::ALL {
            for operation in super::MeLifecycleOperation::ALL {
                super::property_operation_relation_is_exact(capability, state, operation);
            }
        }
    }

    #[test]
    fn operation_relation_is_exact_for_all_seven_states() {
        assert_eq!(super::MeState::ALL.len(), 7);
        assert_eq!(super::MeLifecycleOperation::ALL.len(), 20);
        assert_eq!(super::MeConfigurationCapability::ALL.len(), 3);
        for capability in super::MeConfigurationCapability::ALL {
            assert_capability_relation(capability);
        }
    }

    #[test]
    fn terminated_admits_only_standard_observation_and_fmu_state_operations() {
        for operation in super::MeLifecycleOperation::ALL {
            let admitted = super::expected_relation(
                super::MeConfigurationCapability::TunableStructuralParameter,
                super::MeState::Terminated,
                operation,
            )
            .is_ok();
            let expected = matches!(
                operation,
                super::MeLifecycleOperation::GetContinuousStates
                    | super::MeLifecycleOperation::GetNominalsOfContinuousStates
                    | super::MeLifecycleOperation::GetContinuousStateDerivatives
                    | super::MeLifecycleOperation::GetDirectionalDerivative
                    | super::MeLifecycleOperation::GetEventIndicators
                    | super::MeLifecycleOperation::GetFloat64
                    | super::MeLifecycleOperation::GetFmuState
                    | super::MeLifecycleOperation::SetFmuState
            );
            assert_eq!(admitted, expected, "operation {operation:?}");
        }
    }

    #[test]
    fn stage_resolution_is_idempotent_and_innermost_wins() {
        for incoming in super::ME_STAGES {
            super::property_stage_resolution_is_idempotent_and_innermost_wins(None, incoming);
            for recorded in super::ME_STAGES {
                super::property_stage_resolution_is_idempotent_and_innermost_wins(
                    Some(recorded),
                    incoming,
                );
            }
        }
    }
}
