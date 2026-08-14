//! Exhaustive finite-domain tests for the production FMI ME lifecycle relation.

use crate::fmi_me::lifecycle::{MeLifecycle, MeLifecycleCommand, MeState};
use crate::fmi_me::{MeError, MeStage, resolve_me_stage};

const ME_STAGES: [MeStage; 5] = [
    MeStage::Instantiate,
    MeStage::Initialization,
    MeStage::EventIteration,
    MeStage::ManifoldProjection,
    MeStage::Integration,
];

fn expected_next(state: MeState, command: MeLifecycleCommand) -> Option<MeState> {
    use MeLifecycleCommand as Command;
    use MeState as State;

    match (state, command) {
        (State::Instantiated, Command::EnterInitializationMode) => Some(State::InitializationMode),
        (State::InitializationMode, Command::ExitInitializationMode) => Some(State::EventMode),
        (State::EventMode, Command::UpdateDiscreteStates) => Some(State::EventMode),
        (State::EventMode, Command::EnterContinuousTimeMode) => Some(State::ContinuousTimeMode),
        (State::ContinuousTimeMode, Command::EnterEventMode) => Some(State::EventMode),
        (State::Instantiated, Command::Terminate)
        | (State::InitializationMode, Command::Terminate)
        | (State::EventMode, Command::Terminate)
        | (State::ContinuousTimeMode, Command::Terminate) => Some(State::Terminated),
        _ => None,
    }
}

/// ME-LIFE-001/002: the production relation is exactly SPEC_0038's table and
/// every rejected transition preserves the aggregate.
fn property_transition_relation_is_exact(state: MeState, command: MeLifecycleCommand) {
    let mut lifecycle = MeLifecycle::instantiated();
    lifecycle.restore_for_verification(state);
    let before = lifecycle;
    let result = lifecycle.transition(command);
    match expected_next(state, command) {
        Some(expected) => {
            assert!(result.is_ok(), "a specified lifecycle edge was rejected");
            assert_eq!(
                lifecycle.state(),
                expected,
                "an accepted lifecycle edge reached the wrong state"
            );
        }
        None => {
            assert!(
                result.is_err(),
                "an unspecified lifecycle edge was accepted"
            );
            assert_eq!(
                lifecycle, before,
                "a rejected lifecycle edge mutated the aggregate"
            );
        }
    }
}

/// ME-LIFE-003: Terminated is absorbing for every ordinary lifecycle command.
fn property_terminated_is_absorbing(command: MeLifecycleCommand) {
    let mut lifecycle = MeLifecycle::instantiated();
    lifecycle.restore_for_verification(MeState::Terminated);
    assert!(lifecycle.transition(command).is_err());
    assert_eq!(lifecycle.state(), MeState::Terminated);
}

/// ME-STATE-001 lifecycle clause: restoring an opaque snapshot state reaches
/// that state exactly, including restoration out of Terminated.
fn property_snapshot_restore_is_exact(target: MeState) {
    let mut lifecycle = MeLifecycle::instantiated();
    lifecycle
        .transition(MeLifecycleCommand::Terminate)
        .expect("termination is legal from Instantiated");
    lifecycle.restore_for_verification(target);
    assert_eq!(lifecycle.state(), target);
}

/// ME-ERR-001: stage annotation is idempotent, and an already-recorded inner
/// stage always wins over a coarser incoming stage.
fn property_stage_resolution_is_idempotent_and_innermost_wins(
    recorded: Option<MeStage>,
    incoming: MeStage,
) {
    let resolved = resolve_me_stage(recorded, incoming);
    assert_eq!(resolved, recorded.unwrap_or(incoming));
    assert_eq!(
        resolve_me_stage(Some(resolved), incoming),
        resolved,
        "annotating an already-resolved error must be idempotent"
    );

    let error = MeError::Contract {
        reason: "bounded stage proof".to_string(),
    };
    let error = match recorded {
        Some(stage) => error.at_stage(stage),
        None => error,
    };
    let rendered_before = error.to_string();
    let annotated = error.at_stage(incoming);
    assert_eq!(annotated.stage(), Some(resolved));
    assert!(matches!(annotated.kind(), MeError::Contract { .. }));
    assert_eq!(annotated.to_string(), rendered_before);

    let annotated_again = annotated.at_stage(incoming);
    assert_eq!(annotated_again.stage(), Some(resolved));
    assert!(matches!(annotated_again.kind(), MeError::Contract { .. }));
    assert_eq!(annotated_again.to_string(), rendered_before);
}

#[cfg(test)]
mod tests {
    #[test]
    fn transition_relation_is_exact_and_rejection_preserves_state() {
        for state in super::MeState::ALL {
            for command in super::MeLifecycleCommand::ALL {
                super::property_transition_relation_is_exact(state, command);
            }
        }
    }

    #[test]
    fn terminated_is_absorbing() {
        for command in super::MeLifecycleCommand::ALL {
            super::property_terminated_is_absorbing(command);
        }
    }

    #[test]
    fn snapshot_restore_is_exact() {
        for target in super::MeState::ALL {
            super::property_snapshot_restore_is_exact(target);
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
