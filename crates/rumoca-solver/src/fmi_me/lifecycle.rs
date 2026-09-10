//! Pure FMI Model Exchange lifecycle state machine.
//!
//! The dynamic FMI facade has to accept calls from an untyped importer, but it
//! never writes a raw state.  Every production transition passes through this
//! small total relation, which is also the boundary exercised by Kani.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeState {
    Instantiated,
    ConfigurationMode,
    InitializationMode,
    EventMode,
    ReconfigurationMode,
    ContinuousTimeMode,
    Terminated,
}

impl MeState {
    #[cfg(test)]
    pub(crate) const ALL: [Self; 7] = [
        Self::Instantiated,
        Self::ConfigurationMode,
        Self::InitializationMode,
        Self::EventMode,
        Self::ReconfigurationMode,
        Self::ContinuousTimeMode,
        Self::Terminated,
    ];

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::Instantiated => "Instantiated",
            Self::ConfigurationMode => "ConfigurationMode",
            Self::InitializationMode => "InitializationMode",
            Self::EventMode => "EventMode",
            Self::ReconfigurationMode => "ReconfigurationMode",
            Self::ContinuousTimeMode => "ContinuousTimeMode",
            Self::Terminated => "Terminated",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeLifecycleCommand {
    EnterConfigurationMode,
    ExitConfigurationMode,
    EnterInitializationMode,
    ExitInitializationMode,
    UpdateDiscreteStates,
    EnterContinuousTimeMode,
    EnterEventMode,
    Terminate,
}

impl MeLifecycleCommand {
    #[cfg(test)]
    pub(crate) const ALL: [Self; 8] = [
        Self::EnterConfigurationMode,
        Self::ExitConfigurationMode,
        Self::EnterInitializationMode,
        Self::ExitInitializationMode,
        Self::UpdateDiscreteStates,
        Self::EnterContinuousTimeMode,
        Self::EnterEventMode,
        Self::Terminate,
    ];

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::EnterConfigurationMode => "enter_configuration_mode",
            Self::ExitConfigurationMode => "exit_configuration_mode",
            Self::EnterInitializationMode => "enter_initialization_mode",
            Self::ExitInitializationMode => "exit_initialization_mode",
            Self::UpdateDiscreteStates => "update_discrete_states",
            Self::EnterContinuousTimeMode => "enter_continuous_time_mode",
            Self::EnterEventMode => "enter_event_mode",
            Self::Terminate => "terminate",
        }
    }
}

/// The structural-parameter capability that guards Configuration Mode.
///
/// The three variants encode both entry edges: every declared structural
/// parameter admits pre-initialization configuration, while only a tunable
/// structural parameter admits reconfiguration from Event Mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeConfigurationCapability {
    Absent,
    FixedStructuralParameter,
    TunableStructuralParameter,
}

impl MeConfigurationCapability {
    #[cfg(test)]
    pub(crate) const ALL: [Self; 3] = [
        Self::Absent,
        Self::FixedStructuralParameter,
        Self::TunableStructuralParameter,
    ];

    const fn admits_initial_configuration(self) -> bool {
        !matches!(self, Self::Absent)
    }

    const fn admits_reconfiguration(self) -> bool {
        matches!(self, Self::TunableStructuralParameter)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct MeLifecycleViolation {
    pub(crate) state: MeState,
    pub(crate) command: MeLifecycleCommand,
}

/// Private valid-by-construction lifecycle aggregate.
///
/// Its state is private. Ordinary lifecycle mutation implements the exact
/// transition table in SPEC_0038, while the separately scoped restore path is
/// reserved for validated component snapshots. A rejected transition cannot
/// mutate it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct MeLifecycle {
    state: MeState,
    configuration: MeConfigurationCapability,
}

impl MeLifecycle {
    pub(crate) const fn instantiated(configuration: MeConfigurationCapability) -> Self {
        Self {
            state: MeState::Instantiated,
            configuration,
        }
    }

    pub(crate) const fn state(self) -> MeState {
        self.state
    }

    pub(crate) const fn next(
        self,
        command: MeLifecycleCommand,
    ) -> Result<MeState, MeLifecycleViolation> {
        use MeLifecycleCommand as Command;
        use MeState as State;

        let next = match (self.state, command) {
            (State::Instantiated, Command::EnterConfigurationMode)
                if self.configuration.admits_initial_configuration() =>
            {
                State::ConfigurationMode
            }
            (State::ConfigurationMode, Command::ExitConfigurationMode) => State::Instantiated,
            (State::Instantiated, Command::EnterInitializationMode) => State::InitializationMode,
            (State::InitializationMode, Command::ExitInitializationMode) => State::EventMode,
            (State::EventMode, Command::UpdateDiscreteStates) => State::EventMode,
            (State::EventMode, Command::EnterConfigurationMode)
                if self.configuration.admits_reconfiguration() =>
            {
                State::ReconfigurationMode
            }
            (State::ReconfigurationMode, Command::ExitConfigurationMode) => State::EventMode,
            (State::EventMode, Command::EnterContinuousTimeMode) => State::ContinuousTimeMode,
            (State::ContinuousTimeMode, Command::EnterEventMode) => State::EventMode,
            (State::Instantiated, Command::Terminate)
            | (State::ConfigurationMode, Command::Terminate)
            | (State::InitializationMode, Command::Terminate)
            | (State::EventMode, Command::Terminate)
            | (State::ReconfigurationMode, Command::Terminate)
            | (State::ContinuousTimeMode, Command::Terminate) => State::Terminated,
            _ => {
                return Err(MeLifecycleViolation {
                    state: self.state,
                    command,
                });
            }
        };
        Ok(next)
    }

    pub(crate) fn transition(
        &mut self,
        command: MeLifecycleCommand,
    ) -> Result<(), MeLifecycleViolation> {
        let next = self.next(command)?;
        self.state = next;
        Ok(())
    }

    /// Restore is not an ordinary lifecycle command: an opaque component
    /// snapshot carries the already-validated state it was captured in.
    pub(super) fn restore(&mut self, state: MeState) {
        self.state = state;
    }

    #[cfg(test)]
    pub(crate) fn restore_for_verification(&mut self, state: MeState) {
        self.restore(state);
    }

    pub(crate) const fn is_terminated(self) -> bool {
        matches!(self.state, MeState::Terminated)
    }
}
