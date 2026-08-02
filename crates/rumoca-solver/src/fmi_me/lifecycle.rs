//! Pure FMI Model Exchange lifecycle state machine.
//!
//! The dynamic FMI facade has to accept calls from an untyped importer, but it
//! never writes a raw state.  Every production transition passes through this
//! small total relation, which is also the boundary exercised by Kani.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeState {
    Instantiated,
    InitializationMode,
    EventMode,
    ContinuousTimeMode,
    Terminated,
}

impl MeState {
    #[cfg(any(test, kani))]
    pub(crate) const ALL: [Self; 5] = [
        Self::Instantiated,
        Self::InitializationMode,
        Self::EventMode,
        Self::ContinuousTimeMode,
        Self::Terminated,
    ];

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::Instantiated => "Instantiated",
            Self::InitializationMode => "InitializationMode",
            Self::EventMode => "EventMode",
            Self::ContinuousTimeMode => "ContinuousTimeMode",
            Self::Terminated => "Terminated",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeLifecycleCommand {
    EnterInitializationMode,
    ExitInitializationMode,
    UpdateDiscreteStates,
    EnterContinuousTimeMode,
    EnterEventMode,
    Terminate,
}

impl MeLifecycleCommand {
    #[cfg(any(test, kani))]
    pub(crate) const ALL: [Self; 6] = [
        Self::EnterInitializationMode,
        Self::ExitInitializationMode,
        Self::UpdateDiscreteStates,
        Self::EnterContinuousTimeMode,
        Self::EnterEventMode,
        Self::Terminate,
    ];

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::EnterInitializationMode => "enter_initialization_mode",
            Self::ExitInitializationMode => "exit_initialization_mode",
            Self::UpdateDiscreteStates => "update_discrete_states",
            Self::EnterContinuousTimeMode => "enter_continuous_time_mode",
            Self::EnterEventMode => "enter_event_mode",
            Self::Terminate => "terminate",
        }
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
}

impl MeLifecycle {
    pub(crate) const fn instantiated() -> Self {
        Self {
            state: MeState::Instantiated,
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
            (State::Instantiated, Command::EnterInitializationMode) => State::InitializationMode,
            (State::InitializationMode, Command::ExitInitializationMode) => State::EventMode,
            (State::EventMode, Command::UpdateDiscreteStates) => State::EventMode,
            (State::EventMode, Command::EnterContinuousTimeMode) => State::ContinuousTimeMode,
            (State::ContinuousTimeMode, Command::EnterEventMode) => State::EventMode,
            (State::Instantiated, Command::Terminate)
            | (State::InitializationMode, Command::Terminate)
            | (State::EventMode, Command::Terminate)
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

    #[cfg(any(test, kani))]
    pub(crate) fn restore_for_verification(&mut self, state: MeState) {
        self.restore(state);
    }

    pub(crate) const fn is_terminated(self) -> bool {
        matches!(self.state, MeState::Terminated)
    }
}
