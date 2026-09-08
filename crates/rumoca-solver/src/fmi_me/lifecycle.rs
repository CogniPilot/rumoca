//! Pure FMI 3.0.2 Model Exchange lifecycle admission.
//!
//! The dynamic facade accepts calls from an untyped importer. Every lifecycle
//! transition, getter, and mutator therefore enters through the one closed
//! [`MeLifecycleOperation`] x [`MeState`] relation below. Successful admission
//! exclusively borrows this lifecycle instance and mints one operation-specific,
//! affine guard for the corresponding private kernel.

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

    /// The FMI 3 write mode a Float64 write is judged against in this lifecycle
    /// state, or `None` where no boundary write is admitted. Model Exchange
    /// never enters Co-Simulation Step Mode, so no state maps to it; the
    /// configuration, reconfiguration, and terminated states admit no write.
    pub(crate) const fn fmi3_write_mode(self) -> Option<rumoca_ir_solve::fmi::Fmi3WriteMode> {
        use rumoca_ir_solve::fmi::Fmi3WriteMode;
        match self {
            Self::Instantiated => Some(Fmi3WriteMode::Instantiated),
            Self::InitializationMode => Some(Fmi3WriteMode::InitializationMode),
            Self::EventMode => Some(Fmi3WriteMode::EventMode),
            Self::ContinuousTimeMode => Some(Fmi3WriteMode::ContinuousTimeMode),
            Self::ConfigurationMode | Self::ReconfigurationMode | Self::Terminated => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeLifecycleOperation {
    EnterConfigurationMode,
    ExitConfigurationMode,
    EnterInitializationMode,
    ExitInitializationMode,
    UpdateDiscreteStates,
    EnterContinuousTimeMode,
    EnterEventMode,
    Terminate,
    SetTime,
    SetContinuousStates,
    GetContinuousStates,
    GetNominalsOfContinuousStates,
    GetContinuousStateDerivatives,
    GetDirectionalDerivative,
    GetEventIndicators,
    SetFloat64,
    GetFloat64,
    CompletedIntegratorStep,
    GetFmuState,
    SetFmuState,
}

impl MeLifecycleOperation {
    #[cfg(test)]
    pub(crate) const ALL: [Self; 20] = [
        Self::EnterConfigurationMode,
        Self::ExitConfigurationMode,
        Self::EnterInitializationMode,
        Self::ExitInitializationMode,
        Self::UpdateDiscreteStates,
        Self::EnterContinuousTimeMode,
        Self::EnterEventMode,
        Self::Terminate,
        Self::SetTime,
        Self::SetContinuousStates,
        Self::GetContinuousStates,
        Self::GetNominalsOfContinuousStates,
        Self::GetContinuousStateDerivatives,
        Self::GetDirectionalDerivative,
        Self::GetEventIndicators,
        Self::SetFloat64,
        Self::GetFloat64,
        Self::CompletedIntegratorStep,
        Self::GetFmuState,
        Self::SetFmuState,
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
            Self::SetTime => "set_time",
            Self::SetContinuousStates => "set_continuous_states",
            Self::GetContinuousStates => "get_continuous_states",
            Self::GetNominalsOfContinuousStates => "get_nominals_of_continuous_states",
            Self::GetContinuousStateDerivatives => "get_continuous_state_derivatives",
            Self::GetDirectionalDerivative => "get_directional_derivative",
            Self::GetEventIndicators => "get_event_indicators",
            Self::SetFloat64 => "set_float64",
            Self::GetFloat64 => "get_float64",
            Self::CompletedIntegratorStep => "completed_integrator_step",
            Self::GetFmuState => "get_fmu_state",
            Self::SetFmuState => "set_fmu_state",
        }
    }
}

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
    pub(crate) operation: MeLifecycleOperation,
}

/// Opaque lifecycle payload carried only by an instance-branded FMU snapshot.
#[derive(Clone)]
pub(super) struct MeSavedLifecycle {
    state: MeState,
}

macro_rules! stay_admissions {
    ($(($variant:ident, $token:ident, $method:ident)),+ $(,)?) => {
        $(
            pub(super) struct $token<'lifecycle> {
                lifecycle: &'lifecycle mut MeLifecycle,
            }

            impl $token<'_> {
                pub(super) fn consume(self) {
                    let _ = self.lifecycle.state;
                }
            }
        )+

        impl MeLifecycle {
            $(
                pub(super) fn $method(&mut self) -> Result<$token<'_>, MeLifecycleViolation> {
                    self.admit_stay(MeLifecycleOperation::$variant)?;
                    Ok($token { lifecycle: self })
                }
            )+
        }
    };
}

stay_admissions!(
    (SetTime, MeSetTimeAdmission, admit_set_time),
    (
        SetContinuousStates,
        MeSetContinuousStatesAdmission,
        admit_set_continuous_states
    ),
    (
        GetContinuousStates,
        MeGetContinuousStatesAdmission,
        admit_get_continuous_states
    ),
    (
        GetNominalsOfContinuousStates,
        MeGetNominalsOfContinuousStatesAdmission,
        admit_get_nominals_of_continuous_states
    ),
    (
        GetContinuousStateDerivatives,
        MeGetContinuousStateDerivativesAdmission,
        admit_get_continuous_state_derivatives
    ),
    (
        GetDirectionalDerivative,
        MeGetDirectionalDerivativeAdmission,
        admit_get_directional_derivative
    ),
    (
        GetEventIndicators,
        MeGetEventIndicatorsAdmission,
        admit_get_event_indicators
    ),
    (SetFloat64, MeSetFloat64Admission, admit_set_float64),
    (GetFloat64, MeGetFloat64Admission, admit_get_float64),
    (
        CompletedIntegratorStep,
        MeCompletedIntegratorStepAdmission,
        admit_completed_integrator_step
    ),
);

impl MeSetFloat64Admission<'_> {
    pub(super) fn state(&self) -> MeState {
        self.lifecycle.state
    }

    pub(super) fn saved_lifecycle(&self) -> MeSavedLifecycle {
        MeSavedLifecycle {
            state: self.lifecycle.state,
        }
    }
}

macro_rules! transition_admissions {
    ($(($variant:ident, $token:ident, $method:ident)),+ $(,)?) => {
        $(
            pub(super) struct $token<'lifecycle> {
                lifecycle: &'lifecycle mut MeLifecycle,
                next_state: MeState,
            }

            impl $token<'_> {
                pub(super) fn commit(self) {
                    self.lifecycle.state = self.next_state;
                }
            }
        )+

        impl MeLifecycle {
            $(
                pub(super) fn $method(&mut self) -> Result<$token<'_>, MeLifecycleViolation> {
                    let next_state = self.admit_transition(MeLifecycleOperation::$variant)?;
                    Ok($token {
                        lifecycle: self,
                        next_state,
                    })
                }
            )+
        }
    };
}

transition_admissions!(
    (
        EnterConfigurationMode,
        MeEnterConfigurationModeAdmission,
        admit_enter_configuration_mode
    ),
    (
        ExitConfigurationMode,
        MeExitConfigurationModeAdmission,
        admit_exit_configuration_mode
    ),
    (
        EnterInitializationMode,
        MeEnterInitializationModeAdmission,
        admit_enter_initialization_mode
    ),
    (
        ExitInitializationMode,
        MeExitInitializationModeAdmission,
        admit_exit_initialization_mode
    ),
    (
        UpdateDiscreteStates,
        MeUpdateDiscreteStatesAdmission,
        admit_update_discrete_states
    ),
    (
        EnterContinuousTimeMode,
        MeEnterContinuousTimeModeAdmission,
        admit_enter_continuous_time_mode
    ),
    (
        EnterEventMode,
        MeEnterEventModeAdmission,
        admit_enter_event_mode
    ),
    (Terminate, MeTerminateAdmission, admit_terminate),
);

pub(super) struct MeGetFmuStateAdmission<'lifecycle> {
    lifecycle: &'lifecycle mut MeLifecycle,
    operation: MeLifecycleOperation,
}

impl MeGetFmuStateAdmission<'_> {
    pub(super) fn saved_lifecycle(&self) -> MeSavedLifecycle {
        MeSavedLifecycle {
            state: self.lifecycle.state(),
        }
    }

    pub(super) fn consume(self) {
        let _ = self.operation;
    }
}

pub(super) struct MeSetFmuStateAdmission<'lifecycle> {
    lifecycle: &'lifecycle mut MeLifecycle,
    operation: MeLifecycleOperation,
}

impl MeSetFmuStateAdmission<'_> {
    pub(super) fn restore(self, saved: &MeSavedLifecycle) {
        let _ = self.operation;
        self.lifecycle.state = saved.state;
    }
}

#[derive(Debug, PartialEq, Eq)]
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

    pub(crate) const fn state(&self) -> MeState {
        self.state
    }

    #[cfg(test)]
    pub(super) fn matches_saved(&self, saved: &MeSavedLifecycle) -> bool {
        self.state == saved.state
    }

    fn violation(&self, operation: MeLifecycleOperation) -> MeLifecycleViolation {
        MeLifecycleViolation {
            state: self.state,
            operation,
        }
    }

    fn admit_transition(
        &self,
        operation: MeLifecycleOperation,
    ) -> Result<MeState, MeLifecycleViolation> {
        use MeLifecycleOperation as Operation;
        use MeState as State;

        match (self.state, operation) {
            (State::Instantiated, Operation::EnterConfigurationMode)
                if self.configuration.admits_initial_configuration() =>
            {
                Ok(State::ConfigurationMode)
            }
            (State::ConfigurationMode, Operation::ExitConfigurationMode) => Ok(State::Instantiated),
            (State::Instantiated, Operation::EnterInitializationMode) => {
                Ok(State::InitializationMode)
            }
            (State::InitializationMode, Operation::ExitInitializationMode) => Ok(State::EventMode),
            (State::EventMode, Operation::UpdateDiscreteStates) => Ok(State::EventMode),
            (State::EventMode, Operation::EnterConfigurationMode)
                if self.configuration.admits_reconfiguration() =>
            {
                Ok(State::ReconfigurationMode)
            }
            (State::ReconfigurationMode, Operation::ExitConfigurationMode) => Ok(State::EventMode),
            (State::EventMode, Operation::EnterContinuousTimeMode) => Ok(State::ContinuousTimeMode),
            (State::ContinuousTimeMode, Operation::EnterEventMode) => Ok(State::EventMode),
            (
                State::EventMode | State::ReconfigurationMode | State::ContinuousTimeMode,
                Operation::Terminate,
            ) => Ok(State::Terminated),
            _ => Err(self.violation(operation)),
        }
    }

    fn admit_stay(&self, operation: MeLifecycleOperation) -> Result<(), MeLifecycleViolation> {
        use MeLifecycleOperation as Operation;
        use MeState as State;

        match (self.state, operation) {
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
            ) => Ok(()),
            _ => Err(self.violation(operation)),
        }
    }

    pub(super) fn admit_get_fmu_state(&mut self) -> MeGetFmuStateAdmission<'_> {
        MeGetFmuStateAdmission {
            lifecycle: self,
            operation: MeLifecycleOperation::GetFmuState,
        }
    }

    pub(super) fn admit_set_fmu_state(&mut self) -> MeSetFmuStateAdmission<'_> {
        MeSetFmuStateAdmission {
            lifecycle: self,
            operation: MeLifecycleOperation::SetFmuState,
        }
    }

    #[cfg(test)]
    pub(crate) fn relation_for_verification(
        configuration: MeConfigurationCapability,
        state: MeState,
        operation: MeLifecycleOperation,
    ) -> Result<Option<MeState>, MeLifecycleViolation> {
        let lifecycle = Self {
            state,
            configuration,
        };
        match operation {
            MeLifecycleOperation::EnterConfigurationMode
            | MeLifecycleOperation::ExitConfigurationMode
            | MeLifecycleOperation::EnterInitializationMode
            | MeLifecycleOperation::ExitInitializationMode
            | MeLifecycleOperation::UpdateDiscreteStates
            | MeLifecycleOperation::EnterContinuousTimeMode
            | MeLifecycleOperation::EnterEventMode
            | MeLifecycleOperation::Terminate => lifecycle.admit_transition(operation).map(Some),
            MeLifecycleOperation::GetFmuState | MeLifecycleOperation::SetFmuState => Ok(None),
            _ => lifecycle.admit_stay(operation).map(|()| None),
        }
    }
}
