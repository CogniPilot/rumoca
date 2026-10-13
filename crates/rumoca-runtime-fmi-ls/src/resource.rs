//! Lease ownership delegates every transition to the existing ME owner.

use rumoca_solver::fmi_me::{
    MeAdvanceOutcome, MeError, MeInstanceConfig, MeOutputCursor, MeRetainedComponent,
    MeSessionError, MeSessionOptions, MeSimulationSession,
};

#[derive(Debug, thiserror::Error)]
pub enum FmiLsResourceError {
    #[error(transparent)]
    Component(#[from] MeError),
    #[error(transparent)]
    Session(#[from] MeSessionError),
    #[error("FMI getter result allocation refused: {0}")]
    ResultBuffer(#[from] std::collections::TryReserveError),
}

#[cfg(any(
    test,
    all(target_arch = "wasm32", target_os = "wasi", target_env = "p2")
))]
pub(super) fn result_buffer<T: Default + Clone>(
    count: usize,
) -> Result<Vec<T>, FmiLsResourceError> {
    let mut values = Vec::new();
    values.try_reserve_exact(count)?;
    values.resize(count, T::default());
    Ok(values)
}

/// Holds one actual component or its owned session lease, never another kernel.
/// This is not a lifecycle state: the retained kernel remains its sole authority.
pub struct FmiLsResource {
    owner: Option<ResourceOwner>,
}

/// Results read from the original driver after a successful FMI step.
pub struct FmiLsStepResult {
    pub last_successful_time: f64,
    pub terminate_simulation: bool,
    pub early_return: bool,
}

enum ResourceOwner {
    Retained(MeRetainedComponent),
    Session(Box<MeSimulationSession<'static, 'static>>),
}

impl FmiLsResource {
    pub fn diagnostics_since(&self, first: usize) -> (Vec<rumoca_solver::SimDiagnostic>, usize) {
        match self.owner.as_ref().expect("resource is owned") {
            ResourceOwner::Retained(component) => component.diagnostics_since(first),
            ResourceOwner::Session(session) => session.diagnostics_since(first),
        }
    }
    pub fn do_step(
        &mut self,
        current: f64,
        step: f64,
    ) -> Result<FmiLsStepResult, FmiLsResourceError> {
        let ResourceOwner::Session(session) = self.owner.as_mut().expect("resource is owned")
        else {
            return Err(ownership_refusal());
        };
        let target = current + step;
        if !current.is_finite()
            || !step.is_finite()
            || step <= 0.0
            || current != session.time()
            || !target.is_finite()
            || target <= current
            || session.stop_time().is_some_and(|stop| target > stop)
        {
            return Err(MeSessionError::Contract {
                reason: "FMI DoStep requires the actual communication point and a positive finite step within its horizon".into(),
            }.into());
        }
        session.advance_to(target, &mut MeOutputCursor::empty())?;
        Ok(FmiLsStepResult {
            last_successful_time: session.time(),
            terminate_simulation: session.is_terminated(),
            early_return: session.time() < target,
        })
    }
    /// The actual owner checks each reference and its aggregate extent before allocation.
    pub fn value_count(
        &self,
        references: &[u32],
        kind: rumoca_ir_solve::SolveVariableValueKind,
    ) -> Result<usize, FmiLsResourceError> {
        match self.owner.as_ref().expect("resource is owned") {
            ResourceOwner::Retained(component) => component
                .native_value_count(references, kind)
                .map_err(Into::into),
            ResourceOwner::Session(session) => session
                .native_value_count(references, kind)
                .map_err(Into::into),
        }
    }

    pub fn terminate(&mut self) -> Result<(), FmiLsResourceError> {
        match self.owner.as_mut().expect("resource is owned") {
            ResourceOwner::Retained(component) => component.terminate().map_err(Into::into),
            ResourceOwner::Session(session) => session.terminate().map_err(Into::into),
        }
    }

    /// ExitInit derives options from the entered owner before consuming its receipt.
    pub fn complete_initialization(&mut self) -> Result<(), FmiLsResourceError> {
        let options = match self.owner.as_ref().expect("resource is owned") {
            ResourceOwner::Retained(component) => component.initialization_options()?,
            ResourceOwner::Session(_) => return Err(ownership_refusal()),
        };
        self.exit_initialization_mode(options)
    }

    /// Consume an already issued owner; this cannot prepare or admit an engine.
    pub fn from_retained(component: MeRetainedComponent) -> Self {
        Self {
            owner: Some(ResourceOwner::Retained(component)),
        }
    }

    pub fn enter_initialization_mode(
        &mut self,
        config: &MeInstanceConfig,
    ) -> Result<(), FmiLsResourceError> {
        match self.owner.as_mut().expect("resource is owned") {
            ResourceOwner::Retained(component) => component
                .enter_initialization_mode(config)
                .map_err(Into::into),
            ResourceOwner::Session(_) => Err(ownership_refusal()),
        }
    }

    /// Complete the actual Initialization Mode through the original driver.
    pub fn exit_initialization_mode(
        &mut self,
        options: MeSessionOptions,
    ) -> Result<(), FmiLsResourceError> {
        let owner = self.owner.take().expect("resource is owned");
        match owner {
            ResourceOwner::Retained(component) => {
                let (owner, result) = match component
                    .complete_native_initialization_with_integrator_retaining(
                        options,
                        rumoca_solver_rk45::model_exchange_integrator,
                    ) {
                    Ok(session) => (ResourceOwner::Session(Box::new(session)), Ok(())),
                    Err((component, error)) => {
                        (ResourceOwner::Retained(*component), Err(error.into()))
                    }
                };
                self.owner = Some(owner);
                result
            }
            ResourceOwner::Session(session) => {
                self.owner = Some(ResourceOwner::Session(session));
                Err(ownership_refusal())
            }
        }
    }

    /// FMI Reset releases the real session and restores the original source.
    /// The next initialization takes its actual time from the caller again.
    pub fn reset(&mut self) -> Result<(), FmiLsResourceError> {
        let owner = self.owner.take().expect("resource is owned");
        let mut component = match owner {
            ResourceOwner::Retained(component) => component,
            ResourceOwner::Session(session) => match session.into_retained_component() {
                Ok(component) => component,
                Err((session, error)) => {
                    self.owner = Some(ResourceOwner::Session(session));
                    return Err(error.into());
                }
            },
        };
        let result = component.reset().map_err(Into::into);
        self.owner = Some(ResourceOwner::Retained(component));
        result
    }

    /// Advance the existing master; this does not schedule another model entry.
    pub fn advance_to(
        &mut self,
        time: f64,
        cursor: &mut MeOutputCursor,
    ) -> Result<MeAdvanceOutcome, FmiLsResourceError> {
        match self.owner.as_mut().expect("resource is owned") {
            ResourceOwner::Session(session) => session.advance_to(time, cursor).map_err(Into::into),
            ResourceOwner::Retained(_) => Err(ownership_refusal()),
        }
    }
}

fn ownership_refusal() -> FmiLsResourceError {
    MeSessionError::Contract {
        reason: "the operation requires the resource's actual retained or active session owner"
            .to_owned(),
    }
    .into()
}

macro_rules! typed_accessors {
    ($set:ident, $get:ident, $value:ty) => {
        impl FmiLsResource {
            pub fn $set(
                &mut self,
                references: &[u32],
                values: &[$value],
            ) -> Result<(), FmiLsResourceError> {
                match self.owner.as_mut().expect("resource is owned") {
                    ResourceOwner::Retained(component) => {
                        component.$set(references, values).map_err(Into::into)
                    }
                    ResourceOwner::Session(session) => {
                        session.$set(references, values).map_err(Into::into)
                    }
                }
            }

            pub fn $get(
                &self,
                references: &[u32],
                values: &mut [$value],
            ) -> Result<(), FmiLsResourceError> {
                match self.owner.as_ref().expect("resource is owned") {
                    ResourceOwner::Retained(component) => {
                        component.$get(references, values).map_err(Into::into)
                    }
                    ResourceOwner::Session(session) => {
                        session.$get(references, values).map_err(Into::into)
                    }
                }
            }
        }
    };
}

typed_accessors!(set_native_float64, get_native_float64, f64);
typed_accessors!(set_native_integer, get_native_integer, i64);
typed_accessors!(set_boolean, get_boolean, bool);

#[cfg(test)]
mod tests;
