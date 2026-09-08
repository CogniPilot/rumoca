//! Indivisible ownership of one FMI component and every capability derived
//! from that exact component.
//!
//! Keeping the fields in this child module makes correlation a Rust privacy
//! invariant. The parent master algorithm can only use the accessors below; it
//! cannot destructure the aggregate or pair a root-search state, derivative
//! controller, or maximum-step reference issued by another kernel.

use std::{cell::RefCell, rc::Rc};

use crate::fmi_me::{
    MeTime, MeValueRef, SolveMeKernel,
    integrator::{MeAcceptedStep, MeDerivativeController, MeIntegratorBackend, MeStepProposal},
    root::{MeRootApplication, MeRootSearchState, RootScanTarget},
};
use crate::runtime::timeout::TimeoutBudget;

use super::{
    super::{FinishedInitialization, MeSessionError, MeSessionOptions, caught_sample},
    InitializationStatus,
};

/// The indivisible component/root-search owner.
pub(in crate::fmi_me::session) struct MeHostComponent {
    kernel: Rc<RefCell<SolveMeKernel>>,
    derivatives: MeDerivativeController,
    root_search: MeRootSearchState,
    max_step_duration_reference: Option<MeValueRef>,
}

impl MeHostComponent {
    pub(in crate::fmi_me::session) fn new(
        kernel: Rc<RefCell<SolveMeKernel>>,
    ) -> Result<Self, MeSessionError> {
        let (root_search, max_step_duration_reference) = {
            let kernel = kernel.borrow();
            (
                MeRootSearchState::new(&kernel)?,
                kernel.max_step_duration_value_reference(),
            )
        };
        let derivatives = MeDerivativeController::over_kernel(Rc::clone(&kernel))?;
        Ok(Self {
            kernel,
            derivatives,
            root_search,
            max_step_duration_reference,
        })
    }

    /// Borrow the kernel cell without exposing the cloneable owning handle.
    ///
    /// The session may operate on this component, but only this aggregate can
    /// retain it or hand it to another construction owner.
    pub(in crate::fmi_me::session) fn kernel(&self) -> &RefCell<SolveMeKernel> {
        self.kernel.as_ref()
    }

    pub(in crate::fmi_me::session) fn derivatives(&self) -> &MeDerivativeController {
        &self.derivatives
    }

    pub(in crate::fmi_me::session) const fn state_domain(
        &self,
    ) -> crate::fmi_me::MeContinuousStateDomain {
        self.root_search.state_domain()
    }

    pub(in crate::fmi_me::session) fn root_nominals(&self) -> Result<&[f64], MeSessionError> {
        self.root_search.nominals()
    }

    pub(in crate::fmi_me::session) fn max_step_duration_reference(&self) -> Option<&MeValueRef> {
        self.max_step_duration_reference.as_ref()
    }

    /// Finish FMI initialization without exposing mutable component-derived
    /// capabilities to the parent master algorithm.
    pub(in crate::fmi_me::session) fn finish_initialization(
        &mut self,
        options: &MeSessionOptions,
        status: InitializationStatus,
    ) -> Result<FinishedInitialization, MeSessionError> {
        match status {
            InitializationStatus::Terminated {
                states,
                termination,
            } => {
                self.root_search.configure_terminated();
                Ok(FinishedInitialization::Terminated {
                    states,
                    termination,
                })
            }
            InitializationStatus::Active {
                states,
                nominals,
                next_event_time,
            } => {
                self.root_search.configure_active(options, nominals)?;
                Ok(FinishedInitialization::Active {
                    states,
                    next_event_time,
                })
            }
        }
    }

    /// Refresh only the policy data FMI says changed, deriving the replacement
    /// from this component's retained root-search domain.
    pub(in crate::fmi_me::session) fn refresh_root_nominals(
        &mut self,
        nominals: Vec<f64>,
    ) -> Result<(), MeSessionError> {
        self.root_search.refresh_nominals(nominals)
    }

    /// Refresh the retained root-indicator seed using this exact component's
    /// kernel and this exact root-search workspace.
    pub(in crate::fmi_me::session) fn refresh_retained_indicators(
        &self,
        time: f64,
        states: &[f64],
    ) -> Result<(), MeSessionError> {
        self.root_search.refresh_retained_indicators(|indicators| {
            let mut kernel = self.kernel.borrow_mut();
            kernel.set_time(MeTime::at(time))?;
            kernel.set_continuous_states(states)?;
            kernel.get_event_indicators(indicators)?;
            Ok(())
        })
    }

    /// Validate a proposal using the derivative controller and root policy
    /// issued alongside this exact component kernel.
    pub(in crate::fmi_me::session) fn accept_proposal(
        &self,
        backend: &dyn MeIntegratorBackend,
        budget: &TimeoutBudget,
        proposal: MeStepProposal,
    ) -> Result<MeAcceptedStep, MeSessionError> {
        let mut target = ComponentScanTarget {
            component: self,
            backend,
            budget,
        };
        self.root_search.accept_step(&mut target, proposal)
    }

    /// Scan one accepted interval using the kernel, derivative controller,
    /// policy, and workspace owned by this exact component aggregate.
    pub(in crate::fmi_me::session) fn scan_accepted_interval(
        &self,
        backend: &dyn MeIntegratorBackend,
        budget: &TimeoutBudget,
        accepted: &MeAcceptedStep,
    ) -> Result<Option<MeRootApplication>, MeSessionError> {
        let mut target = ComponentScanTarget {
            component: self,
            backend,
            budget,
        };
        self.root_search
            .scan_accepted_interval(&mut target, accepted)
    }
}

/// The scan view minted only inside the indivisible component owner.
struct ComponentScanTarget<'a> {
    component: &'a MeHostComponent,
    backend: &'a dyn MeIntegratorBackend,
    budget: &'a TimeoutBudget,
}

impl RootScanTarget for ComponentScanTarget<'_> {
    fn sample_states(&mut self, time: f64, states: &mut [f64]) -> Result<(), MeSessionError> {
        match caught_sample(self.backend, &self.component.derivatives, time, states) {
            Ok(sampled) => sampled,
            Err(payload) => std::panic::resume_unwind(payload),
        }
    }

    fn indicators_at(
        &mut self,
        time: f64,
        states: &[f64],
        indicators: &mut [f64],
    ) -> Result<(), MeSessionError> {
        let mut kernel = self.component.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        kernel.get_event_indicators(indicators)?;
        Ok(())
    }

    fn check_budget(&self) -> Result<(), MeSessionError> {
        Ok(self.budget.check()?)
    }
}
