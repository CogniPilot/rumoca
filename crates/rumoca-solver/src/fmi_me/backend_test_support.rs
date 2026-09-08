//! A feature-gated harness for driving a real numerical plugin.
//!
//! It exists only under `cfg(any(test, feature = "test-support"))`, so no
//! production build carries it. A backend crate that lives outside
//! `rumoca-solver` cannot mint a derivative handle, a continuous point, or an
//! advance request on its own; this module hands it exactly the checked inputs
//! [`super::MeIntegratorBackend`] consumes, opens the activation window the
//! production host opens per call, and enforces the component's own monotone
//! `fmi3SetTime` lower bound. A plugin that queries a coordinate behind the
//! retained bound is refused just as the real component refuses it, which is
//! what lets a backend regression observe an in-place numerical restart at a
//! problem's original `t0`.

use std::{cell::Cell, rc::Rc};

use super::integrator::{MeDerivativeActivation, MeDerivativeController};
use super::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError, MeNumericalSetup,
};

/// The public probe a backend crate drives its integrator through.
pub struct MeBackendProbe {
    controller: MeDerivativeController,
    min_time: Rc<Cell<f64>>,
    state_domain: super::MeContinuousStateDomain,
}

impl MeBackendProbe {
    /// A probe over the constant-rate system `y' = rates`, whose retained
    /// monotone lower bound starts unconstrained.
    #[must_use]
    pub fn linear(rates: Vec<f64>) -> Self {
        // The verification world has no linked FMI component. Its one
        // construction owner consumes the complete derivative vector and
        // derives the matching domain before moving the vector into the
        // controller; callers never supply or retain a parallel count.
        let state_domain = super::MeContinuousStateDomain::from_verification_rates(&rates);
        let min_time = Rc::new(Cell::new(f64::NEG_INFINITY));
        let controller = MeDerivativeController::over_monotone_linear(rates, Rc::clone(&min_time));
        Self {
            controller,
            min_time,
            state_domain,
        }
    }

    /// Advance the retained monotone `fmi3SetTime` lower bound, as the host does
    /// when it commits a completed integrator step.
    pub fn set_monotone_bound(&self, time: f64) {
        self.min_time.set(time);
    }

    /// Issue the one retained derivative handle `initialize` receives.
    #[must_use]
    pub fn issue_handle(&self) -> MeDerivativeHandle {
        self.controller.issue_handle()
    }

    /// Open the activation window for exactly one backend call. The capability
    /// closes when the returned guard is dropped.
    #[must_use]
    pub fn activate(&self) -> MeBackendWindow<'_> {
        MeBackendWindow {
            _activation: self.controller.activate(),
        }
    }

    /// Build a checked continuous point at the probe's width.
    pub fn point(
        &self,
        time: f64,
        states: Vec<f64>,
    ) -> Result<MeContinuousPoint, MeIntegrationError> {
        MeContinuousPoint::new(time, states, self.state_domain)
    }

    /// Build the same opaque numerical setup a production host hands a
    /// backend. This exists only in the test-support surface; production
    /// plugins have no setup constructor and cannot supply a second width.
    pub fn numerical_setup(
        &self,
        relative_tolerance: f64,
        absolute_tolerance: f64,
        initial_step_hint: Option<f64>,
    ) -> Result<MeNumericalSetup, MeIntegrationError> {
        let tolerances = super::MeSolverTolerances::check(relative_tolerance, absolute_tolerance)
            .map_err(|error| MeIntegrationError::contract(error.to_string()))?;
        MeNumericalSetup::from_checked_host(
            tolerances,
            vec![1.0; self.state_domain.len()],
            initial_step_hint,
        )
    }

    /// Build a checked advance request whose reachable bound is `latest`.
    pub fn request(
        &self,
        current: MeContinuousPoint,
        latest: f64,
    ) -> Result<MeAdvanceRequest, MeIntegrationError> {
        MeAdvanceRequest::new(current, Some(latest), latest, None, None)
    }

    /// The first typed failure a handle callback latched, if any. A refused
    /// derivative query (a coordinate behind the monotone bound) surfaces here.
    #[must_use]
    pub fn take_error(&self) -> Option<MeIntegrationError> {
        self.controller.take_error()
    }
}

/// An open activation window; the capability closes when this is dropped.
pub struct MeBackendWindow<'probe> {
    _activation: MeDerivativeActivation<'probe>,
}
