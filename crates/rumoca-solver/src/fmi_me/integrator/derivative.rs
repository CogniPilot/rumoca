//! The retained opaque derivative handle and its host-private activation
//! controller (SPEC_0044 §6, ME-INT-001/002).
//!
//! A persistent implicit method cannot store a capability lent for the duration
//! of one call, and rebuilding its problem on every accepted step would discard
//! exactly the order and difference history SPEC_0044 §7 requires evidence
//! against. The contract is therefore one *retained* handle whose reachability
//! is governed by a host-owned activation state rather than by a Rust lifetime.
//!
//! [`MeDerivativeHandle`] is the whole plugin-visible surface: one opaque,
//! non-`Clone` value naming no kernel, no Solve root, no FMI lifecycle
//! transition, no event/root/trace policy, and no concrete solver. A plugin may
//! store it inside a persistent numerical problem. It may evaluate through it
//! only while the host is executing `initialize`, exactly one `advance`, or
//! `truncate_reset`; during `sample` and outside every host call the capability
//! is inactive and every request is a typed contract failure.
//!
//! [`MeDerivativeController`] is the other half and never leaves the common
//! host. It owns activation, issues one handle per `initialize`, and reads the
//! latched failure. The shared cell behind both is an implementation detail: no
//! public API names it, and no public API hands out an `Rc`.

use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use super::MeIntegrationError;
use crate::fmi_me::{
    MeContinuousStateDomain, MeDirectionalKnownBatch, MeDirectionalUnknownBatch, MeError, MeTime,
    SolveMeKernel,
};

/// What a handle actually evaluates.
///
/// Host-private, so the concrete component source and its constructor are not
/// nameable from a plugin and no plugin-visible type mentions the FMI kernel.
pub(in crate::fmi_me) trait MeDerivativeComponent {
    fn state_count(&self) -> usize;

    /// `fmi3SetTime` + `fmi3SetContinuousStates` +
    /// `fmi3GetContinuousStateDerivatives`.
    fn derivatives_into(&self, time: f64, states: &[f64], out: &mut [f64]) -> Result<(), MeError>;

    /// `fmi3SetTime` + `fmi3SetContinuousStates` +
    /// `fmi3GetDirectionalDerivative`.
    fn directional_derivative_into(
        &self,
        time: f64,
        states: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeError>;
}

/// The sole production derivative source: the one leased FMI component.
struct KernelDerivatives {
    kernel: Rc<RefCell<SolveMeKernel>>,
    state_domain: MeContinuousStateDomain,
    state_knowns: MeDirectionalKnownBatch,
    derivative_unknowns: MeDirectionalUnknownBatch,
}

impl MeDerivativeComponent for KernelDerivatives {
    fn state_count(&self) -> usize {
        self.state_domain.len()
    }

    fn derivatives_into(&self, time: f64, states: &[f64], out: &mut [f64]) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        kernel.get_continuous_state_derivatives(out)
    }

    fn directional_derivative_into(
        &self,
        time: f64,
        states: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        kernel.get_directional_derivative(&self.derivative_unknowns, &self.state_knowns, seed, out)
    }
}

/// The activation state and the latched failure the handle and the controller
/// share.
///
/// Private, unnameable, and never handed out: "the backend retains a handle"
/// and "the host retains the controller" are the only two facts either side of
/// the boundary can observe.
struct DerivativeCell {
    component: Box<dyn MeDerivativeComponent>,
    active: Cell<bool>,
    pending: RefCell<Option<MeIntegrationError>>,
}

impl DerivativeCell {
    /// The activation gate SPEC_0044 §6's ruling puts in front of every
    /// derivative request.
    fn require_active(&self, operation: &'static str) -> Result<(), MeIntegrationError> {
        if self.active.get() {
            return Ok(());
        }
        Err(MeIntegrationError::DerivativeCapabilityInactive { operation })
    }

    /// Latch the typed cause and hand the backend only the opaque refusal.
    ///
    /// Every public handle entry funnels through here, so no failure reaches a
    /// backend un-latched and none reaches it with its identity attached
    /// The first failure wins: a later one is a
    /// consequence of it.
    fn refuse(&self, error: MeIntegrationError) -> MeDerivativeRefused {
        let mut pending = self.pending.borrow_mut();
        if pending.is_none() {
            *pending = Some(error);
        }
        MeDerivativeRefused
    }

    fn evaluate(
        &self,
        time: f64,
        states: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeIntegrationError> {
        self.require_active("a state-derivative evaluation")?;
        self.component
            .derivatives_into(time, states, out)
            .map_err(MeIntegrationError::from)
    }

    fn evaluate_directional(
        &self,
        time: f64,
        states: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeIntegrationError> {
        self.require_active("a directional-derivative evaluation")?;
        self.component
            .directional_derivative_into(time, states, seed, out)
            .map_err(MeIntegrationError::from)
    }
}

/// The retained opaque derivative handle a numerical plugin may store.
///
/// It is deliberately **not** `Clone`: a plugin can neither duplicate nor mint
/// one, and the host issues exactly one per [`MeIntegratorBackend::initialize`]
/// through its private controller. Nothing on this type names a kernel, a Solve
/// object, an FMI lifecycle transition, an event/root/trace policy, or a
/// concrete solver, so ME-INT-002's public-API scan has nothing to find.
///
/// [`MeIntegratorBackend::initialize`]: super::MeIntegratorBackend::initialize
pub struct MeDerivativeHandle {
    shared: Rc<DerivativeCell>,
}

impl std::fmt::Debug for MeDerivativeHandle {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("MeDerivativeHandle")
            .field("state_count", &self.state_count())
            .field("active", &self.shared.active.get())
            .finish()
    }
}

/// What a refused retained-handle evaluation tells the backend.
///
/// Deliberately identity-free. The typed cause — component failure, discard,
/// inactive-capability misuse, allocation — is latched inside the host-private
/// cell, and only the common host may read or consume it. A backend therefore
/// cannot erase the host's failure, cannot substitute its own for it, and
/// cannot reconstruct it from this marker.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
#[error("a retained derivative evaluation was refused; the host owns the typed cause")]
pub struct MeDerivativeRefused;

impl From<MeDerivativeRefused> for MeIntegrationError {
    fn from(_: MeDerivativeRefused) -> Self {
        Self::DerivativeRefused
    }
}

impl MeDerivativeHandle {
    /// The component's continuous-state width.
    #[must_use]
    pub fn state_count(&self) -> usize {
        self.shared.component.state_count()
    }

    /// The state derivatives at `(time, states)`.
    ///
    /// A failure is **latched, not returned**: every public entry on this type
    /// records the typed cause in the host-private cell before control returns
    /// to the backend, so catching or ignoring the refusal cannot bypass the
    /// host's precedence.
    pub fn derivatives(&self, time: f64, states: &[f64]) -> Result<Vec<f64>, MeDerivativeRefused> {
        let width = self.state_count();
        let mut values = Vec::new();
        if values.try_reserve_exact(width).is_err() {
            return Err(self.shared.refuse(MeIntegrationError::Allocation {
                context: "derivative evaluation",
                entries: width,
            }));
        }
        values.resize(width, 0.0);
        match self.shared.evaluate(time, states, &mut values) {
            Ok(()) => Ok(values),
            Err(error) => Err(self.shared.refuse(error)),
        }
    }

    /// The infallible-signature form a numerical library's callback needs.
    ///
    /// A failure fills `out` with NaN and latches the typed cause.
    pub fn derivatives_into(&self, time: f64, states: &[f64], out: &mut [f64]) {
        if let Err(error) = self.shared.evaluate(time, states, out) {
            out.fill(f64::NAN);
            self.shared.refuse(error);
        }
    }

    /// The directional derivative an implicit plugin's Newton direction needs.
    pub fn directional_derivative_into(
        &self,
        time: f64,
        states: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) {
        if let Err(error) = self.shared.evaluate_directional(time, states, seed, out) {
            out.fill(f64::NAN);
            self.shared.refuse(error);
        }
    }

    /// Whether a callback of this handle has already been refused.
    ///
    /// A numerical library that must unwind on the first bad callback reads
    /// this. It is **non-consuming and identity-free**: consumption authority
    /// is host-private, so a backend cannot take, clear, or replace the typed
    /// failure the host is about to join with the backend's own result
    #[must_use]
    pub fn has_failed(&self) -> bool {
        self.shared.pending.borrow().is_some()
    }
}

/// The host-private half of the retained capability.
///
/// It never crosses the plugin boundary. Activation, handle issuance, and the
/// latched failure are all host authority, which is what makes "the plugin may
/// retain the handle" strictly weaker than "the plugin may reach the component".
pub(in crate::fmi_me) struct MeDerivativeController {
    shared: Rc<DerivativeCell>,
}

impl MeDerivativeController {
    /// The production controller over the one leased FMI component.
    pub(in crate::fmi_me) fn over_kernel(
        kernel: Rc<RefCell<SolveMeKernel>>,
    ) -> Result<Self, MeError> {
        let (state_domain, state_knowns, derivative_unknowns) = {
            let kernel = kernel.borrow();
            let state_domain = kernel.continuous_state_domain();
            let state_references = kernel.continuous_state_value_references()?;
            let derivative_references = kernel.continuous_state_derivative_value_references()?;
            (
                state_domain,
                kernel.directional_known_batch(state_references)?,
                kernel.directional_unknown_batch(derivative_references)?,
            )
        };
        Ok(Self::over_component(Box::new(KernelDerivatives {
            kernel,
            state_domain,
            state_knowns,
            derivative_unknowns,
        })))
    }

    fn over_component(component: Box<dyn MeDerivativeComponent>) -> Self {
        Self {
            shared: Rc::new(DerivativeCell {
                component,
                active: Cell::new(false),
                pending: RefCell::new(None),
            }),
        }
    }

    /// Issue the one handle a plugin retains for this `initialize`.
    pub(in crate::fmi_me) fn issue_handle(&self) -> MeDerivativeHandle {
        MeDerivativeHandle {
            shared: Rc::clone(&self.shared),
        }
    }

    /// Open the activation window for exactly one host call.
    ///
    /// The returned guard deactivates on drop, so "the host deactivates on
    /// every exit" is structural rather than a discipline every call site has
    /// to remember.
    pub(in crate::fmi_me) fn activate(&self) -> MeDerivativeActivation<'_> {
        self.shared.active.set(true);
        MeDerivativeActivation {
            shared: &self.shared,
        }
    }

    #[must_use]
    pub(in crate::fmi_me) fn is_active(&self) -> bool {
        self.shared.active.get()
    }

    /// The first typed failure a callback latched, if any.
    ///
    /// Consumption is host-only: this is the sole way the latch is ever
    /// emptied, and no plugin-visible method can reach it.
    #[must_use]
    pub(in crate::fmi_me) fn take_error(&self) -> Option<MeIntegrationError> {
        self.shared.pending.borrow_mut().take()
    }
}

/// The open activation window of one host call.
///
/// Deactivation is infallible and unconditional, so a backend error, an early
/// return, or a panic can never strand an active capability.
pub(in crate::fmi_me) struct MeDerivativeActivation<'host> {
    shared: &'host Rc<DerivativeCell>,
}

impl Drop for MeDerivativeActivation<'_> {
    fn drop(&mut self) {
        self.shared.active.set(false);
    }
}

/// A constant-rate derivative source that enforces a monotone time bound.
///
/// `y' = rates` (a constant vector), so the Jacobian is zero and the implicit
/// solve is well posed with a directional derivative of zero. `min_time` models
/// the component's retained `fmi3SetTime` lower bound: a derivative query at a
/// coordinate strictly behind it is refused with the same typed component
/// failure the real component raises, which is what makes an in-place numerical
/// restart at a problem's original `t0` observable to a backend regression.
#[cfg(any(test, feature = "test-support"))]
struct MonotoneLinearComponent {
    rates: Vec<f64>,
    min_time: Rc<Cell<f64>>,
}

#[cfg(any(test, feature = "test-support"))]
impl MonotoneLinearComponent {
    fn require_within_bound(&self, time: f64) -> Result<(), MeError> {
        if time < self.min_time.get() {
            return Err(MeError::Contract {
                reason: format!(
                    "derivative query at t={time} precedes the retained monotone bound {}",
                    self.min_time.get()
                ),
            });
        }
        Ok(())
    }
}

#[cfg(any(test, feature = "test-support"))]
impl MeDerivativeComponent for MonotoneLinearComponent {
    fn state_count(&self) -> usize {
        self.rates.len()
    }

    fn derivatives_into(&self, time: f64, states: &[f64], out: &mut [f64]) -> Result<(), MeError> {
        self.require_within_bound(time)?;
        if states.len() != self.rates.len() || out.len() != self.rates.len() {
            return Err(MeError::Contract {
                reason: "monotone-linear derivative source width mismatch".to_owned(),
            });
        }
        out.copy_from_slice(&self.rates);
        Ok(())
    }

    fn directional_derivative_into(
        &self,
        time: f64,
        _states: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeError> {
        self.require_within_bound(time)?;
        if seed.len() != self.rates.len() || out.len() != self.rates.len() {
            return Err(MeError::Contract {
                reason: "monotone-linear directional source width mismatch".to_owned(),
            });
        }
        // The Jacobian of a constant rate is zero.
        out.fill(0.0);
        Ok(())
    }
}

#[cfg(any(test, feature = "test-support"))]
impl MeDerivativeController {
    /// A controller over a constant-rate source with a shared monotone bound.
    ///
    /// The caller retains a clone of `min_time` to advance the bound between
    /// accepted steps, exactly as the host advances the component's own
    /// `fmi3SetTime` lower bound on a completed step.
    pub(in crate::fmi_me) fn over_monotone_linear(
        rates: Vec<f64>,
        min_time: Rc<Cell<f64>>,
    ) -> Self {
        Self::over_component(Box::new(MonotoneLinearComponent { rates, min_time }))
    }
}

/// A closure-backed derivative source.
///
/// The manufactured-solution convergence suite drives a plugin through exactly
/// the handle contract a real component is driven through, so one plugin
/// implementation cannot tell a manufactured solution from an FMI component.
#[cfg(test)]
pub(in crate::fmi_me) type DerivativeClosure = Rc<dyn Fn(f64, &[f64]) -> Vec<f64>>;

#[cfg(test)]
struct ClosureDerivatives {
    derivative: DerivativeClosure,
    state_count: usize,
}

#[cfg(test)]
impl MeDerivativeComponent for ClosureDerivatives {
    fn state_count(&self) -> usize {
        self.state_count
    }

    fn derivatives_into(&self, time: f64, states: &[f64], out: &mut [f64]) -> Result<(), MeError> {
        let values = (self.derivative)(time, states);
        if values.len() != out.len() {
            return Err(MeError::Contract {
                reason: format!(
                    "the manufactured solution returned {} values for a width of {}",
                    values.len(),
                    out.len()
                ),
            });
        }
        out.copy_from_slice(&values);
        Ok(())
    }

    fn directional_derivative_into(
        &self,
        _time: f64,
        _states: &[f64],
        _seed: &[f64],
        _out: &mut [f64],
    ) -> Result<(), MeError> {
        Err(MeError::DirectionalDerivativeUnavailable {
            reason: "a manufactured solution supplies no linearization".to_owned(),
        })
    }
}

#[cfg(test)]
impl MeDerivativeController {
    /// A controller over a manufactured solution.
    pub(in crate::fmi_me) fn over_closure(
        state_count: usize,
        derivative: DerivativeClosure,
    ) -> Self {
        Self::over_component(Box::new(ClosureDerivatives {
            derivative,
            state_count,
        }))
    }
}

/// A handle attached to no live host call at all.
///
/// The time-only plugin and the arity fixtures integrate nothing the component
/// supplies. Handing them this proves it: the controller is dropped as this
/// returns, so the window can never open and every evaluation is the typed
/// inactive-capability failure rather than a silent zero.
#[cfg(test)]
pub(in crate::fmi_me) fn detached_handle() -> MeDerivativeHandle {
    MeDerivativeController::over_closure(0, Rc::new(|_time, _states| Vec::new())).issue_handle()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn controller() -> MeDerivativeController {
        MeDerivativeController::over_closure(1, Rc::new(|time, states| vec![time + states[0]]))
    }

    #[test]
    fn a_handle_evaluates_only_inside_an_open_activation_window() {
        let controller = controller();
        let handle = controller.issue_handle();
        assert!(!controller.is_active());
        assert!(handle.derivatives(1.0, &[2.0]).is_err());
        assert!(handle.has_failed());
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::DerivativeCapabilityInactive { .. })
        ));

        {
            let _window = controller.activate();
            assert!(controller.is_active());
            assert_eq!(
                handle.derivatives(1.0, &[2.0]).expect("an open window"),
                vec![3.0]
            );
            assert!(!handle.has_failed());
        }

        assert!(!controller.is_active());
        assert!(handle.derivatives(1.0, &[2.0]).is_err());
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::DerivativeCapabilityInactive { .. })
        ));
    }

    /// The plugin-visible surface can observe that a refusal happened, but it
    /// cannot take, clear, or replace the typed cause. Only the host-private
    /// controller empties the latch.
    #[test]
    fn a_backend_can_observe_a_refusal_but_never_consume_it() {
        let controller = controller();
        let handle = controller.issue_handle();

        let refusal = handle
            .derivatives(1.0, &[2.0])
            .expect_err("the window is closed");
        assert_eq!(refusal, MeDerivativeRefused);
        assert!(
            !refusal.to_string().contains("inactive"),
            "the marker must carry no failure identity: {refusal}"
        );

        // Every plugin-visible read leaves the latch exactly where it was.
        for _ in 0..3 {
            assert!(handle.has_failed());
        }
        let mut out = [0.0];
        handle.derivatives_into(1.0, &[2.0], &mut out);
        handle.directional_derivative_into(1.0, &[2.0], &[1.0], &mut out);
        assert!(handle.has_failed());

        // And the host still gets the first typed cause, unreplaced.
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::DerivativeCapabilityInactive {
                operation: "a state-derivative evaluation"
            })
        ));
        assert!(!handle.has_failed());
    }

    /// The infallible-signature forms cannot return the refusal, so they fill
    /// NaN and latch it as typed data for the host and the plugin to read.
    #[test]
    fn an_inactive_infallible_request_latches_a_typed_failure() {
        let controller = controller();
        let handle = controller.issue_handle();
        let mut out = [0.0];
        handle.derivatives_into(1.0, &[2.0], &mut out);
        assert!(out[0].is_nan());
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::DerivativeCapabilityInactive { .. })
        ));
        assert!(controller.take_error().is_none());

        let mut sensitivity = [0.0];
        handle.directional_derivative_into(1.0, &[2.0], &[1.0], &mut sensitivity);
        assert!(sensitivity[0].is_nan());
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::DerivativeCapabilityInactive { .. })
        ));
    }

    /// The window closes on every exit, including a panic unwinding out of the
    /// host call, because deactivation is the guard's `Drop`.
    #[test]
    fn the_window_closes_even_when_the_host_call_unwinds() {
        let controller = controller();
        let unwound = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _window = controller.activate();
            panic!("a backend panicked mid-advance");
        }));
        assert!(unwound.is_err());
        assert!(!controller.is_active());
    }

    /// The first failure is the one that survives: a later consequence must not
    /// overwrite the cause.
    #[test]
    fn only_the_first_latched_failure_is_retained() {
        let controller = controller();
        let handle = controller.issue_handle();
        let mut out = [0.0];
        handle.derivatives_into(1.0, &[2.0], &mut out);
        handle.directional_derivative_into(1.0, &[2.0], &[1.0], &mut out);
        let first = controller.take_error().expect("one latched failure");
        assert!(
            first.to_string().contains("a state-derivative evaluation"),
            "{first}"
        );
        assert!(controller.take_error().is_none());
    }

    #[test]
    fn a_detached_handle_can_never_reach_a_component() {
        let handle = detached_handle();
        assert_eq!(handle.state_count(), 0);
        assert!(handle.derivatives(0.0, &[]).is_err());
        assert!(
            handle.has_failed(),
            "the refusal is latched even with no host controller left to read it"
        );
    }
}
