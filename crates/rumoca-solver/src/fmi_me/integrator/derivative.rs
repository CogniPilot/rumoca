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
//! store it inside a persistent numerical problem. It may evaluate derivatives,
//! JVPs, or the host-owned observable-error callback only while the host is
//! executing `initialize`, exactly one `advance`, or `truncate_reset`; during
//! `sample` and outside every host call the capability is inactive and every
//! request is a typed contract failure.
//!
//! [`MeDerivativeController`] is the other half and never leaves the common
//! host. It owns activation, issues one handle per `initialize`, and reads the
//! latched failure. The shared cell behind both is an implementation detail: no
//! public API names it, and no public API hands out an `Rc`.

use std::{
    cell::{Cell, RefCell},
    panic::{AssertUnwindSafe, catch_unwind},
    rc::Rc,
};

use super::MeIntegrationError;
use crate::fmi_me::{MeError, MeTime, SolveMeKernel};

/// What a handle actually evaluates.
///
/// Host-private, so the concrete component source and its constructor are not
/// nameable from a plugin and no plugin-visible type mentions the FMI kernel.
pub(in crate::fmi_me) trait MeDerivativeComponent {
    fn state_count(&self) -> usize;

    /// `fmi3SetTime` + `fmi3SetContinuousStates` +
    /// `fmi3GetContinuousStateDerivatives`.
    fn derivatives_into(
        &self,
        time: f64,
        states: &[f64],
        event_boundary: Option<f64>,
        out: &mut [f64],
    ) -> Result<(), MeError>;

    /// `fmi3SetTime` + `fmi3SetContinuousStates` +
    /// `fmi3GetDirectionalDerivative`.
    fn directional_derivative_into(
        &self,
        time: f64,
        states: &[f64],
        event_boundary: Option<f64>,
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeError>;

    /// Project the checked published continuous Real channels at one trial
    /// state. The implementation owns typed channel selection and the
    /// transactional standard ME value projection; no metadata or component
    /// internals cross the opaque handle boundary.
    fn observable_channel_count(&self) -> usize {
        0
    }

    fn observable_channel_nominals(&self) -> &[f64] {
        &[]
    }

    fn observable_values_into(
        &self,
        _time: f64,
        _states: &[f64],
        _event_boundary: Option<f64>,
        _out: &mut [f64],
    ) -> Result<(), MeError> {
        Err(MeError::Contract {
            reason: "this component has no observable-error projection".to_owned(),
        })
    }
}

/// The sole production derivative source: the one leased FMI component.
struct KernelDerivatives {
    kernel: Rc<RefCell<SolveMeKernel>>,
    state_count: usize,
    observable_channels: Vec<super::super::MeObservableChannel>,
    observable_nominals: Vec<f64>,
}

impl MeDerivativeComponent for KernelDerivatives {
    fn state_count(&self) -> usize {
        self.state_count
    }

    fn derivatives_into(
        &self,
        time: f64,
        states: &[f64],
        event_boundary: Option<f64>,
        out: &mut [f64],
    ) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::new(time, event_boundary))?;
        kernel.set_continuous_states(states)?;
        kernel.continuous_state_derivatives_into(out)
    }

    fn directional_derivative_into(
        &self,
        time: f64,
        states: &[f64],
        event_boundary: Option<f64>,
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::new(time, event_boundary))?;
        kernel.set_continuous_states(states)?;
        kernel.get_directional_derivative(seed, out)
    }

    fn observable_channel_count(&self) -> usize {
        self.observable_channels.len()
    }

    fn observable_channel_nominals(&self) -> &[f64] {
        &self.observable_nominals
    }

    fn observable_values_into(
        &self,
        time: f64,
        states: &[f64],
        event_boundary: Option<f64>,
        out: &mut [f64],
    ) -> Result<(), MeError> {
        if out.len() != self.observable_channels.len() {
            return Err(MeError::Contract {
                reason: "observable projection buffer does not match its typed channel inventory"
                    .to_owned(),
            });
        }
        let mut kernel = self.kernel.borrow_mut();
        with_observable_transaction(&mut kernel, time, states, event_boundary, |kernel| {
            let observation = kernel.observe()?;
            let mut visible = Vec::new();
            kernel.get_outputs(&observation, observation.time(), &mut visible)?;
            copy_observable_channels(&visible, &self.observable_channels, out)
        })
    }
}

pub(in crate::fmi_me) fn with_observable_transaction<T>(
    kernel: &mut SolveMeKernel,
    time: f64,
    states: &[f64],
    event_boundary: Option<f64>,
    operation: impl FnOnce(&mut SolveMeKernel) -> Result<T, MeError>,
) -> Result<T, MeError> {
    let saved = kernel.fmu_state();
    let result = catch_unwind(AssertUnwindSafe(|| {
        kernel.set_time(MeTime::new(time, event_boundary))?;
        kernel.set_continuous_states(states)?;
        operation(kernel)
    }));
    let restored = kernel.reset_to_fmu_state(&saved);
    match (result, restored) {
        (Ok(Ok(value)), Ok(())) => Ok(value),
        (Ok(Err(error)), Ok(())) => Err(error),
        (Ok(Ok(_)), Err(error)) => Err(MeError::Contract {
            reason: format!("observable projection could not restore the component: {error}"),
        }),
        (Ok(Err(error)), Err(restore)) => Err(MeError::Contract {
            reason: format!(
                "observable projection failed ({error}) and restoration failed ({restore})"
            ),
        }),
        (Err(_), Ok(())) => Err(MeError::Contract {
            reason: "observable projection panicked after restoring the component".to_owned(),
        }),
        (Err(_), Err(restore)) => Err(MeError::Contract {
            reason: format!("observable projection panicked and restoration failed ({restore})"),
        }),
    }
}

fn copy_observable_channels(
    visible: &[f64],
    channels: &[super::super::MeObservableChannel],
    out: &mut [f64],
) -> Result<(), MeError> {
    for (slot, channel) in out.iter_mut().zip(channels) {
        *slot = *visible
            .get(channel.visible_index)
            .ok_or_else(|| MeError::Contract {
                reason: "typed observable channel points outside the visible value inventory"
                    .to_owned(),
            })?;
    }
    Ok(())
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
    event_boundary: Cell<Option<f64>>,
    pending: RefCell<Option<MeIntegrationError>>,
    relative_tolerance: f64,
    absolute_tolerance: f64,
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
            .derivatives_into(time, states, self.event_boundary.get(), out)
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
            .directional_derivative_into(time, states, self.event_boundary.get(), seed, out)
            .map_err(MeIntegrationError::from)
    }

    fn project_observable_values(
        &self,
        time: f64,
        states: &[f64],
        out: &mut [f64],
    ) -> Result<(), MeIntegrationError> {
        match catch_unwind(AssertUnwindSafe(|| {
            self.component
                .observable_values_into(time, states, self.event_boundary.get(), out)
        })) {
            Ok(Ok(())) => Ok(()),
            Ok(Err(error)) => Err(MeIntegrationError::Component(error)),
            Err(_) => Err(MeIntegrationError::contract(
                "observable-error component projection panicked",
            )),
        }
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

    /// Evaluate the host-owned observable error norm for one actual/trial
    /// pair. `estimated_delta` is added to `actual_states`; an estimate of
    /// numerical-minus-exact error must be negated by the caller.
    /// The state error criterion remains the numerical method's own
    /// independent check; this capability only measures published continuous
    /// Real channels, including host-projected algebraics.
    pub fn observable_error(
        &self,
        time: f64,
        actual_states: &[f64],
        estimated_delta: &[f64],
    ) -> Result<f64, MeDerivativeRefused> {
        if let Err(error) = self.shared.require_active("an observable-error evaluation") {
            return Err(self.shared.refuse(error));
        }
        let width = self.state_count();
        if actual_states.len() != width || estimated_delta.len() != width {
            return Err(self.shared.refuse(MeIntegrationError::contract(
                "observable-error state vectors do not match the checked component width",
            )));
        }
        if !time.is_finite()
            || actual_states.iter().any(|value| !value.is_finite())
            || estimated_delta.iter().any(|value| !value.is_finite())
        {
            return Err(self.shared.refuse(MeIntegrationError::contract(
                "observable-error inputs must be finite",
            )));
        }
        let mut trial_states = Vec::new();
        if trial_states.try_reserve_exact(width).is_err() {
            return Err(self.shared.refuse(MeIntegrationError::Allocation {
                context: "observable-error trial states",
                entries: width,
            }));
        }
        trial_states.extend(
            actual_states
                .iter()
                .copied()
                .zip(estimated_delta.iter().copied())
                .map(|(actual, delta)| actual + delta),
        );
        if trial_states.iter().any(|value| !value.is_finite()) {
            return Err(self.shared.refuse(MeIntegrationError::contract(
                "observable-error trial states must be finite",
            )));
        }
        match observable_error_norm(&self.shared, time, actual_states, &trial_states) {
            Ok(norm) => Ok(norm),
            Err(error) => Err(self.shared.refuse(error)),
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

fn observable_error_norm(
    shared: &DerivativeCell,
    time: f64,
    actual_states: &[f64],
    trial_states: &[f64],
) -> Result<f64, MeIntegrationError> {
    let channel_count = shared.component.observable_channel_count();
    let nominals = shared.component.observable_channel_nominals();
    if nominals.len() != channel_count
        || nominals
            .iter()
            .any(|nominal| !nominal.is_finite() || *nominal <= 0.0)
    {
        return Err(MeIntegrationError::contract(
            "observable-error projection has invalid typed nominal metadata",
        ));
    }
    if channel_count == 0 {
        return Ok(0.0);
    }
    let mut actual = Vec::new();
    let mut trial = Vec::new();
    if actual.try_reserve_exact(channel_count).is_err()
        || trial.try_reserve_exact(channel_count).is_err()
    {
        return Err(MeIntegrationError::Allocation {
            context: "observable-error projection values",
            entries: channel_count,
        });
    }
    actual.resize(channel_count, 0.0);
    trial.resize(channel_count, 0.0);
    actual.fill(f64::NAN);
    trial.fill(f64::NAN);
    shared.project_observable_values(time, actual_states, &mut actual)?;
    shared.project_observable_values(time, trial_states, &mut trial)?;
    if actual.iter().chain(&trial).any(|value| !value.is_finite()) {
        return Err(MeIntegrationError::contract(
            "observable-error projection returned a non-finite value",
        ));
    }
    let mut norm: f64 = 0.0;
    for ((actual, trial), nominal) in actual.into_iter().zip(trial).zip(nominals) {
        let delta = (trial - actual).abs();
        let bound = observable_channel_bound(
            *nominal,
            actual,
            trial,
            shared.relative_tolerance,
            shared.absolute_tolerance,
        )?;
        let ratio = delta / bound;
        if !delta.is_finite() || !ratio.is_finite() {
            return Err(MeIntegrationError::contract(
                "observable-error normalization overflowed or became non-finite",
            ));
        }
        norm = norm.max(ratio);
    }
    Ok(norm)
}

/// Apply the same absolute-nominal scaling used to construct state LTE
/// tolerances. The relative term follows the current/trial state magnitude;
/// the nominal is an absolute-unit scale, not an extra relative floor.
fn observable_channel_bound(
    nominal: f64,
    actual: f64,
    trial: f64,
    relative_tolerance: f64,
    absolute_tolerance: f64,
) -> Result<f64, MeIntegrationError> {
    if !nominal.is_finite()
        || nominal <= 0.0
        || !actual.is_finite()
        || !trial.is_finite()
        || !relative_tolerance.is_finite()
        || relative_tolerance < 0.0
        || !absolute_tolerance.is_finite()
        || absolute_tolerance <= 0.0
    {
        return Err(MeIntegrationError::contract(
            "observable-error normalization received invalid scale inputs",
        ));
    }
    let absolute = (absolute_tolerance * nominal).clamp(f64::MIN_POSITIVE, f64::MAX);
    let magnitude = actual.abs().max(trial.abs());
    let bound = absolute + relative_tolerance * magnitude;
    if !bound.is_finite() || bound <= 0.0 {
        return Err(MeIntegrationError::contract(
            "observable-error normalization overflowed or became non-finite",
        ));
    }
    Ok(bound)
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
        relative_tolerance: f64,
        absolute_tolerance: f64,
    ) -> Result<Self, MeIntegrationError> {
        let state_count = kernel.borrow().model_description().continuous_state_count;
        let observable_channels = kernel.borrow().observable_channels().to_vec();
        let observable_nominals = observable_channels
            .iter()
            .map(|channel| channel.nominal)
            .collect();
        Self::over_component_with_tolerances(
            Box::new(KernelDerivatives {
                observable_nominals,
                observable_channels,
                kernel,
                state_count,
            }),
            relative_tolerance,
            absolute_tolerance,
        )
    }

    #[cfg(test)]
    fn over_component(component: Box<dyn MeDerivativeComponent>) -> Self {
        Self::over_component_with_tolerances(component, 1.0e-6, 1.0e-9)
            .expect("the fixed test capability tolerances are valid")
    }

    fn over_component_with_tolerances(
        component: Box<dyn MeDerivativeComponent>,
        relative_tolerance: f64,
        absolute_tolerance: f64,
    ) -> Result<Self, MeIntegrationError> {
        if !relative_tolerance.is_finite()
            || !absolute_tolerance.is_finite()
            || relative_tolerance < 0.0
            || absolute_tolerance <= 0.0
        {
            return Err(MeIntegrationError::contract(
                "observable-error norm tolerances must be finite, with rtol >= 0 and atol > 0",
            ));
        }
        Ok(Self {
            shared: Rc::new(DerivativeCell {
                component,
                active: Cell::new(false),
                event_boundary: Cell::new(None),
                pending: RefCell::new(None),
                relative_tolerance,
                absolute_tolerance,
            }),
        })
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
    #[cfg(test)]
    pub(in crate::fmi_me) fn activate(&self) -> MeDerivativeActivation<'_> {
        self.activate_until(None)
    }

    /// Open one activation window while preserving a scheduled event's left
    /// limit through every derivative callback made by the numerical plugin.
    pub(in crate::fmi_me) fn activate_until(
        &self,
        event_boundary: Option<f64>,
    ) -> MeDerivativeActivation<'_> {
        self.shared.event_boundary.set(event_boundary);
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
        self.shared.event_boundary.set(None);
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
pub(in crate::fmi_me) type ObservableClosure = Rc<dyn Fn(f64, &[f64]) -> Vec<f64>>;

#[cfg(test)]
struct ClosureDerivatives {
    derivative: DerivativeClosure,
    state_count: usize,
    observable: Option<ObservableClosure>,
    observable_channel_count: usize,
    observable_nominals: Vec<f64>,
}

#[cfg(test)]
impl MeDerivativeComponent for ClosureDerivatives {
    fn state_count(&self) -> usize {
        self.state_count
    }

    fn derivatives_into(
        &self,
        time: f64,
        states: &[f64],
        _event_boundary: Option<f64>,
        out: &mut [f64],
    ) -> Result<(), MeError> {
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
        _event_boundary: Option<f64>,
        _seed: &[f64],
        _out: &mut [f64],
    ) -> Result<(), MeError> {
        Err(MeError::DirectionalDerivativeUnavailable {
            reason: "a manufactured solution supplies no linearization".to_owned(),
        })
    }

    fn observable_channel_count(&self) -> usize {
        self.observable_channel_count
    }

    fn observable_channel_nominals(&self) -> &[f64] {
        &self.observable_nominals
    }

    fn observable_values_into(
        &self,
        time: f64,
        states: &[f64],
        _event_boundary: Option<f64>,
        out: &mut [f64],
    ) -> Result<(), MeError> {
        let Some(observable) = &self.observable else {
            return Err(MeError::Contract {
                reason: "the closure fixture has no observable projection".to_owned(),
            });
        };
        let values = observable(time, states);
        if values.len() != out.len() {
            return Err(MeError::Contract {
                reason: format!(
                    "the observable fixture returned {} values for a width of {}",
                    values.len(),
                    out.len()
                ),
            });
        }
        out.copy_from_slice(&values);
        Ok(())
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
            observable: None,
            observable_channel_count: 0,
            observable_nominals: Vec::new(),
        }))
    }

    pub(in crate::fmi_me) fn over_observable_closure(
        state_count: usize,
        derivative: DerivativeClosure,
        observable: ObservableClosure,
        observable_channel_count: usize,
        relative_tolerance: f64,
        absolute_tolerance: f64,
    ) -> Result<Self, MeIntegrationError> {
        Self::over_component_with_tolerances(
            Box::new(ClosureDerivatives {
                derivative,
                state_count,
                observable: Some(observable),
                observable_channel_count,
                observable_nominals: vec![1.0; observable_channel_count],
            }),
            relative_tolerance,
            absolute_tolerance,
        )
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

    fn observable_controller(lambda: f64) -> MeDerivativeController {
        MeDerivativeController::over_observable_closure(
            1,
            Rc::new(|_time, _states| vec![0.0]),
            Rc::new(move |_time, states| vec![lambda * (1.0 - states[0])]),
            1,
            1.0e-3,
            1.0e-6,
        )
        .expect("analytic observable fixture tolerances are valid")
    }

    fn scaled_observable_controller(
        nominal: f64,
        scale: f64,
        relative_tolerance: f64,
        absolute_tolerance: f64,
    ) -> MeDerivativeController {
        MeDerivativeController::over_component_with_tolerances(
            Box::new(ClosureDerivatives {
                derivative: Rc::new(|_time, _states| vec![0.0]),
                state_count: 1,
                observable: Some(Rc::new(move |_time, states| vec![scale * states[0]])),
                observable_channel_count: 1,
                observable_nominals: vec![nominal],
            }),
            relative_tolerance,
            absolute_tolerance,
        )
        .expect("analytic observable fixture tolerances are valid")
    }

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

    #[test]
    fn observable_error_catches_a_published_q_change_beyond_its_budget() {
        let controller = observable_controller(1.0e3);
        let handle = controller.issue_handle();
        let _window = controller.activate();
        let norm = handle
            .observable_error(0.0, &[1.0], &[5.0e-4])
            .expect("the host projection is finite");
        assert!(norm > 1.0, "q must exceed its combined budget: {norm}");
    }

    #[test]
    fn observable_error_does_not_replace_the_independent_state_criterion() {
        let controller = observable_controller(1.0e-4);
        let handle = controller.issue_handle();
        let _window = controller.activate();
        let norm = handle
            .observable_error(0.0, &[1.0], &[9.0e-3])
            .expect("the host projection is finite");
        assert!(norm < 1.0, "q remains within its combined budget: {norm}");
    }

    #[test]
    fn observable_budget_uses_nominal_scaled_absolute_tolerance_at_zero() {
        let small_nominal = scaled_observable_controller(0.01, 1.0, 0.0, 1.0e-6);
        let large_nominal = scaled_observable_controller(3.0, 1.0, 0.0, 1.0e-6);
        let small_norm = {
            let handle = small_nominal.issue_handle();
            let _window = small_nominal.activate();
            handle
                .observable_error(0.0, &[0.0], &[2.0e-8])
                .expect("the zero-valued small-nominal projection is finite")
        };
        let large_norm = {
            let handle = large_nominal.issue_handle();
            let _window = large_nominal.activate();
            handle
                .observable_error(0.0, &[0.0], &[2.0e-8])
                .expect("the zero-valued large-nominal projection is finite")
        };
        assert!(
            small_norm > 1.0,
            "nominal .01 must budget 1e-8: {small_norm}"
        );
        assert!(large_norm < 1.0, "nominal 3 must budget 3e-6: {large_norm}");
    }

    #[test]
    fn observable_budget_uses_nominal_scaled_absolute_tolerance_at_nonzero_value() {
        let small_nominal = scaled_observable_controller(0.01, 1.0, 1.0e-3, 1.0e-6);
        let large_nominal = scaled_observable_controller(3.0, 1.0, 1.0e-3, 1.0e-6);
        let small_norm = {
            let handle = small_nominal.issue_handle();
            let _window = small_nominal.activate();
            handle
                .observable_error(0.0, &[2.0], &[0.002003])
                .expect("the nonzero small-nominal projection is finite")
        };
        let large_norm = {
            let handle = large_nominal.issue_handle();
            let _window = large_nominal.activate();
            handle
                .observable_error(0.0, &[2.0], &[0.002003])
                .expect("the nonzero large-nominal projection is finite")
        };
        assert!(
            small_norm > 1.0,
            "nominal .01 must retain the tighter budget: {small_norm}"
        );
        assert!(
            large_norm < 1.0,
            "nominal 3 must widen only the absolute term: {large_norm}"
        );
    }

    #[test]
    fn observable_budget_is_invariant_under_consistent_unit_rescaling() {
        let base = scaled_observable_controller(0.01, 1.0, 1.0e-3, 1.0e-6);
        let rescaled = scaled_observable_controller(1.0, 100.0, 1.0e-3, 1.0e-6);
        let base_norm = {
            let handle = base.issue_handle();
            let _window = base.activate();
            handle
                .observable_error(0.0, &[2.0], &[0.002003])
                .expect("the base-unit projection is finite")
        };
        let rescaled_norm = {
            let handle = rescaled.issue_handle();
            let _window = rescaled.activate();
            handle
                .observable_error(0.0, &[2.0], &[0.002003])
                .expect("the rescaled projection is finite")
        };
        assert!((base_norm - rescaled_norm).abs() < 1.0e-12);
    }

    #[test]
    fn observable_budget_clamps_absolute_underflow_and_overflow_like_state_lte() {
        let underflow = scaled_observable_controller(f64::MIN_POSITIVE, 1.0, 0.0, 1.0e-6);
        let underflow_norm = {
            let handle = underflow.issue_handle();
            let _window = underflow.activate();
            handle
                .observable_error(0.0, &[0.0], &[f64::MIN_POSITIVE])
                .expect("absolute underflow is clamped to the smallest positive value")
        };
        assert_eq!(underflow_norm, 1.0);

        let overflow = scaled_observable_controller(f64::MAX, 1.0, 0.0, 2.0);
        let overflow_norm = {
            let handle = overflow.issue_handle();
            let _window = overflow.activate();
            handle
                .observable_error(0.0, &[0.0], &[f64::MAX])
                .expect("absolute overflow is clamped to the largest finite value")
        };
        assert_eq!(overflow_norm, 1.0);
    }

    #[test]
    fn observable_error_uses_the_same_activation_and_failure_latch() {
        let controller = observable_controller(1.0);
        let handle = controller.issue_handle();
        assert!(matches!(
            handle.observable_error(0.0, &[1.0], &[0.0]),
            Err(MeDerivativeRefused)
        ));
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::DerivativeCapabilityInactive { .. })
        ));
    }

    #[test]
    fn a_hostile_plugin_cannot_hide_an_observable_projection_panic() {
        let controller = MeDerivativeController::over_observable_closure(
            1,
            Rc::new(|_time, _states| vec![0.0]),
            Rc::new(|_time, _states| -> Vec<f64> { panic!("injected component projection panic") }),
            1,
            1.0e-3,
            1.0e-6,
        )
        .expect("hostile projection fixture tolerances are valid");
        let handle = controller.issue_handle();
        let _window = controller.activate();
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            handle.observable_error(0.0, &[1.0], &[0.0])
        }));
        assert!(caught.is_ok(), "the handle must convert component panic");
        assert!(matches!(caught.unwrap(), Err(MeDerivativeRefused)));
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::Contract { .. })
        ));
    }

    #[test]
    fn observable_error_is_zero_for_an_empty_published_inventory() {
        let controller = MeDerivativeController::over_observable_closure(
            1,
            Rc::new(|_time, _states| vec![0.0]),
            Rc::new(|_time, _states| Vec::new()),
            0,
            1.0e-3,
            1.0e-6,
        )
        .expect("an empty observable inventory is a valid supplemental capability");
        let handle = controller.issue_handle();
        let _window = controller.activate();
        assert_eq!(
            handle
                .observable_error(0.0, &[1.0], &[1.0])
                .expect("no supplemental channels means zero error"),
            0.0
        );
    }

    #[test]
    fn observable_error_refuses_an_explicit_invalid_nominal() {
        let controller = MeDerivativeController::over_component_with_tolerances(
            Box::new(ClosureDerivatives {
                derivative: Rc::new(|_time, _states| vec![0.0]),
                state_count: 1,
                observable: Some(Rc::new(|_time, _states| vec![1.0])),
                observable_channel_count: 1,
                observable_nominals: vec![0.0],
            }),
            1.0e-3,
            1.0e-6,
        )
        .expect("the capability tolerances are valid");
        let handle = controller.issue_handle();
        let _window = controller.activate();
        assert!(handle.observable_error(0.0, &[1.0], &[0.0]).is_err());
        assert!(matches!(
            controller.take_error(),
            Some(MeIntegrationError::Contract { .. })
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
