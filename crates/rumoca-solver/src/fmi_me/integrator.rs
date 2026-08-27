//! The checked numerical-plugin contract (SPEC_0044 §6, ME-INT-001/002).
//!
//! A numerical plugin owns exactly one thing: advancing the continuous state by
//! one accepted internal step and sampling that step's native continuous
//! extension. Everything else — FMI lifecycle transitions, Event Mode, root
//! meaning, output scheduling, trace roles, component policy — belongs to
//! [`super::session::MeSimulationSession`].
//!
//! The aggregates below have private fields and checked constructors, so an
//! invalid boundary value can never enter the master algorithm.
//!
//! # Provenance
//!
//! A plugin never returns, and never can name, anything that claims host
//! provenance. Its advance returns a [`MeStepCandidate`]: raw numbers, with no
//! request, no previous point, no component width, and no checked coordinate.
//! The host lends the actual [`MeAdvanceRequest`] for that one call and then
//! **consumes** it into the host-private `MeStepProposal` while checking the
//! candidate against it, so the proposal holds the request's own coordinates by
//! construction and the accepted proof holds the proposal. Forging a foreign
//! request, replaying an earlier one, crossing a bound, and asserting a
//! component width are therefore all unrepresentable rather than merely
//! rejected, and no token, identifier, or second correlation fact exists to be
//! guessed or compared.

use super::MeError;

/// The unrelated-solver conformance fixture required by ME-INT-001. It is
/// compiled only under test, so it can never become a production path.
#[cfg(test)]
pub(super) mod conformance;
/// The retained opaque derivative handle and its host-private activation
/// controller. SPEC_0044 §6's ruling keeps the concrete component source and
/// the handle constructor inside the common host.
mod derivative;
pub mod time_only;

pub(super) use derivative::MeDerivativeController;
pub use derivative::{MeDerivativeHandle, MeDerivativeRefused};

#[cfg(test)]
pub(super) use derivative::{DerivativeClosure, detached_handle};

/// Solver-neutral roundoff tolerance for one accepted interval.
///
/// SPEC_0044 §6 fixes this formula so no plugin supplies either side of an
/// accuracy comparison: it depends only on the host-issued current coordinate
/// and the accepted interval duration.
#[must_use]
pub fn accepted_step_roundoff(current_time: f64, accepted_interval_duration: f64) -> f64 {
    (100.0 * f64::EPSILON * (current_time.abs() + accepted_interval_duration.abs()))
        .max(f64::MIN_POSITIVE)
}

/// Whether a finite coordinate lies in a closed accepted interval under the
/// host's solver-neutral roundoff policy.
///
/// This is the single containment rule imported by every numerical plugin. It
/// is deliberately independent of state width and solver representation: the
/// checked host interval supplies both endpoints, and the plugin supplies
/// neither side of the comparison.
#[must_use]
pub fn accepted_interval_contains(start: f64, end: f64, time: f64) -> bool {
    if !start.is_finite() || !end.is_finite() || !time.is_finite() || end < start {
        return false;
    }
    let roundoff = accepted_step_roundoff(start, end - start);
    time >= start - roundoff && time <= end + roundoff
}

/// The kind of numerical failure a plugin's own library reported.
///
/// SPEC_0044 §6 ME-INT-003: a backend's failure
/// identity is typed data. Rendered diagnostic text may accompany a category,
/// but the category — not the prose — is what a host, a worker bucket, or a
/// census switches on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeNumericalFailure {
    /// The plugin could not build its own problem or initial history.
    Construction,
    /// The plugin's advance exhausted its library's step budget or its
    /// nonlinear solve failed to converge.
    AdvanceExhausted,
    /// The plugin's native continuous extension could not produce a sample.
    Interpolation,
    /// Truncate/reset could not re-establish the plugin's history.
    Reset,
    /// The plugin's linear algebra failed at a point the component accepted.
    LinearAlgebra,
}

impl MeNumericalFailure {
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::Construction => "problem construction",
            Self::AdvanceExhausted => "advance exhaustion",
            Self::Interpolation => "continuous-extension interpolation",
            Self::Reset => "history reset",
            Self::LinearAlgebra => "linear algebra",
        }
    }
}

/// Typed failures a numerical plugin may report.
///
/// SPEC_0044 §6 ME-INT-003: component, integrator, timeout, allocation, and
/// discard failures stay typed. No variant flattens a component status or a
/// library's exhaustion into a rendered string; [`Self::Numerical`] carries the
/// category as data and the library's own text only as optional detail.
#[derive(Debug, thiserror::Error)]
pub enum MeIntegrationError {
    /// A component evaluation the plugin requested failed. The component's own
    /// typed failure travels unchanged.
    #[error(transparent)]
    Component(#[from] MeError),

    /// `fmi3Discard`: the component refused the evaluation the plugin's trial
    /// needed, and the plugin cannot prove the trial left no committed state.
    #[error("component discarded the evaluation at t={time}")]
    ComponentDiscard { time: f64 },

    /// The plugin's own step controller could not make progress.
    #[error("{method} step size underflowed while advancing from t={from_time} toward t={to_time}")]
    StepSizeUnderflow {
        method: &'static str,
        from_time: f64,
        to_time: f64,
    },

    /// The plugin's numerical library failed in a typed category.
    #[error("{method} {} failed{}", category.label(), render_detail(detail))]
    Numerical {
        method: &'static str,
        category: MeNumericalFailure,
        detail: String,
    },

    /// A checked boundary aggregate was constructed with an invalid value.
    #[error("{reason}")]
    Contract { reason: String },

    /// The plugin requested a component derivative while the host had the
    /// retained capability deactivated.
    ///
    /// SPEC_0044 §6's ruling makes retention strictly weaker than reachability:
    /// the window is open only inside `initialize`, exactly one `advance`, and
    /// `truncate_reset`. `sample` and every coordinate outside a host call are
    /// closed, and misuse is this typed contract failure rather than a stale
    /// evaluation at whatever coordinate the component happened to hold.
    #[error(
        "the retained derivative capability is inactive: {operation} is legal only inside \
         initialize, one advance, or truncate/reset"
    )]
    DerivativeCapabilityInactive { operation: &'static str },

    /// A backend propagated the opaque refusal of a retained-handle
    /// evaluation.
    ///
    /// It carries no cause on purpose: the typed one is latched host-side and
    /// the host substitutes it before this variant can reach a caller
    #[error("a retained derivative evaluation was refused; the host owns the typed cause")]
    DerivativeRefused,

    /// A plugin or host buffer could not be reserved.
    #[error("{context} allocation failed for {entries} entries")]
    Allocation {
        context: &'static str,
        entries: usize,
    },
}

fn render_detail(detail: &str) -> String {
    if detail.is_empty() {
        String::new()
    } else {
        format!(": {detail}")
    }
}

impl MeIntegrationError {
    pub(crate) fn contract(reason: impl Into<String>) -> Self {
        Self::Contract {
            reason: reason.into(),
        }
    }

    /// Mint a typed library failure without letting its identity live only in
    /// rendered prose.
    #[must_use]
    pub fn numerical(
        method: &'static str,
        category: MeNumericalFailure,
        detail: impl Into<String>,
    ) -> Self {
        Self::Numerical {
            method,
            category,
            detail: detail.into(),
        }
    }
}

/// The checked, host-issued numerical configuration a plugin is built with.
///
/// SPEC_0044 §6 ME-INT-001 limits a plugin's component capability to the
/// derivative source and forbids a plugin
/// from reading component nominal policy or the public [`crate::SimOptions`]
/// (which carries output cadence, experiment coordinates, timeout, pacing, and
/// solver selection). Everything numerically relevant a solver legitimately
/// needs is validated once, here, by the host.
#[derive(Debug, Clone)]
pub struct MeNumericalSetup {
    relative_tolerance: f64,
    absolute_tolerance: f64,
    state_nominals: Vec<f64>,
    initial_step_hint: Option<f64>,
}

impl MeNumericalSetup {
    /// Construct the configuration, proving every value the plugin may scale
    /// with. The nominal vector is the complete component width the host
    /// already validated for its own root policy; there is no fallback entry.
    pub fn new(
        relative_tolerance: f64,
        absolute_tolerance: f64,
        state_nominals: Vec<f64>,
        state_count: usize,
        initial_step_hint: Option<f64>,
    ) -> Result<Self, MeIntegrationError> {
        for (label, value) in [
            ("relative tolerance", relative_tolerance),
            ("absolute tolerance", absolute_tolerance),
        ] {
            if !value.is_finite() || value <= 0.0 {
                return Err(MeIntegrationError::contract(format!(
                    "numerical {label} must be finite and positive, got {value}"
                )));
            }
        }
        if state_nominals.len() != state_count {
            return Err(MeIntegrationError::contract(format!(
                "numerical setup carries {} nominals for a component of width {state_count}",
                state_nominals.len()
            )));
        }
        if let Some(index) = state_nominals
            .iter()
            .position(|nominal| !nominal.is_finite() || *nominal <= 0.0)
        {
            return Err(MeIntegrationError::contract(format!(
                "continuous-state nominal {index} is not finite and positive"
            )));
        }
        if let Some(hint) = initial_step_hint
            && (!hint.is_finite() || hint <= 0.0)
        {
            return Err(MeIntegrationError::contract(format!(
                "initial step hint {hint} must be finite and positive"
            )));
        }
        Ok(Self {
            relative_tolerance,
            absolute_tolerance,
            state_nominals,
            initial_step_hint,
        })
    }

    #[must_use]
    pub fn relative_tolerance(&self) -> f64 {
        self.relative_tolerance
    }

    #[must_use]
    pub fn absolute_tolerance(&self) -> f64 {
        self.absolute_tolerance
    }

    /// The complete positive finite nominal vector, host-validated.
    #[must_use]
    pub fn state_nominals(&self) -> &[f64] {
        &self.state_nominals
    }

    /// A solver-neutral first-step magnitude. It is not the trace schedule and
    /// carries no experiment coordinate.
    #[must_use]
    pub fn initial_step_hint(&self) -> Option<f64> {
        self.initial_step_hint
    }
}

/// The latest coordinate one accepted step may reach.
///
/// Owned once, here, so the session and the checked request cannot disagree
/// about which bound is binding: the session uses it to decide whether a soft
/// observation is reachable, and `MeAdvanceRequest::new` uses it to prove it.
#[must_use]
pub fn reachable_bound(
    current_time: f64,
    hard_stop_time: Option<f64>,
    yield_time: f64,
    max_step_duration: Option<f64>,
) -> f64 {
    let mut latest = yield_time;
    if let Some(stop) = hard_stop_time {
        latest = latest.min(stop);
    }
    if let Some(duration) = max_step_duration {
        latest = latest.min(current_time + duration);
    }
    latest
}

/// Canonicalize a coordinate so numerically equal times are bitwise equal.
///
/// `-0.0 == 0.0` numerically but has different bits, and every same-coordinate
/// rule in SPEC_0050 and SPEC_0044 §6 is a bitwise rule. Mapping negative zero
/// onto positive zero at checked construction is what keeps "one FMI time" and
/// "one coordinate" the same statement.
#[must_use]
pub fn canonical_coordinate(time: f64) -> f64 {
    if time == 0.0 { 0.0 } else { time }
}

/// A checked continuous coordinate: finite time, finite states, exact
/// component width.
///
/// Only the host constructs one. `state_count` is the linked component's own
/// continuous-state width, so no caller can assert an arity the component does
/// not have; a plugin reads points the host issues and
/// returns raw [`MeStepCandidate`] numbers instead of minting them.
///
/// An external crate cannot build one at all:
///
/// ```compile_fail
/// let _ = rumoca_solver::fmi_me::MeContinuousPoint::new(0.0, vec![1.0], 1);
/// ```
///
/// `Clone` remains, and grants nothing: no host operation consumes a
/// caller-supplied point, so duplicating one cannot carry it back across the
/// boundary.
#[derive(Debug, Clone, PartialEq)]
pub struct MeContinuousPoint {
    time: f64,
    states: Vec<f64>,
}

impl MeContinuousPoint {
    /// Construct a point of exactly `state_count` finite values at a finite
    /// time, where `state_count` is the linked component's width.
    pub(super) fn new(
        time: f64,
        states: Vec<f64>,
        state_count: usize,
    ) -> Result<Self, MeIntegrationError> {
        if !time.is_finite() {
            return Err(MeIntegrationError::contract(format!(
                "continuous point time {time} must be finite"
            )));
        }
        if states.len() != state_count {
            return Err(MeIntegrationError::contract(format!(
                "continuous point carries {} states for a component of width {state_count}",
                states.len()
            )));
        }
        if let Some(index) = states.iter().position(|value| !value.is_finite()) {
            return Err(MeIntegrationError::contract(format!(
                "continuous point state {index} is not finite"
            )));
        }
        Ok(Self {
            time: canonical_coordinate(time),
            states,
        })
    }

    #[must_use]
    pub fn time(&self) -> f64 {
        self.time
    }

    #[must_use]
    pub fn states(&self) -> &[f64] {
        &self.states
    }

    #[must_use]
    pub fn width(&self) -> usize {
        self.states.len()
    }

    #[must_use]
    pub fn into_states(self) -> Vec<f64> {
        self.states
    }
}

/// One host-issued request for exactly one accepted internal step.
///
/// SPEC_0044 §6 distinguishes three coordinates. Every constructed non-soft
/// bound is strictly later than the current coordinate beyond roundoff, because
/// the session resolves a coincident bound without a numerical advance before
/// it builds a request. A soft observation is likewise strictly later than the
/// current coordinate — a current observation is materialized before request
/// construction — and never lies beyond the request's own reachable interval.
///
/// It is host-issued in the strict sense: the constructor is private, the type
/// is **not** `Clone`, and a plugin only ever sees one behind a borrow that
/// expires with its `advance` call. So a plugin can neither build a request,
/// retain one, nor return one.
///
/// ```compile_fail
/// let _ = rumoca_solver::fmi_me::MeAdvanceRequest::new;
/// ```
#[derive(Debug)]
pub struct MeAdvanceRequest {
    current: MeContinuousPoint,
    hard_stop_time: Option<f64>,
    yield_time: f64,
    observation_time: Option<f64>,
    max_step_duration: Option<f64>,
    latest_accepted_time: f64,
}

impl MeAdvanceRequest {
    /// Construct the request, rejecting every coordinate the master algorithm
    /// must have resolved before asking for a numerical advance.
    pub(super) fn new(
        current: MeContinuousPoint,
        hard_stop_time: Option<f64>,
        yield_time: f64,
        observation_time: Option<f64>,
        max_step_duration: Option<f64>,
    ) -> Result<Self, MeIntegrationError> {
        let now = current.time();
        let roundoff = accepted_step_roundoff(now, 0.0);
        if let Some(stop) = hard_stop_time {
            require_later_bound("hard stop", stop, now, roundoff)?;
        }
        require_later_bound("yield", yield_time, now, roundoff)?;
        if let Some(duration) = max_step_duration
            && (!duration.is_finite() || duration <= 0.0)
        {
            return Err(MeIntegrationError::contract(format!(
                "maximum step duration {duration} must be finite and positive"
            )));
        }

        let latest = reachable_bound(now, hard_stop_time, yield_time, max_step_duration);
        if !latest.is_finite() || latest - now <= roundoff {
            return Err(MeIntegrationError::contract(format!(
                "the request's reachable bound {latest} is not strictly later than t={now}"
            )));
        }

        // A current observation is materialized before
        // request construction, and the checked aggregate — not the plugin —
        // owns the proof that a soft observation is ordered inside the interval
        // this request can actually reach.
        if let Some(observation) = observation_time {
            if !observation.is_finite() {
                return Err(MeIntegrationError::contract(format!(
                    "soft observation {observation} must be finite"
                )));
            }
            if observation - now <= roundoff {
                return Err(MeIntegrationError::contract(format!(
                    "soft observation {observation} is not strictly later than t={now}; the \
                     session materializes a current observation without a numerical advance"
                )));
            }
            if observation - latest > roundoff {
                return Err(MeIntegrationError::contract(format!(
                    "soft observation {observation} lies beyond the request's reachable bound \
                     {latest}"
                )));
            }
        }

        Ok(Self {
            current,
            hard_stop_time,
            yield_time,
            observation_time,
            max_step_duration,
            latest_accepted_time: latest,
        })
    }

    #[must_use]
    pub fn current(&self) -> &MeContinuousPoint {
        &self.current
    }

    #[must_use]
    pub fn hard_stop_time(&self) -> Option<f64> {
        self.hard_stop_time
    }

    #[must_use]
    pub fn yield_time(&self) -> f64 {
        self.yield_time
    }

    #[must_use]
    pub fn observation_time(&self) -> Option<f64> {
        self.observation_time
    }

    /// The maximum accepted-interval duration the component's current delay
    /// expressions allow, when one is currently binding.
    #[must_use]
    pub fn max_step_duration(&self) -> Option<f64> {
        self.max_step_duration
    }

    /// The latest coordinate an accepted step may reach: the earliest of the
    /// hard stop, the yield boundary, and the maximum-duration bound.
    #[must_use]
    pub fn latest_accepted_time(&self) -> f64 {
        self.latest_accepted_time
    }

    /// The **least** host-issued public coordinate within `roundoff` of
    /// `accepted_time`, if any.
    ///
    /// Selecting in insertion order lets a hard stop
    /// and a yield that are distinct but within roundoff snap to the later
    /// coordinate. The binding coordinate is always the earliest one the step
    /// could legally have reached. Three fixed comparisons, no allocation and
    /// no sort, because this runs once per accepted step.
    fn binding_public_coordinate(&self, accepted_time: f64, roundoff: f64) -> Option<f64> {
        let mut binding: Option<f64> = None;
        let mut consider = |candidate: f64| {
            if (accepted_time - candidate).abs() <= roundoff
                && binding.is_none_or(|best| candidate < best)
            {
                binding = Some(candidate);
            }
        };
        if let Some(stop) = self.hard_stop_time {
            consider(stop);
        }
        consider(self.yield_time);
        if let Some(duration) = self.max_step_duration {
            consider(self.current.time() + duration);
        }
        binding
    }
}

fn require_later_bound(
    label: &str,
    bound: f64,
    now: f64,
    roundoff: f64,
) -> Result<(), MeIntegrationError> {
    if !bound.is_finite() {
        return Err(MeIntegrationError::contract(format!(
            "{label} bound {bound} must be finite"
        )));
    }
    if bound - now <= roundoff {
        return Err(MeIntegrationError::contract(format!(
            "{label} bound {bound} must be strictly later than t={now} beyond roundoff"
        )));
    }
    Ok(())
}

/// The raw, explicitly **unproved** numerical result of one advance.
///
/// This is the entire return value of the plugin boundary: an endpoint
/// coordinate, that endpoint's continuous states, and the declared local
/// accuracy order of the plugin's own native continuous extension over the step
/// it just took. It names no request, carries no previous point, asserts no
/// component width, and canonicalizes nothing, so it makes no claim the host
/// would otherwise have to disbelieve. Constructing one is therefore
/// unrestricted, and useless as an attack: the host validates every field
/// against the actual request it retained before any of it becomes a checked
/// coordinate.
///
/// It is the one aggregate an external plugin may build:
///
/// ```
/// use rumoca_solver::fmi_me::MeStepCandidate;
///
/// let candidate = MeStepCandidate::new(0.25, vec![1.5], 4);
/// assert_eq!(candidate.accepted_time(), 0.25);
/// ```
#[derive(Debug)]
pub struct MeStepCandidate {
    accepted_time: f64,
    accepted_states: Vec<f64>,
    order: u32,
}

impl MeStepCandidate {
    /// Report the endpoint one accepted internal step reached, that endpoint's
    /// continuous states, and the declared local order of the native continuous
    /// extension covering the step.
    #[must_use]
    pub fn new(accepted_time: f64, accepted_states: Vec<f64>, order: u32) -> Self {
        Self {
            accepted_time,
            accepted_states,
            order,
        }
    }

    /// The reported endpoint coordinate, still unproved.
    #[must_use]
    pub fn accepted_time(&self) -> f64 {
        self.accepted_time
    }

    /// The reported endpoint states, still unproved.
    #[must_use]
    pub fn accepted_states(&self) -> &[f64] {
        &self.accepted_states
    }
}

/// One numerical step, holding the very request that produced it.
///
/// This is deliberately **not** the accepted step and carries no proof name: it
/// proves only the candidate-side obligations: the request's own start point,
/// the linked component's state arity, finite endpoints, forward progress
/// beyond roundoff, a positive declared local continuous-extension order, and
/// the stop/yield/maximum-duration bounds under the host roundoff policy.
///
/// Provenance here is *containment*, not comparison. [`Self::bind`] consumes
/// the request by value, so the proposal holds the actual host-issued
/// coordinates the plugin was serving and there is no correlation token,
/// identifier, or pointer identity anywhere to be forged, guessed, or checked
///
/// It is host-private and has no public constructor at all: a plugin cannot
/// mint one, name one, or receive one.
///
/// ```compile_fail
/// let _: Option<rumoca_solver::fmi_me::MeStepProposal> = None;
/// ```
///
/// One SPEC_0044 §6 obligation remains, and cannot be proved from the candidate
/// at all: that the plugin's native continuous extension really covers this
/// interval. The host proves it and mints the sole checked
/// [`MeAcceptedStep`]. There is therefore exactly one type named as the proof,
/// and it is complete.
#[derive(Debug)]
pub(super) struct MeStepProposal {
    request: MeAdvanceRequest,
    accepted: MeContinuousPoint,
    order: u32,
}

impl MeStepProposal {
    /// Consume the request the host is actually serving, checking an unproved
    /// candidate against it, and prove every candidate-side §6 obligation
    /// listed on the type.
    ///
    /// Taking `request` **by value** is the construction proof: the session
    /// owns exactly one request per accepted step, cannot clone it, and gives
    /// it up here, so a proposal that exists at all was built from the request
    /// its own coordinates came from. Nothing needs to be compared afterwards.
    ///
    /// `component_state_count` is the linked component's own continuous-state
    /// width, supplied by the session; the candidate has no say in it. The start
    /// point is the request's current point, never the candidate's, so a
    /// candidate computed against a stale or invented interval cannot smuggle
    /// its own origin in: it can only fail the progress and bound checks
    /// against the actual current coordinate.
    ///
    /// A public endpoint within roundoff of a host-issued coordinate is
    /// normalized onto the least such coordinate, the semantically binding one;
    /// this is not a truncation and does not reset the backend.
    pub(super) fn bind(
        request: MeAdvanceRequest,
        candidate: MeStepCandidate,
        component_state_count: usize,
    ) -> Result<Self, MeIntegrationError> {
        let MeStepCandidate {
            accepted_time,
            accepted_states,
            order,
        } = candidate;
        if order == 0 {
            return Err(MeIntegrationError::contract(
                "an accepted step must declare a positive local continuous-extension order",
            ));
        }
        let previous_width = request.current().width();
        if previous_width != component_state_count {
            return Err(MeIntegrationError::contract(format!(
                "the host issued a request of width {previous_width} for a component of width \
                 {component_state_count}"
            )));
        }
        if accepted_states.len() != component_state_count {
            return Err(MeIntegrationError::contract(format!(
                "the numerical candidate reports {} states for a component of width \
                 {component_state_count}",
                accepted_states.len()
            )));
        }
        // Finiteness, arity, and the negative-zero coordinate rule are proved
        // once, here, by the same checked constructor the host uses everywhere
        // else. Until this succeeds the candidate holds no checked coordinate.
        let accepted =
            MeContinuousPoint::new(accepted_time, accepted_states, component_state_count)?;
        let previous_time = request.current().time();
        let duration = accepted.time() - previous_time;
        let roundoff = accepted_step_roundoff(previous_time, duration);
        if duration <= roundoff {
            return Err(MeIntegrationError::contract(format!(
                "accepted step from t={previous_time} to t={} made no progress beyond roundoff",
                accepted.time()
            )));
        }
        let accepted = normalize_endpoint(accepted, &request, roundoff)?;
        let latest = request.latest_accepted_time();
        if accepted.time() - latest > roundoff {
            return Err(MeIntegrationError::contract(format!(
                "accepted step reached t={} past the host bound t={latest}",
                accepted.time()
            )));
        }
        Ok(Self {
            request,
            accepted,
            order,
        })
    }

    /// The consumed request's own current point: the coordinate this step
    /// started from, by construction rather than by assertion.
    #[must_use]
    pub(super) fn previous(&self) -> &MeContinuousPoint {
        self.request.current()
    }

    #[must_use]
    pub(super) fn accepted(&self) -> &MeContinuousPoint {
        &self.accepted
    }

    /// The plugin's declared local accuracy order for this step's native
    /// continuous extension.
    #[must_use]
    pub(super) fn order(&self) -> u32 {
        self.order
    }
}

/// The sole checked accepted step (SPEC_0044 §6 aggregate table).
///
/// Its construction contract is complete: every candidate-side obligation the
/// host-private `MeStepProposal` proved, **plus** complete-interval sampling.
/// It exists only after the host has sampled the plugin's native continuous
/// extension at both endpoints and required componentwise agreement with the
/// checked points under the session's own state-consistency bound, and it is
/// the only value root scanning and endpoint commitment consume. A plugin
/// cannot mint one at all:
///
/// ```compile_fail
/// let _ = rumoca_solver::fmi_me::MeAcceptedStep::from_validated_proposal;
/// ```
///
/// It holds the proposal, which holds the consumed request, so the host's
/// correlation to the one request it issued survives acceptance by containment
#[derive(Debug)]
pub struct MeAcceptedStep {
    proposal: MeStepProposal,
    left_states: Vec<f64>,
    right_states: Vec<f64>,
}

impl MeAcceptedStep {
    /// Issue the completed proof. The only caller is
    /// [`super::root::accept_step`], which runs the endpoint validation
    /// immediately before.
    pub(super) fn from_validated_proposal(
        proposal: MeStepProposal,
        left_states: Vec<f64>,
        right_states: Vec<f64>,
    ) -> Self {
        Self {
            proposal,
            left_states,
            right_states,
        }
    }

    #[must_use]
    pub fn previous(&self) -> &MeContinuousPoint {
        self.proposal.previous()
    }

    #[must_use]
    pub fn accepted(&self) -> &MeContinuousPoint {
        self.proposal.accepted()
    }

    /// The plugin's declared local accuracy order for this step's native
    /// continuous extension.
    #[must_use]
    pub fn order(&self) -> u32 {
        self.proposal.order()
    }

    /// The validated endpoint samples, reused instead of re-sampling the
    /// plugin at coordinates the host has already proved.
    pub(super) fn left_states(&self) -> &[f64] {
        &self.left_states
    }

    pub(super) fn right_states(&self) -> &[f64] {
        &self.right_states
    }
}

/// Snap a public endpoint that matches a host-issued coordinate within roundoff
/// onto the **least** such coordinate.
///
/// Selecting in insertion order lets a hard stop and a
/// yield that are distinct but within roundoff snap to the later coordinate.
/// The binding coordinate is always the earliest one the step could legally
/// have reached, so canonicalization takes the least match.
fn normalize_endpoint(
    accepted: MeContinuousPoint,
    request: &MeAdvanceRequest,
    roundoff: f64,
) -> Result<MeContinuousPoint, MeIntegrationError> {
    let width = accepted.width();
    let Some(coordinate) = request.binding_public_coordinate(accepted.time(), roundoff) else {
        return Ok(accepted);
    };
    if coordinate.to_bits() == accepted.time().to_bits() {
        return Ok(accepted);
    }
    MeContinuousPoint::new(coordinate, accepted.into_states(), width)
}

/// The one numerical capability a concrete solver supplies.
///
/// SPEC_0044 §6 ME-INT-001's exact thin surface, with the retained-handle
/// ruling applied, and nothing else: initialize, one accepted numerical step
/// over the host-provided derivative callback, that step's native continuous
/// extension plus its declared positive local order, and generic
/// truncate/reset. Four operations, no fifth. In particular there is no
/// backend-identity operation: a solver family label is not one of the
/// capabilities ME-INT-001 admits, and a diagnostic that needed one would put
/// solver identity back into the common trait.
///
/// Implementors receive no event-indicator callback, no lifecycle transition,
/// no output schedule, and no trace. They see only the host-issued coordinates
/// in [`MeAdvanceRequest`], the checked [`MeNumericalSetup`] they were built
/// with, and the opaque [`MeDerivativeHandle`] the host issues at `initialize`.
///
/// The handle is *retained* rather than lent, so a persistent numerical problem
/// can own it across accepted steps. Reachability is governed by the host's
/// private activation controller instead: the window is open for exactly this
/// trait's `initialize`, `advance`, and `truncate_reset` calls, and closed
/// during `sample` and between calls.
///
/// Solver-specific stepper state, stage values, BDF history, and dense-output
/// coefficients stay private to the implementor: ME-INT-002 keeps them out of
/// every type on this boundary, and the only numbers that cross are the
/// candidate endpoint and the fixed-width sampler writes.
pub trait MeIntegratorBackend {
    /// (Re)initialize the plugin's own history at a checked point.
    ///
    /// `derivatives` is the plugin's retained capability from here on: a
    /// persistent problem may store it. The host issues exactly one handle per
    /// call and drops the plugin's previous one.
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError>;

    /// Advance by exactly one accepted internal step from
    /// `request.current()`.
    ///
    /// `request` is borrowed for exactly this call: it is the actual request
    /// the host is serving, it cannot be cloned, and it cannot outlive the
    /// call.
    ///
    /// The plugin evaluates through its retained handle, which the host has
    /// activated for the duration of this one call.
    ///
    /// The plugin returns an explicitly unproved [`MeStepCandidate`]: raw
    /// numbers, never a proof and never anything naming a request. The host
    /// immediately binds that candidate to the request above and validates it
    /// there; only the host mints the checked [`MeAcceptedStep`]
    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError>;

    /// Evaluate the most recently accepted step's native continuous extension.
    ///
    /// `time` always lies inside the closed accepted interval, and `states` is
    /// exactly the component's continuous-state width. The plugin writes that
    /// fixed-width slice **completely** with finite values and can therefore
    /// never change the checked state shape. The host
    /// poisons the slice before every call and rejects a successful partial or
    /// non-finite write. The plugin never learns why the host wants the sample,
    /// and its retained handle is deactivated for the whole call: a native
    /// continuous extension reads stored stage values, it does not evaluate
    /// the component.
    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError>;

    /// Discard the uncompleted trial and restart the plugin's history at
    /// `point`, which the host located inside the last accepted interval.
    ///
    /// The retained handle is activated for the duration of this one call.
    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError>;
}

/// Invoke one continuous-extension sample and prove a complete finite write.
///
/// This is host/conformance authority rather than a plugin method: poisoning
/// the fixed-width slice first distinguishes a real finite zero from an entry
/// the plugin omitted or left stale. The common session and the solver-neutral
/// admission suite share this one boundary rule.
pub(in crate::fmi_me) fn sample_complete(
    backend: &dyn MeIntegratorBackend,
    time: f64,
    states: &mut [f64],
) -> Result<(), MeIntegrationError> {
    states.fill(f64::NAN);
    backend.sample(time, states)?;
    let Some(index) = states.iter().position(|value| !value.is_finite()) else {
        return Ok(());
    };
    Err(MeIntegrationError::contract(format!(
        "the continuous extension did not write a finite state at index {index} for t={time}"
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn point(time: f64, states: &[f64]) -> MeContinuousPoint {
        MeContinuousPoint::new(time, states.to_vec(), states.len())
            .expect("fixture point is checked")
    }

    fn request(now: f64, yield_time: f64) -> MeAdvanceRequest {
        MeAdvanceRequest::new(point(now, &[1.0]), None, yield_time, None, None)
            .expect("fixture request is checked")
    }

    /// Bind a candidate exactly as the session does: consuming the actual
    /// request, at the linked component's width.
    fn bind(
        request: MeAdvanceRequest,
        time: f64,
        states: &[f64],
        order: u32,
    ) -> Result<MeStepProposal, MeIntegrationError> {
        let width = request.current().width();
        MeStepProposal::bind(
            request,
            MeStepCandidate::new(time, states.to_vec(), order),
            width,
        )
    }

    #[test]
    fn a_continuous_point_rejects_non_finite_values_and_wrong_widths() {
        assert!(MeContinuousPoint::new(f64::NAN, vec![0.0], 1).is_err());
        assert!(MeContinuousPoint::new(0.0, vec![f64::INFINITY], 1).is_err());
        assert!(MeContinuousPoint::new(0.0, vec![0.0, 1.0], 1).is_err());
        assert!(MeContinuousPoint::new(0.0, vec![0.0], 1).is_ok());
    }

    #[test]
    fn a_request_rejects_a_bound_the_session_should_have_resolved_itself() {
        let now = point(1.0, &[0.0]);
        assert!(MeAdvanceRequest::new(now.clone(), None, 1.0, None, None).is_err());
        assert!(MeAdvanceRequest::new(now.clone(), Some(1.0), 2.0, None, None).is_err());
        assert!(MeAdvanceRequest::new(now.clone(), None, 2.0, None, Some(0.0)).is_err());
        assert!(MeAdvanceRequest::new(now, None, 2.0, Some(1.5), Some(0.25)).is_err());
    }

    #[test]
    fn a_soft_observation_must_be_ordered_inside_the_reachable_interval() {
        let now = point(1.0, &[0.0]);
        // At the current coordinate: the session materializes it directly.
        assert!(MeAdvanceRequest::new(now.clone(), None, 2.0, Some(1.0), None).is_err());
        // Behind the current coordinate.
        assert!(MeAdvanceRequest::new(now.clone(), None, 2.0, Some(0.5), None).is_err());
        // Beyond every reachable bound.
        assert!(MeAdvanceRequest::new(now.clone(), None, 2.0, Some(3.0), None).is_err());
        // Inside the interval, and exactly on the reachable bound.
        assert!(MeAdvanceRequest::new(now.clone(), None, 2.0, Some(1.5), None).is_ok());
        assert!(MeAdvanceRequest::new(now, None, 2.0, Some(2.0), None).is_ok());
    }

    #[test]
    fn an_accepted_step_requires_progress_and_a_positive_declared_order() {
        assert!(bind(request(0.0, 1.0), 0.0, &[1.0], 5).is_err());
        assert!(bind(request(0.0, 1.0), 0.5, &[1.0], 0).is_err());
        assert!(bind(request(0.0, 1.0), 0.5, &[1.0], 5).is_ok());
    }

    #[test]
    fn an_accepted_step_may_not_cross_a_host_bound_beyond_roundoff() {
        assert!(bind(request(0.0, 1.0), 1.5, &[1.0], 5).is_err());
    }

    #[test]
    fn an_endpoint_within_roundoff_normalizes_onto_the_host_coordinate() {
        let drifted = 1.0_f64.next_up();
        let step = bind(request(0.0, 1.0), drifted, &[1.0], 5)
            .expect("a one-ulp overshoot of the yield boundary is within roundoff");
        assert_eq!(step.accepted().time().to_bits(), 1.0_f64.to_bits());
    }

    #[test]
    fn canonicalization_selects_the_least_coordinate_within_roundoff() {
        // A hard stop and a yield that differ by less than the roundoff
        // tolerance: the binding coordinate is the earlier hard stop, whatever
        // order the request stored them in.
        let now = point(0.0, &[1.0]);
        let stop = 1.0_f64;
        let yielded = stop.next_up().next_up();
        let request = MeAdvanceRequest::new(now, Some(stop), yielded, None, None)
            .expect("both bounds are strictly later than t=0");
        let step = bind(request, yielded, &[1.0], 3)
            .expect("the endpoint matches both coordinates within roundoff");
        assert_eq!(step.accepted().time().to_bits(), stop.to_bits());
    }

    #[test]
    fn the_maximum_step_duration_bounds_the_accepted_interval() {
        let bounded = || {
            MeAdvanceRequest::new(point(0.0, &[1.0]), None, 10.0, None, Some(0.25))
                .expect("bounded request is checked")
        };
        assert!((bounded().latest_accepted_time() - 0.25).abs() <= f64::EPSILON);
        assert!(bind(bounded(), 0.5, &[1.0], 4).is_err());
        assert!(bind(bounded(), 0.25, &[1.0], 4).is_ok());
    }

    /// A candidate is raw numbers, so every claim it might have made is proved
    /// against the *actual* request instead.
    #[test]
    fn a_candidate_cannot_assert_its_own_component_state_arity() {
        // Wider, narrower, and empty: the linked component's width decides.
        assert!(bind(request(0.0, 1.0), 0.5, &[1.0, 2.0], 3).is_err());
        assert!(bind(request(0.0, 1.0), 0.5, &[], 3).is_err());
        // The candidate cannot talk its way past the width either: binding
        // takes the count from the component, not from the candidate.
        assert!(
            MeStepProposal::bind(
                request(0.0, 1.0),
                MeStepCandidate::new(0.5, vec![1.0, 2.0], 3),
                2, // an arity the linked component does not have
            )
            .is_err(),
            "the request's own checked width contradicts the claimed component width"
        );
    }

    #[test]
    fn a_candidate_with_a_non_finite_endpoint_never_becomes_a_coordinate() {
        assert!(bind(request(0.0, 1.0), f64::NAN, &[1.0], 3).is_err());
        assert!(bind(request(0.0, 1.0), f64::INFINITY, &[1.0], 3).is_err());
        assert!(bind(request(0.0, 1.0), 0.5, &[f64::NAN], 3).is_err());
    }

    /// The binding's start point is the consumed request's, never the
    /// candidate's, so a candidate computed for an earlier or invented interval
    /// fails against the coordinate the session is actually standing on.
    #[test]
    fn a_replayed_candidate_is_rejected_against_the_actual_current_coordinate() {
        let replayed = bind(request(0.0, 1.0), 0.5, &[1.0], 3).expect("the first step is legal");
        assert_eq!(replayed.previous().time().to_bits(), 0.0_f64.to_bits());

        // The session has moved on to t=0.5; the same candidate returned again
        // now claims no progress at all.
        assert!(bind(request(0.5, 1.0), 0.5, &[1.0], 3).is_err());
        // ...and a candidate from *behind* the current coordinate likewise.
        assert!(bind(request(0.5, 1.0), 0.25, &[1.0], 3).is_err());
    }

    /// Provenance is containment: the proposal reports the consumed request's
    /// own coordinates, so there is nothing to compare and nothing that could
    /// disagree.
    #[test]
    fn a_binding_reports_the_consumed_requests_own_coordinates() {
        let served = MeAdvanceRequest::new(point(0.25, &[1.0]), Some(0.75), 2.0, None, None)
            .expect("fixture request is checked");
        let bound = bind(served, 0.75, &[2.0], 3).expect("the candidate binds");
        assert_eq!(bound.previous().time().to_bits(), 0.25_f64.to_bits());
        assert_eq!(bound.previous().states(), &[1.0]);
        // The endpoint was admitted against the hard stop the request carried,
        // which is the binding bound rather than the later yield.
        assert_eq!(bound.accepted().time().to_bits(), 0.75_f64.to_bits());
    }

    #[test]
    fn a_checked_coordinate_canonicalizes_negative_zero() {
        let point = MeContinuousPoint::new(-0.0, vec![1.0], 1).expect("checked point");
        assert_eq!(point.time().to_bits(), 0.0_f64.to_bits());
        assert_eq!(canonical_coordinate(-0.0).to_bits(), 0.0_f64.to_bits());
        assert_eq!(canonical_coordinate(1.5).to_bits(), 1.5_f64.to_bits());
        assert!(canonical_coordinate(f64::NAN).is_nan());
    }

    #[test]
    fn the_roundoff_policy_is_solver_neutral_and_scale_aware() {
        assert!(accepted_step_roundoff(0.0, 0.0) > 0.0);
        assert!(accepted_step_roundoff(1.0e6, 1.0) > accepted_step_roundoff(1.0, 1.0));
    }

    #[test]
    fn accepted_interval_containment_is_finite_closed_and_roundoff_aware() {
        assert!(accepted_interval_contains(
            0.084,
            0.085,
            0.085_f64.next_up()
        ));
        assert!(accepted_interval_contains(0.084, 0.085, 0.084));
        assert!(!accepted_interval_contains(0.084, 0.085, 0.086));
        assert!(!accepted_interval_contains(0.085, 0.084, 0.084));
        assert!(!accepted_interval_contains(0.084, 0.085, f64::NAN));
    }

    #[test]
    fn the_numerical_setup_rejects_every_unproven_scaling_value() {
        assert!(MeNumericalSetup::new(0.0, 1.0e-6, vec![1.0], 1, None).is_err());
        assert!(MeNumericalSetup::new(1.0e-6, f64::NAN, vec![1.0], 1, None).is_err());
        assert!(MeNumericalSetup::new(1.0e-6, 1.0e-6, vec![1.0, 1.0], 1, None).is_err());
        assert!(MeNumericalSetup::new(1.0e-6, 1.0e-6, vec![0.0], 1, None).is_err());
        assert!(MeNumericalSetup::new(1.0e-6, 1.0e-6, vec![1.0], 1, Some(-1.0)).is_err());
        assert!(MeNumericalSetup::new(1.0e-6, 1.0e-6, vec![1.0], 1, Some(0.1)).is_ok());
        assert!(MeNumericalSetup::new(1.0e-6, 1.0e-6, Vec::new(), 0, None).is_ok());
    }

    #[test]
    fn a_library_failure_keeps_its_identity_as_typed_data() {
        let failure = MeIntegrationError::numerical(
            "bdf",
            MeNumericalFailure::AdvanceExhausted,
            "step limit",
        );
        let MeIntegrationError::Numerical { category, .. } = failure else {
            panic!("the constructor mints a typed numerical failure");
        };
        assert_eq!(category, MeNumericalFailure::AdvanceExhausted);
    }
}
