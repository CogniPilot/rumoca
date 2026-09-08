//! Host-owned event-indicator scanning and root application (SPEC_0044 §6).
//!
//! No root result crosses the numerical-plugin boundary. The host retains the
//! full standard event-indicator vector from the previous completed step,
//! samples the plugin's native continuous extension monotonically across each
//! accepted interval, and classifies the raw `fmi3GetEventIndicators` vector
//! with FMI's exact `z > 0` versus `z <= 0` domains. Crossing, arming,
//! application-side, and simultaneous-event policy live only here.
//!
//! Every failure on this path keeps its identity: component, integrator,
//! allocation, scan-resolution, and root-application failures are distinct
//! variants of [`MeSessionError`], never rendered prose inside a contract
//! failure.
//!
//! Nothing in this module is part of the solver-plugin API: SPEC_0044 §6 makes
//! the policy, the application, the root-search types, and the scan capability
//! host-private with no unchecked constructor.

use std::cell::RefCell;

use super::{
    MeContinuousStateDomain, MeSolverTolerances, SolveMeKernel,
    integrator::{MeAcceptedStep, MeContinuousPoint, MeStepProposal, accepted_step_roundoff},
    kernel::{RootScanIndicatorWidth, RootScanShape, RootScanStateWidth},
    session::{MeSessionError, MeSessionOptions, try_copied, try_filled},
};

#[cfg(test)]
use super::integrator::{MeAdvanceRequest, MeStepCandidate};

/// FMI 3.0.2's asymmetric event-indicator domains.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum IndicatorDomain {
    /// `z > 0`.
    Positive,
    /// `z <= 0`, including exact zero.
    NonPositive,
}

impl IndicatorDomain {
    #[must_use]
    pub(super) fn of(value: f64) -> Self {
        if value > 0.0 {
            Self::Positive
        } else {
            Self::NonPositive
        }
    }
}

/// The private, host-constructed scan and location policy.
///
/// One policy applies to every plugin. Its resolution never caps the integrator
/// step: it bounds the width of each adjacent *sampled* interval inside an
/// accepted step, so a large accepted interval is simply sampled at many
/// checked coordinates. The resolution is a distinct session option, never the
/// output cadence: changing trace density must not change event semantics
#[derive(Debug, Clone)]
pub(super) struct MeRootSearchPolicy {
    scan_resolution: f64,
    location_tolerance: f64,
    tolerances: MeSolverTolerances,
    nominals: MeStateNominals,
    state_domain: MeContinuousStateDomain,
}

#[derive(Debug, Clone)]
struct MeStateNominals(Vec<f64>);

impl MeStateNominals {
    fn check(
        nominals: Vec<f64>,
        state_domain: MeContinuousStateDomain,
    ) -> Result<Self, MeSessionError> {
        if nominals.len() != state_domain.len() {
            return Err(MeSessionError::Options {
                reason: format!(
                    "the root-search policy needs one nominal per continuous state; got {} for a \
                     component of width {}",
                    nominals.len(),
                    state_domain.len()
                ),
            });
        }
        for (index, nominal) in nominals.iter().copied().enumerate() {
            if !nominal.is_finite() || nominal <= 0.0 {
                return Err(MeSessionError::Options {
                    reason: format!(
                        "continuous-state nominal {index} is {nominal}; the root-search policy \
                         requires positive finite nominals"
                    ),
                });
            }
        }
        Ok(Self(nominals))
    }

    fn as_slice(&self) -> &[f64] {
        &self.0
    }
}

impl MeRootSearchPolicy {
    /// Build the policy from host options and the component's nominals.
    ///
    /// The nominal vector must be the component's complete continuous-state
    /// width, and every entry finite and positive: a missing or invalid nominal
    /// is a typed construction failure, never a substituted `1.0`
    pub(super) fn new(
        options: &MeSessionOptions,
        nominals: Vec<f64>,
        state_domain: MeContinuousStateDomain,
    ) -> Result<Self, MeSessionError> {
        let nominals = MeStateNominals::check(nominals, state_domain)?;
        Ok(Self {
            scan_resolution: options.root_scan_resolution(),
            location_tolerance: options.root_location_tolerance(),
            tolerances: options.tolerances(),
            nominals,
            state_domain,
        })
    }

    #[must_use]
    pub(super) fn scan_resolution(&self) -> f64 {
        self.scan_resolution
    }

    #[must_use]
    pub(super) fn location_tolerance(&self) -> f64 {
        self.location_tolerance
    }

    #[must_use]
    pub(super) const fn state_domain(&self) -> MeContinuousStateDomain {
        self.state_domain
    }

    /// The complete positive finite nominal vector the host proved.
    #[must_use]
    pub(super) fn nominals(&self) -> &[f64] {
        self.nominals.as_slice()
    }

    /// The componentwise state-consistency bound SPEC_0044 §6 fixes:
    /// `max(abs_tol, rel_tol * max(nominal_i, |x0_i|, |x1_i|))`.
    ///
    /// `index` is always inside the proven nominal width, because the policy
    /// only compares vectors it has already width-checked.
    fn state_consistency_bound(&self, nominal: f64, x0: f64, x1: f64) -> f64 {
        let scale = nominal.max(x0.abs()).max(x1.abs());
        self.tolerances
            .absolute()
            .max(self.tolerances.relative() * scale)
    }

    /// Require the plugin's sampler to agree with a checked endpoint.
    pub(super) fn require_endpoint_agreement(
        &self,
        label: &str,
        checked: &MeContinuousPoint,
        sampled: &[f64],
    ) -> Result<(), MeSessionError> {
        for (index, ((expected, actual), nominal)) in checked
            .states()
            .iter()
            .copied()
            .zip(sampled.iter().copied())
            .zip(self.nominals.as_slice().iter().copied())
            .enumerate()
        {
            if !actual.is_finite() {
                return Err(MeSessionError::Contract {
                    reason: format!("{label} sampler returned a non-finite state {index}"),
                });
            }
            let bound = self.state_consistency_bound(nominal, expected, actual);
            if (expected - actual).abs() > bound {
                return Err(MeSessionError::Contract {
                    reason: format!(
                        "{label} sampler state {index} is {actual}, but the checked point carries \
                         {expected}; the disagreement exceeds {bound}"
                    ),
                });
            }
        }
        Ok(())
    }
}

/// The checked left/application pair a located event is applied from.
///
/// Root-search types are host-private and have no unchecked constructor. The
/// complete left and application indicator vectors are construction *inputs*:
/// the constructor proves they are finite, of one width, and exhibit at least
/// one domain change at the application coordinate. It retains only the two
/// checked points, because under SPEC_0044 §8's strict surface no indicator
/// value crosses back to the component — argument-free Event Mode is what
/// updates relation memory, so a retained simultaneous set would have no
/// consumer and no authority.
#[derive(Debug, Clone)]
pub(super) struct MeRootApplication {
    left: MeContinuousPoint,
    application: MeContinuousPoint,
}

impl MeRootApplication {
    pub(super) fn new(
        left: MeContinuousPoint,
        application: MeContinuousPoint,
        left_indicators: Vec<f64>,
        application_indicators: Vec<f64>,
    ) -> Result<Self, MeSessionError> {
        if left_indicators.is_empty() || left_indicators.len() != application_indicators.len() {
            return Err(MeSessionError::Contract {
                reason: format!(
                    "root application carries {} left and {} application indicators",
                    left_indicators.len(),
                    application_indicators.len()
                ),
            });
        }
        for (label, indicators) in [
            ("left", &left_indicators),
            ("application", &application_indicators),
        ] {
            if let Some(index) = indicators.iter().position(|value| !value.is_finite()) {
                return Err(MeSessionError::Contract {
                    reason: format!("{label} event indicator {index} is not finite"),
                });
            }
        }
        if application.time() < left.time() {
            return Err(MeSessionError::Contract {
                reason: format!(
                    "root application coordinate {} precedes its left limit {}",
                    application.time(),
                    left.time()
                ),
            });
        }
        if application.width() != left.width() {
            return Err(MeSessionError::Contract {
                reason: format!(
                    "root application carries {} left and {} application states",
                    left.width(),
                    application.width()
                ),
            });
        }
        if !domains_changed(&left_indicators, &application_indicators) {
            return Err(MeSessionError::Contract {
                reason: "root application requires at least one changed indicator domain"
                    .to_owned(),
            });
        }
        Ok(Self { left, application })
    }

    #[must_use]
    pub(super) fn left(&self) -> &MeContinuousPoint {
        &self.left
    }

    #[must_use]
    pub(super) fn application(&self) -> &MeContinuousPoint {
        &self.application
    }
}

/// What the host may ask while scanning one accepted interval.
///
/// The plugin supplies only the state sampler, into a host-owned fixed-width
/// buffer; the indicator evaluation is a standard component call the host owns.
pub(super) trait RootScanTarget {
    /// The plugin's native continuous extension at `time`.
    ///
    /// The plugin's retained derivative capability is deactivated for the whole
    /// call, so this returns the host's own category when a sampler reaches for
    /// the component instead of reading its stored stage values.
    fn sample_states(&mut self, time: f64, states: &mut [f64]) -> Result<(), MeSessionError>;

    /// `fmi3SetTime` + `fmi3SetContinuousStates` + `fmi3GetEventIndicators`.
    ///
    /// An interior component error aborts the scan with its typed status; the
    /// host does not skip, subdivide, retry, or repair the observation. The
    /// buffer is an exact-width borrow of storage [`RootScanWorkspace::new`]
    /// sized once; a target consumes it and refuses a mismatched width, and
    /// the slice type leaves it no way to resize the caller's storage.
    fn indicators_at(
        &mut self,
        time: f64,
        states: &[f64],
        indicators: &mut [f64],
    ) -> Result<(), MeSessionError>;

    /// The session's wall-clock budget, consulted once per sampled coordinate.
    ///
    /// An exhausted budget is a typed abort, not permission to sample coarser
    fn check_budget(&self) -> Result<(), MeSessionError>;
}

/// Prove the plugin's continuous extension covers the proposed interval, and
/// mint the sole checked [`MeAcceptedStep`].
///
/// SPEC_0044 §6's aggregate table puts complete-interval sampling inside the
/// accepted-step construction contract, and a plugin cannot prove that about
/// itself. This is therefore the only constructor of an accepted step, and the
/// host is its only caller.
///
/// The validation runs for **every** proposal, including models with no event
/// indicators at all: the sampler contract does not disappear because a model
/// has no roots. The zero-state case is
/// vacuous.
fn accept_step<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    proposal: MeStepProposal,
) -> Result<MeAcceptedStep, MeSessionError> {
    if proposal.previous().state_domain() != policy.state_domain()
        || proposal.accepted().state_domain() != policy.state_domain()
    {
        return Err(MeSessionError::Contract {
            reason: "accepted-step proposal belongs to a different continuous-state domain"
                .to_owned(),
        });
    }
    let width = policy.state_domain().len();
    let mut left_states = try_filled(width, 0.0, "accepted-step left sample")?;
    target.sample_states(proposal.previous().time(), &mut left_states)?;
    policy.require_endpoint_agreement(
        "accepted-interval left endpoint",
        proposal.previous(),
        &left_states,
    )?;
    let mut right_states = try_filled(width, 0.0, "accepted-step right sample")?;
    target.sample_states(proposal.accepted().time(), &mut right_states)?;
    policy.require_endpoint_agreement(
        "accepted-interval right endpoint",
        proposal.accepted(),
        &right_states,
    )?;
    Ok(MeAcceptedStep::from_validated_proposal(
        proposal,
        left_states,
        right_states,
    ))
}

/// One sampled scan coordinate.
///
/// Both populations are distinct role types over fixed-width `Box<[f64]>`
/// storage. Neither role has `push`, `clear`, `extend`, `reserve`, or `resize`,
/// so the only mutation a consumer can express is an exact-width slice write.
/// A role swap or grow-or-shrink repair path is unrepresentable rather than
/// merely refused.
struct ScanSample {
    time: f64,
    states: RootScanStateBuffer,
    indicators: RootScanIndicatorBuffer,
}

impl ScanSample {
    fn reserved(shape: &RootScanShape) -> Result<Self, MeSessionError> {
        Ok(Self {
            time: 0.0,
            states: RootScanStateBuffer::try_for_width(
                shape.state_width(),
                "scan endpoint states",
            )?,
            indicators: RootScanIndicatorBuffer::try_for_width(
                shape.indicator_width(),
                "scan endpoint indicators",
            )?,
        })
    }
}

mod fixed_scan_buffer {
    use super::{MeSessionError, RootScanIndicatorWidth, RootScanStateWidth, try_filled};

    pub(super) enum RootScanStateRole {}
    pub(super) enum RootScanIndicatorRole {}

    pub(super) struct FixedRootScanBuffer<Role> {
        values: Box<[f64]>,
        role: std::marker::PhantomData<fn() -> Role>,
    }

    impl<Role> std::ops::Deref for FixedRootScanBuffer<Role> {
        type Target = [f64];

        fn deref(&self) -> &Self::Target {
            &self.values
        }
    }

    impl<Role> std::ops::DerefMut for FixedRootScanBuffer<Role> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.values
        }
    }

    pub(super) type RootScanStateBuffer = FixedRootScanBuffer<RootScanStateRole>;
    pub(super) type RootScanIndicatorBuffer = FixedRootScanBuffer<RootScanIndicatorRole>;

    impl FixedRootScanBuffer<RootScanStateRole> {
        pub(super) fn try_for_width(
            width: RootScanStateWidth,
            context: &'static str,
        ) -> Result<Self, MeSessionError> {
            Ok(Self {
                values: try_filled(width.len(), 0.0, context)?.into_boxed_slice(),
                role: std::marker::PhantomData,
            })
        }
    }

    impl FixedRootScanBuffer<RootScanIndicatorRole> {
        pub(super) fn try_for_width(
            width: RootScanIndicatorWidth,
            context: &'static str,
        ) -> Result<Self, MeSessionError> {
            Ok(Self {
                values: try_filled(width.len(), 0.0, context)?.into_boxed_slice(),
                role: std::marker::PhantomData,
            })
        }
    }
}

use fixed_scan_buffer::{RootScanIndicatorBuffer, RootScanStateBuffer};

/// Every buffer one accepted-interval scan and its refinement sample into, plus
/// the retained previous-completed-step indicator vector the scan seeds from.
///
/// [`RootScanWorkspace::new`] is the sole owner. It consumes one kernel-issued
/// [`RootScanShape`], whose state and published-indicator widths have distinct
/// roles. Every state buffer is reserved only through the state capability and
/// every indicator buffer only through the indicator capability, so exchanging
/// the two widths is a compiler error. The scan consumes exact-width
/// `copy_from_slice` and the fixed slice types leave no resize path anywhere
/// between the owner and the checked `fmi3GetEventIndicators` call.
pub(super) struct RootScanWorkspace {
    /// The full standard indicator vector kept from the previous completed step.
    retained: RootScanIndicatorBuffer,
    lower: ScanSample,
    upper: ScanSample,
    states: RootScanStateBuffer,
    indicators: RootScanIndicatorBuffer,
}

impl RootScanWorkspace {
    fn new(shape: RootScanShape) -> Result<Self, MeSessionError> {
        Ok(Self {
            retained: RootScanIndicatorBuffer::try_for_width(
                shape.indicator_width(),
                "retained event indicators",
            )?,
            lower: ScanSample::reserved(&shape)?,
            upper: ScanSample::reserved(&shape)?,
            states: RootScanStateBuffer::try_for_width(shape.state_width(), "scan sample")?,
            indicators: RootScanIndicatorBuffer::try_for_width(
                shape.indicator_width(),
                "scan event indicators",
            )?,
        })
    }

    /// Refresh the retained previous-completed-step indicator vector in place.
    ///
    /// `read` writes exactly the shape-issued indicator width into the owner's
    /// fixed buffer or fails; there is no allocation, replacement, or resize.
    /// The buffer is the same one every scan seeds `lower.indicators` from, so
    /// the refreshed value and the scanned seed are one storage.
    pub(super) fn refresh_retained(
        &mut self,
        read: impl FnOnce(&mut [f64]) -> Result<(), MeSessionError>,
    ) -> Result<(), MeSessionError> {
        read(&mut self.retained)
    }

    /// Whether this component carries any event indicators at all.
    #[must_use]
    pub(super) fn has_indicators(&self) -> bool {
        !self.retained.is_empty()
    }

    #[cfg(test)]
    fn seed_retained(&mut self, values: &[f64]) {
        self.retained.copy_from_slice(values);
    }

    #[cfg(test)]
    pub(super) fn verification_buffer_identity(&self) -> RootScanBufferIdentity {
        let id = |buffer: &[f64]| (buffer.as_ptr() as usize, buffer.len());
        RootScanBufferIdentity {
            retained: id(&self.retained),
            lower_states: id(&self.lower.states),
            lower_indicators: id(&self.lower.indicators),
            upper_states: id(&self.upper.states),
            upper_indicators: id(&self.upper.indicators),
            states: id(&self.states),
            indicators: id(&self.indicators),
        }
    }
}

/// The indivisible root-search owner for one initialized ME host.
///
/// Its sole constructor takes the host's kernel directly and derives both the
/// continuous-state domain and scan-buffer shape from that same kernel. No
/// sibling module can pair a workspace issued by one kernel with policy from
/// another, and no caller supplies either width as an integer.
pub(super) struct MeRootSearchState {
    state_domain: MeContinuousStateDomain,
    policy: Option<MeRootSearchPolicy>,
    workspace: RefCell<RootScanWorkspace>,
}

impl MeRootSearchState {
    pub(super) fn new(kernel: &SolveMeKernel) -> Result<Self, MeSessionError> {
        let state_domain = kernel.continuous_state_domain();
        let workspace = RootScanWorkspace::new(kernel.root_scan_shape())?;
        Ok(Self {
            state_domain,
            policy: None,
            workspace: RefCell::new(workspace),
        })
    }

    pub(super) const fn state_domain(&self) -> MeContinuousStateDomain {
        self.state_domain
    }

    pub(super) fn configure_active(
        &mut self,
        options: &MeSessionOptions,
        nominals: Vec<f64>,
    ) -> Result<(), MeSessionError> {
        self.policy = Some(MeRootSearchPolicy::new(
            options,
            nominals,
            self.state_domain,
        )?);
        Ok(())
    }

    pub(super) fn configure_terminated(&mut self) {
        self.policy = None;
    }

    fn policy(&self) -> Result<&MeRootSearchPolicy, MeSessionError> {
        self.policy.as_ref().ok_or_else(Self::missing_policy_error)
    }

    pub(super) fn nominals(&self) -> Result<&[f64], MeSessionError> {
        self.policy().map(MeRootSearchPolicy::nominals)
    }

    pub(super) fn refresh_nominals(&mut self, nominals: Vec<f64>) -> Result<(), MeSessionError> {
        let checked = MeStateNominals::check(nominals, self.state_domain)?;
        let policy = self
            .policy
            .as_mut()
            .ok_or_else(Self::missing_policy_error)?;
        policy.nominals = checked;
        Ok(())
    }

    fn missing_policy_error() -> MeSessionError {
        MeSessionError::Contract {
            reason: "a session that terminated during initialization has no root-search policy"
                .to_owned(),
        }
    }

    pub(super) fn accept_step<T: RootScanTarget>(
        &self,
        target: &mut T,
        proposal: MeStepProposal,
    ) -> Result<MeAcceptedStep, MeSessionError> {
        accept_step(target, self.policy()?, proposal)
    }

    pub(super) fn scan_accepted_interval<T: RootScanTarget>(
        &self,
        target: &mut T,
        accepted: &MeAcceptedStep,
    ) -> Result<Option<MeRootApplication>, MeSessionError> {
        scan_accepted_interval_with_workspace(
            target,
            self.policy()?,
            accepted,
            &mut self.workspace.borrow_mut(),
        )
    }

    /// Refresh the retained previous-completed-step indicators in the exact
    /// workspace that the next scan will consume.
    pub(super) fn refresh_retained_indicators(
        &self,
        read: impl FnOnce(&mut [f64]) -> Result<(), MeSessionError>,
    ) -> Result<(), MeSessionError> {
        let mut workspace = self.workspace.borrow_mut();
        if workspace.has_indicators() {
            workspace.refresh_retained(read)?;
        }
        Ok(())
    }
}

/// The `(pointer, length)` identity of every buffer the scan workspace owns.
///
/// A resize or a wholesale replacement of any buffer changes its pointer or its
/// length, so an equality assertion across a whole scan-plus-refinement dies if
/// either is reintroduced.
#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RootScanBufferIdentity {
    retained: (usize, usize),
    lower_states: (usize, usize),
    lower_indicators: (usize, usize),
    upper_states: (usize, usize),
    upper_indicators: (usize, usize),
    states: (usize, usize),
    indicators: (usize, usize),
}

/// The workspace's sampling buffers, borrowed for one bracket refinement.
///
/// Refinement consumes the same owner-sized storage the coarse scan sampled
/// into; the slice fields make a refiner-side resize unrepresentable.
struct ScanScratch<'workspace> {
    states: &'workspace mut [f64],
    indicators: &'workspace mut [f64],
}

/// The checked monotone coordinate grid one accepted interval is scanned on.
///
/// Storage is O(1): the grid is an index range, not a materialized vector, so
/// the promised resolution is never silently weakened to fit an allocation
/// A step count the host cannot represent is a typed
/// resource failure, not a coarser scan.
struct ScanGrid {
    start: f64,
    end: f64,
    steps: u64,
    resolution: f64,
}

impl ScanGrid {
    fn new(start: f64, end: f64, resolution: f64) -> Result<Self, MeSessionError> {
        let width = end - start;
        if width <= 0.0 || !width.is_finite() {
            return Err(MeSessionError::Contract {
                reason: format!("a scan grid needs a positive width, got [{start}, {end}]"),
            });
        }
        let unrepresentable = || MeSessionError::RootScanUnrepresentable {
            start,
            end,
            resolution,
        };
        let requested = (width / resolution).ceil();
        // The exact condition, from the counter type: `steps` is indexed as a
        // `u64` and every index is also used as an `f64` fraction, so the count
        // must be an integer both types represent exactly. That is `2^53`, the
        // largest integer with an exact `f64` image — not an approximation of
        // `u64::MAX`.
        if !requested.is_finite() || !(1.0..=EXACT_INTEGER_LIMIT).contains(&requested) {
            return Err(unrepresentable());
        }
        let steps = (requested as u64).max(1);
        // Every adjacent sampled interval must actually be representable at the
        // promised width; if the local ULP swallows it, that is a typed
        // resource failure, never a coarser scan.
        let sub_width = width / (steps as f64);
        if sub_width <= 0.0
            || sub_width > resolution + accepted_step_roundoff(start, sub_width)
            || start + sub_width <= start
            || end - sub_width >= end
        {
            return Err(unrepresentable());
        }
        Ok(Self {
            start,
            end,
            steps,
            resolution,
        })
    }

    /// The `step`-th interior-or-final coordinate, for `step` in `1..=steps`.
    ///
    /// The final coordinate is exactly the accepted endpoint. An interior
    /// coordinate that floating point cannot separate from its neighbours is a
    /// typed failure: silently returning the endpoint would weaken the promised
    /// resolution exactly like the deleted count cap.
    fn coordinate(&self, step: u64, previous: f64) -> Result<f64, MeSessionError> {
        if step >= self.steps {
            return Ok(self.end);
        }
        let fraction = (step as f64) / (self.steps as f64);
        let coordinate = self.start + (self.end - self.start) * fraction;
        // The adjacent width is compared under the same solver-neutral roundoff
        // policy the accepted-step contract uses, so exact arithmetic on a
        // representable grid is never mistaken for a weakened resolution.
        let gap = coordinate - previous;
        if coordinate <= previous
            || coordinate >= self.end
            || gap > self.resolution + accepted_step_roundoff(previous, gap)
        {
            return Err(MeSessionError::RootScanUnrepresentable {
                start: self.start,
                end: self.end,
                resolution: self.resolution,
            });
        }
        Ok(coordinate)
    }
}

#[cfg(test)]
fn verification_shape(policy: &MeRootSearchPolicy, indicator_values: &[f64]) -> RootScanShape {
    RootScanShape::verification_fixture(
        RootScanStateWidth::verification_fixture(policy.state_domain().len()),
        RootScanIndicatorWidth::verification_fixture(indicator_values.len()),
    )
}

#[cfg(test)]
fn scan_accepted_interval<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    accepted: &MeAcceptedStep,
    retained: &[f64],
) -> Result<Option<MeRootApplication>, MeSessionError> {
    let mut workspace = RootScanWorkspace::new(verification_shape(policy, retained))?;
    workspace.seed_retained(retained);
    scan_accepted_interval_with_workspace(target, policy, accepted, &mut workspace)
}

/// Scan `[previous, accepted]` and return the earliest domain change, if any.
///
/// The retained indicator vector the scan seeds from lives inside `workspace`,
/// sized with it from the kernel-issued indicator role; the scan never queries
/// before that point. The endpoint samples come from the checked step, which
/// [`accept_step`] already validated.
fn scan_accepted_interval_with_workspace<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    accepted: &MeAcceptedStep,
    workspace: &mut RootScanWorkspace,
) -> Result<Option<MeRootApplication>, MeSessionError> {
    if !workspace.has_indicators() {
        return Ok(None);
    }
    let RootScanWorkspace {
        retained,
        lower,
        upper,
        states,
        indicators,
    } = workspace;
    // Fixed-width borrows of the owner's storage. Every write below is an
    // exact-width `copy_from_slice`; the slice types leave no resize path.
    let states: &mut [f64] = &mut states[..];
    let indicators: &mut [f64] = &mut indicators[..];
    lower.time = accepted.previous().time();
    lower.states.copy_from_slice(accepted.left_states());
    lower.indicators.copy_from_slice(&retained[..]);
    let grid = ScanGrid::new(
        accepted.previous().time(),
        accepted.accepted().time(),
        policy.scan_resolution(),
    )?;
    for step in 1..=grid.steps {
        // A scan is host work bounded by the session's own budget, never a
        // reason to coarsen: exhausting the budget is a typed abort.
        target.check_budget()?;
        let coordinate = grid.coordinate(step, lower.time)?;
        let is_end = coordinate.to_bits() == grid.end.to_bits();
        if is_end {
            states.copy_from_slice(accepted.right_states());
        } else {
            target.sample_states(coordinate, states)?;
        }
        target.indicators_at(coordinate, states, indicators)?;
        require_finite_indicators(indicators)?;
        if domains_changed(&lower.indicators, indicators) {
            upper.time = coordinate;
            upper.states.copy_from_slice(states);
            upper.indicators.copy_from_slice(indicators);
            let mut scratch = ScanScratch { states, indicators };
            return refine_bracket(target, policy, lower, upper, &mut scratch).map(Some);
        }
        if is_end {
            break;
        }
        lower.time = coordinate;
        lower.states.copy_from_slice(states);
        lower.indicators.copy_from_slice(indicators);
    }
    Ok(None)
}

/// The only runtime check left on a scanned indicator vector: finiteness.
///
/// The width is a construction-issued fact — every scan buffer is the workspace
/// width, and the checked `fmi3GetEventIndicators` boundary refuses any other
/// buffer — so the scan re-proves nothing about it. Whether the *values* the
/// component just computed are finite is a genuine runtime property.
fn require_finite_indicators(indicators: &[f64]) -> Result<(), MeSessionError> {
    if let Some(index) = indicators.iter().position(|value| !value.is_finite()) {
        return Err(MeSessionError::Contract {
            reason: format!("event indicator {index} is not finite"),
        });
    }
    Ok(())
}

fn domains_changed(before: &[f64], after: &[f64]) -> bool {
    before
        .iter()
        .zip(after)
        .any(|(before, after)| IndicatorDomain::of(*before) != IndicatorDomain::of(*after))
}

/// The refined bracket one indicator's domain change was localized to.
struct RefinedBracket {
    left_time: f64,
    application_time: f64,
}

/// Refine every changed indicator inside the first bracket and apply the
/// earliest domain change observable at the checked policy resolution.
///
/// The winning indicator's own refined left coordinate is retained:
/// event-left evidence is never moved back to the coarse bracket's lower point,
/// which can be a whole scan resolution earlier.
fn refine_bracket<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    lower: &ScanSample,
    upper: &ScanSample,
    scratch: &mut ScanScratch<'_>,
) -> Result<MeRootApplication, MeSessionError> {
    let state_domain = policy.state_domain();
    let mut winner: Option<RefinedBracket> = None;
    for index in 0..lower.indicators.len() {
        let before = IndicatorDomain::of(lower.indicators[index]);
        let after = IndicatorDomain::of(upper.indicators[index]);
        if before == after {
            continue;
        }
        let refined = refine_indicator(target, policy, lower, upper, index, before, scratch)?;
        let earlier = winner
            .as_ref()
            .is_none_or(|best| refined.application_time < best.application_time);
        if earlier {
            winner = Some(refined);
        }
    }
    let Some(winner) = winner else {
        return Err(MeSessionError::Contract {
            reason: "the scan reported a domain change no indicator refinement could confirm"
                .to_owned(),
        });
    };

    let left_states = if winner.left_time.to_bits() == lower.time.to_bits() {
        try_copied(&lower.states, "refined left states")?
    } else {
        target.sample_states(winner.left_time, scratch.states)?;
        try_copied(scratch.states, "refined left states")?
    };
    let left_indicators = if winner.left_time.to_bits() == lower.time.to_bits() {
        try_copied(&lower.indicators, "refined left indicators")?
    } else {
        target.indicators_at(winner.left_time, &left_states, scratch.indicators)?;
        require_finite_indicators(scratch.indicators)?;
        try_copied(scratch.indicators, "refined left indicators")?
    };

    let application_states = if winner.application_time.to_bits() == upper.time.to_bits() {
        try_copied(&upper.states, "application states")?
    } else {
        target.sample_states(winner.application_time, scratch.states)?;
        try_copied(scratch.states, "application states")?
    };
    target.indicators_at(
        winner.application_time,
        &application_states,
        scratch.indicators,
    )?;
    require_finite_indicators(scratch.indicators)?;
    let application_indicators = try_copied(scratch.indicators, "application indicators")?;

    let left = MeContinuousPoint::new(winner.left_time, left_states, state_domain)?;
    let application =
        MeContinuousPoint::new(winner.application_time, application_states, state_domain)?;
    MeRootApplication::new(left, application, left_indicators, application_indicators)
}

/// Bisect toward the least coordinate in the newly entered domain.
///
/// Exhausting the host-owned iteration budget without attaining the checked
/// location tolerance is the typed `RootApplicationUnavailable` failure
/// SPEC_0044 §6 requires, not a plausible application.
fn refine_indicator<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    lower: &ScanSample,
    upper: &ScanSample,
    index: usize,
    entry_domain: IndicatorDomain,
    scratch: &mut ScanScratch<'_>,
) -> Result<RefinedBracket, MeSessionError> {
    let mut low = lower.time;
    let mut high = upper.time;
    // A bisection to the location tolerance over a bracket that is already at
    // most one scan resolution wide terminates in a bounded, host-owned count.
    for _ in 0..MAX_REFINEMENT_ITERATIONS {
        target.check_budget()?;
        if high - low <= policy.location_tolerance() {
            return Ok(RefinedBracket {
                left_time: low,
                application_time: high,
            });
        }
        let middle = low + 0.5 * (high - low);
        if middle <= low || middle >= high {
            // Adjacent representable coordinates: no tighter bracket exists,
            // and the location tolerance is attained as far as f64 allows.
            return Ok(RefinedBracket {
                left_time: low,
                application_time: high,
            });
        }
        target.sample_states(middle, scratch.states)?;
        target.indicators_at(middle, scratch.states, scratch.indicators)?;
        require_finite_indicators(scratch.indicators)?;
        let Some(value) = scratch.indicators.get(index).copied() else {
            return Err(MeSessionError::Contract {
                reason: format!("event indicator {index} is missing from the refined vector"),
            });
        };
        if IndicatorDomain::of(value) == entry_domain {
            low = middle;
        } else {
            high = middle;
        }
    }
    Err(MeSessionError::RootApplicationUnavailable {
        time: high,
        reason: format!(
            "indicator {index} was not localized to {} within {MAX_REFINEMENT_ITERATIONS} \
             refinements; the bracket is still [{low}, {high}]",
            policy.location_tolerance()
        ),
    })
}

const MAX_REFINEMENT_ITERATIONS: usize = 128;

/// `2^53`: the largest integer whose `f64` image is exact, hence the largest
/// scan-step count the host can both index as a `u64` and divide as an `f64`
/// without losing a coordinate. A request beyond it is a typed resource
/// failure, never a coarser scan.
const EXACT_INTEGER_LIMIT: f64 = 9_007_199_254_740_992.0;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmi_me::session::MeSessionOptionsInput;

    fn options() -> MeSessionOptions {
        MeSessionOptions::new(MeSessionOptionsInput {
            stop_time: Some(1.0),
            relative_tolerance: 1.0e-6,
            absolute_tolerance: 1.0e-8,
            output_interval: 0.1,
            root_scan_resolution: 0.1,
            root_location_tolerance: 1.0e-9,
            max_wall_seconds: None,
            records_trace: false,
        })
        .expect("fixture options are checked")
    }

    fn policy() -> MeRootSearchPolicy {
        MeRootSearchPolicy::new(
            &options(),
            vec![1.0],
            MeContinuousStateDomain::verification_fixture(1),
        )
        .expect("fixture policy is checked")
    }

    fn point(time: f64, state: f64) -> MeContinuousPoint {
        MeContinuousPoint::new(
            time,
            vec![state],
            MeContinuousStateDomain::verification_fixture(1),
        )
        .expect("fixture point is checked")
    }

    /// `x(t) = t - 0.25`, one indicator equal to the state.
    struct LinearCrossing {
        indicator_calls: usize,
    }

    impl LinearCrossing {
        fn new() -> Self {
            Self { indicator_calls: 0 }
        }
    }

    impl RootScanTarget for LinearCrossing {
        fn sample_states(&mut self, time: f64, states: &mut [f64]) -> Result<(), MeSessionError> {
            states[0] = time - 0.25;
            Ok(())
        }

        fn indicators_at(
            &mut self,
            _time: f64,
            states: &[f64],
            indicators: &mut [f64],
        ) -> Result<(), MeSessionError> {
            self.indicator_calls += 1;
            // The same exact-width refusal the checked component call owns:
            // a target consumes the owner-sized buffer, it never repairs it.
            if indicators.len() != 1 {
                return Err(MeSessionError::Contract {
                    reason: format!(
                        "event-indicator buffer has {} entries for 1 indicators",
                        indicators.len()
                    ),
                });
            }
            indicators[0] = states[0];
            Ok(())
        }

        fn check_budget(&self) -> Result<(), MeSessionError> {
            Ok(())
        }
    }

    /// Drive the whole host path a session drives: build the accepted step the
    /// plugin would have minted, prove its sampler, then scan only the proof.
    fn scan(
        target: &mut LinearCrossing,
        previous: &MeContinuousPoint,
        accepted: &MeContinuousPoint,
        retained: &[f64],
    ) -> Result<Option<MeRootApplication>, MeSessionError> {
        let policy = policy();
        let request = MeAdvanceRequest::new(previous.clone(), None, accepted.time(), None, None)?;
        let candidate = MeStepCandidate::new(accepted.time(), accepted.states().to_vec(), 3);
        let proposal = MeStepProposal::bind(request, candidate)?;
        let step = accept_step(target, &policy, proposal)?;
        scan_accepted_interval(target, &policy, &step, retained)
    }

    #[test]
    fn a_nominal_and_non_positive_domain_split_at_exact_zero() {
        assert_eq!(IndicatorDomain::of(1.0e-300), IndicatorDomain::Positive);
        assert_eq!(IndicatorDomain::of(0.0), IndicatorDomain::NonPositive);
        assert_eq!(IndicatorDomain::of(-0.0), IndicatorDomain::NonPositive);
    }

    #[test]
    fn the_policy_rejects_a_non_positive_or_incomplete_nominal_vector() {
        let domain = MeContinuousStateDomain::verification_fixture(1);
        let options = options();
        assert!(MeRootSearchPolicy::new(&options, vec![0.0], domain).is_err());
        assert!(MeRootSearchPolicy::new(&options, vec![f64::INFINITY], domain).is_err());
        assert!(MeRootSearchPolicy::new(&options, Vec::new(), domain).is_err());
        assert!(MeRootSearchPolicy::new(&options, vec![1.0, 1.0], domain).is_err());
    }

    #[test]
    fn scanning_locates_the_earliest_domain_change_in_the_interval() {
        let mut target = LinearCrossing::new();
        let application = scan(&mut target, &point(0.0, -0.25), &point(1.0, 0.75), &[-0.25])
            .expect("the scan succeeds")
            .expect("a crossing exists inside the interval");

        assert!((application.application().time() - 0.25).abs() <= 1.0e-8);
        assert!(target.indicator_calls > 1);
    }

    /// The four lower/upper scan buffers, the retained seed, and the two
    /// sampling buffers keep one `(pointer, length)` identity across a whole
    /// coarse scan **and** its bracket refinement.
    ///
    /// The crossing forces the refinement path, so `upper.states` and
    /// `upper.indicators` are written, not only the lower pair. Because every
    /// buffer is a fixed-width `Box<[f64]>` mutated only by `copy_from_slice`,
    /// the identity holds. It dies the instant a resize/repair path (a `Vec`
    /// grown by `try_reserve_exact` + `clear` + `extend`) or a wholesale
    /// replacement (`self.upper.states = ...`) is reintroduced anywhere in the
    /// scan or refinement, because either changes a pointer or a length.
    ///
    /// This identity is evidence, not proof: it dies on a resize or a
    /// replacement of a persistent buffer, and on nothing else. A temporary
    /// allocated inside a call, evaluated into, and copied back into the
    /// persistent buffer changes no pointer and delivers correct values, so
    /// both the identity rows and the value assertions stay green; the
    /// companion source scan bans only resize-shaped tokens, not `vec![`,
    /// `Vec::new(`, or `.to_vec(`. That shape is live in this very call
    /// graph: the indicator read this workspace refreshes through
    /// (`refresh_retained_indicators_into`, `get_event_indicators`,
    /// `event_indicators_into`) allocates internally today on the
    /// linearization-cache-hit and dynamic-deadline paths. That residue is
    /// owned by adversarial review of the evaluation call chain, not by this
    /// witness.
    #[test]
    fn the_scan_buffers_keep_one_identity_across_a_scan_and_its_refinement() {
        let mut target = LinearCrossing::new();
        let policy = policy();
        let previous = point(0.0, -0.25);
        let accepted = point(1.0, 0.75);
        let request = MeAdvanceRequest::new(previous.clone(), None, accepted.time(), None, None)
            .expect("the advance request is checked");
        let candidate = MeStepCandidate::new(accepted.time(), accepted.states().to_vec(), 3);
        let proposal = MeStepProposal::bind(request, candidate).expect("the proposal binds");
        let step = accept_step(&mut target, &policy, proposal).expect("the sampler agrees");
        let retained = [-0.25];
        let mut workspace = RootScanWorkspace::new(verification_shape(&policy, &retained))
            .expect("the workspace reserves");
        workspace.seed_retained(&retained);

        let before = workspace.verification_buffer_identity();
        let application =
            scan_accepted_interval_with_workspace(&mut target, &policy, &step, &mut workspace)
                .expect("the scan succeeds")
                .expect("a crossing exists inside the interval");
        assert!((application.application().time() - 0.25).abs() <= 1.0e-8);
        assert!(
            target.indicator_calls > 2,
            "the crossing must force the bracket-refinement path so the upper buffers are written"
        );
        let after = workspace.verification_buffer_identity();
        assert_eq!(
            before, after,
            "a scan-plus-refinement resized or replaced a scan buffer: {before:?} -> {after:?}"
        );

        // The retained refresh must hand out the construction-reserved
        // retained buffer itself and leave every buffer identity untouched. A
        // refresh that read into a temporary would fail the in-closure
        // identity assertion; one that assigned `self.retained` a freshly
        // boxed slice would change the pointer captured on either side of
        // this call.
        workspace
            .refresh_retained(|retained| {
                assert_eq!(
                    (retained.as_ptr() as usize, retained.len()),
                    after.retained,
                    "refresh_retained must expose the retained buffer, not a temporary"
                );
                retained.copy_from_slice(&[0.75]);
                Ok(())
            })
            .expect("the in-place retained refresh succeeds");
        let refreshed = workspace.verification_buffer_identity();
        assert_eq!(
            after, refreshed,
            "a retained refresh resized or replaced a scan buffer: {after:?} -> {refreshed:?}"
        );
    }

    #[test]
    fn unequal_state_and_indicator_widths_keep_their_roles_through_a_real_scan() {
        struct TwoStatesOneIndicator;

        impl RootScanTarget for TwoStatesOneIndicator {
            fn sample_states(
                &mut self,
                time: f64,
                states: &mut [f64],
            ) -> Result<(), MeSessionError> {
                states.copy_from_slice(&[time - 0.25, 10.0 + time]);
                Ok(())
            }

            fn indicators_at(
                &mut self,
                _time: f64,
                states: &[f64],
                indicators: &mut [f64],
            ) -> Result<(), MeSessionError> {
                indicators.copy_from_slice(&[states[0]]);
                Ok(())
            }

            fn check_budget(&self) -> Result<(), MeSessionError> {
                Ok(())
            }
        }

        let domain = MeContinuousStateDomain::verification_fixture(2);
        let policy = MeRootSearchPolicy::new(&options(), vec![1.0, 1.0], domain)
            .expect("two-state policy is checked");
        let previous = MeContinuousPoint::new(0.0, vec![-0.25, 10.0], domain)
            .expect("two-state start is checked");
        let accepted = MeContinuousPoint::new(1.0, vec![0.75, 11.0], domain)
            .expect("two-state endpoint is checked");
        let request = MeAdvanceRequest::new(previous, None, 1.0, None, None)
            .expect("the advance request is checked");
        let proposal = MeStepProposal::bind(
            request,
            MeStepCandidate::new(1.0, accepted.states().to_vec(), 3),
        )
        .expect("the proposal inherits its request's two-state domain");
        let mut target = TwoStatesOneIndicator;
        let step = accept_step(&mut target, &policy, proposal).expect("the sampler agrees");
        let retained = [-0.25];
        let mut workspace = RootScanWorkspace::new(verification_shape(&policy, &retained))
            .expect("the unequal-width workspace reserves");
        workspace.seed_retained(&retained);
        let identity = workspace.verification_buffer_identity();
        assert_eq!(identity.lower_states.1, 2);
        assert_eq!(identity.upper_states.1, 2);
        assert_eq!(identity.states.1, 2);
        assert_eq!(identity.retained.1, 1);
        assert_eq!(identity.lower_indicators.1, 1);
        assert_eq!(identity.upper_indicators.1, 1);
        assert_eq!(identity.indicators.1, 1);

        let application =
            scan_accepted_interval_with_workspace(&mut target, &policy, &step, &mut workspace)
                .expect("the unequal-width scan succeeds")
                .expect("the first state crosses its single indicator");
        assert!((application.application().time() - 0.25).abs() <= 1.0e-8);
    }

    #[test]
    fn the_refined_left_limit_is_retained_rather_than_the_coarse_bracket() {
        let mut target = LinearCrossing::new();
        let application = scan(&mut target, &point(0.0, -0.25), &point(1.0, 0.75), &[-0.25])
            .expect("the scan succeeds")
            .expect("a crossing exists inside the interval");
        let left = application.left().time();
        let applied = application.application().time();
        assert!(applied - left <= policy().location_tolerance());
        assert!(
            left > 0.2,
            "the coarse bracket lower point 0.2 must not become the event-left evidence, got \
             {left}"
        );
    }

    #[test]
    fn an_interval_without_a_domain_change_reports_no_application() {
        let mut target = LinearCrossing::new();
        let application = scan(&mut target, &point(0.3, 0.05), &point(0.5, 0.25), &[0.05])
            .expect("the scan succeeds");
        assert!(application.is_none());
    }

    #[test]
    fn an_empty_indicator_inventory_still_validates_the_sampler_endpoints() {
        let mut target = LinearCrossing::new();
        assert!(
            scan(&mut target, &point(0.0, -0.25), &point(1.0, 0.75), &[])
                .expect("the scan succeeds")
                .is_none()
        );
        // A stale endpoint is rejected even though no indicator exists.
        let mut target = LinearCrossing::new();
        assert!(scan(&mut target, &point(0.0, 5.0), &point(1.0, 0.75), &[]).is_err());
    }

    #[test]
    fn a_sampler_that_disagrees_with_a_checked_endpoint_is_rejected() {
        let mut target = LinearCrossing::new();
        assert!(scan(&mut target, &point(0.0, 5.0), &point(1.0, 0.75), &[-0.25]).is_err());
    }

    #[test]
    fn the_scan_grid_ends_exactly_on_the_accepted_endpoint() {
        let grid = ScanGrid::new(0.0, 1.0, 0.25).expect("a representable grid");
        assert_eq!(grid.steps, 4);
        let mut previous = 0.0_f64;
        let mut coordinates = Vec::new();
        for step in 1..=grid.steps {
            let coordinate = grid
                .coordinate(step, previous)
                .expect("every coordinate on a representable grid exists");
            coordinates.push(coordinate);
            previous = coordinate;
        }
        assert_eq!(
            coordinates.last().copied().map(f64::to_bits),
            Some(1.0_f64.to_bits())
        );
        assert!(coordinates.windows(2).all(|pair| pair[0] < pair[1]));
        assert!(coordinates.windows(2).all(|pair| {
            let gap = pair[1] - pair[0];
            gap <= grid.resolution + accepted_step_roundoff(pair[0], gap)
        }));
    }

    #[test]
    fn an_unrepresentable_scan_resolution_is_a_typed_failure_not_a_coarser_scan() {
        assert!(matches!(
            ScanGrid::new(0.0, 1.0, f64::MIN_POSITIVE),
            Err(MeSessionError::RootScanUnrepresentable { .. })
        ));
    }

    #[test]
    fn a_resolution_below_the_local_ulp_at_a_large_start_time_is_a_typed_failure() {
        // At t = 1e12 the spacing between representable doubles is far larger
        // than the requested resolution, so no grid can honour the promised
        // adjacent width. That is a typed resource failure, never a silent
        // snap onto the endpoint.
        let start = 1.0e12_f64;
        assert!(matches!(
            ScanGrid::new(start, start + 1.0, 1.0e-9),
            Err(MeSessionError::RootScanUnrepresentable { .. })
        ));
    }

    #[test]
    fn a_root_application_requires_a_real_finite_domain_change() {
        assert!(
            MeRootApplication::new(point(0.0, 0.0), point(1.0, 1.0), vec![1.0], vec![2.0]).is_err()
        );
        assert!(
            MeRootApplication::new(point(0.0, 0.0), point(1.0, 1.0), Vec::new(), Vec::new())
                .is_err()
        );
        assert!(
            MeRootApplication::new(point(0.0, 0.0), point(1.0, 1.0), vec![f64::NAN], vec![2.0])
                .is_err()
        );
        assert!(
            MeRootApplication::new(point(0.0, 0.0), point(1.0, 1.0), vec![-1.0], vec![2.0]).is_ok()
        );
    }
}
