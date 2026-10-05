//! Host-owned event-indicator scanning and root application (SPEC_0044 §6).
//!
//! The sign-change classification and the domains it uses are FMI 3.0 section
//! 3. The method of locating the event by sampling the integrator's own
//! continuous extension across the accepted step, rather than by rejecting and
//! reducing the step, is L. F. Shampine, I. Gladwell and R. W. Brankin,
//! "Reliable solution of special event location problems for ODEs", ACM
//! Transactions on Mathematical Software 17(1):11-25, 1991,
//! doi:10.1145/103147.103149.
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

use rumoca_ir_solve::fmi::{RootBracket, RootLocationPlan};

use super::{
    integrator::{MeAcceptedStep, MeContinuousPoint, MeStepProposal, accepted_step_roundoff},
    session::{MeSessionError, try_copied, try_filled},
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
    /// The component's refinement method and budget (SPEC_0044
    /// ME-EVENT-004).
    root_location: RootLocationPlan,
    state_abs_tolerance: f64,
    state_rel_tolerance: f64,
    nominals: Vec<f64>,
}

impl MeRootSearchPolicy {
    /// Build the policy from host options and the component's nominals.
    ///
    /// The nominal vector must be the component's complete continuous-state
    /// width, and every entry finite and positive: a missing or invalid nominal
    /// is a typed construction failure, never a substituted `1.0`
    pub(super) fn new(
        scan_resolution: f64,
        location_tolerance: f64,
        state_abs_tolerance: f64,
        state_rel_tolerance: f64,
        nominals: Vec<f64>,
        state_count: usize,
        root_location: RootLocationPlan,
    ) -> Result<Self, MeSessionError> {
        if nominals.len() != state_count {
            return Err(MeSessionError::Options {
                reason: format!(
                    "the root-search policy needs one nominal per continuous state; got {} for a \
                     component of width {state_count}",
                    nominals.len()
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
        for (label, value) in [
            ("scan resolution", scan_resolution),
            ("location tolerance", location_tolerance),
            ("state absolute tolerance", state_abs_tolerance),
            ("state relative tolerance", state_rel_tolerance),
        ] {
            if !value.is_finite() || value <= 0.0 {
                return Err(MeSessionError::Options {
                    reason: format!("root-search {label} must be finite and positive, got {value}"),
                });
            }
        }
        Ok(Self {
            scan_resolution,
            location_tolerance,
            root_location,
            state_abs_tolerance,
            state_rel_tolerance,
            nominals,
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
    pub(super) fn state_count(&self) -> usize {
        self.nominals.len()
    }

    /// The complete positive finite nominal vector the host proved.
    #[must_use]
    pub(super) fn nominals(&self) -> &[f64] {
        &self.nominals
    }

    /// The componentwise state-consistency bound SPEC_0044 §6 fixes:
    /// `max(abs_tol, rel_tol * max(nominal_i, |x0_i|, |x1_i|))`.
    ///
    /// `index` is always inside the proven nominal width, because the policy
    /// only compares vectors it has already width-checked.
    fn state_consistency_bound(&self, nominal: f64, x0: f64, x1: f64) -> f64 {
        let scale = nominal.max(x0.abs()).max(x1.abs());
        self.state_abs_tolerance
            .max(self.state_rel_tolerance * scale)
    }

    /// Replace the nominal vector after Event Mode reported changed nominals.
    pub(super) fn with_nominals(&self, nominals: Vec<f64>) -> Result<Self, MeSessionError> {
        let state_count = self.nominals.len();
        Self::new(
            self.scan_resolution,
            self.location_tolerance,
            self.state_abs_tolerance,
            self.state_rel_tolerance,
            nominals,
            state_count,
            self.root_location,
        )
    }

    /// Require the plugin's sampler to agree with a checked endpoint.
    pub(super) fn require_endpoint_agreement(
        &self,
        label: &str,
        checked: &MeContinuousPoint,
        sampled: &[f64],
    ) -> Result<(), MeSessionError> {
        if sampled.len() != checked.width() || sampled.len() != self.nominals.len() {
            return Err(MeSessionError::Contract {
                reason: format!(
                    "{label} sampler returned {} states for a component of width {}",
                    sampled.len(),
                    self.nominals.len()
                ),
            });
        }
        for (index, ((expected, actual), nominal)) in checked
            .states()
            .iter()
            .copied()
            .zip(sampled.iter().copied())
            .zip(self.nominals.iter().copied())
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
    /// host does not skip, subdivide, retry, or repair the observation.
    fn indicators_at(
        &mut self,
        time: f64,
        states: &[f64],
        indicators: &mut Vec<f64>,
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
pub(super) fn accept_step<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    proposal: MeStepProposal,
) -> Result<MeAcceptedStep, MeSessionError> {
    let width = policy.state_count();
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
struct ScanSample {
    time: f64,
    states: Vec<f64>,
    indicators: Vec<f64>,
}

impl Default for ScanSample {
    fn default() -> Self {
        Self {
            time: 0.0,
            states: Vec::new(),
            indicators: Vec::new(),
        }
    }
}

#[derive(Default)]
pub(super) struct RootScanWorkspace {
    lower: ScanSample,
    upper: ScanSample,
    states: Vec<f64>,
    indicators: Vec<f64>,
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
        // SPEC_0021: Exception - conversion bounds are established by the adjacent invariant.
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        let steps = (requested as u64).max(1);
        // Every adjacent sampled interval must actually be representable at the
        // promised width; if the local ULP swallows it, that is a typed
        // resource failure, never a coarser scan.
        // SPEC_0021: Exception - conversion bounds are established by the adjacent invariant.
        #[allow(clippy::cast_precision_loss)]
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
        // SPEC_0021: Exception - conversion bounds are established by the adjacent invariant.
        #[allow(clippy::cast_precision_loss)]
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

/// Scan `[previous, accepted]` and return the earliest domain change, if any.
///
/// `retained` is the full indicator vector the host kept from the previous
/// completed step; the scan never queries before that point. The endpoint
/// samples come from the checked step, which [`accept_step`] already validated.
#[cfg(test)]
fn scan_accepted_interval<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    accepted: &MeAcceptedStep,
    retained: &[f64],
) -> Result<Option<MeRootApplication>, MeSessionError> {
    scan_accepted_interval_with_workspace(
        target,
        policy,
        accepted,
        retained,
        &mut RootScanWorkspace::default(),
    )
}

pub(super) fn scan_accepted_interval_with_workspace<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    accepted: &MeAcceptedStep,
    retained: &[f64],
    workspace: &mut RootScanWorkspace,
) -> Result<Option<MeRootApplication>, MeSessionError> {
    if retained.is_empty() {
        return Ok(None);
    }
    let width = policy.state_count();
    let RootScanWorkspace {
        lower,
        upper,
        states,
        indicators,
    } = workspace;
    lower.time = accepted.previous().time();
    copy_scan_values(
        &mut lower.states,
        accepted.left_states(),
        "scan left endpoint",
    )?;
    copy_scan_values(&mut lower.indicators, retained, "retained event indicators")?;
    let grid = ScanGrid::new(
        accepted.previous().time(),
        accepted.accepted().time(),
        policy.scan_resolution(),
    )?;
    resize_scan_values(states, width, "scan sample")?;
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
        require_indicator_width(retained.len(), indicators)?;
        if domains_changed(&lower.indicators, indicators) {
            upper.time = coordinate;
            copy_scan_values(&mut upper.states, states, "scan bracket states")?;
            copy_scan_values(&mut upper.indicators, indicators, "scan bracket indicators")?;
            return refine_bracket(target, policy, lower, upper).map(Some);
        }
        if is_end {
            break;
        }
        lower.time = coordinate;
        copy_scan_values(&mut lower.states, states, "scan lower states")?;
        copy_scan_values(&mut lower.indicators, indicators, "scan lower indicators")?;
    }
    Ok(None)
}

fn resize_scan_values<T: Clone + Default>(
    values: &mut Vec<T>,
    len: usize,
    context: &'static str,
) -> Result<(), MeSessionError> {
    if len > values.len() {
        values
            .try_reserve_exact(len - values.len())
            .map_err(|_| MeSessionError::Allocation {
                context,
                entries: len,
            })?;
    }
    values.resize(len, T::default());
    Ok(())
}

fn copy_scan_values<T: Copy>(
    values: &mut Vec<T>,
    source: &[T],
    context: &'static str,
) -> Result<(), MeSessionError> {
    if source.len() > values.len() {
        values
            .try_reserve_exact(source.len() - values.len())
            .map_err(|_| MeSessionError::Allocation {
                context,
                entries: source.len(),
            })?;
    }
    values.clear();
    values.extend_from_slice(source);
    Ok(())
}

fn require_indicator_width(expected: usize, indicators: &[f64]) -> Result<(), MeSessionError> {
    if indicators.len() != expected {
        return Err(MeSessionError::Contract {
            reason: format!(
                "the component returned {} event indicators for an inventory of {expected}",
                indicators.len()
            ),
        });
    }
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

/// Refine every changed indicator inside the first bracket and apply the
/// earliest domain change observable at the checked policy resolution,
/// together with every change inside the plan's tolerance window.
///
/// The winning indicator's own refined left coordinate is retained:
/// event-left evidence is never moved back to the coarse bracket's lower point,
/// which can be a whole scan resolution earlier.
fn refine_bracket<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    lower: &ScanSample,
    upper: &ScanSample,
) -> Result<MeRootApplication, MeSessionError> {
    let width = policy.state_count();
    let changed = (0..lower.indicators.len())
        .filter(|&index| {
            IndicatorDomain::of(lower.indicators[index])
                != IndicatorDomain::of(upper.indicators[index])
        })
        .collect::<Vec<_>>();
    if changed.is_empty() {
        return Err(MeSessionError::Contract {
            reason: "the scan reported a domain change no indicator refinement could confirm"
                .to_owned(),
        });
    }
    let bracket = refine_earliest(target, policy, lower, upper, &changed)?;
    let left_time = bracket.low();
    let (left_states, left_indicators) = if left_time.to_bits() == lower.time.to_bits() {
        (
            try_copied(&lower.states, "refined left states")?,
            try_copied(&lower.indicators, "refined left indicators")?,
        )
    } else {
        sample_point(target, policy, lower, left_time)?
    };
    let (application_time, application_states, application_indicators) =
        window_application(target, policy, lower, upper, &changed, &bracket)?;
    let left = MeContinuousPoint::new(left_time, left_states, width)?;
    let application = MeContinuousPoint::new(application_time, application_states, width)?;
    MeRootApplication::new(left, application, left_indicators, application_indicators)
}

/// The states and checked indicators of the continuous extension at
/// `coordinate`.
fn sample_point<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    lower: &ScanSample,
    coordinate: f64,
) -> Result<(Vec<f64>, Vec<f64>), MeSessionError> {
    let mut states = try_filled(policy.state_count(), 0.0, "root application sample")?;
    let mut indicators = Vec::new();
    target.sample_states(coordinate, &mut states)?;
    target.indicators_at(coordinate, &states, &mut indicators)?;
    require_indicator_width(lower.indicators.len(), &indicators)?;
    Ok((states, indicators))
}

/// The application coordinate of a located bracket under the plan's
/// `ToleranceWindow` tie-break, with its states and indicators.
///
/// The window end applies every domain change within one location tolerance
/// after the located left limit, and lies within one tolerance after the
/// earliest crossing. It is used when a changed indicator stands in its new
/// domain there; otherwise the earliest one left and returned inside the
/// window, and the located high end, where it had entered, applies.
fn window_application<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    lower: &ScanSample,
    upper: &ScanSample,
    changed: &[usize],
    bracket: &RootBracket,
) -> Result<(f64, Vec<f64>, Vec<f64>), MeSessionError> {
    let at = |target: &mut T, coordinate: f64| {
        if coordinate.to_bits() == upper.time.to_bits() {
            Ok((
                try_copied(&upper.states, "application states")?,
                try_copied(&upper.indicators, "application indicators")?,
            ))
        } else {
            sample_point(target, policy, lower, coordinate)
        }
    };
    let window = bracket.application_window(upper.time);
    if window > bracket.high() {
        let (states, indicators) = at(target, window)?;
        let entered = changed.iter().any(|&index| {
            IndicatorDomain::of(indicators[index]) != IndicatorDomain::of(lower.indicators[index])
        });
        if entered {
            return Ok((window, states, indicators));
        }
    }
    let (states, indicators) = at(target, bracket.high())?;
    Ok((bracket.high(), states, indicators))
}

/// Narrow the bracket toward the least coordinate at which any `changed`
/// indicator has left its domain at `lower`, by the component's refinement
/// method ([`RootBracket`]).
///
/// One evaluation decides every changed indicator at once: the bracket's high
/// end moves whenever any of them has already entered its new domain, so with
/// one crossing per indicator the located bracket holds the earliest one. The
/// trial point is the least Illinois secant crossing over the indicators that
/// have entered at the high end, which only affects how fast the bracket
/// narrows, never which side of a crossing an end lies on.
///
/// Exhausting the host-owned iteration budget without attaining the checked
/// location tolerance is the typed `RootApplicationUnavailable` failure
/// SPEC_0044 §6 requires, not a plausible application.
fn refine_earliest<T: RootScanTarget>(
    target: &mut T,
    policy: &MeRootSearchPolicy,
    lower: &ScanSample,
    upper: &ScanSample,
    changed: &[usize],
) -> Result<RootBracket, MeSessionError> {
    let width = policy.state_count();
    let mut bracket =
        policy
            .root_location
            .open_bracket(lower.time, upper.time, policy.location_tolerance());
    let mut states = try_filled(width, 0.0, "root refinement sample")?;
    let mut indicators = Vec::new();
    let mut at_low = try_copied(&lower.indicators, "refinement low indicators")?;
    let mut at_high = try_copied(&upper.indicators, "refinement high indicators")?;
    let entered = |values: &[f64], index: usize| {
        IndicatorDomain::of(values[index]) != IndicatorDomain::of(lower.indicators[index])
    };
    for _ in 0..policy.root_location.refinement_iteration_cap() {
        target.check_budget()?;
        if bracket.is_located() {
            return Ok(bracket);
        }
        let least = changed
            .iter()
            .filter(|&&index| entered(&at_high, index))
            .map(|&index| bracket.crossing_fraction(at_low[index], at_high[index]))
            .fold(1.0, f64::min);
        // No representable coordinate strictly inside: the location tolerance
        // is attained as far as f64 allows.
        let Some(trial) = bracket.trial(least) else {
            return Ok(bracket);
        };
        target.sample_states(trial, &mut states)?;
        target.indicators_at(trial, &states, &mut indicators)?;
        require_indicator_width(lower.indicators.len(), &indicators)?;
        let any_entered = changed.iter().any(|&index| entered(&indicators, index));
        bracket.narrow(trial, any_entered);
        if any_entered {
            at_high.copy_from_slice(&indicators);
        } else {
            at_low.copy_from_slice(&indicators);
        }
    }
    if bracket.is_located() {
        return Ok(bracket);
    }
    Err(MeSessionError::RootApplicationUnavailable {
        time: bracket.high(),
        reason: format!(
            "indicators {changed:?} were not localized to {} within {} \
             refinements; the bracket is still [{}, {}]",
            policy.location_tolerance(),
            policy.root_location.refinement_iteration_cap(),
            bracket.low(),
            bracket.high()
        ),
    })
}

/// `2^53`: the largest integer whose `f64` image is exact, hence the largest
/// scan-step count the host can both index as a `u64` and divide as an `f64`
/// without losing a coordinate. A request beyond it is a typed resource
/// failure, never a coarser scan.
const EXACT_INTEGER_LIMIT: f64 = 9_007_199_254_740_992.0;

#[cfg(test)]
mod tests {
    use super::*;

    fn policy() -> MeRootSearchPolicy {
        MeRootSearchPolicy::new(
            0.1,
            1.0e-9,
            1.0e-8,
            1.0e-6,
            vec![1.0],
            1,
            RootLocationPlan::STANDARD,
        )
        .expect("fixture policy is checked")
    }

    fn point(time: f64, state: f64) -> MeContinuousPoint {
        MeContinuousPoint::new(time, vec![state], 1).expect("fixture point is checked")
    }

    /// `x(t) = t - 0.25`, one indicator per offset (by default the state
    /// itself).
    struct LinearCrossing {
        indicator_calls: usize,
        offsets: Vec<f64>,
        /// Each indicator is `slope * d + curvature * d * d` for
        /// `d = x - offset`.
        slope: f64,
        curvature: f64,
    }

    impl LinearCrossing {
        fn new() -> Self {
            Self::with_offsets(vec![0.0])
        }

        fn with_offsets(offsets: Vec<f64>) -> Self {
            Self {
                indicator_calls: 0,
                offsets,
                slope: 1.0,
                curvature: 0.0,
            }
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
            indicators: &mut Vec<f64>,
        ) -> Result<(), MeSessionError> {
            self.indicator_calls += 1;
            indicators.clear();
            indicators.extend(self.offsets.iter().map(|offset| {
                let distance = states[0] - offset;
                self.slope * distance + self.curvature * distance * distance
            }));
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
        let proposal = MeStepProposal::bind(request, candidate, previous.width())?;
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
        assert!(
            MeRootSearchPolicy::new(
                0.1,
                1.0e-9,
                1.0e-8,
                1.0e-6,
                vec![0.0],
                1,
                RootLocationPlan::STANDARD
            )
            .is_err()
        );
        assert!(
            MeRootSearchPolicy::new(
                0.1,
                1.0e-9,
                1.0e-8,
                1.0e-6,
                vec![f64::INFINITY],
                1,
                RootLocationPlan::STANDARD
            )
            .is_err()
        );
        assert!(
            MeRootSearchPolicy::new(
                0.1,
                1.0e-9,
                1.0e-8,
                1.0e-6,
                Vec::new(),
                1,
                RootLocationPlan::STANDARD
            )
            .is_err()
        );
        assert!(
            MeRootSearchPolicy::new(
                0.1,
                1.0e-9,
                1.0e-8,
                1.0e-6,
                vec![1.0, 1.0],
                1,
                RootLocationPlan::STANDARD
            )
            .is_err()
        );
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

    #[test]
    fn simultaneous_changes_share_one_refinement_of_the_earliest_crossing() {
        let mut alone = LinearCrossing::new();
        let single = scan(&mut alone, &point(0.0, -0.25), &point(1.0, 0.75), &[-0.25])
            .expect("the scan succeeds")
            .expect("a crossing exists inside the interval");
        // Six indicators cross inside the same scan bracket [0.2, 0.3], the
        // first one exactly where the single indicator does.
        let offsets = vec![0.0, 0.01, 0.02, 0.03, 0.04, 0.045];
        let retained = offsets
            .iter()
            .map(|offset| -0.25 - offset)
            .collect::<Vec<_>>();
        let mut many = LinearCrossing::with_offsets(offsets);
        let joint = scan(&mut many, &point(0.0, -0.25), &point(1.0, 0.75), &retained)
            .expect("the scan succeeds")
            .expect("a crossing exists inside the interval");
        assert_eq!(
            joint.application().time().to_bits(),
            single.application().time().to_bits()
        );
        assert_eq!(
            joint.left().time().to_bits(),
            single.left().time().to_bits()
        );
        assert_eq!(
            many.indicator_calls, alone.indicator_calls,
            "every simultaneous change is decided by the same evaluations"
        );
    }

    #[test]
    fn the_refined_left_limit_is_retained_rather_than_the_coarse_bracket() {
        let mut target = LinearCrossing::new();
        let application = scan(&mut target, &point(0.0, -0.25), &point(1.0, 0.75), &[-0.25])
            .expect("the scan succeeds")
            .expect("a crossing exists inside the interval");
        let left = application.left().time();
        let applied = application.application().time();
        // The application is the window end: one tolerance after the left
        // limit, rounded to the nearest coordinate.
        assert!(applied - left <= policy().location_tolerance() + f64::EPSILON * applied);
        assert!(
            left > 0.2,
            "the coarse bracket lower point 0.2 must not become the event-left evidence, got \
             {left}"
        );
    }

    /// A smooth curved indicator is located in a few refreshes; bisection of
    /// the same bracket to the same tolerance takes
    /// `ceil(log2(0.1 / 1e-9)) = 27`.
    #[test]
    fn a_smooth_crossing_is_located_in_far_fewer_refreshes_than_bisection() {
        let mut target = LinearCrossing::new();
        target.curvature = 3.0;
        let application = scan(&mut target, &point(0.0, -0.25), &point(1.0, 0.75), &[-0.25])
            .expect("the scan succeeds")
            .expect("a crossing exists inside the interval");
        assert!(application.left().time() <= 0.25 && 0.25 <= application.application().time());
        let applied = application.application().time();
        assert!(applied - application.left().time() <= 1.0e-9 + f64::EPSILON * applied);
        // Three scan samples up to the bracket [0.2, 0.3], then the
        // refinement, then the refined left and application coordinates.
        let refreshes = target.indicator_calls - 3 - 2;
        assert!(refreshes <= 8, "{refreshes} refinement refreshes");
    }

    /// An indicator that reaches zero exactly on a scan coordinate is applied
    /// there, as bisection applies it.
    #[test]
    fn a_crossing_on_the_scan_coordinate_is_applied_there() {
        let grid = ScanGrid::new(0.0, 1.0, 0.1).expect("a representable grid");
        let mut coordinate = 0.0;
        for step in 1..=3 {
            coordinate = grid
                .coordinate(step, coordinate)
                .expect("a representable coordinate");
        }
        // A decreasing indicator that is exactly zero, the non-positive
        // domain, at the third scan coordinate.
        let offset = coordinate - 0.25;
        let mut target = LinearCrossing::with_offsets(vec![offset]);
        target.slope = -1.0;
        let application = scan(
            &mut target,
            &point(0.0, -0.25),
            &point(1.0, 0.75),
            &[0.25 + offset],
        )
        .expect("the scan succeeds")
        .expect("a crossing exists inside the interval");
        assert_eq!(
            application.application().time().to_bits(),
            coordinate.to_bits()
        );
    }

    /// The scan sample of `target` at `time`.
    fn sample(target: &mut LinearCrossing, time: f64) -> ScanSample {
        let mut states = vec![0.0];
        let mut indicators = Vec::new();
        target.sample_states(time, &mut states).expect("a sample");
        target
            .indicators_at(time, &states, &mut indicators)
            .expect("indicators");
        ScanSample {
            time,
            states,
            indicators,
        }
    }

    /// Apply the located bracket `[low, high]` of the scan bracket
    /// `[0.2, 0.3]` under the tolerance window; returns the application
    /// coordinate and its indicators.
    fn apply(target: &mut LinearCrossing, low: f64, high: f64) -> (f64, Vec<f64>) {
        let policy = policy();
        let lower = sample(target, 0.2);
        let upper = sample(target, 0.3);
        let changed = (0..lower.indicators.len())
            .filter(|&k| {
                IndicatorDomain::of(lower.indicators[k]) != IndicatorDomain::of(upper.indicators[k])
            })
            .collect::<Vec<_>>();
        let bracket =
            RootLocationPlan::STANDARD.open_bracket(low, high, policy.location_tolerance());
        assert!(bracket.is_located());
        let (time, _, indicators) =
            window_application(target, &policy, &lower, &upper, &changed, &bracket)
                .expect("the window applies");
        (time, indicators)
    }

    /// Two indicators crossing within the tolerance window apply together,
    /// whether the located bracket sits tight against the earliest crossing
    /// (an Illinois-shaped bracket) or spans the tolerance around it (a
    /// bisection-shaped one).
    #[test]
    fn changes_within_the_tolerance_window_apply_together() {
        let tolerance = policy().location_tolerance();
        for (low, high) in [
            (0.25 - 0.01 * tolerance, 0.25 + 0.001 * tolerance),
            (0.25 - 0.1 * tolerance, 0.25 + 0.4 * tolerance),
        ] {
            // The second crossing lies past either high end, inside the window.
            let mut target = LinearCrossing::with_offsets(vec![0.0, 0.6 * tolerance]);
            let (time, indicators) = apply(&mut target, low, high);
            assert!(time - low <= tolerance + f64::EPSILON * time);
            assert!(
                indicators.iter().all(|&value| value > 0.0),
                "both changes apply at {time}: {indicators:?} for [{low}, {high}]"
            );
        }
    }

    /// A change beyond the window stays its own event: the earliest crossing
    /// applies at the window end without it.
    #[test]
    fn a_change_beyond_the_tolerance_window_stays_separate() {
        let tolerance = policy().location_tolerance();
        let low = 0.25 - 0.5 * tolerance;
        let mut target = LinearCrossing::with_offsets(vec![0.0, 1.5 * tolerance]);
        let (time, indicators) = apply(&mut target, low, 0.25 + 0.2 * tolerance);
        assert_eq!(time.to_bits(), (low + tolerance).to_bits());
        assert!(
            indicators[0] > 0.0 && indicators[1] <= 0.0,
            "{indicators:?}"
        );
    }

    /// An earliest crossing that returns inside the window applies at the
    /// located high end, where it had entered.
    #[test]
    fn a_change_that_returns_inside_the_window_applies_at_the_high_end() {
        let tolerance = policy().location_tolerance();
        // Positive only in (0.25, 0.25 + tolerance / 2).
        let mut target = LinearCrossing::new();
        target.slope = 0.5 * tolerance;
        target.curvature = -1.0;
        let high = 0.25 + 0.1 * tolerance;
        let (time, indicators) = apply(&mut target, 0.25 - 0.01 * tolerance, high);
        assert_eq!(time.to_bits(), high.to_bits());
        assert!(indicators[0] > 0.0);
    }

    /// Two indicators crossing 0.7 of a tolerance apart, as the
    /// thyristor firing conditions of a rectifier bridge do, are located and
    /// applied as one event.
    #[test]
    fn nearly_simultaneous_crossings_are_one_event() {
        let tolerance = policy().location_tolerance();
        let mut target = LinearCrossing::with_offsets(vec![0.0, 0.7 * tolerance]);
        let retained = [-0.25, -0.25 - 0.7 * tolerance];
        let application = scan(
            &mut target,
            &point(0.0, -0.25),
            &point(1.0, 0.75),
            &retained,
        )
        .expect("the scan succeeds")
        .expect("a crossing exists inside the interval");
        let applied = application.application().time();
        assert!(applied >= 0.25 + 0.7 * tolerance, "{applied}");
        assert!(applied - application.left().time() <= tolerance + f64::EPSILON * applied);
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
