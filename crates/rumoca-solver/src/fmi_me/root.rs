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
) -> Result<MeRootApplication, MeSessionError> {
    let width = policy.state_count();
    let mut winner: Option<RefinedBracket> = None;
    for index in 0..lower.indicators.len() {
        let before = IndicatorDomain::of(lower.indicators[index]);
        let after = IndicatorDomain::of(upper.indicators[index]);
        if before == after {
            continue;
        }
        let refined = refine_indicator(target, policy, lower, upper, index, before)?;
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

    let mut states = try_filled(width, 0.0, "root refinement sample")?;
    let mut indicators = Vec::new();

    let left_states = if winner.left_time.to_bits() == lower.time.to_bits() {
        try_copied(&lower.states, "refined left states")?
    } else {
        target.sample_states(winner.left_time, &mut states)?;
        try_copied(&states, "refined left states")?
    };
    let left_indicators = if winner.left_time.to_bits() == lower.time.to_bits() {
        try_copied(&lower.indicators, "refined left indicators")?
    } else {
        target.indicators_at(winner.left_time, &left_states, &mut indicators)?;
        require_indicator_width(lower.indicators.len(), &indicators)?;
        try_copied(&indicators, "refined left indicators")?
    };

    let application_states = if winner.application_time.to_bits() == upper.time.to_bits() {
        try_copied(&upper.states, "application states")?
    } else {
        target.sample_states(winner.application_time, &mut states)?;
        try_copied(&states, "application states")?
    };
    target.indicators_at(
        winner.application_time,
        &application_states,
        &mut indicators,
    )?;
    require_indicator_width(lower.indicators.len(), &indicators)?;
    let application_indicators = try_copied(&indicators, "application indicators")?;

    let left = MeContinuousPoint::new(winner.left_time, left_states, width)?;
    let application = MeContinuousPoint::new(winner.application_time, application_states, width)?;
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
) -> Result<RefinedBracket, MeSessionError> {
    let width = policy.state_count();
    let mut low = lower.time;
    let mut high = upper.time;
    let mut states = try_filled(width, 0.0, "root bisection sample")?;
    let mut indicators = Vec::new();
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
        target.sample_states(middle, &mut states)?;
        target.indicators_at(middle, &states, &mut indicators)?;
        require_indicator_width(lower.indicators.len(), &indicators)?;
        let Some(value) = indicators.get(index).copied() else {
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

    fn policy() -> MeRootSearchPolicy {
        MeRootSearchPolicy::new(0.1, 1.0e-9, 1.0e-8, 1.0e-6, vec![1.0], 1)
            .expect("fixture policy is checked")
    }

    fn point(time: f64, state: f64) -> MeContinuousPoint {
        MeContinuousPoint::new(time, vec![state], 1).expect("fixture point is checked")
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
            indicators: &mut Vec<f64>,
        ) -> Result<(), MeSessionError> {
            self.indicator_calls += 1;
            indicators.clear();
            indicators.push(states[0]);
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
        assert!(MeRootSearchPolicy::new(0.1, 1.0e-9, 1.0e-8, 1.0e-6, vec![0.0], 1).is_err());
        assert!(
            MeRootSearchPolicy::new(0.1, 1.0e-9, 1.0e-8, 1.0e-6, vec![f64::INFINITY], 1).is_err()
        );
        assert!(MeRootSearchPolicy::new(0.1, 1.0e-9, 1.0e-8, 1.0e-6, Vec::new(), 1).is_err());
        assert!(MeRootSearchPolicy::new(0.1, 1.0e-9, 1.0e-8, 1.0e-6, vec![1.0, 1.0], 1).is_err());
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
