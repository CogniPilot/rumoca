//! The zero-continuous-state numerical plugin (SPEC_0044 §6, ME-ZERO-001).
//!
//! A model with no continuous states still has one FMI component, one session,
//! one lifecycle, and one trace policy. What it does not have is anything to
//! integrate, so this plugin advances and samples empty state and owns no root,
//! event, or output policy at all: the common host scans exactly as it does for
//! every other plugin.
//!
//! It is nonetheless held to the identical accepted-interval contract. It
//! proves that each request starts at the coordinate it was last left on, and
//! it remembers the accepted interval so its sampler rejects a coordinate
//! outside it — the same coverage obligation a state-carrying plugin owes

use super::{
    MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
    MeIntegratorBackend, MeStepCandidate, accepted_interval_contains, accepted_step_roundoff,
};

/// The declared local order of an empty continuous extension.
///
/// The order is vacuous over empty state, but SPEC_0044 §6 requires every
/// accepted step to declare a positive order, and an exact sampler is exact at
/// every order, so the plugin declares the lowest positive one honestly.
const TIME_ONLY_ORDER: u32 = 1;

/// Advances time over an empty continuous state.
#[derive(Debug)]
pub struct TimeOnlyIntegrator {
    /// The closed interval the most recent accepted step covers. Before any
    /// advance this is the degenerate interval at the initialized coordinate.
    interval: Option<(f64, f64)>,
}

impl Default for TimeOnlyIntegrator {
    fn default() -> Self {
        Self::new()
    }
}

impl TimeOnlyIntegrator {
    #[must_use]
    pub fn new() -> Self {
        Self { interval: None }
    }

    fn settle_at(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        require_empty_state(point)?;
        self.interval = Some((point.time(), point.time()));
        Ok(())
    }

    fn current_coordinate(&self) -> Result<f64, MeIntegrationError> {
        self.interval.map(|(_, accepted)| accepted).ok_or_else(|| {
            MeIntegrationError::contract(
                "the time-only plugin was asked to advance before it was initialized",
            )
        })
    }
}

impl MeIntegratorBackend for TimeOnlyIntegrator {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        _derivatives: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        // The retained handle is dropped here: an empty continuous state has no
        // derivative to evaluate, so this plugin proves it never keeps one.
        self.settle_at(point)
    }

    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        require_empty_state(request.current())?;
        let settled = self.current_coordinate()?;
        if settled.to_bits() != request.current().time().to_bits() {
            return Err(MeIntegrationError::contract(format!(
                "the time-only plugin is settled at t={settled} but was asked to advance from \
                 t={}",
                request.current().time()
            )));
        }
        // Every host-issued bound is already strictly later than the current
        // coordinate beyond roundoff, so the earliest of them is the accepted
        // endpoint. A soft observation is not a bound, but an exact plugin
        // lands on it so the host reads a settled coordinate rather than an
        // interpolated one.
        // The plugin lands on the observation only when the checked step
        // keeps it there: beyond roundoff of the current coordinate, so the
        // step makes progress, and beyond roundoff of the bound, which the host
        // would otherwise normalize the endpoint onto. Any other observation
        // lies inside the accepted interval and is sampled from it.
        let mut accepted_time = request.latest_accepted_time();
        let current = request.current().time();
        let beyond_roundoff =
            |from: f64, to: f64| to - from > accepted_step_roundoff(current, to - current);
        if let Some(observation) = request.observation_time()
            && beyond_roundoff(current, observation)
            && beyond_roundoff(observation, accepted_time)
        {
            accepted_time = observation;
        }
        // The plugin lands on exactly one host-issued coordinate: either the
        // request's own reachable bound (already the least of the three) or a
        // soft observation strictly inside it. The host's canonicalization onto
        // the least binding coordinate is therefore the identity here, so the
        // interval this records is bit-for-bit the one the host will sample.
        self.interval = Some((request.current().time(), accepted_time));
        Ok(MeStepCandidate::new(
            accepted_time,
            Vec::new(),
            TIME_ONLY_ORDER,
        ))
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        let Some((previous, accepted)) = self.interval else {
            return Err(MeIntegrationError::contract(
                "the time-only continuous extension has no accepted interval to sample",
            ));
        };
        if !accepted_interval_contains(previous, accepted, time) {
            return Err(MeIntegrationError::contract(format!(
                "t={time} lies outside the time-only plugin's accepted interval \
                 [{previous}, {accepted}]"
            )));
        }
        if !states.is_empty() {
            return Err(MeIntegrationError::contract(format!(
                "the time-only continuous extension cannot fill {} continuous states",
                states.len()
            )));
        }
        Ok(())
    }

    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        self.settle_at(point)
    }
}

fn require_empty_state(point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
    if point.states().is_empty() {
        return Ok(());
    }
    Err(MeIntegrationError::contract(format!(
        "the time-only plugin cannot carry {} continuous states",
        point.width()
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmi_me::integrator::detached_handle;

    fn empty(time: f64) -> MeContinuousPoint {
        MeContinuousPoint::new(time, Vec::new(), 0).expect("empty point is checked")
    }

    #[test]
    fn advancing_lands_on_the_earliest_host_bound() {
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(0.0), detached_handle())
            .expect("initialize");
        let request =
            MeAdvanceRequest::new(empty(0.0), Some(0.5), 2.0, None, None).expect("checked request");
        let candidate = plugin.advance(&request).expect("one accepted step");
        assert!((candidate.accepted_time() - 0.5).abs() <= f64::EPSILON);
        assert!(candidate.accepted_states().is_empty());
    }

    #[test]
    fn a_soft_observation_inside_the_interval_shortens_the_accepted_step() {
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(0.0), detached_handle())
            .expect("initialize");
        let request = MeAdvanceRequest::new(empty(0.0), None, 2.0, Some(0.25), None)
            .expect("checked request");
        let candidate = plugin.advance(&request).expect("one accepted step");
        assert!((candidate.accepted_time() - 0.25).abs() <= f64::EPSILON);
    }

    #[test]
    fn an_observation_within_roundoff_of_the_bound_lands_on_the_bound() {
        // A maximum-duration bound that drifted a few ulps past an output
        // coordinate: the host normalizes an endpoint there onto the bound, so
        // the plugin settles on the bound and the next request starts from it.
        let now = 0.4990000000000004;
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(now), detached_handle())
            .expect("initialize");
        let request = MeAdvanceRequest::new(empty(now), None, 1.0, Some(0.5), Some(0.001))
            .expect("checked request");
        let bound = request.latest_accepted_time();
        assert!(bound > 0.5);
        let candidate = plugin.advance(&request).expect("one accepted step");
        let step = super::super::MeStepProposal::bind(request, candidate, 0)
            .expect("the endpoint is a legal accepted step");
        assert_eq!(step.accepted().time().to_bits(), bound.to_bits());
        let next =
            MeAdvanceRequest::new(empty(bound), None, 1.0, None, None).expect("checked request");
        assert!(plugin.advance(&next).is_ok());
    }

    #[test]
    fn an_observation_within_roundoff_of_the_current_coordinate_is_sampled() {
        // The coordinate drifted a few ulps short of an output coordinate: a
        // step onto the observation would make no progress beyond roundoff,
        // so the plugin lands on the bound and the observation is sampled.
        let now = 1.4199999999999684;
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(now), detached_handle())
            .expect("initialize");
        let request = MeAdvanceRequest::new(empty(now), None, 2.0, Some(1.42), Some(0.01))
            .expect("checked request");
        let bound = request.latest_accepted_time();
        let candidate = plugin.advance(&request).expect("one accepted step");
        let step = super::super::MeStepProposal::bind(request, candidate, 0)
            .expect("the endpoint makes progress");
        assert_eq!(step.accepted().time().to_bits(), bound.to_bits());
        assert!(plugin.sample(1.42, &mut []).is_ok());
    }

    #[test]
    fn the_maximum_step_duration_binds_the_time_only_plugin_too() {
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(1.0), detached_handle())
            .expect("initialize");
        let request = MeAdvanceRequest::new(empty(1.0), None, 9.0, None, Some(0.125))
            .expect("checked request");
        let candidate = plugin.advance(&request).expect("one accepted step");
        assert!((candidate.accepted_time() - 1.125).abs() <= 1.0e-15);
    }

    #[test]
    fn the_plugin_refuses_a_state_carrying_point() {
        let mut plugin = TimeOnlyIntegrator::new();
        let stateful =
            MeContinuousPoint::new(0.0, vec![1.0], 1).expect("state-carrying point is checked");
        assert!(plugin.initialize(&stateful, detached_handle()).is_err());
        assert!(plugin.truncate_reset(&stateful).is_err());
    }

    #[test]
    fn a_request_from_a_coordinate_the_plugin_is_not_settled_on_is_rejected() {
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(0.0), detached_handle())
            .expect("initialize");
        let request =
            MeAdvanceRequest::new(empty(0.5), None, 2.0, None, None).expect("checked request");
        assert!(plugin.advance(&request).is_err());
    }

    #[test]
    fn an_uninitialized_plugin_advances_and_samples_nothing() {
        let mut plugin = TimeOnlyIntegrator::new();
        let request =
            MeAdvanceRequest::new(empty(0.0), None, 1.0, None, None).expect("checked request");
        assert!(plugin.advance(&request).is_err());
        assert!(plugin.sample(0.0, &mut []).is_err());
    }

    #[test]
    fn sampling_obeys_the_accepted_interval_like_every_other_plugin() {
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(0.0), detached_handle())
            .expect("initialize");
        let request =
            MeAdvanceRequest::new(empty(0.0), None, 1.0, None, None).expect("checked request");
        plugin.advance(&request).expect("one accepted step");
        assert!(plugin.sample(0.0, &mut []).is_ok());
        assert!(plugin.sample(0.5, &mut []).is_ok());
        assert!(plugin.sample(1.0, &mut []).is_ok());
        assert!(plugin.sample(1.0_f64.next_up(), &mut []).is_ok());
        assert!(plugin.sample(1.5, &mut []).is_err());
        assert!(plugin.sample(-0.5, &mut []).is_err());
        assert!(plugin.sample(f64::NAN, &mut []).is_err());
    }

    #[test]
    fn the_sampler_refuses_to_fill_a_state_carrying_buffer() {
        let mut plugin = TimeOnlyIntegrator::new();
        plugin
            .initialize(&empty(0.0), detached_handle())
            .expect("initialize");
        let mut states = [1.0];
        assert!(plugin.sample(0.0, &mut states).is_err());
    }
}
