//! Deferred-step yield admission for live observations.

use super::{DeferredCrossing, MeSessionError, MeSimulationSession, accepted_step_roundoff};

const LIVE_OBSERVATION_HORIZON_S: f64 = 1.0e6;

impl MeSimulationSession<'_, '_> {
    /// The natural lookahead of an open session is independent of poll cadence.
    /// A time-only step has no numerical state or error control. With monitored
    /// indicators, its lookahead needs only one interval of the checked scan.
    pub(super) fn deferred_observation_bound(&self) -> Result<f64, MeSessionError> {
        let start = self.host.time;
        if self.host.state_count != 0 || self.host.retained_indicators.is_empty() {
            return Ok(start + LIVE_OBSERVATION_HORIZON_S);
        }

        let resolution = self.host.options.root_scan_resolution();
        let duration = resolution.min(LIVE_OBSERVATION_HORIZON_S);
        let end = start + duration;
        if end - start <= accepted_step_roundoff(start, duration) {
            return Err(MeSessionError::RootScanUnrepresentable {
                start,
                end,
                resolution,
            });
        }
        Ok(end)
    }
}

impl DeferredCrossing {
    /// Whether `time` lies strictly inside this step and before any located
    /// root, so it can be served without committing anything. A yield within
    /// roundoff of the step end coincides with it, as in the master loop: the
    /// step is committed (with any event there) and the yield is snapped.
    pub(super) fn serves(&self, time: f64) -> bool {
        let start = self.step.previous().time();
        let end = self.step.accepted().time();
        let roundoff = accepted_step_roundoff(end, end - start);
        time - start > -roundoff
            && end - time > roundoff
            && self
                .located
                .as_ref()
                .is_none_or(|root| root.application().time() - time > roundoff)
    }
}

impl MeSimulationSession<'_, '_> {
    /// The coordinate an output read observes when the caller does not stand
    /// on the accepted point: a deferred live yield, else a snapped one.
    pub(super) fn off_point_observation(&self) -> Option<(f64, &[f64])> {
        match (&self.deferred, self.snapped_yield) {
            (Some(deferred), _) => {
                Some((deferred.observed_time, deferred.observed_states.as_slice()))
            }
            (None, Some(snapped)) => Some((snapped, self.host.states.as_slice())),
            (None, None) => None,
        }
    }
}
