//! Deferred-step yield admission for live observations.

use super::{DeferredCrossing, accepted_step_roundoff};

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
