//! What the tests of this crate observe of a session's internal state.

use super::*;

impl MeSimulationSession<'_, '_> {
    /// The session's own accepted point, bit-exact.
    pub(in crate::fmi_me) fn verification_session_point(&self) -> (u64, Vec<u64>) {
        (
            self.host.time.to_bits(),
            self.host
                .states
                .iter()
                .map(|value| value.to_bits())
                .collect(),
        )
    }

    /// The coordinate the *component* is standing on, bit-exact.
    ///
    /// ME-BUF-001 makes this and [`Self::verification_session_point`] one fact;
    /// an ablation asserts they agree after every failure
    pub(in crate::fmi_me) fn verification_component_point(&self) -> (u64, Vec<u64>) {
        let (_, time, states, _) = self
            .host
            .kernel
            .borrow()
            .expect("verification requires an active session")
            .verification_observable_state();
        (time, states)
    }

    /// Invert every indicator domain the component froze, leaving it standing
    /// where it is.
    pub(in crate::fmi_me) fn verification_invert_frozen_indicator_domains(&self) {
        self.host
            .kernel
            .borrow_mut()
            .expect("verification requires an active session")
            .verification_invert_frozen_indicator_domains();
    }

    /// The rollback-free continuous write of declared input `name` at the
    /// session's point, as a changed frame would attempt it.
    pub(in crate::fmi_me) fn verification_unguarded_continuous_write(
        &self,
        name: &str,
        value: f64,
    ) -> crate::fmi_me::kernel::UnguardedContinuousWrite {
        let references = self
            .input_references(std::iter::once(name))
            .expect("a declared input");
        self.host
            .kernel
            .borrow_mut()
            .expect("verification requires an active session")
            .write_continuous_without_rollback(
                crate::fmi_me::MeTime::at(self.host.time),
                &self.host.states,
                &references,
                &[value],
            )
            .expect("the write is admissible")
    }

    /// The component's complete checkpoint, as the input rollback owner takes
    /// it, and the host's applied inputs.
    pub(in crate::fmi_me) fn verification_checkpoint(
        &self,
    ) -> (crate::fmi_me::MeFmuState, Vec<(String, u64)>) {
        let checkpoint = self
            .host
            .kernel
            .borrow()
            .expect("verification requires an active session")
            .fmu_state()
            .expect("the checkpoint is taken");
        let inputs = self
            .host
            .inputs
            .iter()
            .map(|(name, value)| (name.clone(), value.to_bits()))
            .collect();
        (checkpoint, inputs)
    }

    /// Whether the component and the host's applied inputs still hold
    /// `checkpoint` bit for bit.
    pub(in crate::fmi_me) fn verification_matches_checkpoint(
        &self,
        (checkpoint, inputs): &(crate::fmi_me::MeFmuState, Vec<(String, u64)>),
    ) -> bool {
        self.host
            .kernel
            .borrow()
            .expect("verification requires an active session")
            .verification_matches_snapshot(checkpoint)
            && self
                .host
                .inputs
                .iter()
                .map(|(name, value)| (name.clone(), value.to_bits()))
                .eq(inputs.iter().cloned())
    }

    /// Whether the component and not just the host flag reached FMI
    /// Terminated.  This is deliberately a boolean: the private lifecycle type
    /// does not become part of the session API merely to support an ablation.
    pub(in crate::fmi_me) fn verification_component_is_terminated(&self) -> bool {
        self.host
            .kernel
            .borrow()
            .expect("verification requires an active session")
            .verification_observable_state()
            .0
            == crate::fmi_me::lifecycle::MeState::Terminated
    }

    /// Expire the wall budget now without sleeping: a zero-length deadline is
    /// already reached at every later check.
    pub(in crate::fmi_me) fn verification_expire_budget(&mut self) {
        self.host.budget = TimeoutBudget::new(Some(f64::MIN_POSITIVE));
    }

    /// Whether the retained derivative capability is currently reachable.
    pub(in crate::fmi_me) fn verification_capability_is_active(&self) -> bool {
        self.host.derivatives.is_active()
    }

    /// Issue `fmi3Terminate` on the component underneath the session.
    ///
    /// `fmi3Terminate` is an ordinary standard call; the master algorithm simply
    /// never issues it mid-run, so this is the only way to reach the cases where
    /// an excursion's own body *and* its restoration both fail. It moves nothing
    /// and decides nothing: every later operation an ablation drives is a
    /// production one.
    pub(in crate::fmi_me) fn verification_terminate_component(&mut self) {
        self.host
            .kernel
            .borrow_mut()
            .expect("verification requires an active session")
            .terminate()
            .expect("fmi3Terminate is legal once");
    }

    /// Run one off-point observation against a component that has been
    /// terminated underneath the session.
    ///
    /// The excursion is the production one — nothing here is a test-only branch
    /// inside the master algorithm.
    pub(in crate::fmi_me) fn verification_observe_off_point_after_terminate(
        &mut self,
        time: f64,
        states: &[f64],
    ) -> MeSessionError {
        self.verification_terminate_component();
        self.host
            .observe_off_point(time, states)
            .expect_err("a terminated component refuses the observation and the restoration")
    }
}
