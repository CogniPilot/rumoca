//! Borrowed write scope issued only inside the same kernel's rollback owner.

use super::*;

pub(in crate::fmi_me) struct AcceptedInputWrite<'kernel> {
    kernel: &'kernel mut SolveMeKernel,
}

/// What [`SolveMeKernel::write_continuous_without_rollback`] did.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::fmi_me) enum UnguardedContinuousWrite {
    /// The inputs are written as the input event's exit would leave them.
    Written,
    /// An indicator left its frozen domain: the write needs the input event.
    /// Nothing was changed.
    NeedsInputEvent,
    /// Nothing after the first mutation could be shown infallible, or the
    /// indicator check itself failed. Nothing was changed; the write belongs
    /// under the rollback owner.
    NotProven,
}

impl SolveMeKernel {
    pub(in crate::fmi_me) fn with_input_rollback<T>(
        &mut self,
        write: impl FnOnce(&mut AcceptedInputWrite<'_>) -> Result<T, MeError>,
    ) -> Result<T, (MeError, Result<(), MeError>)> {
        let checkpoint = self.fmu_state().map_err(|error| (error, Ok(())))?;
        let attempted = write(&mut AcceptedInputWrite { kernel: self });
        match attempted {
            Ok(written) => Ok(written),
            Err(error) => {
                #[cfg(test)]
                if FAIL_RESTORE.with(|fault| fault.replace(false)) {
                    return Err((error, Err(contract("injected input restoration failure"))));
                }
                Err((error, self.reset_to_fmu_state(&checkpoint)))
            }
        }
    }

    /// Whether a write of the changed `references` at `time` may stay in
    /// Continuous-Time Mode instead of running an input event.
    ///
    /// It may when no changed input reaches an event owner in the same
    /// instant (`InputEventReach`, derived from this model's refresh owners),
    /// no time event is still due at `time` (an unconsumed scheduled
    /// coincidence or the announced pending stop; the input event would
    /// consume it), the model has no delay channels (Event Mode holds their
    /// history) and no retained state constraints (an event projects the
    /// states onto them), and no native stateless transport owns the event
    /// boundary. A scheduled instant already processed does not refuse: the
    /// schedule consumes every coincident event of an instant at once, and an
    /// input event there runs only the rows no schedule owns, as at any other
    /// instant. The indicator domains are checked at the write itself
    /// ([`AcceptedInputWrite::write_continuous`]).
    pub(in crate::fmi_me) fn admits_continuous_input_write<'reference>(
        &self,
        references: impl IntoIterator<Item = &'reference MeValueRef>,
        time: f64,
    ) -> bool {
        #[cfg(any(test, feature = "input-write-audit"))]
        if crate::fmi_me::session::input_write_audit::input_event_forced() {
            return false;
        }
        #[cfg(feature = "native-wasm")]
        if self.native_transport.is_some() {
            return false;
        }
        if self.lifecycle.state() != MeState::ContinuousTimeMode
            || self.runtime.has_delay_channels()
            || self.runtime.requires_state_manifold_projection()
            || self
                .stop_schedule
                .unconsumed_coincident_event_at(time)
                .is_some()
            || self
                .pending_event_stop
                .is_some_and(|(stop, _)| time_match_with_tol(stop, time))
        {
            return false;
        }
        let first_input = self.runtime.model().problem.solve_layout.parameter_count;
        let reach = self.runtime.input_event_reach();
        references
            .into_iter()
            .all(|reference| match reference.backing {
                MeFloat64Backing::InputParameter(index) => index
                    .checked_sub(first_input)
                    .is_some_and(|input| !reach.reaches(input)),
                MeFloat64Backing::MaxStepDuration(_) => false,
            })
    }

    /// Derive the input event reach
    /// [`Self::admits_continuous_input_write`] consults now, so no later write
    /// derives it.
    pub(in crate::fmi_me) fn derive_input_event_reach(&self) {
        self.runtime.input_event_reach();
    }

    /// [`AcceptedInputWrite::write_continuous`] without a restoration point,
    /// for a write [`Self::admits_continuous_input_write`] admitted.
    ///
    /// Every check runs before the first mutation, and the write is taken only
    /// where nothing after that mutation can fail: no scheduled root relation
    /// memory to clear (its slots are checked while it clears them), no delay
    /// point to commit and no native coordinates to synchronize. Indicators
    /// are checked first, without writing anything, which needs the kernel to
    /// stand already at the write's coordinate and a refresh whose settled
    /// bits no warm start can change.
    pub(in crate::fmi_me) fn write_continuous_without_rollback(
        &mut self,
        time: MeTime,
        states: &[f64],
        references: &[MeValueRef],
        values: &[f64],
    ) -> Result<UnguardedContinuousWrite, MeError> {
        use UnguardedContinuousWrite::{NeedsInputEvent, NotProven, Written};
        #[cfg(feature = "native-wasm")]
        if self.native_transport.is_some() {
            return Ok(NotProven);
        }
        let events = &self.runtime.model().problem.events;
        if self.runtime.has_delay_channels() || !events.scheduled_root_conditions.is_empty() {
            return Ok(NotProven);
        }
        self.check_time(time)?;
        self.check_continuous_states(states)?;
        let indices = self.admitted_float64_inputs(references, values)?;
        if !self.indicator_plan.is_empty() {
            if !self.stands_at(time, states)
                || !self
                    .runtime
                    .model()
                    .problem
                    .continuous
                    .refresh_owners
                    .algebraic()
                    .is_exact_causal_schedule()
            {
                return Ok(NotProven);
            }
            #[cfg(any(test, feature = "input-write-audit"))]
            let before = self.fmu_state()?;
            let held = self.indicator_domains_hold_without_caching();
            #[cfg(any(test, feature = "input-write-audit"))]
            assert!(
                self.verification_matches_snapshot(&before),
                "the indicator check of a continuous write changed the component"
            );
            match held {
                Ok(true) => {}
                // Defensive: Continuous-Time Mode is entered with the domains
                // seeded at the settled coordinate, every completed step
                // refreezes them or enters Event Mode, and a continuous write
                // changes no input an indicator reads, so a kernel standing at
                // the host point holds its domains.
                Ok(false) => return Ok(NeedsInputEvent),
                Err(_) => return Ok(NotProven),
            }
        }
        self.time = time.time;
        self.event_boundary = time.event_boundary;
        self.states.copy_from_slice(states);
        self.store_float64_inputs(&indices, values);
        self.write_input_event_clock_lanes();
        self.clear_callback_value_caches();
        self.commit_seed();
        Ok(Written)
    }

    /// Whether the kernel stands bit for bit at `time` and `states`, so
    /// positioning it there would change nothing.
    fn stands_at(&self, time: MeTime, states: &[f64]) -> bool {
        self.time.to_bits() == time.time.to_bits()
            && self.event_boundary.map(f64::to_bits) == time.event_boundary.map(f64::to_bits)
            && self.states.len() == states.len()
            && self
                .states
                .iter()
                .zip(states)
                .all(|(current, written)| current.to_bits() == written.to_bits())
    }

    /// The periodic-clock activation lanes an input event at the current time
    /// leaves: its passes tick no clock at an instant whose scheduled event was
    /// already processed, and write the lanes of the current time otherwise.
    fn write_input_event_clock_lanes(&mut self) {
        let row_filter = if self
            .stop_schedule
            .scheduled_event_coincidence_at(self.time)
            .is_some()
        {
            EventUpdateRowFilter::UnownedOnly
        } else {
            EventUpdateRowFilter::All
        };
        row_filter.write_pass_clock_activation_params(
            self.runtime.model(),
            &mut self.params,
            self.time,
        );
    }

    /// Whether every event indicator at the current coordinate still lies in
    /// the domain the component froze, so an input event would latch no
    /// crossing.
    fn indicator_domains_hold(&mut self) -> Result<bool, MeError> {
        if self.indicator_plan.is_empty() {
            return Ok(true);
        }
        let mut indicators = std::mem::take(&mut self.indicator_value_scratch);
        let read = self.event_indicators_into(&mut indicators);
        self.indicator_value_scratch = indicators;
        read?;
        Ok(self.frozen_domains_contain(&self.indicator_value_scratch))
    }

    /// [`Self::indicator_domains_hold`] through
    /// [`Self::event_indicators_without_caching`], which writes nothing.
    fn indicator_domains_hold_without_caching(&self) -> Result<bool, MeError> {
        #[cfg(any(test, feature = "input-write-audit"))]
        if crate::fmi_me::session::input_write_audit::take_poisoned_indicator_check() {
            return Err(contract("injected indicator-check failure"));
        }
        let mut indicators = vec![0.0; self.indicator_plan.len()];
        self.event_indicators_without_caching(&mut indicators)?;
        Ok(self.frozen_domains_contain(&indicators))
    }

    /// Whether `indicators` lie in the domains the component froze.
    fn frozen_domains_contain(&self, indicators: &[f64]) -> bool {
        self.frozen_indicator_positive.len() == indicators.len()
            && self
                .frozen_indicator_positive
                .iter()
                .zip(indicators)
                .all(|(frozen, indicator)| *frozen == (*indicator > 0.0))
    }
}

impl AcceptedInputWrite<'_> {
    pub(in crate::fmi_me) fn position(
        &mut self,
        time: MeTime,
        states: &[f64],
        entry: MeEventEntry,
    ) -> Result<(), MeError> {
        self.kernel.set_time(time)?;
        self.kernel.set_continuous_states(states)?;
        self.kernel.enter_event_mode(entry)
    }

    pub(in crate::fmi_me) fn set_float64(
        &mut self,
        references: &[MeValueRef],
        values: &[f64],
    ) -> Result<(), MeError> {
        self.kernel.set_float64(references, values)
    }

    /// Write inputs no event owner reads at `time` without leaving
    /// Continuous-Time Mode, after
    /// [`SolveMeKernel::admits_continuous_input_write`] admitted them.
    ///
    /// Returns `false`, having written no input, when an indicator left its
    /// frozen domain: the input event would latch that crossing. Otherwise the
    /// component ends where the input event's exit would leave it: the cells
    /// written and every value cache cleared, the clock activation lanes its
    /// passes write, scheduled root relation memory cleared, and the committed
    /// seed restarted from the current coordinate, which drops the derivative
    /// it resolved under the old inputs.
    pub(in crate::fmi_me) fn write_continuous(
        &mut self,
        time: MeTime,
        states: &[f64],
        references: &[MeValueRef],
        values: &[f64],
    ) -> Result<bool, MeError> {
        self.kernel.set_time(time)?;
        self.kernel.set_continuous_states(states)?;
        if !self.kernel.indicator_domains_hold()? {
            return Ok(false);
        }
        self.kernel.set_float64(references, values)?;
        self.kernel.write_input_event_clock_lanes();
        self.kernel.clear_all_scheduled_root_relation_memory()?;
        self.kernel.clear_callback_value_caches();
        self.kernel.commit_seed();
        Ok(true)
    }

    #[cfg(feature = "native-wasm")]
    pub(super) fn kernel(&mut self) -> &mut SolveMeKernel {
        self.kernel
    }

    #[cfg(feature = "native-wasm")]
    pub(in crate::fmi_me) fn set_native_integer(
        &mut self,
        references: &[u32],
        values: &[i64],
    ) -> Result<(), MeError> {
        self.kernel.set_native_integer(references, values)
    }

    #[cfg(feature = "native-wasm")]
    pub(in crate::fmi_me) fn set_boolean(
        &mut self,
        references: &[u32],
        values: &[bool],
    ) -> Result<(), MeError> {
        self.kernel.set_boolean(references, values)
    }
}

#[cfg(test)]
thread_local! {
    static SNAPSHOTS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static FAIL_RESTORE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

#[cfg(test)]
pub(super) fn count_snapshot() {
    SNAPSHOTS.with(|count| count.set(count.get() + 1));
}

#[cfg(all(test, feature = "native-wasm"))]
pub(in crate::fmi_me) fn snapshot_count() -> usize {
    SNAPSHOTS.with(std::cell::Cell::get)
}

#[cfg(test)]
pub(in crate::fmi_me) fn fail_input_restore() {
    FAIL_RESTORE.with(|fault| fault.set(true));
}
