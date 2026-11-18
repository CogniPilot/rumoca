//! Borrowed write scope issued only inside the same kernel's rollback owner.

use super::*;

pub(in crate::fmi_me) struct AcceptedInputWrite<'kernel> {
    kernel: &'kernel mut SolveMeKernel,
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
    /// no time event is due at `time` (the input event would consume it), the
    /// model has no delay channels (Event Mode holds their history) and no
    /// retained state constraints (an event projects the states onto them),
    /// and no native stateless transport owns the event boundary. The indicator
    /// domains are checked at the write itself
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
                .scheduled_event_coincidence_at(time)
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
        Ok(
            self.frozen_indicator_positive.len() == self.indicator_value_scratch.len()
                && self
                    .frozen_indicator_positive
                    .iter()
                    .zip(&self.indicator_value_scratch)
                    .all(|(frozen, indicator)| *frozen == (*indicator > 0.0)),
        )
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
    /// written and every value cache cleared, scheduled root relation memory
    /// cleared, and the committed seed restarted from the current coordinate,
    /// which drops the derivative it resolved under the old inputs.
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
