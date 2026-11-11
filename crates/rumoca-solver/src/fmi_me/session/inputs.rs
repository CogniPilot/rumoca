//! Input transactions belong to the common master (SPEC_0044 ME-HOST-001/ME-BUF-001).

use super::host_state::{close_excursion, update_discrete_states_to_completion};
use super::{MeSessionError, MeSessionLoss, MeSimulationSession};
use crate::fmi_me::kernel::AcceptedInputWrite;
use crate::fmi_me::{MeError, MeEventCause, MeTime, MeValueRef};

impl MeSimulationSession<'_, '_> {
    /// Apply one declared input through the same transaction as a batch.
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), MeSessionError> {
        self.set_inputs(&[(name, value)])
    }

    /// Atomically apply declared inputs and correlate retained history once.
    ///
    /// Unknown names and the component's invalid-value checks reject the whole
    /// batch without changing inputs. Bit-identical writes preserve numerical
    /// history. A failure after the component commits ends the session, because
    /// no consumer may use owners that disagree about the accepted point.
    /// Repeated names retain the component's source-ordered last-write behavior.
    ///
    /// A batch whose changed inputs reach no event owner in this instant, at a
    /// coordinate where no time event is due, stays in Continuous-Time Mode:
    /// the component writes it as an input event's exit would leave it, and
    /// the plugin history restarts once, as after that event.
    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), MeSessionError> {
        self.require_live()?;
        let references = self.input_references(inputs)?;
        let values: Vec<f64> = inputs.iter().map(|(_, value)| *value).collect();
        let mut current = vec![0.0; references.len()];
        self.host
            .kernel
            .borrow()?
            .get_float64(&references, &mut current)?;
        if current
            .iter()
            .zip(&values)
            .all(|(before, after)| before.to_bits() == after.to_bits())
        {
            self.record_inputs(inputs);
            return Ok(());
        }
        // A changed input is a discontinuity at the caller's coordinate: a
        // deferred live yield (issue #513) becomes a real step boundary first,
        // but only for a batch the component admits; a refused one changes nothing.
        self.host
            .kernel
            .borrow()?
            .check_float64_inputs(&references, &values)?;
        self.truncate_deferred_at_observation()?;
        self.clear_snapped_yield();
        let changed = references
            .iter()
            .zip(current.iter().zip(&values))
            .filter(|(_, (before, after))| before.to_bits() != after.to_bits())
            .map(|(reference, _)| reference);
        let continuous = self
            .host
            .kernel
            .borrow()?
            .admits_continuous_input_write(changed, self.host.time);
        if continuous && self.write_continuous_inputs(&references, &values)? {
            #[cfg(any(test, feature = "input-write-audit"))]
            super::input_write_audit::count_input_write(true);
            self.record_inputs(inputs);
            let restarted = self.restart_plugin_history();
            return self
                .host
                .guard_mutation(MeSessionLoss::InputApplication, restarted);
        }
        #[cfg(any(test, feature = "input-write-audit"))]
        super::input_write_audit::count_input_write(false);
        self.write_inputs(&references, &values)?;
        let correlated = self.correlate_inputs(inputs);
        self.host
            .guard_mutation(MeSessionLoss::InputApplication, correlated)
    }

    fn input_references(&self, inputs: &[(&str, f64)]) -> Result<Vec<MeValueRef>, MeSessionError> {
        let kernel = self.host.kernel.borrow()?;
        inputs
            .iter()
            .map(|(name, _)| {
                kernel
                    .input_value_reference(name)
                    .ok_or_else(|| MeSessionError::Contract {
                        reason: format!("'{name}' is not a declared input of this component"),
                    })?
                    .ok_or_else(|| MeSessionError::Contract {
                        reason: format!("declared input '{name}' has no value reference"),
                    })
            })
            .collect()
    }

    fn write_inputs(
        &mut self,
        references: &[MeValueRef],
        values: &[f64],
    ) -> Result<(), MeSessionError> {
        self.write_input_transaction(|kernel| kernel.set_float64(references, values))
    }

    /// Write inputs in Continuous-Time Mode; `false`, with nothing written,
    /// when the component needs the input event after all.
    fn write_continuous_inputs(
        &self,
        references: &[MeValueRef],
        values: &[f64],
    ) -> Result<bool, MeSessionError> {
        self.input_rollback(|scope| {
            scope.write_continuous(
                MeTime::at(self.host.time),
                &self.host.states,
                references,
                values,
            )
        })
    }

    pub(super) fn write_input_transaction(
        &mut self,
        write: impl FnOnce(&mut AcceptedInputWrite<'_>) -> Result<(), MeError>,
    ) -> Result<(), MeSessionError> {
        let entry = self
            .host
            .event_entry(MeEventCause::InputEvent, self.host.time);
        self.input_rollback(|scope| {
            scope.position(MeTime::at(self.host.time), &self.host.states, entry)?;
            write(scope)
        })
    }

    /// Run one component input write under the component's rollback owner.
    fn input_rollback<T>(
        &self,
        write: impl FnOnce(&mut AcceptedInputWrite<'_>) -> Result<T, MeError>,
    ) -> Result<T, MeSessionError> {
        let result = {
            let mut kernel = self.host.kernel.borrow_mut()?;
            match kernel.with_input_rollback(write) {
                Ok(written) => Ok(written),
                Err((error, restoration)) => {
                    close_excursion(self.host.time, restoration, Err(error.into()))
                }
            }
        };
        if matches!(result, Err(MeSessionError::AcceptedPointLost { .. })) {
            self.host
                .guard_mutation(MeSessionLoss::InputApplication, result)
        } else {
            result
        }
    }

    fn record_inputs(&mut self, inputs: &[(&str, f64)]) {
        self.host.inputs.extend(
            inputs
                .iter()
                .map(|(name, value)| ((*name).to_owned(), *value)),
        );
    }

    fn correlate_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), MeSessionError> {
        self.record_inputs(inputs);
        self.correlate_input_write()
    }

    pub(super) fn correlate_input_write(&mut self) -> Result<(), MeSessionError> {
        let discrete = update_discrete_states_to_completion(
            &mut *self.host.kernel.borrow_mut()?,
            self.host.time,
        )?;
        // A setter does not request a trace row. The next explicit observation
        // or advance owns publication, including repeated batches at one time.
        self.complete_event_mode(discrete, self.host.time, false)
    }
}
