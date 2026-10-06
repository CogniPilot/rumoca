//! Input transactions belong to the common master (SPEC_0044 ME-HOST-001/ME-BUF-001).

use super::host_state::{close_excursion, update_discrete_states_to_completion};
use super::{MeSessionError, MeSessionLoss, MeSimulationSession};
use crate::fmi_me::{MeEventCause, MeTime, MeValueRef};

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
    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), MeSessionError> {
        self.require_live()?;
        let references = self.input_references(inputs)?;
        let values: Vec<f64> = inputs.iter().map(|(_, value)| *value).collect();
        let mut current = vec![0.0; references.len()];
        self.host
            .kernel
            .borrow()
            .get_float64(&references, &mut current)?;
        if current
            .iter()
            .zip(&values)
            .all(|(before, after)| before.to_bits() == after.to_bits())
        {
            self.record_inputs(inputs);
            return Ok(());
        }
        self.write_inputs(&references, &values)?;
        let correlated = self.correlate_inputs(inputs);
        self.host
            .guard_mutation(MeSessionLoss::InputApplication, correlated)
    }

    fn input_references(&self, inputs: &[(&str, f64)]) -> Result<Vec<MeValueRef>, MeSessionError> {
        let kernel = self.host.kernel.borrow();
        inputs
            .iter()
            .map(|(name, _)| {
                if !self.has_input(name) {
                    return Err(MeSessionError::Contract {
                        reason: format!("'{name}' is not a declared input of this component"),
                    });
                }
                kernel
                    .value_reference(name)
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
        let entry = self
            .host
            .event_entry(MeEventCause::InputEvent, self.host.time);
        let result = {
            let mut kernel = self.host.kernel.borrow_mut();
            let checkpoint = kernel.fmu_state();
            let written = (|| {
                kernel.set_time(MeTime::at(self.host.time))?;
                kernel.set_continuous_states(&self.host.states)?;
                kernel.enter_event_mode(entry)?;
                kernel.set_float64(references, values)
            })();
            match written {
                Ok(()) => Ok(()),
                Err(error) => close_excursion(
                    self.host.time,
                    kernel.reset_to_fmu_state(&checkpoint),
                    Err(error.into()),
                ),
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
        let discrete = update_discrete_states_to_completion(
            &mut self.host.kernel.borrow_mut(),
            self.host.time,
        )?;
        // A setter does not request a trace row. The next explicit observation
        // or advance owns publication, including repeated batches at one time.
        self.complete_event_mode(discrete, self.host.time, false)
    }
}
