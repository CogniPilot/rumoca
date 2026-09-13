//! Evaluate a retained observation without changing the completed FMU state.

use super::*;

#[cfg(test)]
mod tests;

impl MeHostState {
    pub(in crate::fmi_me::session) fn observe_saved_point(
        &self,
        saved: &crate::fmi_me::MeFmuState,
        time: f64,
        states: &[f64],
    ) -> Result<Vec<f64>, MeSessionError> {
        self.with_saved_component_state(saved, || self.observe_off_point(time, states))
    }

    fn with_saved_component_state<T>(
        &self,
        saved: &crate::fmi_me::MeFmuState,
        body: impl FnOnce() -> Result<T, MeSessionError>,
    ) -> Result<T, MeSessionError> {
        let current = self.kernel.borrow().fmu_state();
        let attempted = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.kernel.borrow_mut().reset_to_fmu_state(saved)?;
            body()
        }));
        let restoration = self.kernel.borrow_mut().reset_to_fmu_state(&current);
        match attempted {
            Ok(attempted) => {
                let closed = close_excursion(self.time, restoration, attempted);
                if matches!(closed, Err(MeSessionError::AcceptedPointLost { .. })) {
                    record_usability_loss(&self.usability, MeSessionLoss::AcceptedPoint);
                }
                closed
            }
            Err(payload) => {
                self.usability.set(Some(unwound_loss(
                    self.usability.get(),
                    restoration.is_ok(),
                )));
                std::panic::resume_unwind(payload);
            }
        }
    }
}
