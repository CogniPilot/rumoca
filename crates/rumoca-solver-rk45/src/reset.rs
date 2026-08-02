use rumoca_solver::fmi_me::{MeFmuState, ModelExchangeKernel};

use super::{Rk45Backend, SimError};

/// The host's own restart point plus the component state (`fmi3GetFMUState`)
/// it was captured with.
#[derive(Clone)]
pub(super) struct Rk45ResetSnapshot {
    time: f64,
    state: Vec<f64>,
    next_step: f64,
    component: MeFmuState,
}

impl Rk45Backend {
    pub(super) fn reset_snapshot(&self) -> Rk45ResetSnapshot {
        Rk45ResetSnapshot {
            time: self.time,
            state: self.state.clone(),
            next_step: self.next_step,
            component: self.kernel.fmu_state(),
        }
    }

    pub(super) fn reset_to_snapshot(
        &mut self,
        snapshot: &Rk45ResetSnapshot,
        t_start: f64,
    ) -> Result<(), SimError> {
        self.time = if t_start.to_bits() == snapshot.time.to_bits() {
            snapshot.time
        } else {
            t_start
        };
        self.state.clone_from(&snapshot.state);
        self.next_step = snapshot.next_step;
        self.termination = None;
        if self.time.to_bits() == snapshot.time.to_bits() {
            self.kernel.reset_to_fmu_state(&snapshot.component)?;
        } else {
            self.kernel
                .restart_from_fmu_state(&snapshot.component, self.time)?;
        }
        Ok(())
    }
}
