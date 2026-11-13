//! Float64 values of a scalar instance by FMI value reference, read the way
//! [`super::MeSimulationSession::selected_values`] reads names: one observation
//! at the coordinate the session publishes (an off-point excursion is closed
//! back onto the accepted point), the whole checked batch projected from it,
//! and the same single warning report after it.

use std::rc::Rc;

use rumoca_ir_solve::SolveVariableValueKind;

use super::{MeRetainedComponent, MeSessionError, MeSimulationSession};
use crate::fmi_me::{MeError, instance::MeKernelCell};

fn project_checked(kernel: &Rc<MeKernelCell>, references: &[u32]) -> Result<Vec<f64>, MeError> {
    let kernel = kernel.borrow()?;
    let observation = kernel.observe()?;
    let values = kernel.project_scalar_float64(&observation, references)?;
    kernel.report_warnings(&observation)?;
    Ok(values)
}

/// Copy a staged batch out only when it fills the caller's result exactly.
fn deliver(staged: Vec<f64>, values: &mut [f64]) -> Result<(), MeError> {
    if staged.len() != values.len() {
        return Err(MeError::Contract {
            reason: format!(
                "typed FMI batch requires {} values, got {}",
                staged.len(),
                values.len()
            ),
        });
    }
    values.copy_from_slice(&staged);
    Ok(())
}

impl MeSimulationSession<'_, '_> {
    /// Whether this session's component is a scalar product, fixed at construction.
    #[must_use]
    pub fn is_scalar_product(&self) -> bool {
        self.host.kernel.is_scalar()
    }

    /// `fmi3GetFloat64` of a scalar instance after initialization.
    pub fn get_scalar_float64(
        &self,
        references: &[u32],
        values: &mut [f64],
    ) -> Result<(), MeSessionError> {
        self.require_live()?;
        let read = |kernel: &Rc<MeKernelCell>| project_checked(kernel, references);
        let staged = match self.off_point_observation() {
            Some((time, states)) => self.host.anchor().read_settled(time, states, read)?,
            None => read(&self.host.kernel)?,
        };
        Ok(deliver(staged, values)?)
    }

    /// `fmi3SetFloat64` of an executing scalar instance: inputs only, through
    /// the common accepted-input transaction, exactly as native VR writes.
    pub fn set_scalar_float64(
        &mut self,
        references: &[u32],
        values: &[f64],
    ) -> Result<(), MeSessionError> {
        self.require_live()?;
        let (targets, unchanged) = {
            let kernel = self.host.kernel.borrow()?;
            // Causality is checked even when the proposed bits already match.
            let targets = kernel.scalar_float64_targets(references, values.len(), true)?;
            let current = kernel.read_scalar_host_cells(references)?;
            let unchanged = current
                .iter()
                .zip(values)
                .all(|(before, after)| before.to_bits() == after.to_bits());
            (targets, unchanged)
        };
        if unchanged {
            return Ok(());
        }
        self.write_input_transaction(|write| write.set_float64(&targets, values))?;
        // The named-input map is reporting data, never VR input authority.
        self.host.inputs.clear();
        let correlated = self.correlate_input_write();
        self.host
            .guard_mutation(super::MeSessionLoss::InputApplication, correlated)
    }

    /// Scalar count of a checked batch of an executing scalar instance.
    pub fn scalar_value_count(
        &self,
        references: &[u32],
        kind: SolveVariableValueKind,
    ) -> Result<usize, MeSessionError> {
        Ok(self
            .host
            .kernel
            .borrow()?
            .scalar_value_count(references, kind, false)?)
    }
}

impl MeRetainedComponent {
    /// `fmi3GetFloat64` of a scalar instance before initialization completes:
    /// host-written cells only.
    pub fn get_scalar_float64(
        &self,
        references: &[u32],
        values: &mut [f64],
    ) -> Result<(), MeError> {
        let staged = self.kernel.borrow()?.read_scalar_host_cells(references)?;
        deliver(staged, values)
    }

    /// `fmi3SetFloat64` of a scalar instance before initialization completes:
    /// inputs and settable parameters, through the kernel's own checked write.
    pub fn set_scalar_float64(
        &mut self,
        references: &[u32],
        values: &[f64],
    ) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut()?;
        let targets = kernel.scalar_float64_targets(references, values.len(), false)?;
        kernel.set_float64(&targets, values)
    }

    /// Scalar count of a checked batch before initialization completes.
    pub fn scalar_value_count(
        &self,
        references: &[u32],
        kind: SolveVariableValueKind,
    ) -> Result<usize, MeError> {
        self.kernel
            .borrow()?
            .scalar_value_count(references, kind, true)
    }
}
