//! Float64, Int64 and Boolean values of a scalar instance by FMI value
//! reference, read the way [`super::MeSimulationSession::selected_values`]
//! reads names: one observation at the coordinate the session publishes (an
//! off-point excursion is closed back onto the accepted point), the whole
//! checked batch projected from it, and the same single warning report after
//! it. Every type writes through the same accepted-input transaction.

use std::rc::Rc;

use rumoca_ir_solve::SolveVariableValueKind;

use super::{MeRetainedComponent, MeSessionError, MeSimulationSession};
use crate::fmi_me::{MeError, ScalarCell, instance::MeKernelCell};

fn project_checked(kernel: &Rc<MeKernelCell>, references: &[u32]) -> Result<Vec<f64>, MeError> {
    let kernel = kernel.borrow()?;
    let observation = kernel.observe()?;
    let values = kernel.project_scalar_float64(&observation, references)?;
    kernel.report_warnings(&observation)?;
    Ok(values)
}

fn project_typed<T: ScalarCell>(
    kernel: &Rc<MeKernelCell>,
    references: &[u32],
) -> Result<Vec<T>, MeError> {
    let kernel = kernel.borrow()?;
    let observation = kernel.observe()?;
    let values = kernel.project_scalar_typed(&observation, references)?;
    kernel.report_warnings(&observation)?;
    Ok(values)
}

/// Copy a staged batch out only when it fills the caller's result exactly.
fn deliver<T: Copy>(staged: Vec<T>, values: &mut [T]) -> Result<(), MeError> {
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
        // The whole request and its cardinality are checked before any
        // observation or off-point excursion moves the component.
        let count = self.host.kernel.borrow()?.scalar_value_count(
            references,
            SolveVariableValueKind::Real,
            false,
        )?;
        if count != values.len() {
            return Err(MeError::Contract {
                reason: format!(
                    "typed FMI batch requires {count} values, got {}",
                    values.len()
                ),
            }
            .into());
        }
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
            let targets = kernel.scalar_float64_targets(references, values.len())?;
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

    /// `fmi3GetInt64` of a scalar instance after initialization.
    pub fn get_scalar_integer(
        &self,
        references: &[u32],
        values: &mut [i64],
    ) -> Result<(), MeSessionError> {
        self.get_scalar_typed(references, values)
    }

    /// `fmi3SetInt64` of an executing scalar instance: inputs only.
    pub fn set_scalar_integer(
        &mut self,
        references: &[u32],
        values: &[i64],
    ) -> Result<(), MeSessionError> {
        self.set_scalar_typed(references, values)
    }

    /// `fmi3GetBoolean` of a scalar instance after initialization.
    pub fn get_scalar_boolean(
        &self,
        references: &[u32],
        values: &mut [bool],
    ) -> Result<(), MeSessionError> {
        self.get_scalar_typed(references, values)
    }

    /// `fmi3SetBoolean` of an executing scalar instance: inputs only.
    pub fn set_scalar_boolean(
        &mut self,
        references: &[u32],
        values: &[bool],
    ) -> Result<(), MeSessionError> {
        self.set_scalar_typed(references, values)
    }

    /// The Float64 read's checks and observation for one Integer or Boolean
    /// batch: cardinality before any excursion, one observation, staged
    /// delivery.
    fn get_scalar_typed<T: ScalarCell>(
        &self,
        references: &[u32],
        values: &mut [T],
    ) -> Result<(), MeSessionError> {
        self.require_live()?;
        let count = self
            .host
            .kernel
            .borrow()?
            .scalar_value_count(references, T::KIND, false)?;
        if count != values.len() {
            return Err(MeError::Contract {
                reason: format!(
                    "typed FMI batch requires {count} values, got {}",
                    values.len()
                ),
            }
            .into());
        }
        let read = |kernel: &Rc<MeKernelCell>| project_typed(kernel, references);
        let staged = match self.off_point_observation() {
            Some((time, states)) => self.host.anchor().read_settled(time, states, read)?,
            None => read(&self.host.kernel)?,
        };
        Ok(deliver(staged, values)?)
    }

    /// The Float64 write's transaction for one Integer or Boolean batch: every
    /// value is checked and encoded before anything is written, and a batch
    /// equal to the current values changes nothing.
    fn set_scalar_typed<T: ScalarCell>(
        &mut self,
        references: &[u32],
        values: &[T],
    ) -> Result<(), MeSessionError> {
        self.require_live()?;
        let (targets, cells, unchanged) = {
            let kernel = self.host.kernel.borrow()?;
            let (targets, cells) = kernel.scalar_typed_targets(references, values)?;
            let current = kernel.read_scalar_typed_host_cells::<T>(references)?;
            (targets, cells, current == values)
        };
        if unchanged {
            return Ok(());
        }
        self.write_input_transaction(|write| write.set_float64(&targets, &cells))?;
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
        let targets = kernel.scalar_float64_targets(references, values.len())?;
        kernel.set_float64(&targets, values)
    }

    /// `fmi3GetInt64` of a scalar instance before initialization completes:
    /// host-written cells only.
    pub fn get_scalar_integer(
        &self,
        references: &[u32],
        values: &mut [i64],
    ) -> Result<(), MeError> {
        let staged = self
            .kernel
            .borrow()?
            .read_scalar_typed_host_cells(references)?;
        deliver(staged, values)
    }

    /// `fmi3SetInt64` of a scalar instance before initialization completes.
    pub fn set_scalar_integer(
        &mut self,
        references: &[u32],
        values: &[i64],
    ) -> Result<(), MeError> {
        self.set_scalar_typed(references, values)
    }

    /// `fmi3GetBoolean` of a scalar instance before initialization completes.
    pub fn get_scalar_boolean(
        &self,
        references: &[u32],
        values: &mut [bool],
    ) -> Result<(), MeError> {
        let staged = self
            .kernel
            .borrow()?
            .read_scalar_typed_host_cells(references)?;
        deliver(staged, values)
    }

    /// `fmi3SetBoolean` of a scalar instance before initialization completes.
    pub fn set_scalar_boolean(
        &mut self,
        references: &[u32],
        values: &[bool],
    ) -> Result<(), MeError> {
        self.set_scalar_typed(references, values)
    }

    fn set_scalar_typed<T: ScalarCell>(
        &mut self,
        references: &[u32],
        values: &[T],
    ) -> Result<(), MeError> {
        let mut kernel = self.kernel.borrow_mut()?;
        let (targets, cells) = kernel.scalar_typed_targets(references, values)?;
        kernel.set_float64(&targets, &cells)
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
