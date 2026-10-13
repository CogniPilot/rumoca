//! Float64 aggregate access uses the same original inventory and native coordinate.

use rumoca_ir_solve::fmi::FmiStorageColumn;
use rumoca_ir_solve::{SolveValueKind, SolveVariableStorageRole, SolveVariableValueKind};

use super::super::*;
use super::{request, values};

impl SolveMeKernel {
    pub fn set_native_float64(
        &mut self,
        references: &[u32],
        values: &[f64],
    ) -> Result<(), MeError> {
        let batches = self.native_float64_input_batches(references, values)?;
        self.commit_native_float64_inputs(&batches)
    }

    fn native_float64_input_batches<'value>(
        &self,
        references: &[u32],
        values: &'value [f64],
    ) -> Result<Vec<(usize, &'value [f64])>, MeError> {
        self.require_active_lifecycle("set_native_float64")?;
        let transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("native Float64 access requires a prepared component"))?;
        let root = transport.borrow().product().inputs().root().clone();
        real_input_batches(root.metadata(), references, values, self.input_access())
    }

    pub(in crate::fmi_me) fn check_native_float64_inputs(
        &self,
        references: &[u32],
        values: &[f64],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("check_native_float64_inputs")?;
        let transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("native Float64 inputs require a prepared component"))?;
        let transport = transport.borrow();
        let batches = real_input_batches(
            transport.product().inputs().root().metadata(),
            references,
            values,
            self.input_access(),
        )?;
        transport
            .check_float64_input_batches(&batches)
            .map_err(super::transport_error)
    }

    pub(in crate::fmi_me::kernel) fn commit_native_float64_inputs(
        &mut self,
        batches: &[(usize, &[f64])],
    ) -> Result<(), MeError> {
        self.native_transport
            .as_ref()
            .expect("caller retained native transport")
            .borrow()
            .check_float64_input_batches(batches)
            .map_err(super::transport_error)?;
        let snapshot = self.fmu_state()?;
        if let Err(error) = apply_native_float64_inputs(self, batches) {
            self.reset_to_fmu_state(&snapshot)
                .expect("same-instance checkpoint is restorable");
            return Err(error);
        }
        Ok(())
    }

    pub fn get_native_float64(
        &self,
        references: &[u32],
        values: &mut [f64],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("get_native_float64")?;
        let transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("native Float64 access requires a prepared component"))?;
        let root = transport.borrow().product().inputs().root().clone();
        let (members, count) = request::read_request(
            root.metadata(),
            references,
            SolveVariableValueKind::Real,
            false,
        )?;
        if count != values.len() {
            return Err(contract(format!(
                "typed FMI batch requires {count} values, got {}",
                values.len()
            )));
        }
        for variable in members.iter().filter_map(request::ReadMember::variable) {
            if variable.role() == Some(SolveVariableStorageRole::ExternalInput) {
                let storage = variable
                    .storage()
                    .expect("request checked original storage");
                transport
                    .borrow()
                    .check_float64_inputs(storage.base(), storage.scalar_count())
                    .map_err(super::transport_error)?;
            }
            if variable.role() == Some(SolveVariableStorageRole::DiscreteReal) {
                values::check_membership(&transport.borrow(), variable)?;
            }
        }
        let y = if members
            .iter()
            .filter_map(request::ReadMember::variable)
            .any(|variable| {
                variable
                    .storage()
                    .is_some_and(|storage| storage.column() == FmiStorageColumn::Y)
                    || variable.role() == Some(SolveVariableStorageRole::DiscreteReal)
            }) {
            self.native_coordinate(
                self.public_time_eval_time(self.time),
                &self.solver_y_guess.borrow(),
                &self.params,
            )?
        } else {
            Vec::new()
        };
        let transport = transport.borrow();
        for variable in members.iter().filter_map(request::ReadMember::variable) {
            if variable.role() == Some(SolveVariableStorageRole::DiscreteReal) {
                values::visit(&transport, variable, |_, value| real(value).map(|_| ()))?;
            }
        }
        let mut offset = 0;
        for member in members {
            let variable = match member {
                request::ReadMember::Time => {
                    values[offset] = self.time;
                    offset += 1;
                    continue;
                }
                request::ReadMember::Variable(variable) => variable,
            };
            let storage = variable
                .storage()
                .expect("request checked original storage");
            let end = offset + storage.scalar_count();
            if variable.role() == Some(SolveVariableStorageRole::DiscreteReal) {
                read_discrete_real(&transport, variable, &mut values[offset..])?;
            } else {
                let source = match storage.column() {
                    FmiStorageColumn::Y => &y,
                    FmiStorageColumn::P => &self.params,
                };
                values[offset..end].copy_from_slice(
                    &source[storage.base()..storage.base() + storage.scalar_count()],
                );
            }
            offset = end;
        }
        Ok(())
    }
}

impl super::super::AcceptedInputWrite<'_> {
    pub(in crate::fmi_me) fn set_native_float64(
        &mut self,
        references: &[u32],
        values: &[f64],
    ) -> Result<(), MeError> {
        let kernel = self.kernel();
        let batches = kernel.native_float64_input_batches(references, values)?;
        apply_native_float64_inputs(kernel, &batches)
    }
}

fn apply_native_float64_inputs(
    kernel: &mut SolveMeKernel,
    batches: &[(usize, &[f64])],
) -> Result<(), MeError> {
    kernel
        .native_transport
        .as_ref()
        .expect("caller retained native transport")
        .borrow_mut()
        .set_float64_input_batches(batches)
        .map_err(super::transport_error)?;
    for &(base, values) in batches {
        kernel.params[base..base + values.len()].copy_from_slice(values);
    }
    kernel.clear_runtime_caches();
    #[cfg(test)]
    input_commit_fault(InputCommitFault::BeforeSync)?;
    kernel.sync_native_coordinates()?;
    #[cfg(test)]
    input_commit_fault(InputCommitFault::BeforeDelay)?;
    kernel.commit_delay_point()
}

#[cfg(test)]
#[derive(Clone, Copy, PartialEq)]
pub(in crate::fmi_me) enum InputCommitFault {
    BeforeSync,
    BeforeDelay,
}

#[cfg(test)]
thread_local! {
    static INPUT_COMMIT_FAULT: std::cell::Cell<Option<InputCommitFault>> = const { std::cell::Cell::new(None) };
}

#[cfg(test)]
pub(in crate::fmi_me) fn fail_input_commit(fault: InputCommitFault) {
    INPUT_COMMIT_FAULT.with(|pending| pending.set(Some(fault)));
}

#[cfg(test)]
fn input_commit_fault(at: InputCommitFault) -> Result<(), MeError> {
    INPUT_COMMIT_FAULT.with(|pending| {
        if pending.get() == Some(at) {
            pending.set(None);
            Err(contract("injected native input commit failure"))
        } else {
            Ok(())
        }
    })
}

/// One Real64 cell as `f64`.
fn real(value: SolveValueKind) -> Result<f64, MeError> {
    match value {
        SolveValueKind::Real64(bits) => Ok(f64::from_bits(bits)),
        _ => Err(contract("native Real descriptor produced another type")),
    }
}

fn real_input_batches<'value>(
    metadata: &rumoca_ir_solve::fmi::FmiMetadata,
    references: &[u32],
    values: &'value [f64],
    access: request::Access,
) -> Result<Vec<(usize, &'value [f64])>, MeError> {
    let variables = request::checked_request(
        metadata,
        references,
        values.len(),
        SolveVariableValueKind::Real,
        access,
    )?;
    let mut offset = 0;
    let batches: Vec<_> = variables
        .into_iter()
        .map(|variable| {
            let storage = variable
                .storage()
                .expect("request checked original storage");
            let end = offset + storage.scalar_count();
            let batch = (storage.base(), &values[offset..end]);
            offset = end;
            batch
        })
        .collect();
    Ok(batches)
}

/// Copy one discrete Real variable's cells into `values`, starting at its first slot.
fn read_discrete_real(
    transport: &rumoca_exec_wasm::NativeTransport,
    variable: &rumoca_ir_solve::fmi::FmiVariable,
    values: &mut [f64],
) -> Result<(), MeError> {
    values::visit(transport, variable, |index, value| {
        values[index] = real(value)?;
        Ok(())
    })
}
