//! Pre-init input access reuses the active kernel's inventory and cell/batch policy.

use super::super::{request, values::TypedScalar};
use super::*;
use rumoca_exec_wasm::NativeTypedInputBatch;
use rumoca_ir_solve::fmi::FmiVariable;
use rumoca_ir_solve::{SolveValueKind, SolveVariableValueKind};

impl MePendingNativeComponent {
    pub(in crate::fmi_me) fn native_value_count(
        &self,
        references: &[u32],
        kind: SolveVariableValueKind,
    ) -> Result<usize, MeError> {
        self.require_active("native_value_count")?;
        let root = self
            .model
            .retained_assertion_root()
            .expect("pending source is retained");
        let (_, count) = request::read_request(root.metadata(), references, kind, true)?;
        Ok(count)
    }
    pub fn set_native_integer(
        &mut self,
        references: &[u32],
        values: &[i64],
    ) -> Result<(), MeError> {
        self.set_typed(references, values)
    }
    pub fn set_boolean(&mut self, references: &[u32], values: &[bool]) -> Result<(), MeError> {
        self.set_typed(references, values)
    }
    pub fn get_native_integer(
        &self,
        references: &[u32],
        values: &mut [i64],
    ) -> Result<(), MeError> {
        self.get_typed(references, values)
    }
    pub fn get_boolean(&self, references: &[u32], values: &mut [bool]) -> Result<(), MeError> {
        self.get_typed(references, values)
    }

    fn set_typed<T: TypedScalar>(
        &mut self,
        references: &[u32],
        values: &[T],
    ) -> Result<(), MeError> {
        self.require_active("set_typed")?;
        let root = self
            .model
            .retained_assertion_root()
            .expect("pending source is retained");
        let variables = request::checked_request(
            root.metadata(),
            references,
            values.len(),
            T::KIND,
            request::Access::WriteInputsAndParameters,
        )?;
        let mut offset = 0;
        let batches: Vec<(usize, NativeTypedInputBatch<'_>)> = variables
            .iter()
            .map(|variable| {
                let storage = variable
                    .storage()
                    .expect("request checked original storage");
                let end = offset + storage.scalar_count();
                let batch = (storage.base(), T::batch(&values[offset..end]));
                offset = end;
                batch
            })
            .collect();
        match self.resource.as_mut().expect("pending resource is owned") {
            PendingNativeResource::Prepared(resource) => resource.set_typed_input_batches(&batches),
            PendingNativeResource::Resolved(resource) => resource.set_typed_input_batches(&batches),
        }
        .map_err(super::super::transport_error)
    }

    fn get_typed<T: TypedScalar>(
        &self,
        references: &[u32],
        values: &mut [T],
    ) -> Result<(), MeError> {
        self.require_active("get_typed")?;
        let root = self
            .model
            .retained_assertion_root()
            .expect("pending source is retained");
        // Pending getters admit host-written cells only, never derived outputs.
        let variables = request::checked_request(
            root.metadata(),
            references,
            values.len(),
            T::KIND,
            request::Access::HostCells,
        )?;
        for variable in &variables {
            self.visit(variable, |_, value| T::decode(value).map(|_| ()))?;
        }
        let mut offset = 0;
        for variable in variables {
            self.visit(variable, |index, value| {
                values[offset + index] = T::decode(value)?;
                Ok(())
            })?;
            offset += variable
                .storage()
                .expect("request checked original storage")
                .scalar_count();
        }
        Ok(())
    }

    fn visit(
        &self,
        variable: &FmiVariable,
        visit: impl FnMut(usize, SolveValueKind) -> Result<(), MeError>,
    ) -> Result<(), MeError> {
        match self.resource.as_ref().expect("pending resource is owned") {
            PendingNativeResource::Prepared(resource) => super::super::values::visit_cells(
                variable,
                resource
                    .input_values()
                    .map(|(lane, value)| (lane.p_index(), value)),
                visit,
            ),
            PendingNativeResource::Resolved(resource) => super::super::values::visit_cells(
                variable,
                resource
                    .input_values()
                    .map(|(lane, value)| (lane.p_index(), value)),
                visit,
            ),
        }
    }

    pub fn set_native_float64(
        &mut self,
        references: &[u32],
        values: &[f64],
    ) -> Result<(), MeError> {
        self.require_active("set_native_float64")?;
        let root = self
            .model
            .retained_assertion_root()
            .expect("pending source is retained");
        let variables = request::checked_request(
            root.metadata(),
            references,
            values.len(),
            SolveVariableValueKind::Real,
            request::Access::WriteInputsAndParameters,
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
        match self.resource.as_mut().expect("pending resource is owned") {
            PendingNativeResource::Prepared(resource) => {
                resource.set_float64_input_batches(&batches)
            }
            PendingNativeResource::Resolved(resource) => {
                resource.set_float64_input_batches(&batches)
            }
        }
        .map_err(super::super::transport_error)
    }

    pub fn get_native_float64(
        &self,
        references: &[u32],
        values: &mut [f64],
    ) -> Result<(), MeError> {
        self.require_active("get_native_float64")?;
        let root = self
            .model
            .retained_assertion_root()
            .expect("pending source is retained");
        let variables = request::checked_request(
            root.metadata(),
            references,
            values.len(),
            SolveVariableValueKind::Real,
            request::Access::HostCells,
        )?;
        for variable in &variables {
            let storage = variable
                .storage()
                .expect("request checked original storage");
            match self.resource.as_ref().expect("pending resource is owned") {
                PendingNativeResource::Prepared(resource) => {
                    resource.check_float64_inputs(storage.base(), storage.scalar_count())
                }
                PendingNativeResource::Resolved(resource) => {
                    resource.check_float64_inputs(storage.base(), storage.scalar_count())
                }
            }
            .map_err(super::super::transport_error)?;
        }
        let (_, p) = self.coordinates();
        let mut offset = 0;
        for variable in variables {
            let storage = variable
                .storage()
                .expect("request checked original storage");
            let end = offset + storage.scalar_count();
            values[offset..end]
                .copy_from_slice(&p[storage.base()..storage.base() + storage.scalar_count()]);
            offset = end;
        }
        Ok(())
    }

    pub(super) fn coordinates(&self) -> (&[f64], &[f64]) {
        match self.resource.as_ref().expect("pending resource is owned") {
            PendingNativeResource::Prepared(resource) => resource.coordinates(),
            PendingNativeResource::Resolved(resource) => resource.coordinates(),
        }
    }
}
