//! Ordinary FMI VR reads reuse the dynamic component's atomic public observation.

use rumoca_ir_solve::RetainedAssertionRoot;
use rumoca_ir_solve::fmi::{FmiStorageColumn, FmiVariable};

use super::super::{MeError, MeObservation, MeTime, SolveMeKernel, contract};
use super::request::{self, ReadMember};

impl SolveMeKernel {
    pub(in crate::fmi_me) fn float64_values_at(
        &mut self,
        time: f64,
        states: &[f64],
        event_boundary: Option<f64>,
        references: &[u32],
        values: &mut [f64],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("get_float64_at")?;
        let root = self.float64_inventory_root()?;
        let (members, count) = request::read_request(
            root.metadata(),
            references,
            rumoca_ir_solve::SolveVariableValueKind::Real,
            false,
        )?;
        if count != values.len() {
            return Err(contract(
                "Float64 result extent differs from the original inventory",
            ));
        }
        if members.is_empty() {
            return Ok(());
        }
        // Preflight typed references before the standard coordinate setters.
        self.set_time(MeTime::new(time, event_boundary))?;
        self.set_continuous_states(states)?;
        if self.native_transport.is_some() {
            return self.get_native_float64(references, values);
        }
        self.read_public_float64(&members, values)
    }

    pub(super) fn float64_inventory_root(&self) -> Result<RetainedAssertionRoot, MeError> {
        if let Some(transport) = &self.native_transport {
            return Ok(transport.borrow().product().inputs().root().clone());
        }
        self.runtime
            .model()
            .retained_assertion_root()
            .cloned()
            .ok_or_else(|| contract("typed Float64 reads require correlated FMI metadata"))
    }

    pub(super) fn read_public_float64(
        &self,
        members: &[ReadMember<'_>],
        values: &mut [f64],
    ) -> Result<(), MeError> {
        if members.is_empty() {
            return Ok(());
        }
        // The existing request owner checked every VR, kind and extent first.
        // One complete observation supplies this synchronous ordinary request;
        // no component cache or coordinate is published by these copies.
        let observation = self.observe()?;
        let mut result = Vec::new();
        result
            .try_reserve_exact(values.len())
            .map_err(|_| MeError::Allocation {
                context: "ordinary Float64 result",
                entries: values.len(),
            })?;
        for member in members {
            match member {
                ReadMember::Time => result.push(observation.time),
                ReadMember::Variable(variable) => {
                    result.extend_from_slice(public_values(variable, &observation)?);
                }
            }
        }
        values.copy_from_slice(&result);
        Ok(())
    }
}

fn public_values<'a>(
    variable: &FmiVariable,
    observation: &'a MeObservation,
) -> Result<&'a [f64], MeError> {
    let storage = variable
        .storage()
        .expect("request checked original storage");
    let source = match storage.column() {
        FmiStorageColumn::Y => &observation.solver_y,
        FmiStorageColumn::P => &observation.parameters,
    };
    let range = storage.base()..storage.base() + storage.scalar_count();
    source
        .get(range)
        .ok_or_else(|| contract("ordinary Float64 storage is outside the public coordinate"))
}
