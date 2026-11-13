//! Float64 values of a scalar (non-native) instance through the retained
//! component's original FMI inventory. There is no second VR table: requests
//! are checked by the same whole-batch owner the native path uses, and values
//! come from one branded observation (or, before initialization, from the
//! host-written cells only).

use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_ir_solve::fmi::{FmiMetadata, FmiStorageColumn};

use super::super::{MeError, MeObservation, SolveMeKernel, contract};
use super::request::{self, ReadMember};

impl SolveMeKernel {
    fn scalar_inventory(&self) -> Result<&FmiMetadata, MeError> {
        if self.native_transport.is_some() {
            return Err(contract(
                "a native instance reads Float64 values through its native transport",
            ));
        }
        self.runtime
            .model()
            .retained_assertion_root()
            .map(|root| root.metadata())
            .ok_or_else(|| contract("scalar FMI values require the correlated retained component"))
    }

    /// Project a whole checked Float64 batch, in request order, from one
    /// observation of this instance. The result is staged and returned only
    /// when every member projected.
    pub(crate) fn project_scalar_float64(
        &self,
        observation: &MeObservation,
        references: &[u32],
    ) -> Result<Vec<f64>, MeError> {
        self.require_active_lifecycle("project_scalar_float64")?;
        self.require_observation_brand(observation)?;
        let (members, count) = request::read_request(
            self.scalar_inventory()?,
            references,
            SolveVariableValueKind::Real,
            false,
        )?;
        let mut values = Vec::with_capacity(count);
        for member in &members {
            match member {
                ReadMember::Time => values.push(observation.time),
                ReadMember::Variable(variable) => {
                    let storage = variable
                        .storage()
                        .expect("request checked original storage");
                    let column = match storage.column() {
                        FmiStorageColumn::Y => &observation.solver_y,
                        FmiStorageColumn::P => &observation.parameters,
                    };
                    let run = column
                        .get(storage.base()..storage.base() + storage.scalar_count())
                        .ok_or_else(|| {
                            contract("checked storage run lies outside the observation")
                        })?;
                    values.extend_from_slice(run);
                }
            }
        }
        Ok(values)
    }

    /// Before initialization completes, read only host-written cells (inputs,
    /// parameters, constants) from this instance's own parameter vector.
    pub(crate) fn read_scalar_host_cells(&self, references: &[u32]) -> Result<Vec<f64>, MeError> {
        self.require_active_lifecycle("read_scalar_host_cells")?;
        let (members, count) = request::read_request(
            self.scalar_inventory()?,
            references,
            SolveVariableValueKind::Real,
            true,
        )?;
        let mut values = Vec::with_capacity(count);
        for member in &members {
            let variable = member
                .variable()
                .ok_or_else(|| contract("independent time is not a host-written cell"))?;
            let storage = variable
                .storage()
                .expect("request checked original storage");
            let run = self
                .params
                .get(storage.base()..storage.base() + storage.scalar_count())
                .ok_or_else(|| contract("checked host cell lies outside the parameters"))?;
            values.extend_from_slice(run);
        }
        Ok(values)
    }

    /// Scalar count of a checked batch, by the same owner as its reads.
    pub(crate) fn scalar_value_count(
        &self,
        references: &[u32],
        kind: SolveVariableValueKind,
        pending: bool,
    ) -> Result<usize, MeError> {
        let (_, count) =
            request::read_request(self.scalar_inventory()?, references, kind, pending)?;
        Ok(count)
    }
}
