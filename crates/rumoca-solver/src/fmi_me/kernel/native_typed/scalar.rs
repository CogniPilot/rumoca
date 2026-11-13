//! Float64 values of a scalar (non-native) instance through the retained
//! component's original FMI inventory. There is no second VR table: requests
//! are checked by the same whole-batch owner the native path uses, and values
//! come from one branded observation (or, before initialization, from the
//! host-written cells only).

use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_ir_solve::fmi::{FmiMetadata, FmiStorageColumn};

use super::super::{MeError, MeFloat64Backing, MeObservation, MeValueRef, SolveMeKernel, contract};
use super::request::{self, Access, ReadMember};
use crate::fmi_me::lifecycle::MeState;

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

    /// The branded input-parameter references of a checked Float64 write, one
    /// per scalar in request order. Causality and the lifecycle window are
    /// checked first, by the request owner. The window is this instance's
    /// actual lifecycle state, not who holds it: in Instantiated and
    /// Initialization Mode inputs and settable parameters are writable,
    /// otherwise (including after a failed ExitInitializationMode that left
    /// a later state) inputs only.
    pub(crate) fn scalar_float64_targets(
        &self,
        references: &[u32],
        value_count: usize,
    ) -> Result<Vec<MeValueRef>, MeError> {
        self.require_active_lifecycle("scalar_float64_targets")?;
        let access = match self.initialization_state() {
            MeState::Instantiated | MeState::InitializationMode => Access::WriteInputsAndParameters,
            _ => Access::WriteInputs,
        };
        let variables = request::checked_request(
            self.scalar_inventory()?,
            references,
            value_count,
            SolveVariableValueKind::Real,
            access,
        )?;
        let mut targets = Vec::with_capacity(value_count);
        for variable in variables {
            let storage = variable
                .storage()
                .expect("request checked original storage");
            targets.extend(
                (storage.base()..storage.base() + storage.scalar_count()).map(|index| MeValueRef {
                    backing: MeFloat64Backing::InputParameter(index),
                    instance_brand: std::rc::Rc::clone(&self.instance_brand),
                }),
            );
        }
        Ok(targets)
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
