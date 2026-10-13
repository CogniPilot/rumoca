//! Reads the original typed inventory without per-scalar request metadata.

use rumoca_exec_wasm::{NativeTransport, NativeTransportError, NativeTypedInputBatch};
use rumoca_ir_solve::fmi::{FmiVariable, reads_input_lanes};
use rumoca_ir_solve::{SolveValueKind, SolveVariableValueKind};

use super::super::{MeError, contract};

pub(super) trait TypedScalar: Sized {
    const KIND: SolveVariableValueKind;
    fn batch(values: &[Self]) -> NativeTypedInputBatch<'_>;
    fn decode(value: SolveValueKind) -> Result<Self, MeError>;
}

impl TypedScalar for i64 {
    const KIND: SolveVariableValueKind = SolveVariableValueKind::Integer;
    fn batch(values: &[Self]) -> NativeTypedInputBatch<'_> {
        NativeTypedInputBatch::Integer(values)
    }
    fn decode(value: SolveValueKind) -> Result<Self, MeError> {
        match value {
            SolveValueKind::Integer(value) => Ok(value),
            _ => Err(contract("native Integer descriptor produced another type")),
        }
    }
}

impl TypedScalar for bool {
    const KIND: SolveVariableValueKind = SolveVariableValueKind::Boolean;
    fn batch(values: &[Self]) -> NativeTypedInputBatch<'_> {
        NativeTypedInputBatch::Boolean(values)
    }
    fn decode(value: SolveValueKind) -> Result<Self, MeError> {
        match value {
            SolveValueKind::Boolean(value) => Ok(value),
            _ => Err(contract("native Boolean descriptor produced another type")),
        }
    }
}

pub(super) fn visit(
    transport: &NativeTransport,
    variable: &FmiVariable,
    callback: impl FnMut(usize, SolveValueKind) -> Result<(), MeError>,
) -> Result<(), MeError> {
    if reads_input_lanes(variable) {
        visit_cells(
            variable,
            transport
                .input_values()
                .map(|(lane, value)| (lane.p_index(), value)),
            callback,
        )
    } else {
        visit_cells(
            variable,
            transport
                .output_values()
                .map(|(lane, value)| (lane.p_index(), value)),
            callback,
        )
    }
}

pub(super) fn visit_cells(
    variable: &FmiVariable,
    cells: impl Iterator<Item = (usize, Result<SolveValueKind, NativeTransportError>)>,
    mut callback: impl FnMut(usize, SolveValueKind) -> Result<(), MeError>,
) -> Result<(), MeError> {
    let storage = variable
        .storage()
        .expect("request checked original storage");
    let mut count = 0;
    for (p_index, value) in cells {
        if p_index >= storage.base() && p_index - storage.base() < storage.scalar_count() {
            callback(
                p_index - storage.base(),
                value.map_err(super::transport_error)?,
            )?;
            count += 1;
        }
    }
    if count != storage.scalar_count() {
        return Err(contract(
            "native typed inventory does not cover the requested VR",
        ));
    }
    Ok(())
}

pub(super) fn check_membership(
    transport: &NativeTransport,
    variable: &FmiVariable,
) -> Result<(), MeError> {
    rumoca_ir_solve::fmi::check_native_typed_membership(
        transport.product().inputs().schedule(),
        variable,
    )
    .map_err(|error| contract(error.to_string()))
}

pub(super) fn input_batches<'value, T: TypedScalar>(
    metadata: &rumoca_ir_solve::fmi::FmiMetadata,
    references: &[u32],
    values: &'value [T],
    access: super::request::Access,
) -> Result<Vec<(usize, NativeTypedInputBatch<'value>)>, MeError> {
    let variables =
        super::request::checked_request(metadata, references, values.len(), T::KIND, access)?;
    let mut offset = 0;
    Ok(variables
        .into_iter()
        .map(|variable| {
            let storage = variable
                .storage()
                .expect("request checked original storage");
            let end = offset + storage.scalar_count();
            let batch = (storage.base(), T::batch(&values[offset..end]));
            offset = end;
            batch
        })
        .collect())
}
