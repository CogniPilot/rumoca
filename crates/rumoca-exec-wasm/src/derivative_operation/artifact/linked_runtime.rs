//! The linked guest's derivative entries, reached through the issued dispatch
//! over the one shared memory; the frame convention is the browser's.
use super::WasmCompileError;
use super::call::{FmiDerivativeCall, FmiKinematicCall};
use crate::linked::LinkedEntry;
use crate::linked::guest::LinkedCall;
use crate::retained_native_program::linked_guest::pointer;

pub(super) struct DerivativeRuntime {
    entry: LinkedCall,
    kinematic: Option<LinkedCall>,
}

impl DerivativeRuntime {
    pub(super) fn linked(entry: &LinkedEntry, kinematic: Option<&LinkedEntry>) -> Self {
        Self {
            entry: LinkedCall::of(entry),
            kinematic: kinematic.map(LinkedCall::of),
        }
    }

    pub(super) fn call(&self, frame: &mut FmiDerivativeCall<'_>) -> Result<u32, WasmCompileError> {
        self.entry.call(
            [
                pointer(frame.y.as_ptr())?,
                pointer(frame.parameters.as_ptr())?,
                pointer(frame.output.as_ptr())?,
                pointer(frame.workspace.as_ptr())?,
                pointer(frame.cache_values.as_ptr())?,
                pointer(frame.cache_present.as_ptr())?,
                pointer(frame.cache_parameters.as_ptr())?,
                u32::from(*frame.cache_valid),
            ],
            frame.time,
        )
    }

    pub(super) fn call_kinematic(
        &self,
        frame: &mut FmiKinematicCall<'_>,
    ) -> Result<u32, WasmCompileError> {
        let entry = self
            .kinematic
            .as_ref()
            .ok_or_else(|| WasmCompileError::Input("no issued kinematic entry".into()))?;
        entry.call(
            [
                pointer(frame.y.as_ptr())?,
                pointer(frame.parameters.as_ptr())?,
                pointer(frame.output.as_ptr())?,
                pointer(frame.workspace.as_ptr())?,
                0,
                0,
                0,
                0,
            ],
            frame.time,
        )
    }
}

/// The issuer's active phase is the only authority to request this recovery.
pub(super) fn restore_coordinate(
    frame: &mut FmiDerivativeCall<'_>,
    workspace: rumoca_ir_solve::fmi::FmiDerivativeWorkspace,
) -> Result<(), WasmCompileError> {
    let start = workspace.snapshot() as usize;
    let bytes = workspace.y_bytes() as usize;
    if start % 8 != 0 || bytes != frame.y.len() * 8 {
        return Err(WasmCompileError::Input(
            "rollback snapshot differs from issued layout".into(),
        ));
    }
    let saved = frame
        .workspace
        .get(start / 8..(start + bytes) / 8)
        .ok_or_else(|| WasmCompileError::Input("rollback snapshot outside workspace".into()))?;
    for (y, word) in frame.y.iter_mut().zip(saved) {
        *y = f64::from_bits(*word);
    }
    Ok(())
}
