//! The linked guest's kernel entries, reached through the issued dispatch over
//! the one shared memory; the call conventions are the browser runtime's.

use crate::linked::guest::LinkedCall;
use crate::linked::{LinkedBinding, LinkedEntryKind, LinkedFaultTables, linked_key_words};
use crate::retained_native_program::linked_guest::pointer;
use crate::{NativeGatherFault, NativeLinearSolveFault, TypedCallFault, WasmCompileError};

pub(super) struct WasmKernelRuntime {
    entry: LinkedCall,
}

impl WasmKernelRuntime {
    /// The issued kernel entry of `kind` at `at` for the source with
    /// `source_key`, whose binding carries exactly the scratch extent and
    /// fault tables emitted with this source's payload. Nothing in the entry
    /// is re-derived here.
    pub(super) fn issued(
        at: crate::LinkedPosition<'_>,
        kind: LinkedEntryKind,
        source_key: &str,
        scratch_bytes: u32,
        (typed, gather, linear): (
            &[TypedCallFault],
            &[NativeGatherFault],
            &[NativeLinearSolveFault],
        ),
    ) -> Result<Self, WasmCompileError> {
        let entry = at.entry(kind)?;
        let key = linked_key_words(source_key)?;
        let LinkedBinding::Kernel {
            scratch_bytes: issued_scratch,
            faults:
                LinkedFaultTables {
                    typed: issued_typed,
                    gather: issued_gather,
                    linear: issued_linear,
                },
        } = entry.binding()
        else {
            return Err(WasmCompileError::Input(
                "the issued kernel entry carries a derivative binding".into(),
            ));
        };
        if entry.key_words() != key
            || *issued_scratch != scratch_bytes
            || issued_typed != typed
            || issued_gather != gather
            || issued_linear != linear
        {
            return Err(WasmCompileError::Input(
                "the issued linked kernel entry differs from its source".into(),
            ));
        }
        Ok(Self {
            entry: LinkedCall::of(entry),
        })
    }

    pub(super) fn call_private(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<u32, WasmCompileError> {
        self.call_status(y, p, time, scratch)
    }

    pub(super) fn call_assignments(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<u32, WasmCompileError> {
        self.call_status(y, p, time, scratch)
    }

    pub(super) fn call_native(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        scratch: &mut [u64],
        lanes: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        self.call_native_at(pointer(y.as_ptr())?, p, time, scratch, lanes)
    }

    /// The caller's Y in place, for a module certified to store nothing into it.
    pub(super) fn call_native_read_only(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [u64],
        lanes: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        self.call_native_at(pointer(y.as_ptr())?, p, time, scratch, lanes)
    }

    fn call_native_at(
        &self,
        y: u32,
        p: &[f64],
        time: f64,
        scratch: &mut [u64],
        lanes: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        let lanes = if lanes.is_empty() {
            0
        } else {
            pointer(lanes.as_ptr())?
        };
        self.entry.call(
            [
                y,
                pointer(p.as_ptr())?,
                pointer(scratch.as_ptr())?,
                lanes,
                0,
                0,
                0,
                0,
            ],
            time,
        )
    }

    fn call_status(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        scratch: &mut [f64],
    ) -> Result<u32, WasmCompileError> {
        self.entry.call(
            [
                pointer(y.as_ptr())?,
                pointer(p.as_ptr())?,
                pointer(scratch.as_ptr())?,
                0,
                0,
                0,
                0,
                0,
            ],
            time,
        )
    }
}
