//! The linked guest's one import: every issued entry is reached through the
//! dispatch by its owner-issued triple, never by a name or a re-derived index.

#![allow(unsafe_code)]

use super::{LinkedEntry, LinkedEntryId, linked_entry_status};
use crate::WasmCompileError;

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    #[link_name = "rumoca_dispatch"]
    fn dispatch(
        kind: u32,
        index: u32,
        k0: u64,
        k1: u64,
        k2: u64,
        k3: u64,
        a0: u32,
        a1: u32,
        time: f64,
        a3: u32,
        a4: u32,
        a5: u32,
        a6: u32,
        a7: u32,
        a8: u32,
    ) -> u32;
}

/// The triple of one issued entry, exactly as its plan holds it.
pub(crate) struct LinkedCall {
    id: LinkedEntryId,
    key: [u64; 4],
}

impl LinkedCall {
    pub(crate) fn of(entry: &LinkedEntry) -> Self {
        Self {
            id: entry.id(),
            key: entry.key_words(),
        }
    }

    /// Registers `a0, a1, a3..a8` in order; an entry ignores the ones its
    /// convention does not take. The dispatch's refusal is an error here.
    pub(crate) fn call(&self, registers: [u32; 8], time: f64) -> Result<u32, WasmCompileError> {
        let [a0, a1, a3, a4, a5, a6, a7, a8] = registers;
        let [k0, k1, k2, k3] = self.key;
        // SAFETY: the fixed core links this import to the issued product's
        // dispatch over the shared memory; every pointer register was taken
        // from a live borrowed frame whose extents the caller checked.
        let status = unsafe {
            dispatch(
                self.id.kind.code(),
                self.id.index,
                k0,
                k1,
                k2,
                k3,
                a0,
                a1,
                time,
                a3,
                a4,
                a5,
                a6,
                a7,
                a8,
            )
        };
        linked_entry_status(status)
    }
}
