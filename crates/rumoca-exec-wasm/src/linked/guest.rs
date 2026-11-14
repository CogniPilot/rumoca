//! The linked guest's one import: every issued entry is reached through the
//! dispatch by its owner-issued triple, never by a name or a re-derived index.

#![allow(unsafe_code)]

use super::{LinkedEntry, LinkedEntryId, LinkedEntryKind, LinkedEntryPlan, linked_entry_status};
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

/// The position a loaded owner binds at: its issued plan and the index its
/// record holds in its kind's capture order. An owner never searches the
/// plan by key; two positions may share one.
#[derive(Clone, Copy)]
pub struct LinkedPosition<'a> {
    pub plan: &'a LinkedEntryPlan,
    pub index: u32,
}

impl<'a> LinkedPosition<'a> {
    /// The issued entry of `kind` at this position.
    pub(crate) fn entry(&self, kind: LinkedEntryKind) -> Result<&'a LinkedEntry, WasmCompileError> {
        self.plan
            .entry(LinkedEntryId {
                kind,
                index: self.index,
            })
            .ok_or_else(|| {
                WasmCompileError::Input(format!(
                    "no issued linked {kind:?} entry at index {}",
                    self.index
                ))
            })
    }
}

thread_local! {
    /// Dispatch calls made by this guest, per entry kind in code order: the
    /// per-path witness that model operations ran as compiled entries.
    static CALLS: std::cell::Cell<[u64; LinkedEntryKind::ALL.len()]> =
        const { std::cell::Cell::new([0; LinkedEntryKind::ALL.len()]) };
}

/// The dispatch calls this guest has made, per kind, in `LinkedEntryKind::ALL`
/// order. A refused identity is counted too: the call was attempted.
pub fn linked_dispatch_calls() -> [(LinkedEntryKind, u64); LinkedEntryKind::ALL.len()] {
    let calls = CALLS.with(std::cell::Cell::get);
    std::array::from_fn(|position| (LinkedEntryKind::ALL[position], calls[position]))
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
        CALLS.with(|calls| {
            let mut counts = calls.get();
            counts[self.id.kind.code() as usize - 1] += 1;
            calls.set(counts);
        });
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
