//! The linked profile of a saved scalar product: every module of the product
//! as a library over one shared `env.memory`, each entry under a unique issued
//! symbol, for a guest that has no JavaScript engine to instantiate modules.
//! The browser profile, which the SDK saves and binds by comparing re-emitted
//! bytes, is unchanged: only an explicit linked emission prints these names.
mod dispatch;
#[cfg(all(
    feature = "fixed-scalar-guest-receiver",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
pub(crate) mod guest;
mod plan;
mod products;
#[cfg(all(test, feature = "host-engine", not(target_arch = "wasm32")))]
mod tests;

pub use plan::{
    LinkedBinding, LinkedDemand, LinkedEntry, LinkedEntryId, LinkedEntryKind, LinkedEntryPlan,
    LinkedFaultTables, LinkedWorkspace, linked_key_words,
};
pub(crate) use products::{DerivativeSymbols, helper_symbol};
#[cfg(feature = "precompiled-programs")]
pub use products::{LinkedAdmission, LinkedArtifact, linked_digest};
pub use products::{LinkedKernelModule, LinkedProduct, LinkedProductAssembler};

use crate::WasmCompileError;

/// The one export through which a linked guest reaches every issued entry.
pub const LINKED_DISPATCH_EXPORT: &str = "rumoca_dispatch";

/// The status the dispatch returns when no issued entry has the requested
/// identity. No entry ran, so first-fault words are stale.
pub const LINKED_DISPATCH_REFUSED: u32 = u32::MAX;

/// Map the dispatch's refusal to an error before any first-fault or helper
/// status is decoded; every other status is the entry's own.
pub fn linked_entry_status(status: u32) -> Result<u32, WasmCompileError> {
    if status == LINKED_DISPATCH_REFUSED {
        return Err(WasmCompileError::Input(
            "linked dispatch refused the entry identity; no entry ran".into(),
        ));
    }
    Ok(status)
}
