//! Private receiver of the trusted whole graph (SPEC_0036 linked construction).
//!
//! Imported bodies are not inspectable here. Only the source-built fixed core
//! composed by the owning graph issuer is an admitted deployment of this code.

#![allow(unsafe_code)]

use std::rc::Rc;

use rumoca_ir_solve::fmi::FmiComponent;

use super::PreparedNativeWasmCode;
use crate::WasmCompileError;

#[link(wasm_import_module = "env")]
unsafe extern "C" {
    #[link_name = "rumoca_source_capsule_len"]
    fn capsule_len() -> i32;
    #[link_name = "rumoca_source_capsule_copy"]
    fn capsule_copy(destination: u32, capacity: u32) -> i32;
    #[link_name = "rumoca_publication_token_len"]
    fn token_len() -> i32;
    #[link_name = "rumoca_publication_token_copy"]
    fn token_copy(destination: u32, capacity: u32) -> i32;
    #[link_name = "eval_assignments"]
    fn native_entry(y: u32, p: u32, time: f64, scratch: u32, typed: u32) -> u32;
}

/// One fresh checked replay and its code, retained together by the fixed adapter.
/// No public constructor accepts source bytes, a root or an imported entry.
///
/// ```compile_fail
/// use rumoca_exec_wasm::{LinkedNativeGuestModel, PreparedNativeWasmCode};
/// use rumoca_ir_solve::fmi::FmiComponent;
/// fn forge(component: FmiComponent, code: PreparedNativeWasmCode) -> LinkedNativeGuestModel {
///     LinkedNativeGuestModel { component, code }
/// }
/// ```
pub struct LinkedNativeGuestModel {
    component: FmiComponent,
    code: PreparedNativeWasmCode,
    publication_token: String,
}

impl LinkedNativeGuestModel {
    pub fn component(&self) -> &FmiComponent {
        &self.component
    }

    pub fn prepared_code(&self) -> &PreparedNativeWasmCode {
        &self.code
    }

    /// Compare the actual publication slot, never use a token to admit an entry.
    pub fn validate_instantiation_token(&self, token: &str) -> Result<(), WasmCompileError> {
        if token != self.publication_token {
            return Err(WasmCompileError::Input(
                "FMI instantiation token differs from the original publication".into(),
            ));
        }
        Ok(())
    }
}

/// Fixed-core bootstrap only. The adapter caches this inventory across resources.
/// The trusted graph binds these internal slots; no caller supplies either half.
#[doc(hidden)]
pub fn receive_fixed_native_guest_model() -> Result<LinkedNativeGuestModel, WasmCompileError> {
    let (component, policy) = replay_linked_source()?;
    let publication_token = String::from_utf8(slot_bytes(SourceSlot::PublicationToken)?)
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    if publication_token.is_empty() {
        return Err(WasmCompileError::Input(
            "linked publication token is empty".into(),
        ));
    }
    let inputs = component
        .runtime_view()
        .retain_assertion_root()
        .native_preparation()
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    // Derive the current inventory once from this exact replay. These bytes are
    // not instantiated: execution uses the graph's original emitted core slot.
    let mut code = super::compile_retained_native_preparation_wasm_with_policy(inputs, policy)?;
    Rc::get_mut(&mut code.code)
        .expect("new preparation has its sole owning reference")
        .linked_entry = Some(StaticLinkedEntry);
    Ok(LinkedNativeGuestModel {
        component,
        code,
        publication_token,
    })
}

fn replay_linked_source()
-> Result<(FmiComponent, crate::CheckedNativeWasmCapacityPolicy), WasmCompileError> {
    let bytes = slot_bytes(SourceSlot::Capsule)?;
    let (capsule, policy) =
        crate::decode_native_source_envelope(&bytes).map_err(WasmCompileError::Input)?;
    drop(bytes);
    let mut reader = serde_json::Deserializer::from_slice(&capsule);
    reader.disable_recursion_limit();
    let component = rumoca_eval_solve::fmi_wire::deserialize_fmi_component(&mut reader)
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    reader
        .end()
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    Ok((component, policy))
}

enum SourceSlot {
    Capsule,
    PublicationToken,
}

fn slot_bytes(slot: SourceSlot) -> Result<Vec<u8>, WasmCompileError> {
    // SAFETY: fixed source-built graph imports are callback-free; the data
    // provider copies into the supplied guest allocation in the shared memory.
    let length = unsafe {
        match slot {
            SourceSlot::Capsule => capsule_len(),
            SourceSlot::PublicationToken => token_len(),
        }
    };
    let length = usize::try_from(length)
        .map_err(|_| WasmCompileError::Input("linked source slot length is invalid".into()))?;
    let mut bytes = Vec::new();
    bytes
        .try_reserve_exact(length)
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    bytes.resize(length, 0);
    let destination = pointer(bytes.as_mut_ptr())?;
    let capacity = u32::try_from(length)
        .map_err(|_| WasmCompileError::Input("linked source slot exceeds memory32".into()))?;
    // SAFETY: destination is initialized, writable for capacity bytes and alive
    // for this call; the trusted copy body checks capacity before writing.
    let copied = unsafe {
        match slot {
            SourceSlot::Capsule => capsule_copy(destination, capacity),
            SourceSlot::PublicationToken => token_copy(destination, capacity),
        }
    };
    if copied != i32::try_from(length).expect("length came from a nonnegative i32") {
        return Err(WasmCompileError::Input(
            "linked source slot copy is incomplete".into(),
        ));
    }
    Ok(bytes)
}

/// Constructible only beside the sole slot replay in this private module.
pub(super) struct StaticLinkedEntry;

impl StaticLinkedEntry {
    pub(super) fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        scratch: &mut [u64],
        typed: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        let y = pointer(y.as_mut_ptr())?;
        let p = pointer(p.as_ptr())?;
        let scratch = pointer(scratch.as_mut_ptr())?;
        let typed = if typed.is_empty() {
            0
        } else {
            pointer(typed.as_mut_ptr())?
        };
        // SAFETY: only NativeInvocationLease reaches this private method. Its
        // actual resource owns the checked, disjoint memory32 spans and holds
        // the original shared-code non-reentry gate across this callback-free call.
        Ok(unsafe { native_entry(y, p, time, scratch, typed) })
    }
}

fn pointer<T>(pointer: *const T) -> Result<u32, WasmCompileError> {
    u32::try_from(pointer as usize)
        .map_err(|_| WasmCompileError::Input("native buffer is outside memory32".into()))
}
