//! Generated-name conventions owned by the IR.
//!
//! Compiler phases synthesize variables whose rendered names follow fixed
//! conventions. This module is the single owner of those conventions:
//! producers build the names through the typed constructors and consumers
//! recover structure through the typed inverses, so no phase re-derives
//! structure by string-matching the raw spellings.

use super::VarName;

/// Namespace identifier (bare, no dot) of the generated pre-slot variables
/// produced by DAE pre-lowering.
///
/// OWNING definition of the generated pre-slot naming convention: pre-lowering
/// replaces `pre(x)` with a generated parameter variable named `__pre__.x`.
/// Consumers must never string-match `"__pre__"` directly — construct slot
/// names with [`pre_slot_name`] and recover structure with [`pre_slot_base`] /
/// [`is_pre_slot`].
pub const PRE_SLOT_NAMESPACE: &str = "__pre__";

/// Runtime-managed parameter that is true only while processing the final
/// simulation event at the configured stop time.
pub const TERMINAL_EVENT_PARAMETER_NAME: &str = "__runtime__.terminal";

/// Namespace identifier of runtime-managed transport-delay value slots.
pub const DELAY_SLOT_NAMESPACE: &str = "__runtime__.delay";

/// Render the generated parameter name for transport-delay channel `index`.
pub fn delay_slot_name(index: usize) -> VarName {
    VarName::new(format!("{DELAY_SLOT_NAMESPACE}.{index}"))
}

/// Inverse of [`delay_slot_name`]: the channel index of a generated delay
/// value slot.
pub fn delay_slot_index(name: &str) -> Option<usize> {
    name.strip_prefix(DELAY_SLOT_NAMESPACE)?
        .strip_prefix('.')?
        .parse()
        .ok()
}

/// True for parameters whose values are maintained by the simulation runtime
/// rather than the parameter evaluator.
pub fn is_runtime_managed_slot(name: &str) -> bool {
    name == TERMINAL_EVENT_PARAMETER_NAME || delay_slot_index(name).is_some()
}

/// Render the generated pre-slot name for `base`: `__pre__.{base}`.
pub fn pre_slot_name(base: &str) -> VarName {
    VarName::new(format!("{PRE_SLOT_NAMESPACE}.{base}"))
}

/// Inverse of [`pre_slot_name`]: the base variable name of a generated
/// pre-slot, or `None` when `name` is not in the pre-slot namespace.
///
/// Exactly one namespace level is stripped, so a chained slot such as
/// `__pre__.__pre__.x` yields the inner slot name `__pre__.x`.
pub fn pre_slot_base(name: &str) -> Option<&str> {
    name.strip_prefix(PRE_SLOT_NAMESPACE)?.strip_prefix('.')
}

/// True when `name` is a generated pre-slot (see [`pre_slot_name`]).
pub fn is_pre_slot(name: &str) -> bool {
    pre_slot_base(name).is_some()
}

/// Boolean snapshot inserted at one non-leading function return.
pub fn function_return_guard_name(source_start: usize) -> VarName {
    VarName::new(format!("__rumoca_return_guard_{source_start}"))
}

/// Boolean snapshot inserted for a return-bearing conditional branch.
pub fn function_branch_guard_name(source_start: usize) -> VarName {
    VarName::new(format!("__rumoca_branch_guard_{source_start}"))
}

/// Compact binder introduced while rectangularizing one affine slice.
pub fn affine_slice_binder_name(source_start: usize) -> String {
    format!("__rumoca_affine_slice_{source_start}")
}

/// Compact binder introduced for one axis of a tensor-valued reduction.
pub fn tensor_reduction_binder_name(source_start: usize, axis: usize) -> String {
    format!("__rumoca_tensor_reduce_{source_start}_{axis}")
}
