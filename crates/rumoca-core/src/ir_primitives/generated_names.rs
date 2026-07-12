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
