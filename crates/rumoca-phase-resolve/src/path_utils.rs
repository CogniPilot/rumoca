//! Narrow owner for qualified class-name parsing in resolve.
//!
//! Resolve keys classes and scopes by their qualified dotted names in
//! `name_to_def`/`package_children`. Walking those names stays centralized
//! here; semantic code must not re-derive scope nesting with ad hoc string
//! splitting. Prefer `ScopeTree`/`DefId` lookups when structure is available.

/// The leaf segment of a qualified class name.
pub(crate) fn class_name_leaf(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}
