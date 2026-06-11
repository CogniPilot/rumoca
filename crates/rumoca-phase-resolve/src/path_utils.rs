//! Narrow owner for qualified class-name parsing in resolve.
//!
//! Resolve keys classes and scopes by their qualified dotted names in
//! `name_to_def`/`package_children`. Walking those names stays centralized
//! here; semantic code must not re-derive scope nesting with ad hoc string
//! splitting. Prefer `ScopeTree`/`DefId` lookups when structure is available.

/// The enclosing class scope of a qualified class name (`A.B.C` -> `A.B`).
pub(crate) fn enclosing_class_scope(name: &str) -> Option<&str> {
    rumoca_core::parent_scope(name)
}

/// Iterate the enclosing class scopes of a qualified class name from the
/// innermost outwards (`A.B.C` yields `A.B`, then `A`).
pub(crate) fn enclosing_class_scopes(name: &str) -> impl Iterator<Item = &str> {
    std::iter::successors(enclosing_class_scope(name), |current| {
        enclosing_class_scope(current)
    })
}

/// Split a qualified class name into `(enclosing scope, leaf name)`.
pub(crate) fn class_scope_split(name: &str) -> Option<(&str, &str)> {
    rumoca_core::split_last_top_level(name)
}

/// The leaf segment of a qualified class name.
pub(crate) fn class_name_leaf(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}
