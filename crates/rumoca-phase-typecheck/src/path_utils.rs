//! Narrow owner for qualified class-name parsing in typecheck.
//!
//! Typecheck resolves instanced scopes and class constants through tables
//! keyed by qualified dotted class names. Walking those names stays
//! centralized here; semantic code must not re-derive class nesting with ad
//! hoc string splitting. Prefer `ComponentPath`, `ast::Name` parts, and
//! `DefId`s whenever structure is available.

/// The enclosing scope of a rendered instance or type-name string
/// (`A.B.C` -> `A.B`). Boundary helper for the String-keyed instance-scope
/// and overlay type-name tables; prefer `ClassTree::enclosing_class_names_of`
/// or `ComponentPath::parent` when structure is available.
pub(crate) fn enclosing_scope_str(name: &str) -> Option<&str> {
    rumoca_core::parent_scope(name)
}

/// The leaf segment of a qualified class name.
pub(crate) fn class_name_leaf(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}

/// Split a qualified class name into `(root segment, rest)`.
pub(crate) fn class_root_split(name: &str) -> Option<(&str, &str)> {
    rumoca_core::split_first_top_level(name)
}

/// True when the class name is qualified (contains a top-level dot).
pub(crate) fn is_qualified_class_name(name: &str) -> bool {
    rumoca_core::has_top_level_dot(name)
}
