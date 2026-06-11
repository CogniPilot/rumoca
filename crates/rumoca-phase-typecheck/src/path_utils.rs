//! Narrow owner for qualified class-name parsing in typecheck.
//!
//! Typecheck resolves instanced scopes and class constants through tables
//! keyed by qualified dotted class names. Walking those names stays
//! centralized here; semantic code must not re-derive class nesting with ad
//! hoc string splitting. Prefer `ComponentPath`, `ast::Name` parts, and
//! `DefId`s whenever structure is available.

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
