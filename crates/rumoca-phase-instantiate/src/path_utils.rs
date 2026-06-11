//! Narrow owner for qualified class-name parsing in instantiate.
//!
//! Instantiation resolves component types through `name_map`/`def_map`
//! tables keyed by qualified dotted class names. Walking those names stays
//! centralized here; semantic code must not re-derive class nesting with ad
//! hoc string splitting. Prefer `ast::Name` parts, `DefId`s, and
//! `QualifiedName` structure whenever they are available.

/// Split a qualified class name into `(enclosing scope, leaf name)`.
pub(crate) fn class_scope_split(name: &str) -> Option<(&str, &str)> {
    rumoca_core::split_last_top_level(name)
}

/// The leaf segment of a qualified class name.
pub(crate) fn class_name_leaf(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}
