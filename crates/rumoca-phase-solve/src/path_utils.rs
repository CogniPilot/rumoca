//! Narrow owner for rendered-name boundary parsing in the solve phase.
//!
//! Solve lowering receives function, type, and selection names as rendered
//! flat strings (solve-IR maps are serialized by name). Segmentation of those
//! strings stays centralized here — semantic code must not re-derive model
//! structure with ad hoc string splitting. Prefer the typed accessors on
//! `VarName`/`Reference`/`ComponentPath` whenever a typed name is available.

/// Final top-level segment of a rendered function or type name.
pub(crate) fn leaf_segment(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}

/// Split a rendered name into `(scope, leaf)` at the last top-level dot.
pub(crate) fn scope_split(name: &str) -> Option<(&str, &str)> {
    rumoca_core::split_last_top_level(name)
}

/// True when the rendered name is nested (has a top-level dot).
pub(crate) fn is_nested_name(name: &str) -> bool {
    rumoca_core::has_top_level_dot(name)
}

/// Top-level segments of a rendered name (subscripts keep embedded dots).
pub(crate) fn segments(name: &str) -> Vec<&str> {
    rumoca_core::split_path_with_indices(name)
}
