//! Shared path/subscript helpers for flatten internals.
//!
//! Flatten keys variables, constants, and functions by rendered flat names.
//! Segmentation of those names stays centralized here — flatten code must not
//! re-derive model structure with ad hoc string splitting. Prefer the typed
//! accessors on `VarName`/`Reference`/`ComponentPath` whenever a typed name
//! is available.

pub(crate) use rumoca_core::{first_path_segment_without_index, strip_array_index};

/// Final top-level segment of a rendered name.
pub(crate) fn leaf_segment(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}

/// The enclosing scope of a rendered name (`a.b.c` -> `a.b`).
pub(crate) fn enclosing_scope(name: &str) -> Option<&str> {
    rumoca_core::parent_scope(name)
}

/// Iterate the enclosing scopes of a rendered name from the innermost
/// outwards (`a.b.c` yields `a.b`, then `a`).
pub(crate) fn enclosing_scopes(name: &str) -> impl Iterator<Item = &str> {
    std::iter::successors(enclosing_scope(name), |current| enclosing_scope(current))
}

/// Split a rendered name into `(enclosing scope, leaf)` at the last
/// top-level dot.
pub(crate) fn scope_split(name: &str) -> Option<(&str, &str)> {
    rumoca_core::split_last_top_level(name)
}

/// Split a rendered name into `(root, rest)` at the first top-level dot.
pub(crate) fn root_split(name: &str) -> Option<(&str, &str)> {
    rumoca_core::split_first_top_level(name)
}

/// True when the rendered name is nested (has a top-level dot).
pub(crate) fn is_nested_name(name: &str) -> bool {
    rumoca_core::has_top_level_dot(name)
}

/// Top-level segments of a rendered name (subscripts keep embedded dots).
pub(crate) fn segments(name: &str) -> Vec<&str> {
    rumoca_core::split_path_with_indices(name)
}

pub(crate) fn unindexed_lookup_variants(path: &str) -> Vec<String> {
    let segments = segments(path);
    let indexed_positions = segments
        .iter()
        .enumerate()
        .filter_map(|(index, segment)| (strip_array_index(segment) != *segment).then_some(index))
        .collect::<Vec<_>>();
    if indexed_positions.is_empty() {
        return Vec::new();
    }
    if indexed_positions.len() > 8 {
        return vec![
            segments
                .iter()
                .map(|segment| strip_array_index(segment))
                .collect::<Vec<_>>()
                .join("."),
        ];
    }

    let mut variants = Vec::new();
    let variant_count = 1_usize << indexed_positions.len();
    for stripped_count in 1..=indexed_positions.len() {
        for mask in 1..variant_count {
            if mask.count_ones() as usize != stripped_count {
                continue;
            }
            let variant = unindexed_lookup_variant(&segments, &indexed_positions, mask);
            let path = variant.join(".");
            if !variants.iter().any(|existing| existing == &path) {
                variants.push(path);
            }
        }
    }
    variants
}

fn unindexed_lookup_variant(
    segments: &[&str],
    indexed_positions: &[usize],
    mask: usize,
) -> Vec<String> {
    let mut variant = segments.iter().map(ToString::to_string).collect::<Vec<_>>();
    for (bit_index, segment_index) in indexed_positions.iter().enumerate() {
        if (mask & (1_usize << bit_index)) == 0 {
            continue;
        }
        variant[*segment_index] = strip_array_index(variant[*segment_index].as_str()).to_string();
    }
    variant
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_first_path_segment_without_index_handles_nested_dots_in_subscript() {
        assert_eq!(
            first_path_segment_without_index("plug_p[a.b].pin[2].i"),
            Some("plug_p")
        );
        assert_eq!(first_path_segment_without_index(""), None);
    }

    #[test]
    fn unindexed_lookup_variants_preserve_specificity_order() {
        assert_eq!(
            unindexed_lookup_variants("a[1].b[2].c"),
            vec!["a.b[2].c", "a[1].b.c", "a.b.c"]
        );
    }
}
