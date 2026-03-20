//! Shared path/subscript helpers for flatten internals.

pub(crate) use rumoca_core::{
    find_last_top_level_dot, has_top_level_dot, parent_scope, split_path_with_indices,
    top_level_last_segment,
};

/// Strip array index from one path segment.
///
/// Examples:
/// - `resistor[1]` -> `resistor`
/// - `p` -> `p`
pub(crate) fn strip_array_index(segment: &str) -> &str {
    if let Some(bracket_pos) = segment.find('[') {
        &segment[..bracket_pos]
    } else {
        segment
    }
}

/// Return the first top-level path segment with array indices removed.
pub(crate) fn first_path_segment_without_index(path: &str) -> Option<&str> {
    split_path_with_indices(path)
        .into_iter()
        .next()
        .map(strip_array_index)
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
}
