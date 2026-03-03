//! Shared path/subscript helpers for flatten internals.

/// Parse a dotted path into parts while preserving bracketed indices on each part.
///
/// Examples:
/// - `resistor[1].p.v` -> `["resistor[1]", "p", "v"]`
/// - `a[1,2].b[3].c` -> `["a[1,2]", "b[3]", "c"]`
pub(crate) fn parse_path_with_indices(path: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut bracket_depth = 0usize;

    for (i, c) in path.char_indices() {
        match c {
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '.' if bracket_depth == 0 => {
                if start < i {
                    parts.push(&path[start..i]);
                }
                start = i + 1;
            }
            _ => {}
        }
    }

    if start < path.len() {
        parts.push(&path[start..]);
    }

    parts
}

/// Return the byte index of the last top-level `.` in a path.
///
/// Dots inside bracketed subscripts are ignored.
pub(crate) fn find_last_top_level_dot(path: &str) -> Option<usize> {
    let mut bracket_depth = 0usize;
    let mut last_dot = None;

    for (idx, byte) in path.bytes().enumerate() {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => last_dot = Some(idx),
            _ => {}
        }
    }

    last_dot
}

/// True when the path has at least one top-level `.`.
pub(crate) fn has_top_level_dot(path: &str) -> bool {
    find_last_top_level_dot(path).is_some()
}

/// Return the parent scope prefix before the last top-level `.`.
pub(crate) fn parent_scope(path: &str) -> Option<&str> {
    find_last_top_level_dot(path).map(|dot_idx| &path[..dot_idx])
}

/// Return the final top-level segment in a dotted path.
pub(crate) fn top_level_last_segment(path: &str) -> &str {
    find_last_top_level_dot(path)
        .map(|dot_idx| &path[dot_idx + 1..])
        .unwrap_or(path)
}

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
    parse_path_with_indices(path)
        .into_iter()
        .next()
        .map(strip_array_index)
}

/// Normalize a full dotted path by stripping array indices from every segment.
///
/// Example: `a[1].b[2].c` -> `a.b.c`
pub(crate) fn normalize_path_without_indices(path: &str) -> String {
    parse_path_with_indices(path)
        .into_iter()
        .map(strip_array_index)
        .collect::<Vec<_>>()
        .join(".")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path_with_indices_keeps_bracket_content() {
        assert_eq!(parse_path_with_indices("a[1].b.c"), vec!["a[1]", "b", "c"]);
        assert_eq!(
            parse_path_with_indices("root.bus[a.b].pin[2].i"),
            vec!["root", "bus[a.b]", "pin[2]", "i"]
        );
        assert_eq!(
            parse_path_with_indices("medium_T[1].X[medium_T[1].nX].foo"),
            vec!["medium_T[1]", "X[medium_T[1].nX]", "foo"]
        );
    }

    #[test]
    fn test_first_path_segment_without_index_handles_nested_dots_in_subscript() {
        assert_eq!(
            first_path_segment_without_index("plug_p[a.b].pin[2].i"),
            Some("plug_p")
        );
        assert_eq!(first_path_segment_without_index(""), None);
    }

    #[test]
    fn test_normalize_path_without_indices() {
        assert_eq!(normalize_path_without_indices("a[1].b[2].c"), "a.b.c");
        assert_eq!(normalize_path_without_indices("x.y"), "x.y");
    }

    #[test]
    fn test_top_level_dot_helpers_ignore_subscript_content() {
        assert_eq!(find_last_top_level_dot("arr[data.medium]"), None);
        assert!(has_top_level_dot("sys.arr[data.medium]"));
        assert_eq!(
            top_level_last_segment("arr[data.medium]"),
            "arr[data.medium]"
        );
        assert_eq!(top_level_last_segment("sys.arr[data.medium].x"), "x");
        assert_eq!(
            parent_scope("sys.arr[data.medium].x"),
            Some("sys.arr[data.medium]")
        );
    }
}
