/// Split a dotted path while preserving dots inside bracketed subscripts.
///
/// For `sys.arr[data.medium].x`, returns `["sys", "arr[data.medium]", "x"]`.
#[cfg(test)]
pub(crate) fn split_path_with_indices(path: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut bracket_depth = 0usize;

    for (idx, ch) in path.char_indices() {
        match ch {
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '.' if bracket_depth == 0 => {
                if start < idx {
                    parts.push(&path[start..idx]);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }

    if start < path.len() {
        parts.push(&path[start..]);
    }

    parts
}

/// Return the byte index of the last top-level `.` in `path`.
///
/// Dots inside bracketed subscripts (for example `a[b.c]`) are ignored.
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

/// True when `path` has at least one top-level `.`.
pub(crate) fn has_top_level_dot(path: &str) -> bool {
    find_last_top_level_dot(path).is_some()
}

/// Return the final top-level segment of `path`.
pub(crate) fn top_level_last_segment(path: &str) -> &str {
    find_last_top_level_dot(path)
        .map(|dot_idx| &path[dot_idx + 1..])
        .unwrap_or(path)
}

/// Return the parent scope prefix of `path` (before the last top-level `.`).
#[cfg(test)]
pub(crate) fn parent_scope(path: &str) -> Option<&str> {
    find_last_top_level_dot(path).map(|dot_idx| &path[..dot_idx])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parent_scope_ignores_dot_inside_subscript() {
        assert_eq!(parent_scope("arr[data.medium]"), None);
        assert_eq!(parent_scope("sys.arr[data.medium]"), Some("sys"));
        assert_eq!(
            parent_scope("sys.arr[data.medium].x"),
            Some("sys.arr[data.medium]")
        );
    }

    #[test]
    fn split_path_with_indices_ignores_nested_subscript_dots() {
        assert_eq!(
            split_path_with_indices("medium_T[1].X[medium_T[1].nX].foo"),
            vec!["medium_T[1]", "X[medium_T[1].nX]", "foo"]
        );
    }

    #[test]
    fn top_level_last_segment_ignores_dot_inside_subscript() {
        assert_eq!(
            top_level_last_segment("arr[data.medium]"),
            "arr[data.medium]"
        );
        assert_eq!(top_level_last_segment("sys.arr[data.medium].x"), "x");
    }
}
