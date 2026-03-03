use std::collections::HashSet;

use rumoca_ir_flat as flat;

/// Extract the top-level prefix from a variable path.
///
/// For "resistor[1].p.i", returns "resistor".
/// For "plug_p.pin[1].i", returns "plug_p".
/// For "v", returns "v".
pub(crate) fn get_top_level_prefix(path: &str) -> Option<String> {
    let first_segment = top_level_segment(path)?;
    Some(normalize_top_level_segment(first_segment).to_string())
}

/// Split a dotted path while preserving dots inside bracket expressions.
///
/// For "bus[data.medium].pin.v", returns ["bus[data.medium]", "pin", "v"].
pub(crate) fn split_path_with_indices(path: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut depth = 0i32;

    for (idx, ch) in path.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            '.' if depth == 0 => {
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

/// Return the top-level path segment, preserving any index expression.
///
/// For "bus[data.medium].pin.v", returns "bus[data.medium]".
pub(crate) fn top_level_segment(path: &str) -> Option<&str> {
    first_path_segment(path)
}

/// Return true when a path has a top-level `.` separator.
///
/// Dots inside bracket expressions are ignored.
pub(crate) fn has_top_level_dot(path: &str) -> bool {
    let mut depth = 0i32;
    for ch in path.chars() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            '.' if depth == 0 => return true,
            _ => {}
        }
    }
    false
}

/// Return the first path segment, splitting on dots that are outside subscript brackets.
fn first_path_segment(path: &str) -> Option<&str> {
    let mut depth = 0i32;
    for (idx, ch) in path.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            '.' if depth == 0 => return Some(&path[..idx]),
            _ => {}
        }
    }
    (depth == 0).then_some(path)
}

/// Strip array indexing from a top-level path segment.
///
/// For "plug[1]" returns "plug".
pub(crate) fn normalize_top_level_segment(segment: &str) -> &str {
    segment.split('[').next().unwrap_or(segment)
}

/// Strip all bracketed subscript groups from a path while preserving dots and identifiers.
///
/// Examples:
/// - `pc[2*i-1].i` -> `pc.i`
/// - `pin_n[1].v` -> `pin_n.v`
/// - `R[1].w` -> `R.w`
pub(crate) fn strip_all_subscripts(path: &str) -> String {
    let mut out = String::with_capacity(path.len());
    let mut depth = 0i32;
    for ch in path.chars() {
        match ch {
            '[' => depth += 1,
            ']' if depth > 0 => depth -= 1,
            ']' => {}
            _ if depth == 0 => out.push(ch),
            _ => {}
        }
    }
    out
}

/// Normalize top-level names by stripping array indexing from each entry.
pub(crate) fn normalized_top_level_names<'a>(
    names: impl Iterator<Item = &'a String>,
) -> HashSet<String> {
    names
        .map(|name| normalize_top_level_segment(name).to_string())
        .collect()
}

/// Check whether a path belongs to a known top-level component set.
///
/// The provided set is expected to contain normalized top-level names
/// (i.e., array indices stripped).
pub(crate) fn path_is_in_top_level_set(
    path: &str,
    normalized_top_level_names: &HashSet<String>,
) -> bool {
    get_top_level_prefix(path)
        .is_some_and(|prefix| normalized_top_level_names.contains(prefix.as_str()))
}

/// Check whether a variable belongs to a top-level member path (has a dot).
pub(crate) fn is_top_level_member(
    name: &flat::VarName,
    normalized_top_level_names: &HashSet<String>,
) -> bool {
    has_top_level_dot(name.as_str())
        && path_is_in_top_level_set(name.as_str(), normalized_top_level_names)
}

/// Strip array subscript suffix from a variable name.
///
/// Connection equations expand array connections per-element, producing VarRef
/// names like `sum.u[1]` or `pc[1].i`. The flat variable map may store the
/// unsubscripted path (`sum.u`, `pc.i`). This removes the last embedded
/// bracketed index while preserving any trailing field suffix.
pub(crate) fn strip_subscript(name: &flat::VarName) -> Option<flat::VarName> {
    let s = name.as_str();
    let (start, end) = last_top_level_subscript_span(s)?;
    let mut out = String::with_capacity(s.len());
    out.push_str(&s[..start]);
    out.push_str(&s[end + 1..]);
    Some(flat::VarName::new(out))
}

/// Build a fallback chain by repeatedly stripping one top-level subscript group.
///
/// For `a[1].b[2].c[3]`, this returns:
/// - `a[1].b[2].c`
/// - `a[1].b.c`
/// - `a.b.c`
pub(crate) fn subscript_fallback_chain(name: &flat::VarName) -> Vec<flat::VarName> {
    let mut chain = Vec::new();
    let mut current = name.clone();

    while let Some(stripped) = strip_subscript(&current) {
        if stripped == current {
            break;
        }
        chain.push(stripped.clone());
        current = stripped;
    }

    chain
}

/// Return the byte-span of the last top-level `[ ... ]` group in a name.
///
/// Top-level means brackets at depth 0; nested brackets inside the subscript
/// expression are ignored for group selection.
pub(crate) fn last_top_level_subscript_span(name: &str) -> Option<(usize, usize)> {
    let mut depth = 0i32;
    let mut group_start = None;
    let mut last_group = None;

    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => {
                if depth == 0 {
                    group_start = Some(idx);
                }
                depth += 1;
            }
            ']' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    let start = group_start?;
                    last_group = Some((start, idx));
                }
            }
            _ => {}
        }
    }

    (depth == 0).then_some(last_group).flatten()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_top_level_prefix_strips_index() {
        assert_eq!(
            get_top_level_prefix("plug_p[2].pin[1].i"),
            Some("plug_p".to_string())
        );
    }

    #[test]
    fn test_get_top_level_prefix_ignores_dots_inside_subscript_expression() {
        assert_eq!(
            get_top_level_prefix("arr[record.value].field"),
            Some("arr".to_string())
        );
    }

    #[test]
    fn test_top_level_segment_preserves_bracket_expression() {
        assert_eq!(
            top_level_segment("bus[data.medium].pin"),
            Some("bus[data.medium]")
        );
    }

    #[test]
    fn test_split_path_with_indices_preserves_dot_inside_brackets() {
        assert_eq!(
            split_path_with_indices("bus[data.medium].pin.v"),
            vec!["bus[data.medium]", "pin", "v"]
        );
    }

    #[test]
    fn test_has_top_level_dot_ignores_dot_inside_subscript_expression() {
        assert!(!has_top_level_dot("plug[data.medium]"));
        assert!(has_top_level_dot("plug[data.medium].pin"));
    }

    #[test]
    fn test_normalized_top_level_names_deduplicates_indices() {
        let names = [
            "plug".to_string(),
            "plug[1]".to_string(),
            "frame_a[2]".to_string(),
        ];
        let normalized = normalized_top_level_names(names.iter());
        assert!(normalized.contains("plug"));
        assert!(normalized.contains("frame_a"));
        assert_eq!(normalized.len(), 2);
    }

    #[test]
    fn test_path_is_in_top_level_set_uses_normalized_prefix() {
        let names = ["plug[1]".to_string(), "frame_a".to_string()];
        let normalized = normalized_top_level_names(names.iter());
        assert!(path_is_in_top_level_set("plug.pin.i", &normalized));
        assert!(path_is_in_top_level_set("frame_a.R.w[2]", &normalized));
        assert!(!path_is_in_top_level_set("other.pin.i", &normalized));
    }

    #[test]
    fn test_is_top_level_member_requires_member_path() {
        let names = ["plug".to_string()];
        let normalized = normalized_top_level_names(names.iter());
        assert!(is_top_level_member(
            &flat::VarName::new("plug.pin.i"),
            &normalized
        ));
        assert!(!is_top_level_member(
            &flat::VarName::new("plug"),
            &normalized
        ));
        assert!(
            !is_top_level_member(&flat::VarName::new("plug[data.medium]"), &normalized),
            "dot inside a bracketed index does not make a top-level member path"
        );
    }

    #[test]
    fn test_subscript_fallback_chain_peels_all_subscript_layers() {
        let chain = subscript_fallback_chain(&flat::VarName::new("a[1].b[2].c[3]"));
        assert_eq!(
            chain,
            vec![
                flat::VarName::new("a[1].b[2].c"),
                flat::VarName::new("a[1].b.c"),
                flat::VarName::new("a.b.c")
            ]
        );
    }

    #[test]
    fn test_strip_all_subscripts_normalizes_embedded_indices() {
        assert_eq!(strip_all_subscripts("pc[((2*1)-1)].i"), "pc.i");
        assert_eq!(strip_all_subscripts("pin_n[1].v"), "pin_n.v");
        assert_eq!(strip_all_subscripts("R[1].w"), "R.w");
    }
}
