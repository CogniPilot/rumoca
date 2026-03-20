//! Path/index matching helpers for connector-connection expansion.
//!
//! Keeps string/index normalization logic isolated from connection-set orchestration.

use crate::path_utils::{split_path_with_indices, strip_array_index};
use rumoca_ir_flat as flat;

pub(super) fn normalized_base_key_from_segments(segments: &[&str]) -> String {
    segments
        .iter()
        .map(|segment| strip_array_index(segment))
        .collect::<Vec<_>>()
        .join(".")
}

pub(super) fn normalized_base_key_from_owned_parts(parts: &[String]) -> String {
    parts
        .iter()
        .map(|part| strip_array_index(part.as_str()))
        .collect::<Vec<_>>()
        .join(".")
}

/// Compare a name part against a search segment, handling array indices.
///
/// If the segment has no index, matches any index on the name (array expansion).
/// If the segment has an index, the name must have the exact same index.
///
/// Examples:
/// - (`resistor[1]`, `resistor`) -> true (segment has no index, allows any)
/// - (`resistor[1]`, `resistor[1]`) -> true (same index)
/// - (`resistor[2]`, `resistor[1]`) -> false (different index)
/// - (`resistor`, `resistor[1]`) -> false (segment has index, name doesn't)
pub(super) fn compare_path_part(name_part: &str, segment: &str) -> bool {
    compare_path_part_with_mode(name_part, segment, false)
}

/// Compare path parts with optional support for indexed segment vs. indexless name.
///
/// `allow_collapsed_index_match=true` allows a segment like `n[2]` to match a
/// name part `n`. This is needed when flattened connector-array fields are stored
/// as `...n.i` (array) while connection paths refer to connector elements as
/// `...n[2]`.
pub(super) fn compare_path_part_with_mode(
    name_part: &str,
    segment: &str,
    allow_collapsed_index_match: bool,
) -> bool {
    let name_base = strip_array_index(name_part);
    let segment_base = strip_array_index(segment);
    if name_base != segment_base {
        return false;
    }
    // If segment specifies an exact index, enforce match.
    match (segment.find('['), name_part.find('[')) {
        (Some(si), Some(ni)) => name_part[ni..] == segment[si..],
        (Some(_), None) => allow_collapsed_index_match,
        _ => true,
    }
}

/// Append index text to `indices`, preferring `name_part` index and optionally
/// falling back to `segment` when name has no explicit index.
fn append_indices(
    indices: &mut String,
    name_part: &str,
    segment: &str,
    allow_segment_fallback: bool,
) {
    if let Some(idx) = extract_array_index(name_part) {
        indices.push_str(&idx);
        return;
    }
    if allow_segment_fallback && let Some(idx) = extract_array_index(segment) {
        indices.push_str(&idx);
    }
}

/// Extract the suffix after a prefix, handling array indices.
///
/// For example:
/// - extract_suffix("r1.n.v", "r1.n") returns Some(("v", ""))
/// - extract_suffix("resistor[1].p.v", "resistor.p") returns Some(("v", "[1]"))
///
/// Returns (suffix, array_indices) where array_indices is the index pattern found.
pub(super) fn extract_suffix(full_name: &str, prefix: &str) -> Option<(String, String)> {
    // First try exact prefix match.
    let prefix_dot = format!("{}.", prefix);
    if full_name.starts_with(&prefix_dot) {
        return Some((full_name[prefix_dot.len()..].to_string(), String::new()));
    }

    // Try matching with array indices.
    let prefix_segments = split_path_with_indices(prefix);
    let name_parts = split_path_with_indices(full_name);

    if name_parts.len() <= prefix_segments.len() {
        return None;
    }

    // Collect array indices as we match segments.
    let mut indices = String::new();

    for (i, segment) in prefix_segments.iter().enumerate() {
        if i >= name_parts.len() {
            return None;
        }

        let name_part = name_parts[i];

        if !compare_path_part_with_mode(name_part, segment, true) {
            return None;
        }

        // Preserve connector-element indices from segment when flattened names
        // store the connector as an indexless array field (e.g., n[2] vs n.i).
        append_indices(&mut indices, name_part, segment, true);
    }

    // Build the suffix from remaining parts.
    let suffix = name_parts[prefix_segments.len()..].join(".");
    Some((suffix, indices))
}

/// Extract array index from a path part.
///
/// "resistor[1]" -> Some("[1]")
/// "p" -> None
pub(super) fn extract_array_index(s: &str) -> Option<String> {
    s.find('[').map(|bracket_pos| s[bracket_pos..].to_string())
}

/// Scalarize a matched collapsed connector-array field for element connections.
///
/// Example:
/// - path: `s[1].inductance.n[2]`
/// - matched var: `s[1].inductance.n.i` (dims=[4])
///   -> returns `s[1].inductance.n.i[2]`
///
/// This keeps element-level connections scalar when flattened variables store
/// connector arrays as indexless array fields.
pub(super) fn scalarize_collapsed_connector_element(
    var: &flat::VarName,
    path: &str,
    flat: &flat::Model,
) -> flat::VarName {
    if !flat.variables.contains_key(var) {
        return var.clone();
    }
    let path_segments = split_path_with_indices(path);
    let var_parts = split_path_with_indices(var.as_str());
    let upto = path_segments.len().min(var_parts.len());
    let mut missing_index: Option<String> = None;
    for i in 0..upto {
        let seg_idx = extract_array_index(path_segments[i]);
        let part_idx = extract_array_index(var_parts[i]);
        if seg_idx.is_some()
            && part_idx.is_none()
            && strip_array_index(path_segments[i]) == strip_array_index(var_parts[i])
        {
            missing_index = seg_idx;
        }
    }
    if let Some(idx) = missing_index {
        return flat::VarName::new(format!("{}{}", var.as_str(), idx));
    }
    var.clone()
}

/// True when any segment in a connector path contains an explicit array index.
pub(super) fn path_has_explicit_index(path: &str) -> bool {
    split_path_with_indices(path)
        .iter()
        .any(|segment| extract_array_index(segment).is_some())
}

/// Extract bracketed index groups from an index pattern string.
///
/// Example: `"[1][2]" -> vec!["[1]", "[2]"]`.
fn extract_index_groups(indices: &str) -> Vec<String> {
    let mut groups = Vec::new();
    let mut start: Option<usize> = None;
    for (i, ch) in indices.char_indices() {
        match ch {
            '[' => start = Some(i),
            ']' => {
                if let Some(s) = start.take() {
                    groups.push(indices[s..=i].to_string());
                }
            }
            _ => {}
        }
    }
    groups
}

/// Remove index groups that are fixed by explicit indices in `path`.
///
/// Example:
/// - `indices="[1][2]"`, `path="s[1].p"` -> `"[2]"`
/// - `indices="[1]"`, `path="resistor[1].p"` -> `""`
pub(super) fn strip_explicit_path_indices(indices: &str, path: &str) -> String {
    let explicit_count = split_path_with_indices(path)
        .into_iter()
        .filter(|segment| extract_array_index(segment).is_some())
        .count();
    strip_explicit_index_count(indices, explicit_count)
}

/// Remove the first `explicit_count` bracket groups from an index pattern.
pub(super) fn strip_explicit_index_count(indices: &str, explicit_count: usize) -> String {
    if explicit_count == 0 {
        return indices.to_string();
    }
    let groups = extract_index_groups(indices);
    if groups.len() <= explicit_count {
        String::new()
    } else {
        groups[explicit_count..].concat()
    }
}

/// True if `full_name` carries an index on the last segment of `prefix` where
/// `prefix` itself has no index on that segment.
///
/// Example:
/// - full=`plug_p.pin[2].i`, prefix=`plug_p.pin` => true
/// - full=`resistor[1].p.i`, prefix=`resistor.p` => false (missing index is not on last segment)
pub(super) fn missing_index_on_last_prefix_segment(full_name: &str, prefix: &str) -> bool {
    let prefix_segments = split_path_with_indices(prefix);
    let name_parts = split_path_with_indices(full_name);
    if name_parts.len() <= prefix_segments.len() || prefix_segments.is_empty() {
        return false;
    }

    let last = prefix_segments.len() - 1;
    for (i, segment) in prefix_segments.iter().enumerate() {
        if strip_array_index(name_parts[i]) != strip_array_index(segment) {
            return false;
        }
        let seg_idx = extract_array_index(segment);
        let name_idx = extract_array_index(name_parts[i]);
        if seg_idx.is_none() && name_idx.is_some() && i != last {
            return false;
        }
    }

    extract_array_index(prefix_segments[last]).is_none()
        && extract_array_index(name_parts[last]).is_some()
}

/// Select the trailing index groups that map to an array variable's dimensions.
///
/// If `indices` has more groups than `dims_len`, keep the right-most groups.
/// This preserves outer component indices while selecting only element indices
/// for collapsed connector-array member variables.
pub(super) fn select_indices_for_dims(indices: &str, dims_len: usize) -> Option<String> {
    if dims_len == 0 {
        return None;
    }
    let groups = extract_index_groups(indices);
    if groups.is_empty() {
        return None;
    }
    let take = dims_len.min(groups.len());
    Some(groups[groups.len() - take..].concat())
}

/// Parse an index group like `"[2]"` into its integer value.
fn parse_index_group_value(group: &str) -> Option<i64> {
    if group.len() < 3 || !group.starts_with('[') || !group.ends_with(']') {
        return None;
    }
    group[1..group.len() - 1].trim().parse().ok()
}

/// True if projected bracket indices are valid for the corresponding dimensions.
pub(super) fn projected_indices_within_dims(indices: &str, dims: &[i64]) -> bool {
    let groups = extract_index_groups(indices);
    if groups.len() != dims.len() {
        return false;
    }
    groups.iter().zip(dims.iter()).all(|(group, dim)| {
        let Some(idx) = parse_index_group_value(group) else {
            return false;
        };
        *dim >= 1 && idx >= 1 && idx <= *dim
    })
}

/// Scalar size of an array shape, clamping non-positive dims to 1.
pub(super) fn scalar_size_from_dims(dims: &[i64]) -> usize {
    if dims.is_empty() {
        1
    } else {
        dims.iter().copied().map(|d| d.max(1) as usize).product()
    }
}

pub(super) fn extract_suffix_and_indices_for_path(
    parts: &[String],
    path_segments: &[&str],
    path_explicit_index_count: usize,
) -> Option<(String, String)> {
    if parts.len() <= path_segments.len() {
        return None;
    }

    let mut matched_prefix_indices = String::new();
    for (i, segment) in path_segments.iter().enumerate() {
        if i >= parts.len() || !compare_path_part_with_mode(parts[i].as_str(), segment, true) {
            return None;
        }
        append_indices(
            &mut matched_prefix_indices,
            parts[i].as_str(),
            segment,
            true,
        );
    }

    let suffix = parts[path_segments.len()..].join(".");
    let normalized_indices =
        strip_explicit_index_count(&matched_prefix_indices, path_explicit_index_count);
    Some((suffix, normalized_indices))
}

pub(super) fn suffix_indices_key(suffix: &str, normalized_indices: &str) -> String {
    format!("{suffix}\x1f{normalized_indices}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_explicit_index_count() {
        assert_eq!(strip_explicit_index_count("[1][2]", 1), "[2]");
        assert_eq!(strip_explicit_index_count("[1]", 1), "");
        assert_eq!(strip_explicit_index_count("[1][2]", 0), "[1][2]");
    }

    #[test]
    fn test_missing_index_on_last_prefix_segment() {
        assert!(missing_index_on_last_prefix_segment(
            "plug_p.pin[2].i",
            "plug_p.pin"
        ));
        assert!(!missing_index_on_last_prefix_segment(
            "resistor[1].p.i",
            "resistor.p"
        ));
        assert!(missing_index_on_last_prefix_segment(
            "bus[data.medium].pin[2].i",
            "bus[data.medium].pin"
        ));
    }

    #[test]
    fn test_extract_suffix_preserves_connector_element_indices() {
        assert_eq!(
            extract_suffix("resistor[1].p.v", "resistor.p"),
            Some(("v".to_string(), "[1]".to_string()))
        );
        assert_eq!(
            extract_suffix("r1.n.v", "r1.n"),
            Some(("v".to_string(), String::new()))
        );
    }
}
