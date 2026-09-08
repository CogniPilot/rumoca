//! Path/index matching helpers for connector-connection expansion.
//!
//! Keeps string/index normalization logic isolated from connection-set orchestration.

use crate::path_utils::{segments as path_segments_of, strip_array_index};
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
    match (extract_array_index(segment), extract_array_index(name_part)) {
        (Some(segment_idx), Some(name_idx)) => name_idx == segment_idx,
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
    let prefix_segments = path_segments_of(prefix);
    let name_parts = path_segments_of(full_name);

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

    let suffix = name_parts[prefix_segments.len()..].join(".");
    Some((suffix, indices))
}

/// Extract array index from a path part.
///
/// "resistor[1]" -> Some("[1]")
/// "p" -> None
pub(super) fn extract_array_index(s: &str) -> Option<String> {
    let base = strip_array_index(s);
    let suffix = s.get(base.len()..)?;
    (!suffix.is_empty() && balanced_index_groups(suffix).is_some()).then(|| suffix.to_string())
}

/// Collect every concrete array coordinate introduced by expanding `pattern`.
///
/// Indices already written in `pattern` are fixed selections and therefore do
/// not contribute axes to the expanded value. Every index added by `full_name`
/// does, including comma-separated coordinates and indices on more than one
/// path segment.
pub(super) fn expanded_coordinates_for_pattern(
    full_name: &str,
    pattern: &str,
) -> Result<Vec<i64>, &'static str> {
    let full_segments = path_segments_of(full_name);
    let pattern_segments = path_segments_of(pattern);
    if full_segments.len() != pattern_segments.len() {
        return Err("expanded member and endpoint pattern have different path ranks");
    }

    let mut coordinates = Vec::new();
    for (full, pattern) in full_segments.iter().zip(pattern_segments) {
        if strip_array_index(full) != strip_array_index(pattern) {
            return Err("expanded member does not match its endpoint pattern");
        }
        let pattern_indices = extract_array_index(pattern).unwrap_or_default();
        let pattern_coordinates = literal_coordinates(&pattern_indices)
            .ok_or("endpoint pattern indices are not concrete scalar Integers")?;
        let full_indices = extract_array_index(full).unwrap_or_default();
        let full_coordinates = literal_coordinates(&full_indices)
            .ok_or("expanded member indices are not concrete scalar Integers")?;
        if !full_coordinates.starts_with(&pattern_coordinates) {
            return Err("expanded member does not preserve fixed endpoint indices");
        }
        coordinates.extend_from_slice(&full_coordinates[pattern_coordinates.len()..]);
    }
    Ok(coordinates)
}

pub(super) fn literal_coordinates(indices: &str) -> Option<Vec<i64>> {
    if indices.is_empty() {
        return Some(Vec::new());
    }
    let mut coordinates = Vec::new();
    for group in balanced_index_groups(indices)? {
        coordinates.extend(parse_literal_index_group_values(group)?);
    }
    Some(coordinates)
}

pub(super) fn split_trailing_index_groups(path: &str) -> Option<(String, Vec<String>)> {
    let bytes = path.as_bytes();
    let mut end = bytes.len();
    if bytes.last().copied() != Some(b']') {
        return None;
    }

    let mut groups_rev: Vec<String> = Vec::new();
    while end > 0 && bytes[end - 1] == b']' {
        let start = find_trailing_group_start(bytes, end)?;
        groups_rev.push(path[start..end].to_string());
        end = start;
    }

    if groups_rev.is_empty() || end == 0 {
        return None;
    }

    let mut groups = groups_rev;
    groups.reverse();
    Some((path[..end].to_string(), groups))
}

fn find_trailing_group_start(bytes: &[u8], end: usize) -> Option<usize> {
    let mut depth = 0usize;
    for i in (0..end - 1).rev() {
        match bytes[i] {
            b']' => depth += 1,
            b'[' if depth == 0 => return Some(i),
            b'[' => depth = depth.checked_sub(1)?,
            _ => {}
        }
    }
    None
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
    var: &rumoca_core::VarName,
    path: &str,
    flat: &flat::Model,
) -> rumoca_core::VarName {
    if !flat.variables.contains_key(var) {
        return var.clone();
    }
    let path_segments = path_segments_of(path);
    let var_parts = path_segments_of(var.as_str());
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
        return rumoca_core::VarName::new(format!("{}{}", var.as_str(), idx));
    }
    var.clone()
}

/// True when any segment in a connector path contains an explicit array index.
pub(super) fn path_has_explicit_index(path: &str) -> bool {
    path_segments_of(path)
        .iter()
        .any(|segment| extract_array_index(segment).is_some())
}

/// Extract bracketed index groups from an index pattern string.
///
/// Example: `"[1][2]" -> vec!["[1]", "[2]"]`.
fn extract_index_groups(indices: &str) -> Vec<String> {
    if indices.is_empty() {
        return Vec::new();
    }
    balanced_index_groups(indices)
        .expect("index pattern must contain balanced bracket groups")
        .into_iter()
        .map(str::to_string)
        .collect()
}

fn balanced_index_groups(indices: &str) -> Option<Vec<&str>> {
    let mut groups = Vec::new();
    let mut cursor = 0usize;
    while cursor < indices.len() {
        if !indices[cursor..].starts_with('[') {
            return None;
        }
        let end = matching_index_group_end(indices, cursor)?;
        groups.push(&indices[cursor..end]);
        cursor = end;
    }
    (!groups.is_empty()).then_some(groups)
}

fn matching_index_group_end(text: &str, start: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (offset, ch) in text[start..].char_indices() {
        let idx = start + offset;
        match ch {
            '[' => depth += 1,
            ']' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return (idx > start + 1).then_some(idx + ch.len_utf8());
                }
            }
            _ => {}
        }
    }
    None
}

/// Remove index groups that are fixed by explicit indices in `path`.
///
/// Example:
/// - `indices="[1][2]"`, `path="s[1].p"` -> `"[2]"`
/// - `indices="[1]"`, `path="resistor[1].p"` -> `""`
pub(super) fn strip_explicit_path_indices(indices: &str, path: &str) -> String {
    let explicit_count = path_segments_of(path)
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

/// Parse one bracket group containing scalar literal coordinates.
///
/// Modelica permits both `a[1,2]` and nested array indexing such as
/// `a[1][2]`. Callers that reason about array rank must count the two
/// coordinates in the first form separately rather than treating the whole
/// bracket group as one dimension.
pub(super) fn parse_literal_index_group_values(group: &str) -> Option<Vec<i64>> {
    let groups = balanced_index_groups(group)?;
    let [group] = groups.as_slice() else {
        return None;
    };
    let values: Option<Vec<i64>> = group[1..group.len() - 1]
        .split(',')
        .map(|value| value.trim().parse().ok())
        .collect();
    values.filter(|values| !values.is_empty())
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

    #[test]
    fn extract_array_index_requires_balanced_index_suffix() {
        assert_eq!(extract_array_index("resistor[1]"), Some("[1]".to_string()));
        assert_eq!(
            extract_array_index("pin[index.with.dot][2]"),
            Some("[index.with.dot][2]".to_string())
        );
        assert_eq!(extract_array_index("p"), None);
        assert_eq!(extract_array_index("pin[1"), None);
        assert_eq!(extract_array_index("pin[1]tail"), None);
        assert_eq!(extract_array_index("pin[]"), None);
    }

    #[test]
    fn split_trailing_index_groups_rejects_malformed_trailing_groups() {
        assert_eq!(
            split_trailing_index_groups("connector.field[2][3]"),
            Some((
                "connector.field".to_string(),
                vec!["[2]".to_string(), "[3]".to_string()]
            ))
        );
        assert_eq!(
            split_trailing_index_groups("cell[1].field[2]"),
            Some(("cell[1].field".to_string(), vec!["[2]".to_string()]))
        );
        assert_eq!(split_trailing_index_groups("connector.field[2]tail"), None);
        assert_eq!(split_trailing_index_groups("connector.field[2"), None);
        assert_eq!(split_trailing_index_groups("[2]"), None);
    }

    #[test]
    fn expanded_coordinates_preserve_every_unfixed_axis() {
        assert_eq!(
            expanded_coordinates_for_pattern(
                "bank[1].sensor[2,3].channel[4][5].v",
                "bank.sensor.channel.v"
            ),
            Ok(vec![1, 2, 3, 4, 5])
        );
        assert_eq!(
            expanded_coordinates_for_pattern("bank[9,8].sensor[2].v", "bank[9].sensor.v"),
            Ok(vec![8, 2])
        );
        assert!(expanded_coordinates_for_pattern("bank[i].sensor.v", "bank.sensor.v").is_err());
    }
}
