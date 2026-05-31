//! Shared path/subscript helpers for flatten internals.

pub(crate) use rumoca_core::{
    find_last_top_level_dot, first_path_segment_without_index, has_top_level_dot, parent_scope,
    split_first_top_level, split_last_top_level, split_path_with_indices, strip_array_index,
    top_level_last_segment,
};

pub(crate) fn unindexed_lookup_variants(path: &str) -> Vec<String> {
    let segments = split_path_with_indices(path);
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
