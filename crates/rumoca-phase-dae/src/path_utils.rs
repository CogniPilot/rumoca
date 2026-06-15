pub(crate) use rumoca_core::{
    get_top_level_prefix, has_top_level_subscript, is_top_level_member,
    last_top_level_subscript_span, normalize_top_level_segment, normalized_top_level_names,
    path_is_in_top_level_set, strip_all_subscripts, subscript_fallback_chain,
};

use std::collections::HashSet;

/// True when the rendered name is nested below a component (has a top-level dot).
pub(crate) fn is_nested_name(name: &str) -> bool {
    rumoca_core::has_top_level_dot(name)
}

/// Final top-level segment of a rendered name.
pub(crate) fn leaf_segment(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}

/// First top-level segment of a rendered name, keeping any subscript text.
pub(crate) fn first_rendered_segment(name: &str) -> Option<&str> {
    rumoca_core::rendered_top_level_segment(name)
}

pub(crate) fn resolve_known_path_suffix(
    name: &str,
    known_names: &HashSet<String>,
) -> Option<String> {
    if known_names.contains(name) {
        return Some(name.to_string());
    }

    let path = rumoca_core::ComponentPath::from_flat_path(name);
    path.suffixes_excluding_self()
        .map(|suffix| suffix.to_flat_string())
        .find(|candidate| known_names.contains(candidate))
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
            first_rendered_segment("bus[data.medium].pin"),
            Some("bus[data.medium]")
        );
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
            &rumoca_core::VarName::new("plug.pin.i"),
            &normalized
        ));
        assert!(!is_top_level_member(
            &rumoca_core::VarName::new("plug"),
            &normalized
        ));
        assert!(
            !is_top_level_member(&rumoca_core::VarName::new("plug[data.medium]"), &normalized),
            "dot inside a bracketed index does not make a top-level member path"
        );
    }

    #[test]
    fn test_subscript_fallback_chain_peels_all_subscript_layers() {
        let chain = subscript_fallback_chain("a[1].b[2].c[3]");
        assert_eq!(
            chain,
            vec![
                rumoca_core::VarName::new("a[1].b[2].c"),
                rumoca_core::VarName::new("a[1].b.c"),
                rumoca_core::VarName::new("a.b.c")
            ]
        );
    }

    #[test]
    fn test_strip_all_subscripts_normalizes_embedded_indices() {
        assert_eq!(strip_all_subscripts("pc[((2*1)-1)].i"), "pc.i");
        assert_eq!(strip_all_subscripts("pin_n[1].v"), "pin_n.v");
        assert_eq!(strip_all_subscripts("R[1].w"), "R.w");
    }

    #[test]
    fn test_resolve_known_path_suffix_prefers_nearest_known_suffix() {
        let known = HashSet::from([
            "aimcData.statorCoreParameters.wRef".to_string(),
            "statorCoreParameters.wRef".to_string(),
        ]);

        assert_eq!(
            resolve_known_path_suffix("aimc.aimcData.statorCoreParameters.wRef", &known).as_deref(),
            Some("aimcData.statorCoreParameters.wRef")
        );
    }
}
