//! Shared enumeration comparison helpers for compile-time evaluators.
//!
//! MLS §4.9.5 identifies enum literals by type + literal identity. During
//! compilation, enum values can appear with different qualification prefixes
//! (for example, imported short names vs fully qualified names).

use crate::{parent_scope, split_last_top_level, top_level_last_segment, top_level_path_ends_with};

/// Compare two enum value paths with qualification-tolerant semantics.
///
/// Accepted equal forms include:
/// - exact match: `Type.Lit` == `Type.Lit`
/// - boundary suffix match: `A.B.Type.Lit` == `Type.Lit`
/// - shared `<EnumType>.<Literal>` tail: `X.Y.Type.Lit` == `P.Q.Type.Lit`
pub fn enum_values_equal(lhs: &str, rhs: &str) -> bool {
    if lhs == rhs {
        return true;
    }

    if top_level_path_ends_with(lhs, rhs) || top_level_path_ends_with(rhs, lhs) {
        return true;
    }

    let Some((lhs_type, lhs_literal)) = enum_type_and_literal_tail(lhs) else {
        return false;
    };
    let Some((rhs_type, rhs_literal)) = enum_type_and_literal_tail(rhs) else {
        return false;
    };

    lhs_type == rhs_type && lhs_literal == rhs_literal
}

fn enum_type_and_literal_tail(path: &str) -> Option<(&str, &str)> {
    let (prefix, literal) = split_last_top_level(path)?;
    let enum_type = parent_scope(prefix).map_or(prefix, |_| top_level_last_segment(prefix));
    Some((enum_type, literal))
}

#[cfg(test)]
mod tests {
    use super::enum_values_equal;

    #[test]
    fn enum_equal_exact_and_suffix_forms() {
        assert!(enum_values_equal(
            "SimpleController.PI",
            "SimpleController.PI"
        ));
        assert!(enum_values_equal(
            "Modelica.Blocks.Types.SimpleController.PI",
            "SimpleController.PI"
        ));
    }

    #[test]
    fn enum_equal_accepts_shared_type_literal_tail() {
        assert!(enum_values_equal(
            "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve",
            "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve"
        ));
    }

    #[test]
    fn enum_equal_rejects_different_type_or_literal() {
        assert!(!enum_values_equal("PkgA.Mode.On", "PkgB.Other.On"));
        assert!(!enum_values_equal("PkgA.Mode.On", "PkgA.Mode.Off"));
    }

    #[test]
    fn enum_equal_suffix_match_requires_top_level_segment_boundary() {
        assert!(enum_values_equal(
            "a[index.with.dot].SimpleController.PI",
            "SimpleController.PI"
        ));
        assert!(!enum_values_equal(
            "Pkg.SimpleController.PI",
            "Controller.PI"
        ));
        assert!(!enum_values_equal(
            "Pkg.SimpleController.PI",
            "Simple.Controller.PI"
        ));
    }
}
