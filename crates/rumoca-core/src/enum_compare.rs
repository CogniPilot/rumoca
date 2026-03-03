//! Shared enumeration comparison helpers for compile-time evaluators.
//!
//! MLS §4.9.5 identifies enum literals by type + literal identity. During
//! compilation, enum values can appear with different qualification prefixes
//! (for example, imported short names vs fully qualified names).

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

    if dotted_boundary_suffix_match(lhs, rhs) || dotted_boundary_suffix_match(rhs, lhs) {
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

fn dotted_boundary_suffix_match(longer: &str, shorter: &str) -> bool {
    if shorter.is_empty() || longer.len() <= shorter.len() {
        return false;
    }
    longer
        .strip_suffix(shorter)
        .is_some_and(|prefix| prefix.ends_with('.'))
}

fn enum_type_and_literal_tail(path: &str) -> Option<(&str, &str)> {
    let (prefix, literal) = path.rsplit_once('.')?;
    let enum_type = prefix.rsplit_once('.').map_or(prefix, |(_, ty)| ty);
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
}
