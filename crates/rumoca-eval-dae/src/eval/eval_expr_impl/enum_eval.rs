use super::*;

pub(super) fn lookup_enum_literal_ordinal(
    raw: &str,
    ordinals: &IndexMap<String, i64>,
) -> Option<i64> {
    if let Some(&ordinal) = ordinals.get(raw) {
        return Some(ordinal);
    }
    let mut raw_parts = rumoca_core::ComponentPath::from_flat_path(raw).into_parts();
    if raw_parts.len() < 2 {
        return None;
    }
    let literal = raw_parts.pop()?;
    let literal = literal.as_str();
    let prefix = raw_parts.join(".");
    if let Some(unquoted) = strip_quoted_identifier(literal) {
        let alt = format!("{prefix}.{unquoted}");
        return ordinals
            .get(&alt)
            .copied()
            .or_else(|| lookup_enum_literal_by_unambiguous_suffix(literal, ordinals));
    }
    let alt = format!("{prefix}.'{literal}'");
    ordinals
        .get(&alt)
        .copied()
        .or_else(|| lookup_enum_literal_by_unambiguous_suffix(literal, ordinals))
}

pub(super) fn strip_quoted_identifier(segment: &str) -> Option<&str> {
    if segment.len() >= 2 && segment.starts_with('\'') && segment.ends_with('\'') {
        Some(&segment[1..segment.len() - 1])
    } else {
        None
    }
}

fn lookup_enum_literal_by_unambiguous_suffix(
    raw_literal: &str,
    ordinals: &IndexMap<String, i64>,
) -> Option<i64> {
    let target = canonical_enum_literal_segment(raw_literal);
    let mut found = None;
    for (name, ordinal) in ordinals {
        let path = rumoca_core::ComponentPath::from_flat_path(name);
        let Some(literal) = (path.len() >= 2).then(|| path.parts().last()).flatten() else {
            continue;
        };
        if canonical_enum_literal_segment(literal) != target {
            continue;
        }
        match found {
            Some(existing) if existing != *ordinal => return None,
            Some(_) => {}
            None => found = Some(*ordinal),
        }
    }
    found
}

fn canonical_enum_literal_segment(segment: &str) -> &str {
    strip_quoted_identifier(segment).unwrap_or(segment)
}
