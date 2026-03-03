/// Return a component-path base name with all bracketed subscripts removed.
///
/// Examples:
/// - `x[1]` -> `x`
/// - `support[1].phi` -> `support.phi`
/// - `a[1].b[2].c` -> `a.b.c`
pub fn component_base_name(name: &str) -> Option<String> {
    let mut parts = Vec::new();
    let mut segment_start = 0usize;
    let mut depth = 0i32;

    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            '.' if depth == 0 => {
                if segment_start >= idx {
                    return None;
                }
                let segment = &name[segment_start..idx];
                let base = segment
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(segment);
                if base.is_empty() {
                    return None;
                }
                parts.push(base.to_string());
                segment_start = idx + 1;
            }
            _ => {}
        }
    }

    if depth != 0 || segment_start >= name.len() {
        return None;
    }
    let tail = &name[segment_start..];
    let base = tail.split_once('[').map(|(base, _)| base).unwrap_or(tail);
    if base.is_empty() {
        return None;
    }
    parts.push(base.to_string());
    Some(parts.join("."))
}

#[cfg(test)]
mod tests {
    use super::component_base_name;

    #[test]
    fn test_component_base_name_simple() {
        assert_eq!(component_base_name("x").as_deref(), Some("x"));
        assert_eq!(component_base_name("x[1]").as_deref(), Some("x"));
        assert_eq!(component_base_name("x[1][2]").as_deref(), Some("x"));
    }

    #[test]
    fn test_component_base_name_mid_path() {
        assert_eq!(
            component_base_name("support[1].phi").as_deref(),
            Some("support.phi")
        );
        assert_eq!(component_base_name("a[1].b[2].c").as_deref(), Some("a.b.c"));
        assert_eq!(
            component_base_name("foo.bar[3].baz").as_deref(),
            Some("foo.bar.baz")
        );
    }

    #[test]
    fn test_component_base_name_rejects_malformed() {
        assert_eq!(component_base_name("x["), None);
        assert_eq!(component_base_name("x].y"), None);
        assert_eq!(component_base_name(".x"), None);
        assert_eq!(component_base_name("x..y"), None);
    }
}
