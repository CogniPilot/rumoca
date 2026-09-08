/// Structured view of a flattened scalar name such as `x[1,2]`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScalarNameRef<'a> {
    pub base: &'a str,
    pub indices: Vec<i64>,
}

/// Parse a flattened scalar name into its base name and integer subscripts.
pub fn parse_scalar_name(name: &str) -> Option<ScalarNameRef<'_>> {
    let (base, raw_indices) = split_trailing_subscript_suffix(name)?;
    let indices = parse_scalar_indices(raw_indices)?;
    (!indices.is_empty()).then_some(ScalarNameRef { base, indices })
}

/// Return the base name for a flattened scalar name.
pub fn strip_scalar_name_subscripts(name: &str) -> Option<&str> {
    parse_scalar_name(name).map(|scalar| scalar.base)
}

/// Return the base before a syntactic trailing subscript suffix.
///
/// This is intentionally broader than [`strip_scalar_name_subscripts`]: state
/// detection must recognize `der(x[2:n])` even though `2:n` is not a scalar
/// integer index list.
pub fn strip_trailing_subscript_suffix(name: &str) -> Option<&str> {
    if let Some(base) = strip_scalar_name_subscripts(name) {
        return Some(base);
    }
    split_trailing_subscript_suffix(name).map(|(base, _subscript)| base)
}

/// Split the final syntactic subscript suffix from a Modelica-style reference.
///
/// This recognizes a balanced trailing bracket group without requiring integer
/// scalar indices, so display/codegen boundaries can preserve text such as
/// `a[i + 1]` while still ignoring dots or brackets inside earlier segments.
pub fn split_trailing_subscript_suffix(name: &str) -> Option<(&str, &str)> {
    if !name.ends_with(']') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, ch) in name.char_indices().rev() {
        match ch {
            ']' => depth += 1,
            '[' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    let body = &name[idx + 1..name.len() - 1];
                    let base = &name[..idx];
                    return valid_trailing_subscript_split(base, body).then_some((base, body));
                }
            }
            _ => {}
        }
    }
    None
}

fn valid_trailing_subscript_split(base: &str, body: &str) -> bool {
    !body.trim().is_empty() && !base.is_empty() && has_balanced_subscripts(base)
}

fn parse_scalar_indices(raw_indices: &str) -> Option<Vec<i64>> {
    raw_indices
        .split(',')
        .map(str::trim)
        .map(str::parse::<i64>)
        .collect::<Result<Vec<_>, _>>()
        .ok()
}

fn has_balanced_subscripts(name: &str) -> bool {
    let mut depth = 0usize;
    for ch in name.chars() {
        match ch {
            '[' => depth += 1,
            ']' => {
                let Some(next_depth) = depth.checked_sub(1) else {
                    return false;
                };
                depth = next_depth;
            }
            _ => {}
        }
    }
    depth == 0
}
