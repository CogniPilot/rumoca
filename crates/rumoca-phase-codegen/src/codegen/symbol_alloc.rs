use std::collections::{HashMap, HashSet};

use super::*;

pub(super) fn symbol_ref_priority(reference: &str) -> (usize, usize) {
    let (base, _) = split_modelica_subscript(reference);
    let depth = modelica_path_depth(base);
    let indexed = usize::from(reference.contains('['));
    (depth, indexed)
}

#[derive(Clone)]
struct SymbolPolicy {
    reserved: HashSet<String>,
    generated_prefixes: Vec<String>,
    separator: String,
}

impl SymbolPolicy {
    fn language_neutral() -> Self {
        Self {
            reserved: HashSet::new(),
            generated_prefixes: Vec::new(),
            separator: "_".to_string(),
        }
    }

    fn from_value(value: &Value) -> Result<Self, minijinja::Error> {
        let mut policy = Self::language_neutral();

        if let Some(separator) = get_str_attr(value, "separator")
            && !separator.is_empty()
        {
            policy.separator = separator;
        }
        if let Ok(reserved) = value.get_attr("reserved") {
            for item in value_list_strings(&reserved)? {
                policy.reserved.insert(item);
            }
        }
        if let Ok(prefixes) = value.get_attr("generated_prefixes") {
            policy.generated_prefixes = value_list_strings(&prefixes)?;
        }

        Ok(policy)
    }
}

struct SymbolAllocator {
    used: HashSet<String>,
    policy: SymbolPolicy,
}

impl SymbolAllocator {
    fn new(policy: SymbolPolicy) -> Self {
        let used = policy.reserved.clone();
        Self { used, policy }
    }

    fn allocate(
        &mut self,
        candidates: &[String],
        candidate_counts: &HashMap<String, usize>,
    ) -> RenderResult {
        for candidate in candidates {
            let count = candidate_counts.get(candidate).copied().ok_or_else(|| {
                render_err(format!(
                    "symbol candidate `{candidate}` missing from allocation count table"
                ))
            })?;
            if count <= 1 && self.reserve_if_available(candidate) {
                return Ok(candidate.clone());
            }
        }

        for candidate in candidates {
            if self.reserve_if_available(candidate) {
                return Ok(candidate.clone());
            }
        }

        let base = candidates
            .last()
            .ok_or_else(|| render_err("symbol allocation requires at least one candidate"))?;
        let mut idx = 2usize;
        loop {
            let candidate = format!("{base}_{idx}");
            if self.reserve_if_available(&candidate) {
                return Ok(candidate);
            }
            idx = idx.checked_add(1).ok_or_else(|| {
                render_err(format!(
                    "exhausted usize suffix range while allocating a unique codegen name for `{base}`"
                ))
            })?;
        }
    }

    fn reserve_if_available(&mut self, candidate: &str) -> bool {
        if candidate.is_empty()
            || self.used.contains(candidate)
            || self.policy.reserved.contains(candidate)
        {
            return false;
        }
        if self
            .policy
            .generated_prefixes
            .iter()
            .any(|prefix| self.used.contains(&format!("{prefix}{candidate}")))
        {
            return false;
        }

        self.used.insert(candidate.to_string());
        for prefix in &self.policy.generated_prefixes {
            self.used.insert(format!("{prefix}{candidate}"));
        }
        true
    }
}

pub(super) fn allocate_symbols_function(
    symbol_refs: Value,
    policy: Value,
) -> Result<Value, minijinja::Error> {
    let policy = SymbolPolicy::from_value(&policy)?;
    let references = value_list_strings(&symbol_refs)?;
    let symbols = allocate_symbols_for_refs(references, policy)?;
    Ok(Value::from_serialize(symbols))
}

pub(super) fn target_symbols_function(
    symbol_refs: Value,
    policy: Value,
    symbol_aliases: Value,
) -> Result<Value, minijinja::Error> {
    let policy = SymbolPolicy::from_value(&policy)?;
    let references = value_list_strings(&symbol_refs)?;
    let mut requests = render_vec_with_capacity(references.len(), "target symbol request count")?;
    for reference in references {
        let candidates = symbol_candidates(&reference, &policy)?;
        requests.push((reference, candidates));
    }

    requests.sort_by(|(a_ref, a_candidates), (b_ref, b_candidates)| {
        symbol_ref_priority(a_ref)
            .cmp(&symbol_ref_priority(b_ref))
            .then_with(|| {
                a_candidates
                    .first()
                    .map(String::as_str)
                    .unwrap_or("")
                    .cmp(b_candidates.first().map(String::as_str).unwrap_or(""))
            })
            .then_with(|| a_ref.cmp(b_ref))
    });

    let mut candidate_counts = HashMap::<String, usize>::new();
    for (_, candidates) in &requests {
        let mut seen = HashSet::new();
        for candidate in candidates {
            if seen.insert(candidate) {
                *candidate_counts.entry(candidate.clone()).or_insert(0) += 1;
            }
        }
    }

    let mut allocator = SymbolAllocator::new(policy);
    let mut out = IndexMap::new();
    for (reference, candidates) in requests {
        let symbol = allocator.allocate(&candidates, &candidate_counts)?;
        out.insert(reference, symbol);
    }
    for (alias, target) in value_symbol_aliases(&symbol_aliases)? {
        let symbol = out.get(&target).cloned().ok_or_else(|| {
            render_err(format!(
                "symbol alias `{alias}` targets unknown source reference `{target}`"
            ))
        })?;
        out.insert(alias, symbol);
    }
    Ok(Value::from_serialize(out))
}

fn allocate_symbols_for_refs(
    mut references: Vec<String>,
    policy: SymbolPolicy,
) -> Result<IndexMap<String, String>, minijinja::Error> {
    references.sort_by(|a, b| {
        symbol_ref_priority(a)
            .cmp(&symbol_ref_priority(b))
            .then_with(|| a.cmp(b))
    });
    references.dedup();

    let mut requests =
        render_vec_with_capacity(references.len(), "symbol allocation request count")?;
    for reference in references {
        let candidates = symbol_candidates(&reference, &policy)?;
        requests.push((reference, candidates));
    }

    let mut candidate_counts = HashMap::<String, usize>::new();
    for (_, candidates) in &requests {
        let mut seen = HashSet::new();
        for candidate in candidates {
            if seen.insert(candidate) {
                *candidate_counts.entry(candidate.clone()).or_insert(0) += 1;
            }
        }
    }

    let mut allocator = SymbolAllocator::new(policy);
    let mut out = IndexMap::new();
    for (reference, candidates) in requests {
        let symbol = allocator.allocate(&candidates, &candidate_counts)?;
        out.insert(reference, symbol);
    }
    Ok(out)
}

fn symbol_candidates(
    modelica_ref: &str,
    policy: &SymbolPolicy,
) -> Result<Vec<String>, minijinja::Error> {
    let (base_ref, subscript) = split_modelica_subscript(modelica_ref);
    let suffix = subscript
        .map(|value| scalarized_subscript_suffix(value, &policy.separator))
        .transpose()?;
    let segments = split_modelica_path(base_ref)?;
    let mut candidates = render_vec_with_capacity(segments.len(), "symbol candidate count")?;
    for start in (0..segments.len()).rev() {
        let base = readable_path_suffix(&segments[start..], &policy.separator)?;
        if base.is_empty() {
            continue;
        }
        candidates.push(with_optional_suffix(
            &base,
            suffix.as_deref(),
            &policy.separator,
        )?);
    }

    if candidates.is_empty() {
        return Err(render_err(format!(
            "source reference `{modelica_ref}` does not contain any valid identifier segment"
        )));
    }
    candidates.dedup();
    Ok(candidates)
}

fn split_modelica_subscript(reference: &str) -> (&str, Option<&str>) {
    if let Some((base, subscript)) = rumoca_core::split_trailing_subscript_suffix(reference) {
        return (base, Some(subscript));
    }
    (reference, None)
}

fn split_modelica_path(name: &str) -> Result<Vec<&str>, minijinja::Error> {
    let mut segments =
        render_vec_with_capacity(modelica_path_depth(name), "Modelica path segment count")?;
    let mut depth = 0usize;
    let mut start = 0usize;
    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            '.' if depth == 0 => {
                segments.push(&name[start..idx]);
                start = idx + 1;
            }
            _ => {}
        }
    }
    segments.push(&name[start..]);
    Ok(segments)
}

fn modelica_path_depth(name: &str) -> usize {
    let mut depth = 0usize;
    let mut segments = 1usize;
    for ch in name.chars() {
        match ch {
            '[' => depth += 1,
            ']' => depth = depth.saturating_sub(1),
            '.' if depth == 0 => segments += 1,
            _ => {}
        }
    }
    segments
}

fn readable_path_suffix(segments: &[&str], separator: &str) -> Result<String, minijinja::Error> {
    let mut out = String::new();
    for segment in segments {
        let Some(segment) = readable_identifier_segment(segment)? else {
            continue;
        };
        push_joined_string_part(&mut out, &segment, separator, "symbol candidate text")?;
    }
    Ok(out)
}

fn readable_identifier_segment(segment: &str) -> Result<Option<String>, minijinja::Error> {
    let capacity = segment
        .len()
        .checked_add(1)
        .ok_or_else(|| render_err("identifier segment capacity overflows host index range"))?;
    let mut out = render_string_with_capacity(capacity, "identifier segment text")?;
    let mut last_was_underscore = false;
    for ch in segment.chars() {
        let valid = ch.is_ascii_alphanumeric() || ch == '_';
        if valid {
            if out.is_empty() && ch.is_ascii_digit() {
                out.push('_');
            }
            out.push(ch);
            last_was_underscore = ch == '_';
        } else if !last_was_underscore {
            out.push('_');
            last_was_underscore = true;
        }
    }
    while out.ends_with('_') {
        out.pop();
    }
    Ok((!out.is_empty()).then_some(out))
}

fn scalarized_subscript_suffix(
    subscript: &str,
    separator: &str,
) -> Result<String, minijinja::Error> {
    let mut out = String::new();
    for part in subscript.split(',').map(str::trim) {
        if part.is_empty() {
            continue;
        }
        push_joined_string_part(&mut out, part, separator, "scalarized subscript suffix")?;
    }
    Ok(out)
}

fn with_optional_suffix(
    base: &str,
    suffix: Option<&str>,
    separator: &str,
) -> Result<String, minijinja::Error> {
    match suffix {
        Some(suffix) if !suffix.is_empty() => {
            let capacity = base
                .len()
                .checked_add(separator.len())
                .and_then(|len| len.checked_add(suffix.len()))
                .ok_or_else(|| {
                    render_err("symbol candidate capacity overflows host index range")
                })?;
            let mut value = render_string_with_capacity(capacity, "symbol candidate text")?;
            value.push_str(base);
            value.push_str(separator);
            value.push_str(suffix);
            Ok(value)
        }
        _ => {
            let mut value = render_string_with_capacity(base.len(), "symbol candidate text")?;
            value.push_str(base);
            Ok(value)
        }
    }
}

fn push_joined_string_part(
    out: &mut String,
    part: &str,
    separator: &str,
    context: &'static str,
) -> Result<(), minijinja::Error> {
    if out.is_empty() {
        reserve_render_string_capacity(out, part.len(), context)?;
        out.push_str(part);
    } else {
        let additional = separator
            .len()
            .checked_add(part.len())
            .ok_or_else(|| render_err(format!("{context} capacity overflows host index range")))?;
        reserve_render_string_capacity(out, additional, context)?;
        out.push_str(separator);
        out.push_str(part);
    }
    Ok(())
}

pub(super) fn symbol_function(symbols: Value, name: Value) -> RenderResult {
    let name = value_to_string(&name);
    lookup_symbol_value(Some(&symbols), &name).ok_or_else(|| {
        render_err(format!(
            "missing emitted symbol for source reference `{name}`"
        ))
    })
}

pub(super) fn lookup_symbol_value(symbols: Option<&Value>, name: &str) -> Option<String> {
    let symbols = symbols?;
    symbols
        .get_item(&Value::from(name))
        .ok()
        .filter(|value| !value.is_undefined() && !value.is_none())
        .map(|value| value_to_string(&value))
        .filter(|value| !value.is_empty())
}

pub(super) fn emitted_symbol(reference: &str, cfg: &ExprConfig) -> RenderResult {
    if let Some(symbols) = cfg.symbols.as_ref() {
        return lookup_symbol_value(Some(symbols), reference).ok_or_else(|| {
            render_err(format!(
                "missing emitted symbol for source reference `{reference}`"
            ))
        });
    }
    if cfg.sanitize_dots || cfg.subscript_underscore {
        Ok(sanitize_name(reference))
    } else {
        Ok(escape_reserved_keyword(reference))
    }
}
