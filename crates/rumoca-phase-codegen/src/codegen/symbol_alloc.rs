use std::collections::{HashMap, HashSet};

use super::*;

fn symbol_ref_priority(reference: &str) -> (usize, usize) {
    let (base, _) = split_modelica_subscript(reference);
    let depth = modelica_path_depth(base);
    let indexed = usize::from(reference.contains('['));
    (depth, indexed)
}

/// The order references claim their spellings in: shortest, least qualified
/// first, and references that can only be spelled inside the generated
/// namespace last.
///
/// Deferral is the leading component, outranking depth and every component
/// after it, and that is what stops a prefix-spelled reference from stealing a
/// legitimate name. `rumoca_galec_x` cannot keep its own spelling, so it falls
/// back to the stripped `x`; going first — which ordering by depth and name
/// alone does, both references being short and unqualified — it would take `x`
/// and push the source variable actually named `x`, which collides with
/// nothing, to `x_2`. Deferring every such reference means the stripped
/// spelling is only ever taken after the references entitled to it have
/// theirs, and the prefix-spelled one lands on the numbered spelling instead.
fn allocation_order<'a>(
    reference: &'a str,
    candidates: &[String],
    policy: &SymbolPolicy,
) -> (usize, usize, usize, &'a str) {
    let (depth, indexed) = symbol_ref_priority(reference);
    let deferred = usize::from(policy.only_generated_spellings(candidates));
    (deferred, depth, indexed, reference)
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
            // An empty prefix names no generated spelling, and keeping it
            // would reject every candidate below.
            policy
                .generated_prefixes
                .retain(|prefix| !prefix.is_empty());
        }

        Ok(policy)
    }

    /// The generated prefix a candidate is spelled inside, if any.
    ///
    /// A target emits its own `<prefix>...` identifiers (loop counters, output
    /// copies, helper functions, status fields) without allocating them, so
    /// that whole namespace belongs to the generator, not to source names.
    fn generated_prefix_of(&self, candidate: &str) -> Option<&str> {
        self.generated_prefixes
            .iter()
            .map(String::as_str)
            .find(|prefix| candidate.starts_with(prefix))
    }

    /// Whether every member of the numbered family `<base>_2`, `<base>_3`, ...
    /// is outside the generated namespace.
    ///
    /// The family shares the stem `<base>_`, so it is enough that the stem and
    /// every generated prefix be prefix-INCOMPARABLE: neither one a prefix of
    /// the other. A prefix no longer than the stem can start a family member
    /// only by starting the stem; a longer one only by being started by the
    /// stem. Both are ruled out, so no index can re-enter the namespace.
    ///
    /// The second direction is deliberately conservative. A prefix that
    /// extends the stem (stem `x_` against a prefix `x_2`) would swallow only
    /// the indices spelled with its tail, so rejecting such a base gives up a
    /// usable family — but it keeps the guarantee an all-or-nothing property
    /// of the base instead of a per-index search, which is what lets the
    /// numbered loop run without a second escape hatch.
    ///
    /// `base == "rumoca_galec"` with the prefix `rumoca_galec_` is exactly the
    /// case this rejects: the base is allocatable (it lacks the trailing
    /// separator) while every numbered spelling built from it is not.
    fn numbered_family_escapes_generated_prefixes(&self, base: &str) -> bool {
        let stem = format!("{base}{FALLBACK_INDEX_SEPARATOR}");
        !self
            .generated_prefixes
            .iter()
            .any(|prefix| stem.starts_with(prefix.as_str()) || prefix.starts_with(&stem))
    }

    /// Whether a reference can only be spelled inside the generated namespace,
    /// so that allocating it must fall back to a stripped spelling.
    fn only_generated_spellings(&self, candidates: &[String]) -> bool {
        candidates
            .iter()
            .all(|candidate| self.generated_prefix_of(candidate).is_some())
    }
}

/// The separator the numbered fallback puts between a base and its index.
///
/// Fixed rather than taken from `SymbolPolicy::separator`: the termination
/// argument in [`SymbolAllocator::allocate`] reasons about the exact spelling
/// of the numbered family, and `fallback_base` proves its premise against this
/// one stem.
const FALLBACK_INDEX_SEPARATOR: &str = "_";

/// The escape `fallback_base` applies when the numbered family would re-enter
/// the generated namespace.
///
/// A leading underscore, the same escape `readable_identifier_segment` already
/// uses for a leading digit, so this introduces no spelling the target could
/// not emit before. Prepending is what makes it effective: the numbering only
/// appends, so a character in front of the base cannot be undone by it, and a
/// prefix that starts with the base no longer starts the escaped spelling.
/// Whether the escape actually cleared the namespace is checked, not assumed.
const FALLBACK_BASE_ESCAPE: &str = "_";

struct SymbolAllocator {
    used: HashSet<String>,
    policy: SymbolPolicy,
}

impl SymbolAllocator {
    fn new(policy: SymbolPolicy) -> Self {
        let used = policy.reserved.clone();
        Self { used, policy }
    }

    /// Allocate one unique spelling, preferring the caller's candidates and
    /// falling back to a numbered spelling.
    ///
    /// # Termination
    ///
    /// The candidate loops are finite. The numbered loop draws from the
    /// infinite family `<base>_2`, `<base>_3`, ... whose members are pairwise
    /// distinct, and `fallback_base` returns a base for which
    /// [`SymbolPolicy::numbered_family_escapes_generated_prefixes`] holds (or
    /// fails closed), so the generated-prefix rule rejects no member of that
    /// family. Every remaining rejection in `reserve_if_available` is
    /// witnessed by an entry of `used` — the candidate itself, or
    /// `<prefix><candidate>` for one of the generated prefixes — and the
    /// candidate is recoverable from that witness together with the prefix, so
    /// distinct candidates consume distinct (witness, prefix) pairs. `used`
    /// does not grow while the loop runs, because `reserve_if_available`
    /// inserts only on the iteration that returns. The loop therefore rejects
    /// at most `used.len() * (generated_prefixes.len() + 1)` indices and
    /// returns on the next one.
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
        let base = self.fallback_base(base)?;
        if self.reserve_if_available(&base) {
            return Ok(base);
        }
        let mut idx = 2usize;
        loop {
            let candidate = format!("{base}{FALLBACK_INDEX_SEPARATOR}{idx}");
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

    /// The base the numbered fallback counts from.
    ///
    /// The numbering only appends, so the base decides once and for all
    /// whether the family `<base>_2`, `<base>_3`, ... can ever leave the
    /// generated namespace. Two distinct hazards have to be cleared, and
    /// clearing both is what makes [`SymbolAllocator::allocate`] terminate:
    ///
    /// 1. The base is itself spelled inside a generated prefix
    ///    (`rumoca_galec_x`). Every suffixed spelling keeps that prefix, so
    ///    the leading generated prefixes are stripped, which moves the name
    ///    out of the generator's namespace.
    /// 2. The base only borders the namespace (`rumoca_galec`, one separator
    ///    short of the prefix `rumoca_galec_`). The base is allocatable, but
    ///    `rumoca_galec_2` and every sibling is not. Stripping does nothing
    ///    here — nothing is spelled inside a prefix yet — so the base is
    ///    escaped instead, by prepending [`FALLBACK_BASE_ESCAPE`].
    ///
    /// The escape is applied at most once and then verified: a policy whose
    /// prefixes also swallow the escaped spelling fails closed rather than
    /// looping. Any collision with a real source name that survives is a plain
    /// duplicate, resolved by the numbering exactly as for any other.
    fn fallback_base(&self, base: &str) -> Result<String, minijinja::Error> {
        let mut fallback = base;
        while let Some(prefix) = self.policy.generated_prefix_of(fallback) {
            let stripped = &fallback[prefix.len()..];
            if stripped.is_empty() {
                break;
            }
            fallback = stripped;
        }
        // Mirrors the leading-digit rule of `readable_identifier_segment`: a
        // stripped tail may start with a digit, which no C-like target accepts
        // as the first character of an identifier.
        let fallback = if fallback.starts_with(|ch: char| ch.is_ascii_digit()) {
            format!("{FALLBACK_BASE_ESCAPE}{fallback}")
        } else {
            fallback.to_owned()
        };
        // Hazard 2: stripping cannot help a base that merely borders the
        // namespace, so escape it, once.
        let escaped = if self
            .policy
            .numbered_family_escapes_generated_prefixes(&fallback)
        {
            fallback
        } else {
            format!("{FALLBACK_BASE_ESCAPE}{fallback}")
        };
        if !self
            .policy
            .numbered_family_escapes_generated_prefixes(&escaped)
        {
            return Err(render_err(format!(
                "cannot allocate a codegen name for `{base}` outside the generated \
                 prefix namespace"
            )));
        }
        Ok(escaped)
    }

    fn reserve_if_available(&mut self, candidate: &str) -> bool {
        if candidate.is_empty()
            || self.used.contains(candidate)
            || self.policy.reserved.contains(candidate)
        {
            return false;
        }
        // The generated prefixes are the target's own namespace: templates emit
        // `<prefix>...` identifiers verbatim without allocating them, so a
        // source name spelled that way would silently become one of them (an
        // array-copy loop counter shadowing a same-named local, for example).
        if self.policy.generated_prefix_of(candidate).is_some() {
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

fn allocate_symbols_for_refs(
    mut references: Vec<String>,
    policy: SymbolPolicy,
) -> Result<IndexMap<String, String>, minijinja::Error> {
    // Sorted only so that `dedup` sees every repeat: a reference allocated
    // twice would burn a second spelling and keep the later one.
    references.sort();
    references.dedup();

    let mut requests =
        render_vec_with_capacity(references.len(), "symbol allocation request count")?;
    for reference in references {
        let candidates = symbol_candidates(&reference, &policy)?;
        requests.push((reference, candidates));
    }
    requests.sort_by(|(a_ref, a_candidates), (b_ref, b_candidates)| {
        allocation_order(a_ref, a_candidates, &policy).cmp(&allocation_order(
            b_ref,
            b_candidates,
            &policy,
        ))
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
