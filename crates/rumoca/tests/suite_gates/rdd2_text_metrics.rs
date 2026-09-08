//! Pure-text metrics over emitted C, plus their unit tests.
//!
//! This module is deliberately NOT behind the `rdd2-metric-gates` feature. Its
//! functions need no model corpus, no release binary and no filesystem -- they
//! are string analysis over C text -- so gating them would mean the detectors'
//! own correctness tests compiled only under a feature that nothing in CI or
//! `xtask` ever enables. A pin that nothing exercises is not a pin. The
//! future measured ceilings that need a corpus belong to their authenticated
//! product gate; these detectors remain independently exercised here.
//!
//! The tests at the bottom are the real contract: every case is a spelling
//! that an earlier revision of this code got wrong, so the detectors cannot
//! quietly regress to counting substrings.
//!
//! Scope limitation: these scanners consume the emitted C dialect and do not
//! skip comments or string literals. The emitter contract therefore excludes
//! metric tokens such as `(void)` and `for (` from generated prose.

use std::collections::BTreeSet;

/// Count non-overlapping occurrences of `needle` in `haystack`.
pub(super) fn count_occurrences(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

/// Count `for` loop headers, tolerating `for(` as well as `for (` so a
/// formatting change cannot silently zero the metric.
pub(super) fn count_for_loops(source: &str) -> usize {
    let mut count = 0;
    let mut search_from = 0;
    while let Some(rel) = source[search_from..].find("for") {
        let start = search_from + rel;
        search_from = start + 3;
        let preceded_by_ident = source[..start]
            .chars()
            .next_back()
            .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');
        if preceded_by_ident {
            continue;
        }
        if source[skip_whitespace(source, start + 3)..].starts_with('(') {
            count += 1;
        }
    }
    count
}

/// Whether `segment` is a bare name or member hop (`x`, `.field`, `->field`).
/// A leading hop is only legal after a subscript already closed, which is what
/// `after_subscript` says.
pub(super) fn is_member_path(segment: &str, after_subscript: bool) -> bool {
    if segment.is_empty() {
        return after_subscript;
    }
    let normalized = segment.replace("->", ".");
    let plain = normalized
        .bytes()
        .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'.');
    let head_ok = match normalized.as_bytes().first() {
        Some(b'.') => after_subscript,
        Some(byte) => byte.is_ascii_alphabetic() || *byte == b'_',
        None => false,
    };
    plain && head_ok
}

/// Whether `text` is an lvalue built only from a name, member hops, and
/// literal integer subscripts, ending in a subscript: `a[0]`, `s->buf[3][1]`,
/// `a[0].b[1]`. Anything with an operator, a call, a cast, or a non-literal
/// subscript is rejected, so the detector never counts real computation.
pub(super) fn is_literal_indexed_lvalue(text: &str) -> bool {
    let text = text.trim();
    if !text.ends_with(']') {
        return false;
    }
    let mut saw_subscript = false;
    let mut rest = text;
    while let Some(open) = rest.find('[') {
        if !is_member_path(&rest[..open], saw_subscript) {
            return false;
        }
        let tail = &rest[open + 1..];
        let Some(close) = tail.find(']') else {
            return false;
        };
        let subscript = &tail[..close];
        if subscript.is_empty() || !subscript.bytes().all(|byte| byte.is_ascii_digit()) {
            return false;
        }
        saw_subscript = true;
        rest = &tail[close + 1..];
    }
    saw_subscript && rest.is_empty()
}

/// Count statement lines of the shape `dst[<literals>] = src[<literals>];`,
/// where BOTH sides are plain literal-indexed lvalues. These are unrolled
/// copies: no arithmetic, no call, nothing for a reviewer to check beyond the
/// indices themselves.
pub(super) fn count_elementwise_copy_lines(source: &str) -> usize {
    source
        .lines()
        .filter(|line| {
            let statement = line.trim();
            let Some(body) = statement.strip_suffix(';') else {
                return false;
            };
            let Some((lhs, rhs)) = body.split_once('=') else {
                return false;
            };
            // Reject compound/comparison assignment (`+=`, `==`, `<=`, `!=`).
            if lhs.ends_with(['+', '-', '*', '/', '%', '&', '|', '^', '<', '>', '!', '='])
                || rhs.starts_with('=')
            {
                return false;
            }
            is_literal_indexed_lvalue(lhs) && is_literal_indexed_lvalue(rhs)
        })
        .count()
}

/// Whether `name` is a bare C identifier.
pub(super) fn is_bare_identifier(name: &str) -> bool {
    let mut bytes = name.bytes();
    bytes
        .next()
        .is_some_and(|byte| byte.is_ascii_alphabetic() || byte == b'_')
        && bytes.all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
}

/// Index of the first non-whitespace byte at or after `pos`.
pub(super) fn skip_whitespace(source: &str, pos: usize) -> usize {
    source[pos..]
        .find(|ch: char| !ch.is_whitespace())
        .map_or(source.len(), |offset| pos + offset)
}

/// If `literal` sits at `pos` after optional whitespace, the index just past it.
///
/// Whitespace skipping is the point: `((int32_t) (i))` and `((int32_t)(i))` are
/// the same construct, and a literal substring match sees only the second.
pub(super) fn eat(source: &str, pos: usize, literal: &str) -> Option<usize> {
    let pos = skip_whitespace(source, pos);
    source[pos..]
        .starts_with(literal)
        .then_some(pos + literal.len())
}

/// Like [`eat`], but the literal must not be followed by an identifier
/// character -- so `int32_t` does not match inside `int32_ted`.
pub(super) fn eat_word(source: &str, pos: usize, word: &str) -> Option<usize> {
    let end = eat(source, pos, word)?;
    let next_is_ident = source[end..]
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');
    (!next_is_ident).then_some(end)
}

/// Read the identifier starting at `pos` (after optional whitespace); returns
/// the text and the index just past it.
pub(super) fn read_identifier(source: &str, pos: usize) -> Option<(&str, usize)> {
    let start = skip_whitespace(source, pos);
    let end = start
        + source[start..]
            .find(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '_'))
            .unwrap_or(source.len() - start);
    let name = &source[start..end];
    is_bare_identifier(name).then_some((name, end))
}

/// Every entity this translation unit *declares* as `int32_t`: `for (int32_t i
/// = ...)` induction variables, plain locals, function parameters, and struct
/// fields from the companion header.
///
/// Harvesting the declarations instead of guessing from a naming convention is
/// what makes the redundant-cast metric sound. A cast is only reported when the
/// operand is an entity this very code declared `int32_t`, so the detector can
/// never accuse a `real_T`/`float` variable of carrying a redundant integer
/// cast -- the false-positive class that would otherwise make a zero ceiling a
/// hair trigger.
///
/// Known limitation: the emitter generates unique names per translation unit,
/// so a name is assumed to have one type. If that ever stops holding, the
/// assertion prints the offending operands, which makes a false positive
/// immediately diagnosable rather than mysterious.
pub(super) fn declared_int32_entities(sources: &[&str]) -> BTreeSet<String> {
    const TYPE: &str = "int32_t";
    let mut declared = BTreeSet::new();
    for source in sources {
        let mut search_from = 0;
        while let Some(rel) = source[search_from..].find(TYPE) {
            let start = search_from + rel;
            search_from = start + TYPE.len();
            // `uint32_t` contains `int32_t`; require a token boundary before it.
            let preceded_by_ident = source[..start]
                .chars()
                .next_back()
                .is_some_and(|ch| ch.is_ascii_alphanumeric() || ch == '_');
            if preceded_by_ident {
                continue;
            }
            let Some(after_type) = eat_word(source, start, TYPE) else {
                continue;
            };
            harvest_declarators(source, after_type, &mut declared);
        }
    }
    declared
}

/// Index just past the `open`..`close` group starting at `pos`, honouring
/// nesting. `None` when `pos` does not begin such a group.
pub(super) fn skip_group(source: &str, pos: usize, open: char, close: char) -> Option<usize> {
    let start = skip_whitespace(source, pos);
    if !source[start..].starts_with(open) {
        return None;
    }
    let mut depth = 0usize;
    for (offset, ch) in source[start..].char_indices() {
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                return Some(start + offset + 1);
            }
        }
    }
    None
}

/// Skip any array extents (`[3][4]`) following a declarator name.
pub(super) fn skip_array_extents(source: &str, mut pos: usize) -> usize {
    while let Some(next) = skip_group(source, pos, '[', ']') {
        pos = next;
    }
    pos
}

/// Index of the `,` or `;` that ends an initializer beginning at `from`.
pub(super) fn end_of_initializer(source: &str, from: usize) -> usize {
    let mut depth = 0i32;
    for (offset, ch) in source[from..].char_indices() {
        match ch {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' if depth == 0 => return from + offset,
            ')' | ']' | '}' => depth -= 1,
            ',' | ';' if depth == 0 => return from + offset,
            _ => {}
        }
    }
    source.len()
}

/// Harvest every declarator of one `int32_t` declaration into `declared`.
///
/// Handles the comma-separated list (`int32_t a, b;`), array extents
/// (`int32_t v[3];`), initializers (`for (int32_t i = 1; ...)`), and parameters
/// (`f(int32_t n)`). A cast (`(int32_t)`) has no declarator, so it is excluded
/// by construction: `read_identifier` fails immediately on the `)`.
pub(super) fn harvest_declarators(
    source: &str,
    after_type: usize,
    declared: &mut BTreeSet<String>,
) {
    let mut cursor = after_type;
    loop {
        let Some((name, after_name)) = read_identifier(source, cursor) else {
            return;
        };
        let pos = skip_whitespace(source, skip_array_extents(source, after_name));
        let Some(next) = source[pos..].chars().next() else {
            return;
        };
        if !matches!(next, '=' | ';' | ',' | ')') {
            return;
        }
        declared.insert(name.to_string());
        let after_declarator = match next {
            '=' => end_of_initializer(source, pos + 1),
            _ => pos,
        };
        if !source[after_declarator..].starts_with(',') {
            return;
        }
        cursor = after_declarator + 1;
    }
}

/// Strip fully-enclosing redundant parentheses: `((x))` -> `x`.
pub(super) fn strip_enclosing_parens(text: &str) -> &str {
    let mut text = text.trim();
    while let Some(inner) = text
        .strip_prefix('(')
        .and_then(|rest| rest.strip_suffix(')'))
    {
        // Only strip when the opening paren is matched by the closing one.
        let mut depth = 0i32;
        let balanced = inner.chars().all(|ch| {
            match ch {
                '(' => depth += 1,
                ')' => depth -= 1,
                _ => {}
            }
            depth >= 0
        });
        if !balanced || depth != 0 {
            break;
        }
        text = inner.trim();
    }
    text
}

/// Whether `operand` names an entity known to be `int32_t` already.
///
/// Accepts a bare identifier and a member path (`s->n`, `a.b`, `ctx->x.y`):
/// Generated C reaches into its context struct routinely, and a cast around
/// `ctx->count` is exactly as redundant as one around a loop variable. The
/// final component decides, since that is the entity being read.
pub(super) fn names_declared_int32(operand: &str, declared: &BTreeSet<String>) -> bool {
    let operand = strip_trailing_literal_subscripts(strip_enclosing_parens(operand));
    if operand.is_empty() {
        return false;
    }
    let components = c_member_path_components(operand);
    // Every component must be a plain name, so an expression like `a[i].b` or
    // `f(x).b` is never mistaken for a member path.
    let all_plain = components
        .iter()
        .all(|part| is_bare_identifier(part.trim()));
    let last = components
        .last()
        .map(|part| part.trim())
        .unwrap_or_default();
    all_plain && declared.contains(last)
}

/// Drop trailing literal array subscripts: `a[0][1]` -> `a`.
///
/// An element of an `int32_t` array is itself `int32_t`, so `(int32_t)a[0]` is
/// exactly as redundant as `(int32_t)a`. Without this, the two spellings
/// disagreed: the unparenthesised `(int32_t)a[0]` was counted (the operand
/// reader stops at the identifier) while `((int32_t)(a[0]))` was not (the
/// parenthesised operand kept its subscript and failed the member-path check).
/// Only literal subscripts are stripped -- a computed index makes the whole
/// expression computed, which is the case the metric deliberately excludes.
fn strip_trailing_literal_subscripts(text: &str) -> &str {
    let mut text = text.trim();
    while let Some(open) = text.rfind('[') {
        let Some(inner) = text[open + 1..].strip_suffix(']') else {
            break;
        };
        if inner.is_empty() || !inner.bytes().all(|byte| byte.is_ascii_digit()) {
            break;
        }
        text = text[..open].trim_end();
    }
    text
}

/// Split a C member-access path into its components: `s->a.b` -> `[s, a, b]`.
///
/// This is a C-expression helper with a deliberately narrow owner, not a
/// Modelica name tokenizer. The input is a fragment of emitted C where `.` and
/// `->` are struct member access; Modelica name hierarchy still comes from the
/// AST/IR everywhere it actually matters, which is what the repo's
/// dot-tokenization policy is protecting. Written as an explicit scan rather
/// than a `split` call so it reads as the C parser it is.
pub(super) fn c_member_path_components(path: &str) -> Vec<&str> {
    let bytes = path.as_bytes();
    let mut components = Vec::new();
    let mut start = 0;
    let mut index = 0;
    while index < bytes.len() {
        let separator_width = match bytes[index] {
            b'.' => 1,
            b'-' if bytes.get(index + 1) == Some(&b'>') => 2,
            _ => {
                index += 1;
                continue;
            }
        };
        components.push(&path[start..index]);
        index += separator_width;
        start = index;
    }
    components.push(&path[start..]);
    components
}

/// Text of the parenthesised group that starts at `pos` (which must be at `(`
/// after optional whitespace), plus the index just past its closing paren.
pub(super) fn read_parenthesised(source: &str, pos: usize) -> Option<(&str, usize)> {
    let open = skip_whitespace(source, pos);
    if !source[open..].starts_with('(') {
        return None;
    }
    let mut depth = 0usize;
    for (offset, ch) in source[open..].char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    let end = open + offset;
                    return Some((&source[open + 1..end], end + 1));
                }
            }
            _ => {}
        }
    }
    None
}

/// Every redundant `(int32_t)` cast: a cast to `int32_t` of an operand this
/// translation unit already declared `int32_t`.
///
/// Deliberately independent of what follows the cast. `((int32_t)(i)) - 1` and
/// `a[((int32_t)(i))]` are equally redundant, and an earlier revision that
/// required a trailing `- 1` simply did not see the second form. Casts around
/// *computed* index expressions (`((int32_t)((7 + (i - 1)))) - 1`) are NOT
/// counted: those are intentional, because that is where width and signedness
/// genuinely need pinning.
///
/// Returns the offending operand texts so a failure names names.
pub(super) fn redundant_int32_casts(source: &str, declared: &BTreeSet<String>) -> Vec<String> {
    let mut offenders = Vec::new();
    let mut search_from = 0;
    while let Some(rel) = source[search_from..].find('(') {
        let open = search_from + rel;
        search_from = open + 1;
        // `(int32_t)` — the cast itself, whitespace-tolerant.
        let Some(after_type) = eat_word(source, open + 1, "int32_t") else {
            continue;
        };
        let Some(after_cast) = eat(source, after_type, ")") else {
            continue;
        };
        // The operand: `(x)` or a bare `x`.
        let operand = match read_parenthesised(source, after_cast) {
            Some((inner, _)) => inner.to_string(),
            None => match read_identifier(source, after_cast) {
                Some((name, _)) => name.to_string(),
                None => continue,
            },
        };
        if names_declared_int32(&operand, declared) {
            offenders.push(operand.trim().to_string());
        }
    }
    offenders
}

/// Detector unit tests. Each case below is a spelling that an earlier revision
/// of this file got wrong -- three redundant casts it failed to see, and one
/// innocent cast it wrongly reported. They are pinned here so the detector
/// cannot silently regress to counting substrings, and so the metric can be
/// trusted without re-running the emitter.
#[test]
pub(super) fn test_redundant_cast_detector_covers_every_spelling_and_stays_type_aware() {
    let harvest = |source: &str| declared_int32_entities(&[source]);

    // A bare induction variable, the canonical case.
    let canonical = "for (int32_t i = 1; i <= 3; i++) { a[((int32_t)(i)) - 1] = 0; }";
    assert_eq!(
        redundant_int32_casts(canonical, &harvest(canonical)),
        vec!["i"],
        "the canonical `((int32_t)(i)) - 1` must be reported"
    );

    // ESCAPE 1: one space between the cast and its operand. A literal-prefix
    // match sees nothing here.
    let spaced = "for (int32_t i = 1; i <= 3; i++) { a[((int32_t) (i)) - 1] = 0; }";
    assert_eq!(
        redundant_int32_casts(spaced, &harvest(spaced)),
        vec!["i"],
        "whitespace between `(int32_t)` and its operand must not hide the cast"
    );

    // ESCAPE 2: no `- 1` offset. Equally redundant, and an offset-dependent
    // matcher misses it entirely.
    let no_offset = "for (int32_t i = 0; i < 3; i++) { a[((int32_t)(i))] = 0; }";
    assert_eq!(
        redundant_int32_casts(no_offset, &harvest(no_offset)),
        vec!["i"],
        "a redundant cast with no `- 1` offset must still be reported"
    );

    // ESCAPE 3: member paths, which generated C emits routinely.
    let member = "int32_t n; void f(S *s) { a[((int32_t)(s->n)) - 1] = 0; }";
    assert_eq!(
        redundant_int32_casts(member, &harvest(member)),
        vec!["s->n"],
        "a cast of an `int32_t` struct field must be reported"
    );
    let dotted = "int32_t n; void f(S s) { a[((int32_t)(s.n)) - 1] = 0; }";
    assert_eq!(
        redundant_int32_casts(dotted, &harvest(dotted)),
        vec!["s.n"],
        "dotted member access must be handled like `->`"
    );

    // FALSE POSITIVE: a bare identifier that is NOT an int32_t entity. A
    // name-shaped matcher trips a hard assert here; a type-aware one does not.
    let real_operand = "float x_real; void f(void) { a[((int32_t)(x_real)) - 1] = 0; }";
    assert!(
        redundant_int32_casts(real_operand, &harvest(real_operand)).is_empty(),
        "casting a non-`int32_t` operand is meaningful and must never be reported"
    );

    // Intentional: a cast around a computed index expression.
    let computed = "for (int32_t i = 1; i <= 3; i++) { a[((int32_t)((7 + (i - 1)))) - 1] = 0; }";
    assert!(
        redundant_int32_casts(computed, &harvest(computed)).is_empty(),
        "casts around computed index expressions are deliberate and must not be reported"
    );

    // An element of an int32_t array is int32_t, so both spellings of a
    // subscripted operand must agree. They did not before: the parenthesised
    // form kept its subscript and slipped through.
    let subscripted = "int32_t v[3]; void f(void) { a[((int32_t)(v[0]))] = 0; }";
    assert_eq!(
        redundant_int32_casts(subscripted, &harvest(subscripted)),
        vec!["v[0]"],
        "a cast of an int32_t array element must be reported"
    );
    let bare_subscripted = "int32_t v[3]; void f(void) { a[(int32_t)v[0]] = 0; }";
    assert_eq!(
        redundant_int32_casts(bare_subscripted, &harvest(bare_subscripted)).len(),
        1,
        "the unparenthesised spelling must agree with the parenthesised one"
    );
    let computed_subscript = "int32_t v[3]; int32_t i; void f(void) { a[((int32_t)(v[i]))] = 0; }";
    assert!(
        redundant_int32_casts(computed_subscript, &harvest(computed_subscript)).is_empty(),
        "a computed subscript makes the operand computed, which is not counted"
    );

    // `uint32_t` contains `int32_t`; harvesting must not confuse the two.
    let unsigned = "uint32_t u; void f(void) { a[((int32_t)(u)) - 1] = 0; }";
    assert!(
        redundant_int32_casts(unsigned, &harvest(unsigned)).is_empty(),
        "a `uint32_t` declaration must not be harvested as `int32_t`"
    );
    assert!(
        !declared_int32_entities(&["uint32_t u;"]).contains("u"),
        "`uint32_t u` must not register `u` as an int32_t entity"
    );

    // Declarations the harvester must find: locals, parameters, array extents,
    // multi-declarator lists, and header struct fields.
    let declared = declared_int32_entities(&[
        "int32_t local; void f(int32_t param) { int32_t a, b; int32_t v[3]; }",
        "typedef struct { int32_t field; } Ctx;",
    ]);
    for name in ["local", "param", "a", "b", "v", "field"] {
        assert!(
            declared.contains(name),
            "`{name}` should have been harvested as a declared int32_t entity, got {declared:?}"
        );
    }
}

/// The `for` counter must survive a formatting change, and the elementwise
/// detector must not count computation as a copy.
#[test]
pub(super) fn test_surface_counters_are_not_fooled_by_formatting_or_arithmetic() {
    assert_eq!(count_occurrences("(void)a; (void)b;", "(void)"), 2);
    assert_eq!(count_occurrences("nothing here", "(void)"), 0);

    assert_eq!(count_for_loops("for (i;;) {} for(j;;) {}"), 2);
    assert_eq!(
        count_for_loops("formatter(x); before(y);"),
        0,
        "an identifier ending in `for` is not a loop header"
    );

    assert_eq!(count_elementwise_copy_lines("    a[0] = b[1];\n"), 1);
    assert_eq!(
        count_elementwise_copy_lines("    ctx->S[0][2] = v[1];\n"),
        1
    );
    assert_eq!(
        count_elementwise_copy_lines("    a[0] = b[1] + c[2];\n"),
        0,
        "arithmetic is not a copy"
    );
    assert_eq!(
        count_elementwise_copy_lines("    a[i] = b[i];\n"),
        0,
        "variable subscripts are a loop body, not an unrolled copy"
    );
    assert_eq!(
        count_elementwise_copy_lines("    a[0] += b[1];\n"),
        0,
        "compound assignment is not a copy"
    );
    assert_eq!(
        count_elementwise_copy_lines("    if (a[0] == b[1]) { return; }\n"),
        0,
        "a comparison is not a copy"
    );
}
