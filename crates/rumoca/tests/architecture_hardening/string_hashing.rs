//! Zero-string-hashing enforcement.
//!
//! Policy: string bytes are hashed in exactly ONE place in the compiler — the
//! interner's `string -> id` map. Everywhere else hashes an id.
//!
//! Why this is a hard zero rather than an allowlist:
//!
//! * `VarName` is **already interned** (`rumoca-core::ir_primitives::var_name`):
//!   it carries a `VarNameId(u32)` whose docs say it exists "so hot lookup paths
//!   do not repeatedly compare". Hashing `name.as_str()` reaches past that id to
//!   hash the very string the interner was built to avoid.
//! * The identity to hash is the interned `VarName`, NOT `ComponentReference::
//!   def_id`. A `DefId` names a *declaration*, and one declaration backs many
//!   flattened instances — `phase-flatten/src/postprocess_def_id.rs` keys
//!   `by_def_id: HashMap<DefId, Vec<IndexedVarRef>>`, a vector per id, and
//!   `Reference`'s own doc says it "records the declaration being referenced;
//!   it does not assign a `DefId` to this expression". So `resistor1.v` and
//!   `resistor2.v` share a `DefId`. Worse, `var_refs_semantically_equal`
//!   compares `VarName` and ignores `component_ref`, so hashing `def_id` would
//!   give EQUAL expressions UNEQUAL hashes — breaking eq⇒hash in the direction
//!   that silently drops real duplicates from their bucket.
//! * Rendered names are not semantics (SPEC_0032 §3, "Semantic identity MUST be
//!   proven from compiler-owned data ... Strings are not semantics"). Two
//!   aliases of one declaration render differently and would hash differently;
//!   two distinct declarations can render alike and collide.
//!
//! **Interning is not a universal escape hatch.** `VarName::intern` finds an id
//! by hashing the whole string, then takes a global `RwLock`, probes a map and
//! allocates. A site that holds only a `&str` and interns *in order to hash* has
//! therefore paid the string hash anyway plus a lock and an allocation — it is
//! slower than the hash it replaced, and it feeds an interner that has no
//! eviction and hands out ids assumed stable for the life of the process. The
//! two correct answers at such a site are to restructure so the caller passes an
//! already-interned name, or to hash a bounded summary (a length) and let the
//! equality check that every fingerprint caller must run anyway separate the
//! bucket. Never intern user-supplied, non-identity text — the LSP server lives
//! for hours on an arbitrary edit stream.
//!
//! **Process-local ids are not stable across runs.** `VarNameId` is explicitly
//! process-local, so a hash that feeds a golden file, a cache key, or any
//! on-disk artifact must key on a stable identity (a `DefId`, or content hashed
//! by a real digest) rather than an interned index. That is why these detectors
//! scope themselves to `std::hash::Hasher`, the process-local identity hasher,
//! and leave content digests (`blake3`) alone.
//!
//! # What is detected
//!
//! Four independent detectors, because string bytes reach a `Hasher` by four
//! different syntactic routes and each of the first three was evaded in review:
//!
//! 1. [`explicit_hash_offenders`] — an expression that renders text and hashes
//!    it (`x.as_str().hash(state)`, `x.as_bytes().hash(state)`, …).
//! 2. [`derived_hash_offenders`] — `#[derive(Hash)]` on a type with a
//!    string-shaped field. The attribute is parsed across **multiple lines**,
//!    and a field counts as string-shaped when a string type appears anywhere in
//!    it, so `Vec<String>`, `Option<String>` and `IndexMap<String, T>` count
//!    exactly as `String` does — the derive hashes those bytes just the same.
//! 3. [`manual_hash_offenders`] — a hand-written `impl Hash for T` that hashes
//!    a string-shaped field of `T`. Field types come from an index of every
//!    declaration in the workspace, so the type does not have to be declared in
//!    the same file as its impl.
//! 4. [`hash_scope_offenders`] — inside a scope that owns a `std::hash::Hasher`
//!    (a `Hash` impl, or a function taking one), any `as_bytes()` conversion, or
//!    a `.hash(` call on a binding the scope itself declares with a string type.
//!    This is what catches `state.write(self.name.as_bytes())` and the helper
//!    shape `fn h(text: &str, state: &mut impl Hasher) { text.hash(state) }`.
//!
//! [`test_detectors_reject_known_evasions`] holds one fixture per construct and
//! fails if any of them stops being detected, so the gate's detection power is
//! itself under test rather than merely asserted.

use super::*;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// The single module permitted to hash string bytes: the interner itself.
/// Its whole purpose is turning content into identity, so the `string -> id`
/// map necessarily hashes the content once.
const INTERNER_MODULE: &str = "crates/rumoca-core/src/ir_primitives/var_name.rs";

/// This gate file itself. It is scanned out because it *states* the policy: its
/// pattern tables, fixtures and assertion text spell out the very constructs it
/// hunts for, so scanning it reports the prose as a violation. Excluding the
/// file is the correct fix — rewording the message to dodge the match would
/// leave the detector able to flag itself again the next time the text is
/// reflowed. Nothing here is compiler code, so no coverage is lost.
const GATE_MODULE: &str = "crates/rumoca/tests/architecture_hardening/string_hashing.rs";

/// Files whose contents *define* the policy rather than obey it.
fn is_policy_owned(rel: &str) -> bool {
    rel == INTERNER_MODULE || rel == GATE_MODULE
}

// ---------------------------------------------------------------------------
// Lexical helpers
// ---------------------------------------------------------------------------

/// Strip string literals and a trailing line comment so a pattern quoted inside
/// a string, or written in prose, does not count as a violation. Doc comments
/// (`///`) fall out with ordinary comments.
fn code_only(line: &str) -> String {
    let mut out = String::with_capacity(line.len());
    let mut in_string = false;
    let mut prev = '\0';
    let mut chars = line.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '"' if prev != '\\' => in_string = !in_string,
            '/' if !in_string && chars.peek() == Some(&'/') => break,
            _ if !in_string => out.push(ch),
            _ => {}
        }
        prev = ch;
    }
    out
}

fn is_ident_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

/// `haystack` contains `word` bounded by non-identifier characters, so `str`
/// matches `&'a str` but not `strip`, and `Path` does not match `PathKind`.
fn contains_word(haystack: &str, word: &str) -> bool {
    haystack.match_indices(word).any(|(at, _)| {
        let before = haystack[..at].chars().next_back();
        let after = haystack[at + word.len()..].chars().next();
        !before.is_some_and(is_ident_char) && !after.is_some_and(is_ident_char)
    })
}

/// Drop a leading `pub`, `pub(crate)`, `pub(in path)` so declaration and field
/// parsing sees the item itself.
fn strip_visibility(code: &str) -> &str {
    let trimmed = code.trim_start();
    let Some(rest) = trimmed.strip_prefix("pub") else {
        return trimmed;
    };
    let rest = match rest.starts_with('(') {
        true => rest.split_once(')').map_or(rest, |(_, tail)| tail),
        false => rest,
    };
    rest.trim_start()
}

/// Types whose `Hash` walks text. `Path`/`PathBuf`/`OsStr` are included because
/// hashing a path hashes its bytes exactly as hashing a `String` does.
const STRING_TYPE_WORDS: &[&str] = &[
    "String", "str", "Cow", "OsString", "OsStr", "PathBuf", "Path",
];

/// True when a *type* mentions a string anywhere, at any nesting depth.
///
/// Depth-insensitivity is the point: `#[derive(Hash)]` over `Vec<String>` or
/// `IndexMap<String, T>` hashes those bytes exactly as a bare `String` field
/// does, and a detector that only looked at the outermost type missed the live
/// `ComponentPath { parts: Vec<String> }` and `QualifiedName` offenders.
fn type_text_is_string_shaped(code: &str) -> bool {
    STRING_TYPE_WORDS
        .iter()
        .any(|word| contains_word(code, word))
}

// ---------------------------------------------------------------------------
// Sources
// ---------------------------------------------------------------------------

fn is_scanned_rust_source(rel: &str) -> bool {
    rel.starts_with("crates/") && rel.ends_with(".rs")
}

fn collect_scanned_sources(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut files);
    files
}

/// Every scanned file as `(workspace-relative path, contents)`.
fn scanned_workspace_sources(root: &Path) -> Vec<(String, String)> {
    let mut sources = Vec::new();
    for path in collect_scanned_sources(root) {
        let rel = normalized_rel_path(path.strip_prefix(root).unwrap_or(&path));
        if !is_scanned_rust_source(&rel) || is_policy_owned(&rel) {
            continue;
        }
        if let Ok(content) = fs::read_to_string(&path) {
            sources.push((rel, content));
        }
    }
    sources
}

fn report(rel: &str, offenders: &[usize], out: &mut Vec<String>) {
    out.extend(offenders.iter().map(|line| format!("{rel}:{line}")));
}

// ---------------------------------------------------------------------------
// 1. Explicit "render text, then hash it" expressions
// ---------------------------------------------------------------------------

/// Expressions that hash string bytes directly.
fn explicit_string_hash_patterns() -> Vec<&'static str> {
    vec![
        "as_str().hash(",
        "to_string().hash(",
        ".text.hash(",
        "text().hash(",
        "as_ref().hash(",
        "as_bytes().hash(",
        "to_owned().hash(",
        "to_lowercase().hash(",
        "to_uppercase().hash(",
        "as_os_str().hash(",
        "join(\".\").hash(",
    ]
}

/// One-based line numbers whose code hashes a rendered string.
fn explicit_hash_offenders(lines: &[&str]) -> Vec<usize> {
    let patterns = explicit_string_hash_patterns();
    lines
        .iter()
        .enumerate()
        .filter(|(_, line)| {
            let code = code_only(line);
            patterns.iter().any(|pattern| code.contains(pattern))
        })
        .map(|(index, _)| index + 1)
        .collect()
}

// ---------------------------------------------------------------------------
// 2. `#[derive(Hash)]` over a string-shaped field
// ---------------------------------------------------------------------------

/// A `#[derive(...)]` attribute starting at `start`, joined across however many
/// lines it spans, plus the index of its last line.
///
/// Rustfmt wraps long derive lists, so a single-line check reported nothing for
///
/// ```text
/// #[derive(
///     Debug, Clone, Hash,
/// )]
/// ```
///
/// which is a plain reformat away from any derive in the tree.
fn derive_attribute(lines: &[&str], start: usize) -> Option<(String, usize)> {
    let first = code_only(lines.get(start)?);
    if !first.trim_start().starts_with("#[derive(") {
        return None;
    }
    let mut text = String::new();
    let mut opened = 0usize;
    let mut closed = 0usize;
    for (offset, line) in lines.iter().enumerate().skip(start).take(16) {
        let code = code_only(line);
        opened += code.matches('(').count();
        closed += code.matches(')').count();
        text.push_str(&code);
        if opened <= closed {
            return Some((text, offset));
        }
    }
    None
}

/// The source lines of the type declaration a `#[derive(...)]` applies to.
///
/// Terminating on the declaration's own end matters in both directions: a
/// braced body ends at its matching `}`, and a tuple or unit struct ends at the
/// `;` that closes it. Without the `;` case the scan runs on into the following
/// `impl` block and reports a `fn parse(value: &str)` parameter as a hashed
/// field, so a newtype over a non-string (`ManifestId(Uuid)`) is flagged while
/// a real newtype over `String` further down the file is missed.
fn derived_declaration_body<'a>(lines: &[&'a str], start: usize) -> Vec<&'a str> {
    let mut body = Vec::new();
    let mut brace_depth = 0usize;
    let mut paren_depth = 0usize;
    for line in lines.iter().skip(start).take(400) {
        let code = code_only(line);
        body.push(*line);
        brace_depth += code.matches('{').count();
        brace_depth = brace_depth.saturating_sub(code.matches('}').count());
        paren_depth += code.matches('(').count();
        paren_depth = paren_depth.saturating_sub(code.matches(')').count());
        let closed_tuple_or_unit =
            brace_depth == 0 && paren_depth == 0 && code.trim_end().ends_with(';');
        if closed_tuple_or_unit || (brace_depth == 0 && code.contains('}')) {
            break;
        }
    }
    body
}

/// One-based line numbers of `#[derive(...)]` attributes that derive `Hash` for
/// a declaration carrying string-shaped data.
fn derived_hash_offenders(lines: &[&str]) -> Vec<usize> {
    let mut offenders = Vec::new();
    let mut index = 0usize;
    while index < lines.len() {
        let Some((attribute, end)) = derive_attribute(lines, index) else {
            index += 1;
            continue;
        };
        if contains_word(&attribute, "Hash") {
            let body = derived_declaration_body(lines, end + 1);
            if body
                .iter()
                .any(|line| type_text_is_string_shaped(&code_only(line)))
            {
                offenders.push(index + 1);
            }
        }
        index = end + 1;
    }
    offenders
}

// ---------------------------------------------------------------------------
// 3. Hand-written `impl Hash` over a string-shaped field
// ---------------------------------------------------------------------------

/// The string-shaped data a type exposes to a hand-written `Hash`.
#[derive(Default)]
struct StringPayload {
    /// Named fields (of the struct, or of any enum variant) whose type mentions
    /// a string type.
    named: BTreeSet<String>,
    /// Whether any tuple position or unnamed variant payload is string-shaped.
    positional: bool,
}

/// Type name to the string-shaped data it carries, across the whole workspace.
///
/// Keyed by bare type name, so two same-named types in different crates merge.
/// The merge is deliberately conservative: a manual `Hash` that hashes a field
/// which is string-shaped on *some* type of that name is worth a human look.
type StringPayloadIndex = BTreeMap<String, StringPayload>;

/// The type name declared by a `struct`/`enum` head line.
fn declared_type_name(code: &str) -> Option<String> {
    let rest = strip_visibility(code);
    let rest = rest
        .strip_prefix("struct ")
        .or_else(|| rest.strip_prefix("enum "))?;
    let name: String = rest
        .trim_start()
        .chars()
        .take_while(|c| is_ident_char(*c))
        .collect();
    (!name.is_empty()).then_some(name)
}

/// A `name: Type` field, as `(name, type text)`.
fn named_field(code: &str) -> Option<(String, String)> {
    let (name, type_text) = strip_visibility(code).split_once(':')?;
    let name = name.trim();
    (!name.is_empty() && name.chars().all(is_ident_char))
        .then(|| (name.to_string(), type_text.to_string()))
}

/// A parenthesised payload holding a string: `struct Sha1Hex(String);`,
/// `enum Name { Quoted(String, Span) }`.
fn paren_payload_is_string(code: &str) -> bool {
    let Some(open) = code.find('(') else {
        return false;
    };
    let Some(close) = code.rfind(')') else {
        return false;
    };
    close > open && type_text_is_string_shaped(&code[open + 1..close])
}

/// Record the string-shaped data of every declaration in one file.
fn index_string_payloads(lines: &[&str], index: &mut StringPayloadIndex) {
    for (start, line) in lines.iter().enumerate() {
        let Some(type_name) = declared_type_name(&code_only(line)) else {
            continue;
        };
        let entry = index.entry(type_name).or_default();
        for body_line in derived_declaration_body(lines, start) {
            let code = code_only(body_line);
            match named_field(&code) {
                Some((field, type_text)) if type_text_is_string_shaped(&type_text) => {
                    entry.named.insert(field);
                }
                Some(_) => {}
                None => entry.positional |= paren_payload_is_string(&code),
            }
        }
    }
}

/// The type of a hand-written `impl ... Hash for T`.
fn hash_impl_type(code: &str) -> Option<String> {
    let trimmed = code.trim_start();
    if !trimmed.starts_with("impl") {
        return None;
    }
    let rest = trimmed.split_once("Hash for")?.1;
    let name: String = rest
        .trim_start()
        .chars()
        .take_while(|c| is_ident_char(*c))
        .collect();
    (!name.is_empty()).then_some(name)
}

/// The receiver of a `.hash(` call when it is a plain binding (`text.hash(s)`)
/// rather than a field or call chain.
fn plain_receiver_before_hash(code: &str) -> Option<&str> {
    let at = code.find(".hash(")?;
    let head = &code[..at];
    let start = head
        .rfind(|c: char| !is_ident_char(c))
        .map_or(0, |at| at + 1);
    let receiver = &head[start..];
    let preceded_by_field_access = head[..start].ends_with('.');
    (!receiver.is_empty() && !preceded_by_field_access).then_some(receiver)
}

/// True when this line of a hand-written `Hash` feeds string-shaped data of the
/// implementing type into the hasher.
fn line_hashes_string_payload(code: &str, payload: &StringPayload) -> bool {
    let hashes_named = payload.named.iter().any(|field| {
        code.contains(&format!("{field}.hash("))
            || code.contains(&format!("Hash::hash(&self.{field}"))
            || code.contains(&format!("Hash::hash({field}"))
    });
    if hashes_named {
        return true;
    }
    if !payload.positional {
        return false;
    }
    // Unnamed string payloads are reached either positionally or through a
    // match binding, and neither carries a name this scan can type-check, so a
    // type that both hand-rolls `Hash` and holds an unnamed string is treated as
    // hashing it. That pairing is rare and always worth a human look.
    (0..8).any(|position| code.contains(&format!("self.{position}.hash(")))
        || plain_receiver_before_hash(code).is_some()
}

/// One-based line numbers inside `impl Hash for T` blocks that hash `T`'s
/// string-shaped data.
fn manual_hash_offenders(lines: &[&str], index: &StringPayloadIndex) -> Vec<usize> {
    let mut offenders = Vec::new();
    for (start, line) in lines.iter().enumerate() {
        let Some(type_name) = hash_impl_type(&code_only(line)) else {
            continue;
        };
        let Some(payload) = index.get(&type_name) else {
            continue;
        };
        let body = block_lines(lines, start);
        offenders.extend(body.into_iter().filter(|at| {
            lines
                .get(*at)
                .is_some_and(|line| line_hashes_string_payload(&code_only(line), payload))
        }));
    }
    offenders.iter().map(|at| at + 1).collect()
}

/// Indices of the lines of the braced block opened at or after `start`.
fn block_lines(lines: &[&str], start: usize) -> Vec<usize> {
    let mut body = Vec::new();
    let mut depth = 0usize;
    let mut opened = false;
    for (at, line) in lines.iter().enumerate().skip(start).take(400) {
        let code = code_only(line);
        depth += code.matches('{').count();
        opened |= code.contains('{');
        depth = depth.saturating_sub(code.matches('}').count());
        body.push(at);
        if opened && depth == 0 {
            break;
        }
    }
    body
}

// ---------------------------------------------------------------------------
// 4. Anything string-shaped inside a scope that owns a `std::hash::Hasher`
// ---------------------------------------------------------------------------

/// True when this line opens a scope that hashes for *identity*: an
/// `impl Hash for`, or a signature taking a `std::hash::Hasher`.
///
/// The `&mut`/bound requirement is what keeps content digests out. `let mut
/// hasher = blake3::Hasher::new()` names a `Hasher` too, but a digest over
/// file contents is the sanctioned way to build a cache key that survives the
/// process, and the interner cannot serve that need.
fn opens_hash_scope(code: &str) -> bool {
    if code.trim_start().starts_with("impl") && code.contains("Hash for") {
        return true;
    }
    contains_word(code, "Hasher")
        && (code.contains("&mut") || code.contains("impl Hasher") || code.contains(": Hasher"))
}

/// A line that builds a process-local identity hasher.
///
/// This is the other half of "scope that owns a `Hasher`": a function whose
/// *signature* says nothing about hashing can still build a `DefaultHasher` and
/// feed a `&str` straight into it, which is the most natural way to sidestep a
/// signature-driven check.
fn constructs_identity_hasher(code: &str) -> bool {
    code.contains("DefaultHasher::new(")
}

fn indent_width(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

/// The line opening the function that encloses `at`, found by indentation.
fn enclosing_fn(lines: &[&str], at: usize) -> Option<usize> {
    let indent = indent_width(lines.get(at)?);
    (0..at).rev().find(|candidate| {
        let line = lines[*candidate];
        code_only(line).contains("fn ") && indent_width(line) < indent
    })
}

/// Every line that opens a scope in which string bytes must not reach a hasher.
fn hash_scope_starts(lines: &[&str]) -> Vec<usize> {
    let mut starts = Vec::new();
    for (at, line) in lines.iter().enumerate() {
        let code = code_only(line);
        if opens_hash_scope(&code) {
            starts.push(at);
        } else if constructs_identity_hasher(&code)
            && let Some(owner) = enclosing_fn(lines, at)
        {
            starts.push(owner);
        }
    }
    starts.sort_unstable();
    starts.dedup();
    starts
}

/// Bindings a scope declares with a string type: parameters and annotated lets.
///
/// Splitting on punctuation first is what makes parameters visible — a
/// signature packs several `name: type` pairs onto one line, and
/// `fn hash_name(name: &str, state: &mut impl Hasher)` must yield `name`.
fn string_bindings_in_scope(lines: &[&str], body: &[usize]) -> BTreeSet<String> {
    let mut bindings = BTreeSet::new();
    for line in body.iter().filter_map(|at| lines.get(*at)) {
        let code = code_only(line);
        for fragment in code.split(['(', ')', ',', '{', '}', ';']) {
            let fragment = fragment
                .trim()
                .trim_start_matches("let ")
                .trim_start_matches("mut ");
            if let Some((name, type_text)) = named_field(fragment)
                && type_text_is_string_shaped(&type_text)
            {
                bindings.insert(name);
            }
        }
    }
    bindings
}

/// One-based line numbers where a hashing scope converts text to bytes, or
/// hashes a string binding it declared itself.
fn hash_scope_offenders(lines: &[&str]) -> Vec<usize> {
    let mut offenders = Vec::new();
    let mut next = 0usize;
    for start in hash_scope_starts(lines) {
        if start < next {
            continue;
        }
        let body = block_lines(lines, start);
        next = body.last().map_or(start + 1, |at| at + 1);
        let bindings = string_bindings_in_scope(lines, &body);
        offenders.extend(
            body.into_iter()
                .filter(|at| scope_line_hashes_text(lines, *at, &bindings))
                .map(|at| at + 1),
        );
    }
    offenders
}

fn scope_line_hashes_text(lines: &[&str], at: usize, bindings: &BTreeSet<String>) -> bool {
    lines.get(at).is_some_and(|line| {
        let code = code_only(line);
        code.contains(".as_bytes()")
            || bindings
                .iter()
                .any(|binding| code.contains(&format!("{binding}.hash(")))
    })
}

// ---------------------------------------------------------------------------
// Gates
// ---------------------------------------------------------------------------

/// Guidance printed with every offender list. Written once because all four
/// detectors report the same design error.
const POLICY_ADVICE: &str = "\
Identity comes from an interned id, not from rendered text — hash the \
`VarNameId` a `VarName` already carries. Do NOT hash `ComponentReference::\
def_id`: it names a DECLARATION, many flat instances share one, and equality \
ignores it, so hashing it gives equal expressions unequal hashes. If you hold \
only a `&str` and would have to intern it just to hash it, you have paid the \
string hash anyway plus a lock and an allocation — restructure so the caller \
passes an already-interned name, or hash a bounded summary (a length) and let \
the equality check the caller already runs separate the bucket. Never intern \
user-supplied text: the interner has no eviction and the LSP server runs for \
hours.";

fn workspace_payload_index(sources: &[(String, String)]) -> StringPayloadIndex {
    let mut index = StringPayloadIndex::new();
    for (_, content) in sources {
        let lines: Vec<&str> = content.lines().collect();
        index_string_payloads(&lines, &mut index);
    }
    index
}

#[test]
fn test_string_bytes_are_hashed_only_by_the_interner() {
    let sources = scanned_workspace_sources(&workspace_root());
    let mut offenders = Vec::new();

    for (rel, content) in &sources {
        let lines: Vec<&str> = content.lines().collect();
        report(rel, &explicit_hash_offenders(&lines), &mut offenders);
        report(rel, &hash_scope_offenders(&lines), &mut offenders);
    }

    assert!(
        offenders.is_empty(),
        "string bytes are hashed outside the interner. {POLICY_ADVICE} The only \
module permitted to hash string bytes is {INTERNER_MODULE}. Offenders: \
{offenders:#?}"
    );
}

#[test]
fn test_no_derived_hash_over_string_fields() {
    let sources = scanned_workspace_sources(&workspace_root());
    let mut offenders = Vec::new();

    for (rel, content) in &sources {
        let lines: Vec<&str> = content.lines().collect();
        report(rel, &derived_hash_offenders(&lines), &mut offenders);
    }

    assert!(
        offenders.is_empty(),
        "`#[derive(Hash)]` on a type with string-shaped data hashes those bytes \
implicitly, which no `.hash(` grep can see — including through a container \
(`Vec<String>`, `IndexMap<String, T>`). Key the type on an interned id \
(`VarNameId`, `DefId`) and derive `Hash` over that, or implement `Hash` by hand \
over the id fields. {POLICY_ADVICE} Offenders (derive site): {offenders:#?}"
    );
}

#[test]
fn test_no_manual_hash_impl_over_string_fields() {
    let sources = scanned_workspace_sources(&workspace_root());
    let index = workspace_payload_index(&sources);
    let mut offenders = Vec::new();

    for (rel, content) in &sources {
        let lines: Vec<&str> = content.lines().collect();
        report(rel, &manual_hash_offenders(&lines, &index), &mut offenders);
    }

    assert!(
        offenders.is_empty(),
        "a hand-written `impl Hash` feeds a string-shaped field into the hasher. \
Writing `Hash` by hand does not exempt a type from the policy — it is the most \
common way around the derive check. {POLICY_ADVICE} Offenders: {offenders:#?}"
    );
}

// ---------------------------------------------------------------------------
// The gate's own test: every construct that once slipped through
// ---------------------------------------------------------------------------

/// One evasion construct: source text that must be reported, and by which
/// detector.
struct Evasion {
    what: &'static str,
    source: &'static str,
    detect: fn(&[&str], &StringPayloadIndex) -> Vec<usize>,
}

fn detect_explicit(lines: &[&str], _index: &StringPayloadIndex) -> Vec<usize> {
    explicit_hash_offenders(lines)
}

fn detect_derived(lines: &[&str], _index: &StringPayloadIndex) -> Vec<usize> {
    derived_hash_offenders(lines)
}

fn detect_manual(lines: &[&str], index: &StringPayloadIndex) -> Vec<usize> {
    manual_hash_offenders(lines, index)
}

fn detect_scope(lines: &[&str], _index: &StringPayloadIndex) -> Vec<usize> {
    hash_scope_offenders(lines)
}

/// Every construct an adversarial probe drove through the previous detector,
/// plus the shapes of the live offenders it missed.
fn known_evasions() -> Vec<Evasion> {
    vec![
        Evasion {
            what: "(a) hand-written `impl Hash` over a `String` field",
            source: "\
struct Probe {
    name: String,
}
impl Hash for Probe {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
",
            detect: detect_manual,
        },
        Evasion {
            what: "(b) writing the bytes straight into the hasher",
            source: "\
struct Probe {
    name: String,
}
impl Hash for Probe {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.name.as_bytes());
    }
}
",
            detect: detect_scope,
        },
        Evasion {
            what: "(c) a `#[derive(...)]` wrapped across lines",
            source: "\
#[derive(
    Debug, Clone, Hash,
)]
struct Probe {
    name: String,
}
",
            detect: detect_derived,
        },
        Evasion {
            what: "(d) a derived `Hash` reaching a string through a container",
            source: "\
#[derive(Debug, Hash)]
struct Probe {
    parts: Vec<String>,
}
",
            detect: detect_derived,
        },
        Evasion {
            what: "(e) a helper that hashes the `&str` its caller passed in",
            source: "\
fn hash_name(name: &str, state: &mut impl Hasher) {
    name.hash(state);
}
",
            detect: detect_scope,
        },
        Evasion {
            what: "(f) a newtype over a string hashed positionally",
            source: "\
struct Probe(String);
impl Hash for Probe {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
",
            detect: detect_manual,
        },
        Evasion {
            what: "(g) rendering the interned name back to text before hashing",
            source: "\
fn key(name: &VarName) -> u64 {
    let mut state = DefaultHasher::new();
    name.as_str().hash(&mut state);
    state.finish()
}
",
            detect: detect_explicit,
        },
        Evasion {
            what: "(h) a private hasher built inside a function that takes text",
            source: "\
fn cache_key(source: &str, model: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    source.hash(&mut hasher);
    model.hash(&mut hasher);
    hasher.finish()
}
",
            detect: detect_scope,
        },
    ]
}

/// Source that must NOT be reported, so the detectors are not merely "return
/// everything".
fn permitted_shapes() -> Vec<(&'static str, &'static str)> {
    vec![
        (
            "deriving `Hash` over interned ids",
            "\
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Key {
    name: VarNameId,
    count: usize,
}
",
        ),
        (
            "a hand-written `Hash` over the id field of a type that also holds text",
            "\
struct FlatPath {
    name: VarName,
    parts: Vec<String>,
}
impl Hash for FlatPath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
",
        ),
        (
            "a content digest for a cache key that must survive the process",
            "\
fn digest(text: &str) -> String {
    let mut hasher = blake3::Hasher::new();
    hasher.update(text.as_bytes());
    hasher.finalize().to_hex().to_string()
}
",
        ),
        (
            "hashing a bounded summary instead of the bytes",
            "\
fn hash_text(text: &str, state: &mut impl Hasher) {
    text.len().hash(state);
}
",
        ),
    ]
}

/// A detector that has never been seen to fire is not evidence of anything.
///
/// Each fixture is source text, not a file on disk, so the gate proves its own
/// detection power on every CI run instead of relying on a probe someone ran
/// once by hand.
#[test]
fn test_detectors_reject_known_evasions() {
    for evasion in known_evasions() {
        let lines: Vec<&str> = evasion.source.lines().collect();
        let mut index = StringPayloadIndex::new();
        index_string_payloads(&lines, &mut index);
        let found = (evasion.detect)(&lines, &index);
        assert!(
            !found.is_empty(),
            "the string-hashing gate no longer detects {}. Fixture:\n{}",
            evasion.what,
            evasion.source
        );
    }
}

#[test]
fn test_detectors_accept_id_hashing_and_content_digests() {
    for (what, source) in permitted_shapes() {
        let lines: Vec<&str> = source.lines().collect();
        let mut index = StringPayloadIndex::new();
        index_string_payloads(&lines, &mut index);
        let found: Vec<usize> = [
            explicit_hash_offenders(&lines),
            derived_hash_offenders(&lines),
            manual_hash_offenders(&lines, &index),
            hash_scope_offenders(&lines),
        ]
        .concat();
        assert!(
            found.is_empty(),
            "the string-hashing gate falsely rejects {what} at lines {found:?}. \
Fixture:\n{source}"
        );
    }
}
