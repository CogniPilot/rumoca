//! The instantiate-time evaluator must decline, never invent.
//!
//! Policy: SPEC_0008 §"Fail-Fast Error Semantics" — "Recovery by substituting
//! `0`, `1`, `false`, an empty shape, an empty collection, `Span::DUMMY`, a
//! first enum variant, an arbitrary component, or any other default is
//! forbidden" — and SPEC_0032 §3, which is why a rendered name is not a value.
//!
//! Three silent-wrong-results defects traced back to two concrete shapes in the
//! AST evaluators, and each was caught by review or by a failing gate rather
//! than by the code that produced it:
//!
//! 1. **`start` read as a value.** MLS §4.9 makes `start` an initial *guess* for
//!    a simulation variable. The parser seeds `start` with the declared type's
//!    default (`0.0`, `0`, `false`) for every declaration, so a "binding, else
//!    start" fallback answers "what is this parameter?" with a number the model
//!    never wrote. It pruned an if-branch on a discrete Boolean unknown
//!    `CV(start = false)` and deleted the else-branch residual equation.
//!
//! 2. **A rendered component reference compared against a value.** Turning an
//!    unresolved reference into its own spelling and comparing that text to an
//!    enumeration literal made `Medium.ThermoStates == IndependentVariables.ph`
//!    answer a confident `false` for an unresolvable package alias.
//!
//! Both gates below allow **zero** occurrences. Unlike
//! `TEXTUAL_MODEL_PATH_RECOVERY_DEBT` there is no tolerated baseline: a model
//! that "compiles" on an invented value is overwhelmingly unlikely to match its
//! reference trace, so the coverage such a model appears to provide is
//! illusory. A hard, spanned error is strictly better than a wrong number.

use super::*;
use std::path::{Path, PathBuf};

/// Crates whose entire purpose is turning Modelica expressions into values.
///
/// The AST-level evaluator has no business reading a component's `start`
/// attribute at all: `start` is not among the inputs to "what is this
/// expression's value?". (DAE/Solve-level crates are excluded on purpose —
/// there `start` is the modelled initial condition, which is its real meaning.)
const AST_EVALUATION_CRATE_SRC: &str = "crates/rumoca-eval-ast/src";

/// Crates that consume the AST `Component`, where `start` is an
/// `ast::Expression` attribute and a component's value must come from its
/// binding (MLS §4.4.4).
///
/// DAE/Solve crates are excluded deliberately: there `Variable::start` is the
/// modelled initial condition, and MLS §8.6 does make it a parameter's value.
const AST_CONSUMING_CRATE_SRCS: &[&str] = &[
    "crates/rumoca-eval-ast/src",
    "crates/rumoca-phase-resolve/src",
    "crates/rumoca-phase-typecheck/src",
    "crates/rumoca-phase-instantiate/src",
    "crates/rumoca-phase-flatten/src",
];

/// Reads of a component's binding, which is where a value legitimately lives.
const BINDING_MARKERS: &[&str] = &[".binding", "has_explicit_binding"];

/// Combinators that turn the next expression into an *alternative* to the
/// binding. `binding -> <combinator> -> start` is the fabrication, whichever
/// combinator spells it.
const FALLBACK_COMBINATORS: &[&str] = &[
    "unwrap_or",
    "or_else",
    ".or(",
    "then_some",
    "chain(",
    "else",
];

/// Identifiers that name a component reference rather than a value.
const COMPONENT_REFERENCE_NAMES: &[&str] = &[
    "comp_ref",
    "component_ref",
    "cref",
    "comp_reference",
    "reference",
];

fn production_rs_files() -> Vec<PathBuf> {
    let root = workspace_root();
    let mut files = Vec::new();
    for crate_dir in workspace_crate_dirs(&root) {
        let src = crate_dir.join("src");
        if src.is_dir() {
            collect_rs_files(&src, &mut files);
        }
    }
    files.retain(|path| !is_test_only_source(path));
    files
}

fn ast_consuming_rs_files() -> Vec<PathBuf> {
    let root = workspace_root();
    let mut files = Vec::new();
    for src in AST_CONSUMING_CRATE_SRCS {
        collect_rs_files(&root.join(src), &mut files);
    }
    files.retain(|path| !is_test_only_source(path));
    files
}

/// Test fixtures build components by hand and are allowed to poke `start`.
fn is_test_only_source(path: &Path) -> bool {
    let name = path
        .file_name()
        .map(|name| name.to_string_lossy().to_string())
        .unwrap_or_else(|| path.display().to_string());
    if name == "tests.rs" || name.ends_with("_tests.rs") {
        return true;
    }
    path.components()
        .any(|component| component.as_os_str() == "tests")
}

/// Drop `#[cfg(test)]` modules and line comments, then flatten to statements.
///
/// Statements — not lines — are the unit that matters: the fabrication was
/// written across five lines with the combinator and the `.start` read far
/// apart, so a line-at-a-time scan would have missed it. Splitting on `;` and
/// on braces keeps a chunk inside one expression, so a combinator in one
/// function and a `.start` read in the next are not conflated.
fn production_statements(content: &str) -> Vec<String> {
    let mut kept = String::new();
    let mut skip_depth: Option<i32> = None;
    let mut pending_cfg_test = false;

    for line in content.lines() {
        let code = line.split("//").next().unwrap_or("");
        if let Some(depth) = skip_depth.as_mut() {
            *depth += brace_delta(code);
            if *depth <= 0 {
                skip_depth = None;
            }
            continue;
        }
        if line.trim_start().starts_with("#[cfg(test)]") {
            pending_cfg_test = true;
            continue;
        }
        if std::mem::take(&mut pending_cfg_test)
            && let Some(depth) = block_opening_depth(code)
        {
            skip_depth = Some(depth);
            continue;
        }
        kept.push_str(code);
        kept.push(' ');
    }

    kept.split([';', '{', '}'])
        .map(str::to_string)
        .filter(|statement| !statement.trim().is_empty())
        .collect()
}

fn brace_delta(code: &str) -> i32 {
    let opens = code.matches('{').count() as i32;
    let closes = code.matches('}').count() as i32;
    opens - closes
}

/// Net brace depth when `code` opens a block that stays open, else `None`.
fn block_opening_depth(code: &str) -> Option<i32> {
    let depth = brace_delta(code);
    (code.contains('{') && depth > 0).then_some(depth)
}

/// True when `text` reads a `.start` *field* (not `.start_is_modification`,
/// not the `.start()` method on spans and ranges).
fn reads_start_field(text: &str) -> bool {
    let mut rest = text;
    while let Some(idx) = rest.find(".start") {
        let after = &rest[idx + ".start".len()..];
        let next = after.chars().next();
        let is_field = !matches!(next, Some(c) if c.is_alphanumeric() || c == '_' || c == '(');
        if is_field {
            return true;
        }
        rest = after;
    }
    false
}

#[test]
fn test_ast_evaluator_never_reads_start_as_a_value() {
    let root = workspace_root();
    let eval_src = root.join(AST_EVALUATION_CRATE_SRC);
    let mut files = Vec::new();
    collect_rs_files(&eval_src, &mut files);
    files.retain(|path| !is_test_only_source(path));

    let mut offenders = Vec::new();
    for path in files {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for statement in production_statements(&content) {
            if reads_start_field(&statement) {
                let rel = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
                offenders.push(format!("{rel}: {}", statement.trim()));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "an AST evaluation crate read a component's `start` attribute. MLS §4.9 \
makes `start` an initial guess, and the parser seeds it with the declared \
type's default, so reading it answers an unknown with an invented value \
(SPEC_0008). Return `None` and let the caller take the undecidable path. \
Allowed count is ZERO — there is no debt list: {offenders:#?}"
    );
}

/// The offset just past the first occurrence of any `needles` at or after
/// `from`, or `None`.
fn find_any_after(text: &str, needles: &[&str], from: usize) -> Option<usize> {
    needles
        .iter()
        .filter_map(|needle| {
            text[from..]
                .find(needle)
                .map(|idx| from + idx + needle.len())
        })
        .min()
}

/// The offset of the first `.start` *field* read at or after `from`.
fn find_start_field(text: &str, from: usize) -> Option<usize> {
    let mut cursor = from;
    while let Some(idx) = text[cursor..].find(".start") {
        let absolute = cursor + idx;
        let after = &text[absolute + ".start".len()..];
        let next = after.chars().next();
        if !matches!(next, Some(c) if c.is_alphanumeric() || c == '_' || c == '(') {
            return Some(absolute);
        }
        cursor = absolute + ".start".len();
    }
    None
}

/// Longest gap tolerated between the binding read and the `start` read.
///
/// Wide enough for the five-line `if has_explicit_binding && !start_is_modification
/// && !matches!(comp.start, ...)` form once whitespace is collapsed, narrow
/// enough that unrelated attribute lists do not pair up by accident.
const MAX_FALLBACK_SPAN: usize = 200;

/// `binding` -> fallback combinator -> `start`, in that order and close together.
fn binding_else_start_fallback(statement: &str) -> Option<String> {
    let mut search_from = 0;
    while let Some(after_binding) = find_any_after(statement, BINDING_MARKERS, search_from) {
        search_from = after_binding;
        let Some(after_combinator) = find_any_after(statement, FALLBACK_COMBINATORS, after_binding)
        else {
            continue;
        };
        let Some(start_at) = find_start_field(statement, after_combinator) else {
            continue;
        };
        if start_at - after_binding <= MAX_FALLBACK_SPAN {
            return Some(
                statement[after_binding..start_at + ".start".len()]
                    .trim()
                    .to_string(),
            );
        }
    }
    None
}

#[test]
fn test_no_binding_else_start_value_fallback_in_production_code() {
    let root = workspace_root();
    let mut offenders = Vec::new();

    for path in ast_consuming_rs_files() {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for statement in production_statements(&content) {
            let Some(excerpt) = binding_else_start_fallback(&statement) else {
                continue;
            };
            let rel = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
            offenders.push(format!("{rel}: ...{excerpt}"));
        }
    }

    assert!(
        offenders.is_empty(),
        "production code fell back to a component's `start` attribute for its \
value. `start` is an initial guess (MLS §4.9) that the parser seeds with the \
declared type's default, so \"binding, else start\" silently invents `0`, \
`0.0` or `false` for an unbound declaration (SPEC_0008). Use the declaration \
binding alone and treat its absence as undecidable. Allowed count is ZERO — \
there is no debt list: {offenders:#?}"
    );
}

#[test]
fn test_no_rendered_component_reference_compared_against_a_value() {
    let root = workspace_root();
    let mut offenders = Vec::new();

    for path in production_rs_files() {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for statement in production_statements(&content) {
            let Some(offence) = rendered_reference_value_offence(&statement) else {
                continue;
            };
            let rel = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
            offenders.push(format!("{rel} [{offence}]: {}", statement.trim()));
        }
    }

    assert!(
        offenders.is_empty(),
        "production code used a component reference's rendered text as a value. \
A name is not a value (SPEC_0032 §3): comparing a reference's spelling against \
an enumeration literal, or substituting the spelling when resolution failed, \
turns \"unknown\" into a confident wrong answer (SPEC_0008). Recognize an \
enumeration literal against the class tree instead, and otherwise return \
`None`. Allowed count is ZERO — there is no debt list: {offenders:#?}"
    );
}

/// Which fabrication shape, if any, this statement contains.
fn rendered_reference_value_offence(statement: &str) -> Option<&'static str> {
    let renders_reference = COMPONENT_REFERENCE_NAMES
        .iter()
        .any(|name| statement.contains(&format!("{name}.to_string()")));
    if !renders_reference {
        return None;
    }
    if statement.contains("==") || statement.contains("!=") {
        return Some("compared against a value");
    }
    if statement.contains("map(|_|") {
        return Some("substituted for an unresolved value");
    }
    None
}
