//! Formatter stability properties.
//!
//! Formatting must be a pure whitespace/layout rewrite: running it twice must
//! be the same as running it once (idempotence), and the significant token
//! stream — identifiers, numbers, punctuation, string literals, quoted
//! identifiers and *comment text* — must survive byte-for-byte.
//!
//! These properties are checked over three corpora:
//! 1. hand-written fixtures, including the reproducers for the comment- and
//!    state-machine corruption bugs,
//! 2. every `examples/models/*.mo` file in the repository (always runs),
//! 3. the cached Modelica Standard Library under `target/msl`, when present.
//!
//! `format` parses its input, so a successful second pass also proves that the
//! first pass produced source that still parses (SPEC_0022).

use super::*;

use std::fs;
use std::path::{Path, PathBuf};

const PROFILES: [FormatProfile; 2] = [FormatProfile::Dymola, FormatProfile::Canonical];

// ---------------------------------------------------------------------------
// Significant-token scanner
// ---------------------------------------------------------------------------

fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Index just past a `"`- or `'`-delimited region starting at `start`.
fn end_of_quoted(chars: &[char], start: usize, quote: char) -> usize {
    let mut index = start + 1;
    while index < chars.len() {
        match chars[index] {
            '\\' => index += 2,
            c if c == quote => return index + 1,
            _ => index += 1,
        }
    }
    chars.len()
}

/// Index just past the `*/` closing the block comment starting at `start`.
fn end_of_block_comment(chars: &[char], start: usize) -> usize {
    let mut index = start + 2;
    while index + 1 < chars.len() {
        if chars[index] == '*' && chars[index + 1] == '/' {
            return index + 2;
        }
        index += 1;
    }
    chars.len()
}

/// Index of the line terminator ending the `//` comment starting at `start`.
fn end_of_line_comment(chars: &[char], start: usize) -> usize {
    let mut index = start + 2;
    while index < chars.len() && !matches!(chars[index], '\n' | '\r') {
        index += 1;
    }
    index
}

fn end_of_word(chars: &[char], start: usize) -> usize {
    let mut index = start;
    while index < chars.len() && is_word_char(chars[index]) {
        index += 1;
    }
    index
}

/// The whitespace-insensitive token stream of a Modelica source text.
///
/// Whitespace between tokens is dropped — that is exactly what the formatter is
/// allowed to change. Everything else is emitted verbatim, so a rule that
/// rewrites bytes inside a comment or string shows up as a token mismatch.
/// Only trailing horizontal whitespace inside a `//` comment is normalized,
/// because `trim_trailing_whitespace` legitimately strips it.
fn significant_tokens(source: &str) -> Vec<String> {
    let chars: Vec<char> = source.chars().collect();
    let mut tokens = Vec::new();
    let mut index = 0usize;
    while index < chars.len() {
        let c = chars[index];
        if c.is_whitespace() {
            index += 1;
            continue;
        }
        let end = match c {
            '"' => end_of_quoted(&chars, index, '"'),
            '\'' => end_of_quoted(&chars, index, '\''),
            '/' if chars.get(index + 1) == Some(&'*') => end_of_block_comment(&chars, index),
            '/' if chars.get(index + 1) == Some(&'/') => end_of_line_comment(&chars, index),
            _ if is_word_char(c) => end_of_word(&chars, index),
            _ => index + 1,
        };
        let text: String = chars[index..end].iter().collect();
        if text.starts_with("//") {
            tokens.push(text.trim_end_matches([' ', '\t']).to_string());
        } else {
            tokens.push(text);
        }
        index = end;
    }
    tokens
}

// ---------------------------------------------------------------------------
// Property harness
// ---------------------------------------------------------------------------

/// `None` when the two token streams agree, otherwise a compact excerpt around
/// the first divergence (whole streams would be unreadable for MSL files).
fn token_mismatch_summary(before: &[String], after: &[String]) -> Option<String> {
    if before == after {
        return None;
    }
    let shared = before.len().min(after.len());
    let first = before
        .iter()
        .zip(after.iter())
        .position(|(lhs, rhs)| lhs != rhs)
        .unwrap_or(shared);
    let window = first.saturating_sub(4);
    Some(format!(
        "token {first}: {:?} -> {:?}",
        &before[window..before.len().min(first + 4)],
        &after[window..after.len().min(first + 4)],
    ))
}

/// `None` when the two texts agree, otherwise the first differing line.
fn text_mismatch_summary(before: &str, after: &str) -> Option<String> {
    if before == after {
        return None;
    }
    let first = before
        .lines()
        .zip(after.lines())
        .position(|(lhs, rhs)| lhs != rhs);
    Some(match first {
        Some(index) => format!(
            "line {}: {:?} -> {:?}",
            index + 1,
            before.lines().nth(index),
            after.lines().nth(index)
        ),
        None => format!(
            "line count differs: {} -> {}",
            before.lines().count(),
            after.lines().count()
        ),
    })
}

/// Assert the formatter is idempotent and token-preserving for `source`.
fn assert_format_stable(source: &str, label: &str) {
    let source_tokens = significant_tokens(source);
    for profile in PROFILES {
        let once = format(source, &FormatOptions::for_profile(profile))
            .map_err(|error| format!("{label} [{profile:?}]: first pass failed: {error}"))
            .expect("formatter must accept a parseable source");
        // A successful second pass also proves `once` still parses.
        let twice = format(&once, &FormatOptions::for_profile(profile))
            .map_err(|error| format!("{label} [{profile:?}]: second pass failed: {error}"))
            .expect("formatter output must re-parse and re-format");
        assert_eq!(
            text_mismatch_summary(&once, &twice),
            None,
            "{label} [{profile:?}]: formatting is not idempotent"
        );
        assert_eq!(
            token_mismatch_summary(&source_tokens, &significant_tokens(&once)),
            None,
            "{label} [{profile:?}]: formatting changed a token, string, or comment"
        );
    }
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

const FIXTURES: &[(&str, &str)] = &[
    (
        "compact_equation_spacing",
        "model Ball\n  Real x(start=10);\nequation\n  x=1;\n  y = 2;\nend Ball;",
    ),
    (
        "compact_statement_assignment",
        "function f\noutput Real y;\nalgorithm\ny:=1;\nend f;",
    ),
    (
        "call_output_assignment",
        "function f\noutput Real a;\noutput Real b;\nalgorithm\n(a,b):=g();\nend f;",
    ),
    (
        "expression_list_commas",
        "model C\n  Real x = f(1,2, 3, a=4, b =5);\n  Real y[3] = {1,2, 3};\nend C;",
    ),
    (
        "block_comment_in_equation_gap",
        "model C\n  Real x;\n  Real y;\nequation\n  x = /* gain == 2 */ y;\nend C;\n",
    ),
    (
        "block_comment_in_statement_gap",
        "function f\n  output Real y;\nalgorithm\n  y := /* reset:=0 */ 1;\nend f;\n",
    ),
    (
        "block_comment_in_modification_gap",
        "model C\n  Real x(start = /* nominal = 5 */ 1);\nend C;\n",
    ),
    (
        "escaped_backslash_string",
        "model C\n  constant String s = \"a\\\\\";\n  Real y ;   \nequation\n  y = 1;\nend C;\n",
    ),
    (
        "escaped_quote_string",
        "model C\n  constant String s = \"say \\\"hi\\\"\";\n  Real y ;   \nequation\n  y = 1;\nend C;\n",
    ),
    (
        "keyword_inside_block_comment",
        "model C\n/*\nequation\nalgorithm\nprotected\n*/\n\nReal x = 1;\nend C;\n",
    ),
    (
        "keyword_prefixed_identifier",
        "package P\n  type equationCount = Real;\n  model C\nequationCount a;\nReal x = 1;\n  end C;\nend P;\n",
    ),
    (
        "quoted_identifier_operator",
        "record Complex\nencapsulated operator '-'\nfunction negate\ninput Real x;\nalgorithm\nx := -x;\nend negate;\nend '-';\nend Complex;\n",
    ),
    (
        "multiline_string_description",
        "model C\n  Real x \"first line\nsecond line\";\nend C;\n",
    ),
    (
        "line_comments_everywhere",
        "// leading\nmodel C // trailing on class\n  Real x; // trailing on component\nequation\n  // standalone\n  x = 1; // trailing on equation\nend C;\n",
    ),
    (
        "annotation_graphics",
        "model C\n  Real x;\nequation\n  x = 1;\n  annotation(Icon(coordinateSystem(extent={{-100,-100},{100,100}})));\nend C;\n",
    ),
    (
        "conditional_and_loops",
        "model C\n  Real x;\n  Real y;\nequation\n  if x > 0 then\n    y = 1;\n  elseif x < 0 then\n    y = -1;\n  else\n    y = 0;\n  end if;\n  for i in 1:3 loop\n    x = i;\n  end for;\nend C;\n",
    ),
    (
        "extends_and_imports",
        "within Lib.Sub;\nmodel C\n  import Modelica.Constants;\n  extends Base(a = 1, b=2);\n  Real x = Constants.pi;\nend C;\n",
    ),
    (
        "nested_packages",
        "package P \"Top\"\n  package Inner \"Nested\"\n    model M \"Doc\"\n      Real x = 1;\n    end M;\n  end Inner;\nend P;\n",
    ),
    (
        "when_and_pre",
        "model C\n  Boolean b;\n  discrete Real x;\nequation\n  b = time > 1;\n  when b then\n    x = pre(x) + 1;\n  end when;\nend C;\n",
    ),
    (
        "crlf_source",
        "model C\r\n  Real x;\r\nequation\r\n  x = 1;\r\nend C;\r\n",
    ),
];

#[test]
fn format_is_stable_and_token_preserving_for_fixtures() {
    for (label, source) in FIXTURES {
        assert_format_stable(source, label);
    }
}

#[test]
fn significant_tokens_ignores_layout_but_keeps_comment_text() {
    assert_eq!(
        significant_tokens("x  =   1 ;"),
        significant_tokens("x=1;"),
        "layout differences must not change the token stream"
    );
    assert_ne!(
        significant_tokens("x = /* a == 2 */ 1;"),
        significant_tokens("x = /* a = = 2 */ 1;"),
        "comment text must be part of the token stream"
    );
    assert_ne!(
        significant_tokens("s = \"a b\";"),
        significant_tokens("s = \"a  b\";"),
        "string literal text must be part of the token stream"
    );
    assert_eq!(
        significant_tokens("x = 1; // note   "),
        significant_tokens("x = 1; // note"),
        "trailing whitespace inside a line comment is trimmable layout"
    );
}

// ---------------------------------------------------------------------------
// Repository corpora
// ---------------------------------------------------------------------------

fn crate_manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn collect_modelica_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_modelica_files(&path, out);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("mo") {
            out.push(path);
        }
    }
}

#[test]
fn format_is_stable_over_repo_example_models() {
    let root = crate_manifest_dir().join("../../examples/models");
    let mut files = Vec::new();
    collect_modelica_files(&root, &mut files);
    files.sort();
    assert!(
        !files.is_empty(),
        "expected Modelica example models under {}",
        root.display()
    );
    for path in files {
        let source = fs::read_to_string(&path).expect("read example model");
        assert_format_stable(&source, &path.display().to_string());
    }
}

/// The vendored MSL checkout, when the `target/msl` cache has been populated.
fn cached_msl_source_root() -> Option<PathBuf> {
    let root =
        crate_manifest_dir().join("../../target/msl/ModelicaStandardLibrary-4.1.0/Modelica 4.1.0");
    root.is_dir().then_some(root)
}

/// Message used both when skipping the sweep and when refusing to skip it.
const MSL_SWEEP_SKIP_MESSAGE: &str =
    "cached MSL not found under target/msl; skipping MSL formatter stability sweep";
const MSL_SWEEP_REQUIRED_MARKER: &str = "../../target/msl/formatter-stability-required";

/// Fail closed when the cache is missing but the sweep was demanded.
///
/// CI writes a fixed marker beside the MSL cache before populating it, so a
/// fetch failure must fail this 2500-file sweep instead of silently turning it
/// into a no-op. Same contract as `fmt_msl_copy_has_no_drift_and_bad_file_is_rewritten` in
/// `crates/rumoca/tests/cli_fmt_lint.rs`, expressed with `assert!` so the check
/// carries no bare panic (review-scan panic-discipline).
fn resolve_msl_sweep_root(cached_root: Option<PathBuf>, require_sweep: bool) -> Option<PathBuf> {
    assert!(
        cached_root.is_some() || !require_sweep,
        "formatter stability marker is present: {MSL_SWEEP_SKIP_MESSAGE}"
    );
    cached_root
}

fn msl_sweep_is_required() -> bool {
    crate_manifest_dir()
        .join(MSL_SWEEP_REQUIRED_MARKER)
        .is_file()
}

#[test]
#[should_panic(expected = "formatter stability marker is present")]
fn missing_msl_cache_fails_closed_when_the_sweep_is_required() {
    let _ = resolve_msl_sweep_root(None, true);
}

#[test]
fn missing_msl_cache_skips_only_when_the_sweep_is_not_required() {
    assert!(resolve_msl_sweep_root(None, false).is_none());
    let cached = PathBuf::from("target/msl");
    assert_eq!(
        resolve_msl_sweep_root(Some(cached.clone()), true),
        Some(cached)
    );
}

#[test]
fn format_is_stable_over_cached_msl() {
    let Some(root) = resolve_msl_sweep_root(cached_msl_source_root(), msl_sweep_is_required())
    else {
        eprintln!("{MSL_SWEEP_SKIP_MESSAGE}");
        return;
    };
    let mut files = Vec::new();
    collect_modelica_files(&root, &mut files);
    files.sort();
    assert!(
        files.len() > 1000,
        "expected the cached MSL tree to hold the full library, found {} files",
        files.len()
    );
    for path in files {
        let Ok(source) = fs::read_to_string(&path) else {
            continue;
        };
        assert_format_stable(&source, &path.display().to_string());
    }
}
