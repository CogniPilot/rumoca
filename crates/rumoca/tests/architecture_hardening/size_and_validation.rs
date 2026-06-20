use super::*;

#[test]
fn test_rs_files_stay_under_spec_0021_hard_limit() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders: Vec<String> = rs_files
        .into_iter()
        .filter(|path| is_line_count_checked_source_file(path))
        .filter_map(|path| {
            let content = fs::read_to_string(&path).ok()?;
            let line_count = content.lines().count();
            let has_exception = content.contains("SPEC_0021")
                && content.contains("file-size")
                && content.contains("split plan");
            (line_count > 2000 && !has_exception)
                .then(|| format!("{} ({line_count} lines)", path.display()))
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "Rust files over the SPEC_0021 hard action threshold of 2,000 lines need \
an explicit SPEC_0021 file-size exception and split plan: {offenders:#?}"
    );
}

fn is_line_count_checked_source_file(path: &Path) -> bool {
    let rel = path
        .strip_prefix(workspace_root())
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    !rel.contains("/generated/")
}

#[test]
fn test_production_dummy_span_fallbacks_do_not_increase() {
    const MAX_DUMMY_SPAN_FALLBACKS: usize = 0;

    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_dummy_span_fallback_counted_file(path))
        .flat_map(|path| dummy_span_fallback_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production Span::DUMMY fallback expressions increased from the \
baseline of {MAX_DUMMY_SPAN_FALLBACKS} to {}. SPEC_0008 requires preserving \
real provenance or bubbling an explicit unspanned error instead of falling \
back to Span::DUMMY. New code must remove at least as many fallback sites as \
it adds. Offenders: {offenders:#?}",
        offenders.len()
    );
}

#[test]
fn test_production_expression_span_dummy_fallbacks_do_not_increase() {
    const MAX_EXPRESSION_SPAN_DUMMY_FALLBACKS: usize = 0;

    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_production_span_debt_counted_file(path))
        .flat_map(|path| expression_span_dummy_fallback_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production expression/reference span fallbacks to Span::DUMMY increased from \
the baseline of {MAX_EXPRESSION_SPAN_DUMMY_FALLBACKS} to {}. Source-derived IR \
must use a required owner span and bubble a typed unspanned contract error when \
provenance is missing; new code must remove at least as many fallback sites as \
it adds. Offenders: {offenders:#?}",
        offenders.len()
    );
}

fn expression_span_dummy_fallback_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_expression_span_dummy_fallback(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn line_has_expression_span_dummy_fallback(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && line.contains(".span().unwrap_or(")
        && (line.contains("Span::DUMMY") || line.contains("rumoca_core::Span::DUMMY"))
}

#[test]
fn test_production_source_map_dummy_span_fallbacks_do_not_increase() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_dummy_span_fallback_counted_file(path))
        .flat_map(|path| source_map_location_to_span_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production SourceMap::location_to_span calls are forbidden because \
that API falls back to Span::DUMMY when source lookup fails. New code must use \
try_location_to_span and bubble missing source context explicitly instead of \
silently manufacturing dummy provenance. Offenders: {offenders:#?}",
    );
}

fn is_dummy_span_fallback_counted_file(path: &Path) -> bool {
    is_production_span_debt_counted_file(path)
}

#[test]
fn test_span_type_does_not_implement_default() {
    let path = workspace_root().join("crates/rumoca-core/src/ir_primitives.rs");
    let content = fs::read_to_string(&path).expect("read rumoca-core ir primitives");
    let span_struct = content
        .find("pub struct Span")
        .expect("Span struct declaration should exist");
    let prefix = &content[..span_struct];
    let derive_start = prefix.rfind("#[derive(");
    let derive_default = derive_start
        .and_then(|start| {
            prefix[start..]
                .find(")]")
                .map(|end| &prefix[start..start + end])
        })
        .is_some_and(|derive| derive.contains("Default"));

    assert!(
        !derive_default && !content.contains("impl Default for Span"),
        "Span must not implement Default; use Span::DUMMY only at explicit \
source-free boundaries or Span::source_free_serde_default for serde absent-span \
compatibility"
    );
}

#[test]
fn test_production_span_defaults_are_forbidden() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_production_span_debt_counted_file(path))
        .flat_map(|path| default_span_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production span fields must not be default-constructed because \
Span::default() creates the dummy sentinel and hides provenance loss. Use a real owner \
span, bubble a provenance error, or route explicitly source-free constructs \
through a named helper. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_production_dummy_span_uses_do_not_increase() {
    const MAX_DUMMY_SPAN_USES: usize = 0;

    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_production_span_debt_counted_file(path))
        .flat_map(|path| dummy_span_use_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production Span::DUMMY uses increased from the baseline of \
{MAX_DUMMY_SPAN_USES} to {}. SPEC_0008 requires preserving real provenance \
where possible; new source-free IR must use an explicit generated/unspanned \
path instead of adding direct dummy-span construction. Offenders: {offenders:#?}",
        offenders.len()
    );
}

#[test]
fn test_production_source_free_span_escape_hatches_do_not_increase() {
    const MAX_SOURCE_FREE_SPAN_ESCAPE_HATCHES: usize = 0;

    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_production_span_debt_counted_file(path))
        .flat_map(|path| source_free_span_escape_hatch_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production source-free span escape-hatch uses increased from the \
baseline of {MAX_SOURCE_FREE_SPAN_ESCAPE_HATCHES} to {}. Source-free spans are \
only valid at explicit serde/default or source-free IR boundaries; source-derived \
lowering must preserve an owner span or bubble an unspanned contract error. \
Offenders: {offenders:#?}",
        offenders.len()
    );
}

#[test]
fn test_generated_subscripts_do_not_use_dummy_span_in_new_production_code() {
    const MAX_GENERATED_SUBSCRIPT_DUMMY_SPANS: usize = 0;

    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_production_span_debt_counted_file(path))
        .flat_map(|path| generated_dummy_subscript_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production generated subscripts using Span::DUMMY increased from the \
baseline of {MAX_GENERATED_SUBSCRIPT_DUMMY_SPANS} to {}. Generated subscripts \
must inherit an owner span from the source expression, equation, or declaration \
that caused them. Offenders: {offenders:#?}",
        offenders.len()
    );
}

#[test]
fn test_generated_span_helpers_are_forbidden() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_production_span_debt_counted_file(path))
        .flat_map(|path| generated_span_helper_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production generated span helpers are forbidden because generated \
source-derived IR should inherit a real owner span. Truly source-free \
boundaries must be named explicitly as source-free/unspanned instead of \
        `generated_*_span`. Offenders: {offenders:#?}"
    );
}

#[test]
fn test_raw_generated_subscript_constructors_do_not_increase() {
    const MAX_RAW_GENERATED_SUBSCRIPT_CONSTRUCTORS: usize = 0;

    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let offenders = rs_files
        .into_iter()
        .filter(|path| is_production_span_debt_counted_file(path))
        .flat_map(|path| raw_generated_subscript_locations(&path))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "production raw generated-subscript constructors increased from the \
baseline of {MAX_RAW_GENERATED_SUBSCRIPT_CONSTRUCTORS} to {}. Fallible \
production code should prefer `try_generated_*` or `generated_*_with_provenance` \
so dummy spans cannot silently enter generated IR. Offenders: {offenders:#?}",
        offenders.len()
    );
}

fn is_production_span_debt_counted_file(path: &Path) -> bool {
    let rel = path
        .strip_prefix(workspace_root())
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");
    !rel.contains("/generated/")
        && !rel.contains("/tests/")
        && !rel.ends_with("/tests.rs")
        && !rel.ends_with("_tests.rs")
        && !rel.ends_with("/test_support.rs")
        && !rel.ends_with("architecture_hardening_test.rs")
        && !rel.contains("/architecture_hardening/")
}

fn dummy_span_fallback_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_dummy_span_fallback(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn source_map_location_to_span_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_source_map_location_to_span_call(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn line_has_source_map_location_to_span_call(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (line.contains(".location_to_span(") || line.contains("fn location_to_span(&self"))
}

fn line_has_dummy_span_fallback(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    code.contains("Span::DUMMY")
        && (code.contains("unwrap_or(")
            || code.contains("unwrap_or_else(")
            || code.contains("map_or("))
}

fn default_span_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_default_span_construction(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn line_has_default_span_construction(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (line.contains("Span::default()")
            || line.contains("span: Default::default()")
            || line.contains("span: std::default::Default::default()")
            || line.contains("span: core::default::Default::default()"))
}

fn dummy_span_use_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_dummy_span_use(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn source_free_span_escape_hatch_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_source_free_span_escape_hatch(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn generated_dummy_subscript_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_generated_dummy_subscript(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn generated_span_helper_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_generated_span_helper(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn raw_generated_subscript_locations(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };
    production_source_lines(&content)
        .filter(|(_, line)| line_has_raw_generated_subscript_constructor(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn line_has_generated_span_helper(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//") && trimmed.contains("fn generated_") && trimmed.contains("_span")
}

fn line_has_raw_generated_subscript_constructor(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (line.contains("Subscript::generated_index(")
            || line.contains("Subscript::generated_colon(")
            || line.contains("Subscript::generated_expr("))
}

fn line_has_generated_dummy_subscript(line: &str) -> bool {
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && line.contains("Span::DUMMY")
        && (line.contains("Subscript::generated_index(")
            || line.contains("Subscript::generated_colon(")
            || line.contains("Subscript::generated_expr(")
            || line.contains("Subscript::colon("))
}

fn line_has_dummy_span_use(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    let trimmed = line.trim_start();
    !trimmed.starts_with("//")
        && (code.contains("Span::DUMMY") || code.contains("rumoca_core::Span::DUMMY"))
}

fn line_has_source_free_span_escape_hatch(line: &str) -> bool {
    let code = line_without_literals_or_line_comment(line);
    code.contains("Span::source_free_serde_default(")
        || code.contains("source_free_span()")
        || code.contains("ScalarProgramBlock::source_free(")
        || code.contains("source_free_generated_index(")
}

fn line_without_literals_or_line_comment(line: &str) -> String {
    let mut code = String::with_capacity(line.len());
    let mut idx = 0;
    while idx < line.len() {
        if line[idx..].starts_with("//") {
            break;
        }

        if let Some(end) = raw_string_literal_end(line, idx) {
            code.push('"');
            idx = end;
            continue;
        }

        let Some(ch) = line[idx..].chars().next() else {
            break;
        };
        if ch == '"' {
            code.push('"');
            idx = cooked_string_literal_end(line, idx + ch.len_utf8());
            continue;
        }

        code.push(ch);
        idx += ch.len_utf8();
    }
    code
}

fn cooked_string_literal_end(line: &str, mut idx: usize) -> usize {
    let mut escaped = false;
    while idx < line.len() {
        let Some(ch) = line[idx..].chars().next() else {
            break;
        };
        idx += ch.len_utf8();
        if escaped {
            escaped = false;
            continue;
        }
        match ch {
            '\\' => escaped = true,
            '"' => break,
            _ => {}
        }
    }
    idx
}

fn raw_string_literal_end(line: &str, start: usize) -> Option<usize> {
    let bytes = line.as_bytes();
    let mut idx = match bytes.get(start..) {
        Some([b'r', ..]) => start + 1,
        Some([b'b', b'r', ..]) => start + 2,
        _ => return None,
    };

    let mut hashes = 0;
    while bytes.get(idx) == Some(&b'#') {
        idx += 1;
        hashes += 1;
    }
    if bytes.get(idx) != Some(&b'"') {
        return None;
    }

    let mut search = idx + 1;
    while search < bytes.len() {
        if raw_string_closes_at(bytes, search, hashes) {
            return Some(search + 1 + hashes);
        }
        search += 1;
    }
    Some(bytes.len())
}

fn raw_string_closes_at(bytes: &[u8], quote_idx: usize, hashes: usize) -> bool {
    if bytes.get(quote_idx) != Some(&b'"') {
        return false;
    }
    let close_end = quote_idx + 1 + hashes;
    close_end <= bytes.len()
        && bytes[quote_idx + 1..close_end]
            .iter()
            .all(|byte| *byte == b'#')
}

fn production_source_lines(content: &str) -> impl Iterator<Item = (usize, &str)> {
    let mut pending_cfg_test = false;
    let mut skipped_cfg_test_depth = 0i32;
    content.lines().enumerate().filter(move |(_, line)| {
        if skipped_cfg_test_depth > 0 {
            skipped_cfg_test_depth += brace_delta(line);
            return false;
        }

        let trimmed = line.trim_start();
        if pending_cfg_test {
            if trimmed.starts_with("#[") {
                return false;
            }
            pending_cfg_test = false;
            if starts_cfg_test_item(trimmed) {
                skipped_cfg_test_depth = brace_delta(line).max(0);
                return false;
            }
        }

        if trimmed.starts_with("#[cfg(test)]") {
            pending_cfg_test = true;
            return false;
        }
        true
    })
}

fn starts_cfg_test_item(trimmed: &str) -> bool {
    trimmed.starts_with("mod ")
        || trimmed.starts_with("pub mod ")
        || trimmed.starts_with("fn ")
        || trimmed.starts_with("pub fn ")
        || trimmed.starts_with("use ")
}

fn brace_delta(line: &str) -> i32 {
    line.chars().fold(0, |delta, ch| match ch {
        '{' => delta + 1,
        '}' => delta - 1,
        _ => delta,
    })
}

#[test]
fn test_production_source_lines_skip_inline_cfg_test_modules() {
    let content = r#"
fn production() {
    let _span = Span::DUMMY;
}

#[cfg(test)]
mod tests {
    fn fixture() {
        let _span = Span::DUMMY;
    }
}

fn production_after_tests() {
    let _span = rumoca_core::Span::DUMMY;
}
"#;

    let direct_uses = production_source_lines(content)
        .filter(|(_, line)| line_has_dummy_span_use(line))
        .count();

    assert_eq!(direct_uses, 2);
}

#[test]
fn test_dummy_span_scan_ignores_string_literals() {
    assert!(!line_has_dummy_span_use(
        r#"let needles = ["SourceId(0)", "Span::DUMMY"];"#
    ));
    assert!(!line_has_dummy_span_use(
        r##"let snippet = r#"let span = Span::DUMMY;"#;"##
    ));
    assert!(!line_has_dummy_span_use(
        "let span = real_span; // Span::DUMMY"
    ));
    assert!(line_has_dummy_span_use("let span = Span::DUMMY;"));
}

#[test]
fn test_ir_crates_have_no_public_scalarize_functions() {
    // SPEC_0007 keeps scalarization out of IR crates; backend/evaluator
    // fallback helpers live in rumoca-eval-solve.
    let root = workspace_root();
    let mut offenders = Vec::new();

    for path in collect_ir_crate_rs_files(&root) {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        offenders.extend(content.lines().enumerate().filter_map(|(line_idx, line)| {
            public_scalarize_function_location(&path, line_idx, line)
        }));
    }

    assert!(
        offenders.is_empty(),
        "found public scalarise/to_scalar functions in rumoca-ir-* crates (SPEC_0007 violation). \
Move scalarization logic to rumoca-eval-solve or an execution adapter crate: {offenders:#?}"
    );
}

#[test]
fn test_ast_visitor_helpers_stay_split_by_behavior_shape() {
    let root = workspace_root();
    let visitor_root = root.join("crates/rumoca-ir-ast/src/visitor");
    let facade = root.join("crates/rumoca-ir-ast/src/visitor.rs");
    let facade_content = fs::read_to_string(&facade).expect("read AST visitor facade");

    for required in ["read_only.rs", "rewrite.rs", "query.rs"] {
        assert!(
            visitor_root.join(required).exists(),
            "rumoca-ir-ast visitor helpers must stay split by behavior shape; missing {required}"
        );
    }

    assert!(
        facade_content.contains("mod read_only;")
            && facade_content.contains("mod rewrite;")
            && facade_content.contains("mod query;"),
        "AST visitor facade must route through read_only/rewrite/query modules"
    );

    let read_only = fs::read_to_string(visitor_root.join("read_only.rs"))
        .expect("read AST read-only visitor module");
    let rewrite =
        fs::read_to_string(visitor_root.join("rewrite.rs")).expect("read AST rewrite module");
    assert!(
        read_only.contains("pub trait Visitor") && !read_only.contains("transform_expression"),
        "read_only.rs must contain traversal only, without rewrite-shape transforms"
    );
    assert!(
        rewrite.contains("pub trait ExpressionTransformer")
            && !rewrite.contains("pub trait Visitor"),
        "rewrite.rs must contain rewrite-shape transforms without owning read-only traversal"
    );
}

#[test]
fn test_ast_visitor_helpers_do_not_import_phase_or_eval_semantics() {
    let root = workspace_root();
    let visitor_root = root.join("crates/rumoca-ir-ast/src/visitor");
    let mut offenders = Vec::new();

    for path in [
        root.join("crates/rumoca-ir-ast/src/visitor.rs"),
        visitor_root.join("read_only.rs"),
        visitor_root.join("rewrite.rs"),
        visitor_root.join("query.rs"),
    ] {
        let content = fs::read_to_string(&path).expect("read AST visitor source");
        for (line_idx, line) in content.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("use rumoca_phase_") || trimmed.starts_with("use rumoca_eval_") {
                offenders.push(format!("{}:{}", path.display(), line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "AST visitor helpers are allowed to traverse/query/rewrite AST shape, \
but must not import phase or evaluator semantics: {offenders:#?}"
    );
}

#[test]
fn test_session_root_facade_exports_are_minimal() {
    let session_lib = workspace_root().join("crates/rumoca-compile/src/lib.rs");
    let content = fs::read_to_string(&session_lib).expect("read rumoca-compile lib.rs");
    let root_pub_uses = collect_root_pub_use_statements(&content);

    let expected = vec!["pub use compile::{Session, SessionConfig};".to_string()];

    assert_eq!(
        root_pub_uses, expected,
        "unexpected rumoca-compile root exports. \
Author reminder: SPEC_0029_CRATE_BOUNDARIES.md §9 requires `rumoca-compile` \
root exports to stay minimal (`Session`, `SessionConfig`) and to keep other APIs namespaced."
    );
}

/// SPEC_0029 §3a: `rumoca-core` is the sole Tier-1 foundation. No
/// `rumoca-ir-core` crate exists. Enforce that the directory has not been
/// re-created and that no Cargo.toml declares a `rumoca-ir-core` dependency.
#[test]
fn test_no_separate_ir_core_foundation_crate() {
    let root = workspace_root();

    assert!(
        !root.join("crates/rumoca-ir-core").exists(),
        "crates/rumoca-ir-core was dissolved into rumoca-core per SPEC_0029 §3a; \
do not re-create it"
    );

    let mut offenders = Vec::new();
    for entry in fs::read_dir(root.join("crates")).expect("read crates dir") {
        let entry = entry.expect("crates entry");
        let cargo = entry.path().join("Cargo.toml");
        let Ok(content) = fs::read_to_string(&cargo) else {
            continue;
        };
        for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
            if section_contains_dependency(&content, section, "rumoca-ir-core") {
                offenders.push(format!("{} [{section}]", cargo.display()));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "found Cargo.toml entries declaring removed `rumoca-ir-core` dep: {offenders:#?}; \
SPEC_0029 §3a: depend on rumoca-core instead"
    );

    let workspace_cargo =
        fs::read_to_string(root.join("Cargo.toml")).expect("read workspace Cargo.toml");
    assert!(
        !workspace_cargo.contains("\"crates/rumoca-ir-core\""),
        "workspace Cargo.toml still references crates/rumoca-ir-core in [members]"
    );
    assert!(
        !workspace_cargo.contains("rumoca-ir-core ="),
        "workspace Cargo.toml still declares rumoca-ir-core in [workspace.dependencies]"
    );
}

/// SPEC_0029 §12 + SPEC_0007 §Structural Transformation Scope: DAE structural
/// transformations (index reduction, state demotion, BLT, tearing) live in
/// `rumoca-phase-structural`. `rumoca-phase-solve` only lowers a finalized
/// DAE to Solve-IR; it does not mutate DAE mathematical structure.
#[test]
fn test_dae_structural_transforms_live_in_phase_structural() {
    let root = workspace_root();

    let banned_symbols = [
        "index_reduce_missing_state_derivatives",
        "demote_states_without_derivative_refs",
        "demote_states_without_assignable_derivative_rows",
        "demote_orphan_states_without_equation_refs",
        "demote_states_without_retained_derivative_rows",
    ];

    let mut offenders = Vec::new();
    let phase_solve_src = root.join("crates/rumoca-phase-solve/src");
    let mut rs_files = Vec::new();
    collect_rs_files(&phase_solve_src, &mut rs_files);
    for path in rs_files {
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for (line_idx, line) in content.lines().enumerate() {
            // Only flag definition sites (fn name) or pub uses; calls to these
            // symbols (if any) are legitimate downstream usage from
            // phase-structural via a public API.
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }
            if let Some(banned) = banned_symbols.iter().find(|name| {
                trimmed.starts_with(&format!("pub fn {name}"))
                    || trimmed.starts_with(&format!("fn {name}"))
                    || trimmed.contains("pub use ") && line.contains(*name)
            }) {
                offenders.push(format!(
                    "{}:{} defines {banned}",
                    path.display(),
                    line_idx + 1
                ));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "DAE structural transformations must live in rumoca-phase-structural \
(SPEC_0029 §12, SPEC_0007 §Structural Transformation Scope), not phase-solve: \
{offenders:#?}"
    );

    assert!(
        root.join("crates/rumoca-phase-structural/src/dae_prepare")
            .exists(),
        "phase-structural must own the dae_prepare module after relocation"
    );
}

/// SPEC_0007 Stage 3 Contract: source temporal operators are eliminated in every DAE partition.
/// The `phase-dae::appendix_b_validation::validate_no_source_temporal_operator_survives`
/// gate must exist as a positive runtime check (not just absent from compile).
#[test]
fn test_source_temporal_operator_validation_gate_exists() {
    let path = workspace_root().join("crates/rumoca-phase-dae/src/appendix_b_validation.rs");
    let content = fs::read_to_string(&path).expect("read appendix_b_validation.rs");

    assert!(
        content.contains("validate_no_source_temporal_operator_survives"),
        "phase-dae::appendix_b_validation must define validate_no_source_temporal_operator_survives \
to enforce SPEC_0007 Stage 3 Contract (no pre/edge/change/sample/previous in solver-facing DAE-IR)"
    );

    let error_path = workspace_root().join("crates/rumoca-phase-dae/src/errors.rs");
    let error_content = fs::read_to_string(&error_path).expect("read errors.rs");
    assert!(
        error_content.contains("SourceTemporalOperatorSurvivedDaeBoundary"),
        "ToDaeError must include SourceTemporalOperatorSurvivedDaeBoundary so the gate \
produces a structured diagnostic per SPEC_0008"
    );

    let pre_lowering_path = workspace_root().join("crates/rumoca-phase-dae/src/pre_lowering.rs");
    let pre_lowering_content =
        fs::read_to_string(&pre_lowering_path).expect("read pre_lowering.rs");
    for partition in [
        "dae.continuous.equations",
        "dae.discrete.real_updates",
        "dae.discrete.valued_updates",
        "dae.conditions.equations",
    ] {
        assert!(
            pre_lowering_content.contains(partition),
            "phase-dae::pre_lowering must process every DAE partition; missing {partition}"
        );
    }
}

/// SPEC_0007 Stage 4 Contract: Solve-IR is the register-machine form of
/// Appendix B and must have a positive validation gate at the lowering boundary.
#[test]
fn test_solve_ir_appendix_b_validation_gate_exists() {
    let root = workspace_root();
    let path = root.join("crates/rumoca-phase-solve/src/appendix_b_validation.rs");
    let content = fs::read_to_string(&path).expect("read phase-solve appendix_b_validation.rs");

    for required in [
        "validate_solve_input_appendix_b_invariants",
        "validate_solve_problem_appendix_b_invariants",
        "validate_solve_artifacts_appendix_b_invariants",
        "find_source_temporal_operator",
        "validate_function_calls_resolve",
        "validate_row_ops",
        "validate_compute_node",
        "SeedUse::Forbidden",
        "SeedUse::Allowed",
    ] {
        assert!(
            content.contains(required),
            "phase-solve Appendix-B validation must define `{required}`"
        );
    }

    let lib_path = root.join("crates/rumoca-phase-solve/src/lib.rs");
    let lib_content = fs::read_to_string(&lib_path).expect("read phase-solve lib.rs");
    assert!(
        lib_content.contains("mod appendix_b_validation;")
            && lib_content.contains("validate_solve_input_appendix_b_invariants(dae_model)")
            && lib_content.contains("validate_solve_problem_appendix_b_invariants(&problem)")
            && lib_content.contains("validate_solve_artifacts_appendix_b_invariants(&artifacts)"),
        "lower_solve_problem must call the Solve-IR Appendix-B validation gate \
at the DAE input, SolveProblem output, and SolveArtifacts output boundaries"
    );
    assert!(
        !lib_content.contains("pub fn lower_continuous_solve_artifacts"),
        "continuous artifact helper must stay private so public callers use \
the validated SolveArtifacts lowering path"
    );

    let solve_model_path = root.join("crates/rumoca-phase-solve/src/solve_model.rs");
    let solve_model_content =
        fs::read_to_string(&solve_model_path).expect("read phase-solve solve_model.rs");
    let problem_lowering = solve_model_content
        .find("crate::lower_solve_problem_with_solver_len(&dae_model, solver_len)?")
        .or_else(|| {
            solve_model_content.find("crate::lower_solve_problem_with_solver_len_and_model_span(")
        })
        .expect(
            "lower_dae_to_solve_model_inner must lower through the validated SolveProblem path",
        );
    let artifact_lowering = solve_model_content
        .find("crate::lower_solve_artifacts_with_mass_matrix(&problem, mass_matrix)?")
        .expect("lower_dae_to_solve_model_inner must derive runtime artifacts from SolveProblem");
    assert!(
        problem_lowering < artifact_lowering,
        "lower_dae_to_solve_model_inner must run validated Solve-IR lowering before artifact lowering"
    );
}

/// SPEC_0008: generated `connect()` flat equations must carry the originating
/// `connect()` source span, not `Span::DUMMY`. The `ConnectionSet` type must
/// carry a `span` field and the connection-derived equation-generation
/// functions (`generate_equality_equations` and `generate_flow_equation`)
/// must pass it through. Unconnected-flow defaults (`flow = 0` for flow
/// variables never mentioned in a connect()) are true compiler-generated
/// constructs with no source span, so they are allowed to use `Span::DUMMY`
/// per SPEC_0008's "compiler-generated constructs" exception.
#[test]
fn test_connection_equations_carry_real_spans() {
    let root = workspace_root();
    let connections_mod =
        fs::read_to_string(root.join("crates/rumoca-phase-flatten/src/connections/mod.rs"))
            .expect("read connections/mod.rs");
    assert!(
        connections_mod.contains("span: rumoca_core::Span"),
        "ConnectionSet must carry a `span` field so generated equations can \
preserve the originating connect() span (SPEC_0008)"
    );

    let eq_gen = fs::read_to_string(
        root.join("crates/rumoca-phase-flatten/src/connections/equation_generation.rs"),
    )
    .expect("read equation_generation.rs");

    // Scope the gate to the two connection-derived generator functions.
    let mut offenders = Vec::new();
    let mut current_fn: Option<&str> = None;
    let connection_derived_fns = ["generate_equality_equations", "generate_flow_equation"];
    for (idx, line) in eq_gen.lines().enumerate() {
        let trimmed = line.trim_start();
        if let Some(fn_name) = trimmed
            .strip_prefix("pub(super) fn ")
            .or_else(|| trimmed.strip_prefix("fn "))
            .or_else(|| trimmed.strip_prefix("pub fn "))
        {
            let name = fn_name.split('(').next().unwrap_or("").trim();
            current_fn = connection_derived_fns
                .iter()
                .copied()
                .find(|&candidate| candidate == name);
        }
        if let Some(fn_name) = current_fn
            && !trimmed.starts_with("//")
            && line.contains("Equation::new_array")
            && line.contains("Span::DUMMY")
        {
            offenders.push(format!(
                "{}:{} in fn {fn_name}",
                "equation_generation.rs",
                idx + 1
            ));
        }
    }
    assert!(
        offenders.is_empty(),
        "found Equation::new_array calls passing Span::DUMMY in connection-derived \
generators: {offenders:#?}. SPEC_0008: pass the connection set's span instead."
    );
}

#[test]
fn test_core_semantic_phase_diagnostics_do_not_reconstruct_source_zero() {
    let root = workspace_root();
    let phase_src_dirs = [
        "crates/rumoca-phase-typecheck/src",
        "crates/rumoca-phase-flatten/src",
        "crates/rumoca-phase-dae/src",
        "crates/rumoca-phase-solve/src",
    ];
    let banned = [
        "Span::from_offsets(SourceId(0)",
        "Span::from_offsets(rumoca_core::SourceId(0)",
        "Span::new(SourceId(0)",
        "Span::new(rumoca_core::SourceId(0)",
    ];

    let offenders = phase_src_dirs
        .iter()
        .flat_map(|phase_src_dir| source_zero_span_offenders(&root.join(phase_src_dir), &banned))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "semantic phase diagnostics must preserve source identity instead of reconstructing spans with SourceId(0): {offenders:?}"
    );
}

fn source_zero_span_offenders(phase_src_dir: &Path, banned: &[&str]) -> Vec<String> {
    let mut files = Vec::new();
    collect_rs_files(phase_src_dir, &mut files);
    files
        .into_iter()
        .flat_map(|path| source_zero_span_offenders_in_file(&path, banned))
        .collect()
}

fn source_zero_span_offenders_in_file(path: &Path, banned: &[&str]) -> Vec<String> {
    let content = fs::read_to_string(path).expect("read phase source file");
    let production_content = strip_cfg_test_modules(&content);
    production_content
        .lines()
        .enumerate()
        .filter(|(_, line)| banned.iter().any(|needle| line.contains(needle)))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

fn strip_cfg_test_modules(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    let mut pending_cfg_test = false;
    let mut skipping_depth: Option<usize> = None;

    for line in content.lines() {
        let trimmed = line.trim_start();
        if let Some(depth) = skipping_depth.as_mut() {
            *depth = depth.saturating_add(line.matches('{').count());
            *depth = depth.saturating_sub(line.matches('}').count());
            if *depth == 0 {
                skipping_depth = None;
            }
            out.push('\n');
            continue;
        }

        if trimmed.starts_with("#[cfg(test)]") {
            pending_cfg_test = true;
            out.push('\n');
            continue;
        }

        if pending_cfg_test && trimmed.starts_with("mod tests") {
            let depth = line
                .matches('{')
                .count()
                .saturating_sub(line.matches('}').count());
            if depth > 0 {
                skipping_depth = Some(depth);
            }
            pending_cfg_test = false;
            out.push('\n');
            continue;
        }

        pending_cfg_test = false;
        out.push_str(line);
        out.push('\n');
    }

    out
}

/// SPEC_0029 §12 + WASM optionality: `rumoca-bind-wasm` must not pull in
/// `diffsol` (or any concrete solver backend) transitively in its default
/// feature set. Confirmed via `cargo metadata --no-deps` + a feature-aware
/// dependency walk.
#[test]
fn test_bind_wasm_default_graph_does_not_include_diffsol() {
    use std::process::Command;
    let output = Command::new(env!("CARGO"))
        .args([
            "tree",
            "-p",
            "rumoca-bind-wasm",
            "--edges",
            "normal",
            "--prefix",
            "depth",
        ])
        .current_dir(workspace_root())
        .output()
        .expect("run cargo tree");
    assert!(
        output.status.success(),
        "cargo tree -p rumoca-bind-wasm failed: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let banned: Vec<_> = stdout
        .lines()
        .filter(|l| {
            l.contains("diffsol")
                || l.contains("rumoca-sim")
                || l.contains("rumoca-solver-diffsol")
                || l.contains("rumoca-solver-rk45")
        })
        .map(str::to_owned)
        .collect();
    assert!(
        banned.is_empty(),
        "rumoca-bind-wasm default transitive graph still includes solver \
crates (SPEC_0029 §12 + WASM optionality):\n{}\nUse default-features=false on \
rumoca-tool-lsp to drop the simulation tail.",
        banned.join("\n"),
    );
}
