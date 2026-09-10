//! SPEC_0021 file-size gate and production span-debt accounting.

use super::super::*;
use super::helpers::*;

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
