use super::*;
use rumoca_compile::compile::core::{PrimaryLabel, SourceMap};
use rumoca_core::Span;

#[test]
fn source_diagnostic_report_preserves_spans() {
    let mut source_map = SourceMap::new();
    let source_id = source_map.add("Pkg/A.mo", "model A end A;");
    let span = Span::from_offsets(source_id, 6, 7);
    let diagnostic = CommonDiagnostic::error(
        "PKG-007",
        "duplicate class name",
        PrimaryLabel::new(span).with_message("duplicate class here"),
    );

    let report = build_source_diagnostic_report(&diagnostic, &source_map);
    let rendered = format!("{report:?}");
    assert!(
        rendered.contains("PKG-007"),
        "code should be preserved: {rendered}"
    );
    assert!(
        rendered.contains("duplicate class name"),
        "message should be preserved: {rendered}"
    );
    assert!(
        rendered.contains("Pkg/A.mo"),
        "source file should be shown: {rendered}"
    );
}

#[test]
fn source_diagnostic_report_formats_global_errors_without_source_blocks() {
    let report = build_source_diagnostic_report(
        &CommonDiagnostic::global_error("PKG-006", "directory '/tmp/Pkg' is missing package.mo"),
        &SourceMap::new(),
    );
    let rendered = format!("{report:?}");
    assert!(
        rendered.contains("PKG-006"),
        "code should be preserved: {rendered}"
    );
    assert!(
        rendered.contains("missing package.mo"),
        "message should be preserved: {rendered}"
    );
    assert!(
        !rendered.contains("unknown"),
        "global errors should not invent fake source blocks: {rendered}"
    );
}
