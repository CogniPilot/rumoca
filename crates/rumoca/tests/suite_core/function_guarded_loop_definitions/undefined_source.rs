//! Exact source-owner refusal checks shared by the loop controls.

pub(crate) fn assert_refusal(
    error: &rumoca::CompilerError,
    source: &str,
    model: &str,
    feature: &str,
    detail: &str,
) {
    let rumoca::CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    } = error
    else {
        panic!("expected checked source diagnostics: {error}");
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == model)
        .expect("the requested model owns the refusal");
    assert_eq!(
        failure
            .phase
            .as_ref()
            .map(|phase| phase.to_string())
            .as_deref(),
        Some("ToDae")
    );
    assert!(
        failure
            .error_code
            .as_deref()
            .is_some_and(|code| code.ends_with("ED019")),
        "{error}"
    );
    assert_eq!(
        failure.error,
        format!("unsupported Flat semantic owner `{feature}`: {detail}")
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("the refusal retains its source label");
    assert!(label.primary);
    let (_, issued_source) = source_map
        .as_ref()
        .expect("the refusal retains its source map")
        .get_source(label.span.source)
        .expect("the primary label belongs to an actual source");
    assert_eq!(issued_source, source);
    assert!(label.span.start.0 < label.span.end.0);
    assert!(source.get(label.span.start.0..label.span.end.0).is_some());
}
