use super::*;

#[cfg(feature = "fmi")]
#[test]
fn fmi_component_error_preserves_source_diagnostic() {
    let result = compile_scalar_cuda_smoke_demo();
    let sources = source_map(&result);
    let source = sources.get_id("ScalarCudaSmoke.mo").unwrap();
    let (_, text) = sources.get_source(source).unwrap();
    let start = text.find("der(x)").unwrap();
    let span = sources
        .try_span(source, start, start + "der(x)".len())
        .unwrap();
    let error = fmi_component_error(
        &result,
        rumoca_sim::SimulationDiagnosticError::RuntimePreparation {
            message: "checked FMI component refused".to_string(),
            span: Some(span),
        },
    );
    let Some(CompilerError::SourceDiagnosticsError {
        summary,
        diagnostics,
        source_map,
    }) = error.downcast_ref::<CompilerError>()
    else {
        panic!("source-backed FMI refusal lost its diagnostic: {error:#}");
    };
    assert_eq!(summary, "Construct checked FMI component");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code.as_deref(), Some("EX002"));
    assert_eq!(diagnostics[0].message, "checked FMI component refused");
    assert_eq!(diagnostics[0].labels.len(), 1);
    assert!(diagnostics[0].labels[0].primary);
    assert_eq!(diagnostics[0].labels[0].span, span);
    assert_eq!(
        diagnostics[0].labels[0].message.as_deref(),
        Some("simulation failure originates here")
    );
    assert_eq!(
        source_map.get_source(source),
        Some(("ScalarCudaSmoke.mo", text))
    );
}

#[cfg(feature = "fmi")]
#[test]
fn fmi_component_error_keeps_source_free_cause() {
    let result = compile_scalar_cuda_smoke_demo();
    let error = fmi_component_error(
        &result,
        rumoca_sim::SimulationDiagnosticError::RuntimePreparation {
            message: "source-free FMI refusal".to_string(),
            span: None,
        },
    );
    assert_eq!(
        format!("{error:#}"),
        "Construct checked FMI component: source-free FMI refusal"
    );
    assert!(error.downcast_ref::<CompilerError>().is_none());
    assert!(
        error
            .downcast_ref::<rumoca_sim::SimulationDiagnosticError>()
            .is_some()
    );
}
