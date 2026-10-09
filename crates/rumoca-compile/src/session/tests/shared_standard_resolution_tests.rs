use super::*;

const ADVISORY_TARGET: &str = r#"
function Bare
  input Real u;
  output Real y;
  external "C" y=bare(u);
end Bare;
model Target
  parameter Integer n=2;
  input Real u[n];
  output Real y[n];
equation
  y=u;
end Target;
"#;

fn warning_codes(diagnostics: &ModelDiagnostics) -> Vec<&str> {
    diagnostics
        .diagnostics
        .iter()
        .filter(|diagnostic| {
            matches!(
                diagnostic.severity,
                rumoca_core::DiagnosticSeverity::Warning
            )
        })
        .filter_map(|diagnostic| diagnostic.code.as_deref())
        .collect()
}

#[test]
fn strict_and_standard_share_actual_construction_and_cached_resolve_warnings() {
    let mut session = Session::default();
    session.add_document("target.mo", ADVISORY_TARGET).unwrap();
    let strict = session.compile_model_strict("Target").unwrap();
    let PhaseResult::Success(standard) = session.compile_model_phases("Target").unwrap() else {
        panic!("Standard target should succeed");
    };
    assert!(Arc::ptr_eq(&strict.result().dae, &standard.dae));
    assert_eq!(
        serde_json::to_value(&strict.result().flat).unwrap(),
        serde_json::to_value(&standard.flat).unwrap()
    );
    let first = session.compile_model_diagnostics("Target");
    assert_eq!(warning_codes(&first), ["WR001", "WD001", "WD001"]);
    let warm = session.compile_model_diagnostics("Target");
    assert_eq!(
        serde_json::to_value(&first).unwrap(),
        serde_json::to_value(&warm).unwrap()
    );
    let save = session.semantic_diagnostics_query("Target", SemanticDiagnosticsMode::Save);
    assert_eq!(warning_codes(&save), ["WD001", "WD001"]);
}

fn assert_strict_fallback(unrelated: &str) {
    let mut session = Session::default();
    session
        .add_document(
            "target.mo",
            "model Target input Real u; output Real y; equation y=u; end Target;",
        )
        .unwrap();
    let _ = session.update_document("unrelated.mo", unrelated);
    let strict = session.compile_model_strict("Target").unwrap();
    assert!(session.resolved_cached().is_none());
    assert_eq!(strict.result().flat.variables.len(), 2);
    assert!(
        session
            .compile_model_diagnostics("Target")
            .global_resolution_failure
    );
    assert!(
        session
            .compile_model_strict_reachable_uncached_with_recovery("Target")
            .requested_succeeded()
    );
}

#[test]
fn strict_alone_preserves_unrelated_parse_and_resolution_failure_fallback() {
    assert_strict_fallback("model Broken Real x; equation x=missing; end Broken;");
    assert_strict_fallback("model Broken Real x equation x=1; end Broken;");
}

#[test]
fn generic_resolution_before_diagnostics_does_not_swallow_advisories() {
    let mut session = Session::default();
    session.add_document("target.mo", ADVISORY_TARGET).unwrap();
    session.resolved().unwrap();
    session.resolved().unwrap();
    assert_eq!(
        warning_codes(&session.compile_model_diagnostics("Target")),
        ["WR001", "WD001", "WD001"]
    );
}

#[test]
fn uncached_strict_preserves_exact_standard_input_and_warning_policy() {
    let mut session = Session::default();
    session.add_document("target.mo", ADVISORY_TARGET).unwrap();
    let report = session.compile_model_strict_reachable_uncached_with_recovery("Target");
    let Some(PhaseResult::Success(uncached)) = report.requested_result else {
        panic!("uncached target should succeed");
    };
    let PhaseResult::Success(standard) = session.compile_model_phases("Target").unwrap() else {
        panic!("Standard target should succeed");
    };
    assert_eq!(
        serde_json::to_value(&uncached.flat).unwrap(),
        serde_json::to_value(&standard.flat).unwrap()
    );
    assert_eq!(
        warning_codes(&session.compile_model_diagnostics("Target")),
        ["WR001", "WD001", "WD001"]
    );
}

#[test]
fn shared_input_keeps_standard_construction_refusal() {
    let mut session = Session::default();
    session
        .add_document("target.mo", "model Target Real x; end Target;")
        .unwrap();
    let strict = session.compile_model_strict("Target").unwrap_err();
    assert!(!strict.failures.is_empty());
    let diagnostics = session.compile_model_diagnostics("Target");
    assert!(!diagnostics.global_resolution_failure);
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .any(CommonDiagnostic::is_error)
    );
}
