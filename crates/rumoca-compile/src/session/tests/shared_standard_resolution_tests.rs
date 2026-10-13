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

/// A strict compile after a standard compile adds no standard build and no
/// closure resolve: it reads the standard resolve already in the session.
fn assert_reuses_standard_resolve(before: crate::compile::SessionCacheStatsSnapshot) {
    let delta = crate::compile::session_cache_stats().delta_since(before);
    assert_eq!(delta.standard_resolved_builds, 0);
    assert_eq!(delta.strict_resolved_builds, 0);
    assert_eq!(delta.standard_resolved_cache_hits, 1);
}

#[test]
fn strict_reuses_an_existing_standard_construction_and_cached_resolve_warnings() {
    let _guard = session_stats_test_guard();
    let mut session = Session::default();
    session.add_document("target.mo", ADVISORY_TARGET).unwrap();
    let PhaseResult::Success(standard) = session.compile_model_phases("Target").unwrap() else {
        panic!("Standard target should succeed");
    };
    let before = crate::compile::session_cache_stats();
    let strict = session.compile_model_strict("Target").unwrap();
    assert_reuses_standard_resolve(before);
    assert!(Arc::ptr_eq(&strict.result().dae, &standard.dae));
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
    let _guard = session_stats_test_guard();
    assert_strict_fallback("model Broken Real x; equation x=missing; end Broken;");
    assert_strict_fallback("model Broken Real x equation x=1; end Broken;");
}

#[test]
fn generic_resolution_before_diagnostics_does_not_swallow_advisories() {
    let _guard = session_stats_test_guard();
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
fn uncached_strict_reuses_an_existing_standard_input_and_warning_policy() {
    let _guard = session_stats_test_guard();
    let mut session = Session::default();
    session.add_document("target.mo", ADVISORY_TARGET).unwrap();
    let PhaseResult::Success(standard) = session.compile_model_phases("Target").unwrap() else {
        panic!("Standard target should succeed");
    };
    let before = crate::compile::session_cache_stats();
    let report = session.compile_model_strict_reachable_uncached_with_recovery("Target");
    assert_reuses_standard_resolve(before);
    let Some(PhaseResult::Success(uncached)) = report.requested_result else {
        panic!("uncached target should succeed");
    };
    assert_eq!(uncached.flat.variables.len(), standard.flat.variables.len());
    assert_eq!(
        warning_codes(&session.compile_model_diagnostics("Target")),
        ["WR001", "WD001", "WD001"]
    );
}

#[test]
fn shared_input_keeps_standard_construction_refusal() {
    let _guard = session_stats_test_guard();
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

#[test]
fn strict_recovery_compile_resolves_the_target_closure_without_a_standard_build() {
    let _guard = session_stats_test_guard();
    let mut session = Session::default();
    session.add_document("target.mo", ADVISORY_TARGET).unwrap();
    assert!(
        session
            .compile_model_strict_reachable_with_recovery("Target")
            .requested_succeeded()
    );
    assert!(session.resolved_cached().is_none());
}

#[test]
fn fresh_session_strict_compile_of_one_model_performs_no_standard_resolved_build() {
    let _guard = session_stats_test_guard();
    let mut session = Session::default();
    session.add_document("target.mo", ADVISORY_TARGET).unwrap();
    session
        .add_document("other.mo", "model Other Real x; equation x=1; end Other;")
        .unwrap();
    crate::compile::reset_session_cache_stats();
    let strict = session.compile_model_strict("Target").unwrap();
    assert_eq!(strict.result().flat.variables.len(), 3);
    assert_eq!(
        crate::compile::session_cache_stats().standard_resolved_builds,
        0
    );
    assert!(session.resolved_cached().is_none());
}

/// The flat output of one target does not depend on session history: a fresh
/// closure-only compile and a compile that reuses the session-wide standard
/// resolve serialize to the same canonical bytes, with no table excluded.
#[test]
fn closure_and_standard_compiles_serialize_to_identical_canonical_flat_output() {
    let _guard = session_stats_test_guard();
    let build = || {
        let mut session = Session::default();
        session
            .add_document("unrelated.mo", UNRELATED_CLASSES)
            .unwrap();
        session.add_document("target.mo", RICH_TARGET).unwrap();
        session
    };
    let mut fresh = build();
    crate::compile::reset_session_cache_stats();
    let closure = fresh.compile_model_strict("Target").unwrap();
    assert_eq!(
        crate::compile::session_cache_stats().standard_resolved_builds,
        0
    );
    let mut warmed = build();
    let PhaseResult::Success(standard) = warmed.compile_model_phases("Target").unwrap() else {
        panic!("Standard target should succeed");
    };
    assert_eq!(
        closure.result().flat.canonical_json().unwrap(),
        standard.flat.canonical_json().unwrap()
    );
}

const UNRELATED_CLASSES: &str = r#"
type Mode = enumeration(idle, busy);
record Pair Real a; Real b; end Pair;
function Twice input Real u; output Real y; algorithm y := 2 * u; end Twice;
model Other Mode m = Mode.busy; Pair p(a = 1, b = 2); Real x; equation x = Twice(p.a); end Other;
"#;

const RICH_TARGET: &str = r#"
type Level = enumeration(low, high);
record Sample Real v; Integer k; end Sample;
function Scale input Real u; output Real y; algorithm y := 3 * u; end Scale;
model Target
  parameter Level level = Level.high;
  Sample s(v = 1, k = 2);
  Real x(start = 0);
equation
  der(x) = Scale(s.v) + (if level == Level.high then 1 else 0);
end Target;
"#;
