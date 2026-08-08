use super::*;

#[test]
fn parse_diagnostics_missing_semicolon_before_equation_has_nonzero_range() {
    let source =
        "model Ball\n  Real x(start=0);\n  Real v(start=1)\nequation\n  der(x) = v;\nend Ball;\n";
    let diagnostics = compute_diagnostics(source, "input.mo", None);
    assert!(!diagnostics.is_empty(), "expected parse diagnostics");
    let first = &diagnostics[0];
    assert!(
        first.range.start.line > 0 || first.range.start.character > 0,
        "expected range recovered away from line 1 when possible"
    );
    assert!(
        !first.message.contains("`equation` is a reserved keyword"),
        "should avoid reserved-keyword mislabel for section transition"
    );
}

/// `pose.z` names a member that `SE2` does not have.
///
/// `SE2` is an operator record, so MLS §4.5/§14 give it a closed member set that
/// is fully known from the declaration alone — unlike an expandable connector,
/// whose members are supplied by `connect` equations and therefore stay deferred
/// past Resolve. Resolve is consequently the first phase that can prove the tail
/// is wrong, and SPEC_0008 ("unresolved references MUST be hard errors" in
/// Resolve) makes it the owner: this is `ER002` at Resolve, not `ET001` at
/// Typecheck. The reported range is the whole failing reference `pose.z`, which
/// is the span Resolve carries for the reference.
#[test]
fn unknown_operator_record_member_is_reported_as_resolve_error_via_lsp_compile_diagnostics() {
    let source = "operator record SE2\n  Real x;\n  Real y;\n  Real theta;\nend SE2;\n\nmodel Test2\n  SE2 pose;\nequation\n  der(pose.x) = 1;\n  der(pose.y) = 0;\n  der(pose.z) = 2;\nend Test2;\n";
    let mut session = Session::default();
    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    let unknown_member = diagnostics
        .iter()
        .find(|diag| {
            diag.code == Some(NumberOrString::String("ER002".to_string()))
                && diag
                    .message
                    .contains("unresolved component reference: 'pose.z'")
        })
        .unwrap_or_else(|| {
            panic!(
                "expected ER002 unresolved-member diagnostic, got: {:?}",
                diagnostics
            )
        });
    assert_eq!(unknown_member.range.start.line, 11);
    assert_eq!(unknown_member.range.start.character, 6);
    assert_eq!(unknown_member.range.end.line, 11);
    assert_eq!(unknown_member.range.end.character, 12);
    assert_eq!(unknown_member.data, Some(json!({ "precise_range": true })));
}

#[test]
fn unqualified_typo_is_resolve_error_in_save_diagnostics() {
    let source = r#"record Inner
  Real x;
end Inner;

model Active
  Inner pid;
  Real x2;
equation
  pid.x = 0;
  der(x2) = x;
end Active;
"#;
    let mut session = Session::default();
    let diagnostics = compute_diagnostics_with_options(
        source,
        "input.mo",
        Some(&mut session),
        &LintOptions::default(),
        SemanticDiagnosticsMode::Save,
    );
    let unresolved = diagnostics
        .iter()
        .find(|diag| {
            diag.code == Some(NumberOrString::String("ER002".to_string()))
                && diag.message.contains("unresolved component reference: 'x'")
        })
        .unwrap_or_else(|| panic!("expected ER002 unresolved `x`, got: {diagnostics:?}"));
    assert_eq!(unresolved.range.start.line, 9);
    assert_eq!(unresolved.range.start.character, 12);
    assert_eq!(unresolved.range.end.line, 9);
    assert_eq!(unresolved.range.end.character, 13);
}
