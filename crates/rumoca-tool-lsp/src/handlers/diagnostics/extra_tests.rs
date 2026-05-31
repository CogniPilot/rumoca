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

#[test]
fn unknown_operator_record_member_is_reported_via_lsp_compile_diagnostics() {
    let source = "operator record SE2\n  Real x;\n  Real y;\n  Real theta;\nend SE2;\n\nmodel Test2\n  SE2 pose;\nequation\n  der(pose.x) = 1;\n  der(pose.y) = 0;\n  der(pose.z) = 2;\nend Test2;\n";
    let mut session = Session::default();
    let diagnostics = compute_diagnostics(source, "input.mo", Some(&mut session));
    let unknown_member = diagnostics
        .iter()
        .find(|diag| {
            diag.code == Some(NumberOrString::String("ET001".to_string()))
                && diag.message.contains("unknown member `z`")
        })
        .unwrap_or_else(|| {
            panic!(
                "expected ET001 unknown-member diagnostic, got: {:?}",
                diagnostics
            )
        });
    assert_eq!(unknown_member.range.start.line, 11);
    assert_eq!(unknown_member.range.start.character, 11);
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
    let diagnostics = compute_diagnostics_with_mode(
        source,
        "input.mo",
        Some(&mut session),
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
