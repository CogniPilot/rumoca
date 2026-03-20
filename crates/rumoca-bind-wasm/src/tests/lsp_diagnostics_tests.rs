use super::*;

#[test]
fn test_lsp_diagnostics_reports_unknown_builtin_modifier_with_multiple_classes() {
    let mut session = Session::default();
    let source = r#"
    package Lib
      model Helper
        Real y;
      equation
        y = 1.0;
      end Helper;
    end Lib;

    model M
      Real x(startd = 1.0);
    equation
      der(x) = -x;
    end M;
    "#;

    let json =
        lsp_diagnostics_in_session(&mut session, source).expect("diagnostics should serialize");
    let diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&json).expect("diagnostics should be valid JSON");

    assert!(
        diagnostics.iter().any(|d| {
            d.get("code").and_then(|c| c.as_str()) == Some("ET001")
                && d.get("message")
                    .and_then(|m| m.as_str())
                    .is_some_and(|m| m.contains("unknown modifier `startd`"))
        }),
        "expected ET001 unknown-modifier diagnostic, got: {:?}",
        diagnostics
    );
}

#[test]
fn test_lsp_diagnostics_reports_unknown_builtin_modifier_startdt() {
    let mut session = Session::default();
    let source = r#"
    model M
      Real x(startdt = 1.0);
    equation
      der(x) = -x;
    end M;
    "#;

    let json =
        lsp_diagnostics_in_session(&mut session, source).expect("diagnostics should serialize");
    let diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&json).expect("diagnostics should be valid JSON");

    assert!(
        diagnostics.iter().any(|d| {
            d.get("code").and_then(|c| c.as_str()) == Some("ET001")
                && d.get("message")
                    .and_then(|m| m.as_str())
                    .is_some_and(|m| m.contains("unknown modifier `startdt`"))
        }),
        "expected ET001 unknown-modifier diagnostic, got: {:?}",
        diagnostics
    );
}

#[test]
fn test_lsp_code_actions_returns_unknown_modifier_fix() {
    let mut session = Session::default();
    let source = r#"
    model M
      Real x(startdt = 1.0);
    equation
      der(x) = -x;
    end M;
    "#;

    let diag_json =
        lsp_diagnostics_in_session(&mut session, source).expect("diagnostics should serialize");
    let diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&diag_json).expect("diagnostics should be valid JSON");
    let et001 = diagnostics
        .iter()
        .find(|d| d.get("code").and_then(|c| c.as_str()) == Some("ET001"))
        .expect("expected ET001 diagnostic");

    let range = et001.get("range").expect("diagnostic range");
    let start = range
        .get("start")
        .expect("range.start should exist")
        .as_object()
        .expect("range.start should be object");
    let end = range
        .get("end")
        .expect("range.end should exist")
        .as_object()
        .expect("range.end should be object");
    let start_line = start
        .get("line")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;
    let start_character = start
        .get("character")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;
    let end_line = end.get("line").and_then(|v| v.as_u64()).unwrap_or_default() as u32;
    let end_character = end
        .get("character")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;

    let actions_json = lsp_code_actions(
        source,
        start_line,
        start_character,
        end_line,
        end_character,
        &serde_json::to_string(&vec![et001]).expect("serialize diagnostics"),
    )
    .expect("code actions should serialize");
    let actions: Vec<serde_json::Value> =
        serde_json::from_str(&actions_json).expect("actions should be valid JSON");
    assert!(
        actions.iter().any(|action| {
            action
                .get("title")
                .and_then(|t| t.as_str())
                .is_some_and(|title| title.contains("Replace `startdt` with `start`"))
        }),
        "expected unknown-modifier quick-fix action, got: {actions:?}"
    );
}

#[test]
fn test_lsp_code_actions_returns_missing_semicolon_fix() {
    let mut session = Session::default();
    let source = r#"
    model M
      Real x(start=0)
    equation
      der(x) = -x;
    end M;
    "#;

    let diag_json =
        lsp_diagnostics_in_session(&mut session, source).expect("diagnostics should serialize");
    let diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&diag_json).expect("diagnostics should be valid JSON");
    let missing_semicolon_diag = diagnostics
        .iter()
        .find(|d| {
            d.get("code").and_then(|c| c.as_str()) == Some("EP001")
                && d.get("message").and_then(|m| m.as_str()).is_some_and(|m| {
                    m.contains("missing `;`") || m.contains("unexpected `equation`")
                })
        })
        .unwrap_or_else(|| panic!("expected missing-semicolon diagnostic, got: {diagnostics:?}"));

    let range = missing_semicolon_diag
        .get("range")
        .expect("diagnostic range");
    let start = range
        .get("start")
        .expect("range.start should exist")
        .as_object()
        .expect("range.start should be object");
    let end = range
        .get("end")
        .expect("range.end should exist")
        .as_object()
        .expect("range.end should be object");
    let start_line = start
        .get("line")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;
    let start_character = start
        .get("character")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;
    let end_line = end.get("line").and_then(|v| v.as_u64()).unwrap_or_default() as u32;
    let end_character = end
        .get("character")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;

    let actions_json = lsp_code_actions(
        source,
        start_line,
        start_character,
        end_line,
        end_character,
        &serde_json::to_string(&vec![missing_semicolon_diag]).expect("serialize diagnostics"),
    )
    .expect("code actions should serialize");
    let actions: Vec<serde_json::Value> =
        serde_json::from_str(&actions_json).expect("actions should be valid JSON");
    assert!(
        actions.iter().any(|action| {
            action
                .get("title")
                .and_then(|t| t.as_str())
                .is_some_and(|title| title.contains("Insert missing `;`"))
        }),
        "expected missing-semicolon quick-fix action, got: {actions:?}"
    );
}

#[test]
fn test_lsp_code_actions_returns_did_you_mean_type_fix() {
    let mut session = Session::default();
    let source = r#"
    model Ball
      Real x(start=0);
      Readl v(start=1);
    equation
      der(x) = v;
      der(v) = -9.81;
    end Ball;
    "#;

    let diag_json =
        lsp_diagnostics_in_session(&mut session, source).expect("diagnostics should serialize");
    let diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&diag_json).expect("diagnostics should be valid JSON");
    let unresolved_type_diag = diagnostics
        .iter()
        .find(|d| {
            d.get("code").and_then(|c| c.as_str()) == Some("ER002")
                && d.get("message")
                    .and_then(|m| m.as_str())
                    .is_some_and(|m| m.contains("unresolved type reference"))
        })
        .unwrap_or_else(|| panic!("expected unresolved-type diagnostic, got: {diagnostics:?}"));

    let range = unresolved_type_diag.get("range").expect("diagnostic range");
    let start = range
        .get("start")
        .expect("range.start should exist")
        .as_object()
        .expect("range.start should be object");
    let end = range
        .get("end")
        .expect("range.end should exist")
        .as_object()
        .expect("range.end should be object");
    let start_line = start
        .get("line")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;
    let start_character = start
        .get("character")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;
    let end_line = end.get("line").and_then(|v| v.as_u64()).unwrap_or_default() as u32;
    let end_character = end
        .get("character")
        .and_then(|v| v.as_u64())
        .unwrap_or_default() as u32;

    let actions_json = lsp_code_actions(
        source,
        start_line,
        start_character,
        end_line,
        end_character,
        &serde_json::to_string(&vec![unresolved_type_diag]).expect("serialize diagnostics"),
    )
    .expect("code actions should serialize");
    let actions: Vec<serde_json::Value> =
        serde_json::from_str(&actions_json).expect("actions should be valid JSON");
    assert!(
        actions.iter().any(|action| {
            action
                .get("title")
                .and_then(|t| t.as_str())
                .is_some_and(|title| title.contains("Replace with `Real`"))
        }),
        "expected did-you-mean quick-fix action, got: {actions:?}"
    );
}

#[test]
fn test_lsp_diagnostics_reports_builtin_modifier_type_mismatch() {
    let mut session = Session::default();
    let source = r#"
    model M
      Boolean df = true;
      Real v(start = df);
    equation
      der(v) = -v;
    end M;
    "#;

    let json =
        lsp_diagnostics_in_session(&mut session, source).expect("diagnostics should serialize");
    let diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&json).expect("diagnostics should be valid JSON");

    assert!(
        diagnostics.iter().any(|d| {
            d.get("code").and_then(|c| c.as_str()) == Some("ET002")
                && d.get("message").and_then(|m| m.as_str()).is_some_and(|m| {
                    m.contains("modifier `start`") && m.contains("expects `Real`, found `Boolean`")
                })
        }),
        "expected ET002 modifier type mismatch diagnostic, got: {:?}",
        diagnostics
    );
}
