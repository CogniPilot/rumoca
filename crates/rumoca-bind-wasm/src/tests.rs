use super::*;

static SESSION_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

const MINI_MODELICA_LIBRARY: &str = r#"
    within ;
    package Modelica
      package Blocks
        package Sources
          model Constant
            parameter Real k = 1.0;
            output Real y;
          equation
            y = k;
          end Constant;
        end Sources;
      end Blocks;
    end Modelica;
    "#;

const USES_MODELICA_SOURCE: &str = r#"
    model UsesModelica
      import Modelica.Blocks.Sources.Constant;
      Constant c(k = 2.0);
      Real y;
    equation
      y = c.y;
    end UsesModelica;
    "#;

fn mini_modelica_library_json() -> String {
    serde_json::json!({
        "Modelica/package.mo": MINI_MODELICA_LIBRARY,
    })
    .to_string()
}

fn session_test_guard() -> std::sync::MutexGuard<'static, ()> {
    SESSION_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

#[test]
fn test_get_version() {
    let version = get_version();
    assert!(!version.is_empty());
}

#[test]
fn test_get_git_commit() {
    let commit = get_git_commit();
    assert!(!commit.is_empty());
}

#[test]
fn test_get_build_time_utc() {
    let build_time = get_build_time_utc();
    assert!(!build_time.is_empty());
}

#[test]
fn test_parse_valid() {
    let result = validate_source_syntax("model M Real x; end M;", "test.mo");
    assert!(result.is_ok());
}

#[test]
fn test_parse_invalid() {
    let result = validate_source_syntax("model M Real x end M;", "test.mo");
    assert!(result.is_err());
}

#[test]
fn test_list_classes_includes_nested_packages() {
    let source = r#"
    package Lib
      package Nested
        model Probe
          Real x;
        equation
          x = 1.0;
        end Probe;
      end Nested;
    end Lib;
    "#;

    let mut session = Session::default();
    session.update_document("input.mo", source);
    let json = list_classes_in_session(&mut session).expect("list_classes should succeed");
    let tree: serde_json::Value = serde_json::from_str(&json).expect("valid JSON");
    assert_eq!(
        tree.get("total_classes")
            .and_then(|v| v.as_u64())
            .unwrap_or_default(),
        3
    );
    let classes = tree
        .get("classes")
        .and_then(|v| v.as_array())
        .expect("classes array");
    assert!(
        classes
            .iter()
            .any(|node| { node.get("qualified_name").and_then(|v| v.as_str()) == Some("Lib") }),
        "expected top-level package Lib in class tree: {tree:?}"
    );
}

const DOC_MODEL_SOURCE: &str = r#"
    model DocModel "Short description"
      Real x "State";
    equation
      der(x) = -x;
      annotation(
        Documentation(
          info = "<html><p>Detailed docs</p></html>",
          revisions = "<html><ul><li>r1</li></ul></html>"
        )
      );
    end DocModel;
    "#;

#[cfg(target_arch = "wasm32")]
#[test]
fn test_get_class_info_extracts_documentation_annotation() {
    let mut session = Session::default();
    session.update_document("input.mo", DOC_MODEL_SOURCE);
    let json =
        get_class_info_in_session(&mut session, "DocModel").expect("get_class_info should succeed");
    let info: serde_json::Value = serde_json::from_str(&json).expect("valid JSON");
    assert_eq!(
        info.get("class_type").and_then(|v| v.as_str()),
        Some("model"),
        "unexpected class info payload: {info:?}"
    );
    assert!(
        info.get("documentation_html")
            .and_then(|v| v.as_str())
            .is_some_and(|s| s.contains("Detailed docs")),
        "expected Documentation(info=...) to be extracted: {info:?}"
    );
    assert!(
        info.get("documentation_revisions_html")
            .and_then(|v| v.as_str())
            .is_some_and(|s| s.contains("<li>r1</li>")),
        "expected Documentation(revisions=...) to be extracted: {info:?}"
    );
    assert!(
        info.get("source_modelica")
            .and_then(|v| v.as_str())
            .is_some_and(|s| s.contains("model DocModel")),
        "expected reconstructed Modelica source in class info: {info:?}"
    );
}

#[cfg(not(target_arch = "wasm32"))]
#[test]
fn test_extract_documentation_annotation_fields_native() {
    let parsed = parse_source_to_ast(DOC_MODEL_SOURCE, "input.mo").expect("parse should succeed");
    let class =
        find_class_by_qualified_name(&parsed, "DocModel").expect("DocModel should be present");
    let docs = extract_documentation_fields(&class.annotation);
    assert!(
        docs.info_html
            .as_deref()
            .is_some_and(|s| s.contains("Detailed docs")),
        "expected Documentation(info=...) to be extracted, got: {:?}",
        docs.info_html
    );
    assert!(
        docs.revisions_html
            .as_deref()
            .is_some_and(|s| s.contains("<li>r1</li>")),
        "expected Documentation(revisions=...) to be extracted, got: {:?}",
        docs.revisions_html
    );
    assert!(
        class.to_modelica("").contains("model DocModel"),
        "expected reconstructed Modelica source to contain model header"
    );
}

#[test]
fn test_compile_to_json_valid_model() {
    let mut session = Session::default();
    let source = r#"
    model Ball
      Real x(start=0);
      Real v(start=1);
    equation
      der(x) = v;
      der(v) = -9.81;
    end Ball;
    "#;

    let json =
        compile_source_in_session(&mut session, source, "Ball").expect("compile should succeed");
    let result: serde_json::Value =
        serde_json::from_str(&json).expect("compile should return valid JSON");
    let balance = result
        .get("balance")
        .expect("compile output should include balance section");
    assert!(
        balance
            .get("is_balanced")
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected Ball to be balanced, got: {balance:?}"
    );
    assert_eq!(
        balance
            .get("num_equations")
            .and_then(|v| v.as_u64())
            .unwrap_or_default(),
        2
    );
    assert_eq!(
        balance
            .get("num_unknowns")
            .and_then(|v| v.as_i64())
            .unwrap_or_default(),
        2
    );
}

#[test]
fn test_load_libraries_creates_usable_library_source_set() {
    let _guard = session_test_guard();
    clear_library_cache();

    let result_json =
        load_libraries(&mini_modelica_library_json()).expect("load_libraries should succeed");
    let result: serde_json::Value =
        serde_json::from_str(&result_json).expect("load_libraries should return JSON");

    assert_eq!(
        result
            .get("parsed_count")
            .and_then(|value| value.as_u64())
            .unwrap_or_default(),
        1
    );
    assert_eq!(
        result
            .get("inserted_count")
            .and_then(|value| value.as_u64())
            .unwrap_or_default(),
        1
    );
    assert_eq!(
        result
            .get("error_count")
            .and_then(|value| value.as_u64())
            .unwrap_or_default(),
        0
    );
    assert_eq!(get_library_count(), 1);

    let compiled = compile(USES_MODELICA_SOURCE, "UsesModelica")
        .expect("compile should succeed with preloaded library");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");
    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected library-backed model to compile successfully, got: {compiled_result:?}"
    );

    clear_library_cache();
}

#[test]
fn test_compile_with_libraries_uses_supplied_library_sources() {
    let _guard = session_test_guard();
    clear_library_cache();

    let compiled = compile_with_libraries(
        USES_MODELICA_SOURCE,
        "UsesModelica",
        &mini_modelica_library_json(),
    )
    .expect("compile_with_libraries should succeed");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");

    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected compile_with_libraries to honor supplied libraries, got: {compiled_result:?}"
    );
    assert!(
        get_library_count() >= 1,
        "expected compile_with_libraries to populate at least one cached library document"
    );

    clear_library_cache();
}

#[test]
fn test_compile_with_libraries_ignores_unrelated_session_parse_errors() {
    let _guard = session_test_guard();
    clear_library_cache();

    {
        let mut lock = SESSION.lock().expect("session lock");
        let session = lock.get_or_insert_with(Session::default);
        session.update_document("Broken.mo", "model Broken\n  Real x\nend Broken;\n");
    }

    let compiled = compile_with_libraries(
        USES_MODELICA_SOURCE,
        "UsesModelica",
        &mini_modelica_library_json(),
    )
    .expect("focused compile should ignore unrelated session parse errors");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");

    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected compile_with_libraries to ignore unrelated parse errors, got: {compiled_result:?}"
    );

    clear_library_cache();
}

#[test]
fn test_compile_to_json_prepared_retains_observables_from_native_orbit_model() {
    let mut session = Session::default();
    let source = r#"
    model SatelliteOrbit2D
      parameter Real mu = 398600.4418;
      parameter Real r0 = 7000;
      parameter Real v0 = sqrt(mu / r0);
      Real rx(start = r0, fixed = true);
      Real ry(start = 0, fixed = true);
      Real vx(start = 0, fixed = true);
      Real vy(start = v0, fixed = true);
      Real inv_r;
      Real inv_v2;
      Real inv_h;
      Real inv_energy;
      Real inv_a;
      Real inv_rv;
      Real inv_ex;
      Real inv_ey;
      Real inv_ecc;
    equation
      der(rx) = vx;
      der(ry) = vy;
      inv_r = sqrt(rx * rx + ry * ry);
      inv_v2 = vx * vx + vy * vy;
      inv_h = rx * vy - ry * vx;
      inv_energy = 0.5 * inv_v2 - mu / inv_r;
      inv_a = 1 / (2 / inv_r - inv_v2 / mu);
      inv_rv = rx * vx + ry * vy;
      inv_ex = ((inv_v2 - mu / inv_r) * rx - inv_rv * vx) / mu;
      inv_ey = ((inv_v2 - mu / inv_r) * ry - inv_rv * vy) / mu;
      inv_ecc = sqrt(inv_ex * inv_ex + inv_ey * inv_ey);
      der(vx) = -mu * rx / (inv_r ^ 3);
      der(vy) = -mu * ry / (inv_r ^ 3);
    end SatelliteOrbit2D;
    "#;

    let json = compile_source_in_session(&mut session, source, "SatelliteOrbit2D")
        .expect("compile should succeed for orbit model");
    let result: serde_json::Value =
        serde_json::from_str(&json).expect("compile should return valid JSON");

    let native_y = result
        .get("dae_native")
        .and_then(|d| d.get("y"))
        .and_then(|y| y.as_object())
        .expect("dae_native.y should exist for orbit model");
    assert!(
        native_y.contains_key("inv_r"),
        "native dae should include algebraic variable inv_r, got keys: {:?}",
        native_y.keys().collect::<Vec<_>>()
    );

    let prepared = result
        .get("dae_prepared")
        .and_then(|d| d.as_object())
        .expect("dae_prepared should exist");
    let observables = prepared
        .get("__rumoca_observables")
        .and_then(|v| v.as_array())
        .expect("dae_prepared.__rumoca_observables should exist");
    assert!(
        !observables.is_empty(),
        "expected prepared dae to retain at least one observable"
    );

    let observable_names: std::collections::HashSet<String> = observables
        .iter()
        .filter_map(|v| v.get("name").and_then(|n| n.as_str()).map(str::to_string))
        .collect();
    for expected in [
        "inv_r",
        "inv_v2",
        "inv_h",
        "inv_energy",
        "inv_a",
        "inv_rv",
        "inv_ex",
        "inv_ey",
        "inv_ecc",
    ] {
        assert!(
            observable_names.contains(expected),
            "missing expected retained observable `{expected}`; got: {:?}",
            observable_names
        );
    }
}

#[test]
fn test_render_template_preserves_prepared_observables_from_json_context() {
    let mut session = Session::default();
    let source = r#"
    model SatelliteOrbit2D
      parameter Real mu = 398600.4418;
      parameter Real r0 = 7000;
      parameter Real v0 = sqrt(mu / r0);
      Real rx(start = r0, fixed = true);
      Real ry(start = 0, fixed = true);
      Real vx(start = 0, fixed = true);
      Real vy(start = v0, fixed = true);
      Real inv_r;
      Real inv_v2;
      Real inv_h;
      Real inv_energy;
      Real inv_a;
      Real inv_rv;
      Real inv_ex;
      Real inv_ey;
      Real inv_ecc;
    equation
      der(rx) = vx;
      der(ry) = vy;
      inv_r = sqrt(rx * rx + ry * ry);
      inv_v2 = vx * vx + vy * vy;
      inv_h = rx * vy - ry * vx;
      inv_energy = 0.5 * inv_v2 - mu / inv_r;
      inv_a = 1 / (2 / inv_r - inv_v2 / mu);
      inv_rv = rx * vx + ry * vy;
      inv_ex = ((inv_v2 - mu / inv_r) * rx - inv_rv * vx) / mu;
      inv_ey = ((inv_v2 - mu / inv_r) * ry - inv_rv * vy) / mu;
      inv_ecc = sqrt(inv_ex * inv_ex + inv_ey * inv_ey);
      der(vx) = -mu * rx / (inv_r ^ 3);
      der(vy) = -mu * ry / (inv_r ^ 3);
    end SatelliteOrbit2D;
    "#;

    let compiled = compile_source_in_session(&mut session, source, "SatelliteOrbit2D")
        .expect("compile should succeed for orbit model");
    let parsed: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");
    let prepared = parsed
        .get("dae_prepared")
        .expect("compile response should contain dae_prepared");

    let rendered = render_template(
        &prepared.to_string(),
        "{% for o in dae.__rumoca_observables %}{{ o.name }}\n{% endfor %}",
    )
    .expect("render_template should succeed with JSON context");

    for expected in [
        "inv_r",
        "inv_v2",
        "inv_h",
        "inv_energy",
        "inv_a",
        "inv_rv",
        "inv_ex",
        "inv_ey",
        "inv_ecc",
    ] {
        assert!(
            rendered.lines().any(|line| line.trim() == expected),
            "expected rendered observables to contain `{expected}`, got:\n{rendered}"
        );
    }
}

#[test]
fn test_compile_to_json_recovers_after_syntax_diagnostics() {
    let mut session = Session::default();
    let invalid = r#"
    model Ball
      Real x(start=0);
      Real v(start=1)
    equation
      der(x) = v;
      der(v) = -9.81;
    end Ball;
    "#;
    let valid = r#"
    model Ball
      Real x(start=0);
      Real v(start=1);
    equation
      der(x) = v;
      der(v) = -9.81;
    end Ball;
    "#;

    let diags_json = lsp_diagnostics_in_session(&mut session, invalid)
        .expect("diagnostics should still return syntax errors");
    let diags: Vec<serde_json::Value> =
        serde_json::from_str(&diags_json).expect("diagnostics payload should be valid JSON");
    assert!(
        diags.iter().any(|d| {
            d.get("code")
                .and_then(|c| c.as_str())
                .is_some_and(|code| code.starts_with("EP"))
        }),
        "expected syntax diagnostics for invalid source, got: {diags:?}"
    );

    let json = compile_source_in_session(&mut session, valid, "Ball")
        .expect("compile should recover after diagnostics");
    let result: serde_json::Value =
        serde_json::from_str(&json).expect("compile should return valid JSON");
    assert!(
        result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected recovered compile to be balanced, got: {result:?}"
    );
}

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
