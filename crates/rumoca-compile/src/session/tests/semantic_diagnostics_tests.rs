use super::*;

fn expect_warm_child_diagnostics(session: &mut Session) {
    set_child_diagnostics_cache_sentinel(session);
    let diagnostics = session.compile_model_diagnostics("Child");
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ETEST")),
        "unrelated edits should keep the Child semantic diagnostics cache warm"
    );
}

fn expect_cold_child_diagnostics(session: &mut Session) {
    let diagnostics = session.compile_model_diagnostics("Child");
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ETEST")),
        "rebuilt Child diagnostics should stay clean"
    );
}

#[test]
fn save_semantic_diagnostics_use_dedicated_cache_namespace() {
    let source = r#"model M
  Real x(start=0);
equation
  der(x) = 1;
end M;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.semantic_diagnostics_query("M", SemanticDiagnosticsMode::Save);
    assert!(
        first.diagnostics.is_empty(),
        "save diagnostics should build cleanly before cache mutation"
    );
    assert!(
        !session.has_resolved_cached(),
        "save diagnostics should not populate the legacy resolved owner"
    );
    assert!(
        !session.has_standard_resolved_cached(),
        "save diagnostics should stay off the standard resolved cache"
    );

    model_stage_semantic_diagnostics_artifact_mut(&mut session, "M", SemanticDiagnosticsMode::Save)
        .diagnostics
        .diagnostics
        .push(CommonDiagnostic::warning(
            "ESAVE",
            "save diagnostics cache sentinel",
            PrimaryLabel::new(Span::DUMMY).with_message("save cache sentinel"),
        ));

    let warm_save = session.semantic_diagnostics_query("M", SemanticDiagnosticsMode::Save);
    assert!(
        warm_save
            .diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("ESAVE")),
        "save diagnostics should reuse the save-mode cache artifact"
    );

    let standard = session.compile_model_diagnostics("M");
    assert!(
        standard
            .diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("ESAVE")),
        "standard diagnostics should not reuse save-mode cache artifacts"
    );
}

#[test]
fn body_semantic_diagnostics_use_distinct_cache_from_model_stage() {
    let source = r#"function F
  input Real x;
  output Real y;
algorithm
  y := x;
end F;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.compile_model_diagnostics("F");
    assert!(
        first.diagnostics.is_empty(),
        "function diagnostics should be clean before cache mutation"
    );
    body_semantic_diagnostics_artifact_mut(&mut session, "F", SemanticDiagnosticsMode::Standard)
        .diagnostics
        .diagnostics
        .push(CommonDiagnostic::warning(
            "EBODY",
            "body diagnostics cache sentinel",
            PrimaryLabel::new(Span::DUMMY).with_message("body cache sentinel"),
        ));

    let warm = session.compile_model_diagnostics("F");
    assert!(
        warm.diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("EBODY")),
        "non-simulatable classes should reuse body-stage diagnostics"
    );
    assert!(
        session
            .query_state
            .flat
            .semantic_diagnostics
            .model_stage_artifacts
            .is_empty(),
        "body-stage diagnostics should not populate the model-stage cache"
    );
}

#[test]
fn simulatable_semantic_diagnostics_merge_body_and_model_stage_layers() {
    let source = r#"model M
  Real x(start=0);
equation
  der(x) = 1;
end M;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.compile_model_diagnostics("M");
    assert!(
        first.diagnostics.is_empty(),
        "model diagnostics should be clean before cache mutation"
    );
    body_semantic_diagnostics_artifact_mut(&mut session, "M", SemanticDiagnosticsMode::Standard)
        .diagnostics
        .diagnostics
        .push(CommonDiagnostic::warning(
            "EBODY",
            "body diagnostics cache sentinel",
            PrimaryLabel::new(Span::DUMMY).with_message("body cache sentinel"),
        ));
    model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "M",
        SemanticDiagnosticsMode::Standard,
    )
    .diagnostics
    .diagnostics
    .push(CommonDiagnostic::warning(
        "EMODEL",
        "model-stage diagnostics cache sentinel",
        PrimaryLabel::new(Span::DUMMY).with_message("model cache sentinel"),
    ));

    let warm = session.compile_model_diagnostics("M");
    assert!(
        warm.diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("EBODY")),
        "simulatable diagnostics should retain body-stage warnings"
    );
    assert!(
        warm.diagnostics
            .iter()
            .any(|diag| diag.code.as_deref() == Some("EMODEL")),
        "simulatable diagnostics should still include model-stage diagnostics"
    );
}

#[test]
fn interface_semantic_diagnostics_control_model_stage_execution() {
    let source = r#"model M
  Real x(start=0);
equation
  der(x) = 1;
end M;
"#;

    let mut session = Session::default();
    session
        .add_document("test.mo", source)
        .expect("document should parse");

    let first = session.compile_model_diagnostics("M");
    assert!(
        first.diagnostics.is_empty(),
        "model diagnostics should be clean before cache mutation"
    );
    interface_semantic_diagnostics_artifact_mut(
        &mut session,
        "M",
        SemanticDiagnosticsMode::Standard,
    )
    .class_type = Some(ast::ClassType::Function);
    model_stage_semantic_diagnostics_artifact_mut(
        &mut session,
        "M",
        SemanticDiagnosticsMode::Standard,
    )
    .diagnostics
    .diagnostics
    .push(CommonDiagnostic::warning(
        "EMODEL",
        "model-stage diagnostics cache sentinel",
        PrimaryLabel::new(Span::DUMMY).with_message("model cache sentinel"),
    ));

    let warm = session.compile_model_diagnostics("M");
    assert!(
        warm.diagnostics
            .iter()
            .all(|diag| diag.code.as_deref() != Some("EMODEL")),
        "interface-stage class type should gate model-stage diagnostics on warm reuse"
    );
}

#[test]
fn cache_soak_mixed_edits_keep_unrelated_hits_and_rebuild_on_dependency_changes() {
    let base_v1 = r#"model Base
  Real y(start=0);
equation
  der(y) = 1;
end Base;
"#;
    let base_v2 = r#"model Base
  Real y(start=0);
equation
  der(y) = 3;
end Base;
"#;
    let child = r#"model Child
  Base base;
  Real x(start=0);
equation
  der(x) = base.y;
end Child;
"#;

    let mut session = Session::default();
    session
        .add_document("base.mo", base_v1)
        .expect("Base should parse");
    session
        .add_document("child.mo", child)
        .expect("Child should parse");
    session
        .add_document(
            "noise.mo",
            "model Noise\n  Real z;\nequation\n  z = 0;\nend Noise;\n",
        )
        .expect("Noise should parse");

    assert!(
        matches!(
            session
                .compile_model_phases("Child")
                .expect("Child should compile"),
            PhaseResult::Success(_)
        ),
        "initial Child compile should succeed"
    );
    expect_cold_child_navigation(&mut session);
    expect_cold_child_diagnostics(&mut session);

    for round in 0..8 {
        let marker = format!("cached-child-{round}");
        set_child_compile_cache_marker(&mut session, marker.clone());
        let noise = format!(
            "model Noise\n  Real z;\nequation\n  z = {};\nend Noise;\n",
            round + 1
        );

        let parse_err = session.update_document("noise.mo", &noise);
        assert!(parse_err.is_none(), "Noise edit should remain valid");
        expect_cached_child_compile(&mut session, &marker);
        expect_warm_child_navigation(&mut session);
        expect_warm_child_diagnostics(&mut session);

        let next_base = if round % 2 == 0 { base_v2 } else { base_v1 };
        let parse_err = session.update_document("base.mo", next_base);
        assert!(parse_err.is_none(), "Base edit should remain valid");
        assert!(
            matches!(
                session
                    .compile_model_phases("Child")
                    .expect("dependency edit should invalidate Child compile cache"),
                PhaseResult::Success(_)
            ),
            "dependency edits must rebuild Child compile results"
        );
        expect_cold_child_navigation(&mut session);
        expect_cold_child_diagnostics(&mut session);
    }
}
