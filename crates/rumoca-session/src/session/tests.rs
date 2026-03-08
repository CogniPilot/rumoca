use super::*;

#[test]
fn test_session_add_document() {
    let mut session = Session::default();
    session
        .add_document("test.mo", "model M Real x; end M;")
        .unwrap();
    assert!(session.get_document("test.mo").is_some());
}

#[test]
fn test_session_compile() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M Real x(start=0); equation der(x) = 1; end M;",
        )
        .unwrap();

    let names = session.model_names().unwrap();
    assert_eq!(names, &["M"]);

    let result = session.compile_model("M").unwrap();
    assert!(result.is_balanced());
}

#[test]
fn test_compile_extracts_experiment_stop_time() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(StopTime=2.5));
                end M;
                "#,
        )
        .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_start_time, None);
    assert_eq!(result.experiment_stop_time, Some(2.5));
    assert_eq!(result.experiment_tolerance, None);
    assert_eq!(result.experiment_interval, None);
    assert_eq!(result.experiment_solver, None);
}

#[test]
fn test_compile_ignores_negative_experiment_stop_time() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(StopTime=-1));
                end M;
                "#,
        )
        .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_stop_time, None);
}

#[test]
fn test_compile_extracts_experiment_tolerance_interval_and_solver() {
    let mut session = Session::default();
    session
            .add_document(
                "test.mo",
                r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(StartTime=0.1, StopTime=2.5, Tolerance=1e-5, Interval=0.01, Algorithm="Dassl"));
                end M;
                "#,
            )
            .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_start_time, Some(0.1));
    assert_eq!(result.experiment_stop_time, Some(2.5));
    assert_eq!(result.experiment_tolerance, Some(1e-5));
    assert_eq!(result.experiment_interval, Some(0.01));
    assert_eq!(result.experiment_solver.as_deref(), Some("Dassl"));
}

#[test]
fn test_compile_extracts_solver_from_openmodelica_simulation_flags() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(start=0);
                equation
                  der(x) = 1;
                annotation(experiment(__OpenModelica_simulationFlags(s="rungekutta")));
                end M;
                "#,
        )
        .unwrap();

    let result = session.compile_model("M").unwrap();
    assert_eq!(result.experiment_solver.as_deref(), Some("rungekutta"));
}

#[test]
fn test_typecheck_error_code_preserves_et004() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  parameter Real a[:];
                  Real x[size(a, 1)];
                equation
                  x = a;
                end M;
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some("ET004"));
        }
        other => panic!("expected typecheck failure, got {:?}", other),
    }
}

#[test]
fn test_record_forwarding_rebinds_dependent_record_fields() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                package P
                  record R
                    parameter Real a = 2;
                    final parameter Real b = a;
                  end R;

                  model Inner
                    parameter R r;
                    parameter Real x = r.b;
                  end Inner;

                  model Mid
                    parameter R r;
                    Inner i(r = r);
                  end Mid;

                  model Top
                    parameter R r(a = 5);
                    Mid mid(r = r);
                  end Top;
                end P;
                "#,
        )
        .unwrap();

    let result = session.compile_model("P.Top").unwrap();
    let mid_rb = result
        .dae
        .parameters
        .get(&dae::VarName::new("mid.r.b"))
        .expect("mid.r.b must exist in DAE parameters");
    let mid_irb = result
        .dae
        .parameters
        .get(&dae::VarName::new("mid.i.r.b"))
        .expect("mid.i.r.b must exist in DAE parameters");

    let mid_rb_start = mid_rb.start.as_ref().expect("mid.r.b start expected");
    let mid_irb_start = mid_irb.start.as_ref().expect("mid.i.r.b start expected");

    match mid_rb_start {
        dae::Expression::Literal(dae::Literal::Integer(5)) => {}
        dae::Expression::Literal(dae::Literal::Real(v)) if (v - 5.0).abs() <= f64::EPSILON => {}
        other => panic!(
            "record forwarding must propagate dependent field b via overridden a, got {:?}",
            other
        ),
    }
    match mid_irb_start {
        dae::Expression::Literal(dae::Literal::Integer(5)) => {}
        dae::Expression::Literal(dae::Literal::Real(v)) if (v - 5.0).abs() <= f64::EPSILON => {}
        other => panic!(
            "nested forwarding must preserve dependent record field values, got {:?}",
            other
        ),
    }
}

#[test]
fn test_compile_model_surfaces_todae_unresolved_reference_code() {
    let mut session = Session::default();
    session
        .add_document(
            "model.mo",
            r#"
                function F
                  input Real x;
                  output Real y;
                algorithm
                  y := x + missingRef;
                end F;

                model M
                  Real x(start=0);
                equation
                  der(x) = F(x);
                end M;
                "#,
        )
        .unwrap();
    // Add a second document so resolve runs in multi-document mode; unresolved
    // refs are then checked by downstream phases.
    session
        .add_document(
            "helper.mo",
            r#"
                model Helper
                  Real y;
                equation
                  y = 1.0;
                end Helper;
                "#,
        )
        .unwrap();

    let err = session
        .compile_model("M")
        .expect_err("compile_model should fail on unresolved ToDae reference");
    let err_text = err.to_string();
    assert!(
        err_text.contains("rumoca::todae::ED008"),
        "expected ToDae ED008 in compile_model error, got: {err_text}"
    );

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::ToDae);
            assert_eq!(error_code.as_deref(), Some("rumoca::todae::ED008"));
        }
        other => panic!("expected ToDae failure, got {other:?}"),
    }
}

#[test]
fn test_typecheck_error_code_preserves_et001_for_unknown_builtin_modifier() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  Real x(startd = 1.0);
                equation
                  der(x) = -x;
                end M;
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some("ET001"));
        }
        other => panic!("expected typecheck failure, got {:?}", other),
    }
}

#[test]
fn test_unknown_builtin_modifier_is_not_ignored_with_multiple_classes() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
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
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Typecheck);
            assert_eq!(error_code.as_deref(), Some("ET001"));
        }
        other => panic!("expected typecheck failure, got {:?}", other),
    }

    let diagnostics = session.compile_model_diagnostics("M");
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("ET001")),
        "expected ET001 diagnostics, got: {:?}",
        diagnostics.diagnostics
    );
}

#[test]
fn test_compile_model_diagnostics_for_valid_function_has_no_phase_errors() {
    let mut session = Session::default();
    session
        .add_document(
            "func.mo",
            r#"
                function F
                  input Real x;
                  output Real y;
                algorithm
                  y := x;
                end F;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("F");
    assert!(
        diagnostics.diagnostics.is_empty(),
        "expected no diagnostics for valid function, got: {:?}",
        diagnostics.diagnostics
    );
}

#[test]
fn test_merge_duplicate_class_diagnostic_has_primary_label() {
    let mut session = Session::default();
    session
        .add_document(
            "A.mo",
            r#"
                model M
                  Real x;
                end M;
                "#,
        )
        .unwrap();
    session
        .add_document(
            "B.mo",
            r#"
                model M
                  Real y;
                end M;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("M");
    let duplicate = diagnostics
        .diagnostics
        .iter()
        .find(|d| d.message.contains("Duplicate class 'M'"))
        .unwrap_or_else(|| {
            panic!(
                "expected duplicate-class diagnostic, got: {:?}",
                diagnostics.diagnostics
            )
        });
    let primary = duplicate.labels.iter().find(|label| label.primary);
    assert!(
        primary.is_some(),
        "expected primary label on merge diagnostic: {:?}",
        duplicate
    );
    let span = primary.expect("checked above").span;
    assert!(
        span.start.0 > 0 && span.end.0 > span.start.0,
        "expected non-dummy merge diagnostic span, got {:?}",
        span
    );
}

#[test]
fn test_needs_inner_diagnostic_has_source_label() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                model M
                  outer Real shared;
                equation
                  shared = 1.0;
                end M;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("M");
    let needs_inner = diagnostics
        .diagnostics
        .iter()
        .find(|d| d.code.as_deref() == Some("EI008"))
        .unwrap_or_else(|| panic!("expected EI008 needs-inner diagnostic, got: {diagnostics:?}"));

    let primary = needs_inner.labels.iter().find(|label| label.primary);
    assert!(
        primary.is_some(),
        "expected primary label on needs-inner diagnostic: {:?}",
        needs_inner
    );
    let span = primary.expect("checked above").span;
    assert!(
        span.start.0 > 0 && span.end.0 > span.start.0,
        "expected non-dummy needs-inner span, got {:?}",
        span
    );
}

#[test]
fn test_synthesized_inner_warning_is_emitted() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                package P
                  model Env
                    parameter Real g = 9.81;
                  end Env;

                  model M
                    outer Env env;
                    Real y;
                  equation
                    y = env.g;
                  end M;
                end P;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("P.M");
    let synth = diagnostics
        .diagnostics
        .iter()
        .find(|d| d.code.as_deref() == Some("EI013"))
        .unwrap_or_else(|| {
            panic!("expected EI013 synthesized-inner warning, got: {diagnostics:?}")
        });

    assert!(
        synth
            .message
            .contains("synthesizing root-level inner declaration"),
        "expected synthesized-inner message, got: {}",
        synth.message
    );
}

#[test]
fn test_instantiate_error_code_preserves_ei012_for_partial_component_instantiation() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                package PartialMedium
                  replaceable partial model BaseProperties
                    Real p;
                  end BaseProperties;
                end PartialMedium;

                model M
                  replaceable package Medium = PartialMedium;
                  Medium.BaseProperties medium;
                equation
                  medium.p = 1;
                end M;
                "#,
        )
        .unwrap();

    let phase_result = session.compile_model_phases("M").unwrap();
    match phase_result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Instantiate);
            assert!(
                error_code
                    .as_deref()
                    .is_some_and(|code| code.ends_with("EI012"))
            );
        }
        other => panic!("expected instantiate failure, got {:?}", other),
    }
}

#[test]
fn test_strict_reachable_ignores_unreachable_failures_when_requested_succeeds() {
    let mut session = Session::default();
    let source = r#"
            package P
              model Good
                Real x;
              equation
                x = 1;
              end Good;

              model BadNeedsInner
                outer Real shared;
              equation
                shared = 1;
              end BadNeedsInner;

              model BadNeedsInner2
                outer Real shared2;
              equation
                shared2 = 2;
              end BadNeedsInner2;
            end P;
        "#;
    session.add_document("test.mo", source).unwrap();

    let report = session.compile_model_strict_reachable_with_recovery("P.Good");
    assert!(report.requested_succeeded());
    assert!(
        report.failures.is_empty(),
        "unreachable package siblings must not affect strict compile targets"
    );
}

#[test]
fn test_strict_reachable_keeps_collecting_when_requested_fails() {
    let mut session = Session::default();
    let source = r#"
            package P
              model Good
                Real x;
              equation
                x = 1;
              end Good;

              model BadNeedsInner
                outer Real shared;
              equation
                shared = 1;
              end BadNeedsInner;

              model BadNeedsInner2
                outer Real shared2;
              equation
                shared2 = 2;
              end BadNeedsInner2;
            end P;
        "#;
    session.add_document("test.mo", source).unwrap();

    let report = session.compile_model_strict_reachable_with_recovery("P.BadNeedsInner");
    assert!(!report.requested_succeeded());

    let failed_models: std::collections::HashSet<_> = report
        .failures
        .iter()
        .map(|f| f.model_name.as_str())
        .collect();
    assert!(failed_models.contains("P.BadNeedsInner"));
    assert!(!failed_models.contains("P.BadNeedsInner2"));
    assert!(report.summary.total() >= 1);
}

#[test]
fn test_strict_reachable_ignores_unrelated_parse_errors() {
    let mut session = Session::default();
    session
        .add_document(
            "root.mo",
            r#"
            model Root
              Real x;
            equation
              x = 1;
            end Root;
            "#,
        )
        .expect("root should parse");

    let parse_err = session.update_document(
        "bad.mo",
        r#"
        model Bad
          Real x
        equation
          x = 1;
        end Bad;
        "#,
    );
    assert!(parse_err.is_some(), "bad.mo should fail to parse");

    let report = session.compile_model_strict_reachable_with_recovery("Root");
    assert!(report.requested_succeeded());
    assert!(report.failures.is_empty());
}

#[test]
fn test_strict_reachable_reports_parse_errors_in_target_closure() {
    let mut session = Session::default();
    session
        .add_document(
            "root.mo",
            r#"
            model Root
              Real x;
            equation
              x = 1;
            end Root;
            "#,
        )
        .expect("root should parse");

    let parse_err = session.update_document(
        "root.mo",
        r#"
        model Root
          Real x
        equation
          x = 1;
        end Root;
        "#,
    );
    assert!(parse_err.is_some(), "root.mo should fail to parse");

    let report = session.compile_model_strict_reachable_with_recovery("Root");
    assert!(!report.requested_succeeded());
    assert!(
        report
            .failures
            .iter()
            .any(|failure| failure.model_name == "root.mo"),
        "target parse errors should be reported as strict compile failures"
    );
}

fn planned_reachability(
    session: &mut Session,
    requested_model: &str,
) -> (Vec<String>, Vec<String>) {
    session
        .build_resolved()
        .expect("session should resolve for planner test");
    let tree = &session
        .ensure_resolved()
        .expect("resolved tree should be available")
        .0;
    let dep_cache = super::dependency_fingerprint::DependencyFingerprintCache::from_tree(tree);
    let planner = super::reachability::ReachabilityPlanner::new(
        dep_cache.class_dependencies(),
        &session.model_names,
    );
    (
        planner.reachable_classes(requested_model),
        planner.compile_targets(requested_model),
    )
}

#[test]
fn test_reachability_planner_tracks_import_dependencies() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
            package P
              model Dep
                Real y;
              equation
                y = 1;
              end Dep;

              model Root
                import P.Dep;
                Real x;
              equation
                x = 2;
              end Root;

              model Unused
                Real z;
              equation
                z = 3;
              end Unused;
            end P;
            "#,
        )
        .expect("test document should parse");

    let (reachable_classes, compile_targets) = planned_reachability(&mut session, "P.Root");
    assert!(reachable_classes.iter().any(|name| name == "P.Dep"));
    assert!(compile_targets.iter().any(|name| name == "P.Dep"));
    assert!(!compile_targets.iter().any(|name| name == "P.Unused"));
}

#[test]
fn test_reachability_planner_tracks_extends_dependencies() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
            package P
              model Base
                Real x;
              equation
                x = 1;
              end Base;

              model Child
                extends Base;
              end Child;

              model Unused
                Real z;
              equation
                z = 3;
              end Unused;
            end P;
            "#,
        )
        .expect("test document should parse");

    let (reachable_classes, compile_targets) = planned_reachability(&mut session, "P.Child");
    assert!(reachable_classes.iter().any(|name| name == "P.Base"));
    assert!(compile_targets.iter().any(|name| name == "P.Base"));
    assert!(!compile_targets.iter().any(|name| name == "P.Unused"));
}

#[test]
fn test_reachability_planner_tracks_component_type_dependencies() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
            package P
              model Helper
                Real y;
              equation
                y = 1;
              end Helper;

              model Root
                Helper h;
              equation
                h.y = 2;
              end Root;

              model Unused
                Real z;
              equation
                z = 3;
              end Unused;
            end P;
            "#,
        )
        .expect("test document should parse");

    let (reachable_classes, compile_targets) = planned_reachability(&mut session, "P.Root");
    assert!(reachable_classes.iter().any(|name| name == "P.Helper"));
    assert!(compile_targets.iter().any(|name| name == "P.Helper"));
    assert!(!compile_targets.iter().any(|name| name == "P.Unused"));
}

#[test]
fn test_reachability_planner_tracks_function_call_dependencies() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
            package P
              function F
                input Real u;
                output Real y;
              algorithm
                y := u;
              end F;

              model Root
                Real x;
              equation
                x = F(time);
              end Root;
            end P;
            "#,
        )
        .expect("test document should parse");

    let (reachable_classes, compile_targets) = planned_reachability(&mut session, "P.Root");
    assert!(reachable_classes.iter().any(|name| name == "P.F"));
    assert!(!compile_targets.iter().any(|name| name == "P.F"));
    assert_eq!(compile_targets, vec!["P.Root".to_string()]);
}

#[test]
fn test_strict_reachable_failure_summary_surfaces_resolve_root_cause() {
    let mut session = Session::default();
    let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  PID pid();
end Ball;
"#;
    session.add_document("Ball.mo", source).unwrap();

    let report = session.compile_model_strict_reachable_with_recovery("Ball");
    assert!(!report.requested_succeeded());
    let summary = report.failure_summary(8);
    let first_line = summary.lines().next().unwrap_or_default();

    assert!(
        first_line.contains("unresolved import"),
        "expected first line to include unresolved import root cause, got: {summary}"
    );
}

#[test]
fn test_compile_phase_timing_stats_record_single_compile() {
    let before = compile_phase_timing_stats();
    let before_flatten = rumoca_phase_flatten::flatten_phase_timing_stats();
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M Real x(start=0); equation der(x) = 1; end M;",
        )
        .expect("test setup should parse");

    let _ = session
        .compile_model("M")
        .expect("test model should compile successfully");

    let after = compile_phase_timing_stats();
    assert!(after.instantiate.calls > before.instantiate.calls);
    assert!(after.typecheck.calls > before.typecheck.calls);
    assert!(after.flatten.calls > before.flatten.calls);
    assert!(after.todae.calls > before.todae.calls);

    let after_flatten = rumoca_phase_flatten::flatten_phase_timing_stats();
    assert!(after_flatten.connections.calls > before_flatten.connections.calls);
}

#[test]
fn test_compile_recovers_after_document_parse_error() {
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

    let parse_err = session.update_document("input.mo", invalid);
    assert!(
        parse_err.is_some(),
        "invalid source should produce parse error"
    );

    let parse_err_after_fix = session.update_document("input.mo", valid);
    assert!(
        parse_err_after_fix.is_none(),
        "fixed source should clear parse error"
    );

    let phase = session
        .compile_model_phases("Ball")
        .expect("compile_model_phases should run");
    assert!(
        matches!(phase, PhaseResult::Success(_)),
        "expected Ball to compile after fix, got: {:?}",
        phase
    );
}

#[test]
fn test_update_document_keeps_last_successful_parse_for_semantic_features() {
    let mut session = Session::default();
    let valid = r#"
        model Ball
          Real x(start=0);
          Real v(start=1);
        equation
          der(x) = v;
          der(v) = -9.81;
        end Ball;
        "#;
    let invalid = r#"
        model Ball
          Real x(start=0);
          Real v(start=1);
        equation
          der(x) = v;
          der(v) = -9.81;
          de
        end Ball;
        "#;

    let first_err = session.update_document("input.mo", valid);
    assert!(first_err.is_none(), "valid source should parse");

    // Build and cache resolved state from the valid source.
    let models = session.model_names().expect("model_names should resolve");
    assert!(models.iter().any(|name| name == "Ball"));
    assert!(session.has_resolved_cached(), "resolved cache should exist");

    let parse_err = session.update_document("input.mo", invalid);
    assert!(
        parse_err.is_some(),
        "invalid source should produce parse error"
    );
    assert!(
        session
            .get_document("input.mo")
            .and_then(|doc| doc.parsed.as_ref())
            .is_some(),
        "last successful parse should be retained for semantic features"
    );
    assert!(
        session.has_resolved_cached(),
        "resolved cache should be retained while source is temporarily invalid"
    );

    // Compile paths must still fail while parse errors are present.
    assert!(
        session.compile_model_phases("Ball").is_err(),
        "compile should fail while parse error exists"
    );
}

#[test]
fn test_replace_parsed_source_set_excludes_active_document() {
    let mut session = Session::default();

    let lib_src = "package Lib model M Real x; equation der(x)=1; end M; end Lib;";
    let parsed = rumoca_phase_parse::parse_to_ast(lib_src, "lib.mo").expect("parse library");
    let inserted = session.replace_parsed_source_set(
        "library::lib",
        vec![("lib.mo".to_string(), parsed)],
        Some("lib.mo"),
    );
    assert_eq!(inserted, 0, "active document path should be excluded");
    assert!(
        session.get_document("lib.mo").is_none(),
        "excluded document must not be inserted from source-set"
    );
}

#[test]
fn test_compile_model_phases_uses_cache_until_session_invalidated() {
    reset_compile_phase_timing_stats();
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            "model M Real x(start=0); equation der(x) = 1; end M;",
        )
        .expect("test setup should parse");

    let before = compile_phase_timing_stats();
    let first = session
        .compile_model_phases("M")
        .expect("first compile should run");
    assert!(matches!(first, PhaseResult::Success(_)));
    let after_first = compile_phase_timing_stats();
    assert!(
        after_first.instantiate.calls > before.instantiate.calls,
        "first compile should execute instantiate"
    );

    let second = session
        .compile_model_phases("M")
        .expect("second compile should use cache");
    assert!(matches!(second, PhaseResult::Success(_)));
    let after_second = compile_phase_timing_stats();
    assert_eq!(
        after_second.instantiate.calls, after_first.instantiate.calls,
        "second compile should be served from cache"
    );

    let parse_err = session.update_document(
        "test.mo",
        "model M Real x(start=0); equation der(x) = 2; end M;",
    );
    assert!(
        parse_err.is_none(),
        "valid update should not return parse error"
    );

    let third = session
        .compile_model_phases("M")
        .expect("third compile should run after invalidation");
    assert!(matches!(third, PhaseResult::Success(_)));
    let after_third = compile_phase_timing_stats();
    assert!(
        after_third.instantiate.calls > after_second.instantiate.calls,
        "cache should invalidate after document update"
    );
}

#[test]
fn test_compile_cache_survives_unrelated_document_update() {
    let mut session = Session::default();
    session
        .add_document(
            "a.mo",
            r#"
            model A
              Real x(start=0);
            equation
              der(x) = 1;
            end A;
            "#,
        )
        .expect("A should parse");
    session
        .add_document(
            "b.mo",
            r#"
            model B
              Real y(start=0);
            equation
              der(y) = 2;
            end B;
            "#,
        )
        .expect("B should parse");

    let _ = session
        .compile_model_phases("A")
        .expect("first compile should run");
    let cache_entry = session
        .compile_cache
        .get_mut("A")
        .expect("A should have compile cache entry after first compile");
    cache_entry.result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-A".to_string()],
    };

    let b_update_err = session.update_document(
        "b.mo",
        r#"
            model B
              Real y(start=0);
            equation
              der(y) = 3;
            end B;
            "#,
    );
    assert!(b_update_err.is_none(), "B update should remain valid");

    let second = session
        .compile_model_phases("A")
        .expect("A should still compile after unrelated edit");
    match second {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, vec!["cached-A".to_string()]);
        }
        other => panic!("expected cached result after unrelated edit, got {other:?}"),
    }
}

#[test]
fn test_compile_cache_invalidates_when_dependency_changes() {
    let mut session = Session::default();
    session
        .add_document(
            "base.mo",
            r#"
            model Base
              Real x(start=0);
            equation
              der(x) = 1;
            end Base;
            "#,
        )
        .expect("Base should parse");
    session
        .add_document(
            "child.mo",
            r#"
            model Child
              Base base;
              Real y(start=0);
            equation
              der(y) = base.x;
            end Child;
            "#,
        )
        .expect("Child should parse");

    let _ = session
        .compile_model_phases("Child")
        .expect("first Child compile should run");
    let cache_entry = session
        .compile_cache
        .get_mut("Child")
        .expect("Child should have compile cache entry after first compile");
    cache_entry.result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-Child".to_string()],
    };

    let base_update_err = session.update_document(
        "base.mo",
        r#"
            model Base
              Real x(start=0);
            equation
              der(x) = 5;
            end Base;
            "#,
    );
    assert!(base_update_err.is_none(), "Base update should remain valid");

    let second = session
        .compile_model_phases("Child")
        .expect("Child should recompile after Base changed");
    assert!(
        matches!(second, PhaseResult::Success(_)),
        "Child cache must invalidate when dependency Base changes"
    );
}

#[test]
fn test_compile_models_parallel_reuses_cache() {
    let mut session = Session::default();
    session
        .add_document(
            "models.mo",
            r#"
            model A
              Real x(start=0);
            equation
              der(x) = 1;
            end A;

            model B
              Real y(start=0);
            equation
              der(y) = 2;
            end B;
            "#,
        )
        .expect("models should parse");

    let _first = session
        .compile_models_parallel(&["A", "B"])
        .expect("first parallel compile should run");
    session
        .compile_cache
        .get_mut("A")
        .expect("A cache entry")
        .result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-A".to_string()],
    };
    session
        .compile_cache
        .get_mut("B")
        .expect("B cache entry")
        .result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-B".to_string()],
    };

    let second = session
        .compile_models_parallel(&["A", "B"])
        .expect("second parallel compile should hit cache");
    assert_eq!(second.len(), 2);
    match &second[0].1 {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, &vec!["cached-A".to_string()])
        }
        other => panic!("expected cached A result, got {other:?}"),
    }
    match &second[1].1 {
        PhaseResult::NeedsInner { missing_inners } => {
            assert_eq!(missing_inners, &vec!["cached-B".to_string()])
        }
        other => panic!("expected cached B result, got {other:?}"),
    }
}

#[test]
fn test_compile_model_strict_reachable_with_recovery_reuses_cache() {
    let mut session = Session::default();
    session
        .add_document(
            "pkg.mo",
            r#"
            package P
              model A
                Real x(start=0);
              equation
                der(x) = 1;
              end A;

              model B
                Real y(start=0);
              equation
                der(y) = 2;
              end B;
            end P;
            "#,
        )
        .expect("package should parse");

    let first = session.compile_model_strict_reachable_with_recovery("P.A");
    assert!(first.requested_succeeded(), "P.A should compile");
    session
        .compile_cache
        .get_mut("P.A")
        .expect("P.A cache entry")
        .result = PhaseResult::NeedsInner {
        missing_inners: vec!["cached-P.A".to_string()],
    };

    let second = session.compile_model_strict_reachable_with_recovery("P.A");
    assert!(
        !second.requested_succeeded(),
        "requested result should come from cache override"
    );
    match second.requested_result {
        Some(PhaseResult::NeedsInner { missing_inners }) => {
            assert_eq!(missing_inners, vec!["cached-P.A".to_string()]);
        }
        other => panic!("expected cached strict requested result, got {other:?}"),
    }
}
