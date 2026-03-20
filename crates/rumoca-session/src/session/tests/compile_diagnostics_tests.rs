use super::*;

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
fn test_strict_reachable_ignores_unrelated_library_resolve_errors() {
    let mut session = Session::default();
    session
        .add_document(
            "good_dep.mo",
            r#"
            within Lib;
            model GoodDep
              Real x(start=0);
            equation
              der(x) = 1;
            end GoodDep;
            "#,
        )
        .expect("good dependency should parse");
    session
        .add_document(
            "broken.mo",
            r#"
            within Lib;
            partial model PartialBase
            end PartialBase;

            model Broken
              PartialBase base;
            end Broken;
            "#,
        )
        .expect("broken sibling should parse");
    session
        .add_document(
            "lib.mo",
            r#"
            package Lib
            end Lib;
            "#,
        )
        .expect("library package should parse");
    session
        .add_document(
            "root.mo",
            r#"
            model Root
              Lib.GoodDep dep;
            end Root;
            "#,
        )
        .expect("root should parse");

    let report = session.compile_model_strict_reachable_with_recovery("Root");
    assert!(
        report.requested_succeeded(),
        "strict compile must ignore unrelated library resolve errors"
    );
    assert!(
        report.failures.is_empty(),
        "unrelated library resolve diagnostics must not leak into Root"
    );
}

#[test]
fn test_compiled_library_tolerant_strict_reachable_ignores_unrelated_library_errors() {
    let parsed = vec![
        (
            "good_dep.mo".to_string(),
            rumoca_phase_parse::parse_to_ast(
                r#"
                within Lib;
                model GoodDep
                  Real x(start=0);
                equation
                  der(x) = 1;
                end GoodDep;
                "#,
                "good_dep.mo",
            )
            .expect("good dependency should parse"),
        ),
        (
            "broken.mo".to_string(),
            rumoca_phase_parse::parse_to_ast(
                r#"
                within Lib;
                model Broken
                  MissingType x;
                end Broken;
                "#,
                "broken.mo",
            )
            .expect("broken sibling should still parse"),
        ),
        (
            "lib.mo".to_string(),
            rumoca_phase_parse::parse_to_ast(
                r#"
                package Lib
                end Lib;
                "#,
                "lib.mo",
            )
            .expect("library package should parse"),
        ),
        (
            "root.mo".to_string(),
            rumoca_phase_parse::parse_to_ast(
                r#"
                model Root
                  Lib.GoodDep dep;
                end Root;
                "#,
                "root.mo",
            )
            .expect("root should parse"),
        ),
    ];

    let library = CompiledLibrary::from_parsed_batch_tolerant(parsed)
        .expect("tolerant compiled library should index despite unrelated errors");
    assert!(
        library.model_names().iter().any(|name| name == "Root"),
        "Root must still be discoverable without a whole-library strict resolve"
    );

    let report = library.compile_model_strict_reachable_with_recovery("Root");
    assert!(
        report.requested_succeeded(),
        "strict closure compile must ignore unrelated library diagnostics"
    );
    assert!(
        report.failures.is_empty(),
        "unrelated library diagnostics must not leak into Root"
    );
}

#[test]
fn test_compiled_library_strict_reachable_uncached_does_not_fill_cache() {
    let definition = rumoca_phase_parse::parse_to_ast(
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
        "pkg.mo",
    )
    .expect("package should parse");

    let library = CompiledLibrary::from_stored_definition(definition)
        .expect("compiled library should build from one parsed package");

    let report = library.compile_model_strict_reachable_uncached_with_recovery("P.A");
    assert!(report.requested_succeeded(), "P.A should compile");
    assert!(
        library
            .compile_cache
            .lock()
            .expect("compiled library cache poisoned")
            .is_empty(),
        "uncached strict compile should not retain phase results in the shared library cache"
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

#[test]
fn test_strict_reachable_reports_parse_errors_in_required_dependency() {
    let mut session = Session::default();
    session
        .add_document(
            "root.mo",
            r#"
            model Root
              Helper h;
            end Root;
            "#,
        )
        .expect("root should parse");

    let parse_err = session.update_document(
        "Helper.mo",
        r#"
        model Helper
          Real x
        equation
          der(x) = 1;
        end Helper;
        "#,
    );
    assert!(parse_err.is_some(), "Helper.mo should fail to parse");

    let report = session.compile_model_strict_reachable_with_recovery("Root");
    assert!(!report.requested_succeeded());
    assert!(
        report
            .failures
            .iter()
            .any(|failure| failure.model_name == "Helper.mo"),
        "required dependency parse errors should be preserved in strict compile failures"
    );
    assert!(
        !report
            .failures
            .iter()
            .any(|failure| failure.error.contains("unresolved type reference")),
        "required dependency parse errors should not degrade into unresolved type errors: {:?}",
        report.failures
    );
}

#[test]
fn test_resolve_cache_isolated_between_strict_and_standard_modes() {
    let mut session = Session::default();
    session
        .add_document(
            "root.mo",
            r#"
            model Root
              PID pid(k=1, Ti=1.0, Td=0.1);
            end Root;
            "#,
        )
        .expect("root should parse");

    let report = session.compile_model_strict_reachable_with_recovery("Root");
    assert!(
        !report.requested_succeeded(),
        "strict compile must report failure for unresolved type"
    );

    let names = session.model_names();
    assert!(
        names.is_err(),
        "standard resolve must not reuse strict compile recovery cache"
    );
}

#[test]
fn completion_class_names_tolerate_unrelated_resolve_diagnostics() {
    let mut session = Session::default();
    session
        .add_document(
            "input.mo",
            r#"
            connector Bus
            end Bus;

            block BusTranscription
              Bus stackBus;
            end BusTranscription;

            package Lib
              model A
                Real x;
              end A;
            end Lib;
            "#,
        )
        .expect("input should parse");

    let strict_names = session
        .all_class_names_for_completion()
        .expect("completion name collection should tolerate unrelated diagnostics");
    assert!(
        strict_names.iter().any(|name| name == "Lib.A"),
        "completion name collection should still expose Lib.A: {strict_names:?}"
    );

    let standard_names = session.all_class_names();
    assert!(
        standard_names.is_err(),
        "standard resolve should still fail on the unrelated connector diagnostic"
    );
}

#[test]
fn class_name_collection_excludes_components_and_loop_indices() {
    let mut session = Session::default();
    session
        .add_document(
            "input.mo",
            r#"
            package Lib
              model M
                Real x;
              algorithm
                for i in 1:3 loop
                end for;
              end M;
            end Lib;
            "#,
        )
        .expect("input should parse");

    let names = session
        .all_class_names_for_completion()
        .expect("completion class collection should succeed");
    assert_eq!(names, vec!["Lib".to_string(), "Lib.M".to_string()]);
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
        &session.query_state.resolved.model_names,
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
    assert_eq!(compile_targets, vec!["P.Root".to_string()]);
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
    assert_eq!(compile_targets, vec!["P.Child".to_string()]);
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
    assert_eq!(compile_targets, vec!["P.Root".to_string()]);
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
