use super::*;

#[test]
fn nested_type_in_an_instantiating_model_does_not_capture_a_device_sibling_type() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                package Root
                  package Examples
                    model Test
                      record Shared
                        Real unrelated;
                      end Shared;
                      Root.Internal.Device device;
                    end Test;
                  end Examples;
                  package Internal
                    function consume
                      input Shared constants;
                      output Real value;
                    algorithm
                      value := constants.value;
                    end consume;
                    model Device
                      constant Shared constants;
                      Shared copied = constants;
                      Real observed = consume(constants);
                    end Device;
                    record Shared
                      Real value;
                    end Shared;
                  end Internal;
                end Root;
            "#,
        )
        .expect("source should parse");

    let diagnostics = session.compile_model_diagnostics("Root.Examples.Test");
    assert!(
        diagnostics
            .diagnostics
            .iter()
            .all(|diagnostic| !diagnostic.message.contains("type mismatch")),
        "device-local sibling types must not bind to an unrelated nested record: {diagnostics:?}"
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
fn test_typecheck_diagnostic_preserves_source_file_in_multi_document_session() {
    let mut session = Session::default();
    session
        .add_document(
            "library.mo",
            r#"
                package Library
                  model Helper
                    Real x;
                  equation
                    x = 1.0;
                  end Helper;
                end Library;
                "#,
        )
        .unwrap();
    session
        .add_document(
            "target.mo",
            r#"
                model Target
                  Real x(startd = 1.0);
                equation
                  der(x) = -x;
                end Target;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("Target");
    let type_error = diagnostics
        .diagnostics
        .iter()
        .find(|diag| diag.code.as_deref() == Some("ET001"))
        .unwrap_or_else(|| panic!("expected ET001 diagnostic, got: {diagnostics:?}"));
    let primary = type_error
        .labels
        .iter()
        .find(|label| label.primary)
        .unwrap_or_else(|| panic!("expected primary label, got: {type_error:?}"));
    let source_map = diagnostics
        .source_map
        .as_ref()
        .expect("model diagnostics should include source map");
    let (file_name, _) = source_map
        .get_source(primary.span.source)
        .unwrap_or_else(|| panic!("missing source for span: {:?}", primary.span));

    assert_eq!(file_name, "target.mo");
    let diagnostic_snapshot = serde_json::json!({
        "code": type_error.code.as_deref(),
        "message": type_error.message.as_str(),
        "primary_file": file_name,
        "primary_label": primary.message.as_deref(),
    });
    assert_eq!(
        diagnostic_snapshot,
        serde_json::json!({
            "code": "ET001",
            "message": "unknown modifier `startd` for builtin component `x` of type `Real`",
            "primary_file": "target.mo",
            "primary_label": "unknown modifier"
        })
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
fn test_configured_instantiation_depth_limit_reports_ei030_with_span() {
    let mut session = Session::new(SessionConfig {
        instantiation_depth_limit: 1,
        ..SessionConfig::default()
    });
    session
        .add_document(
            "depth.mo",
            r#"
                model A
                  B b;
                end A;

                model B
                  C c;
                end B;

                model C
                  Real x;
                equation
                  x = 1;
                end C;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("A");
    let depth = diagnostics
        .diagnostics
        .iter()
        .find(|d| {
            d.code
                .as_deref()
                .is_some_and(|code| code.ends_with("EI030"))
        })
        .unwrap_or_else(|| panic!("expected EI030 depth diagnostic, got: {diagnostics:?}"));

    assert!(
        depth.message.contains("depth 2") && depth.message.contains("limit 1"),
        "expected configured depth in diagnostic message, got: {}",
        depth.message
    );
    let primary = depth.labels.iter().find(|label| label.primary);
    assert!(
        primary.is_some(),
        "expected primary label on depth diagnostic: {depth:?}"
    );
    let span = primary.expect("checked above").span;
    assert!(
        span.start.0 > 0 && span.end.0 > span.start.0,
        "expected non-dummy depth diagnostic span, got {span:?}"
    );
}

#[test]
fn test_recursive_class_instantiation_reports_ei031_with_span() {
    let mut session = Session::default();
    session
        .add_document(
            "cycle.mo",
            r#"
                model SelfCycle
                  SelfCycle child;
                end SelfCycle;
                "#,
        )
        .unwrap();

    let diagnostics = session.compile_model_diagnostics("SelfCycle");
    let cycle = diagnostics
        .diagnostics
        .iter()
        .find(|d| {
            d.code
                .as_deref()
                .is_some_and(|code| code.ends_with("EI031"))
        })
        .unwrap_or_else(|| panic!("expected EI031 cycle diagnostic, got: {diagnostics:?}"));

    assert!(
        cycle.message.contains("SelfCycle"),
        "expected cycle message to name the recursive class, got: {}",
        cycle.message
    );
    let primary = cycle.labels.iter().find(|label| label.primary);
    assert!(
        primary.is_some(),
        "expected primary label on cycle diagnostic: {cycle:?}"
    );
    let span = primary.expect("checked above").span;
    assert!(
        span.start.0 > 0 && span.end.0 > span.start.0,
        "expected non-dummy cycle diagnostic span, got {span:?}"
    );
}

#[test]
fn test_array_expansion_is_not_disabled_near_depth_limit() {
    let mut session = Session::new(SessionConfig {
        instantiation_depth_limit: 2,
        ..SessionConfig::default()
    });
    session
        .add_document(
            "array_depth.mo",
            r#"
                model Leaf
                  Real x;
                equation
                  x = 1;
                end Leaf;

                model Holder
                  Leaf leaf[2];
                end Holder;

                model Root
                  Holder holder;
                end Root;
                "#,
        )
        .unwrap();

    let compiled = session
        .compile_model("Root")
        .unwrap_or_else(|error| panic!("array expansion near depth limit should compile: {error}"));
    let variables = compiled.dae.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.role() == rumoca_ir_dae::VariableRole::Algebraic)
            .map(|(_, variable)| variable.name().to_string())
            .collect::<Vec<_>>()
    });

    assert!(
        variables.iter().any(|name| name == "holder.leaf[1].x")
            && variables.iter().any(|name| name == "holder.leaf[2].x"),
        "expected structured array elements to expand near depth limit, got {variables:?}"
    );
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
fn test_dae_strict_compile_projects_flat_derived_metadata() {
    let mut session = Session::default();
    session
        .add_document(
            "M.mo",
            r#"
                model M
                  parameter Real p;
                  discrete Real z;
                  Real x(start = 0);
                equation
                  der(x) = p;
                  z = time;
                end M;
                "#,
        )
        .unwrap();

    let result = session
        .compile_model_dae_strict_reachable_uncached_with_recovery("M")
        .expect("DAE strict compile should succeed");

    assert!(result.has_unbound_fixed_parameters);
    assert_eq!(result.active_discrete_scalar_count, 1);
    assert_eq!(result.balance_detail.state_unknowns, 1);
}

#[test]
fn test_dae_expression_spans_use_target_source_file_in_multi_document_session() {
    let mut session = Session::default();
    session
        .add_document(
            "library_first.mo",
            r#"
                model LibraryFirst
                end LibraryFirst;
                "#,
        )
        .unwrap();
    session
        .add_document(
            "target_second.mo",
            r#"
                model TargetSecond
                  Real x;
                  Real y;
                equation
                  x = y + 1;
                  y = 2;
                end TargetSecond;
                "#,
        )
        .unwrap();

    let result = session
        .compile_model_dae_strict_reachable_uncached_with_recovery("TargetSecond")
        .expect("DAE strict compile should succeed");
    let source_map = result
        .source_map
        .as_ref()
        .expect("DAE result should carry source map");
    let target_source_id = source_map
        .get_id("target_second.mo")
        .expect("target document should be in source map");

    let rhs_span = result.dae.inspect(|view| {
        (0..view.continuous_equation_count())
            .find_map(|index| view.continuous_equation(index))
            .map(|equation| equation.provenance().span())
            .expect("expected a source span on a checked DAE equation")
    });

    assert_eq!(rhs_span.source, target_source_id);
    let (file_name, source) = source_map
        .get_source(rhs_span.source)
        .expect("RHS span source id should resolve");
    assert_eq!(file_name, "target_second.mo");
    assert!(
        rhs_span.end.0 <= source.len(),
        "RHS span must be in bounds for target source: {rhs_span:?}"
    );
}

#[test]
fn test_strict_reachable_ignores_unrelated_source_root_resolve_errors() {
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
        .expect("source-root package should parse");
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
        "strict compile must resolve the selected source closure independently"
    );
    assert!(
        report.failures.is_empty(),
        "unrelated source-root resolve diagnostics must not leak into Root"
    );
}

#[test]
fn strict_compilation_carries_exact_target_resolve_proof() {
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
            model Broken
              MissingType value;
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
        .expect("source-root package should parse");
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

    let compilation = session
        .compile_model_strict("Root")
        .expect("the selected target closure should compile");

    assert!(
        !session.has_resolved_cached(),
        "the unrelated invalid sibling must keep global planning incomplete"
    );
    let resolved = compilation.resolved().inner();
    assert!(
        resolved.get_class_by_qualified_name("Root").is_some(),
        "the proof must contain the requested model"
    );
    assert!(
        resolved
            .get_class_by_qualified_name("Lib.GoodDep")
            .is_some(),
        "the proof must contain the requested model's dependency"
    );
    assert!(
        resolved.get_class_by_qualified_name("Lib.Broken").is_none(),
        "the proof must be the retained target closure, not the incomplete global plan"
    );
}

#[test]
fn test_strict_reachable_ignores_broken_sibling_in_the_same_document() {
    let source = r#"
        package Types
          type Signal = Real;
        end Types;

        package Lib
          import Signal = Types.Signal;

          model Good
            Signal x(start=0);
          equation
            der(x) = 1;
          end Good;

          model Broken
            MissingType value;
          end Broken;
        end Lib;

        model Root
          Lib.Good good;
        end Root;
        "#;

    let mut session = Session::default();
    session
        .add_document("models.mo", source)
        .expect("the shared document should parse");

    let target = session
        .resolve_strict_target("Root")
        .unwrap_or_else(|failure| {
            panic!(
                "the exact Root definition closure should resolve: {:?}",
                failure.failures
            )
        });
    assert!(
        target
            .resolved
            .inner()
            .get_class_by_qualified_name("Lib.Broken")
            .is_none(),
        "the completed Resolve proof must not retain the unresolved sibling"
    );

    let report = session.compile_model_strict_reachable_with_recovery("Root");
    assert!(
        report.requested_succeeded(),
        "the exact Root definition closure must exclude unresolved Lib.Broken: {:?}",
        report.failures
    );
    assert!(
        report.failures.is_empty(),
        "whole-document resolve diagnostics must not leak into Root"
    );
}

#[test]
fn test_compiled_source_root_planning_strict_reachable_ignores_unrelated_source_root_errors() {
    let sources = [
        (
            "good_dep.mo",
            r#"
                within Lib;
                model GoodDep
                  Real x(start=0);
                equation
                  der(x) = 1;
                end GoodDep;
                "#,
        ),
        (
            "broken.mo",
            r#"
                within Lib;
                model Broken
                  MissingType x;
                end Broken;
                "#,
        ),
        (
            "lib.mo",
            r#"
                package Lib
                end Lib;
                "#,
        ),
        (
            "root.mo",
            r#"
                model Root
                  Lib.GoodDep dep;
                end Root;
                "#,
        ),
    ];
    let mut source_map = rumoca_core::SourceMap::new();
    let parsed = sources
        .into_iter()
        .map(|(name, source)| {
            source_map.add(name, source);
            (
                name.to_owned(),
                rumoca_phase_parse::parse_to_ast(source, name)
                    .unwrap_or_else(|error| panic!("{name} should parse: {error}")),
            )
        })
        .collect();

    let source_root =
        CompiledSourceRoot::from_parsed_batch_with_resolution_planning(parsed, source_map)
            .expect("planning-only source root should retain unresolved diagnostics");
    assert!(
        source_root.model_names().iter().any(|name| name == "Root"),
        "Root must remain discoverable through the planning view"
    );
    let report = source_root
        .compile_model_strict_reachable_with_recovery("Root")
        .expect("strict target compilation should run");
    assert!(
        report.requested_succeeded(),
        "the independently resolved Root closure must compile"
    );
}

#[test]
fn test_compiled_source_root_strict_reachable_uncached_does_not_fill_cache() {
    let source = r#"
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
        "#;
    let definition =
        rumoca_phase_parse::parse_to_ast(source, "pkg.mo").expect("package should parse");
    let mut source_map = rumoca_core::SourceMap::new();
    source_map.add("pkg.mo", source);
    let source_root = CompiledSourceRoot::from_stored_definition(definition, source_map)
        .expect("compiled source root should build from one parsed package");

    let report = source_root.compile_model_strict_reachable_uncached_with_recovery("P.A");
    assert!(report.requested_succeeded(), "P.A should compile");
    assert!(
        source_root
            .compile_cache
            .lock()
            .expect("compiled source-root cache poisoned")
            .is_empty(),
        "uncached strict compile should not retain phase results in the shared source-root cache"
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
    let requested_span = match &report.requested_result {
        Some(PhaseResult::NeedsInner {
            missing_inners,
            missing_spans,
        }) => {
            assert_eq!(missing_inners, &vec!["shared".to_string()]);
            missing_spans
                .first()
                .copied()
                .expect("NeedsInner result should retain the outer declaration span")
        }
        other => panic!("expected NeedsInner requested result, got {other:?}"),
    };
    let source_map = report
        .source_map
        .as_ref()
        .expect("strict report should carry the source map");
    let (_, source) = source_map
        .get_source(requested_span.source)
        .expect("NeedsInner span should resolve through the report source map");
    let requested_snippet = &source[requested_span.start.0..requested_span.end.0];
    assert!(
        requested_snippet.contains("shared"),
        "NeedsInner span should point at the outer declaration, got {requested_snippet:?}"
    );

    let failure = report
        .failures
        .iter()
        .find(|failure| failure.model_name == "P.BadNeedsInner")
        .expect("requested failure should be reported");
    let primary = failure
        .primary_label
        .as_ref()
        .expect("NeedsInner failure should have a primary outer declaration label");
    assert_eq!(primary.span, requested_span);
    assert_eq!(primary.message.as_deref(), Some("missing matching `inner`"));
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

            model Broken
              MissingType unresolved;
            end Broken;
            "#,
        )
        .expect("input should parse");

    let strict_names = session
        .all_class_names_for_completion()
        .expect("completion may consume the planning-only tree");
    assert!(
        strict_names.iter().any(|name| name == "Lib.A"),
        "completion must retain declared classes: {strict_names:?}"
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
    let tree = session
        .ensure_resolved()
        .expect("resolved tree should be available")
        .inner();
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
    let failure = report
        .failures
        .iter()
        .find(|failure| failure.error.contains("unresolved import"))
        .expect("reachable unresolved import must retain its Resolve failure");
    let span = failure
        .primary_label
        .as_ref()
        .expect("reachable Resolve failure must retain exact provenance")
        .span;
    assert_eq!(
        &source[span.start.0..span.end.0],
        "import Modelica.Blocks.Continuous.PID"
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
fn test_regression_compile_enum_2d_index_lookup() {
    let mut session = Session::default();
    let source = r#"
package P
  type L = enumeration(U, X, Z, ZERO, ONE);

  model M
    parameter Integer map[L, L] = [1,1,1,1,1;
                                   1,2,2,2,2;
                                   1,2,3,3,3;
                                   1,2,3,4,4;
                                   1,2,3,4,5];
    L a(start=L.U);
    L b(start=L.U);
    Integer f(start=1);
  algorithm
    if change(a) or change(b) then
      f := map[a, b];
    end if;
  end M;
end P;
"#;
    session.update_document("input.mo", source);
    let result = session.compile_model_phases("P.M");
    assert!(
        matches!(result, Ok(PhaseResult::Success(_))),
        "expected compile success for enum 2D lookup model, got {result:?}"
    );
}

/// An under-determined model must surface as a structured ToDae/ED001 failure
/// carrying the balance breakdown, not just a rendered summary string.
#[test]
fn strict_dae_recovery_detailed_reports_todae_ed001_with_balance_detail() {
    let mut session = Session::default();
    session
        .add_document(
            "Unbalanced.mo",
            r#"
                model Unbalanced
                  Real x;
                  Real y;
                equation
                  x = 1;
                end Unbalanced;
                "#,
        )
        .unwrap();

    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed("Unbalanced")
        .expect_err("an under-determined model must not compile strictly");

    assert_eq!(failure.phase, Some(FailedPhase::ToDae));
    assert_eq!(failure.phase_name(), "ToDae");
    assert_eq!(
        failure.error_code.as_deref(),
        Some("ED001"),
        "expected the bare SPEC_0008 code, got {:?}",
        failure.error_code
    );
    let detail = failure
        .balance_detail
        .as_ref()
        .expect("ED001 must carry the balance breakdown");
    assert!(
        detail.balance() != 0,
        "carried detail must reproduce the unbalanced verdict: {detail:?}"
    );
    assert_eq!(detail.equations_unknowns().1, detail.unknowns());
    assert!(failure.summary.contains("unbalanced model"));
}

/// The direct regression for the ThreeTanks mislabel: a resolve-stage failure
/// must report `phase: None` (rendered as `Resolve`), never `ToDae`.
#[test]
fn strict_dae_recovery_detailed_reports_none_phase_for_resolve_failure() {
    let mut session = Session::default();
    session
        .add_document(
            "Unresolved.mo",
            r#"
                model Unresolved
                  Real x;
                equation
                  x = nParallel;
                end Unresolved;
                "#,
        )
        .unwrap();

    let failure = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed("Unresolved")
        .expect_err("an unresolved reference must not compile strictly");

    assert_eq!(
        failure.phase, None,
        "resolve failures never reach a model phase, got {:?}",
        failure.phase
    );
    assert_eq!(failure.phase_name(), "Resolve");
    assert!(
        failure.balance_detail.is_none(),
        "a resolve failure carries no balance breakdown"
    );
    assert!(
        !failure.summary.contains("failed in ToDae"),
        "summary must not claim ToDae: {}",
        failure.summary
    );
}

/// The string API must keep returning exactly the detailed summary so
/// its six existing callers do not change behavior.
#[test]
fn strict_dae_recovery_string_api_matches_detailed_summary() {
    let mut session = Session::default();
    session
        .add_document(
            "Unbalanced2.mo",
            r#"
                model Unbalanced2
                  Real x;
                  Real y;
                equation
                  x = 1;
                end Unbalanced2;
                "#,
        )
        .unwrap();

    let detailed_summary = session
        .compile_model_dae_strict_reachable_uncached_with_recovery_detailed("Unbalanced2")
        .expect_err("model is unbalanced")
        .summary
        .clone();
    let string_summary = session
        .compile_model_dae_strict_reachable_uncached_with_recovery("Unbalanced2")
        .expect_err("model is unbalanced");
    assert_eq!(detailed_summary, string_summary);
}
