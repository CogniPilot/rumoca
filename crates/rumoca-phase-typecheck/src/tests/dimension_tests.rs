//! Array-dimension evaluation on the class tree: explicit and colon extents,
//! redeclared-package dimensions, and the import-visible constants and
//! functions extents resolve through.

use super::*;

#[test]
fn test_dimension_evaluation() {
    // Test that shape_expr is evaluated to shape during typecheck
    let source = r#"
        model Test
            parameter Integer n = 3;
            Real x[n];
            Real y[2, 3];
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed;
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    // Check y has evaluated dimensions [2, 3]
    let y = test_class.components.get("y").expect("y should exist");
    assert_eq!(y.shape, vec![2, 3], "y should have shape [2, 3]");

    // Note: x[n] requires parameter evaluation which depends on context
    // The dimension may or may not be evaluated depending on whether n is known
}

#[test]
fn test_colon_dimension_inference() {
    // Test that colon dimensions are inferred from binding
    let source = r#"
        model Test
            Real x[:] = {1, 2, 3};
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed;
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");

    // Check x has inferred dimension [3]
    let x = test_class.components.get("x").expect("x should exist");
    assert_eq!(x.shape, vec![3], "x should have inferred shape [3]");
}

#[test]
fn test_redeclared_phase_system_dimension_resolves() {
    // Regression for PowerSystems-style connector dimensions:
    // PhaseSystem.n must resolve through the full type scope when a connector
    // extends another connector and redeclares the replaceable package.
    let source = r#"
        package PhaseSystems
          partial package PartialPhaseSystem
            constant Integer n;
            constant Integer m;
            type Voltage = Real;
            type Current = Real;
          end PartialPhaseSystem;

          package TwoConductor
            extends PartialPhaseSystem(n=2, m=0);
          end TwoConductor;
        end PhaseSystems;

        package Interfaces
          connector TerminalDC
            replaceable package PhaseSystem = PhaseSystems.PartialPhaseSystem;
            PhaseSystem.Voltage v[PhaseSystem.n];
            flow PhaseSystem.Current i[PhaseSystem.n];
          end TerminalDC;
        end Interfaces;

        package Ports
          connector TwoPin
            extends Interfaces.TerminalDC(
              redeclare package PhaseSystem = PhaseSystems.TwoConductor
            );
          end TwoPin;
        end Ports;

        model Test
          Ports.TwoPin term;
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Test")
        {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    typecheck_instanced_test_projection(&instanced.tree, &mut instanced.overlay, "Test")
        .expect("instanced typecheck should succeed");
    let term = instanced
        .overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == "term")
        .expect("term occurrence");
    assert!(
        term.dims.is_empty(),
        "term connector itself should remain scalar"
    );
}

#[test]
fn test_redeclared_phase_system_dimension_resolves_in_nested_component() {
    // Mirrors `voltage.term.v[PhaseSystem.n]` shape in PowerSystems examples.
    let source = r#"
        package PhaseSystems
          partial package PartialPhaseSystem
            constant Integer n;
            type Voltage = Real;
            type Current = Real;
          end PartialPhaseSystem;

          package TwoConductor
            extends PartialPhaseSystem(n=2);
          end TwoConductor;
        end PhaseSystems;

        package Interfaces
          connector TerminalDC
            replaceable package PhaseSystem = PhaseSystems.PartialPhaseSystem;
            PhaseSystem.Voltage v[PhaseSystem.n];
            flow PhaseSystem.Current i[PhaseSystem.n];
          end TerminalDC;
        end Interfaces;

        package Ports
          connector TwoPin
            extends Interfaces.TerminalDC(
              redeclare package PhaseSystem = PhaseSystems.TwoConductor
            );
          end TwoPin;
        end Ports;

        model Source
          Ports.TwoPin term;
        end Source;

        model Top
          Source voltage;
        end Top;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay = match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, "Top") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    typecheck_instanced_test_projection(&instanced.tree, &mut instanced.overlay, "Top")
        .expect("instanced typecheck should succeed");
}

#[test]
fn test_parameter_colon_dimension_without_binding_is_allowed() {
    // Parameter `[:]` may remain unresolved until instantiation binds it.
    let source = r#"
        model Test
            parameter Real p[:];
        end Test;
    "#;

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let typed = typecheck(resolved).expect("typecheck should succeed");

    let tree = typed;
    let test_class = tree
        .definitions
        .classes
        .get("Test")
        .expect("Test class should exist");
    let p = test_class.components.get("p").expect("p should exist");
    assert!(
        p.shape.is_empty(),
        "unbound parameter colon dimensions should remain unresolved"
    );
}

/// Resolve `source` and run import-constant collection, returning the eval
/// context it produced.
///
/// The context is the observation point for this defect: Resolve/Instantiate
/// qualify a dimension reference to a constant's canonical name, and the
/// collector's contract is to make that canonical fact available. A constant
/// whose import was silently dropped never reaches the context, so the
/// canonical key is exactly the load-bearing evidence.
fn collected_import_constants(source: &str) -> rumoca_eval_ast::eval::TypeCheckEvalContext {
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.inner().clone();
    let mut ctx = rumoca_eval_ast::eval::TypeCheckEvalContext::for_pre_identity_structural();
    TypeChecker::collect_import_constants(&tree, &mut ctx);
    ctx
}

/// MLS §13.2.1 (PKG-005): a qualified import may name an element of a package,
/// including a package constant, which then evaluates as an array dimension.
#[test]
fn imported_package_constant_evaluates_as_dimension() {
    let source = r#"
        package Top
          package P
            constant Integer n = 5;
          end P;
          model M
            import Top.P.n;
            Real x[n];
          end M;
        end Top;
    "#;
    assert_eq!(
        collected_import_constants(source).integers.get("Top.P.n"),
        Some(&5),
        "import of package constant must publish the canonical Top.P.n = 5"
    );
}

/// Two unqualified imports reaching the *same* constant identity converge on
/// one binding and must not be refused as ambiguous (MLS §5.3.1).
#[test]
fn convergent_wildcards_to_one_constant_still_bind_as_dimension() {
    let source = r#"
        package Top
          package P
            constant Integer n = 4;
          end P;
          model M
            import Top.P.*;
            import Top.P.*;
            Real x[n];
          end M;
        end Top;
    "#;
    assert_eq!(
        collected_import_constants(source).integers.get("Top.P.n"),
        Some(&4),
        "two wildcards to the same constant must still publish Top.P.n = 4"
    );
}

/// A renamed import of a package constant binds the constant under its
/// canonical identity, so the local alias resolves as a dimension.
#[test]
fn renamed_import_of_constant_evaluates_as_dimension() {
    let source = r#"
        package Top
          package P
            constant Integer n = 5;
          end P;
          model M
            import k = Top.P.n;
            Real x[k];
          end M;
        end Top;
    "#;
    assert_eq!(
        collected_import_constants(source).integers.get("Top.P.n"),
        Some(&5),
        "renamed import k = Top.P.n must publish the canonical Top.P.n = 5"
    );
}

/// A shorter spelling shared with an unrelated package must not pollute the
/// binding: keys derive from the imported declaration's identity, not from the
/// short name. `Top.Aaa.n = 9` must not shadow the imported `Top.Zzz.n = 2`.
#[test]
fn imported_constant_is_keyed_by_identity_not_short_name() {
    let source = r#"
        package Top
          package Aaa
            constant Integer n = 9;
          end Aaa;
          package Zzz
            constant Integer n = 2;
          end Zzz;
          model M
            import Top.Zzz.n;
            Real x[n];
          end M;
        end Top;
    "#;
    let ctx = collected_import_constants(source);
    assert_eq!(
        ctx.integers.get("Top.Zzz.n"),
        Some(&2),
        "import of Top.Zzz.n must publish the canonical Top.Zzz.n = 2"
    );
    assert_eq!(
        ctx.integers.get("n"),
        None,
        "the imported constant must be keyed by identity, never by the bare short name n"
    );
}

/// Distinct constants named `n` reaching a scope through two different
/// wildcards remain genuinely ambiguous and are refused (MLS §5.3.1, ER112).
#[test]
fn distinct_wildcard_constants_remain_ambiguous() {
    let source = r#"
        package Top
          package Aaa
            constant Integer n = 3;
          end Aaa;
          package Bbb
            constant Integer n = 8;
          end Bbb;
          model M
            import Top.Aaa.*;
            import Top.Bbb.*;
            Real x[n];
          end M;
        end Top;
    "#;
    let diagnostics =
        resolve(parse(source)).expect_err("ambiguous unqualified imports must refuse");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("ER112")),
        "two wildcards to distinct constants named n must raise ER112"
    );
}

/// A qualified import naming a nonexistent package member is unresolved and
/// must be refused (ER002), never silently treated as an evaluable constant.
#[test]
fn qualified_import_of_missing_member_is_refused() {
    let source = r#"
        package Top
          package P
            constant Integer n = 5;
          end P;
          model M
            import Top.P.zzz;
            Real x[zzz];
          end M;
        end Top;
    "#;
    let diagnostics = resolve(parse(source)).expect_err("missing import member must refuse");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("ER002")),
        "import of nonexistent Top.P.zzz must raise ER002"
    );
}

/// A constant inherited into the imported package (MLS §7.1) must be read from
/// the class that actually declares it, located by its Resolve-published target
/// identity, and keyed by the name the reference resolves to (the import path).
/// The declaration is not a direct member of the imported package, so the
/// direct-member assumption that caused the original defect must not recur.
#[test]
fn wildcard_import_of_inherited_constant_publishes_import_path_key() {
    let source = r#"
        package Top
          package Base
            constant Integer n = 6;
          end Base;
          package Mid
            extends Top.Base;
          end Mid;
          model M
            import Top.Mid.*;
            Real x[n];
          end M;
        end Top;
    "#;
    assert_eq!(
        collected_import_constants(source).integers.get("Top.Mid.n"),
        Some(&6),
        "an inherited constant imported through the deriving package must publish Top.Mid.n = 6"
    );
}

/// Two classes that import distinct packages under the same local alias must not
/// collide in the shared context: keys are the canonical, globally unique
/// qualified names, and the shared short alias is never published, so the result
/// cannot depend on scope iteration order.
#[test]
fn same_alias_distinct_packages_do_not_collide_and_publish_no_short_key() {
    let source = r#"
        package Top
          package A
            constant Integer n = 11;
          end A;
          package B
            constant Integer n = 22;
          end B;
          model M1
            import X = Top.A;
            Real x[X.n];
          end M1;
          model M2
            import X = Top.B;
            Real y[X.n];
          end M2;
        end Top;
    "#;
    let ctx = collected_import_constants(source);
    assert_eq!(
        ctx.integers.get("Top.A.n"),
        Some(&11),
        "package A's constant is keyed canonically"
    );
    assert_eq!(
        ctx.integers.get("Top.B.n"),
        Some(&22),
        "package B's constant is keyed canonically and independently"
    );
    assert_eq!(
        ctx.integers.get("X.n"),
        None,
        "the shared local alias must never be published as a key"
    );
}

/// Instantiate `model_name` from `source` and run the instanced typecheck,
/// returning the overlay so extents evaluated from function calls (MLS §10.1,
/// §12.4) can be observed per occurrence.
fn instanced_overlay_for(source: &str, model_name: &str) -> InstanceOverlay {
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let mut instanced = {
        let tree = resolved.inner().clone();
        let overlay =
            match rumoca_phase_instantiate::instantiate_model_with_outcome(&tree, model_name) {
                rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
                rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                    missing_inners,
                    ..
                } => panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}"),
                rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                    panic!("fixture instantiation failed: {error}")
                }
            };
        rumoca_ir_ast::InstancedTree::new(tree, overlay)
    };
    typecheck_instanced_test_projection(&instanced.tree, &mut instanced.overlay, model_name)
        .expect("instanced typecheck should succeed");
    instanced.overlay
}

/// Evaluated dims of the overlay occurrence named `qualified_name`.
fn overlay_dims(overlay: &InstanceOverlay, qualified_name: &str) -> Vec<i64> {
    overlay
        .components
        .values()
        .find(|component| component.qualified_name.to_flat_string() == qualified_name)
        .unwrap_or_else(|| panic!("occurrence {qualified_name} must exist"))
        .dims
        .clone()
}

/// One package function reached four ways an import can expose it (MLS §13.2):
/// qualified member import, renamed import, wildcard import, and a renamed
/// package import used with a qualified call.
const IMPORTED_FUNCTION_EXTENT_FIXTURE: &str = r#"
    package Top
      package P
        function f
          input Integer u;
          output Integer y;
        algorithm
          y := u + 1;
        end f;
      end P;
      model MSel
        import Top.P.f;
        Real x[f(3)];
      end MSel;
      model MRen
        import g = Top.P.f;
        Real x[g(3)];
      end MRen;
      model MWild
        import Top.P.*;
        Real x[f(4)];
      end MWild;
      model MQual
        import Q = Top.P;
        Real x[Q.f(5)];
      end MQual;
    end Top;

    model Harness
      Top.MSel sel;
      Top.MRen ren;
      Top.MWild wild;
      Top.MQual qual;
    end Harness;
"#;

/// MLS §12.4: a pure function call is a valid array extent. Every import form
/// must evaluate through the call's resolved declaration identity: the
/// function catalog holds only canonical keys, so a spelled alias that
/// bypassed identity selection would leave these extents unevaluated.
#[test]
fn imported_function_extents_evaluate_by_declaration_identity() {
    let overlay = instanced_overlay_for(IMPORTED_FUNCTION_EXTENT_FIXTURE, "Harness");
    assert_eq!(
        overlay_dims(&overlay, "sel.x"),
        vec![4],
        "qualified import `import Top.P.f` must evaluate f(3) = 4"
    );
    assert_eq!(
        overlay_dims(&overlay, "ren.x"),
        vec![4],
        "renamed import `import g = Top.P.f` must evaluate g(3) = 4"
    );
    assert_eq!(
        overlay_dims(&overlay, "wild.x"),
        vec![5],
        "wildcard import `import Top.P.*` must evaluate f(4) = 5"
    );
    assert_eq!(
        overlay_dims(&overlay, "qual.x"),
        vec![6],
        "renamed package import `import Q = Top.P` must evaluate Q.f(5) = 6"
    );
}

/// The compile-time function catalog publishes exactly one key per function:
/// its canonical qualified name. Import aliases, terminal short names, and
/// alias-qualified spellings are never published; call selection reads the
/// call's Resolve-issued target identity instead.
#[test]
fn function_catalog_publishes_canonical_keys_only() {
    let parsed = parse(IMPORTED_FUNCTION_EXTENT_FIXTURE);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.inner().clone();
    let functions = build_function_defs_for_eval(&tree);
    assert!(
        functions.contains_key("Top.P.f"),
        "the function must be published under its canonical qualified name"
    );
    for spelled in ["f", "g", "Q.f", "P.f"] {
        assert!(
            !functions.contains_key(spelled),
            "spelling {spelled:?} must never be published: only the canonical \
             key exists and calls select by declaration identity"
        );
    }
}

/// Two models that import distinct functions under the same local alias must
/// each evaluate their own target: selection is by the call's resolved
/// declaration identity, and the shared alias is never a catalog key, so the
/// result cannot depend on scope iteration order.
#[test]
fn same_alias_distinct_functions_select_their_own_declarations() {
    let source = r#"
        package Top
          package A
            function f
              input Integer u;
              output Integer y;
            algorithm
              y := u + 11;
            end f;
          end A;
          package B
            function f
              input Integer u;
              output Integer y;
            algorithm
              y := u + 22;
            end f;
          end B;
          model M1
            import h = Top.A.f;
            Real x[h(0)];
          end M1;
          model M2
            import h = Top.B.f;
            Real x[h(0)];
          end M2;
        end Top;

        model Harness
          Top.M1 m1;
          Top.M2 m2;
        end Harness;
    "#;
    let overlay = instanced_overlay_for(source, "Harness");
    assert_eq!(
        overlay_dims(&overlay, "m1.x"),
        vec![11],
        "M1's alias h must select Top.A.f by identity"
    );
    assert_eq!(
        overlay_dims(&overlay, "m2.x"),
        vec![22],
        "M2's alias h must select Top.B.f by identity, not M1's earlier alias"
    );

    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.inner().clone();
    let functions = build_function_defs_for_eval(&tree);
    assert!(
        functions.contains_key("Top.A.f") && functions.contains_key("Top.B.f"),
        "both functions must be published canonically"
    );
    assert!(
        !functions.contains_key("h"),
        "the shared local alias must never be published as a key"
    );
}

/// A function declaration with no algorithm section has no interpretable body
/// (MLS §12.4 evaluates translation-time calls from the algorithm), so it is
/// absent from the compile-time function catalog under every key.
#[test]
fn function_without_algorithm_is_not_published() {
    let source = r#"
        package Top
          package P
            function e
              input Integer u;
              output Integer y;
            end e;
          end P;
          model M
            import Top.P.e;
            Real x[3];
          end M;
        end Top;
    "#;
    let parsed = parse(source);
    let resolved = resolve(parsed).expect("resolve should succeed");
    let tree = resolved.inner().clone();
    let e_def_id = tree
        .get_def_id_by_name("Top.P.e")
        .expect("Top.P.e must be resolved");
    let functions = build_function_defs_for_eval(&tree);
    assert!(
        !functions.contains_key("Top.P.e"),
        "an algorithm-free function must not enter the catalog canonically"
    );
    assert!(
        functions
            .values()
            .all(|class| class.def_id != Some(e_def_id)),
        "an algorithm-free function must not enter the catalog under any key"
    );
}

/// INST-050 (MLS §5.3.1): a function name supplied by two unqualified imports
/// is ambiguous, and a call through that name refuses at Resolve. The refusal
/// happens before any catalog exists, so no catalog spelling can resurrect
/// the ambiguous call.
#[test]
fn ambiguous_wildcard_function_import_refuses_at_resolve() {
    let source = r#"
        package Top
          package A
            function f
              input Integer u;
              output Integer y;
            algorithm
              y := u + 1;
            end f;
          end A;
          package B
            function f
              input Integer u;
              output Integer y;
            algorithm
              y := u + 2;
            end f;
          end B;
          model M
            import Top.A.*;
            import Top.B.*;
            Real x[f(3)];
          end M;
        end Top;
    "#;
    let diagnostics =
        resolve(parse(source)).expect_err("ambiguous unqualified function imports must refuse");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.code.as_deref() == Some("ER112")),
        "two wildcards supplying the called function name f must raise ER112"
    );
}
