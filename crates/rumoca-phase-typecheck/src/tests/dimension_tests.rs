//! Array-dimension evaluation on the class tree: explicit and colon extents,
//! redeclared-package dimensions, and the constant prefixes they resolve
//! through.

use super::*;

#[test]
fn instance_scalar_rank_requires_consensus_and_retains_empty_array_evidence() {
    let tree = resolve(parse(
        r#"
        model Test
            replaceable parameter Real value = asin(0.5);
            parameter Real values[:] = {value};
        end Test;
    "#,
    ))
    .expect("resolve")
    .into_inner();
    let model = tree.get_class_by_qualified_name("Test").unwrap();
    let component = &model.components["value"];
    let binding = model.components["values"].binding.as_ref().unwrap();
    for (array, unapplied, expected) in [
        (false, false, Some(vec![1])),
        (true, false, None),
        (false, true, None),
    ] {
        let mut overlay = InstanceOverlay::new();
        add_test_instance(&mut overlay, "value", component, None);
        if array {
            // A sibling empty array has no element to inspect. Its producer
            // must retain this declaration-level veto before dropping it.
            overlay
                .array_component_declarations
                .insert(component.def_id.unwrap());
        }
        if unapplied {
            overlay
                .components
                .values_mut()
                .next()
                .unwrap()
                .has_unapplied_redeclare = true;
        }
        let mut ctx = rumoca_eval_ast::eval::TypeCheckEvalContext::new();
        ctx.declared_dimensions = Arc::new(
            rumoca_eval_ast::eval::DeclaredDimensions::from_instanced(&tree, &overlay),
        );
        assert_eq!(
            rumoca_eval_ast::eval::infer_dimensions_from_binding(binding, &ctx),
            expected
        );
    }
}

#[test]
fn declared_constant_rank_preserves_alias_dimensions_and_component_prefixes() {
    let source = r#"
        package Constants
            type Scalar = Real;
            type ScalarAlias = Scalar;
            type Vector = Real[3];
            type VectorAlias = Vector;
            constant ScalarAlias scalar = 2*asin(1.0);
            constant VectorAlias vector = {scalar, scalar, scalar};
            constant Real explicit[3] = vector;
            constant Real inferred[:] = vector;
        end Constants;
        record Box
            constant Real scalar = 2*asin(1.0);
        end Box;
        model Test
            Box boxes[3];
            Box box;
            parameter Real a[:] = {Constants.scalar};
            parameter Real b[:, :] = {Constants.vector};
            parameter Real c[:, :] = {Constants.explicit};
            parameter Real d[:, :] = {Constants.inferred};
            parameter Real e[:, :] = {boxes.scalar};
            parameter Real f[:] = {box.scalar};
        end Test;
    "#;
    let tree = resolve(parse(source))
        .expect("resolved declarations")
        .into_inner();
    let mut checker = TypeChecker::new();
    checker.eval_ctx.declared_dimensions =
        std::sync::Arc::new(rumoca_eval_ast::eval::DeclaredDimensions::from_resolved_tree(&tree));
    let model = tree.get_class_by_qualified_name("Test").expect("model");
    for (name, expected) in [
        ("a", Some(vec![1])),
        ("b", None),
        ("c", None),
        ("d", None),
        ("e", None),
        ("f", Some(vec![1])),
    ] {
        let binding = model.components[name].binding.as_ref().expect("binding");
        // No constant values or instance extents have been supplied. Only the
        // scalar alias and scalar record field prove a complete shape; the
        // array-valued paths must remain unknown.
        assert_eq!(
            rumoca_eval_ast::eval::infer_dimensions_from_binding_with_scope(
                binding,
                &checker.eval_ctx,
                "Test"
            ),
            expected,
            "{name}"
        );
    }
}

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

    let tree = typed.into_inner();
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

    let tree = typed.into_inner();
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
    let mut instanced = rumoca_phase_instantiate::instantiate(resolved, "Test")
        .expect("instantiate should succeed");
    typecheck_instanced(&instanced.tree, &mut instanced.overlay, "Test")
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
    let mut instanced =
        rumoca_phase_instantiate::instantiate(resolved, "Top").expect("instantiate should succeed");
    typecheck_instanced(&instanced.tree, &mut instanced.overlay, "Top")
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

    let tree = typed.into_inner();
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

#[test]
fn test_import_constant_prefixes_include_alias_and_full_path() {
    let import = ScopeImport::Renamed {
        alias: rumoca_core::ComponentPath::from_flat_path("Medium"),
        path: vec![
            "Modelica".to_string(),
            "Media".to_string(),
            "Air".to_string(),
            "ReferenceMoistAir".to_string(),
        ],
        def_id: DefId::new(7),
    };
    let mut prefixes = TypeChecker::import_constant_prefixes(&import);
    prefixes.sort_by(|a, b| a.0.cmp(&b.0));

    assert!(
        prefixes.iter().any(|(name, _)| name == "Medium"),
        "renamed import alias should be included"
    );
    assert!(
        prefixes
            .iter()
            .any(|(name, _)| name == "Modelica.Media.Air.ReferenceMoistAir"),
        "full import path should be included for strict structural lookup"
    );
    assert!(
        prefixes.iter().any(|(name, _)| name == "ReferenceMoistAir"),
        "terminal import symbol should be included for compatibility"
    );
}
