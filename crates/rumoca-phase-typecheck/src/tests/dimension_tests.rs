//! Array-dimension evaluation on the class tree: explicit and colon extents,
//! redeclared-package dimensions, and the constant prefixes they resolve
//! through.

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
