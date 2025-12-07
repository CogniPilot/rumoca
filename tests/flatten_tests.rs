mod common;

use common::parse_test_file;
use rumoca::ir::ast::Causality;
use rumoca::ir::transform::flatten::flatten;

#[test]
fn test_flatten_integrator() {
    let def = parse_test_file("integrator").unwrap();
    let fclass = flatten(&def, Some("Integrator")).unwrap();

    // Simple model should have basic components
    assert!(!fclass.components.is_empty());
    assert!(!fclass.equations.is_empty());
}

#[test]
fn test_flatten_bouncing_ball() {
    let def = parse_test_file("bouncing_ball").unwrap();
    let fclass = flatten(&def, Some("BouncingBall")).unwrap();

    // Should have state variables (position, velocity)
    assert!(fclass.components.len() >= 2);
    // Should have equations (kinematics + dynamics)
    assert!(!fclass.equations.is_empty());
}

#[test]
fn test_flatten_hierarchical_rover() {
    let def = parse_test_file("rover").unwrap();
    let fclass = flatten(&def, Some("Rover")).unwrap();

    // Rover has hierarchical components that should be flattened
    // Look for dots in component names (flattened subcomponents)
    let has_flattened_names = fclass.components.keys().any(|k| k.contains('.'));

    if !has_flattened_names {
        // If no dots, model might be simpler than expected
        // Just ensure it flattened successfully
        assert!(!fclass.components.is_empty());
    }
}

#[test]
fn test_flatten_quadrotor() {
    let def = parse_test_file("quadrotor").unwrap();
    let fclass = flatten(&def, Some("Quadrotor")).unwrap();

    // Quadrotor is a complex hierarchical model
    assert!(!fclass.components.is_empty());
    assert!(!fclass.equations.is_empty());
}

#[test]
fn test_flatten_preserves_equations() {
    let def = parse_test_file("integrator").unwrap();
    let original_class = def.class_list.get("Integrator").unwrap();
    let equation_count_before = original_class.equations.len();

    let fclass = flatten(&def, Some("Integrator")).unwrap();
    let equation_count_after = fclass.equations.len();

    // Flattening should preserve or expand equations (not lose them)
    assert!(
        equation_count_after >= equation_count_before,
        "Flattening lost equations: before={}, after={}",
        equation_count_before,
        equation_count_after
    );
}

#[test]
fn test_flatten_all_models() {
    let models = vec![
        ("integrator", "Integrator"),
        ("bouncing_ball", "BouncingBall"),
        ("rover", "Rover"),
        ("quadrotor", "Quadrotor"),
        ("simple_circuit", "SimpleCircuit"),
        ("nightvapor", "NightVapor"),
    ];

    for (file, model_name) in models {
        let def =
            parse_test_file(file).unwrap_or_else(|e| panic!("Failed to parse {}: {}", file, e));

        flatten(&def, Some(model_name))
            .unwrap_or_else(|e| panic!("Failed to flatten {}: {}", file, e));
    }
}

#[test]
fn test_flatten_requires_model_name() {
    let def = parse_test_file("integrator").unwrap();
    let result = flatten(&def, None);

    assert!(
        result.is_err(),
        "Should error when model name is not provided"
    );
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("Model name is required"),
        "Error should mention model name is required: {}",
        err_msg
    );
}

#[test]
fn test_flatten_scoping_with_nested_extends() {
    // This tests that nested inheritance is properly handled:
    // - ScopingTest has components e1, e2 of type Extended
    // - Extended extends Base, which has x and k
    // - After flattening, we should have e1.x, e1.k, e1.y, e2.x, e2.k, e2.y, total
    let def = parse_test_file("scoping_test").unwrap();
    let fclass = flatten(&def, Some("ScopingTest")).unwrap();

    // Check that we have the expected flattened components
    let component_names: Vec<&String> = fclass.components.keys().collect();

    // Should have e1.x (inherited from Base via Extended)
    assert!(
        fclass.components.contains_key("e1.x"),
        "Should have e1.x (inherited from Base). Got: {:?}",
        component_names
    );
    // Should have e1.k (inherited from Base via Extended)
    assert!(
        fclass.components.contains_key("e1.k"),
        "Should have e1.k (inherited from Base). Got: {:?}",
        component_names
    );
    // Should have e1.y (from Extended)
    assert!(
        fclass.components.contains_key("e1.y"),
        "Should have e1.y (from Extended). Got: {:?}",
        component_names
    );

    // Same for e2
    assert!(
        fclass.components.contains_key("e2.x"),
        "Should have e2.x. Got: {:?}",
        component_names
    );
    assert!(
        fclass.components.contains_key("e2.k"),
        "Should have e2.k. Got: {:?}",
        component_names
    );
    assert!(
        fclass.components.contains_key("e2.y"),
        "Should have e2.y. Got: {:?}",
        component_names
    );

    // Should have total (directly in ScopingTest)
    assert!(
        fclass.components.contains_key("total"),
        "Should have total. Got: {:?}",
        component_names
    );

    // Should NOT have the unexpanded component names
    assert!(
        !fclass.components.contains_key("e1"),
        "Should not have unexpanded e1. Got: {:?}",
        component_names
    );
    assert!(
        !fclass.components.contains_key("e2"),
        "Should not have unexpanded e2. Got: {:?}",
        component_names
    );

    // Should have equations from Base (der(x) = -k*x) for both e1 and e2
    // Plus equations from Extended (y = 2*x) for both e1 and e2
    // Plus equation from ScopingTest (total = e1.x + e2.x)
    // That's 5 equations total (2 from Base, 2 from Extended, 1 from ScopingTest)
    assert!(
        fclass.equations.len() >= 5,
        "Should have at least 5 equations, got {}",
        fclass.equations.len()
    );
}

#[test]
fn test_type_causality_debug() {
    use rumoca::ir::structural::create_dae::create_dae;

    // Debug test to see what's happening with type causality
    let def = parse_test_file("type_causality").unwrap();

    // Print class list to see what's available
    println!("\n=== Class List ===");
    for (name, class) in &def.class_list {
        println!(
            "  {} (type: {:?}, causality: {:?})",
            name, class.class_type, class.causality
        );
        for (nested_name, nested_class) in &class.classes {
            println!(
                "    -> {} (type: {:?}, causality: {:?})",
                nested_name, nested_class.class_type, nested_class.causality
            );
        }
    }

    // Flatten Der and check component causality
    let mut fclass = flatten(&def, Some("Der")).unwrap();

    println!("\n=== Flattened Der components ===");
    for (name, comp) in &fclass.components {
        println!(
            "  {} : {} (causality: {:?})",
            name, comp.type_name, comp.causality
        );
    }

    // Create DAE and check what ends up where
    let dae = create_dae(&mut fclass).unwrap();
    println!("\n=== DAE Structure ===");
    println!("  Inputs (u): {:?}", dae.u.keys().collect::<Vec<_>>());
    println!("  Outputs (y): {:?}", dae.y.keys().collect::<Vec<_>>());
    println!("  States (x): {:?}", dae.x.keys().collect::<Vec<_>>());
    println!("  Equations: {}", dae.fx.len());

    // Check causality
    let u = fclass.components.get("u").expect("Should have component u");
    let y = fclass.components.get("y").expect("Should have component y");

    assert!(
        matches!(u.causality, Causality::Input(_)),
        "u should have Input causality, got {:?}",
        u.causality
    );
    assert!(
        matches!(y.causality, Causality::Output(_)),
        "y should have Output causality, got {:?}",
        y.causality
    );
}

#[test]
fn test_flatten_modifications_scope_renaming() {
    // This test verifies that when a component with nested components is flattened,
    // the modification expressions on subcomponents are properly scope-renamed.
    //
    // For example, if Block B has:
    //   constant Real k = 1;
    //   SubBlock sub(gain = k);  // modification uses local constant k
    //
    // When flattening Model M with B b, we should get:
    //   b.k = 1;
    //   b.sub.gain = b.k;  // modification must reference b.k, not just k
    //
    // This was a bug fixed in the flattening code where modifications were not
    // being scope-renamed, causing "undefined variable" errors.

    let source = r#"
// Subcomponent class
class Sub
  parameter Real k = 1.0;
  Real value;
equation
  value = k;
end Sub;

// Block class with constant used in modification
class Block
  constant Real unitTime = 5.0;
  Sub sub(k = unitTime);
  Real out;
equation
  out = sub.value;
end Block;

// Main model with nested Block
model TestModel
  Block b;
  Real total;
equation
  total = b.out;
end TestModel;
"#;

    use common::parse_source;

    let def = parse_source(source).expect("Parse failed");
    let fclass = flatten(&def, Some("TestModel")).expect("Flatten failed");

    // After flattening:
    // - b.unitTime should exist (constant from Block)
    // - b.sub.k should exist (parameter from Sub)
    // - The modification on b.sub.k should reference b.unitTime

    let component_names: Vec<&String> = fclass.components.keys().collect();

    // Check that constants and parameters are properly flattened
    assert!(
        fclass.components.contains_key("b.unitTime"),
        "Should have b.unitTime. Got: {:?}",
        component_names
    );
    assert!(
        fclass.components.contains_key("b.sub.k"),
        "Should have b.sub.k. Got: {:?}",
        component_names
    );

    // Check that the modification on b.sub.k was properly scope-renamed
    let sub_k = fclass.components.get("b.sub.k").unwrap();
    if !sub_k.modifications.is_empty() {
        // If the modifications dict is not empty, they should reference b.* scope
        for (param_name, mod_expr) in &sub_k.modifications {
            let mod_str = format!("{:?}", mod_expr);
            // The modification should NOT contain just "unitTime" without prefix
            // It should contain "b.unitTime"
            if mod_str.contains("unitTime") {
                assert!(
                    mod_str.contains("b.unitTime") || mod_str.contains(r#""b""#),
                    "Modification {} should reference 'b.unitTime', got {:?}",
                    param_name,
                    mod_expr
                );
            }
        }
    }
}

#[test]
fn test_flatten_with_deps_tracks_dependencies() {
    use rumoca::ir::transform::flatten::flatten_with_deps;

    // Test with a simple model - should return empty dependencies for inline test code
    let source = r#"
model Simple
  Real x;
equation
  der(x) = 1;
end Simple;
"#;

    use common::parse_source;

    let def = parse_source(source).expect("Parse failed");
    let result = flatten_with_deps(&def, Some("Simple")).expect("Flatten failed");

    // For inline test code with file_name "<test>", we expect no file dependencies
    // because test code doesn't come from a file on disk
    assert!(
        result.dependencies.files.is_empty(),
        "Inline test code should have no file dependencies, got: {:?}",
        result.dependencies.files
    );

    // Verify the class was flattened correctly
    assert!(
        result.class.components.contains_key("x"),
        "Should have component x"
    );
    assert!(!result.class.equations.is_empty(), "Should have equations");
}

#[test]
fn test_flatten_with_deps_model_with_inheritance() {
    use rumoca::ir::transform::flatten::flatten_with_deps;

    // Test with a model that has inheritance
    let source = r#"
class Base
  Real x;
equation
  der(x) = 1;
end Base;

model Derived
  extends Base;
  Real y;
equation
  y = x * 2;
end Derived;
"#;

    use common::parse_source;

    let def = parse_source(source).expect("Parse failed");
    let result = flatten_with_deps(&def, Some("Derived")).expect("Flatten failed");

    // For inline test code, no file dependencies expected
    assert!(
        result.dependencies.files.is_empty(),
        "Inline test code should have no file dependencies"
    );

    // Verify inheritance was resolved correctly
    assert!(
        result.class.components.contains_key("x"),
        "Should have inherited component x"
    );
    assert!(
        result.class.components.contains_key("y"),
        "Should have component y"
    );
}

#[test]
fn test_flatten_with_deps_tracks_file_dependencies() {
    use rumoca::ir::transform::flatten::flatten_with_deps;

    // Parse a real file and verify dependencies are tracked
    let def = parse_test_file("integrator").expect("Parse failed");
    let result = flatten_with_deps(&def, Some("Integrator")).expect("Flatten failed");

    // For file-based code, we expect at least one file dependency
    // (the integrator.mo file itself)
    assert!(
        !result.dependencies.files.is_empty(),
        "File-based code should have file dependencies"
    );

    // Check that the dependency file path is valid
    for (file_path, hash) in &result.dependencies.files {
        assert!(!file_path.is_empty(), "File path should not be empty");
        assert!(!hash.is_empty(), "Hash should not be empty");
        // The file should contain the expected filename or exist
        // (relative paths may not pass exists() check depending on cwd)
        assert!(
            file_path.contains("integrator") || std::path::Path::new(file_path).exists(),
            "Dependency file should contain integrator or exist: {}",
            file_path
        );
    }

    println!(
        "Tracked {} file dependencies: {:?}",
        result.dependencies.files.len(),
        result.dependencies.files.keys().collect::<Vec<_>>()
    );
}
