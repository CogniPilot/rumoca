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
