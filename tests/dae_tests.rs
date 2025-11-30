mod common;

use common::parse_test_file;
use rumoca::ir::create_dae::create_dae;
use rumoca::ir::flatten::flatten;

#[test]
fn test_create_dae_integrator() {
    let def = parse_test_file("integrator").unwrap();
    let mut fclass = flatten(&def, Some("Integrator")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Integrator should have state variable
    assert!(!dae.x.is_empty(), "Should have states");

    // Should have equations (derivatives are in der() calls, not separate variables)
    assert!(!dae.fx.is_empty(), "Should have continuous equations");
}

#[test]
fn test_create_dae_bouncing_ball() {
    let def = parse_test_file("bouncing_ball").unwrap();
    let mut fclass = flatten(&def, Some("BouncingBall")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Bouncing ball should have states (position, velocity)
    assert!(!dae.x.is_empty(), "Should have states");

    // Should have when equations (bouncing event)
    // Conditions should be detected
    assert!(
        !dae.c.is_empty() || !dae.fc.is_empty(),
        "Should have conditions from when clauses"
    );
}

#[test]
fn test_create_dae_parameters() {
    let def = parse_test_file("bouncing_ball").unwrap();
    let mut fclass = flatten(&def, Some("BouncingBall")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Bouncing ball has parameters (e, g, etc.)
    assert!(
        !dae.p.is_empty() || !dae.cp.is_empty(),
        "Should have parameters"
    );
}

#[test]
fn test_create_dae_metadata() {
    let def = parse_test_file("integrator").unwrap();
    let mut fclass = flatten(&def, Some("Integrator")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Check metadata fields
    assert!(!dae.rumoca_version.is_empty(), "Should have rumoca version");
    assert!(!dae.git_version.is_empty(), "Should have git version");

    // Time component should exist
    assert_eq!(dae.t.name, "t", "Should have time variable");
}

#[test]
fn test_create_dae_rover() {
    let def = parse_test_file("rover").unwrap();
    let mut fclass = flatten(&def, Some("Rover")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Rover is complex and should have many states
    assert!(!dae.x.is_empty(), "Should have states");

    // May have inputs
    if !dae.u.is_empty() {
        println!("Rover has {} inputs", dae.u.len());
    }
}

#[test]
fn test_create_dae_supported_models() {
    // Models that we can fully process (no unsupported features)
    let models = vec![
        ("integrator", "Integrator"),
        ("bouncing_ball", "BouncingBall"),
        ("rover", "Rover"),
        ("quadrotor", "Quadrotor"),
        ("nightvapor", "NightVapor"),
    ];

    for (file, model_name) in models {
        let def =
            parse_test_file(file).unwrap_or_else(|e| panic!("Failed to parse {}: {}", file, e));

        let mut fclass = flatten(&def, Some(model_name))
            .unwrap_or_else(|e| panic!("Failed to flatten {}: {}", file, e));

        let dae = create_dae(&mut fclass)
            .unwrap_or_else(|e| panic!("Failed to create DAE for {}: {}", file, e));

        // All models should have some form of equations or states
        let has_content =
            !dae.x.is_empty() || !dae.y.is_empty() || !dae.fx.is_empty() || !dae.p.is_empty();

        assert!(
            has_content,
            "{} DAE should have some content (states, equations, or parameters)",
            file
        );
    }
}

#[test]
fn test_create_dae_connection_equations() {
    // simple_circuit has connection equations that are now properly expanded
    let def = parse_test_file("simple_circuit").unwrap();

    // Flattening should succeed with connection equation expansion
    let mut fclass = flatten(&def, Some("SimpleCircuit")).unwrap();

    // DAE creation should now succeed with expanded connect equations
    let result = create_dae(&mut fclass);
    assert!(
        result.is_ok(),
        "simple_circuit should now compile successfully: {:?}",
        result.err()
    );

    let dae = result.unwrap();

    // Verify the DAE has the expected structure
    // simple_circuit has: R1, C, R2, L1, AC, G
    // Each TwoPin component contributes states and equations
    assert!(
        !dae.x.is_empty(),
        "simple_circuit should have state variables"
    );
    assert!(
        !dae.fx.is_empty(),
        "simple_circuit should have continuous time equations (including expanded connect equations)"
    );
    assert!(!dae.p.is_empty(), "simple_circuit should have parameters");

    // Verify some expected variables exist
    let var_names: Vec<&str> = dae.x.keys().map(|s| s.as_str()).collect();
    assert!(
        var_names
            .iter()
            .any(|n| n.contains("_v") || n.contains("_i")),
        "Should have voltage or current state variables"
    );
}
