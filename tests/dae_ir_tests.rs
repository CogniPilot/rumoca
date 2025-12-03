mod common;

use common::parse_test_file;
use rumoca::dae::dae_ir::DaeIR;
use rumoca::ir::structural::create_dae::create_dae;
use rumoca::ir::transform::flatten::flatten;
use serde_json::Value;

#[test]
fn test_bouncing_ball_dae_ir_json() {
    let def = parse_test_file("bouncing_ball").unwrap();
    let mut fclass = flatten(&def, Some("BouncingBall")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Create DAE IR
    let dae_ir = DaeIR::from_dae(&dae);

    // Serialize to JSON
    let json_str = serde_json::to_string_pretty(&dae_ir).unwrap();
    let json_value: Value = serde_json::from_str(&json_str).unwrap();

    // Validate basic structure
    assert_eq!(json_value["ir_version"], "dae-0.1.0");
    assert_eq!(json_value["base_modelica_version"], "0.1");
    assert_eq!(json_value["model_name"], "BouncingBall");

    // Validate variables is an object with classified arrays
    let vars = &json_value["variables"];
    assert!(vars.is_object(), "variables should be an object");

    // Validate states (x)
    let states = vars["states"].as_array().unwrap();
    assert!(!states.is_empty(), "Should have states");

    // Find 'h' (height) state
    let h_state = states.iter().find(|s| s["name"] == "h");
    assert!(h_state.is_some(), "Should have 'h' state");
    let h_state = h_state.unwrap();
    assert_eq!(h_state["vartype"], "Real");
    assert!(
        h_state["state_index"].is_number(),
        "State should have state_index"
    );

    // Check that h has correct start value (simple value, not expression)
    assert_eq!(h_state["start"], 1.0);

    // Find 'v' (velocity) state
    let v_state = states.iter().find(|s| s["name"] == "v");
    assert!(v_state.is_some(), "Should have 'v' state");

    // Validate parameters
    let params = vars["parameters"].as_array().unwrap();
    assert!(!params.is_empty(), "Should have parameters");

    // Find 'e' (coefficient of restitution) parameter
    let e_param = params.iter().find(|p| p["name"] == "e");
    assert!(e_param.is_some(), "Should have 'e' parameter");
    let e_param = e_param.unwrap();
    assert_eq!(e_param["vartype"], "Real");

    // Check that e has correct start value (simple value, not expression)
    assert_eq!(e_param["start"], 0.8);

    // Validate equations is an object with classified arrays
    let eqs = &json_value["equations"];
    assert!(eqs.is_object(), "equations should be an object");

    // Validate continuous equations
    let continuous_eqs = eqs["continuous"].as_array().unwrap();
    assert!(
        !continuous_eqs.is_empty(),
        "Should have continuous equations"
    );

    // Count simple equations
    let simple_eqs: Vec<_> = continuous_eqs
        .iter()
        .filter(|eq| eq["eq_type"] == "simple")
        .collect();

    println!("Total continuous equations: {}", continuous_eqs.len());
    println!("Simple equations: {}", simple_eqs.len());

    // Should have at least the continuous equations (z=2*h+v, der(h)=v, der(v)=-9.81)
    assert!(
        simple_eqs.len() >= 3,
        "Should have at least 3 simple equations"
    );

    // Validate structure metadata
    let structure = &json_value["structure"];
    assert!(structure.is_object(), "Should have structure metadata");
    assert_eq!(structure["n_states"], 2, "Should have 2 states (h, v)");
    assert!(
        structure["n_equations"].as_i64().unwrap() >= 3,
        "Should have at least 3 equations"
    );

    println!("\n=== CHECKING DAE IR STRUCTURE ===");
    println!("States: {}", states.len());
    println!("Parameters: {}", params.len());
    println!("Continuous equations: {}", continuous_eqs.len());
    println!("n_states: {}", structure["n_states"]);
    println!("n_algebraic: {}", structure["n_algebraic"]);
    println!("is_ode: {}", structure["is_ode"]);

    // Check for event indicators
    let event_indicators = json_value["event_indicators"].as_array().unwrap();
    println!("Event indicators: {}", event_indicators.len());

    // Check for algorithms (reinit statements)
    let algorithms = json_value["algorithms"].as_array().unwrap();
    println!("Algorithms: {}", algorithms.len());

    if !algorithms.is_empty() {
        let algo = &algorithms[0];
        let stmts = algo["statements"].as_array().unwrap();
        println!("Statements in first algorithm: {}", stmts.len());

        for stmt in stmts {
            println!("  Statement type: {}", stmt["stmt"]);
        }
    }

    println!(
        "\n✓✓✓ SUCCESS: DAE IR format correctly exports with explicit variable classification! ✓✓✓"
    );
}

#[test]
fn test_integrator_dae_ir_json() {
    let def = parse_test_file("integrator").unwrap();
    let mut fclass = flatten(&def, Some("Integrator")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Create DAE IR
    let dae_ir = DaeIR::from_dae(&dae);

    // Serialize to JSON
    let json_str = serde_json::to_string_pretty(&dae_ir).unwrap();
    let json_value: Value = serde_json::from_str(&json_str).unwrap();

    // Basic validation
    assert_eq!(json_value["ir_version"], "dae-0.1.0");
    assert_eq!(json_value["model_name"], "Integrator");

    // Should have classified variables
    let vars = &json_value["variables"];
    assert!(vars["states"].is_array(), "Should have states array");
    assert!(
        vars["parameters"].is_array(),
        "Should have parameters array"
    );

    // Should have classified equations
    let eqs = &json_value["equations"];
    assert!(
        eqs["continuous"].is_array(),
        "Should have continuous equations array"
    );

    let continuous_eqs = eqs["continuous"].as_array().unwrap();
    assert!(
        !continuous_eqs.is_empty(),
        "Integrator should have continuous equations"
    );

    println!("Integrator DAE IR export successful");
}

#[test]
fn test_der_in_equations() {
    // Test that der() function calls appear in equations
    let def = parse_test_file("bouncing_ball").unwrap();
    let mut fclass = flatten(&def, Some("BouncingBall")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    let dae_ir = DaeIR::from_dae(&dae);
    let json_str = serde_json::to_string_pretty(&dae_ir).unwrap();

    // Verify that der() calls appear in the JSON (as function calls in equations)
    assert!(
        json_str.contains("\"op\": \"der\""),
        "Equations should contain der() function calls"
    );

    println!("✓ der() function calls appear in equations");
}
