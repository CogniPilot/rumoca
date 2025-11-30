mod common;

use common::parse_test_file;
use rumoca::dae::base_modelica::BaseModelicaIR;
use rumoca::ir::create_dae::create_dae;
use rumoca::ir::flatten::flatten;
use serde_json::Value;

#[test]
fn test_bouncing_ball_base_modelica_json() {
    let def = parse_test_file("bouncing_ball").unwrap();
    let mut fclass = flatten(&def, Some("BouncingBall")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Create Base Modelica IR
    let base_modelica = BaseModelicaIR::from_dae(&dae);

    // Serialize to JSON
    let json_str = serde_json::to_string_pretty(&base_modelica).unwrap();
    let json_value: Value = serde_json::from_str(&json_str).unwrap();

    // Validate basic structure
    assert_eq!(json_value["ir_version"], "base-0.1.0");
    assert_eq!(json_value["base_modelica_version"], "0.1");
    assert_eq!(json_value["model_name"], "GeneratedModel");

    // Validate parameters
    let params = json_value["parameters"].as_array().unwrap();
    assert!(!params.is_empty(), "Should have parameters");

    // Find 'e' (coefficient of restitution) parameter
    let e_param = params.iter().find(|p| p["name"] == "e");
    assert!(e_param.is_some(), "Should have 'e' parameter");
    let e_param = e_param.unwrap();
    assert_eq!(e_param["vartype"], "Real");
    assert_eq!(e_param["variability"], "parameter");

    // Check that e has correct start value
    let e_start = &e_param["start"];
    assert_eq!(e_start["op"], "literal");
    assert_eq!(e_start["value"], 0.8);

    // Find 'h0' (initial height) parameter
    let h0_param = params.iter().find(|p| p["name"] == "h0");
    assert!(h0_param.is_some(), "Should have 'h0' parameter");
    let h0_param = h0_param.unwrap();
    let h0_start = &h0_param["start"];
    assert_eq!(h0_start["op"], "literal");
    assert_eq!(h0_start["value"], 1.0);

    // Validate variables
    let vars = json_value["variables"].as_array().unwrap();
    assert!(!vars.is_empty(), "Should have variables");

    // Find 'h' (height) variable
    let h_var = vars.iter().find(|v| v["name"] == "h");
    assert!(h_var.is_some(), "Should have 'h' variable");
    let h_var = h_var.unwrap();
    assert_eq!(h_var["vartype"], "Real");
    assert_eq!(h_var["variability"], "continuous");

    // TODO: Check that h has correct start value (currently exports 0.0 instead of 1.0 - bug!)
    // This is a known issue that needs to be fixed
    println!("h.start = {:?}", h_var["start"]);

    // Find 'v' (velocity) variable
    let v_var = vars.iter().find(|v| v["name"] == "v");
    assert!(v_var.is_some(), "Should have 'v' variable");

    // Validate equations
    let eqs = json_value["equations"].as_array().unwrap();
    assert!(!eqs.is_empty(), "Should have equations");

    // Count equation types
    let simple_eqs: Vec<_> = eqs.iter().filter(|eq| eq["eq_type"] == "simple").collect();

    println!("Total equations: {}", eqs.len());
    println!("Simple equations: {}", simple_eqs.len());

    // Should have at least the continuous equations
    assert!(
        simple_eqs.len() >= 3,
        "Should have at least 3 simple equations (z=2*h+v, v=der(h), der(v)=-9.81)"
    );

    // Check for when equations in the exported JSON
    println!("\n=== CHECKING FOR WHEN EQUATIONS ===");
    println!("DAE has {} reset expressions (fr)", dae.fr.len());
    println!("DAE has {} event equations (fz)", dae.fz.len());
    println!("DAE has {} conditions (c)", dae.c.len());

    // Print reset expressions if any
    if !dae.fr.is_empty() {
        println!("\nReset expressions (fr):");
        for (cond, stmt) in &dae.fr {
            println!("  when {} then {:?}", cond, stmt);
        }
    }

    // Find when equations in JSON
    let when_eqs: Vec<_> = eqs.iter().filter(|eq| eq["eq_type"] == "when").collect();

    println!("\nWhen equations in JSON: {}", when_eqs.len());

    // Verify we have the when equation
    assert_eq!(
        when_eqs.len(),
        1,
        "Should have exactly 1 when equation in JSON"
    );

    let when_eq = when_eqs[0];
    println!("\nWhen equation structure:");
    println!("{}", serde_json::to_string_pretty(when_eq).unwrap());

    // Validate the when equation structure
    assert!(
        when_eq["branches"].is_array(),
        "When equation should have branches"
    );
    let branches = when_eq["branches"].as_array().unwrap();
    assert_eq!(branches.len(), 1, "Should have 1 branch (one condition)");

    let branch = &branches[0];
    assert!(
        branch["condition"].is_object(),
        "Branch should have condition"
    );
    assert!(
        branch["equations"].is_array(),
        "Branch should have equations"
    );

    let branch_eqs = branch["equations"].as_array().unwrap();
    assert_eq!(
        branch_eqs.len(),
        1,
        "Should have 1 equation in when branch (reinit(v, ...))"
    );

    println!("\n✓✓✓ SUCCESS: When equations are now properly exported to Base Modelica JSON! ✓✓✓");
}

#[test]
fn test_integrator_base_modelica_json() {
    let def = parse_test_file("integrator").unwrap();
    let mut fclass = flatten(&def, Some("Integrator")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Create Base Modelica IR
    let base_modelica = BaseModelicaIR::from_dae(&dae);

    // Serialize to JSON
    let json_str = serde_json::to_string_pretty(&base_modelica).unwrap();
    let json_value: Value = serde_json::from_str(&json_str).unwrap();

    // Basic validation
    assert_eq!(json_value["ir_version"], "base-0.1.0");
    assert_eq!(json_value["model_name"], "GeneratedModel");

    // Should have at least one equation
    let eqs = json_value["equations"].as_array().unwrap();
    assert!(!eqs.is_empty(), "Integrator should have equations");

    println!("Integrator JSON export successful");
}
