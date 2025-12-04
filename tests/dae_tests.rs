//! DAE (Differential-Algebraic Equation) creation tests.
//!
//! Tests that models are correctly converted to DAE form.

mod common;

use common::{create_dae_from_fixture, parse_test_file};
use rumoca::ir::structural::create_dae::create_dae;
use rumoca::ir::transform::flatten::flatten;

// =============================================================================
// Basic DAE Creation Tests
// =============================================================================

#[test]
fn test_create_dae_integrator() {
    let dae = create_dae_from_fixture("integrator", "Integrator").unwrap();

    // Integrator should have state variable
    assert!(!dae.x.is_empty(), "Should have states");

    // Should have equations (derivatives are in der() calls, not separate variables)
    assert!(!dae.fx.is_empty(), "Should have continuous equations");
}

#[test]
fn test_create_dae_bouncing_ball() {
    let dae = create_dae_from_fixture("bouncing_ball", "BouncingBall").unwrap();

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
    let dae = create_dae_from_fixture("bouncing_ball", "BouncingBall").unwrap();

    // Bouncing ball has parameters (e, g, etc.)
    assert!(
        !dae.p.is_empty() || !dae.cp.is_empty(),
        "Should have parameters"
    );
}

#[test]
fn test_create_dae_metadata() {
    let dae = create_dae_from_fixture("integrator", "Integrator").unwrap();

    // Check metadata fields
    assert!(!dae.rumoca_version.is_empty(), "Should have rumoca version");
    assert!(!dae.git_version.is_empty(), "Should have git version");

    // Time component should exist
    assert_eq!(dae.t.name, "t", "Should have time variable");
}

#[test]
fn test_create_dae_rover() {
    let dae = create_dae_from_fixture("rover", "Rover").unwrap();

    // Rover is complex and should have many states
    assert!(!dae.x.is_empty(), "Should have states");

    // May have inputs
    if !dae.u.is_empty() {
        println!("Rover has {} inputs", dae.u.len());
    }
}

// =============================================================================
// Multi-Model DAE Tests
// =============================================================================

#[test]
fn test_create_dae_supported_models() {
    // Models that we can fully process (no unsupported features)
    let models = [
        ("integrator", "Integrator"),
        ("bouncing_ball", "BouncingBall"),
        ("rover", "Rover"),
        ("quadrotor", "Quadrotor"),
        ("nightvapor", "NightVapor"),
    ];

    for (file, model_name) in models {
        let dae = create_dae_from_fixture(file, model_name)
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

// =============================================================================
// Circuit Model Tests
// =============================================================================

#[test]
fn test_create_dae_connection_equations() {
    // simple_circuit has connection equations that are now properly expanded
    let dae = create_dae_from_fixture("simple_circuit", "SimpleCircuit").unwrap();

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
            .any(|n| n.contains(".v") || n.contains(".i")),
        "Should have voltage or current state variables"
    );
}

#[test]
fn test_simple_circuit_blt_causalization() {
    // This test verifies the BLT transformation properly causalizes the circuit equations.
    // The circuit has:
    // - 2 state variables: C.v (capacitor voltage), L1.i (inductor current)
    // - 30 algebraic variables from expanded connect equations and component equations
    // - 32 equations total (all should be causalized to form: var = expr)
    use rumoca::ir::ast::Equation;

    let def = parse_test_file("simple_circuit").unwrap();
    let mut fclass = flatten(&def, Some("SimpleCircuit")).unwrap();
    let dae = create_dae(&mut fclass).unwrap();

    // Verify we have the expected states
    assert_eq!(dae.x.len(), 2, "Should have exactly 2 states (C.v, L1.i)");
    assert!(dae.x.contains_key("C.v"), "Should have C.v state");
    assert!(dae.x.contains_key("L1.i"), "Should have L1.i state");

    // Verify we have 32 continuous equations
    assert_eq!(
        dae.fx.len(),
        32,
        "Should have exactly 32 continuous equations"
    );

    // Verify all equations are causalized (LHS is a simple variable or der(var))
    for (i, eq) in dae.fx.iter().enumerate() {
        match eq {
            Equation::Simple { lhs, .. } => {
                use rumoca::ir::ast::Expression;
                let is_causalized = match lhs {
                    Expression::ComponentReference(_) => true,
                    Expression::FunctionCall { comp, .. } => comp.to_string() == "der",
                    _ => false,
                };
                assert!(
                    is_causalized,
                    "Equation {} should be causalized (LHS should be var or der(var)), got: {:?}",
                    i, lhs
                );
            }
            _ => {
                // Other equation types (If, For) are allowed
            }
        }
    }

    // Verify algebraic variables (y) are defined - these should all be outputs from flattened components
    assert!(
        dae.y.len() >= 28,
        "Should have at least 28 algebraic variables, got {}",
        dae.y.len()
    );

    // Verify parameters exist
    assert!(dae.p.contains_key("R1.R"), "Should have R1.R parameter");
    assert!(dae.p.contains_key("C.C"), "Should have C.C parameter");
    assert!(dae.p.contains_key("R2.R"), "Should have R2.R parameter");
    assert!(dae.p.contains_key("L1.L"), "Should have L1.L parameter");
}

#[test]
fn test_simple_circuit_equation_order() {
    // This test verifies that equations are in proper topological order after BLT.
    // Each equation's RHS should only reference variables that are either:
    // 1. States (known from integration)
    // 2. Parameters (known constants)
    // 3. Algebraic variables defined by earlier equations
    // 4. The built-in 'time' variable
    use rumoca::ir::ast::{Equation, Expression};
    use std::collections::HashSet;

    let dae = create_dae_from_fixture("simple_circuit", "SimpleCircuit").unwrap();

    // Build set of "known" variables before processing equations
    let mut known: HashSet<String> = HashSet::new();

    // States are known (from integration)
    for name in dae.x.keys() {
        known.insert(name.clone());
    }

    // Parameters are known
    for name in dae.p.keys() {
        known.insert(name.clone());
    }

    // Constants are known
    for name in dae.cp.keys() {
        known.insert(name.clone());
    }

    // Inputs are known
    for name in dae.u.keys() {
        known.insert(name.clone());
    }

    // 'time' is always known
    known.insert("time".to_string());

    // Process equations in order - each should only use known variables
    for (i, eq) in dae.fx.iter().enumerate() {
        if let Equation::Simple { lhs, rhs } = eq {
            // Get the variable being defined
            let defined_var = match lhs {
                Expression::ComponentReference(cref) => Some(cref.to_string()),
                Expression::FunctionCall { comp, args } if comp.to_string() == "der" => {
                    // der(x) doesn't define a new variable, it uses existing state
                    if let Some(Expression::ComponentReference(cref)) = args.first() {
                        // The state should already be known
                        let state_name = cref.to_string();
                        assert!(
                            known.contains(&state_name),
                            "Equation {} uses der({}) but state is not known",
                            i,
                            state_name
                        );
                    }
                    None // der() equations don't define new algebraic vars
                }
                _ => None,
            };

            // Check that RHS only uses known variables
            let rhs_vars = extract_variables(rhs);
            for var in &rhs_vars {
                assert!(
                    known.contains(var),
                    "Equation {} defines {:?} but RHS uses unknown variable '{}'. Known: {:?}",
                    i,
                    defined_var,
                    var,
                    known
                );
            }

            // Add the defined variable to known set
            if let Some(var) = defined_var {
                known.insert(var);
            }
        }
    }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Extract variable names from an expression (helper for tests)
fn extract_variables(expr: &rumoca::ir::ast::Expression) -> Vec<String> {
    use rumoca::ir::ast::Expression;
    let mut vars = Vec::new();

    match expr {
        Expression::ComponentReference(cref) => {
            vars.push(cref.to_string());
        }
        Expression::Binary { lhs, rhs, .. } => {
            vars.extend(extract_variables(lhs));
            vars.extend(extract_variables(rhs));
        }
        Expression::Unary { rhs, .. } => {
            vars.extend(extract_variables(rhs));
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                vars.extend(extract_variables(arg));
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                vars.extend(extract_variables(cond));
                vars.extend(extract_variables(then_expr));
            }
            vars.extend(extract_variables(else_branch));
        }
        Expression::Array { elements } | Expression::Tuple { elements } => {
            for elem in elements {
                vars.extend(extract_variables(elem));
            }
        }
        Expression::Range { start, step, end } => {
            vars.extend(extract_variables(start));
            if let Some(s) = step {
                vars.extend(extract_variables(s));
            }
            vars.extend(extract_variables(end));
        }
        Expression::Terminal { .. } | Expression::Empty => {
            // No variables in terminals/literals
        }
        Expression::Parenthesized { inner } => {
            vars.extend(extract_variables(inner));
        }
        Expression::ArrayComprehension { expr, indices } => {
            vars.extend(extract_variables(expr));
            for idx in indices {
                vars.extend(extract_variables(&idx.range));
            }
        }
    }

    vars
}
