//! Parser tests for Modelica files.
//!
//! Tests that various Modelica model files parse correctly.

mod common;

use common::{STANDARD_FIXTURES, parse_test_file};

#[test]
fn test_parse_integrator() {
    let def = parse_test_file("integrator").unwrap();
    assert_eq!(def.class_list.len(), 1);
    assert!(def.class_list.contains_key("Integrator"));
}

#[test]
fn test_parse_bouncing_ball() {
    let def = parse_test_file("bouncing_ball").unwrap();
    assert!(def.class_list.contains_key("BouncingBall"));

    let bouncing_ball = def.class_list.get("BouncingBall").unwrap();
    // Should have components (position, velocity, parameters)
    assert!(!bouncing_ball.components.is_empty());
    // Should have equations
    assert!(!bouncing_ball.equations.is_empty());
}

#[test]
fn test_parse_rover() {
    let def = parse_test_file("rover").unwrap();
    assert!(def.class_list.contains_key("Rover"));

    let rover = def.class_list.get("Rover").unwrap();
    // Rover is a hierarchical model with subcomponents
    assert!(!rover.components.is_empty());
}

#[test]
fn test_parse_quadrotor() {
    let def = parse_test_file("quadrotor").unwrap();
    assert!(def.class_list.contains_key("Quadrotor"));
}

#[test]
fn test_parse_simple_circuit() {
    let def = parse_test_file("simple_circuit").unwrap();
    // Circuit model should have multiple class definitions
    assert!(!def.class_list.is_empty());
}

#[test]
fn test_parse_nightvapor() {
    let def = parse_test_file("nightvapor").unwrap();
    assert!(def.class_list.contains_key("NightVapor"));
}

#[test]
fn test_all_models_parse_successfully() {
    for model in STANDARD_FIXTURES {
        parse_test_file(model).unwrap_or_else(|e| panic!("Failed to parse {}: {}", model, e));
    }
}
