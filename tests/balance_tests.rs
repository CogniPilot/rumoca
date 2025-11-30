mod common;

use rumoca::Compiler;

#[test]
fn test_balanced_integrator() {
    let result = Compiler::new()
        .model("Integrator")
        .compile_file("tests/fixtures/integrator.mo")
        .unwrap();

    assert!(result.is_balanced(), "Integrator should be balanced");
    assert!(result.balance_status().contains("balanced"));
    assert_eq!(result.balance.num_equations, 1);
    assert_eq!(result.balance.num_unknowns, 1);
    assert_eq!(result.balance.num_states, 1);
}

#[test]
fn test_balanced_bouncing_ball() {
    let result = Compiler::new()
        .model("BouncingBall")
        .compile_file("tests/fixtures/bouncing_ball.mo")
        .unwrap();

    assert!(result.is_balanced(), "BouncingBall should be balanced");
    // h and v are states, one algebraic (flying)
    assert_eq!(result.balance.num_states, 2);
}

#[test]
fn test_over_determined_model() {
    let result = Compiler::new()
        .model("UnbalancedOverdetermined")
        .compile_file("tests/fixtures/unbalanced_overdetermined.mo")
        .unwrap();

    assert!(
        !result.is_balanced(),
        "Over-determined model should not be balanced"
    );
    assert!(result.balance_status().contains("over-determined"));
    assert!(result.balance.num_equations > result.balance.num_unknowns);
}

#[test]
fn test_under_determined_model() {
    let result = Compiler::new()
        .model("UnbalancedUnderdetermined")
        .compile_file("tests/fixtures/unbalanced_underdetermined.mo")
        .unwrap();

    assert!(
        !result.is_balanced(),
        "Under-determined model should not be balanced"
    );
    assert!(result.balance_status().contains("under-determined"));
    assert!(result.balance.num_unknowns > result.balance.num_equations);
}

#[test]
fn test_balance_difference() {
    let result = Compiler::new()
        .model("UnbalancedOverdetermined")
        .compile_file("tests/fixtures/unbalanced_overdetermined.mo")
        .unwrap();

    let diff = result.balance.difference();
    assert!(
        diff > 0,
        "Over-determined model should have positive difference"
    );

    let result = Compiler::new()
        .model("UnbalancedUnderdetermined")
        .compile_file("tests/fixtures/unbalanced_underdetermined.mo")
        .unwrap();

    let diff = result.balance.difference();
    assert!(
        diff < 0,
        "Under-determined model should have negative difference"
    );
}
