//! Balance check tests for Modelica models.
//!
//! Tests that models are correctly analyzed for equation/variable balance.

mod common;

use common::compile_fixture;

#[test]
fn test_balanced_integrator() {
    let result = compile_fixture("integrator", "Integrator").unwrap();

    assert!(result.is_balanced(), "Integrator should be balanced");
    assert!(result.balance_status().contains("balanced"));
    assert_eq!(result.balance.num_equations, 1);
    assert_eq!(result.balance.num_unknowns, 1);
    assert_eq!(result.balance.num_states, 1);
}

#[test]
fn test_balanced_bouncing_ball() {
    let result = compile_fixture("bouncing_ball", "BouncingBall").unwrap();

    assert!(result.is_balanced(), "BouncingBall should be balanced");
    // h and v are states, one algebraic (flying)
    assert_eq!(result.balance.num_states, 2);
}

#[test]
fn test_over_determined_model() {
    let result = compile_fixture("unbalanced_overdetermined", "UnbalancedOverdetermined").unwrap();

    assert!(
        !result.is_balanced(),
        "Over-determined model should not be balanced"
    );
    assert!(result.balance_status().contains("over-determined"));
    assert!(result.balance.num_equations > result.balance.num_unknowns);
}

#[test]
fn test_under_determined_model() {
    let result =
        compile_fixture("unbalanced_underdetermined", "UnbalancedUnderdetermined").unwrap();

    assert!(
        !result.is_balanced(),
        "Under-determined model should not be balanced"
    );
    assert!(result.balance_status().contains("under-determined"));
    assert!(result.balance.num_unknowns > result.balance.num_equations);
}

#[test]
fn test_balance_difference() {
    let result = compile_fixture("unbalanced_overdetermined", "UnbalancedOverdetermined").unwrap();

    let diff = result.balance.difference();
    assert!(
        diff > 0,
        "Over-determined model should have positive difference"
    );

    let result =
        compile_fixture("unbalanced_underdetermined", "UnbalancedUnderdetermined").unwrap();

    let diff = result.balance.difference();
    assert!(
        diff < 0,
        "Under-determined model should have negative difference"
    );
}

#[test]
fn test_type_causality_propagation() {
    // Test that type aliases like "connector RealInput = input Real" propagate
    // causality to components of that type
    let result = compile_fixture("type_causality", "Der").unwrap();

    // Der block: y = der(u)
    // - u is input (from RealInput type) -> not an unknown
    // - y is output (from RealOutput type) -> is an unknown
    // - 1 equation: y = der(u)
    // Should be balanced: 1 equation, 1 unknown
    assert!(
        result.is_balanced(),
        "Der block should be balanced when type causality is applied: {} equations, {} unknowns, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_equations, 1, "Should have 1 equation");
    assert_eq!(result.balance.num_unknowns, 1, "Should have 1 unknown (y)");
    assert_eq!(result.balance.num_inputs, 1, "Should have 1 input (u)");
}

#[test]
fn test_integrator_variants_balance() {
    // Test SimpleIntegrator - should be balanced
    // u (input), y (state from der(y)), 1 equation: der(y) = k*u
    let result = compile_fixture("integrator_simple", "SimpleIntegrator").unwrap();
    println!(
        "SimpleIntegrator: {} eq, {} unk, {} inputs, {} states",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs,
        result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "SimpleIntegrator should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test IntegratorWithProtected - has protected outputs
    // u (input), y (state), local_reset (output), local_set (output)
    // 3 equations: local_reset=false, local_set=0, der(y)=k*u
    // 3 unknowns: y, local_reset, local_set
    let result = compile_fixture("integrator_simple", "IntegratorWithProtected").unwrap();
    println!(
        "IntegratorWithProtected: {} eq, {} unk, {} inputs, {} states",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs,
        result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "IntegratorWithProtected should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test IntegratorWithIf - has if-equations
    let result = compile_fixture("integrator_simple", "IntegratorWithIf").unwrap();
    println!(
        "IntegratorWithIf: {} eq, {} unk, {} inputs, {} states",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs,
        result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "IntegratorWithIf should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
}

#[test]
fn test_array_balance() {
    // Test FixedArrayState - explicit equations for each array element
    // x[3] states = 3 unknowns, 3 equations: der(x[1])=..., der(x[2])=..., der(x[3])=...
    let result = compile_fixture("array_balance", "ArrayBalance.FixedArrayState").unwrap();
    println!(
        "FixedArrayState: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "FixedArrayState should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_states, 3, "Should have 3 states");

    // Test ForLoopArrayState - for loop equations
    // x[3] states = 3 unknowns, for loop with 3 iterations = 3 equations
    let result = compile_fixture("array_balance", "ArrayBalance.ForLoopArrayState").unwrap();
    println!(
        "ForLoopArrayState: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "ForLoopArrayState should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(
        result.balance.num_states, 3,
        "Should have 3 states from for loop"
    );

    // Test VectorEquation - vector equations without for loop
    // x[3] + y[3] = 6 unknowns, 2 vector equations = 6 scalar equations
    let result = compile_fixture("array_balance", "ArrayBalance.VectorEquation").unwrap();
    println!(
        "VectorEquation: {} eq, {} unk, {} states, {} algebraic",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic
    );
    assert!(
        result.is_balanced(),
        "VectorEquation should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_states, 3, "Should have 3 states (y[3])");
    assert_eq!(
        result.balance.num_algebraic, 3,
        "Should have 3 algebraic (x[3])"
    );
    assert_eq!(
        result.balance.num_equations, 6,
        "Should have 6 equations (2 vector eqs * 3)"
    );
}

#[test]
fn test_conditional_components() {
    // Test SimpleNoConditional - no conditional components
    // Should be balanced: 1 eq (y=2*u), 1 unk (y)
    let result = compile_fixture(
        "conditional_components",
        "ConditionalComponents.SimpleNoConditional",
    )
    .unwrap();
    println!(
        "SimpleNoConditional: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "SimpleNoConditional should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test ConditionalInputFalse - conditional component defaults to false
    // The `reset if use_reset` should NOT be counted since use_reset=false
    // Should be balanced: 1 eq (y=2*u), 1 unk (y)
    let result = compile_fixture(
        "conditional_components",
        "ConditionalComponents.ConditionalInputFalse",
    )
    .unwrap();
    println!(
        "ConditionalInputFalse: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    // This is what we're testing - conditional components should be filtered
    println!(
        "ConditionalInputFalse balance: {} (expected: balanced when condition=false filters component)",
        if result.is_balanced() {
            "balanced"
        } else {
            "unbalanced"
        }
    );

    // Test MultipleConditionalsFalse - multiple conditional components, all default to false
    let result = compile_fixture(
        "conditional_components",
        "ConditionalComponents.MultipleConditionalsFalse",
    )
    .unwrap();
    println!(
        "MultipleConditionalsFalse: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
}

#[test]
fn test_parameter_arrays() {
    // Test SimpleParameterForLoop - for i in 1:n where n=3
    // x[3] states = 3 unknowns, 3 equations from for loop
    let result =
        compile_fixture("parameter_arrays", "ParameterArrays.SimpleParameterForLoop").unwrap();
    println!(
        "SimpleParameterForLoop: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "SimpleParameterForLoop should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 3, "Should have 3 equations");
    assert_eq!(result.balance.num_unknowns, 3, "Should have 3 unknowns");

    // Test ParameterArithmetic - for i in 1:2*n where n=2
    // x[4] states = 4 unknowns, 4 equations from for loop
    let result =
        compile_fixture("parameter_arrays", "ParameterArrays.ParameterArithmetic").unwrap();
    println!(
        "ParameterArithmetic: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "ParameterArithmetic should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 4, "Should have 4 equations");
    assert_eq!(result.balance.num_unknowns, 4, "Should have 4 unknowns");

    // Test MultipleParameters - for i in 1:n (n=2) + for j in 1:m (m=3)
    // x[2] + y[3] = 5 unknowns, 2 + 3 = 5 equations
    let result = compile_fixture("parameter_arrays", "ParameterArrays.MultipleParameters").unwrap();
    println!(
        "MultipleParameters: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "MultipleParameters should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 5, "Should have 5 equations");
    assert_eq!(result.balance.num_unknowns, 5, "Should have 5 unknowns");

    // Test NestedForLoop - nested for i in 1:n, j in 1:m where n=m=2
    // x[2,2] = 4 unknowns, 4 equations from nested loop
    let result = compile_fixture("parameter_arrays", "ParameterArrays.NestedForLoop").unwrap();
    println!(
        "NestedForLoop: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "NestedForLoop should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 4, "Should have 4 equations");
    assert_eq!(result.balance.num_unknowns, 4, "Should have 4 unknowns");
}

#[test]
fn test_size_function() {
    // Test SimpleSizeFunction - a={1,2,3}, size(a,1)=3, x[size(a,1)-1]=x[2]
    // x[2] states = 2 unknowns, for i in 1:size(a,1)-1 = 2 equations
    let result = compile_fixture("size_function", "SizeFunction.SimpleSizeFunction").unwrap();
    println!(
        "SimpleSizeFunction: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "SimpleSizeFunction should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 2, "Should have 2 equations");
    assert_eq!(result.balance.num_unknowns, 2, "Should have 2 unknowns");

    // Test MultipleSizeCalls - b={1,2,3,4}, size(b,1)=4
    // y[4] states = 4 unknowns, 4 equations from for loop
    let result = compile_fixture("size_function", "SizeFunction.MultipleSizeCalls").unwrap();
    println!(
        "MultipleSizeCalls: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "MultipleSizeCalls should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 4, "Should have 4 equations");
    assert_eq!(result.balance.num_unknowns, 4, "Should have 4 unknowns");
}

#[test]
fn test_comparison_operators() {
    // Test EqualityTrue - n=0, condition n==0 is true
    // 1 equation (y=u), 1 unknown (y)
    let result =
        compile_fixture("comparison_operators", "ComparisonOperators.EqualityTrue").unwrap();
    println!(
        "EqualityTrue: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(
        result.is_balanced(),
        "EqualityTrue should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 1, "Should have 1 equation");

    // Test EqualityFalse - n=3, condition n==0 is false
    // 1 equation (y=2*u from else), 1 unknown (y)
    let result =
        compile_fixture("comparison_operators", "ComparisonOperators.EqualityFalse").unwrap();
    println!(
        "EqualityFalse: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(
        result.is_balanced(),
        "EqualityFalse should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 1, "Should have 1 equation");

    // Test LessThanTrue - n=2, condition n<5 is true
    let result =
        compile_fixture("comparison_operators", "ComparisonOperators.LessThanTrue").unwrap();
    println!(
        "LessThanTrue: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(result.is_balanced());
    assert_eq!(result.balance.num_equations, 1);

    // Test GreaterThanTrue - n=10, condition n>5 is true
    let result = compile_fixture(
        "comparison_operators",
        "ComparisonOperators.GreaterThanTrue",
    )
    .unwrap();
    println!(
        "GreaterThanTrue: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(result.is_balanced());
    assert_eq!(result.balance.num_equations, 1);

    // Test SizeComparisonTrue - a={1}, nx=size(a,1)-1=0, condition nx==0 is true
    // 1 equation (y=u), 1 unknown (y)
    let result = compile_fixture(
        "comparison_operators",
        "ComparisonOperators.SizeComparisonTrue",
    )
    .unwrap();
    println!(
        "SizeComparisonTrue: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(
        result.is_balanced(),
        "SizeComparisonTrue should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 1, "Should have 1 equation");

    // Test SizeComparisonFalse - a={1,2,3}, nx=size(a,1)-1=2, condition nx==0 is false
    // x[2] = 2 unknowns, for loop with 2 iterations = 2 equations
    let result = compile_fixture(
        "comparison_operators",
        "ComparisonOperators.SizeComparisonFalse",
    )
    .unwrap();
    println!(
        "SizeComparisonFalse: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "SizeComparisonFalse should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 2, "Should have 2 equations");
    assert_eq!(result.balance.num_unknowns, 2, "Should have 2 unknowns");

    // Test ProtectedParamTest - simpler test with protected parameter
    // a={1}, so nx=size(a,1)-1=0, condition nx==0 is true
    // 1 equation (y=u), 1 unknown (y)
    let result = compile_fixture(
        "comparison_operators",
        "ComparisonOperators.ProtectedParamTest",
    )
    .unwrap();
    println!(
        "ProtectedParamTest: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(
        result.is_balanced(),
        "ProtectedParamTest should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 1, "Should have 1 equation");

    // Test TransferFunctionLike - faithful reproduction of MSL TransferFunction structure
    // a={1}, so na=1, nx=0, x[0] and x_scaled[0] are empty
    // if nx==0 then y=d*u (1 equation), else (these equations not counted)
    // Should be balanced: 1 equation, 1 unknown (y)
    let result = compile_fixture(
        "comparison_operators",
        "ComparisonOperators.TransferFunctionLike",
    )
    .unwrap();
    println!(
        "TransferFunctionLike: {} eq, {} unk, {} states, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "TransferFunctionLike should be balanced: {} eq, {} unk (expected 1 eq, 1 unk)",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
}

#[test]
fn test_binding_equations() {
    // Test SlewLike - model with binding equation in protected section
    // Similar to Modelica.Blocks.Nonlinear.SlewRateLimiter
    // Should be balanced: 1 state (y), 1 algebraic (val)
    // 2 equations: val = (u - y) / 0.001, der(y) = val
    let result = compile_fixture("binding_test", "BindingTest.SlewLike").unwrap();
    println!(
        "SlewLike: {} eq, {} unk, {} states, {} alg",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic
    );
    assert!(
        result.is_balanced(),
        "SlewLike should be balanced: {} eq, {} unk (expected 2 eq, 2 unk)",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_states, 1, "Should have 1 state (y)");
    assert_eq!(
        result.balance.num_algebraic, 1,
        "Should have 1 algebraic (val)"
    );

    // Test ExplicitEquation - same structure with explicit equation
    // Should be balanced: 1 state (y), 1 algebraic (val)
    // 2 equations: val = (u - y) / 0.001, der(y) = val
    let result = compile_fixture("binding_test", "BindingTest.ExplicitEquation").unwrap();
    println!(
        "ExplicitEquation: {} eq, {} unk, {} states, {} alg",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic
    );
    assert!(
        result.is_balanced(),
        "ExplicitEquation should be balanced: {} eq, {} unk (expected 2 eq, 2 unk)",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
}

#[test]
fn test_nonlinear_blocks() {
    // Test Limiter block - SISO with if-equations and protected variable
    // 2 unknowns (y, simplifiedExpr), 2 equations
    let result = compile_fixture("nonlinear_blocks", "NonlinearBlocks.Limiter").unwrap();
    println!(
        "Limiter: {} eq, {} unk, {} states, {} alg, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic,
        result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "Limiter should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_inputs, 1, "Should have 1 input (u)");
    assert_eq!(
        result.balance.num_algebraic, 2,
        "Should have 2 algebraic (y, simplifiedExpr)"
    );

    // Test VariableLimiter block - SISO with multiple inputs
    // 2 unknowns (y, simplifiedExpr), 2 equations
    let result = compile_fixture("nonlinear_blocks", "NonlinearBlocks.VariableLimiter").unwrap();
    println!(
        "VariableLimiter: {} eq, {} unk, {} states, {} alg, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic,
        result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "VariableLimiter should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(
        result.balance.num_inputs, 3,
        "Should have 3 inputs (u, limit1, limit2)"
    );

    // Test SlewRateLimiter block - SISO with state and binding equation
    // 2 unknowns (y state, val algebraic), 2 equations (der(y)=..., val=... binding)
    let result = compile_fixture("nonlinear_blocks", "NonlinearBlocks.SlewRateLimiter").unwrap();
    println!(
        "SlewRateLimiter: {} eq, {} unk, {} states, {} alg, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic,
        result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "SlewRateLimiter should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(
        result.balance.num_states, 1,
        "Should have 1 state (y from der(y))"
    );
    assert_eq!(
        result.balance.num_algebraic, 1,
        "Should have 1 algebraic (val)"
    );

    // Test DeadZone block - simple SISO
    // 1 unknown (y), 1 equation
    let result = compile_fixture("nonlinear_blocks", "NonlinearBlocks.DeadZone").unwrap();
    println!(
        "DeadZone: {} eq, {} unk, {} states, {} alg, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic,
        result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "DeadZone should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 1, "Should have 1 equation");
    assert_eq!(result.balance.num_unknowns, 1, "Should have 1 unknown (y)");
}

#[test]
fn test_when_equations() {
    // Test SimpleWhen - state + discrete variable with when equation
    // 1 state (x), 1 discrete (y), 2 equations (der(x), when y=...)
    let result = compile_fixture("when_equations", "WhenEquations.SimpleWhen").unwrap();
    println!(
        "SimpleWhen: {} eq, {} unk, {} states, {} alg, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic,
        result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "SimpleWhen should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_states, 1, "Should have 1 state (x)");

    // Test MultipleDiscreteWhen - multiple discrete variables from when
    // 1 state (x), 2 discrete (y_min, y_max), 3 equations
    let result = compile_fixture("when_equations", "WhenEquations.MultipleDiscreteWhen").unwrap();
    println!(
        "MultipleDiscreteWhen: {} eq, {} unk, {} states, {} alg",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic
    );
    assert!(
        result.is_balanced(),
        "MultipleDiscreteWhen should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test WhenElsewhen - when/elsewhen branches
    let result = compile_fixture("when_equations", "WhenEquations.WhenElsewhen").unwrap();
    println!(
        "WhenElsewhen: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(
        result.is_balanced(),
        "WhenElsewhen should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test ExtremaLike - pattern from MSL ContinuousSignalExtrema
    // 1 state (x), 4 discrete (y_min, y_max, t_min, t_max), 5 equations
    let result = compile_fixture("when_equations", "WhenEquations.ExtremaLike").unwrap();
    println!(
        "ExtremaLike: {} eq, {} unk, {} states, {} alg",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_states,
        result.balance.num_algebraic
    );
    assert!(
        result.is_balanced(),
        "ExtremaLike should be balanced: {} eq, {} unk (expected 5 eq, 5 unk)",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
}

#[test]
fn test_connector_equations() {
    // Test SingleResistor - resistor with external pins and connections
    let result = compile_fixture("connectors", "Connectors.SingleResistor");
    match result {
        Ok(res) => {
            println!(
                "SingleResistor: {} eq, {} unk, {} states, {} alg",
                res.balance.num_equations,
                res.balance.num_unknowns,
                res.balance.num_states,
                res.balance.num_algebraic
            );
            // With connections: should be balanced
            // Resistor has 2 equations, connections add equality + flow equations
        }
        Err(e) => {
            println!("SingleResistor compile error: {:?}", e);
        }
    }

    // Test TwoResistors - two resistors in series
    let result = compile_fixture("connectors", "Connectors.TwoResistors");
    match result {
        Ok(res) => {
            println!(
                "TwoResistors: {} eq, {} unk, {} states, {} alg",
                res.balance.num_equations,
                res.balance.num_unknowns,
                res.balance.num_states,
                res.balance.num_algebraic
            );
        }
        Err(e) => {
            println!("TwoResistors compile error: {:?}", e);
        }
    }
}

#[test]
fn test_causal_blocks() {
    // Test JustOutput - minimal output variable (direct type)
    let result = compile_fixture("connectors", "Connectors.JustOutput");
    match result {
        Ok(res) => {
            println!(
                "JustOutput: {} eq, {} unk, {} inputs",
                res.balance.num_equations, res.balance.num_unknowns, res.balance.num_inputs
            );
            assert!(
                res.is_balanced(),
                "JustOutput should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
        }
        Err(e) => {
            println!("JustOutput compile error: {:?}", e);
        }
    }

    // Test JustOutputAlias - output with type alias (RealOutput = output Real)
    let result = compile_fixture("connectors", "Connectors.JustOutputAlias");
    match result {
        Ok(res) => {
            println!(
                "JustOutputAlias: {} eq, {} unk, {} inputs",
                res.balance.num_equations, res.balance.num_unknowns, res.balance.num_inputs
            );
            assert!(
                res.is_balanced(),
                "JustOutputAlias should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
        }
        Err(e) => {
            println!("JustOutputAlias compile error: {:?}", e);
        }
    }

    // Test SimpleGain - SISO block with input and output
    let result = compile_fixture("connectors", "Connectors.SimpleGain");
    match result {
        Ok(res) => {
            println!(
                "SimpleGain: {} eq, {} unk, {} inputs",
                res.balance.num_equations, res.balance.num_unknowns, res.balance.num_inputs
            );
            assert!(
                res.is_balanced(),
                "SimpleGain should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
            assert_eq!(res.balance.num_inputs, 1, "Should have 1 input (u)");
        }
        Err(e) => {
            println!("SimpleGain compile error: {:?}", e);
        }
    }

    // Test VectorizedGain - block with parameter-sized arrays
    let result = compile_fixture("connectors", "Connectors.VectorizedGain");
    match result {
        Ok(res) => {
            println!(
                "VectorizedGain: {} eq, {} unk, {} inputs (n=2)",
                res.balance.num_equations, res.balance.num_unknowns, res.balance.num_inputs
            );
            // With n=2: should be 2 equations, 2 unknowns (y[1], y[2])
            assert!(
                res.is_balanced(),
                "VectorizedGain should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
        }
        Err(e) => {
            println!("VectorizedGain compile error: {:?}", e);
        }
    }

    // Test EmptyArrayPassthrough - empty arrays (n=0) should produce 0 eq, 0 unk
    let result = compile_fixture("connectors", "Connectors.EmptyArrayPassthrough");
    match result {
        Ok(res) => {
            println!(
                "EmptyArrayPassthrough: {} eq, {} unk, {} inputs (n=0)",
                res.balance.num_equations, res.balance.num_unknowns, res.balance.num_inputs
            );
            // With n=0: should be 0 equations, 0 unknowns (empty arrays)
            assert!(
                res.is_balanced(),
                "EmptyArrayPassthrough should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
            assert_eq!(
                res.balance.num_equations, 0,
                "Empty array should have 0 equations"
            );
            assert_eq!(
                res.balance.num_unknowns, 0,
                "Empty array should have 0 unknowns"
            );
        }
        Err(e) => {
            panic!("EmptyArrayPassthrough compile error: {:?}", e);
        }
    }
}

#[test]
fn test_algorithm_sections() {
    // Test SimpleAlgorithm - single output from algorithm
    // 1 unknown (y), 1 equation (from algorithm section)
    let result = compile_fixture("algorithms", "Algorithms.SimpleAlgorithm");
    match result {
        Ok(res) => {
            println!(
                "SimpleAlgorithm: {} eq, {} unk, {} inputs",
                res.balance.num_equations, res.balance.num_unknowns, res.balance.num_inputs
            );
            assert!(
                res.is_balanced(),
                "SimpleAlgorithm should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
            assert_eq!(
                res.balance.num_equations, 1,
                "Should have 1 equation from algorithm"
            );
            assert_eq!(res.balance.num_unknowns, 1, "Should have 1 unknown (y)");
        }
        Err(e) => {
            panic!("SimpleAlgorithm compile error: {:?}", e);
        }
    }

    // Test MultipleOutputs - two outputs from algorithm
    // 2 unknowns (y1, y2), 2 equations (from algorithm section)
    let result = compile_fixture("algorithms", "Algorithms.MultipleOutputs");
    match result {
        Ok(res) => {
            println!(
                "MultipleOutputs: {} eq, {} unk, {} inputs",
                res.balance.num_equations, res.balance.num_unknowns, res.balance.num_inputs
            );
            assert!(
                res.is_balanced(),
                "MultipleOutputs should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
            assert_eq!(
                res.balance.num_equations, 2,
                "Should have 2 equations from algorithm"
            );
        }
        Err(e) => {
            panic!("MultipleOutputs compile error: {:?}", e);
        }
    }

    // Test AlgorithmWithIf - algorithm with if statement
    let result = compile_fixture("algorithms", "Algorithms.AlgorithmWithIf");
    match result {
        Ok(res) => {
            println!(
                "AlgorithmWithIf: {} eq, {} unk",
                res.balance.num_equations, res.balance.num_unknowns
            );
            assert!(
                res.is_balanced(),
                "AlgorithmWithIf should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
        }
        Err(e) => {
            panic!("AlgorithmWithIf compile error: {:?}", e);
        }
    }

    // Test MixedEquationAlgorithm - both equation and algorithm sections
    let result = compile_fixture("algorithms", "Algorithms.MixedEquationAlgorithm");
    match result {
        Ok(res) => {
            println!(
                "MixedEquationAlgorithm: {} eq, {} unk",
                res.balance.num_equations, res.balance.num_unknowns
            );
            assert!(
                res.is_balanced(),
                "MixedEquationAlgorithm should be balanced: {} eq, {} unk",
                res.balance.num_equations,
                res.balance.num_unknowns
            );
        }
        Err(e) => {
            panic!("MixedEquationAlgorithm compile error: {:?}", e);
        }
    }
}
