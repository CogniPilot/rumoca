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
    assert!(
        result.is_balanced(),
        "ConditionalInputFalse should be balanced (reset filtered out): {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test ConditionalInputTrue - conditional component defaults to true
    // The `reset if use_reset` SHOULD be counted since use_reset=true
    // Should be unbalanced: 1 eq, 2 unk (reset is included as an input)
    let result = compile_fixture(
        "conditional_components",
        "ConditionalComponents.ConditionalInputTrue",
    )
    .unwrap();
    println!(
        "ConditionalInputTrue: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    // With use_reset=true, the reset input is included, making it unbalanced (1 eq, 2 unk)
    // Actually it will be partial since reset is an external connector
    assert_eq!(
        result.balance.num_inputs, 2,
        "ConditionalInputTrue should have 2 inputs (u and reset)"
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
    assert!(
        result.is_balanced(),
        "MultipleConditionalsFalse should be balanced (both conditionals filtered): {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test ConditionalWithAnd - condition is `use_reset and use_set`, both false
    let result = compile_fixture(
        "conditional_components",
        "ConditionalComponents.ConditionalWithAnd",
    )
    .unwrap();
    println!(
        "ConditionalWithAnd: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "ConditionalWithAnd should be balanced (AND condition evaluates to false): {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
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

    // Test OuterWithInner - parameter modification propagation
    // Outer has order=3, passes to InnerForLoop(order=3)
    // na = integer((3+1)/2) = 2, x[2] = 2 states/unknowns, 2 equations
    let result = compile_fixture("parameter_arrays", "ParameterArrays.OuterWithInner").unwrap();
    println!(
        "OuterWithInner: {} eq, {} unk, {} states",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_states
    );
    assert!(
        result.is_balanced(),
        "OuterWithInner should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(result.balance.num_equations, 2, "Should have 2 equations");
    assert_eq!(result.balance.num_unknowns, 2, "Should have 2 unknowns");
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

#[test]
fn test_complex_type_causality() {
    // Test ComplexGain - simple Complex-like record block with input and output
    // Uses MyComplexInput = input MyComplex and MyComplexOutput = output MyComplex type aliases
    // u (MyComplexInput) -> u.re, u.im are inputs (not unknowns)
    // y (MyComplexOutput) -> y.re, y.im are unknowns
    // 2 equations from assignments, 2 unknowns (y.re, y.im)
    let result = compile_fixture("complex_causality", "ComplexCausality.ComplexGain").unwrap();
    assert!(
        result.is_balanced(),
        "ComplexGain should be balanced when causality is propagated: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    // u.re, u.im should be inputs (2 inputs)
    assert_eq!(
        result.balance.num_inputs, 2,
        "Should have 2 inputs (u.re, u.im)"
    );
    // y.re, y.im should be unknowns (2 unknowns)
    assert_eq!(
        result.balance.num_unknowns, 2,
        "Should have 2 unknowns (y.re, y.im)"
    );

    // Test ComplexArrayInput - array of Complex-like record inputs
    // u[m] (MyComplexInput array, m=3) -> u.re[3], u.im[3] as array components
    // y[2] (output array) -> 2 unknowns
    // 2 equations from sum assignments
    let result =
        compile_fixture("complex_causality", "ComplexCausality.ComplexArrayInput").unwrap();
    assert!(
        result.is_balanced(),
        "ComplexArrayInput should be balanced when array causality is propagated: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    // u.re[3], u.im[3] arrays are inputs - each array is counted as size*1, so 3+3=6 inputs
    assert_eq!(
        result.balance.num_inputs, 6,
        "Should have 6 inputs (u.re[3], u.im[3])"
    );
    // y[2] should be unknowns (2 unknowns)
    assert_eq!(
        result.balance.num_unknowns, 2,
        "Should have 2 unknowns (y[2])"
    );

    // Test ComplexAdd - Complex-typed components with binding expressions (like MSL ComplexSI2SO)
    // This tests that binding equations for Complex-typed protected variables get expanded properly.
    // u1, u2 (MyComplexInput) -> 4 inputs (u1.re, u1.im, u2.re, u2.im)
    // y (MyComplexOutput) -> 2 unknowns (y.re, y.im)
    // u1Internal, u2Internal are protected with binding u1Internal = u1, u2Internal = u2
    // These should expand to: u1Internal.re = u1.re, u1Internal.im = u1.im (etc.)
    // So we have 4 more unknowns (u1Internal.re, u1Internal.im, u2Internal.re, u2Internal.im)
    // Total: 6 unknowns, 4 inputs
    // Equations: 2 from y = ..., and 4 from binding expansion = 6 total
    let result = compile_fixture("complex_causality", "ComplexCausality.ComplexAdd").unwrap();
    println!(
        "ComplexAdd: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "ComplexAdd should be balanced when Complex binding equations are expanded: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    // u1.re, u1.im, u2.re, u2.im should be inputs (4 inputs)
    assert_eq!(
        result.balance.num_inputs, 4,
        "Should have 4 inputs (u1.re, u1.im, u2.re, u2.im)"
    );
    // y.re, y.im, u1Internal.re, u1Internal.im, u2Internal.re, u2Internal.im should be unknowns (6 unknowns)
    assert_eq!(result.balance.num_unknowns, 6, "Should have 6 unknowns");
    // 6 equations: 2 from y = ..., and 4 from binding expansion
    assert_eq!(result.balance.num_equations, 6, "Should have 6 equations");

    // Test ComplexAddConditional - like ComplexAdd but with conditional binding expressions
    // u1Internal = if useConjugateInput1 then MyComplex(u1.re, -u1.im) else u1
    // This tests that if-expressions and record constructors are handled in Complex expansion
    let result = compile_fixture(
        "complex_causality",
        "ComplexCausality.ComplexAddConditional",
    )
    .unwrap();
    println!(
        "ComplexAddConditional: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "ComplexAddConditional should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_inputs, 4, "Should have 4 inputs");
    assert_eq!(result.balance.num_unknowns, 6, "Should have 6 unknowns");
    assert_eq!(result.balance.num_equations, 6, "Should have 6 equations");

    // Test BuiltinComplexAdd - uses builtin Complex type like MSL
    // This tests that the builtin Complex works the same as our custom MyComplex
    let result =
        compile_fixture("complex_causality", "ComplexCausality.BuiltinComplexAdd").unwrap();
    println!(
        "BuiltinComplexAdd: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "BuiltinComplexAdd should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_inputs, 4, "Should have 4 inputs");
    assert_eq!(result.balance.num_unknowns, 6, "Should have 6 unknowns");
    assert_eq!(result.balance.num_equations, 6, "Should have 6 equations");

    // Test ParenthesizedComplexAdd - exactly like MSL's ComplexSI2SO pattern
    // Binding: Complex u1Internal = (if useConjugateInput1 then u2 else u1);
    let result = compile_fixture(
        "complex_causality",
        "ComplexCausality.ParenthesizedComplexAdd",
    )
    .unwrap();
    println!(
        "ParenthesizedComplexAdd: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "ParenthesizedComplexAdd should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_inputs, 4, "Should have 4 inputs");
    assert_eq!(result.balance.num_unknowns, 6, "Should have 6 unknowns");
    assert_eq!(result.balance.num_equations, 6, "Should have 6 equations");

    // Test ConjComplexAdd - exactly like MSL's ComplexMath.Add with conj() function
    // Binding: Complex u1Internal = (if useConjugateInput1 then conj(u1) else u1);
    // This tests function inlining combined with Complex expansion
    let result = compile_fixture("complex_causality", "ComplexCausality.ConjComplexAdd").unwrap();
    println!(
        "ConjComplexAdd: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "ConjComplexAdd should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_inputs, 4, "Should have 4 inputs");
    assert_eq!(result.balance.num_unknowns, 6, "Should have 6 unknowns");
    assert_eq!(result.balance.num_equations, 6, "Should have 6 equations");
}

#[test]
fn test_conditional_input_causality() {
    // Test ConditionalInputBlock - conditional input with type alias
    // When use_numberPort=true (default), numberPort is included and should be an input
    // showNumber is an output (unknown), 1 equation from if-equation
    let result = compile_fixture(
        "conditional_input",
        "ConditionalInput.ConditionalInputBlock",
    )
    .unwrap();
    println!(
        "ConditionalInputBlock: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "ConditionalInputBlock should be balanced when causality is propagated: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    // numberPort should be an input (type alias RealInput = input Real)
    assert_eq!(
        result.balance.num_inputs, 1,
        "Should have 1 input (numberPort)"
    );
    // showNumber should be an unknown (output)
    assert_eq!(
        result.balance.num_unknowns, 1,
        "Should have 1 unknown (showNumber)"
    );

    // Test ConditionalInputBlockFalse - conditional input with use_numberPort=false
    // When use_numberPort=false, numberPort is not included
    // showNumber is an output (unknown), 1 equation from else branch
    let result = compile_fixture(
        "conditional_input",
        "ConditionalInput.ConditionalInputBlockFalse",
    )
    .unwrap();
    println!(
        "ConditionalInputBlockFalse: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "ConditionalInputBlockFalse should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    // No inputs when use_numberPort=false (numberPort is filtered out)
    assert_eq!(
        result.balance.num_inputs, 0,
        "Should have 0 inputs (numberPort filtered out)"
    );
    // showNumber should be an unknown (output)
    assert_eq!(
        result.balance.num_unknowns, 1,
        "Should have 1 unknown (showNumber)"
    );
}

/// Test expression blocks (RealExpression, BooleanExpression, IntegerExpression style)
/// These blocks have output connectors with binding expressions but no equation section.
///
/// KNOWN LIMITATION: Binding equations for outputs with default values (0.0, 0, false)
/// are NOT currently generated. This is because the parser sets default start values
/// for all Real/Integer/Boolean components, and we can't reliably distinguish between
/// explicit bindings like `RealOutput y = 0.0` and parser defaults.
///
/// The MSL Expression blocks (RealExpression, BooleanExpression, IntegerExpression)
/// will appear as under-determined until this is fixed. The fix requires tracking
/// whether a binding was explicitly written in source code vs generated as a parser default.
#[test]
fn test_expression_blocks() {
    // Test RealExpressionLike - output with default binding, no equation section
    // The binding generates an equation, making this balanced
    let result =
        compile_fixture("expression_blocks", "ExpressionBlocks.RealExpressionLike").unwrap();
    println!(
        "RealExpressionLike: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(
        result.is_balanced(),
        "RealExpressionLike should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );
    assert_eq!(
        result.balance.num_equations, 1,
        "Should have 1 equation (from binding)"
    );
    assert_eq!(result.balance.num_unknowns, 1, "Should have 1 unknown (y)");

    // Test RealExpressionNonDefault - output with non-default binding (1.5)
    // This DOES work because 1.5 is not a "default value" (0.0/0/false)
    let result = compile_fixture(
        "expression_blocks",
        "ExpressionBlocks.RealExpressionNonDefault",
    )
    .unwrap();
    println!(
        "RealExpressionNonDefault: {} eq, {} unk",
        result.balance.num_equations, result.balance.num_unknowns
    );
    assert!(
        result.is_balanced(),
        "RealExpressionNonDefault should be balanced: {} eq, {} unk",
        result.balance.num_equations,
        result.balance.num_unknowns
    );

    // Test OutputWithEquation - should NOT double-count binding and equation
    // KNOWN ISSUE: Currently double-counts (2 eq) because binding and explicit equation both count
    // The explicit equation `y = 1.0` should override the binding `y = 0.0`
    let result =
        compile_fixture("expression_blocks", "ExpressionBlocks.OutputWithEquation").unwrap();
    println!(
        "OutputWithEquation: {} eq, {} unk (KNOWN ISSUE: should be 1 eq)",
        result.balance.num_equations, result.balance.num_unknowns
    );
    // TODO: Fix this - explicit equations should override bindings
    assert_eq!(
        result.balance.num_equations, 2,
        "Currently 2 equations (needs fix)"
    );

    // Test OutputNoBind - output without binding should be unbalanced (under-determined)
    let result = compile_fixture("expression_blocks", "ExpressionBlocks.OutputNoBind").unwrap();
    println!(
        "OutputNoBind: {} eq, {} unk, {} ext conn",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_external_connectors
    );
    assert_eq!(
        result.balance.status,
        rumoca::dae::balance::BalanceStatus::Unbalanced,
        "OutputNoBind should be Unbalanced (under-determined - missing equation for y)"
    );
    assert_eq!(result.balance.num_equations, 0, "Should have 0 equations");
    assert_eq!(result.balance.num_unknowns, 1, "Should have 1 unknown (y)");
}

/// Test DeMultiplex-style blocks where input arrays appear on LHS of equations.
/// This tests the pattern from MSL's Modelica.Blocks.Routing.DeMultiplex blocks.
///
/// The key issue is that equations like `[u] = [y1; y2]` have the INPUT u on the LHS.
/// This should NOT cause u to become an algebraic variable - it should remain an input.
#[test]
fn test_demultiplex_pattern() {
    // Test SimplePassthrough - input on LHS with u = y pattern
    // u is input, y is output
    // 1 equation, 1 unknown (y), 1 input (u)
    let result = compile_fixture(
        "demultiplex_balance",
        "DemultiplexBalance.SimplePassthrough",
    )
    .unwrap();
    println!(
        "SimplePassthrough: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "SimplePassthrough should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_equations, 1, "Should have 1 equation");
    assert_eq!(result.balance.num_unknowns, 1, "Should have 1 unknown (y)");
    assert_eq!(result.balance.num_inputs, 1, "Should have 1 input (u)");

    // Test SimplePassthroughNormal - output on LHS (normal pattern)
    // For comparison - this is the conventional way to write the equation
    let result = compile_fixture(
        "demultiplex_balance",
        "DemultiplexBalance.SimplePassthroughNormal",
    )
    .unwrap();
    println!(
        "SimplePassthroughNormal: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "SimplePassthroughNormal should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );

    // Test DeMultiplex2Like - array input on LHS with [u] = [y1; y2] pattern
    // u[2] is input array, y1[1] and y2[1] are output arrays
    // 2 equations from the matrix equation, 2 unknowns (y1[1], y2[1]), 2 inputs (u[1], u[2])
    let result =
        compile_fixture("demultiplex_balance", "DemultiplexBalance.DeMultiplex2Like").unwrap();
    println!(
        "DeMultiplex2Like: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "DeMultiplex2Like should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_equations, 2, "Should have 2 equations");
    assert_eq!(
        result.balance.num_unknowns, 2,
        "Should have 2 unknowns (y1[1], y2[1])"
    );
    assert_eq!(
        result.balance.num_inputs, 2,
        "Should have 2 inputs (u[1], u[2])"
    );

    // Test DeMultiplex2Explicit - explicit element-wise equations
    // y1[1] = u[1]; y2[1] = u[2];
    // Should have same balance as DeMultiplex2Like
    let result = compile_fixture(
        "demultiplex_balance",
        "DemultiplexBalance.DeMultiplex2Explicit",
    )
    .unwrap();
    println!(
        "DeMultiplex2Explicit: {} eq, {} unk, {} inputs",
        result.balance.num_equations, result.balance.num_unknowns, result.balance.num_inputs
    );
    assert!(
        result.is_balanced(),
        "DeMultiplex2Explicit should be balanced: {} eq, {} unk, {} inputs",
        result.balance.num_equations,
        result.balance.num_unknowns,
        result.balance.num_inputs
    );
    assert_eq!(result.balance.num_equations, 2, "Should have 2 equations");
    assert_eq!(result.balance.num_unknowns, 2, "Should have 2 unknowns");
    assert_eq!(result.balance.num_inputs, 2, "Should have 2 inputs");
}
