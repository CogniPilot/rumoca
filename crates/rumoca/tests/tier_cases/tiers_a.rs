use super::*;

mod tier_0_minimal {
    use super::*;

    #[test]
    fn t0_01_empty_model() {
        let source = "model Empty end Empty;";
        let r = assert_compiles(source, "Empty");
        assert!(r.is_balanced(), "Empty model should be balanced");
    }

    #[test]
    fn t0_02_single_real() {
        let source = "model SingleReal Real x; equation x = 1; end SingleReal;";
        let r = assert_compiles(source, "SingleReal");
        assert!(r.is_balanced(), "One var, one eq should be balanced");
    }

    #[test]
    fn t0_03_single_integer() {
        let source = "model SingleInteger Integer n; equation n = 1; end SingleInteger;";
        let r = assert_compiles(source, "SingleInteger");
        assert!(r.is_balanced(), "One var, one eq should be balanced");
    }

    #[test]
    fn t0_04_single_boolean() {
        let source = "model SingleBoolean Boolean b; equation b = true; end SingleBoolean;";
        let r = assert_compiles(source, "SingleBoolean");
        assert!(r.is_balanced(), "One var, one eq should be balanced");
    }

    #[test]
    fn t0_05_single_string() {
        let source = r#"model SingleString String s; equation s = "ok"; end SingleString;"#;
        let r = assert_compiles(source, "SingleString");
        assert!(r.is_balanced(), "One var, one eq should be balanced");
    }

    #[test]
    fn t0_06_multiple_reals() {
        let source =
            "model MultiReal Real x; Real y; Real z; equation x = 1; y = 2; z = 3; end MultiReal;";
        let r = assert_compiles(source, "MultiReal");
        assert!(r.is_balanced(), "Three vars, three eqs should be balanced");
    }

    #[test]
    fn t0_07_with_description() {
        let source = r#"model Described "A model with description" Real x "position"; equation x = 1; end Described;"#;
        let r = assert_compiles(source, "Described");
        assert!(r.is_balanced(), "One var, one eq should be balanced");
    }

    #[test]
    fn t0_08_unresolved_variable_fails() {
        let source = r#"
model UnresolvedVar
    Real y;
equation
    y = x + 1.0;
end UnresolvedVar;
"#;
        assert_fails(source, "UnresolvedVar", "Resolve errors");
    }

    #[test]
    fn t0_09_unresolved_function_fails() {
        let source = r#"
model UnresolvedFunc
    Real y;
equation
    y = unknownFunc(1.0);
end UnresolvedFunc;
"#;
        assert_fails(source, "UnresolvedFunc", "Resolve errors");
    }
}

// =============================================================================
// TIER 1: Basic Algebraic Equations
// =============================================================================
// Goal: Simple equations that define variables.

mod tier_1_basic_equations {
    use super::*;

    #[test]
    fn t1_01_simple_assignment() {
        let source = r#"
model SimpleAssign
    Real x;
equation
    x = 1.0;
end SimpleAssign;
"#;
        let r = assert_compiles(source, "SimpleAssign");
        assert_eq!(r.f_x_count, 1, "Should have 1 algebraic equation");
        assert!(r.is_balanced(), "One var, one eq should balance");
    }

    #[test]
    fn t1_02_two_equations() {
        let source = r#"
model TwoEq
    Real x;
    Real y;
equation
    x = 1.0;
    y = 2.0;
end TwoEq;
"#;
        let r = assert_compiles(source, "TwoEq");
        assert_eq!(r.f_x_count, 2);
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_03_addition() {
        let source = r#"
model Addition
    Real x;
    Real y;
    Real z;
equation
    x = 1.0;
    y = 2.0;
    z = x + y;
end Addition;
"#;
        let r = assert_compiles(source, "Addition");
        assert_eq!(r.f_x_count, 3);
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_04_subtraction() {
        let source = r#"
model Subtraction
    Real x;
    Real y;
equation
    x = 5.0;
    y = x - 3.0;
end Subtraction;
"#;
        let r = assert_compiles(source, "Subtraction");
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_05_multiplication() {
        let source = r#"
model Multiplication
    Real x;
    Real y;
equation
    x = 2.0;
    y = x * 3.0;
end Multiplication;
"#;
        let r = assert_compiles(source, "Multiplication");
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_06_division() {
        let source = r#"
model Division
    Real x;
    Real y;
equation
    x = 10.0;
    y = x / 2.0;
end Division;
"#;
        let r = assert_compiles(source, "Division");
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_07_complex_expression() {
        let source = r#"
model ComplexExpr
    Real a;
    Real b;
    Real c;
    Real result;
equation
    a = 1.0;
    b = 2.0;
    c = 3.0;
    result = (a + b) * c - a / b;
end ComplexExpr;
"#;
        let r = assert_compiles(source, "ComplexExpr");
        assert_eq!(r.f_x_count, 4);
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_08_unary_minus() {
        let source = r#"
model UnaryMinus
    Real x;
    Real y;
equation
    x = 5.0;
    y = -x;
end UnaryMinus;
"#;
        let r = assert_compiles(source, "UnaryMinus");
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_09_parentheses() {
        let source = r#"
model Parens
    Real x;
    Real y;
equation
    x = 2.0;
    y = (x + 1.0) * (x - 1.0);
end Parens;
"#;
        let r = assert_compiles(source, "Parens");
        assert!(r.is_balanced());
    }

    #[test]
    fn t1_10_implicit_equation() {
        // Equation not in x = expr form
        let source = r#"
model Implicit
    Real x;
    Real y;
equation
    x + y = 10.0;
    x - y = 2.0;
end Implicit;
"#;
        let r = assert_compiles(source, "Implicit");
        assert!(r.is_balanced());
    }
}

// =============================================================================
// TIER 2: Ordinary Differential Equations
// =============================================================================
// Goal: State variables with der() operator.

mod tier_2_odes {
    use super::*;

    #[test]
    fn t2_01_simple_der() {
        let source = r#"
model SimpleDer
    Real x(start = 0);
equation
    der(x) = 1.0;
end SimpleDer;
"#;
        let r = assert_compiles(source, "SimpleDer");
        assert_eq!(r.states, 1, "Should detect 1 state variable");
        assert_eq!(r.f_x_count, 1, "Should have 1 ODE equation");
        assert!(r.is_balanced());
    }

    #[test]
    fn t2_02_exponential_growth() {
        let source = r#"
model ExpGrowth
    Real x(start = 1.0);
equation
    der(x) = x;
end ExpGrowth;
"#;
        let r = assert_compiles(source, "ExpGrowth");
        assert_eq!(r.states, 1);
        assert!(r.is_balanced());
    }

    #[test]
    fn t2_03_exponential_decay() {
        let source = r#"
model ExpDecay
    Real x(start = 1.0);
equation
    der(x) = -0.1 * x;
end ExpDecay;
"#;
        let r = assert_compiles(source, "ExpDecay");
        assert_eq!(r.states, 1);
        assert!(r.is_balanced());
    }

    #[test]
    fn t2_04_two_states() {
        let source = r#"
model TwoStates
    Real x(start = 1.0);
    Real y(start = 0.0);
equation
    der(x) = -x;
    der(y) = x;
end TwoStates;
"#;
        let r = assert_compiles(source, "TwoStates");
        assert_eq!(r.states, 2, "Should detect 2 state variables");
        assert_eq!(r.f_x_count, 2);
        assert!(r.is_balanced());
    }

    #[test]
    fn t2_05_harmonic_oscillator() {
        let source = r#"
model Oscillator
    Real x(start = 1.0);
    Real v(start = 0.0);
equation
    der(x) = v;
    der(v) = -x;
end Oscillator;
"#;
        let r = assert_compiles(source, "Oscillator");
        assert_eq!(r.states, 2);
        assert_eq!(r.f_x_count, 2);
        assert!(r.is_balanced());
    }

    #[test]
    fn t2_06_damped_oscillator() {
        let source = r#"
model DampedOscillator
    Real x(start = 1.0);
    Real v(start = 0.0);
equation
    der(x) = v;
    der(v) = -x - 0.1 * v;
end DampedOscillator;
"#;
        let r = assert_compiles(source, "DampedOscillator");
        assert_eq!(r.states, 2);
        assert!(r.is_balanced());
    }

    #[test]
    fn t2_07_mixed_state_algebraic() {
        // System with both state and algebraic variables
        let source = r#"
model MixedSystem
    Real x(start = 0);
    Real y;
equation
    der(x) = y;
    y = 1.0 - x;
end MixedSystem;
"#;
        let r = assert_compiles(source, "MixedSystem");
        assert_eq!(r.states, 1, "x is a state");
        println!(
            "MixedSystem: states={}, algebraics={}",
            r.states, r.algebraics
        );
    }

    #[test]
    fn t2_08_start_value() {
        let source = r#"
model StartValue
    Real x(start = 5.0);
equation
    der(x) = -x;
end StartValue;
"#;
        let r = assert_compiles(source, "StartValue");
        // Verify start value is captured
        assert!(r.dae.states.values().any(|v| v.start.is_some()));
    }

    #[test]
    fn t2_09_second_order_as_first() {
        // x'' + x = 0 written as first-order system
        let source = r#"
model SecondOrder
    Real x(start = 1.0);
    Real v(start = 0.0);
equation
    der(x) = v;
    der(v) = -x;
end SecondOrder;
"#;
        let r = assert_compiles(source, "SecondOrder");
        assert_eq!(r.states, 2);
        assert!(r.is_balanced());
    }

    #[test]
    fn t2_10_three_body() {
        // Simple 3-variable coupled system
        let source = r#"
model ThreeBody
    Real x(start = 1);
    Real y(start = 0);
    Real z(start = 0);
equation
    der(x) = -x + y;
    der(y) = x - y + z;
    der(z) = y - z;
end ThreeBody;
"#;
        let r = assert_compiles(source, "ThreeBody");
        assert_eq!(r.states, 3);
        assert!(r.is_balanced());
    }
}

// =============================================================================
// TIER 3: Parameters and Constants
// =============================================================================
// Goal: Fixed values that parameterize the model.

mod tier_3_parameters {
    use super::*;

    #[test]
    fn t3_01_simple_parameter() {
        let source = r#"
model SimpleParam
    parameter Real k = 1.0;
    Real x;
equation
    x = k;
end SimpleParam;
"#;
        let r = assert_compiles(source, "SimpleParam");
        println!(
            "SimpleParam: params={}, algebraics={}",
            r.parameters, r.algebraics
        );
    }

    #[test]
    fn t3_02_parameter_in_ode() {
        let source = r#"
model ParamODE
    parameter Real k = 0.1;
    Real x(start = 1.0);
equation
    der(x) = -k * x;
end ParamODE;
"#;
        let r = assert_compiles(source, "ParamODE");
        assert_eq!(r.states, 1);
    }

    #[test]
    fn t3_03_multiple_parameters() {
        let source = r#"
model MultiParam
    parameter Real m = 1.0;
    parameter Real k = 100.0;
    parameter Real c = 0.5;
    Real x(start = 1);
    Real v(start = 0);
equation
    der(x) = v;
    m * der(v) = -k * x - c * v;
end MultiParam;
"#;
        let r = assert_compiles(source, "MultiParam");
        assert_eq!(r.states, 2);
    }

    #[test]
    fn t3_04_constant() {
        let source = r#"
model WithConstant
    constant Real pi = 3.14159;
    Real x;
equation
    x = 2 * pi;
end WithConstant;
"#;
        let r = assert_compiles(source, "WithConstant");
        println!("WithConstant: constants={}", r.constants);
    }

    #[test]
    fn t3_05_parameter_expression() {
        let source = r#"
model ParamExpr
    parameter Real a = 1.0;
    parameter Real b = 2.0;
    parameter Real c = a + b;
    Real x;
equation
    x = c;
end ParamExpr;
"#;
        let _r = assert_compiles(source, "ParamExpr");
    }

    #[test]
    fn t3_06_integer_parameter() {
        let source = r#"
model IntParam
    parameter Integer n = 10;
    Real x;
equation
    x = n;
end IntParam;
"#;
        let _r = assert_compiles(source, "IntParam");
    }

    #[test]
    fn t3_07_boolean_parameter() {
        let source = r#"
model BoolParam
    parameter Boolean flag = true;
    Real x;
equation
    x = 1.0;
end BoolParam;
"#;
        let _r = assert_compiles(source, "BoolParam");
    }

    #[test]
    fn t3_08_string_parameter() {
        let source = r#"
model StringParam
    parameter String name = "test";
    Real x;
equation
    x = 1.0;
end StringParam;
"#;
        let _r = assert_compiles(source, "StringParam");
    }

    #[test]
    fn t3_09_fixed_attribute() {
        let source = r#"
model FixedAttr
    Real x(start = 1.0, fixed = true);
equation
    der(x) = -x;
end FixedAttr;
"#;
        let r = assert_compiles(source, "FixedAttr");
        if let Some(state) = r.dae.states.values().next() {
            println!("FixedAttr: fixed = {:?}", state.fixed);
        }
    }

    #[test]
    fn t3_10_min_max_nominal() {
        let source = r#"
model Attributes
    Real x(min = 0, max = 100, nominal = 50);
equation
    x = 25;
end Attributes;
"#;
        let _r = assert_compiles(source, "Attributes");
    }

    #[test]
    fn t3_11_input_variable() {
        let source = r#"
model WithInput
    input Real u;
    Real x(start = 0);
equation
    der(x) = u;
end WithInput;
"#;
        let r = assert_compiles(source, "WithInput");
        println!("WithInput: inputs={}, states={}", r.inputs, r.states);
        assert_eq!(r.inputs, 1, "u should be classified as input");
        assert_eq!(r.states, 1, "x should be a state");
    }

    #[test]
    fn t3_12_output_variable() {
        let source = r#"
model WithOutput
    Real x(start = 1);
    output Real y;
equation
    der(x) = -x;
    y = x * 2;
end WithOutput;
"#;
        let r = assert_compiles(source, "WithOutput");
        println!(
            "WithOutput: outputs={}, states={}, algebraics={}",
            r.outputs, r.states, r.algebraics
        );
        assert_eq!(r.outputs, 1, "y should be classified as output");
        assert_eq!(r.states, 1, "x should be a state");
    }

    #[test]
    fn t3_13_input_output_model() {
        let source = r#"
model IOBlock
    input Real u;
    output Real y;
    parameter Real k = 2.0;
equation
    y = k * u;
end IOBlock;
"#;
        let r = assert_compiles(source, "IOBlock");
        println!(
            "IOBlock: inputs={}, outputs={}, params={}",
            r.inputs, r.outputs, r.parameters
        );
        assert_eq!(r.inputs, 1, "u should be input");
        assert_eq!(r.outputs, 1, "y should be output");
        assert_eq!(r.parameters, 1, "k should be parameter");
    }

    #[test]
    fn t3_14_constant_vs_parameter() {
        let source = r#"
model ConstVsParam
    constant Real c = 3.14159;
    parameter Real p = 2.0;
    Real x;
equation
    x = c * p;
end ConstVsParam;
"#;
        let r = assert_compiles(source, "ConstVsParam");
        println!(
            "ConstVsParam: constants={}, params={}, algebraics={}",
            r.constants, r.parameters, r.algebraics
        );
        assert_eq!(r.constants, 1, "c should be constant");
        assert_eq!(r.parameters, 1, "p should be parameter");
    }

    #[test]
    fn t3_15_discrete_variable() {
        let source = r#"
model WithDiscrete
    discrete Real d(start = 0);
    Real x(start = 0);
initial equation
    d = 0;
equation
    der(x) = 1.0;
end WithDiscrete;
"#;
        let r = assert_compiles(source, "WithDiscrete");
        println!(
            "WithDiscrete: discretes={}, states={}",
            r.discrete_reals, r.states
        );
        assert_eq!(r.discrete_reals, 1, "d should be discrete");
        assert_eq!(r.states, 1, "x should be state");
    }
}

// =============================================================================
// TIER 4: Arrays
// =============================================================================

mod tier_4_arrays {
    use super::*;

    #[test]
    fn t4_01_array_declaration() {
        let source = r#"
model ArrayDecl
    Real x[3];
equation
    x[1] = 1.0;
    x[2] = 2.0;
    x[3] = 3.0;
end ArrayDecl;
"#;
        let _r = assert_compiles(source, "ArrayDecl");
    }

    #[test]
    fn t4_02_array_parameter() {
        let source = r#"
model ArrayParam
    parameter Real k[2] = {1.0, 2.0};
    Real x;
equation
    x = k[1] + k[2];
end ArrayParam;
"#;
        let _r = assert_compiles(source, "ArrayParam");
    }

    #[test]
    fn t4_03_for_equation_with_scalar() {
        let source = r#"
model ForScalar
    Real sum;
equation
    sum = 6.0;
end ForScalar;
"#;
        let r = assert_compiles(source, "ForScalar");
        assert_eq!(r.f_x_count, 1);
        assert!(r.is_balanced());
    }

    #[test]
    fn t4_04_for_equation_expansion_basic() {
        let source = r#"
model ForExpansionBasic
    Real y[3];
equation
    for i in 1:3 loop
        y[i] = i;
    end for;
end ForExpansionBasic;
"#;
        let r = assert_compiles(source, "ForExpansionBasic");
        assert_eq!(r.f_x_count, 3, "For loop should expand to 3 equations");
        assert_eq!(r.balance, 0);
    }

    #[test]
    fn t4_05_for_equation_range_step() {
        let source = r#"
model ForRangeStep
    Real y[3];
equation
    for i in 1:2:5 loop
        y[(i + 1) / 2] = i;
    end for;
end ForRangeStep;
"#;
        let r = assert_compiles(source, "ForRangeStep");
        assert_eq!(
            r.f_x_count, 3,
            "For loop 1:2:5 should expand to 3 equations"
        );
    }

    #[test]
    fn t4_06_nested_for_equation() {
        let source = r#"
model NestedFor
    Real x[2,2];
equation
    for i in 1:2 loop
        for j in 1:2 loop
            x[i, j] = i + j;
        end for;
    end for;
end NestedFor;
"#;
        let r = assert_compiles(source, "NestedFor");
        assert_eq!(
            r.f_x_count, 4,
            "Nested for 2x2 should expand to 4 equations"
        );
    }
}

// =============================================================================
// TIER 4b: Array-Constructor Scalar Counting
// =============================================================================

mod tier_4b_array_constructors {
    use super::*;

    #[test]
    fn t4b_01_zeros_lhs_balance() {
        let source = r#"
model ZerosBalance
    Real x[3];
equation
    zeros(3) = x;
end ZerosBalance;
"#;
        let r = assert_compiles(source, "ZerosBalance");
        assert_eq!(
            r.balance, 0,
            "zeros(3) on LHS should count as 3 scalar equations"
        );
    }

    #[test]
    fn t4b_02_ones_lhs_balance() {
        let source = r#"
model OnesBalance
    Real x[3];
equation
    ones(3) = x;
end OnesBalance;
"#;
        let r = assert_compiles(source, "OnesBalance");
        assert_eq!(
            r.balance, 0,
            "ones(3) on LHS should count as 3 scalar equations"
        );
    }

    #[test]
    fn t4b_03_identity_lhs_balance() {
        let source = r#"
model IdentityBalance
    Real A[2,2];
equation
    identity(2) = A;
end IdentityBalance;
"#;
        let r = assert_compiles(source, "IdentityBalance");
        assert_eq!(
            r.balance, 0,
            "identity(2) on LHS should count as 4 scalar equations"
        );
    }

    #[test]
    fn t4b_04_fill_lhs_balance() {
        let source = r#"
model FillBalance
    Real x[2,3];
equation
    fill(0.0, 2, 3) = x;
end FillBalance;
"#;
        let r = assert_compiles(source, "FillBalance");
        assert_eq!(
            r.balance, 0,
            "fill(0,2,3) on LHS should count as 6 scalar equations"
        );
    }

    #[test]
    fn t4b_05_cross_lhs_balance() {
        let source = r#"
model CrossBalance
    Real a[3], b[3], c[3];
equation
    cross(a, b) = c;
    a[1] = 1; a[2] = 0; a[3] = 0;
    b[1] = 0; b[2] = 1; b[3] = 0;
end CrossBalance;
"#;
        let r = assert_compiles(source, "CrossBalance");
        assert_eq!(
            r.balance, 0,
            "cross(a,b) on LHS should count as 3 scalar equations"
        );
    }

    /// MLS §10.1: Equations involving only zero-sized array variables should be
    /// eliminated (scalar_count = 0). When n=0, Real[n] arrays have no scalars,
    /// so `y = u` where both are Real[0] contributes 0 equations.
    #[test]
    fn t4b_06_zero_sized_array_equation() {
        let source = r#"
model ZeroSizedEquation
    parameter Integer n = 0;
    Real y[n];
    Real u[n];
equation
    y = u;
end ZeroSizedEquation;
"#;
        let r = assert_compiles(source, "ZeroSizedEquation");
        assert_eq!(
            r.f_x_count, 0,
            "zero-sized array equation should be eliminated"
        );
        assert_eq!(r.balance, 0, "zero-sized array model should be balanced");
    }

    /// MLS §10.1: Equations where the LHS targets a zero-sized array variable
    /// should be eliminated, even when the RHS contains existing (non-eliminated)
    /// variables. E.g., `port_a.T = fill(port_b.T, 0, 0)` where port_a is Real[0,0].
    #[test]
    fn t4b_07_zero_sized_lhs_with_existing_rhs() {
        let source = r#"
model ZeroSizedLhs
    parameter Integer n = 0;
    Real y[n];
    Real x;
equation
    y = fill(x, n);
    x = 1.0;
end ZeroSizedLhs;
"#;
        let r = assert_compiles(source, "ZeroSizedLhs");
        assert_eq!(
            r.f_x_count, 1,
            "only x=1.0 should remain; y=fill(x,0) is zero-sized"
        );
        assert_eq!(
            r.balance, 0,
            "model with zero-sized LHS equation should be balanced"
        );
    }

    /// Equations with VarRef names containing evaluable integer arithmetic subscripts
    /// (e.g., `pc[((2 * 1) - 1)].i`) must not be incorrectly eliminated as zero-sized.
    /// These arise from for-loop expansion where arithmetic wasn't simplified.
    /// The VarRefs ARE present in flat.variables under their evaluated names (e.g., `pc[1].i`).
    #[test]
    fn t4b_08_arithmetic_subscript_equations() {
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

model ArithSubscript
    parameter Integer N = 1;
    Pin p;
    Pin n;
    Pin pc[2*N];
    Real control[N];
equation
    p.i + n.i = 0;
    for i in 1:N loop
        pc[2*i-1].i + pc[2*i].i = 0;
        pc[2*i-1].v - pc[2*i].v = 0;
        control[i] = pc[2*i-1].i;
    end for;
    p.v - n.v = sum(control);
end ArithSubscript;
"#;
        let r = assert_compiles(source, "ArithSubscript");
        assert_eq!(
            r.balance, 0,
            "equations with arithmetic subscripts should not be eliminated as zero-sized"
        );
    }

    #[test]
    fn t4b_09_var_dims_fallback() {
        let source = r#"
model VarDimsBalance
    Real x[3];
    Real y[3];
equation
    x + y = {1, 2, 3};
    x = {1, 2, 3};
end VarDimsBalance;
"#;
        let r = assert_compiles(source, "VarDimsBalance");
        assert_eq!(
            r.balance, 0,
            "VarRefs with known dims should infer correct scalar count"
        );
    }

    #[test]
    fn t4b_09_zero_sized_array_varrefs() {
        let source = r#"
model ZeroSizedArrayVarRefs
    constant Integer nXi = 0;
    Real[nXi] mXi_flow_a;
    Real[nXi] mXi_flow_b;
    Real x;
equation
    mXi_flow_a + mXi_flow_b = zeros(nXi);
    x = 1.0;
end ZeroSizedArrayVarRefs;
"#;
        let r = assert_compiles(source, "ZeroSizedArrayVarRefs");
        assert_eq!(
            r.balance, 0,
            "Zero-sized array VarRefs (Real[0]) should infer scalar_count=0"
        );
    }

    #[test]
    fn t4b_10_input_equation_with_function_call_unknowns() {
        // When an input variable has an equation `h = func(T)` where T is an
        // algebraic unknown, the equation must be kept — it's T's only constraint.
        // Previously, is_input_default_equation() matched any FunctionCall on RHS
        // and incorrectly skipped such equations (caused balance=-1 in Media models).
        let source = r#"
function specificEnthalpy
    input Real T;
    output Real h;
algorithm
    h := 4186.0 * T;
end specificEnthalpy;

model InputFunctionCallConstraint
    input Real h;
    Real T;
equation
    h = specificEnthalpy(T);
end InputFunctionCallConstraint;
"#;
        let r = assert_compiles(source, "InputFunctionCallConstraint");
        assert_eq!(
            r.balance, 0,
            "Input equation with function call referencing unknowns should be kept"
        );
    }
}

// =============================================================================
// TIER 5: Conditionals
// =============================================================================

mod tier_5_conditionals {
    use super::*;

    fn condition_observer_count(dae: &rumoca_ir_dae::Dae) -> usize {
        dae.relation.len() + dae.synthetic_root_conditions.len()
    }

    #[test]
    fn t5_01_if_expression() {
        let source = r#"
model IfExpr
    Real x;
    Real y;
equation
    x = 5.0;
    y = if x > 0 then 1.0 else -1.0;
end IfExpr;
"#;
        let _r = assert_compiles(source, "IfExpr");
    }

    #[test]
    fn t5_02_if_equation() {
        let source = r#"
model IfEq
    Boolean cond = true;
    Real x;
equation
    if cond then
        x = 1.0;
    else
        x = 0.0;
    end if;
end IfEq;
"#;
        let r = assert_compiles(source, "IfEq");
        // The condition is compile-time constant (`cond = true`), so the
        // conditional branch folds to a single algebraic assignment.
        assert_eq!(
            r.f_x_count, 1,
            "Constant condition should fold to one algebraic equation"
        );
    }

    #[test]
    fn t5_03_if_equation_multiple() {
        let source = r#"
model IfEqMultiple
    Boolean cond = true;
    Real x;
    Real y;
equation
    if cond then
        x = 1.0;
        y = 2.0;
    else
        x = 0.0;
        y = 0.0;
    end if;
end IfEqMultiple;
"#;
        let r = assert_compiles(source, "IfEqMultiple");
        // The condition is compile-time constant (`cond = true`), so both
        // equations in the taken branch remain and the untaken branch is dropped.
        assert_eq!(
            r.f_x_count, 2,
            "Constant condition should keep only the taken branch equations"
        );
    }

    #[test]
    fn t5_04_if_equation_elseif() {
        let source = r#"
model IfEqElseif
    Integer n = 2;
    Real x;
equation
    if n == 1 then
        x = 1.0;
    elseif n == 2 then
        x = 2.0;
    else
        x = 0.0;
    end if;
end IfEqElseif;
"#;
        let r = assert_compiles(source, "IfEqElseif");
        // `n = 2` is compile-time constant, so the `elseif n == 2` branch is selected.
        assert_eq!(
            r.f_x_count, 1,
            "Constant elseif chain should fold to one algebraic equation"
        );
    }

    #[test]
    fn t5_05_when_equation() {
        let source = r#"
model WhenEq
    Real x(start = 0);
    discrete Real y(start = 0);
equation
    der(x) = 1.0;
    when x > 1.0 then
        y = pre(y) + 1;
    end when;
end WhenEq;
"#;
        let r = assert_compiles(source, "WhenEq");
        assert_eq!(r.states, 1, "Should have 1 state (x)");
        assert_eq!(r.discrete_reals, 1, "Should have 1 discrete (y)");
        assert_eq!(r.f_x_count, 1, "Should have 1 ODE equation");
        assert_eq!(r.dae.relation.len(), 1, "Should have 1 when condition");
    }

    #[test]
    fn t5_06_when_simple_assignment() {
        let source = r#"
model WhenSimple
    Real x(start = 0);
    discrete Real d(start = 0);
equation
    der(x) = 1.0;
    when x > 2.0 then
        d = 10.0;
    end when;
end WhenSimple;
"#;
        let r = assert_compiles(source, "WhenSimple");
        assert_eq!(r.states, 1, "Should have 1 state (x)");
        assert_eq!(r.discrete_reals, 1, "Should have 1 discrete (d)");
        assert_eq!(r.dae.relation.len(), 1, "Should have 1 when condition");
        assert_eq!(
            r.dae.f_z.len() + r.dae.f_m.len(),
            1,
            "when condition should lower to one event-partition equation"
        );
    }

    #[test]
    fn t5_07_nested_when_error() {
        let source = r#"
model NestedWhen
    Real x(start = 0);
    discrete Real d(start = 0);
equation
    der(x) = 1.0;
    when x > 1 then
        when x > 2 then
            d = 1;
        end when;
    end when;
end NestedWhen;
"#;
        match compile(source, "NestedWhen") {
            Ok(_) => panic!("Nested when-equations should fail"),
            Err(e) => {
                assert!(
                    e.contains("nested") || e.contains("Flatten"),
                    "Error should mention nested when-equations: {}",
                    e
                );
            }
        }
    }

    #[test]
    fn t5_08_when_in_initial_error() {
        let source = r#"
model WhenInitial
    Real x(start = 0);
    discrete Real d(start = 0);
initial equation
    when x > 1 then
        d = 1;
    end when;
equation
    der(x) = 1.0;
end WhenInitial;
"#;
        match compile(source, "WhenInitial") {
            Ok(_) => panic!("When-equations in initial section should fail"),
            Err(e) => {
                assert!(
                    e.contains("initial") || e.contains("Flatten"),
                    "Error should mention initial equations: {}",
                    e
                );
            }
        }
    }

    #[test]
    fn t5_09_when_with_reinit() {
        let source = r#"
model BouncingBall
    Real h(start = 1) "height";
    Real v(start = 0) "velocity";
    parameter Real g = 9.81;
    parameter Real e = 0.8 "restitution coefficient";
equation
    der(h) = v;
    der(v) = -g;
    when h < 0 then
        reinit(v, -e * pre(v));
    end when;
end BouncingBall;
"#;
        let r = assert_compiles(source, "BouncingBall");
        assert_eq!(r.states, 2, "Should have 2 states (h, v)");
        assert_eq!(r.f_x_count, 2, "Should have 2 ODE equations");
        assert_eq!(r.dae.relation.len(), 1, "Should have 1 when condition");
        assert_eq!(
            r.dae.f_z.len() + r.dae.f_m.len(),
            1,
            "reinit when should lower to one event-partition equation"
        );
    }

    #[test]
    fn t5_10_pre_in_when_assignment() {
        let source = r#"
model PreInWhen
    Real x(start = 0);
    discrete Real d(start = 0);
equation
    der(x) = 1;
    when x > 1 then
        d = pre(d) + 1;
    end when;
end PreInWhen;
"#;
        let r = assert_compiles(source, "PreInWhen");
        assert_eq!(r.states, 1, "Should have 1 state (x)");
        assert_eq!(r.dae.relation.len(), 1, "Should have 1 when condition");
        assert_eq!(
            r.dae.f_z.len() + r.dae.f_m.len(),
            1,
            "when condition should lower to one event-partition equation"
        );
    }

    #[test]
    fn t5_11_pre_discrete_in_continuous_equation() {
        let source = r#"
model PreDiscreteInContinuous
    Real x(start = 0);
    discrete Integer mode(start = 0);
equation
    der(x) = if pre(mode) == 0 then 1 else -1;
    when x > 1 then
        mode = 1;
    end when;
end PreDiscreteInContinuous;
"#;
        let r = compile(source, "PreDiscreteInContinuous")
            .expect("pre() on discrete var in continuous eq should compile");
        assert_eq!(r.states, 1, "Should have 1 state (x)");
        assert_eq!(
            r.discrete_reals, 0,
            "Integer discrete should not be counted as z"
        );
        assert_eq!(
            r.discrete_valued, 1,
            "Integer discrete should be counted as m"
        );
    }

    #[test]
    fn t5_12_pre_in_regular_equation_allowed() {
        let source = r#"
model PreInRegularEquation
    Real x(start = 1);
    discrete Real y(start = 0);
equation
    der(x) = -x;
    when x < 0.5 then
        y = pre(y) + 1;
    end when;
end PreInRegularEquation;
"#;
        let r =
            compile(source, "PreInRegularEquation").expect("pre() in when-clause should compile");
        assert_eq!(r.states, 1, "Should have 1 state (x)");
        assert_eq!(r.discrete_reals, 1, "Should have 1 discrete (y)");
    }

    #[test]
    fn t5_13_reinit_non_state_error() {
        let source = r#"
model ReinitNonState
    Real x(start = 1);
    Real y;
equation
    der(x) = -x;
    y = 2 * x;
    when x < 0.5 then
        reinit(y, 0);
    end when;
end ReinitNonState;
"#;
        match compile(source, "ReinitNonState") {
            Ok(_) => panic!("reinit() on non-state variable should fail"),
            Err(e) => {
                assert!(
                    e.contains("state") || e.contains("ToDae"),
                    "Error should mention state variable requirement: {}",
                    e
                );
            }
        }
    }

    #[test]
    fn t5_14_edge_in_when_condition() {
        let source = r#"
model EdgeInWhen
    Real x(start = 0);
    Boolean b(start = false);
    discrete Real d(start = 0);
equation
    der(x) = 1;
    b = x > 1;
    when edge(b) then
        d = pre(d) + 1;
    end when;
end EdgeInWhen;
"#;
        let r = assert_compiles(source, "EdgeInWhen");
        assert_eq!(r.states, 1, "Should have 1 state (x)");
        assert_eq!(r.dae.relation.len(), 1, "Should have 1 when condition");
    }

    #[test]
    fn t5_15_change_in_when_condition() {
        let source = r#"
model ChangeInWhen
    Real x(start = 0);
    Integer i(start = 0);
    discrete Real d(start = 0);
equation
    der(x) = 1;
    i = if x > 1 then 1 else 0;
    when change(i) then
        d = pre(d) + 1;
    end when;
end ChangeInWhen;
"#;
        let r = assert_compiles(source, "ChangeInWhen");
        assert_eq!(r.states, 1, "Should have 1 state (x)");
        assert!(
            condition_observer_count(&r.dae) >= 1,
            "Should have at least one condition observer for when/change"
        );
    }

    #[test]
    fn t5_16_edge_outside_when_error() {
        let source = r#"
model EdgeOutsideWhen
    Boolean b;
    Real y;
equation
    b = true;
    y = if edge(b) then 1 else 0;
end EdgeOutsideWhen;
"#;
        assert_compiles(source, "EdgeOutsideWhen");
    }

    #[test]
    fn t5_17_change_outside_when_error() {
        let source = r#"
model ChangeOutsideWhen
    Integer i;
    Real y;
equation
    i = 1;
    y = if change(i) then 1 else 0;
end ChangeOutsideWhen;
"#;
        assert_compiles(source, "ChangeOutsideWhen");
    }

    /// When-equation with for-loop using parameter range.
    /// The for-loop `for i in 1:n loop` inside a when-equation must be expanded
    /// using the parameter value. Previously, this was silently dropped when the
    /// range couldn't be evaluated.
    #[test]
    fn t5_18_when_for_parameter_range() {
        let source = r#"
model WhenForParam
    parameter Integer n = 2;
    discrete Real x[n](each start = 0);
    input Integer idx;
equation
    when change(idx) then
        for i in 1:n loop
            x[i] = if i == idx then 1.0 else 0.0;
        end for;
    end when;
end WhenForParam;
"#;
        let r = assert_compiles(source, "WhenForParam");
        assert!(
            condition_observer_count(&r.dae) >= 1,
            "Should have at least one condition observer for when/change"
        );
        assert_eq!(
            r.dae.f_z.len() + r.dae.f_m.len(),
            2,
            "when loop should lower to two event-partition equations"
        );
    }

    /// When-equation with for-loop where the parameter range comes from
    /// an inherited class (extends). Tests that parameter values from
    /// parent classes are available for when-for range evaluation.
    #[test]
    fn t5_19_when_for_inherited_parameter_range() {
        let source = r#"
partial block BaseBlock
    parameter Integer nin = 1 "Number of inputs";
    input Real u[nin];
    output Real y;
end BaseBlock;

block Extractor
    extends BaseBlock;
    discrete Real k[nin](each start = 0);
    input Integer index;
equation
    when {initial(), change(index)} then
        for i in 1:nin loop
            k[i] = if index == i then 1.0 else 0.0;
        end for;
    end when;
    y = k * u;
end Extractor;
"#;
        let r = assert_compiles(source, "Extractor");
        assert!(
            condition_observer_count(&r.dae) >= 1,
            "Should have at least one condition observer for when"
        );
        assert_eq!(
            r.dae.f_z.len() + r.dae.f_m.len(),
            1,
            "when loop should lower to one event-partition equation (nin=1)"
        );
    }

    /// When-equation targeting record variables should classify fields as discrete.
    /// When k[i] = RecordConstructor(...) appears in a when-clause and k is a
    /// record type (expanded to k[1].re, k[1].im), the field variables must be
    /// classified as discrete to avoid counting them as algebraic unknowns.
    #[test]
    fn t5_21_if_equation_dynamic_boolean_condition() {
        // Boolean variable with start=false defined by equation (not binding)
        // should NOT have its start value used for compile-time branch selection.
        // This models the CCCVcharging pattern where:
        //   Boolean CV(start=false, fixed=true);
        //   equation CV = v >= Vend;
        //   if CV then v=Vend; else i=-I*ramp; end if;
        let source = r#"
model DynamicBoolIf
    parameter Real threshold = 5.0;
    Real x;
    Real y;
    Boolean flag(start = false);
equation
    der(x) = 1.0;
    flag = x >= threshold;
    if flag then
        y = threshold;
    else
        y = x;
    end if;
end DynamicBoolIf;
"#;
        let r = assert_compiles(source, "DynamicBoolIf");
        assert_eq!(r.balance, 0);
        // The if-equation must be preserved as a conditional equation, NOT resolved
        // at compile time using the start value (false). Check that at least one
        // equation in f_x contains an If expression (may be nested inside Binary residual).
        let has_conditional = r.dae.f_x.iter().any(|eq| contains_if_expr(&eq.rhs));
        assert!(
            has_conditional,
            "If-equation with dynamic Boolean condition must be preserved, not resolved using start value"
        );
    }

    #[test]
    fn t5_22_cardinality_if_equation_branch_selection() {
        // When cardinality(c) >= 2, default equations guarded by
        // `if cardinality(c) < 2 then default_eq` should be eliminated.
        // This models the StateGraph CompositeStep pattern where
        // inPort/outPort have default equations for unconnected ports.
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

model Comp
    Pin p;
    Pin n;
    Real extra;
equation
    // Default equation for extra when p is not connected (cardinality < 2).
    // When p IS connected, the connection provides the constraint, so no default needed.
    if cardinality(p) < 2 then
        extra = 0.0;
    else
        extra = p.v;
    end if;
    p.v - n.v = 1.0;
    p.i + n.i = 0;
end Comp;

model TestCardinality
    Comp c1;
    Comp c2;
equation
    connect(c1.n, c2.p);
end TestCardinality;
"#;
        let r = assert_compiles(source, "TestCardinality");
        // c1.n and c2.p each have cardinality=1 (one connect statement).
        // c1.p and c2.n each have cardinality=0 (not connected).
        // cardinality(p) in Comp: c1.p has cardinality 0 → `< 2` is true → extra = 0.0
        //                         c2.p has cardinality 1 → `< 2` is true → extra = 0.0
        // Both branches select the `then` clause, so extra = 0.0 (no if-equation in DAE).
        // Without cardinality evaluation: the if-equation stays, adding an unresolved conditional.
        assert_eq!(
            r.balance, 0,
            "cardinality() should be evaluated at compile time for branch selection"
        );
        // Verify the if-equation was resolved at compile time (no If in DAE equations)
        let has_conditional = r.dae.f_x.iter().any(|eq| contains_if_expr(&eq.rhs));
        assert!(
            !has_conditional,
            "cardinality() if-equations should be resolved at compile time, not left as conditionals"
        );
    }

    #[test]
    fn t5_20_when_record_field_discrete_classification() {
        let source = r#"
record Cpx
    Real re;
    Real im;
end Cpx;

model WhenRecord
    Cpx k(re(start = 0), im(start = 0));
    input Integer idx;
    output Real y;
equation
    when change(idx) then
        k = Cpx(1.0, 0.0);
    end when;
    y = k.re;
end WhenRecord;
"#;
        let r = assert_compiles(source, "WhenRecord");
        // k.re and k.im should be discrete (assigned in when-clause)
        // y should be output
        // 1 equation: y = k.re
        // Unknowns: y (output)
        // Balance: 1 - 1 = 0
        assert_eq!(
            r.balance, 0,
            "Record fields assigned in when-clause should be discrete, not algebraic"
        );
    }

    /// Conditional assignments inside when-equations must be preserved in DAE.
    /// MLS §8.3.5 allows if-equations in when-clauses; dropping them silently
    /// causes incorrect event behavior and under-counted active discrete updates.
    #[test]
    fn t5_23_when_if_equation_preserved_in_dae() {
        let source = r#"
model WhenIfPreserved
    Real x(start = 0);
    discrete Real a(start = 0);
    discrete Real b(start = 0);
equation
    der(x) = 1;
    when sample(0, 1) then
        if x > 0.5 then
            a = 1;
            b = 2;
        else
            a = 3;
            b = 4;
        end if;
    end when;
end WhenIfPreserved;
"#;
        let r = assert_compiles(source, "WhenIfPreserved");
        assert!(
            condition_observer_count(&r.dae) >= 1,
            "Should have at least one condition observer for when/sample"
        );
        assert_eq!(
            r.dae.f_z.len() + r.dae.f_m.len(),
            2,
            "conditional when-equation should produce one preserved assignment per target"
        );

        let mut targets = r
            .dae
            .f_z
            .iter()
            .chain(r.dae.f_m.iter())
            .filter_map(|eq| eq.lhs.as_ref().map(|name| name.as_str().to_string()))
            .collect::<Vec<_>>();
        targets.sort();
        assert_eq!(targets, vec!["a", "b"]);
        assert!(
            r.dae
                .f_z
                .iter()
                .chain(r.dae.f_m.iter())
                .all(|eq| contains_if_expr(&eq.rhs)),
            "When conditional assignments should be lowered to If expressions in DAE RHS"
        );
    }

    /// If-equations without an else branch in when-clauses must keep previous
    /// value semantics for non-taken branches (`pre(x)` fallback).
    #[test]
    fn t5_24_when_if_without_else_uses_pre_fallback() {
        let source = r#"
model WhenIfNoElse
    Real x(start = 0);
    discrete Real d(start = 0);
equation
    der(x) = 1;
    when sample(0, 1) then
        if x > 0.5 then
            d = pre(d) + 1;
        end if;
    end when;
end WhenIfNoElse;
"#;
        let r = assert_compiles(source, "WhenIfNoElse");
        assert!(
            condition_observer_count(&r.dae) >= 1,
            "Should have at least one condition observer for when/sample"
        );
        assert_eq!(
            r.dae.f_z.len() + r.dae.f_m.len(),
            1,
            "single-target when-if should produce one preserved assignment"
        );
        let eq = r
            .dae
            .f_z
            .iter()
            .chain(r.dae.f_m.iter())
            .next()
            .expect("expected one event equation");
        assert!(
            contains_if_expr(&eq.rhs),
            "when-if must remain conditional in DAE"
        );
        assert!(
            contains_pre_expr(&eq.rhs),
            "when-if without else must include pre() fallback for non-taken branch"
        );
    }
}

// =============================================================================
// TIER 6: Functions
// =============================================================================
