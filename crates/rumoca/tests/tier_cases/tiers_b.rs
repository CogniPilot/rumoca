use super::*;

mod tier_6_functions {
    use super::*;

    #[test]
    fn t6_01_sin_cos() {
        let source = r#"
model SinCos
    Real x;
    Real y;
    Real t(start = 0);
equation
    der(t) = 1.0;
    x = sin(t);
    y = cos(t);
end SinCos;
"#;
        let _r = assert_compiles(source, "SinCos");
    }

    #[test]
    fn t6_02_sqrt_abs() {
        let source = r#"
model SqrtAbs
    Real x;
    Real y;
equation
    x = 4.0;
    y = sqrt(abs(x));
end SqrtAbs;
"#;
        let _r = assert_compiles(source, "SqrtAbs");
    }
}

/// Test compile-time evaluation of user-defined functions bound to Real parameters.
/// Models the numberOfSymmetricBaseSystems(m) pattern from MSL Polyphase.
/// The function returns Integer but is assigned to a Real parameter that
/// determines array sizes.
#[test]
fn t6_03_user_func_real_param_array_dim() {
    let source = r#"
function numSystems
    input Integer m;
    output Integer n;
algorithm
    if mod(m, 2) == 0 then
        if m == 2 then
            n := 1;
        else
            n := 2 * numSystems(integer(m / 2));
        end if;
    else
        n := 1;
    end if;
end numSystems;

model PolyphaseModel
    parameter Real m = 3;
    parameter Real mSystems = numSystems(integer(m));
    parameter Real mBasic = integer(m / mSystems);
    Real[integer(mBasic)] x;
equation
    for i in 1:integer(mBasic) loop
        x[i] = 0;
    end for;
end PolyphaseModel;
"#;
    let r = assert_compiles(source, "PolyphaseModel");
    assert_eq!(
        r.balance, 0,
        "Real params from user-defined function should be evaluated at compile time"
    );
}

#[test]
fn t6_04_real_param_for_loop_range() {
    let source = r#"
model ForLoopRealRange
    parameter Real n = 3;
    Real[integer(n)] x;
equation
    for i in 1:integer(n) loop
        x[i] = i;
    end for;
end ForLoopRealRange;
"#;
    let r = assert_compiles(source, "ForLoopRealRange");
    assert_eq!(
        r.balance, 0,
        "Real parameter used as for-loop range via integer() should compile and balance"
    );
}

// =============================================================================
// TIER 7: Components and Connectors
// =============================================================================

mod tier_7_components {
    use super::*;

    #[test]
    fn t7_01_simple_component() {
        let source = r#"
model Source
    Real y;
equation
    y = 1.0;
end Source;

model UseSource
    Source s;
    Real x;
equation
    x = s.y;
end UseSource;
"#;
        let _r = assert_compiles(source, "UseSource");
    }

    #[test]
    fn t7_02_component_start_modification() {
        let source = r#"
model Inner
    Real x(start = 5);
equation
    der(x) = -x;
end Inner;

model Outer
    Inner sub(x(start = 10));
end Outer;
"#;
        let r = assert_compiles(source, "Outer");
        assert_eq!(r.states, 1, "Expected 1 state variable (sub.x)");
        let state = r.dae.states.values().next().expect("Should have a state");
        assert!(state.start.is_some(), "State should have a start value");
        if let Some(ref start_expr) = state.start {
            let start_str = format!("{:?}", start_expr);
            assert!(
                start_str.contains("10"),
                "Start value should be 10 from outer modification, got: {}",
                start_str
            );
        }
    }

    #[test]
    fn t7_03_nested_component_modification() {
        let source = r#"
model Level2
    Real x(start = 1);
equation
    der(x) = -x;
end Level2;

model Level1
    Level2 l2;
end Level1;

model Top
    Level1 l1(l2(x(start = 100)));
end Top;
"#;
        let r = assert_compiles(source, "Top");
        assert_eq!(r.states, 1, "Expected 1 state variable");
        let state = r.dae.states.values().next().expect("Should have a state");
        if let Some(ref start_expr) = state.start {
            let start_str = format!("{:?}", start_expr);
            assert!(
                start_str.contains("100"),
                "Start value should be 100 from top-level modification, got: {}",
                start_str
            );
        }
    }

    #[test]
    fn t7_04_parameter_modification() {
        let source = r#"
model Parameterized
    parameter Real k = 1.0;
    Real x(start = 0);
equation
    der(x) = k;
end Parameterized;

model UseParameterized
    Parameterized p(k = 5.0);
end UseParameterized;
"#;
        let r = assert_compiles(source, "UseParameterized");
        assert_eq!(r.states, 1, "Expected 1 state variable");
        assert_eq!(r.parameters, 1, "Expected 1 parameter");
        let param = r
            .dae
            .parameters
            .values()
            .next()
            .expect("Should have a parameter");
        if let Some(ref start) = param.start {
            let start_str = format!("{:?}", start);
            assert!(
                start_str.contains("5"),
                "Parameter k should have value 5.0 from modification, got: {}",
                start_str
            );
        }
    }

    #[test]
    fn t7_05_modification_precedence() {
        let source = r#"
model Base
    Real x(start = 1);
equation
    der(x) = -x;
end Base;

model Middle
    Base b(x(start = 10));
end Middle;

model Top
    Middle m(b(x(start = 100)));
end Top;
"#;
        let r = assert_compiles(source, "Top");
        let state = r.dae.states.values().next().expect("Should have a state");
        if let Some(ref start_expr) = state.start {
            let start_str = format!("{:?}", start_expr);
            assert!(
                start_str.contains("100"),
                "Outermost modification (100) should take precedence, got: {}",
                start_str
            );
        }
    }

    /// Test modification with both nested attribute mods AND a binding value.
    /// MLS §7.2: `field(start=X) = expr` should set both the start attribute
    /// and the binding for the field component.
    /// Bug: process_mod_arg didn't handle Binary { Assign, ClassModification, rhs },
    /// so the entire modification (start AND binding) was silently dropped.
    #[test]
    fn t7_06_nested_modification_with_binding() {
        let source = r#"
record FluidProps
    Real cp;
    Real m_flow;
end FluidProps;

model Source
    Real m_flow_val;
    FluidProps props(cp = 1007, m_flow(start = 0.88) = m_flow_val);
    output Real y;
equation
    m_flow_val = 1.0;
    y = props.m_flow;
end Source;
"#;
        let r = assert_compiles(source, "Source");
        assert_eq!(
            r.balance, 0,
            "Field with nested mod + binding should produce correct equation count"
        );
        // props.m_flow should have an equation (from the binding = m_flow_val)
        // props.cp should have an equation (from the binding = 1007)
        // m_flow_val has an equation, y has an equation
        // Total: 4 equations, 4 unknowns (m_flow_val, props.cp, props.m_flow, y)
    }

    #[test]
    fn t7_07_nested_component_param_for_loop() {
        // Tests that nested component modifications with function-computed
        // parameters work correctly for for-loop range evaluation.
        // The inner Star(m=m) modification creates a binding referencing
        // the outer scope, which must be resolvable at compile time.
        // The key test is that this COMPILES (the for-loop range can be evaluated).
        let source = r#"
function numSys
    input Integer m;
    output Integer n;
algorithm
    if mod(m, 2) == 0 then n := 2;
    else n := 1;
    end if;
end numSys;

model Star
    parameter Integer m = 3;
    final parameter Integer mSystems = numSys(m);
    Real[mSystems] v;
equation
    for k in 1:mSystems loop
        v[k] = 0;
    end for;
end Star;

model Top
    parameter Integer m = 6;
    Star star(m=m);
end Top;
"#;
        // The key assertion is that this compiles at all — the for-loop range
        // evaluation must resolve star.mSystems through function call evaluation
        // with nested modification scope resolution.
        let r = assert_compiles(source, "Top");
        assert_eq!(
            r.balance, 0,
            "Nested component with function-computed param for-loop range should balance"
        );
    }
}

// =============================================================================
// TIER 8: Inheritance
// =============================================================================

mod tier_8_inheritance {
    use super::*;

    #[test]
    fn t8_01_simple_extends() {
        let source = r#"
model Base
    Real x;
equation
    x = 1.0;
end Base;

model Derived
    extends Base;
    Real y;
equation
    y = x + 1;
end Derived;
"#;
        let _r = assert_compiles(source, "Derived");
    }
}

// =============================================================================
// TIER 9: Advanced Features
// =============================================================================

mod tier_9_advanced {
    use super::*;

    #[test]
    fn t9_01_algorithm_section() {
        let source = r#"
model WithAlgorithm
    Real x;
    Real y;
algorithm
    x := 1.0;
    y := x + 1;
end WithAlgorithm;
"#;
        let _r = assert_compiles(source, "WithAlgorithm");
    }
}

// =============================================================================
// TIER 10: Balance Regression Tests
// =============================================================================

mod tier_10_balance_regressions {
    use super::*;

    /// Test that sum() of an array variable doesn't inflate scalar equation count.
    /// MLS §10.3.4: sum() is a reduction operator that produces a scalar from an array.
    /// Bug: `m * i0 = sum(i)` where i is Real[3] was counted as 3 scalar equations
    /// because infer_scalar_count_from_varrefs saw i with dims=[3].
    #[test]
    fn t10_01_sum_reduction_scalar_count() {
        let source = r#"
model SumReduction
    parameter Integer m = 3;
    Real[m] i;
    Real i0;
equation
    m * i0 = sum(i);
    for k in 1:m loop
        i[k] = k * 1.0;
    end for;
end SumReduction;
"#;
        let r = assert_compiles(source, "SumReduction");
        for (idx, eq) in r.dae.f_x.iter().enumerate() {
            println!(
                "  f_x[{}] scalar_count={}: {}",
                idx, eq.scalar_count, eq.origin
            );
        }
        assert_eq!(
            r.balance, 0,
            "sum() reduction should count as 1 scalar equation, not array size"
        );
    }

    /// Test that parameter array VarRefs don't inflate equation scalar count.
    /// MLS §4.7: Parameters are not unknowns, so their dimensions should not
    /// determine the equation's scalar count.
    /// Bug: `y = TransformationMatrix * u` where TransformationMatrix[2,3] is a
    /// parameter was counted as 6 scalar equations instead of 2.
    #[test]
    fn t10_02_parameter_array_scalar_count() {
        let source = r#"
model MatrixTransform
    parameter Real[2,3] T = {{1,0,0},{0,1,0}};
    input Real[3] u;
    output Real[2] y;
equation
    y = T * u;
end MatrixTransform;
"#;
        let r = assert_compiles(source, "MatrixTransform");
        for (idx, eq) in r.dae.f_x.iter().enumerate() {
            println!(
                "  f_x[{}] scalar_count={}: {}",
                idx, eq.scalar_count, eq.origin
            );
        }
        // y = T * u should be 2 scalar equations (y has 2 elements), not 6 (T has 6 elements)
        assert_eq!(
            r.balance, 0,
            "parameter array should not inflate scalar count"
        );
    }

    /// Test that product() of an array variable doesn't inflate scalar count.
    #[test]
    fn t10_03_product_reduction_scalar_count() {
        let source = r#"
model ProductReduction
    parameter Integer n = 4;
    Real[n] x;
    Real p;
equation
    p = product(x);
    for k in 1:n loop
        x[k] = k * 0.5;
    end for;
end ProductReduction;
"#;
        let r = assert_compiles(source, "ProductReduction");
        assert_eq!(
            r.balance, 0,
            "product() reduction should count as 1 scalar equation"
        );
    }

    /// Test: multi-dimensional array subscript scalar count
    ///
    /// For equations like `T[1,1] = expr` where T is Real[M,N],
    /// the scalar count should be 1 (fully subscripted element access).
    /// Bug: count_embedded_subscripts("[1,1]") was counting brackets (1) not indices (2).
    #[test]
    fn t10_04_multidim_subscript_scalar_count() {
        let source = r#"
model MultiDimSubscript
    parameter Integer m = 2;
    parameter Integer n = 3;
    Real[m, n] T;
equation
    T[1,1] = 1.0;
    T[1,2] = 2.0;
    T[1,3] = 3.0;
    T[2,1] = 4.0;
    T[2,2] = 5.0;
    T[2,3] = 6.0;
end MultiDimSubscript;
"#;
        let r = assert_compiles(source, "MultiDimSubscript");
        // 6 scalar equations for 6 scalar unknowns (T has 2*3=6 elements)
        assert_eq!(
            r.balance, 0,
            "Multi-dim array subscript T[i,j]=expr should be scalar (count=1 each)"
        );
    }

    /// Function-call LHS with record argument should use function output size.
    ///
    /// Reproducer for MultiBody-style equations like `angularVelocity2(R_b) = w_rel_b`
    /// where `R_b` is an Orientation record ({T[3,3], w[3]} = 12 scalars) but the
    /// function output is Real[3]. The equation must count as 3 scalars, not 12.
    #[test]
    fn t10_05_function_lhs_record_arg_scalar_count() {
        let source = r#"
package ReproSimple
  record Orientation
    Real[3,3] T;
    Real[3] w;
  end Orientation;

  function angularVelocity2
    input Orientation R;
    output Real[3] w;
  algorithm
    w := R.w;
  end angularVelocity2;

  model FunctionLhsRecordArgScalarCount
    input Orientation R_b;
    output Real[3] w_rel_b;
  equation
    angularVelocity2(R_b) = w_rel_b;
  end FunctionLhsRecordArgScalarCount;
end ReproSimple;
"#;
        let r = assert_compiles(source, "ReproSimple.FunctionLhsRecordArgScalarCount");
        assert_eq!(
            r.balance, 0,
            "function-call LHS should count by output size (Real[3]), not record arg size (12)"
        );
    }

    /// Top-level connector members feeding internal input pins are external inputs.
    ///
    /// Without this, connector fields like `bus.ref` are classified as algebraic
    /// unknowns, causing underdetermined models (`balance=-1` in this reproducer).
    #[test]
    fn t10_06_top_level_connector_member_external_input() {
        let source = r#"
connector Bus
    Real ref;
    Real cmd;
end Bus;

block GainB
    parameter Real k = 2.0;
    input Real u;
    output Real y;
equation
    y = k*u;
end GainB;

model TopConnectorInput
    Bus bus;
    GainB g;
equation
    connect(bus.ref, g.u);
    connect(g.y, bus.cmd);
end TopConnectorInput;
"#;
        let r = assert_compiles(source, "TopConnectorInput");
        assert_eq!(
            r.balance, 0,
            "top-level connector member connected only to internal input should not be an unknown"
        );
    }

    /// Dot-product residuals should be scalar, not vector-sized.
    ///
    /// Reproducer for MultiBody residual forms like `0 = v*e - s`.
    /// Bug: equation scalar count was inferred from VarRefs (`v`, `e`) as 3.
    #[test]
    fn t10_07_vector_dot_product_residual_scalar_count() {
        let source = r#"
model DotProductResidual
    Real[3] a;
    Real[3] b;
    Real s;
equation
    a = {1, 2, 3};
    b = {4, 5, 6};
    0 = a*b - s;
end DotProductResidual;
"#;
        let r = assert_compiles(source, "DotProductResidual");
        assert_eq!(
            r.balance, 0,
            "vector dot-product residual should contribute one scalar equation"
        );
    }

    /// Non-`each` fill() modifiers on arrayed components must distribute per element.
    ///
    /// MLS §7.2.5: `g[n](k=fill(2,n))` gives scalar `k` for each `g[i]`.
    /// Regression: unresolved fill() stayed on each scalar element as an array,
    /// inflating each `g[i]` equation scalar_count from 1 to 2.
    #[test]
    fn t10_08_fill_modifier_distributes_to_scalar_elements() {
        let source = r#"
model FillModifierDistribution
    block GainB
        parameter Real k = 1;
        input Real u;
        output Real y;
    equation
        y = k*u;
    end GainB;

    parameter Integer n = 2;
    GainB g[n](k = fill(2, n));
    input Real[n] u;
    output Real[n] y;
equation
    for i in 1:n loop
        g[i].u = u[i];
        y[i] = g[i].y;
    end for;
end FillModifierDistribution;
"#;
        let r = assert_compiles(source, "FillModifierDistribution");
        assert_eq!(
            r.balance, 0,
            "fill() modifier distribution should keep model balanced"
        );

        let mut gain_eq_counts = Vec::new();
        for eq in &r.dae.f_x {
            if eq.origin.contains("equation from g[") {
                gain_eq_counts.push(eq.scalar_count);
            }
        }
        assert_eq!(
            gain_eq_counts.len(),
            2,
            "expected one equation per GainB element"
        );
        assert!(
            gain_eq_counts.iter().all(|&sc| sc == 1),
            "scalar GainB equations must have scalar_count=1, got {:?}",
            gain_eq_counts
        );
    }
}

// =============================================================================
// Tier 10c: Output binding double-counting
// =============================================================================

mod tier_10c {
    use super::*;

    /// Output variable with binding that references another algebraic variable.
    /// The binding should be the ONLY equation; no explicit equation should duplicate it.
    #[test]
    fn t10c_01_output_binding_references_unknown() {
        let source = r#"
model OutputBindingTest
    Real x;
    Real y;
    output Real z = x + y;
equation
    x = 1.0;
    y = 2.0;
end OutputBindingTest;
"#;
        let r = assert_compiles(source, "OutputBindingTest");
        // z has binding z = x + y which is 1 equation
        // x = 1.0 and y = 2.0 are 2 equations
        // Total: 3 equations, 3 unknowns (x, y, z)
        assert_eq!(
            r.balance, 0,
            "Output with binding referencing unknowns should have balance=0"
        );
    }

    /// Output variable with binding AND explicit equation — binding should be skipped.
    #[test]
    fn t10c_02_output_binding_and_explicit_eq() {
        let source = r#"
model OutputBindingExplicit
    Real x;
    output Real y = x;
equation
    x = 1.0;
    y = x;
end OutputBindingExplicit;
"#;
        let r = assert_compiles(source, "OutputBindingExplicit");
        // x = 1.0 is 1 equation
        // y = x is 1 equation (from equation section)
        // binding y = x should be SKIPPED (explicit equation overrides)
        // Total: 2 equations, 2 unknowns (x, y)
        assert_eq!(
            r.balance, 0,
            "Output with both binding and explicit equation should not double-count"
        );
    }

    /// Multiple outputs with bindings referencing other unknowns, no explicit equations.
    #[test]
    fn t10c_03_multiple_output_bindings() {
        let source = r#"
model MultiOutputBinding
    Real a;
    Real b;
    output Real c = a;
    output Real d = b;
equation
    a = 1.0;
    b = 2.0;
end MultiOutputBinding;
"#;
        let r = assert_compiles(source, "MultiOutputBinding");
        // a = 1.0, b = 2.0, c = a (binding), d = b (binding) = 4 equations, 4 unknowns
        assert_eq!(
            r.balance, 0,
            "Multiple output bindings should each count as exactly one equation"
        );
    }

    /// Constant binding should be KEPT when the explicit equation relates
    /// the variable to another unknown (not a duplicate).
    /// Pattern: `output Real suspend = false; equation suspend = subport.suspend;`
    /// Both equations are needed: binding provides VALUE, explicit provides RELATION.
    #[test]
    fn t10c_04_constant_binding_with_relational_explicit_eq() {
        let source = r#"
model ConstBindingRelational
    Real x = 0;
    Real y;
equation
    x = y;
end ConstBindingRelational;
"#;
        let r = assert_compiles(source, "ConstBindingRelational");
        // x = 0 (binding) + x = y (explicit) = 2 equations, 2 unknowns (x, y)
        assert_eq!(
            r.balance, 0,
            "Constant binding should be kept when explicit eq relates to another unknown"
        );
    }

    /// Same pattern with output variables (like StateGraphRoot.suspend = false).
    #[test]
    fn t10c_05_output_constant_binding_with_relational_explicit_eq() {
        let source = r#"
model OutputConstBindingRelational
    output Real suspend = false;
    Real subport_suspend;
equation
    suspend = subport_suspend;
end OutputConstBindingRelational;
"#;
        let r = assert_compiles(source, "OutputConstBindingRelational");
        // suspend = false (binding) + suspend = subport_suspend (explicit) = 2 equations
        // 2 unknowns: suspend, subport_suspend
        assert_eq!(
            r.balance, 0,
            "Output with constant binding + relational explicit eq should balance"
        );
    }
}

mod tier_10d_empty_arrays {
    use super::*;

    /// Connector with zero-length array flow variables should not generate
    /// spurious connection equations (Real[0] arrays have no scalars).
    #[test]
    fn t10d_01_empty_array_connection() {
        let source = r#"
connector FluidPort
    Real p;
    flow Real m_flow;
    Real[0] Xi_outflow;
    flow Real[0] mXi_flow;
end FluidPort;

model EmptyArrayConnect
    FluidPort port_a;
    FluidPort port_b;
equation
    connect(port_a, port_b);
end EmptyArrayConnect;
"#;
        let r = assert_compiles(source, "EmptyArrayConnect");
        assert_eq!(
            r.balance, 0,
            "Real[0] arrays should not generate connection equations"
        );
    }

    /// Three-port connector with empty arrays (like TeeJunctionIdeal pattern).
    #[test]
    fn t10d_02_three_port_empty_array() {
        let source = r#"
connector FluidPort
    Real p;
    flow Real m_flow;
    Real[0] Xi_outflow;
    flow Real[0] mXi_flow;
end FluidPort;

model ThreePortJunction
    FluidPort port_1;
    FluidPort port_2;
    FluidPort port_3;
equation
    connect(port_1, port_2);
    connect(port_2, port_3);
end ThreePortJunction;
"#;
        let r = assert_compiles(source, "ThreePortJunction");
        // 6 scalar unknowns (3 ports × {p, m_flow})
        // Connection equations: 2 potential (p chain) + 1 flow sum = 3
        // Interface flows: 3 (port_1.m_flow, port_2.m_flow, port_3.m_flow)
        // Total: 3 + 3 = 6 equations, 6 unknowns
        assert_eq!(
            r.balance, 0,
            "Three-port with Real[0] arrays should not generate spurious equations"
        );
    }

    /// Three-port stream connector (like TeeJunctionIdeal pattern).
    /// Stream connection equations (N-1 equalities) should not be double-counted
    /// with the interface stream variable count.
    #[test]
    fn t10d_03_three_port_stream_connector() {
        let source = r#"
connector FluidPort
    Real p;
    flow Real m_flow;
    stream Real h_outflow;
end FluidPort;

model ThreePortJunction
    FluidPort port_1;
    FluidPort port_2;
    FluidPort port_3;
equation
    connect(port_1, port_2);
    connect(port_1, port_3);
end ThreePortJunction;
"#;
        let r = assert_compiles(source, "ThreePortJunction");
        // 9 scalar unknowns (3 ports x {p, m_flow, h_outflow})
        // Connection equations in f_x:
        //   2 potential equalities (p1=p2, p2=p3)
        //   1 flow sum (0 = m1+m2+m3)
        //   2 stream equalities (h1=h2, h2=h3) -- N-1 for N=3
        //   3 unconnected flow (0=m1, 0=m2, 0=m3)
        // = 8 f_x equations
        // Interface: 3 stream vars - 2 stream connection eqs = 1
        // Total: 8 + 1 = 9 equations, 9 unknowns
        assert_eq!(
            r.balance, 0,
            "Three-port stream connector: stream connection eqs should not be double-counted"
        );
    }
}

// =============================================================================
// Tier 10e: VCG break edge correction (MLS §9.4)
// =============================================================================

mod tier_10e_vcg_break_edge {
    use super::*;

    /// QS-style circuit with overconstrained Reference connector.
    /// Uses a nested Reference connector to avoid connector balance issues.
    ///
    /// Circuit: source -> resistor -> ground -> source (loop)
    /// Connections.branch(pin_p.ref, pin_n.ref) in each 2-port component
    /// creates VCG required edges. The connect() loop creates a cycle → 1 break edge.
    /// The Ground's `if isRoot then gamma=0` adds a root equation, making the
    /// system over-determined by 1 unless the break edge correction is applied.
    #[test]
    fn t10e_01_qs_series_loop() {
        let source = r#"
connector Reference
    Real gamma;
end Reference;

connector Pin
    Real v;
    flow Real i;
    Reference ref;
end Pin;

model TwoPort
    Pin pin_p;
    Pin pin_n;
equation
    Connections.branch(pin_p.ref, pin_n.ref);
    pin_p.ref.gamma = pin_n.ref.gamma;
    pin_p.v - pin_n.v = 0;
    pin_p.i + pin_n.i = 0;
end TwoPort;

model Ground
    Pin pin;
equation
    Connections.potentialRoot(pin.ref);
    if Connections.isRoot(pin.ref) then
        pin.ref.gamma = 0;
    end if;
    pin.v = 0;
end Ground;

model SeriesLoop
    TwoPort source;
    TwoPort resistor;
    Ground ground;
equation
    connect(source.pin_p, resistor.pin_p);
    connect(resistor.pin_n, ground.pin);
    connect(ground.pin, source.pin_n);
end SeriesLoop;
"#;
        let r = assert_compiles(source, "SeriesLoop");
        assert_eq!(
            r.dae.oc_break_edge_scalar_count, 1,
            "Should detect 1 break edge with 1 scalar (reference.gamma)"
        );
        assert_eq!(
            r.balance, 0,
            "VCG break edge correction should fix balance for series loop"
        );
    }

    /// QS-style wrapper model where the ROOT class itself extends a 2-port
    /// (has its own Connections.branch) AND contains a sub-component that also
    /// has Connections.branch. The connect() between root pins and sub-component
    /// pins creates a VCG cycle that should produce 1 break edge.
    ///
    /// This pattern appears in FrequencySweepVoltageSource and similar QS models.
    #[test]
    fn t10e_02_qs_wrapper_with_root_branch() {
        let source = r#"
connector Reference
    Real gamma;
end Reference;

connector Pin
    Real v;
    flow Real i;
    Reference ref;
end Pin;

model TwoPort
    Pin pin_p;
    Pin pin_n;
equation
    Connections.branch(pin_p.ref, pin_n.ref);
    pin_p.ref.gamma = pin_n.ref.gamma;
    pin_p.v - pin_n.v = 0;
    pin_p.i + pin_n.i = 0;
end TwoPort;

model InnerSource
    extends TwoPort;
equation
    Connections.potentialRoot(pin_p.ref);
    pin_p.v = 1;
end InnerSource;

partial model Wrapper
    extends TwoPort;
    InnerSource src;
equation
    connect(pin_p, src.pin_p);
    connect(src.pin_n, pin_n);
end Wrapper;
"#;
        let r = assert_compiles(source, "Wrapper");
        assert_eq!(
            r.dae.oc_break_edge_scalar_count, 1,
            "Should detect 1 break edge when root class and sub-component both have VCG branches"
        );
        // balance=2 because the wrapper has open external connectors that generate
        // redundant flow equations. The VCG correctly detects 1 break edge.
        // Real MSL models that use this pattern are balanced by interface flow counting.
        assert_eq!(r.balance, 2);
    }

    /// Array overconstrained connectors should derive optional VCG edges per element.
    /// If connect() edges are not expanded/index-matched, break-edge correction is missed.
    #[test]
    fn t10e_03_array_overconstrained_optional_edges() {
        let source = r#"
connector Ref
    Real gamma;
end Ref;

connector Pin
    Real v;
    flow Real i;
    Ref ref;
end Pin;

model TwoPort
    Pin a;
    Pin b;
equation
    Connections.branch(a.ref, b.ref);
    a.ref.gamma = b.ref.gamma;
    a.v - b.v = 0;
    a.i + b.i = 0;
end TwoPort;

model Ground
    Pin pin;
equation
    Connections.potentialRoot(pin.ref);
    if Connections.isRoot(pin.ref) then
        pin.ref.gamma = 0;
    end if;
    pin.v = 0;
end Ground;

model ArrayLoop
    parameter Integer m = 2;
    TwoPort source[m];
    TwoPort resistor[m];
    Ground ground[m];
equation
    connect(source.a, resistor.a);
    connect(resistor.b, ground.pin);
    connect(ground.pin, source.b);
end ArrayLoop;
"#;

        let r = assert_compiles(source, "ArrayLoop");
        assert_eq!(
            r.dae.oc_break_edge_scalar_count, 2,
            "Two array-indexed VCG loops should produce 2 break-edge scalars"
        );
        assert_eq!(
            r.balance, 0,
            "Indexed optional VCG edges from connect() should eliminate overdetermined loops"
        );
    }
}

// =============================================================================
// Tier 10e2: Inner/outer flow sum merging
// =============================================================================

mod tier_10e2_inner_outer_flow_merge {
    use super::*;

    /// When multiple components redirect outer→inner to the same target,
    /// their flow variables should merge into ONE flow sum equation,
    /// not separate pairwise sums per scope.
    ///
    /// Pattern: StateGraph subgraphStatePort.activeSteps flow variable.
    /// Two steps both have `outer StateGraphRoot` that redirects to the
    /// same `inner StateGraphRoot`. The flow sum should be:
    ///   a.steps + b.steps + root.steps = 0 (ONE equation)
    /// Not:
    ///   a.steps + root.steps = 0; b.steps + root.steps = 0 (TWO equations)
    #[test]
    fn t10e2_01_inner_outer_flow_merge() {
        let source = r#"
connector SubPort
    Real dummy;
    flow Real steps;
end SubPort;

model Root
    SubPort port;
equation
    port.dummy = 0;
end Root;

model Step
    outer Root root;
    SubPort port;
equation
    port.steps = if port.dummy > 0 then 1.0 else 0.0;
    connect(port, root.port);
end Step;

model Top
    inner Root root;
    Step stepA;
    Step stepB;
end Top;
"#;
        let r = assert_compiles(source, "Top");
        // Variables: root.port.dummy, root.port.steps,
        //   stepA.port.dummy, stepA.port.steps,
        //   stepB.port.dummy, stepB.port.steps = 6 unknowns
        // Equations:
        //   root.port.dummy = 0 (from Root body) = 1
        //   stepA.port.steps = if... = 1
        //   stepB.port.steps = if... = 1
        //   connect equality: stepA.port.dummy = root.port.dummy = stepB.port.dummy (2 eqs)
        //   flow sum: stepA.port.steps + stepB.port.steps + root.port.steps = 0 (1 eq)
        // Total: 6 equations, 6 unknowns → balance = 0
        assert_eq!(
            r.balance, 0,
            "Inner/outer redirected flow sums should merge into one equation"
        );
    }
}

// =============================================================================
// Tier 10e3: Conditional component with sibling record reference (MLS §4.8)
// =============================================================================

mod tier_10e3_conditional_sibling_ref {
    use super::*;

    /// MLS §4.8: When a conditional component's condition references a sibling
    /// record's field (e.g., `SubComp sub if data.enabled`), the condition must
    /// be resolved through the sibling's modifications to properly disable the
    /// component and skip connections to it.
    ///
    /// Pattern from Modelica.Electrical.Machines: `DamperCage damperCage(...) if useDamperCage`
    /// where `useDamperCage = smpmData.useDamperCage` and `smpmData(useDamperCage=false)`.
    #[test]
    fn t10e3_01_conditional_component_sibling_record_ref() {
        let source = r#"
connector RealOutput = output Real;

model SubComp
    RealOutput lossPower;
equation
    lossPower = 1.0;
end SubComp;

record DataRecord
    parameter Boolean enabled = true;
end DataRecord;

model Parent
    parameter Boolean useSubComp = data.enabled;
    DataRecord data;
    SubComp sub if useSubComp;
    RealOutput damperLossPower;
equation
    connect(damperLossPower, sub.lossPower);
    if not useSubComp then
        damperLossPower = 0;
    end if;
end Parent;

model Top
    Parent p(data(enabled=false));
end Top;
"#;
        let r = assert_compiles(source, "Top");
        assert_eq!(
            r.balance, 0,
            "Conditional component disabled via sibling record ref should not produce duplicate equations"
        );
    }
}

// =============================================================================
// Tier 10f: Algorithm array output scalar counting (MLS §11.1)
// =============================================================================

mod tier_10f_algorithm_array_outputs {
    use super::*;

    /// Supported model algorithms are lowered into `f_x` equations. After
    /// lowering, `algorithm_outputs` should be zero and balance should still hold.
    #[test]
    fn t10f_01_algorithm_array_output() {
        let source = r#"
model AlgArrayOutput
    Real[3] x;
    Real[3] y;
algorithm
    x := {1, 2, 3};
    y := x;
end AlgArrayOutput;
"#;
        let r = assert_compiles(source, "AlgArrayOutput");
        let detail = rumoca_eval_dae::analysis::balance_detail(&r.dae);
        assert_eq!(
            detail.algorithm_outputs, 0,
            "Lowered model algorithms should not contribute algorithm_outputs directly"
        );
        assert_eq!(
            detail.f_x_scalar, 6,
            "Lowered array equations should retain scalar size"
        );
        assert_eq!(
            r.balance, 0,
            "Lowered algorithm equations must keep model balanced"
        );
    }
}

// =============================================================================
// TIER 10g: Connection array-to-subarray scalar counting
// =============================================================================

mod tier_10g_connection_array_scalar {
    use super::*;

    /// When connect() links arrays of different sizes (e.g., connect(a.y, b.u[1:2])
    /// where a.y is [2] and b.u is [5]), the connection phase may generate a
    /// whole-array equation like `b.u = a.y`. The scalar count for this equation
    /// must be min(|b.u|, |a.y|) = 2, not |b.u| = 5.
    ///
    /// This pattern appears in ModelicaTest.Blocks.MuxDemux where Mux/DeMultiplexer
    /// blocks with different widths are connected via subranges.
    #[test]
    fn t10g_01_connection_array_size_mismatch() {
        let source = r#"
connector RealInput = input Real;
connector RealOutput = output Real;

block PassThrough
    parameter Integer n = 1;
    RealInput u[n];
    RealOutput y[n];
equation
    y = u;
end PassThrough;

model ArraySubrangeConnect
    PassThrough small(n=2);
    PassThrough big(n=5);
    PassThrough other(n=3);
    Real src;
equation
    src = 1.0;
    connect(small.y, big.u[1:2]);
    connect(other.y, big.u[3:5]);
    small.u = {src, src};
    other.u = {src, src, src};
end ArraySubrangeConnect;
"#;
        let r = assert_compiles(source, "ArraySubrangeConnect");
        assert_eq!(
            r.balance, 0,
            "array-to-subarray connection equations should count min(lhs, rhs) scalars"
        );
    }
}

mod tier_10h_discrete_scalar_count {
    use super::*;

    /// Multi-output function calls produce tuple equations like
    /// `(result, stateOut) = random(previous(stateIn))` where `result` is
    /// a continuous output (scalar) and `stateOut` is a discrete array.
    /// The scalar count inference should NOT count the discrete variable's
    /// dimensions — only continuous unknowns determine the equation's
    /// scalar count in the continuous balance.
    ///
    /// Without the fix, `infer_scalar_count_from_varrefs()` picks up
    /// the discrete array size, inflating the equation's scalar count
    /// and causing balance=+1.
    #[test]
    fn t10h_01_discrete_array_not_counted_in_scalar_inference() {
        let source = r#"
function MultiOut
    input Real x;
    output Real y;
    output Real[2] s;
algorithm
    y := x;
    s := {x, x};
end MultiOut;

model TupleDiscreteBalance
    discrete Real[2] state;
    Real result;
equation
    (result, state) = MultiOut(1.0);
end TupleDiscreteBalance;
"#;
        let r = assert_compiles(source, "TupleDiscreteBalance");
        // result (1 output) needs 1 equation; state (discrete) is separate.
        // The tuple equation should count as 1 scalar (for result), not 3.
        assert_eq!(
            r.balance, 0,
            "discrete variables should not inflate continuous equation scalar count"
        );
    }
}

// =============================================================================
// Tier 10h2: Tuple equation scalar count with array elements (MLS §8.4)
// =============================================================================

mod tier_10h2_tuple_array_scalar_count {
    use super::*;

    /// MLS §8.4: A tuple equation `(a, b) = f(...)` where a is Real[2] and b is
    /// Real[2] should count as 4 scalar equations (2+2), not 2 (element count).
    ///
    /// Root cause: extract_lhs_var_size used .count() on tuple elements, which
    /// counts the number of elements rather than summing their scalar sizes.
    #[test]
    fn t10h2_01_tuple_array_elements_scalar_count() {
        let source = r#"
function PairArrays
    input Real x;
    output Real[2] b;
    output Real[2] a;
algorithm
    b := {x, 2*x};
    a := {3*x, 4*x};
end PairArrays;

model TupleArrayCount
    Real[2] b;
    Real[2] a;
equation
    (b, a) = PairArrays(1.0);
end TupleArrayCount;
"#;
        let r = assert_compiles(source, "TupleArrayCount");
        // 4 unknowns (b[2], a[2]) and 4 scalar equations from the tuple
        assert_eq!(
            r.balance, 0,
            "Tuple with array elements should count total scalar size, not element count"
        );
    }
}

// =============================================================================
// Tier 10h4: Subscripted record LHS scalar count
// =============================================================================

mod tier_10h4_subscripted_record_scalar_count {
    use rumoca_ir_flat as flat;
    use rumoca_phase_dae::to_dae;
    type Equation = flat::Equation;
    type EquationOrigin = flat::EquationOrigin;
    type Expression = flat::Expression;
    type Model = flat::Model;
    type VarName = flat::VarName;

    /// Helper to create a residual equation: lhs - rhs = 0
    fn make_residual_eq(lhs: Expression, rhs: Expression) -> Equation {
        Equation {
            residual: Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            span: Default::default(),
            origin: EquationOrigin::ComponentEquation {
                component: String::new(),
            },
            scalar_count: 1,
        }
    }

    /// MLS §10.2: Subscripted record array reference `bw[1] = 0` where `bw` is
    /// Complex[1] should count as 2 scalar equations (one per record field).
    ///
    /// This tests the MSL-style variable naming where record array fields are
    /// expanded as `bw.re` (Real[1]), `bw.im` (Real[1]) — array dim pushed into
    /// fields. The equation VarRef `bw[1]` must strip the subscript to find
    /// the record prefix `bw` in prefix_counts, then compute per-element size.
    ///
    /// Root cause: extract_lhs_var_size previously returned None when
    /// strip_subscript("bw[1]") → "bw" was found in prefix_counts, causing
    /// the fallback to default to scalar_count=1 instead of 2.
    #[test]
    fn t10h4_01_subscripted_record_array_equation() {
        // Construct a Model matching MSL Complex[1] pattern:
        // Variables: bw.re (Real[1]), bw.im (Real[1]), y.re, y.im (outputs)
        // Equations:
        //   bw[1] - 0 = 0 (record-level subscripted equation, should count as 2)
        //   y.re - bw.re = 0 (references bw.re so it gets classified)
        //   y.im - bw.im = 0 (references bw.im so it gets classified)
        let mut flat = Model::new();
        flat.add_variable(
            VarName::new("bw.re"),
            flat::Variable {
                name: VarName::new("bw.re"),
                dims: vec![1],
                is_primitive: true,
                ..Default::default()
            },
        );
        flat.add_variable(
            VarName::new("bw.im"),
            flat::Variable {
                name: VarName::new("bw.im"),
                dims: vec![1],
                is_primitive: true,
                ..Default::default()
            },
        );
        flat.add_variable(
            VarName::new("y.re"),
            flat::Variable {
                name: VarName::new("y.re"),
                causality: rumoca_ir_core::Causality::Output(Default::default()),
                is_primitive: true,
                ..Default::default()
            },
        );
        flat.add_variable(
            VarName::new("y.im"),
            flat::Variable {
                name: VarName::new("y.im"),
                causality: rumoca_ir_core::Causality::Output(Default::default()),
                is_primitive: true,
                ..Default::default()
            },
        );

        // Record-level subscripted equation: bw[1] = 0 → should count as 2 scalars
        flat.add_equation(make_residual_eq(
            Expression::VarRef {
                name: VarName::new("bw[1]"),
                subscripts: vec![],
            },
            Expression::Literal(rumoca_ir_flat::Literal::Integer(0)),
        ));
        // Output equations referencing bw fields
        flat.add_equation(make_residual_eq(
            Expression::VarRef {
                name: VarName::new("y.re"),
                subscripts: vec![],
            },
            Expression::VarRef {
                name: VarName::new("bw.re"),
                subscripts: vec![],
            },
        ));
        flat.add_equation(make_residual_eq(
            Expression::VarRef {
                name: VarName::new("y.im"),
                subscripts: vec![],
            },
            Expression::VarRef {
                name: VarName::new("bw.im"),
                subscripts: vec![],
            },
        ));

        let dae = to_dae(&flat).unwrap();
        // 4 unknowns: bw.re, bw.im (algebraics) + y.re, y.im (outputs)
        // 4 scalar equations: bw[1]=0 (2 scalars) + y.re=bw.re (1) + y.im=bw.im (1)
        assert_eq!(
            dae.algebraics.len(),
            2,
            "should have 2 algebraic vars (bw.re, bw.im)"
        );
        assert_eq!(
            dae.outputs.len(),
            2,
            "should have 2 output vars (y.re, y.im)"
        );
        assert_eq!(dae.f_x.len(), 3, "should have 3 equations");

        // The key assertion: bw[1] = 0 must count as 2 scalars
        let bw_eq = dae
            .f_x
            .iter()
            .find(|eq| eq.origin.contains("bw[1]") || eq.scalar_count == 2);
        assert!(
            bw_eq.is_some_and(|eq| eq.scalar_count == 2),
            "bw[1] = 0 should count as 2 scalars (record has .re + .im); \
             f_x scalar counts: {:?}",
            dae.f_x.iter().map(|eq| eq.scalar_count).collect::<Vec<_>>()
        );
        assert_eq!(
            rumoca_eval_dae::analysis::balance(&dae),
            0,
            "balance should be 0 (4 scalars = 4 unknowns)"
        );
    }
}

/// Tier 10h5: Connected top-level connector input fields must stay as inputs.
///
/// MLS §4.7: Top-level input connector fields (e.g., `u.re`, `u.im` from
/// `ComplexInput u`) are external interfaces. Even when connected internally
/// (e.g., `sub.u.re = u.re`), they remain inputs — the connection propagates
/// the external value inward, it does not make the input an unknown.
mod tier_10h5_connected_toplevel_input {
    use rumoca_ir_flat as flat;
    use rumoca_phase_dae::to_dae;
    type Equation = flat::Equation;
    type EquationOrigin = flat::EquationOrigin;
    type Expression = flat::Expression;
    type Model = flat::Model;
    type VarName = flat::VarName;

    fn make_connection_eq(lhs_name: &str, rhs_name: &str) -> Equation {
        Equation {
            residual: Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new(lhs_name),
                    subscripts: vec![],
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new(rhs_name),
                    subscripts: vec![],
                }),
            },
            span: Default::default(),
            origin: EquationOrigin::Connection {
                lhs: lhs_name.to_string(),
                rhs: rhs_name.to_string(),
            },
            scalar_count: 1,
        }
    }

    fn make_component_eq(lhs_name: &str, rhs_name: &str) -> Equation {
        Equation {
            residual: Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new(lhs_name),
                    subscripts: vec![],
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new(rhs_name),
                    subscripts: vec![],
                }),
            },
            span: Default::default(),
            origin: EquationOrigin::ComponentEquation {
                component: String::new(),
            },
            scalar_count: 1,
        }
    }

    /// Connected top-level ComplexInput fields must remain as DAE inputs.
    ///
    /// Model structure (like ComplexBlocks.ComplexMath.Bode):
    ///   - Top-level: ComplexInput u (fields: u.re, u.im)
    ///   - Sub-component: division with inputs division.u1.re, division.u1.im
    ///   - Connection: connect(u, division.u1) → u.re = division.u1.re
    ///   - Output: y.re, y.im
    ///
    /// Bug: u.re/u.im were classified as algebraic (not input) because
    /// is_internal_input() checked `!var.connected`, and connection equations
    /// set `connected = true` on both sides of the connection.
    #[test]
    fn t10h5_01_connected_toplevel_complex_input() {
        let mut flat = Model::new();

        // Top-level input connector fields (like ComplexInput u)
        flat.add_variable(
            VarName::new("u.re"),
            flat::Variable {
                name: VarName::new("u.re"),
                causality: rumoca_ir_core::Causality::Input(Default::default()),
                is_primitive: true,
                connected: true,
                ..Default::default()
            },
        );
        flat.add_variable(
            VarName::new("u.im"),
            flat::Variable {
                name: VarName::new("u.im"),
                causality: rumoca_ir_core::Causality::Input(Default::default()),
                is_primitive: true,
                connected: true,
                ..Default::default()
            },
        );

        // Sub-component input fields (like division.u1)
        flat.add_variable(
            VarName::new("division.u1.re"),
            flat::Variable {
                name: VarName::new("division.u1.re"),
                causality: rumoca_ir_core::Causality::Input(Default::default()),
                is_primitive: true,
                connected: true,
                ..Default::default()
            },
        );
        flat.add_variable(
            VarName::new("division.u1.im"),
            flat::Variable {
                name: VarName::new("division.u1.im"),
                causality: rumoca_ir_core::Causality::Input(Default::default()),
                is_primitive: true,
                connected: true,
                ..Default::default()
            },
        );

        // Output fields
        flat.add_variable(
            VarName::new("y.re"),
            flat::Variable {
                name: VarName::new("y.re"),
                causality: rumoca_ir_core::Causality::Output(Default::default()),
                is_primitive: true,
                ..Default::default()
            },
        );
        flat.add_variable(
            VarName::new("y.im"),
            flat::Variable {
                name: VarName::new("y.im"),
                causality: rumoca_ir_core::Causality::Output(Default::default()),
                is_primitive: true,
                ..Default::default()
            },
        );

        // Register "u" as a top-level connector
        flat.top_level_connectors.insert("u".to_string());

        // Connection equations: sub.u1 = u (propagates external input inward)
        flat.add_equation(make_connection_eq("division.u1.re", "u.re"));
        flat.add_equation(make_connection_eq("division.u1.im", "u.im"));

        // Component equations: y = division.u1
        flat.add_equation(make_component_eq("y.re", "division.u1.re"));
        flat.add_equation(make_component_eq("y.im", "division.u1.im"));

        let dae = to_dae(&flat).unwrap();

        // u.re, u.im must be inputs (external), NOT algebraics
        assert_eq!(
            dae.inputs.len(),
            2,
            "u.re, u.im should be inputs; got inputs={:?}, algebraics={:?}",
            dae.inputs.keys().collect::<Vec<_>>(),
            dae.algebraics.keys().collect::<Vec<_>>()
        );
        assert!(
            dae.inputs
                .contains_key(&rumoca_ir_dae::VarName::new("u.re")),
            "u.re should be input"
        );
        assert!(
            dae.inputs
                .contains_key(&rumoca_ir_dae::VarName::new("u.im")),
            "u.im should be input"
        );

        // division.u1.re, division.u1.im should be algebraics (connected internal)
        assert_eq!(
            dae.algebraics.len(),
            2,
            "division.u1.re, division.u1.im should be algebraics"
        );
        assert_eq!(dae.outputs.len(), 2, "y.re, y.im should be outputs");

        // Balance: 4 equations, 4 unknowns (2 algebraics + 2 outputs)
        assert_eq!(dae.f_x.len(), 4);
        assert_eq!(
            rumoca_eval_dae::analysis::balance(&dae),
            0,
            "balance should be 0"
        );
    }
}

// =============================================================================
// Tier 10h3: Multi-element Array LHS scalar count
// =============================================================================

mod tier_10h3_matrix_equation_scalar_count {
    use super::*;

    /// MLS §10.6.1: Matrix equations like `[der(x); y] = [y; u]` represent
    /// N scalar equations where N is the total number of leaf elements.
    ///
    /// Root cause: extract_lhs_var_size doesn't handle multi-element Array LHS,
    /// falling through to infer_scalar_count_from_varrefs which finds max_size=1
    /// (all variables are scalar), undercounting from 2 to 1.
    #[test]
    fn t10h3_01_matrix_equation_two_rows() {
        let source = r#"
model MatrixEqTwoRows
    Real x(start = 0);
    Real y;
    input Real u;
equation
    [der(x); y] = [y; u];
end MatrixEqTwoRows;
"#;
        let r = assert_compiles(source, "MatrixEqTwoRows");
        // 2 unknowns: x (state), y (algebraic)
        // 1 matrix equation [der(x); y] = [y; u] = 2 scalar equations
        // Balance: 2 - 2 = 0
        assert_eq!(
            r.balance, 0,
            "Matrix equation [der(x); y] = [y; u] should count as 2 scalar equations"
        );
    }
}

// =============================================================================
// Tier 10i: Tuple rendering in codegen
// =============================================================================

mod tier_10i_tuple_rendering {
    use super::*;

    /// Multi-output function calls produce tuple equations like
    /// `(y1, y2) = func(x)`. The codegen should render the tuple LHS
    /// as `(y1, y2)`, not `/* unknown */`.
    #[test]
    fn t10i_01_tuple_lhs_renders_without_unknown() {
        let source = r#"
function TwoOut
    input Real x;
    output Real a;
    output Real b;
algorithm
    a := x;
    b := 2 * x;
end TwoOut;

model TupleRender
    Real y1;
    Real y2;
equation
    (y1, y2) = TwoOut(1.0);
end TupleRender;
"#;
        let r = assert_compiles(source, "TupleRender");
        assert_eq!(r.balance, 0, "model should be balanced");

        // Render DAE to Modelica and verify tuple is rendered properly
        let rendered = rumoca_phase_codegen::render_template_with_name(
            &r.dae,
            rumoca_phase_codegen::templates::DAE_MODELICA,
            "TupleRender",
        )
        .expect("codegen should succeed");

        assert!(
            !rendered.contains("/* unknown */"),
            "codegen should not produce /* unknown */ for tuple LHS. Got:\n{rendered}"
        );
        // The tuple should render as (y1, y2)
        assert!(
            rendered.contains("(y1, y2)"),
            "codegen should render tuple as (y1, y2). Got:\n{rendered}"
        );
    }
}

// =============================================================================
// Tier 10j: Record array dimension propagation
// =============================================================================

mod tier_10j_record_array_dims {
    use super::*;

    /// When a record array like Complex[n] is NOT expanded at instantiation time
    /// (e.g., because n = size(a, 1) is not evaluable during instantiation),
    /// the fields (z.re, z.im) are created as scalars (dims=[]).
    /// The flatten phase should propagate the parent's array dims to these fields
    /// so that todae correctly counts them in the balance.
    ///
    /// Model structure (like Modelica.ComplexBlocks.Sources.ComplexExpression
    /// with TransferFunction):
    ///   - Record Cpx with fields re, im
    ///   - Array Cpx[n] z where n depends on parameter array size
    ///   - Record-level equations: z[1] = ..., z[2] = ...
    ///
    /// Without fix: z.re counts as 1 scalar, z.im counts as 1 scalar → 2 unknowns
    /// With fix: z.re counts as n scalars, z.im counts as n scalars → 2n unknowns
    #[test]
    fn t10j_01_record_array_field_dims() {
        let source = r#"
record Cpx
    Real re;
    Real im;
end Cpx;

block TF
    parameter Real[:] a;
    parameter Integer na = size(a, 1);
    Cpx[na] z;
    input Real w;
    output Real y;
equation
    for i in 1:na loop
        z[i] = Cpx(a[i] * w, 0);
    end for;
    y = z[1].re;
end TF;

model RecordArrayDimTest
    parameter Real[2] coeff = {1.0, 2.0};
    TF tf(a = coeff);
    input Real w;
equation
    tf.w = w;
end RecordArrayDimTest;
"#;
        let r = assert_compiles(source, "RecordArrayDimTest");
        // The regression target for this test is record-field dimension
        // propagation (`tf.z` -> `tf.z.re`/`tf.z.im`), not top-level input
        // accounting in the overall balance metric.
        let z_re = r
            .dae
            .algebraics
            .get(&rumoca_ir_dae::VarName::new("tf.z.re"))
            .expect("expected tf.z.re algebraic field");
        let z_im = r
            .dae
            .algebraics
            .get(&rumoca_ir_dae::VarName::new("tf.z.im"))
            .expect("expected tf.z.im algebraic field");
        assert_eq!(
            z_re.dims,
            vec![2],
            "record array field tf.z.re should inherit parent array dims"
        );
        assert_eq!(
            z_im.dims,
            vec![2],
            "record array field tf.z.im should inherit parent array dims"
        );
    }

    /// Record field dimensions that depend on a pure recursive function should
    /// evaluate during instantiation so array dimensions are concrete.
    ///
    /// This mirrors MSL patterns like `data.mSystems`/`data.mBasic` used in
    /// array component dimensions.
    #[test]
    fn t10j_02_record_field_function_dimension_eval() {
        let source = r#"
function NumberOfSystems
    input Integer m = 6;
    output Integer n;
algorithm
    n := 1;
    if mod(m, 2) == 0 then
        if m == 2 then
            n := 1;
        else
            n := n * 2 * NumberOfSystems(integer(m / 2));
        end if;
    else
        n := 1;
    end if;
end NumberOfSystems;

record Data
    parameter Integer m = 6;
    parameter Integer mSystems = NumberOfSystems(m);
    parameter Integer mBasic = integer(m / mSystems);
end Data;

model RecordFieldFunctionDimension
    parameter Data data;
    Real x[data.mSystems, data.mBasic];
equation
    for i in 1:data.mSystems loop
        for j in 1:data.mBasic loop
            x[i, j] = 0;
        end for;
    end for;
end RecordFieldFunctionDimension;
"#;

        let r = assert_compiles(source, "RecordFieldFunctionDimension");
        assert_eq!(r.balance, 0, "model should be balanced with concrete dims");

        let dims = r
            .dae
            .algebraics
            .iter()
            .find_map(|(name, var)| (name.as_str() == "x").then_some(var.dims.clone()))
            .expect("expected algebraic variable x in DAE");
        assert_eq!(dims, vec![2, 3], "x should resolve to dimensions [2, 3]");
    }
}

// =============================================================================
// Tier 10h6: Range subscript scalar count in der() equations
// =============================================================================
