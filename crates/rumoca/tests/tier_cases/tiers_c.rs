use super::*;

mod tier_10h6_range_subscript_der {
    use rumoca_ir_flat as flat;
    use rumoca_phase_dae::to_dae;
    type BuiltinFunction = flat::BuiltinFunction;
    type Equation = flat::Equation;
    type EquationOrigin = flat::EquationOrigin;
    type Expression = flat::Expression;
    type Literal = flat::Literal;
    type Model = flat::Model;
    type VarName = flat::VarName;
    type Variability = rumoca_ir_core::Variability;

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

    /// MLS §10.6.1: Range subscripts like `der(x[2:n])` should count as n-1
    /// scalar equations, not 1. The range `[2:n]` selects elements 2 through n,
    /// producing n-2+1 = n-1 scalars.
    ///
    /// Model structure (like Modelica.Blocks.Continuous.Internal.Filter.base.CriticalDamping
    /// and PadeDelay):
    ///   - State variable x with dims [4]
    ///   - Parameter n = 4
    ///   - Equation: der(x[1]) = u (1 scalar)
    ///   - Equation: der(x[2:n]) = x[1:n-1] (should be 3 scalars, not 1)
    ///
    /// Without fix: der(x[2:n]) counts as 1 scalar → balance = -2
    /// With fix: der(x[2:n]) counts as 3 scalars → balance = 0
    #[test]
    fn t10h6_01_range_subscript_der_scalar_count() {
        let mut flat = Model::new();

        // State variable: x with dims [4]
        flat.add_variable(
            VarName::new("x"),
            flat::Variable {
                name: VarName::new("x"),
                dims: vec![4],
                is_primitive: true,
                ..Default::default()
            },
        );

        // Parameter: n = 4
        flat.add_variable(
            VarName::new("n"),
            flat::Variable {
                name: VarName::new("n"),
                variability: Variability::Parameter(Default::default()),
                binding: Some(Expression::Literal(Literal::Integer(4))),
                is_primitive: true,
                ..Default::default()
            },
        );

        // Input: u
        flat.add_variable(
            VarName::new("u"),
            flat::Variable {
                name: VarName::new("u"),
                causality: rumoca_ir_core::Causality::Input(Default::default()),
                is_primitive: true,
                ..Default::default()
            },
        );

        // Equation 1: der(x[1]) - u = 0 (1 scalar)
        flat.add_equation(make_residual_eq(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x[1]"),
                    subscripts: vec![],
                }],
            },
            Expression::VarRef {
                name: VarName::new("u"),
                subscripts: vec![],
            },
        ));

        // Equation 2: der(x[2:n]) - x[1:(n - 1)] = 0 (should be 3 scalars)
        flat.add_equation(make_residual_eq(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x[2:n]"),
                    subscripts: vec![],
                }],
            },
            Expression::VarRef {
                name: VarName::new("x[1:(n - 1)]"),
                subscripts: vec![],
            },
        ));

        let dae = to_dae(&flat).unwrap();

        // 4 unknowns: x (state with dims=[4])
        assert_eq!(dae.states.len(), 1, "should have 1 state variable (x)");

        // der(x[2:n]) should count as 3 scalars (4-2+1=3), not 1
        assert_eq!(
            rumoca_eval_dae::analysis::balance(&dae),
            0,
            "der(x[2:n]) should count as 3 scalar equations (range 2:4 = 3 elements); \
             f_x scalar counts: {:?}",
            dae.f_x.iter().map(|eq| eq.scalar_count).collect::<Vec<_>>()
        );
    }

    /// Range subscripts whose bounds are parameter expressions like `size(a,1)-1`
    /// must be evaluated recursively to compute the correct range size.
    ///
    /// Model structure (like DrydenContinuousTurbulence):
    ///   - Parameter a = Real[3], nx = size(a,1) - 1 = 2
    ///   - State x_scaled with dims [2]
    ///   - Equation: der(x_scaled[2:nx]) = x_scaled[1:nx-1] (range 2:2 = 1 element)
    ///
    /// Without recursive eval: nx binding is `size(a,1)-1` → can't evaluate → uses full dim=2
    /// With recursive eval: evaluates size(a,1)=3, 3-1=2, range 2:2 = 1 element
    #[test]
    fn t10h6_02_range_subscript_recursive_param_eval() {
        let mut flat = Model::new();

        // Parameter array: a with dims [3]
        flat.add_variable(
            VarName::new("a"),
            flat::Variable {
                name: VarName::new("a"),
                variability: Variability::Parameter(Default::default()),
                dims: vec![3],
                binding: Some(Expression::Array {
                    elements: vec![
                        Expression::Literal(Literal::Real(1.0)),
                        Expression::Literal(Literal::Real(2.0)),
                        Expression::Literal(Literal::Real(3.0)),
                    ],
                    is_matrix: false,
                }),
                is_primitive: true,
                ..Default::default()
            },
        );

        // Parameter: nx = size(a, 1) - 1 (binding is a Binary expression)
        flat.add_variable(
            VarName::new("nx"),
            flat::Variable {
                name: VarName::new("nx"),
                variability: Variability::Parameter(Default::default()),
                binding: Some(Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(Expression::BuiltinCall {
                        function: BuiltinFunction::Size,
                        args: vec![
                            Expression::VarRef {
                                name: VarName::new("a"),
                                subscripts: vec![],
                            },
                            Expression::Literal(Literal::Integer(1)),
                        ],
                    }),
                    rhs: Box::new(Expression::Literal(Literal::Integer(1))),
                }),
                is_primitive: true,
                ..Default::default()
            },
        );

        // State: x_scaled with dims [2]
        flat.add_variable(
            VarName::new("x_scaled"),
            flat::Variable {
                name: VarName::new("x_scaled"),
                dims: vec![2],
                is_primitive: true,
                ..Default::default()
            },
        );

        // Input: u
        flat.add_variable(
            VarName::new("u"),
            flat::Variable {
                name: VarName::new("u"),
                causality: rumoca_ir_core::Causality::Input(Default::default()),
                is_primitive: true,
                ..Default::default()
            },
        );

        // Equation 1: der(x_scaled[1]) = u (1 scalar)
        flat.add_equation(make_residual_eq(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x_scaled[1]"),
                    subscripts: vec![],
                }],
            },
            Expression::VarRef {
                name: VarName::new("u"),
                subscripts: vec![],
            },
        ));

        // Equation 2: der(x_scaled[2:nx]) = x_scaled[1:(nx-1)] (range 2:2 = 1 scalar)
        flat.add_equation(make_residual_eq(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x_scaled[2:nx]"),
                    subscripts: vec![],
                }],
            },
            Expression::VarRef {
                name: VarName::new("x_scaled[1:(nx - 1)]"),
                subscripts: vec![],
            },
        ));

        let dae = to_dae(&flat).unwrap();

        // 2 unknowns: x_scaled (state with dims=[2])
        assert_eq!(dae.states.len(), 1, "should have 1 state variable");

        // der(x_scaled[2:nx]) where nx=size(a,1)-1=2, range 2:2 = 1 element
        assert_eq!(
            rumoca_eval_dae::analysis::balance(&dae),
            0,
            "der(x_scaled[2:nx]) with nx=size(a,1)-1=2 should count as 1 scalar; \
             f_x scalar counts: {:?}",
            dae.f_x.iter().map(|eq| eq.scalar_count).collect::<Vec<_>>()
        );
    }

    /// Range bound expressions like `(nx - 1)` in string form must be parsed
    /// as binary expressions to evaluate the range size correctly.
    ///
    /// Model structure (like FilterTests.AllOptions CriticalDamping):
    ///   - Parameter nx = 3 (size(a,1) - 1)
    ///   - State x with dims [3]
    ///   - Equation: der(x[1:(nx - 1)]) = zeros(nx - 1) (range 1:2 = 2 elements)
    #[test]
    fn t10h6_03_range_subscript_string_binary_expression() {
        let mut flat = Model::new();

        // Parameter: nx = 3
        flat.add_variable(
            VarName::new("nx"),
            flat::Variable {
                name: VarName::new("nx"),
                variability: Variability::Parameter(Default::default()),
                binding: Some(Expression::Literal(Literal::Integer(3))),
                is_primitive: true,
                ..Default::default()
            },
        );

        // State: x with dims [3]
        flat.add_variable(
            VarName::new("x"),
            flat::Variable {
                name: VarName::new("x"),
                dims: vec![3],
                is_primitive: true,
                ..Default::default()
            },
        );

        // Input: u
        flat.add_variable(
            VarName::new("u"),
            flat::Variable {
                name: VarName::new("u"),
                causality: rumoca_ir_core::Causality::Input(Default::default()),
                is_primitive: true,
                ..Default::default()
            },
        );

        // Equation 1: der(x[nx]) = u (1 scalar, last element)
        flat.add_equation(make_residual_eq(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x[nx]"),
                    subscripts: vec![],
                }],
            },
            Expression::VarRef {
                name: VarName::new("u"),
                subscripts: vec![],
            },
        ));

        // Equation 2: der(x[1:(nx - 1)]) = zeros(nx - 1) (range 1:2 = 2 scalars)
        flat.add_equation(make_residual_eq(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x[1:(nx - 1)]"),
                    subscripts: vec![],
                }],
            },
            Expression::BuiltinCall {
                function: BuiltinFunction::Zeros,
                args: vec![Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(Expression::VarRef {
                        name: VarName::new("nx"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(Expression::Literal(Literal::Integer(1))),
                }],
            },
        ));

        let dae = to_dae(&flat).unwrap();
        assert_eq!(dae.states.len(), 1, "should have 1 state variable");

        // der(x[1:(nx-1)]) where nx=3, range 1:2 = 2 elements
        // Total: 1 + 2 = 3 scalar equations for 3 scalar unknowns
        assert_eq!(
            rumoca_eval_dae::analysis::balance(&dae),
            0,
            "der(x[1:(nx-1)]) with nx=3 should count as 2 scalar equations; \
             f_x scalar counts: {:?}",
            dae.f_x.iter().map(|eq| eq.scalar_count).collect::<Vec<_>>()
        );
    }
}

// =============================================================================
// Summary Test
// =============================================================================
// Tier 10k: Subscripted connector connections and for-loop record field range
// =============================================================================

mod tier_10k_subscripted_connections {
    use super::*;

    /// MLS §9.1: When a connect statement uses a literal subscript on an array
    /// component (e.g., `connect(src.p, resistor[1].p)`), the flatten phase must
    /// resolve the per-element connection correctly.
    ///
    /// Pattern from Modelica.Electrical.Batteries: `connect(resistor[1].p, r0.n)`
    /// where `resistor` is `Resistor[nRC]`.
    #[test]
    fn t10k_01_subscripted_connector_connection() {
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

model Resistor
    Pin p;
    Pin n;
    parameter Real R = 1;
equation
    p.v - n.v = R * p.i;
    p.i + n.i = 0;
end Resistor;

model Source
    Pin p;
    Pin n;
    parameter Real V = 1;
equation
    p.v - n.v = V;
    p.i + n.i = 0;
end Source;

model Test
    Resistor resistor[2];
    Source src;
equation
    connect(src.p, resistor[1].p);
    connect(resistor[1].n, resistor[2].p);
    connect(resistor[2].n, src.n);
end Test;
"#;
        let r = assert_compiles(source, "Test");
        // 12 unknowns: 2 resistors * 4 pin vars + source * 4 pin vars
        // 12 equations: 3 body (2 per component) + 3 non-flow + 3 flow + 3 current conservation
        assert_eq!(
            r.balance, 0,
            "Subscripted connector connect(src.p, resistor[1].p) should work; balance={}",
            r.balance
        );
    }

    /// MLS §11.1.2: For-loop range referencing a record field parameter
    /// (e.g., `for k in 1:cellData.nRC loop connect(...[k]...)`) must evaluate
    /// the multi-part component reference `cellData.nRC` to resolve the range.
    ///
    /// Pattern from Modelica.Electrical.Batteries.BatteryStacks.CellRCStack
    #[test]
    fn t10k_02_forloop_record_field_range() {
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

model Resistor
    Pin p;
    Pin n;
    parameter Real R = 1;
equation
    p.v - n.v = R * p.i;
    p.i + n.i = 0;
end Resistor;

model Source
    Pin p;
    Pin n;
    parameter Real V = 1;
equation
    p.v - n.v = V;
    p.i + n.i = 0;
end Source;

record CellData
    parameter Integer nRC = 2;
end CellData;

model RCChain
    parameter CellData cellData;
    Resistor resistor[cellData.nRC];
    Source src;
equation
    connect(src.p, resistor[1].p);
    for k in 1:cellData.nRC loop
        if k < cellData.nRC then
            connect(resistor[k].n, resistor[k + 1].p);
        end if;
    end for;
    connect(resistor[cellData.nRC].n, src.n);
end RCChain;

model Test
    RCChain chain(cellData(nRC = 2));
end Test;
"#;
        let r = assert_compiles(source, "Test");
        assert_eq!(
            r.balance, 0,
            "For-loop with record field range cellData.nRC should expand correctly; balance={}",
            r.balance
        );
    }

    /// MLS §9.2 + §10.1: Array connector connect() must preserve element pairing.
    ///
    /// Pattern from `Modelica.Electrical.Polyphase.Basic.SplitToSubsystems`:
    /// `connect(plug_p.pin[(k-1)*mBasic + j], plugs_n[k].pin[j])`
    ///
    /// Regression: collapsed connector-array fields on one side were matched as
    /// indexless array members (e.g. `plugs_n.pin.i`), merging phase-wise
    /// connection sets and generating one flow sum instead of one per phase.
    #[test]
    fn t10k_03_split_to_subsystems_connector_array_pairing() {
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

connector Plug
    parameter Integer m = 3;
    Pin pin[m];
end Plug;

model SplitToSubsystemsLike
    parameter Integer m(min = 1) = 3;
    parameter Integer mSystems = 1;
    parameter Integer mBasic = integer(m / mSystems);
    Plug plug_p(m = m);
    Plug plugs_n[mSystems](each m = mBasic);
equation
    for k in 1:mSystems loop
        for j in 1:mBasic loop
            connect(plug_p.pin[(k - 1) * mBasic + j], plugs_n[k].pin[j]);
        end for;
    end for;
end SplitToSubsystemsLike;
"#;

        let r = assert_compiles(source, "SplitToSubsystemsLike");
        assert_eq!(r.balance, 0, "model should be balanced");

        // Expect one potential equality and one flow-sum per phase (m=3).
        let flow_sum_eqs = r
            .dae
            .f_x
            .iter()
            .filter(|eq| eq.origin.contains("flow sum equation"))
            .count();
        let connection_eq_scalars: usize = r
            .dae
            .f_x
            .iter()
            .filter(|eq| eq.origin.contains("connection equation"))
            .map(|eq| eq.scalar_count)
            .sum();
        assert_eq!(flow_sum_eqs, 3, "should generate one flow sum per phase");
        assert_eq!(
            connection_eq_scalars, 3,
            "should generate one potential equality per phase"
        );
    }

    /// MLS §9.2 + §10.1: Connecting many phases to one star point must not
    /// project source-side indices onto the fixed starpoint connector element.
    ///
    /// Pattern from `Modelica.Electrical.Polyphase.Basic.MultiStar`:
    /// `connect(plug_p.pin[(k-1)*mBasic + j], starpoints.pin[k])` with `k=1`.
    #[test]
    fn t10k_04_multi_star_many_to_one_connection() {
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

connector Plug
    parameter Integer m = 3;
    Pin pin[m];
end Plug;

model MultiStarLike
    parameter Integer m(min = 1) = 3;
    parameter Integer mSystems = 1;
    parameter Integer mBasic = integer(m / mSystems);
    Plug plug_p(m = m);
    Plug starpoints(m = mSystems);
equation
    for k in 1:mSystems loop
        for j in 1:mBasic loop
            connect(plug_p.pin[(k - 1) * mBasic + j], starpoints.pin[k]);
        end for;
    end for;
end MultiStarLike;
"#;

        let r = assert_compiles(source, "MultiStarLike");
        assert_eq!(r.balance, 0, "model should be balanced");

        let flow_sum_eqs = r
            .dae
            .f_x
            .iter()
            .filter(|eq| eq.origin.contains("flow sum equation"))
            .count();
        let connection_eq_scalars: usize = r
            .dae
            .f_x
            .iter()
            .filter(|eq| eq.origin.contains("connection equation"))
            .map(|eq| eq.scalar_count)
            .sum();
        assert_eq!(
            flow_sum_eqs, 1,
            "many-to-one starpoint connection should produce one flow sum"
        );
        assert_eq!(
            connection_eq_scalars, 3,
            "potential equalities should still be generated per phase"
        );
    }

    /// MLS §7.2 + §8.3.3 + §9.2 + §10.1:
    /// for-loop connect ranges using record fields must respect record rebinding
    /// (`cellData = cellData2`) and not fall back to record defaults.
    ///
    /// Regression from `Modelica.Electrical.Batteries.BatteryStacks.CellRCStack`:
    /// stale `cellData.nRC=1` caused only first array connection to be expanded.
    #[test]
    fn t10k_05_record_alias_for_loop_connect_range() {
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

model Resistor
    Pin p;
    Pin n;
equation
    p.v - n.v = 0;
    p.i + n.i = 0;
end Resistor;

model Source
    Pin p;
    Pin n;
equation
    p.v - n.v = 1;
    p.i + n.i = 0;
end Source;

record CellData
    parameter Integer nRC = 1;
end CellData;

model RCChainAlias
    parameter CellData cellData;
    Resistor resistor[cellData.nRC];
    Source src;
equation
    connect(src.p, resistor[1].p);
    for k in 1:cellData.nRC loop
        if k < cellData.nRC then
            connect(resistor[k].n, resistor[k + 1].p);
        end if;
    end for;
    connect(resistor[cellData.nRC].n, src.n);
end RCChainAlias;

model TestAlias
    parameter CellData cellData2(nRC = 2);
    RCChainAlias chain(cellData = cellData2);
end TestAlias;
"#;
        let r = assert_compiles(source, "TestAlias");
        assert_eq!(
            r.balance, 0,
            "record alias-based connect for-loop range should fully expand; balance={}",
            r.balance
        );
    }

    /// Regression from `Modelica.Electrical.Batteries.Examples.BatteryDischargeCharge`:
    /// redeclared record parameter + outer rebinding (`battery2(cellData=cellData2)`)
    /// must not qualify the alias target as `battery2.cellData.cellData2`.
    #[test]
    fn t10k_05b_redeclare_record_rebinding_scope() {
        let source = r#"
package ParameterRecords
  record CellData
    parameter Integer nRC = 1;
  end CellData;

  package TransientData
    record CellData
      extends ParameterRecords.CellData(nRC = 2);
    end CellData;
  end TransientData;
end ParameterRecords;

partial model BaseCellStack
  replaceable parameter ParameterRecords.CellData cellData;
end BaseCellStack;

model CellRCStack
  extends BaseCellStack(
    redeclare ParameterRecords.TransientData.CellData cellData);
  Real x[cellData.nRC];
equation
  for k in 1:cellData.nRC loop
    x[k] = k;
  end for;
end CellRCStack;

model Top
  parameter ParameterRecords.TransientData.CellData cellData2(nRC = 2);
  CellRCStack battery2(cellData = cellData2);
end Top;
"#;

        let r = assert_compiles(source, "Top");
        assert_eq!(
            r.balance, 0,
            "redeclare record rebinding should compile without unresolved refs; balance={}",
            r.balance
        );
    }

    /// MLS §8.4 + §10.1: Aggregate connector field equations over arrays
    /// (e.g. `pin_n.v = plug_n.pin.v`) must contribute one scalar equation per phase.
    ///
    /// Pattern from Modelica.Electrical.Polyphase.Basic.PlugToPins_n.
    #[test]
    fn t10k_06_connector_field_array_alias_equations() {
        let source = r#"
connector Pin
    Real v;
    flow Real i;
end Pin;

connector Plug
    parameter Integer m = 3;
    Pin pin[m];
end Plug;

model PlugToPinsNLike
    parameter Integer m(min = 1) = 3;
    Plug plug_n(m = m);
    Pin pin_n[m];
equation
    pin_n.v = plug_n.pin.v;
    plug_n.pin.i = -pin_n.i;
end PlugToPinsNLike;
"#;

        let r = assert_compiles(source, "PlugToPinsNLike");
        assert_eq!(
            r.balance, 0,
            "aggregate connector-field equations should balance"
        );
        let total_scalar_eq: usize = r.dae.f_x.iter().map(|eq| eq.scalar_count).sum();
        assert_eq!(
            total_scalar_eq, 12,
            "m=3 should yield 12 scalar equations (including unconnected flows)"
        );
    }

    /// Regression pattern from Electrical.Batteries.Utilities.BusTranscription:
    /// an output connector member defined by a component equation and additionally
    /// connected to a non-unknown expandable-bus member must not be double-counted.
    #[test]
    fn t10k_07_expandable_bus_output_connection_not_double_counted() {
        let source = r#"
connector RealInput = input Real;
connector RealOutput = output Real;

expandable connector BusArr
    Real x[1,1];
end BusArr;

block GainR
    RealInput u;
    RealOutput y;
equation
    y = u;
end GainR;

model MiniBusTranscriptionArr
    BusArr inBus;
    BusArr outBus;
protected
    GainR g[1,1];
equation
    connect(g.u, inBus.x);
    connect(g.y, outBus.x);
end MiniBusTranscriptionArr;
"#;

        let r = assert_compiles(source, "MiniBusTranscriptionArr");
        assert_eq!(
            r.balance, 0,
            "output-to-bus connection should not overconstrain when output already has a defining equation"
        );
    }
}

// =============================================================================
// Tier 10L: If-equations inside for-loops (MLS §8.3.3 + §8.3.4)
// =============================================================================

mod tier_10l_if_in_for {
    use super::*;

    /// MLS §8.3.3 + §8.3.4: When a for-equation contains an if-equation,
    /// the loop index variable must be substituted into the if-equation's
    /// conditions and all branch equations before expansion.
    /// Models the ThreeTanks pattern: for i in 1:n loop if cond[i] then ... end if; end for;
    /// Uses a dynamic (non-parameter) Boolean condition so the if-equation is NOT
    /// resolved at compile time and must remain as a conditional expression.
    #[test]
    fn t10l_01_if_equation_in_for_loop() {
        let source = r#"
model Tank
    parameter Integer nPorts = 2;
    Real s[nPorts];
    Real p[nPorts];
    input Boolean regularFlow[nPorts];
equation
    for i in 1:nPorts loop
        if regularFlow[i] then
            s[i] = p[i] * 2;
            p[i] = i;
        else
            s[i] = 0;
            p[i] = 0;
        end if;
    end for;
end Tank;

model Test
    Tank tank;
end Test;
"#;
        let r = assert_compiles(source, "Test");
        assert_eq!(
            r.balance, 0,
            "If-equation inside for-loop: index must be substituted in all branches; balance={}",
            r.balance
        );
    }

    /// MLS §8.3.4: If-equation with mismatched branch equation counts where
    /// condition is a parameter. This pattern is common in MSL (e.g.,
    /// PartialElementaryTwoFlangesAndSupport2: if not useSupport then phi_support = 0; end if)
    #[test]
    fn t10l_02_mismatched_if_branches_parameter_condition() {
        let source = r#"
model Base
    parameter Boolean useSupport = false;
    Real phi;
    Real phi_support;
equation
    phi = 1;
    if not useSupport then
        phi_support = 0;
    end if;
end Base;

model Test
    Base b1(useSupport = false);
    Base b2(useSupport = true);
    Real phi_support2;
equation
    phi_support2 = b2.phi;
    b2.phi_support = 0;
end Test;
"#;
        let r = assert_compiles(source, "Test");
        assert_eq!(
            r.balance, 0,
            "Mismatched if-equation branches with parameter condition should compile; balance={}",
            r.balance
        );
    }

    /// MLS §8.3.4: Same pattern but with extends chain. The if-equation
    /// is in a base class, and the parameter is modified in the instantiation.
    /// This matches the TestBearingConversion pattern more closely.
    #[test]
    fn t10l_03_mismatched_if_branches_extends_chain() {
        let source = r#"
partial model PartialSupport
    parameter Boolean useSupport = false;
    Real phi;
    Real phi_support;
equation
    if not useSupport then
        phi_support = 0;
    end if;
end PartialSupport;

model Gear
    extends PartialSupport;
equation
    phi = 1;
end Gear;

model Test
    Gear g1(useSupport = false);
    Gear g2(useSupport = true);
    Real phi_support2;
equation
    phi_support2 = g2.phi;
    g2.phi_support = 0;
end Test;
"#;
        let r = assert_compiles(source, "Test");
        assert_eq!(
            r.balance, 0,
            "Mismatched if with extends chain should compile; balance={}",
            r.balance
        );
    }

    /// MLS §8.3.4: If-equation where one branch uses a whole-array assignment
    /// and the other uses a for-loop. Both have the same scalar count but
    /// different SimpleEquation counts. This models the LossyGear pattern.
    #[test]
    fn t10l_04_array_vs_forloop_in_if_branches() {
        let source = r#"
model ArrayIf
    parameter Integer n = 4;
    Real x[n];
    input Boolean useDefault;
equation
    if useDefault then
        x = {1, 2, 3, 4};
    else
        for i in 1:n loop
            x[i] = i * 2;
        end for;
    end if;
end ArrayIf;

model Test
    ArrayIf a;
end Test;
"#;
        let r = assert_compiles(source, "Test");
        assert_eq!(
            r.balance, 0,
            "Array vs for-loop if-branches should produce same scalar count; balance={}",
            r.balance
        );
    }
}

// =============================================================================
// Tier 10m: Inner outer bridge flow scoping (MLS §5.4)
// =============================================================================

mod tier_10m_inner_outer_bridge {
    use super::*;

    /// MLS §5.4: When a component is declared `inner outer`, it bridges two
    /// scopes for connection purposes. Same-level connections to the `inner outer`
    /// component should generate flow sums at the parent scope (outer facet),
    /// while child components' outer references connect to the inner facet.
    ///
    /// Pattern from Modelica.StateGraph.PartialCompositeStep:
    ///   inner outer CompositeStepState stateGraphRoot;  // bridges parent↔child
    ///   OuterState outerState;                          // communicates up to parent
    ///   connect(outerState.port, stateGraphRoot.port);  // should use outer facet
    ///   // Children connect via: outer CompositeStepState stateGraphRoot;
    #[test]
    fn t10m_01_inner_outer_bridge_flow_scope() {
        let source = r#"
connector SubPort
    Real dummy;
    flow Real steps;
end SubPort;

model CompositeStepState
    SubPort port;
equation
    port.dummy = 0;
end CompositeStepState;

model Step
    outer CompositeStepState stateGraphRoot;
    SubPort outerStatePort;
equation
    outerStatePort.steps = 1.0;
    connect(outerStatePort, stateGraphRoot.port);
end Step;

model OuterState
    SubPort port;
equation
    port.steps = 1.0;
end OuterState;

model CompositeStep
    inner outer CompositeStepState stateGraphRoot;
    OuterState outerState;
    Step stepA;
    Step stepB;
equation
    connect(outerState.port, stateGraphRoot.port);
end CompositeStep;

model Top
    inner CompositeStepState stateGraphRoot;
    CompositeStep composite;
    Step topStep;
end Top;
"#;
        let r = assert_compiles(source, "Top");
        // Without inner_outer bridge fix: balance=+1 (extra flow sum equation)
        // The connect(outerState.port, stateGraphRoot.port) inside CompositeStep
        // should redirect stateGraphRoot to the parent's inner (top-level) scope.
        // This merges outerState into the top-level flow sum instead of creating
        // a separate 2-term flow sum at the CompositeStep scope.
        //
        // Expected flow sums:
        //   Root scope: stateGraphRoot.port.steps + topStep.outerStatePort.steps
        //             + composite.outerState.port.steps = 0  (3 terms)
        //   CompositeStep scope: composite.stateGraphRoot.port.steps
        //             + composite.stepA.outerStatePort.steps
        //             + composite.stepB.outerStatePort.steps = 0  (3 terms)
        assert_eq!(
            r.balance, 0,
            "Inner outer bridge: same-level connect should scope flow to parent; balance={}",
            r.balance
        );
    }
}

// =============================================================================

#[test]
fn tier_summary() {
    println!("\n=== Tiered Model Test Summary ===\n");
    println!("Tier 0 (Minimal):        Basic model structure");
    println!("Tier 1 (Equations):      Algebraic equations");
    println!("Tier 2 (ODEs):           Differential equations");
    println!("Tier 3 (Parameters):     Parameters and constants");
    println!("Tier 4 (Arrays):         For-equations with arrays");
    println!("Tier 5 (Conditionals):   If-equations, when-equations, pre/edge/change");
    println!("Tier 6 (Functions):      Built-in math functions (sin, cos, sqrt, abs)");
    println!("Tier 7 (Components):     Component instantiation, modifications, connectors");
    println!("Tier 8 (Inheritance):    Simple extends");
    println!("Tier 9 (Advanced):       Algorithm sections");
}
