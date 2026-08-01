//! CONN (Connection) contract tests - MLS §9
//!
//! Tests for the 29 connection contracts defined in SPEC_0022.

use rumoca_compile::compile::FailedPhase;
use rumoca_compile::{Session, SessionConfig};
use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_resolve_failure_with_code,
    expect_success,
};

// =============================================================================
// CONN-001: Homogeneity
// "Connection set shall contain either only flow or only non-flow variables"
// =============================================================================

#[test]
fn conn_001_flow_connects_ok() {
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin p1;
            Pin p2;
            Real v_offset;
            Real i_offset;
        equation
            connect(p1, p2);
            p1.v + v_offset = 1.0;
            p2.i + i_offset = 0.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CONN-002: Type matching
// "Matched primitive components must have the same primitive types"
// =============================================================================

#[test]
fn conn_002_same_type_connectors() {
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin a;
            Pin b;
            Real v_offset;
            Real i_offset;
        equation
            connect(a, b);
            a.v + v_offset = 1.0;
            b.i + i_offset = 0.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_002_type_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector RealOutput = output Real;
        connector BoolInput = input Boolean;

        model Test
            RealOutput a;
            BoolInput b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF002",
    );
}

// =============================================================================
// CONN-003: Flow-to-flow
// "Flow variables may only connect to other flow variables"
// =============================================================================

#[test]
fn conn_003_flow_to_flow_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector FlowOnly
            Real v;
            flow Real i;
        end FlowOnly;

        connector PotentialOnly
            Real v;
            Real i;
        end PotentialOnly;

        model Test
            FlowOnly a;
            PotentialOnly b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF002",
    );
}

// =============================================================================
// CONN-007: Connector not parameter
// "Connector component shall not be declared with parameter or constant"
// =============================================================================

#[test]
fn conn_007_no_parameter_connector() {
    expect_resolve_failure_with_code(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            parameter Pin p;
        equation
        end Test;
    "#,
        "Test",
        "ER027",
    );
}

// =============================================================================
// CONN-009 / CONN-010: Expandable connector restrictions
// =============================================================================

#[test]
fn conn_009_expandable_connector_rejects_flow_member() {
    expect_resolve_failure_with_code(
        r#"
        expandable connector Bus
            flow Real i;
        end Bus;

        model Test
            Bus bus;
        equation
        end Test;
    "#,
        "Test",
        "ER058",
    );
}

#[test]
fn conn_010_expandable_connector_requires_expandable_peer() {
    expect_resolve_failure_with_code(
        r#"
        expandable connector Bus
            Real v;
        end Bus;

        connector Pin
            Real v;
        end Pin;

        model Test
            Bus bus;
            Pin pin;
        equation
            connect(bus, pin);
        end Test;
    "#,
        "Test",
        "ER059",
    );
}

// =============================================================================
// CONN-017: Balance flow = potential
// "For non-partial non-simple non-expandable connector: number of flow = number of potential"
// =============================================================================

#[test]
fn conn_017_balanced_connector() {
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin p;
            Real v_bias;
        equation
            p.v + v_bias = 1.0;
            p.i = 0.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_017_unbalanced_connector_fails() {
    expect_resolve_failure_with_code(
        r#"
        connector BadPin
            Real v;
            Real w;
            flow Real i;
        end BadPin;
        model Test
            BadPin p;
        equation
        end Test;
    "#,
        "Test",
        "ER028",
    );
}

// =============================================================================
// CONN-026: Flow sign convention
// "Flow sign is +1 for inside connectors and -1 for outside connectors"
// =============================================================================

#[test]
fn conn_026_flow_sign_basic() {
    // Basic resistor model: flow conservation is handled by connect
    expect_balanced(
        r#"
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
    "#,
        "Resistor",
    );
}

// =============================================================================
// CONN-029: Connect arguments are connectors
// "Both arguments of connect must be connector references"
// =============================================================================

#[test]
fn conn_029_connect_requires_connectors() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x;
            Real y;
        equation
            connect(x, y);
        end Test;
    "#,
        "Test",
        "ER009",
    );
}

// =============================================================================
// Connection integration tests
// =============================================================================

#[test]
fn conn_series_resistors() {
    expect_balanced(
        r#"
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
        model Ground
            Pin p;
        equation
            p.v = 0;
        end Ground;
        model Source
            Pin p;
            Pin n;
            parameter Real V = 1;
        equation
            p.v - n.v = V;
            p.i + n.i = 0;
        end Source;
        model Test
            Resistor r1(R = 100);
            Resistor r2(R = 200);
            Source src(V = 10);
            Ground gnd;
        equation
            connect(src.p, r1.p);
            connect(r1.n, r2.p);
            connect(r2.n, src.n);
            connect(src.n, gnd.p);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_008_same_dimensions() {
    // Connectors with same structure should connect successfully
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin a;
            Pin b;
            Real v_offset;
            Real i_offset;
        equation
            connect(a, b);
            a.v + v_offset = 1;
            b.i + i_offset = 0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_008_dimension_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector Vec2
            Real v[2];
        end Vec2;

        connector Vec3
            Real v[3];
        end Vec3;

        model Test
            Vec2 a;
            Vec3 b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF002",
    );
}

// =============================================================================
// CONN-027: potentialRoot priority
// "Priority p for potentialRoot must be p >= 0"
// (flatten requires an evaluable non-negative integer literal)
// =============================================================================

#[test]
fn conn_027_negative_potential_root_priority_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            Real e;
            flow Real f;
        end C;
        model Test
            C c1;
            C c2;
        equation
            Connections.potentialRoot(c1, -1);
            connect(c1, c2);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// CONN-028: Parameter/constant variability
// "Primitive components may only connect parameter to parameter and
//  constant to constant" (connector components cannot be parameters)
// =============================================================================

#[test]
fn conn_028_parameter_connector_component_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector C = Real;
        model Test
            parameter C p = 1;
            C v;
        equation
            connect(p, v);
        end Test;
    "#,
        "Test",
        "ER027",
    );
}

// =============================================================================
// CONN-023: Overconstrained not in function
// "None of these operators allowed inside function classes" (MLS §9.4)
// =============================================================================

#[test]
fn conn_023_connections_root_in_function_rejected() {
    expect_resolve_failure_with_code(
        r#"
        function F
            output Real y;
        algorithm
            y := 1;
            Connections.root("x");
        end F;
        model Test
            Real x(start = 0);
        equation
            der(x) = F();
        end Test;
    "#,
        "Test",
        "ER056",
    );
}

// =============================================================================
// CONN-011: At least one connector must reference a declared component
// =============================================================================

#[test]
fn conn_011_expandable_connect_neither_declared_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            expandable connector Bus
            end Bus;
            Bus b1;
            Bus b2;
        equation
            connect(b1.sig, b2.sig);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF020",
    );
}

#[test]
fn conn_011_declared_expandable_member_is_not_treated_as_virtual() {
    expect_success(
        r#"
        partial model M
            expandable connector Bus
                Real sig;
            end Bus;
            Bus b1;
            Bus b2;
        equation
            connect(b1.sig, b2.sig);
        end M;
    "#,
        "M",
    );
}

#[test]
fn conn_011_empty_expandable_buses_can_connect_without_member_synthesis() {
    expect_success(
        r#"
        model M
            expandable connector Bus
            end Bus;
            Bus b1;
            Bus b2;
        equation
            connect(b1, b2);
        end M;
    "#,
        "M",
    );
}

// =============================================================================
// CONN-019: Subscripts shall be evaluable expressions or special operator :
// =============================================================================

// The accepted case first (SPEC_0008): a literal subscript on an array of a
// *simple* connector is evaluable, so `connect(a, gate.x[1])` is a connection
// to one element of `gate.x`. A simple connector has no members to expand, so
// the flat model declares the array once, with its dimension intact, and owns
// nothing named `gate.x[1]`. The generated connection equation therefore has to
// reach the DAE as the declared coordinate carrying a subscript; a reference
// whose *name* embedded the index would name no declaration and be rejected as
// an unresolved Flat reference.
#[test]
fn conn_019_connect_to_array_connector_element_accepted() {
    expect_success(
        r#"
        connector RealInput = input Real;
        connector RealOutput = output Real;
        model Gate
            RealInput x[2];
            RealOutput y;
        equation
            y = x[1] + x[2];
        end Gate;
        model M
            Gate gate;
            RealOutput a;
            RealOutput b;
            Real probe;
        equation
            connect(a, gate.x[1]);
            connect(b, gate.x[2]);
            probe = gate.y;
            a = 1.0;
            b = 2.0;
        end M;
    "#,
        "M",
    );
}

// The same element connection one level deeper: the composite that owns the
// element connection is itself a component, which is the shape that reaches
// flattening as a nested rendered path.
#[test]
fn conn_019_nested_connect_to_array_connector_element_accepted() {
    expect_success(
        r#"
        connector RealInput = input Real;
        connector RealOutput = output Real;
        model Gate
            RealInput x[2];
            RealOutput y;
        equation
            y = x[1] + x[2];
        end Gate;
        model Adder
            Gate gate;
            RealInput u;
            RealOutput c;
        equation
            connect(u, gate.x[2]);
            gate.x[1] = 1.0;
            c = gate.y;
        end Adder;
        model M
            Adder adder;
            Real probe;
        equation
            adder.u = 2.0;
            probe = adder.c;
        end M;
    "#,
        "M",
    );
}

// An endpoint subscript may leave dimensions behind. MLS §10.5: a subscript
// consumes one leading declared dimension, so `snk.u[1]` of a `Real[2,3]`
// declaration denotes `Real[3]` and connects to another `Real[3]`. MLS §9.2
// generates one equality per scalar leaf and MLS §4.8 counts those scalars, so
// the connection contributes three equations, not one. Counting a subscripted
// endpoint as a single scalar leaves this legal model short of equations and
// gets it rejected as unbalanced. OMC (devshell build a96aa1a) `checkModel(M)` reports "14
// equation(s) and 14 variable(s)" and simulates it to snk.s = 7.0.
#[test]
fn conn_019_connect_to_array_connector_slice_is_balanced() {
    expect_balanced(
        r#"
        connector RealInput = input Real;
        connector RealOutput = output Real;
        model Src
            RealOutput y[2,3];
        equation
            y = {{1.0,2.0,3.0},{4.0,5.0,6.0}};
        end Src;
        model Snk
            RealInput u[2,3];
            RealOutput s;
        equation
            s = u[1,1] + u[2,3];
        end Snk;
        model M
            Src src;
            Snk snk;
            Real probe;
        equation
            connect(snk.u[1], src.y[1]);
            connect(snk.u[2], src.y[2]);
            probe = snk.s;
        end M;
    "#,
        "M",
    );
}

// The counterpart rejection (CONN-008, MLS §9.2 "same named elements with the
// same dimensions"): `snk.u[1]` denotes `Real[2]` while `src.y[1]` denotes
// `Real[3]`, so the connection is genuinely unbalanced and keeps the typed
// incompatible-connector error. OMC (devshell build a96aa1a) rejects the same model with "The
// connectors in connect(snk.u[1], src.y[1]) are not type compatible."
#[test]
fn conn_008_connect_array_connector_slice_shape_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector RealInput = input Real;
        connector RealOutput = output Real;
        model Src
            RealOutput y[2,3];
        equation
            y = {{1.0,2.0,3.0},{4.0,5.0,6.0}};
        end Src;
        model Snk
            RealInput u[2,2];
            RealOutput s;
        equation
            s = u[1,1] + u[2,2];
        end Snk;
        model M
            Src src;
            Snk snk;
            Real probe;
        equation
            connect(snk.u[1], src.y[1]);
            probe = snk.s;
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF002",
    );
}

// MLS §10.5 gives a subscript no dimension to select along when the declaration
// has none, so `connect(a[1], b)` on a scalar connector `a` is an error, not a
// connection of the whole of `a`. OMC (devshell build a96aa1a) rejects it with "Wrong number of
// subscripts in a[1] (1 subscripts for 0 dimensions)".
#[test]
fn conn_019_connect_subscript_on_dimensionless_connector_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            Real e;
            flow Real f;
        end C;
        model M
            C a;
            C b;
        equation
            connect(a[1], b);
            a.e = 1.0;
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF026",
    );
}

// MLS §7.3 allows an extends-modification to redeclare a component together
// with array dimensions the base declaration did not have. OMC accepts this
// model. Rumoca's instantiation keeps only the redeclared *type*, so `a`
// reaches flatten carrying the base declaration's rank of zero — a fact about
// this compiler, not about the source. Whatever else this model does
// downstream, the connection phase must not blame the source for it: the
// rank-zero endpoint check is suppressed when the rank is not authoritative.
#[test]
fn conn_019_redeclared_array_dimensions_are_not_reported_as_a_dimensionless_connector() {
    let source = r#"
        connector C
            Real e;
            flow Real f;
        end C;
        model Base
            replaceable C a;
        end Base;
        model Drv
            C p[2];
            Real s;
        equation
            s = p[1].e + p[2].e;
        end Drv;
        model M
            extends Base(redeclare C a[2]);
            Drv d;
            Real probe;
        equation
            connect(a[1], d.p[1]);
            connect(a[2], d.p[2]);
            probe = d.s;
        end M;
    "#;
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse redeclared-dimension model");
    let codes: Vec<String> = session
        .compile_model_diagnostics("M")
        .diagnostics
        .iter()
        .filter_map(|diagnostic| diagnostic.code.clone())
        .collect();
    // The connection phase abstains, so what is left is the instantiate gap
    // itself: the redeclared dimensions never arrive, `a` stays a scalar, and
    // the model is short two equations. Asserting that exact outcome keeps this
    // from passing vacuously on some third result, and makes it fail loudly if
    // the redeclare-dimension gap is ever closed (then this model compiles and
    // this expectation should become `expect_balanced`).
    assert!(
        !codes.iter().any(|code| code.ends_with("EF026")),
        "a rank dropped by the redeclare-dimension gap must not be reported as a \
         dimensionless connector, got codes: {codes:?}"
    );
    assert!(
        codes.iter().any(|code| code.ends_with("ED001")),
        "the surviving failure must be the unbalanced-model report caused by the \
         dropped redeclare dimensions, got codes: {codes:?}"
    );
}

// Acceptance before rejection: the subscript budget comes from the declaration,
// so an element of a parameter-sized connector array stays a legal connect
// argument. OMC (devshell build a96aa1a) `checkModel(M)` reports "6 equation(s) and 6 variable(s)".
#[test]
fn conn_019_connect_to_parameter_sized_connector_array_element_accepted() {
    expect_balanced(
        r#"
        connector RealInput = input Real;
        connector RealOutput = output Real;
        model Gate
            parameter Integer n = 2;
            RealInput x[n];
            RealOutput y;
        equation
            y = x[1] + x[2];
        end Gate;
        model M
            Gate gate;
            RealOutput a;
            RealOutput b;
            Real probe;
        equation
            connect(a, gate.x[1]);
            connect(b, gate.x[2]);
            probe = gate.y;
            a = 1.0;
            b = 2.0;
        end M;
    "#,
        "M",
    );
}

#[test]
fn conn_019_connect_subscript_not_evaluable_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            connector C
                Real e;
                flow Real f;
            end C;
            C a[2];
            C b;
            Integer i(start = 1);
        equation
            i = if time > 1 then 1 else 2;
            connect(a[i], b);
        end M;
    "#,
        "M",
        "ER085",
    );
}

// =============================================================================
// CONN-004: At most one inside output connector or one public outside input
// connector per connection set
// =============================================================================

#[test]
fn conn_004_two_inside_outputs_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            connector RealOutput = output Real;
            block Src
                RealOutput y;
            equation
                y = time;
            end Src;
            Src s1;
            Src s2;
        equation
            connect(s1.y, s2.y);
        end M;
    "#,
        "M",
        "ER101",
    );
}

// =============================================================================
// CONN-006: Cannot connect two connectors of outer elements
// =============================================================================

#[test]
fn conn_006_outer_outer_connect_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            connector C
                Real e;
                flow Real f;
            end C;
            model Inner
                outer C c1;
                outer C c2;
            equation
                connect(c1, c2);
            end Inner;
            inner C c1;
            inner C c2;
            Inner sub;
        end M;
    "#,
        "M",
        "ER103",
    );
}

// =============================================================================
// CONN-013: Every subgraph shall have at least one definite or potential root
// node
// =============================================================================

#[test]
fn conn_013_branch_without_root_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            connector Frame
                Real r[3];
                flow Real f[3];
            end Frame;
            model Body
                Frame frame_a;
            equation
                frame_a.r = {0, 0, 0};
            end Body;
            Body b1;
            Body b2;
        equation
            Connections.branch(b1.frame_a, b2.frame_a);
            connect(b1.frame_a, b2.frame_a);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF004",
    );
}

// =============================================================================
// CONN-018: Simple connector components must be declared as input, output, or
// protected
// =============================================================================

#[test]
fn conn_018_block_simple_connector_without_prefix_rejected() {
    expect_resolve_failure_with_code(
        r#"
        block B
            connector C = Real;
            C c;
        equation
            c = time;
        end B;
    "#,
        "B",
        "ER020",
    );
}

// =============================================================================
// CONN-020: Sizeless array component shall not be used without subscripts
// =============================================================================

#[test]
fn conn_020_sizeless_array_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            Real x[:];
        equation
            x[1] = 1;
        end M;
    "#,
        "M",
        FailedPhase::Typecheck,
        "ET004",
    );
}

// =============================================================================
// CONN-014: Cycle among required spanning-tree-edges is error
// =============================================================================

#[test]
fn conn_014_branch_cycle_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            connector Frame
                Real r;
                flow Real f;
            end Frame;
            Frame a;
            Frame b;
            Frame c;
        equation
            Connections.root(a);
            Connections.branch(a, b);
            Connections.branch(b, c);
            Connections.branch(c, a);
            a.r = 0;
            b.r = 0;
            c.r = 0;
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF022",
    );
}

// =============================================================================
// CONN-015: Error if two definite root nodes connected through required
// spanning tree edges
// =============================================================================

#[test]
fn conn_015_two_connected_definite_roots_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            connector Frame
                Real r;
                flow Real f;
            end Frame;
            Frame a;
            Frame b;
        equation
            Connections.root(a);
            Connections.root(b);
            Connections.branch(a, b);
            a.r = 0;
            b.r = 0;
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF022",
    );
}

// =============================================================================
// CONN-016: Protected outside connector must connect to inside or public
// outside connector
// =============================================================================

#[test]
fn conn_016_connect_to_protected_connector_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            connector C
                Real e;
                flow Real f;
            end C;
            model Sub
                C inside;
            protected
                C hidden;
            equation
                connect(inside, hidden);
            end Sub;
            Sub s;
            C top;
        equation
            connect(s.hidden, top);
        end M;
    "#,
        "M",
        "ER113",
    );
}

// =============================================================================
// CONN-005: Variables with non-empty quantity attribute must match
// =============================================================================

#[test]
fn conn_005_quantity_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            connector CA
                Real e(quantity = "Voltage");
                flow Real f;
            end CA;
            connector CB
                Real e(quantity = "Pressure");
                flow Real f;
            end CB;
            CA a;
            CB b;
        equation
            connect(a, b);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF002",
    );
}

// =============================================================================
// CONN-024: equalityConstraint function shall have specified prototype
// =============================================================================

#[test]
fn conn_024_equality_constraint_bad_prototype_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            record Orientation
                Real T[3, 3];
                encapsulated function equalityConstraint
                    input Real a;
                    output Real residue[3];
                algorithm
                    residue := {0, 0, 0};
                end equalityConstraint;
            end Orientation;
            Real x = 1;
        end M;
    "#,
        "M",
        "ER117",
    );
}

// =============================================================================
// CONN-025: Array dimension n shall be constant Integer expression evaluable
// during translation, n >= 0
// =============================================================================

#[test]
fn conn_025_equality_constraint_noneval_dimension_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            record Orientation
                Real T[3, 3];
                encapsulated function equalityConstraint
                    input Orientation a;
                    input Orientation b;
                    output Real residue[size(a.T, 1)];
                algorithm
                    residue := {0, 0, 0};
                end equalityConstraint;
            end Orientation;
            Real x = 1;
        end M;
    "#,
        "M",
        "ER117",
    );
}

// =============================================================================
// CONN-022: Overdetermined type/record may not have flow components
// =============================================================================

#[test]
fn conn_022_overdetermined_type_with_flow_member_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model M
            record Orientation
                Real T[3, 3];
                flow Real f;
                encapsulated function equalityConstraint
                    input Orientation a;
                    input Orientation b;
                    output Real residue[3];
                algorithm
                    residue := {0, 0, 0};
                end equalityConstraint;
            end Orientation;
            Real x = 1;
        end M;
    "#,
        "M",
        "ER118",
    );
}

// =============================================================================
// CONN-012/021: expandable connector causality deduction. Member synthesis
// from component connects is not implemented yet, so these models are rejected
// explicitly at the pre-connection-set augmentation boundary.
// =============================================================================

#[test]
fn unsupported_expandable_duplicate_sources_fail_closed() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            expandable connector Bus
            end Bus;
            connector RealOutput = output Real;
            block Src
                RealOutput y;
            equation
                y = time;
            end Src;
            Bus bus;
            Src s1;
            Src s2;
        equation
            connect(s1.y, bus.sig);
            connect(s2.y, bus.sig);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF020",
    );
}

#[test]
fn unsupported_expandable_input_without_source_fails_closed() {
    expect_failure_in_phase_with_code(
        r#"
        model M
            expandable connector Bus
            end Bus;
            connector RealInput = input Real;
            block Sink
                RealInput u;
            end Sink;
            Bus bus;
            Sink k;
        equation
            connect(bus.sig, k.u);
        end M;
    "#,
        "M",
        FailedPhase::Flatten,
        "EF020",
    );
}
