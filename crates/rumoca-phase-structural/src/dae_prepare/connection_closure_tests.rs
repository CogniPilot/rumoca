//! Index reduction across connect-generated rows.
//!
//! A connect-generated flow sum arrives as a bare sum residual with no
//! left-hand side (`p.i + n.i`), and a connector node states the same flow
//! through several rows at once. Both shapes decide whether index reduction
//! can follow a constraint through a connection, and both are easy to get
//! wrong in opposite directions — see the two demotion tests below.

use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("connection_closure_test.mo"),
        1,
        2,
    )
}

fn test_variable(name: &str) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn var_index(name: &str, index: i64) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![Subscript::Index {
            value: index,
            span: test_span(),
        }],
        span: test_span(),
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
        span: test_span(),
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn add(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn mul(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn eq(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(),
        origin: "top-level model equation".to_string(),
        scalar_count: 1,
    }
}

fn flow_sum_eq(rhs: Expression) -> Equation {
    Equation {
        origin: "flow sum equation: p.i + n.i = 0".to_string(),
        ..eq(rhs)
    }
}

#[test]
fn only_the_differentiable_index_inverts_a_bare_sum_residual() {
    let mut dae = Dae::new();
    for name in ["p.i", "n.i"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    dae.continuous
        .equations
        .push(flow_sum_eq(add(var("p.i"), var("n.i"))));

    let narrow = collect_residual_defining_expr_index(&dae);
    assert!(
        !narrow.contains_key("p.i"),
        "state selection must not read a flow sum as the definition of one flow"
    );

    let wide = collect_differentiable_defining_expr_index(&dae);
    assert!(
        wide.contains_key("p.i") && wide.contains_key("n.i"),
        "differentiation must be able to solve a flow sum for either flow"
    );
}

#[test]
fn differentiable_index_tracks_exact_array_coordinates() {
    let mut dae = Dae::new();
    let mut coordinates = test_variable("coordinates");
    coordinates.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("coordinates"), coordinates);
    for name in ["left", "right"] {
        dae.variables
            .parameters
            .insert(VarName::new(name), test_variable(name));
    }
    dae.continuous
        .equations
        .push(eq(sub(var_index("coordinates", 1), var("left"))));
    dae.continuous
        .equations
        .push(eq(sub(var_index("coordinates", 2), var("right"))));

    let index = collect_differentiable_defining_expr_index(&dae);

    assert!(
        index.contains_key("coordinates[1]") && index.contains_key("coordinates[2]"),
        "each in-bounds scalar coordinate must retain its own defining row"
    );
    assert!(
        !index.contains_key("coordinates"),
        "component rows cannot certify a definition for the aggregate owner"
    );
}

/// The DC-machine excitation node, reduced to its structural skeleton.
///
/// `psi_e = Le*ie` makes the flux a constrained state, and the current is
/// pinned by a constant source on the other side of a connection:
/// `ie = pin.i`, `pin.i + src.i = 0`, `src.i = I`. Following that chain is the
/// whole point of index reduction here; `der(psi_e)` is zero once it closes.
fn excitation_node_dae() -> Dae {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("psi_e"), test_variable("psi_e"));
    for name in ["ie", "pin.i", "src.i", "vei"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    dae.variables
        .parameters
        .insert(VarName::new("Le"), test_variable("Le"));
    dae.variables
        .parameters
        .insert(VarName::new("I"), test_variable("I"));

    dae.continuous
        .equations
        .push(eq(sub(var("psi_e"), mul(var("Le"), var("ie")))));
    dae.continuous
        .equations
        .push(eq(sub(var("ie"), var("pin.i"))));
    dae.continuous
        .equations
        .push(flow_sum_eq(add(var("pin.i"), var("src.i"))));
    dae.continuous
        .equations
        .push(eq(sub(var("src.i"), var("I"))));
    dae.continuous
        .equations
        .push(eq(sub(var("vei"), der("psi_e"))));
    dae
}

#[test]
fn direct_demotion_follows_a_constrained_state_through_a_flow_sum() {
    let mut dae = excitation_node_dae();

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(demoted, 1, "the pinned flux is not an independent state");
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("psi_e"))
    );
    assert!(
        !dae.continuous
            .equations
            .iter()
            .any(|equation| expr_contains_der_of(&equation.rhs, &VarName::new("psi_e"))),
        "every der(psi_e) must be replaced by the differentiated constraint"
    );
}

/// A translational flange chain, reduced to its structural skeleton.
///
/// Every position row only relates a position to its neighbour's, so the
/// closure of `mass.s` cycles and never settles on a parameter. `mass.s` is a
/// free coordinate and must stay a state, even though nothing in that cycle is
/// itself a state.
#[test]
fn direct_demotion_keeps_a_position_whose_closure_only_cycles() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("mass.s"), test_variable("mass.s"));
    dae.variables
        .states
        .insert(VarName::new("mass.v"), test_variable("mass.v"));
    for name in ["mass.flange.s", "spring.flange.s", "spring.s_rel"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }

    dae.continuous
        .equations
        .push(eq(sub(var("mass.flange.s"), var("mass.s"))));
    dae.continuous
        .equations
        .push(eq(sub(var("mass.flange.s"), var("spring.flange.s"))));
    dae.continuous.equations.push(eq(sub(
        var("spring.s_rel"),
        sub(var("spring.flange.s"), var("mass.flange.s")),
    )));
    dae.continuous
        .equations
        .push(eq(sub(var("mass.v"), der("mass.s"))));
    dae.continuous
        .equations
        .push(eq(sub(der("mass.v"), var("spring.s_rel"))));

    let demoted = demote_direct_assigned_states(&mut dae).expect("direct demotion should succeed");

    assert_eq!(
        demoted, 0,
        "a coordinate whose value closure never settles is not a dummy state"
    );
    assert!(dae.variables.states.contains_key(&VarName::new("mass.s")));
}
