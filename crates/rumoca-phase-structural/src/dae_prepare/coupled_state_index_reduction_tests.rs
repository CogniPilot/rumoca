//! Regression coverage for index-2 loops whose only derivative row couples
//! several state derivatives at once.
//!
//! The shape comes from `Modelica.Magnetic.FundamentalWave`: a converter writes
//! one power-balance row `-v = N.re*der(Phi.re) + N.im*der(Phi.im)` while the
//! reluctance and the magnetic ground pin both flux components algebraically to
//! the same electrical current. Two states then share a single derivative row,
//! so one of them must become a dummy derivative before matching.

use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("coupled_state_index_reduction_test.mo"),
        1,
        2,
    )
}

fn continuous_variable(name: &str) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable
}

fn parameter_variable(name: &str, value: f64) -> Variable {
    let mut variable = continuous_variable(name);
    variable.start = Some(real(value));
    variable.is_tunable = true;
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: test_span(),
    }
}

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Sub, lhs, rhs)
}

fn add(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Add, lhs, rhs)
}

fn mul(lhs: Expression, rhs: Expression) -> Expression {
    binary(OpBinary::Mul, lhs, rhs)
}

fn neg(rhs: Expression) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(rhs),
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

fn eq(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(),
        origin: "top-level model equation".to_string(),
        scalar_count: 1,
    }
}

/// Build the minimal fundamental-wave converter loop.
///
/// ```text
/// vs     = 141.4*time
/// vs     = R*i + v
/// V_m.re = Nre*i
/// V_m.im = Nim*i
/// V_m.re = R_m*(-Phi.re)
/// V_m.im = R_m*(-Phi.im)
/// -v     = Nre*der(Phi.re) + Nim*der(Phi.im)
/// ```
fn coupled_converter_dae() -> Dae {
    let mut dae = Dae::new();
    for (name, value) in [("Nre", 5.0), ("Nim", 2.0), ("R_m", 25.0), ("R", 0.1)] {
        dae.variables
            .parameters
            .insert(VarName::new(name), parameter_variable(name, value));
    }
    for name in ["Phi.re", "Phi.im"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    for name in ["V_m.re", "V_m.im", "i", "v", "vs"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.continuous.equations = vec![
        eq(sub(var("vs"), mul(real(141.4), var("time")))),
        eq(sub(var("vs"), add(mul(var("R"), var("i")), var("v")))),
        eq(sub(var("V_m.re"), mul(var("Nre"), var("i")))),
        eq(sub(var("V_m.im"), mul(var("Nim"), var("i")))),
        eq(sub(var("V_m.re"), mul(var("R_m"), neg(var("Phi.re"))))),
        eq(sub(var("V_m.im"), mul(var("R_m"), neg(var("Phi.im"))))),
        eq(sub(
            neg(var("v")),
            add(
                mul(var("Nre"), der("Phi.re")),
                mul(var("Nim"), der("Phi.im")),
            ),
        )),
    ];
    dae
}

#[test]
fn coupled_converter_loop_reduces_to_one_independent_state() {
    let mut dae = coupled_converter_dae();
    reduce_constrained_dummy_derivatives(&mut dae).expect("constrained dummy reduction");
    let states: Vec<&str> = dae.variables.states.keys().map(VarName::as_str).collect();
    assert_eq!(
        states.len(),
        1,
        "one flux component must become a dummy derivative; states={states:?}"
    );
}

/// An index-2 loop whose only derivative row couples both state derivatives and
/// whose constraint is not in solved form for either state.
///
/// ```text
/// 0 = x*y - 4
/// 0 = u - sin(3*der(x) + 5*der(y))
/// 0 = u - time
/// ```
///
/// The derivative row buries both derivatives inside a call, exactly as the
/// fundamental-wave converter buries them inside `ComplexMath.real`, so nothing
/// can solve it for either derivative. The nonlinear constraint is not a
/// defining expression for either state either, so every substituting strategy
/// fails. Only a dummy derivative can reduce it.
fn unsolved_coupled_constraint_dae() -> Dae {
    let mut dae = Dae::new();
    for name in ["x", "y"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.variables
        .algebraics
        .insert(VarName::new("u"), continuous_variable("u"));
    dae.continuous.equations = vec![
        eq(sub(mul(var("x"), var("y")), real(4.0))),
        eq(sub(
            var("u"),
            Expression::BuiltinCall {
                function: BuiltinFunction::Sin,
                args: vec![add(mul(real(3.0), der("x")), mul(real(5.0), der("y")))],
                span: test_span(),
            },
        )),
        eq(sub(var("u"), var("time"))),
    ];
    dae
}

#[test]
fn coupled_derivative_row_reduces_through_a_dummy_derivative() {
    let dae = unsolved_coupled_constraint_dae();
    let candidates =
        singular_holonomic_state_candidates(&dae).expect("holonomic candidate enumeration");
    let selected = candidates
        .into_iter()
        .find(|candidate| {
            candidate
                .demoted_states
                .iter()
                .any(|(_, name)| name.as_str() == "x")
        })
        .expect("the coupled constraint must yield a dummy-derivative candidate for x");

    let reduced = selected.dae;
    assert!(
        reduced
            .variables
            .algebraics
            .contains_key(&VarName::new("x")),
        "the dependent state must move to the algebraic partition"
    );
    let dummy = VarName::new("__dummyder__.x");
    assert!(
        reduced.variables.algebraics.contains_key(&dummy),
        "der(x) must be replaced by a declared dummy-derivative unknown; algebraics={:?}",
        reduced
            .variables
            .algebraics
            .keys()
            .map(VarName::as_str)
            .collect::<Vec<_>>()
    );
    assert!(
        reduced
            .continuous
            .equations
            .iter()
            .all(|equation| !expr_contains_der_of(&equation.rhs, &VarName::new("x"))),
        "no der(x) may survive the demotion"
    );
    assert!(
        reduced.continuous.equations.iter().any(|equation| equation
            .origin
            .contains("index_reduction:d_dt_holonomic_constraint")
            && expr_contains_var(&equation.rhs, &dummy)),
        "the appended differentiated constraint is the dummy derivative's defining row"
    );
    // Every original row is retained, so the reduced system still lies on the
    // original solution manifold (MLS 3.7: index reduction must not change it).
    assert_eq!(
        reduced.continuous.equations.len(),
        dae.continuous.equations.len() + 1
    );
}

/// `Complex` ports reach the DAE as record-valued references whose difference is
/// selected component-wise. The scalar components are ordinary DAE variables, so
/// differentiation has to project the selection before it can use the
/// derivative map.
#[test]
fn record_field_selection_differentiates_component_wise() {
    let mut dae = Dae::new();
    for name in ["p.re", "p.im", "n.re", "n.im", "dp", "dn"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), continuous_variable(name));
    }
    let mut der_map = HashMap::new();
    der_map.insert("p.im".to_string(), var("dp"));
    der_map.insert("n.im".to_string(), var("dn"));

    let selection = Expression::FieldAccess {
        base: Box::new(sub(var("p"), var("n"))),
        field: "im".to_string(),
        span: test_span(),
    };
    let derivative =
        symbolic_time_derivative(&selection, &dae, &der_map).expect("(p - n).im is differentiable");
    assert!(expr_contains_var(&derivative, &VarName::new("dp")));
    assert!(expr_contains_var(&derivative, &VarName::new("dn")));
}

/// Projection may never invent a component name. A record the DAE did not
/// scalarize has no scalar variables to differentiate, and guessing one would
/// produce a reference no later phase can size.
#[test]
fn record_field_selection_without_scalar_components_is_not_differentiable() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("p"), continuous_variable("p"));
    dae.variables
        .algebraics
        .insert(VarName::new("n"), continuous_variable("n"));

    let selection = Expression::FieldAccess {
        base: Box::new(sub(var("p"), var("n"))),
        field: "im".to_string(),
        span: test_span(),
    };
    assert!(symbolic_time_derivative(&selection, &dae, &HashMap::new()).is_none());
}

/// An exact derivative alias `der(x) = der(q)` must stay in the extracted
/// derivative values. Dropping every value that mentions a derivative — the
/// blunt way to keep mass-matrix rows out of the map — also removes this one and
/// silently disables constrained-dummy reduction for alias chains.
#[test]
fn exact_derivative_alias_survives_the_relaxed_derivative_map() {
    let mut dae = Dae::new();
    for name in ["x", "q"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.variables
        .algebraics
        .insert(VarName::new("alias"), continuous_variable("alias"));
    dae.continuous.equations = vec![
        eq(sub(var("x"), var("alias"))),
        eq(sub(var("alias"), var("q"))),
        eq(sub(der("x"), der("q"))),
        eq(sub(der("q"), real(1.0))),
    ];

    let seed = vec![var("alias")];
    let der_map =
        build_relaxed_derivative_map_for_state_definition(&dae, &seed, &VarName::new("x"))
            .expect("relaxed derivative map");
    assert_eq!(
        der_map.get("q"),
        Some(&der("q")),
        "the alias target keeps its own derivative coordinate"
    );

    let mut reduced = dae;
    let demoted =
        reduce_constrained_dummy_derivatives(&mut reduced).expect("alias chain should reduce");
    assert_eq!(demoted, 1);
    assert!(
        reduced
            .variables
            .algebraics
            .contains_key(&VarName::new("x"))
    );
}
