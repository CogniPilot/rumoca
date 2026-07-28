//! Coverage for constraints that reach their states only through algebraics.
//!
//! The shape comes from `Modelica.Mechanics.MultiBody.Joints.RevolutePlanarLoopConstraint`
//! and every other index-3 cut joint: the constraint row is written against an
//! intermediate algebraic quantity (`0 = ex_a*r_rel_a`) and names neither a
//! state nor the constraint force it has to be matched against. The rows below
//! are the same shape with the multibody kinematics replaced by `y = q*q`, so
//! the second prolongation still needs `der(q)` after `q` itself has been
//! demoted.

use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("indirect_constraint_seed_test.mo"),
        1,
        2,
    )
}

fn continuous_variable(name: &str) -> Variable {
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

fn binary(op: OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
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

/// Index-3 constraint written against an algebraic quantity.
///
/// ```text
/// 0 = der(q) - v      // position ODE
/// 0 = der(v) - a      // velocity ODE
/// 0 = y - q*q         // algebraic definition the constraint is written against
/// 0 = y               // the constraint: names no state and no force
/// 0 = a - time        // drive
/// ```
///
/// Row 3 is one equation more than the matching can place: `y` is already
/// determined by row 2, and `q` is a state, so the row has no free column.
fn indirect_constraint_dae() -> Dae {
    let mut dae = Dae::new();
    for name in ["q", "v"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    for name in ["y", "a"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.continuous.equations = vec![
        eq(binary(OpBinary::Sub, der("q"), var("v"))),
        eq(binary(OpBinary::Sub, der("v"), var("a"))),
        eq(binary(
            OpBinary::Sub,
            var("y"),
            binary(OpBinary::Mul, var("q"), var("q")),
        )),
        eq(var("y")),
        eq(binary(OpBinary::Sub, var("a"), var("time"))),
    ];
    dae
}

fn seeds_of(dae: &Dae) -> Vec<(String, usize)> {
    let defining_expr_index = collect_residual_defining_expr_index(dae);
    let structural_bindings = crate::static_eval::structural_scalar_bindings(dae);
    super::indirect_constraint_seed::indirect_constraint_seeds(
        dae,
        &defining_expr_index,
        &structural_bindings,
        &HashMap::new(),
    )
    .expect("indirect constraint seeds")
    .into_iter()
    .map(|seed| (seed.state_name.as_str().to_string(), seed.equation_index))
    .collect()
}

#[test]
fn constraint_written_against_an_algebraic_seeds_the_state_behind_it() {
    let dae = indirect_constraint_dae();
    assert_eq!(
        seeds_of(&dae),
        vec![("q".to_string(), 3)],
        "`0 = y` must seed `q`, the state it reaches through `y = q*q`"
    );
}

#[test]
fn a_defining_row_is_not_a_constraint_row() {
    // `0 = y - q*q` determines `y`; differentiating it would destroy the value
    // every other row reads, so it must never be seeded as a constraint.
    let dae = indirect_constraint_dae();
    assert!(
        seeds_of(&dae).iter().all(|(_, index)| *index != 2),
        "the defining row of `y` must not be treated as a constraint"
    );
}

#[test]
fn no_seed_when_the_algebraic_has_no_other_definition() {
    // Without row 2 the constraint `0 = y` *is* y's defining equation.
    let mut dae = indirect_constraint_dae();
    dae.continuous.equations.remove(2);
    assert!(
        seeds_of(&dae).is_empty(),
        "a row that determines its own unknown is not a constraint"
    );
}

#[test]
fn prolongation_reaches_the_velocity_state() {
    // Level 1 demotes `q` against `0 = y`; level 2 must demote `v` against the
    // differentiated row `0 = q*v + v*q`. That second differentiation needs
    // `der(q)` after `q` has stopped being a state, which only works when the
    // derivative closure replays the value chosen for the demoted state.
    let dae = indirect_constraint_dae();
    let candidates = singular_holonomic_state_candidates(&dae).expect("holonomic candidates");
    let demoted: Vec<Vec<String>> = candidates
        .iter()
        .map(|candidate| {
            candidate
                .demoted_states
                .iter()
                .map(|(_, name)| name.as_str().to_string())
                .collect()
        })
        .collect();
    assert!(
        demoted.contains(&vec!["q".to_string(), "v".to_string()]),
        "expected a two-level dummy-derivative chain; got {demoted:?}"
    );
}
