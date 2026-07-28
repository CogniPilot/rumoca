//! Coverage for the coupled row-group naming form in the state-driven pass.
//!
//! The shape is the one `Modelica.Mechanics.MultiBody` produces: an earlier
//! constrained-dummy reduction leaves a row that is the *only* continuous
//! definition of a generated `__dummyder__` unknown, and the state-driven pass
//! then needs that row's derivative to reduce a second state. Consuming the row
//! keeps the row and column counts balanced while destroying the rank, so it
//! must be funded instead.

use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dummy_row_group_test.mo"),
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
        name: Reference::new(name),
        subscripts: vec![],
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

fn neg(rhs: Expression) -> Expression {
    Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Box::new(rhs),
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

fn eq(rhs: Expression, origin: &str) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(),
        origin: origin.to_string(),
        scalar_count: 1,
    }
}

/// A generated dummy row plus a state whose derivative only that row supplies.
///
/// ```text
/// __dummyder__.q - g      = 0   (the generated dummy's only defining row)
/// v              - __dummyder__.q = 0
/// g              - x      = 0
/// der(x)         - v      = 0
/// ```
///
/// `v` has no `der(v)` row. Differentiating the dummy row supplies one, so the
/// state-driven pass reaches for exactly the row it must not consume.
fn dummy_row_dae() -> Dae {
    let mut dae = Dae::new();
    for name in ["x", "v"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    for name in ["__dummyder__.q", "g"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), continuous_variable(name));
    }
    let dummy_origin = format!(
        "connection equation: dummy|{}",
        super::constrained_dummy_derivative::CONSTRAINED_DUMMY_ROW_ORIGIN
    );
    dae.continuous.equations = vec![
        eq(sub(var("__dummyder__.q"), var("g")), &dummy_origin),
        eq(
            sub(var("v"), var("__dummyder__.q")),
            "top-level model equation",
        ),
        eq(sub(var("g"), var("x")), "top-level model equation"),
        eq(sub(der("x"), var("v")), "top-level model equation"),
    ];
    dae
}

/// Origin of the row that defines the generated dummy derivative.
fn dummy_row_origin(dae: &Dae) -> Option<&str> {
    dae.continuous
        .equations
        .iter()
        .map(|equation| equation.origin.as_str())
        .find(|origin| {
            origin.contains(super::constrained_dummy_derivative::CONSTRAINED_DUMMY_ROW_ORIGIN)
        })
}

#[test]
fn dummy_defining_row_is_funded_rather_than_consumed() {
    let mut dae = dummy_row_dae();
    let rows_before = dae.continuous.equations.len();
    index_reduce_missing_state_derivatives(&mut dae).expect("index reduction");

    let origin = dummy_row_origin(&dae).expect("the dummy-defining row stays in `continuous`");
    assert!(
        !origin.contains("index_reduction:d_dt_for_"),
        "the dummy-defining row must not be consumed, but its origin reads `{origin}`"
    );
    assert!(
        !dae.variables.states.contains_key(&VarName::new("v")),
        "`v` funds the appended row by becoming an algebraic"
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("__dummyder__.v")),
        "`der(v)` is named rather than inlined"
    );
    assert_eq!(
        dae.continuous.equations.len(),
        rows_before + 1,
        "one scalar state funds exactly one appended scalar row"
    );
}

#[test]
fn funding_a_one_wide_state_from_a_wider_row_is_declined() {
    let mut dae = dummy_row_dae();
    // A three-wide residual cannot buy one scalar unknown: the exchange would
    // add three rows against one new column.
    dae.continuous.equations[0].scalar_count = 3;
    index_reduce_missing_state_derivatives(&mut dae).expect("index reduction");

    let origin = dummy_row_origin(&dae).expect("the dummy-defining row stays in `continuous`");
    assert!(
        !origin.contains("index_reduction:d_dt_for_"),
        "an unfundable group must still refuse to consume the row, but its origin reads `{origin}`"
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("__dummyder__.v")),
        "an unfundable group must not name a derivative it cannot pay for"
    );
}

/// SPEC_0008: the naming form must answer to its balance arithmetic at the
/// point it commits, not only to the static width precondition.
///
/// The precondition reads the *source* rows' declared widths and is satisfied
/// here. What it cannot see is that `v` already occupies the algebraic
/// partition, so demoting it moves no variable in and the commit adds a row and
/// the generated dummy against a partition that did not grow by the state's
/// width. That is the shape SPEC_0008 forbids recovering from silently: it
/// leaves a system that can still admit a perfect matching elsewhere. The pass
/// must name the state and stop.
#[test]
fn a_commit_that_unbalances_the_model_is_refused_by_name() {
    let mut dae = dummy_row_dae();
    let state = VarName::new("v");
    dae.variables
        .algebraics
        .insert(state.clone(), continuous_variable("v"));

    let error =
        super::dummy_row_group::reduce_state_by_row_group(&mut dae, &state, &[(0, var("g"))])
            .expect_err("a commit that does not grow the model by the state's width is refused");
    let StructuralError::ContractViolation { reason, .. } = &error else {
        panic!("expected a contract violation, got {error:?}");
    };
    assert!(
        reason.contains("state `v`") && reason.contains("scalar variable"),
        "the refusal must name the state and the arithmetic, but reads `{reason}`"
    );
    assert!(
        dae.variables.states.contains_key(&state),
        "a refused commit leaves the DAE untouched"
    );
}

/// An alias row cannot fund a consumption when its partner has no value of its
/// own.
///
/// ```text
/// a  - (-x2 - x3) = 0      the only row that determines `a`
/// x1 - a          = 0      an exact alias, and `x1`'s only row
/// ```
///
/// Once an earlier reduction has demoted `x1` to an algebraic, those two rows
/// carry the pair `{x1, a}` between them. Consuming the first — replacing it
/// with a derivative that mentions neither name — leaves one row for two
/// unknowns while the row and column counts stay exactly balanced, so only a
/// matching sees the loss.
#[test]
fn an_alias_to_a_demoted_state_does_not_fund_consumption() {
    let mut dae = Dae::new();
    for name in ["x2", "x3"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    for name in ["a", "x1", "__dummyder__.x1"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.continuous.equations = vec![
        eq(
            sub(var("a"), sub(neg(var("x2")), var("x3"))),
            "top-level model equation",
        ),
        eq(sub(var("x1"), var("a")), "top-level model equation"),
        eq(
            sub(var("__dummyder__.x1"), sub(neg(var("time")), var("time"))),
            "top-level model equation",
        ),
        eq(sub(der("x2"), var("time")), "top-level model equation"),
        eq(sub(der("x3"), var("time")), "top-level model equation"),
    ];

    index_reduce_missing_state_derivatives(&mut dae).expect("index reduction");

    assert_eq!(
        dae.continuous.equations[0].origin, "top-level model equation",
        "`a`'s only defining row must not be consumed, but it now reads `{}`",
        dae.continuous.equations[0].origin
    );
}
