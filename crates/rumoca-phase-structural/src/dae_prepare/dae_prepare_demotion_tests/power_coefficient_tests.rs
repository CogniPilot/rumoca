//! Constant folding of `^` in structural coefficients (MLS 3.7.1).
//!
//! The linear-constraint dummy-derivative analysis reads row coefficients as
//! numbers. A parameter defined with `^` (`R_m = effectiveTurns^2/L`, the MSL
//! magnetic-circuit reluctance) has to fold like any other constant arithmetic;
//! otherwise every row scaled by it looks nonlinear, the row is dropped, and the
//! index-2 constraint that ties the two flux components together is never found.

use super::*;

/// Two states tied by a linear constraint whose coefficient goes through a
/// `^`-defined parameter must still be reduced to one independent state.
#[test]
fn test_constrained_dummy_reduction_folds_power_coefficient_parameter() {
    let mut dae = power_coefficient_dae(pow(var("turns"), real(2.0)));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");

    assert_eq!(
        demoted, 1,
        "the flux pair is tied by one linear constraint, so exactly one state is a dummy"
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("phi_im")),
        "the dependent flux component must become algebraic"
    );
    assert!(
        dae.variables.states.contains_key(&VarName::new("phi_re")),
        "the independent flux component must stay a state"
    );
}

/// Same system with the coefficient written without `^`. This pins the
/// comparison: the reduction above is the behaviour of an already-numeric
/// coefficient, not a special case of the power operator.
#[test]
fn test_constrained_dummy_reduction_matches_literal_coefficient() {
    let mut dae = power_coefficient_dae(real(25.0));

    let demoted = reduce_constrained_dummy_derivatives(&mut dae)
        .expect("constrained dummy reduction should succeed");

    assert_eq!(demoted, 1);
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("phi_im"))
    );
}

fn pow(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Exp,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

/// A reluctance-shaped index-2 system: two flux states, each tied to its own
/// magnetic potential component through `r_m`, with the two potentials fixed to
/// one another by the converter row. `r_m_start` supplies the reluctance value.
fn power_coefficient_dae(r_m_start: Expression) -> Dae {
    let mut dae = Dae::new();
    for name in ["phi_re", "phi_im"] {
        dae.variables
            .states
            .insert(VarName::new(name), test_variable(name));
    }
    for name in ["vm_re", "vm_im", "v"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name));
    }
    let mut turns = test_variable("turns");
    turns.start = Some(real(5.0));
    dae.variables
        .parameters
        .insert(VarName::new("turns"), turns);
    let mut r_m = test_variable("r_m");
    r_m.start = Some(r_m_start);
    dae.variables.parameters.insert(VarName::new("r_m"), r_m);

    // vm_re = r_m*phi_re, vm_im = r_m*phi_im: the reluctance rows.
    dae.continuous
        .equations
        .push(eq(sub(var("vm_re"), mul(var("r_m"), var("phi_re")))));
    dae.continuous
        .equations
        .push(eq(sub(var("vm_im"), mul(var("r_m"), var("phi_im")))));
    // vm_im = 2*vm_re: both potential components are driven by one current, so
    // the two flux components carry only one degree of freedom.
    dae.continuous
        .equations
        .push(eq(sub(var("vm_im"), mul(real(2.0), var("vm_re")))));
    // The induced-voltage row, the only row carrying either flux derivative.
    dae.continuous.equations.push(eq(sub(
        var("v"),
        add(der("phi_re"), mul(real(2.0), der("phi_im"))),
    )));
    dae.continuous
        .equations
        .push(eq(sub(var("v"), mul(real(3.0), var("vm_re")))));
    dae
}
