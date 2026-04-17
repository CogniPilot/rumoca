use super::*;

#[test]
fn test_demote_orphan_states_without_equation_refs() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // x is referenced by a derivative row.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // z is algebraic and references x.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), var_ref("x")),
        span: Span::DUMMY,
        origin: "alg_z".to_string(),
        scalar_count: 1,
    });
    // y is orphaned: no equation reference.

    let demoted = demote_orphan_states_without_equation_refs(&mut dae);
    assert_eq!(demoted, 1, "expected orphan state y to be demoted");
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(!dae.states.contains_key(&VarName::new("y")));
    assert!(dae.algebraics.contains_key(&VarName::new("y")));
}

#[test]
fn test_demote_states_without_derivative_refs() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // x has an ODE row.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // y is referenced as a value, but never appears as der(y).
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), var_ref("x")),
        span: Span::DUMMY,
        origin: "alg_y".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_states_without_derivative_refs(&mut dae);
    assert_eq!(demoted, 1, "expected state y to be demoted");
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(!dae.states.contains_key(&VarName::new("y")));
    assert!(dae.algebraics.contains_key(&VarName::new("y")));
}

#[test]
fn test_demote_states_without_assignable_derivative_rows_demotes_unmatched() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x.re"), Variable::new(VarName::new("x.re")));
    dae.states
        .insert(VarName::new("x.im"), Variable::new(VarName::new("x.im")));

    // One equation with derivatives of both states -> only one unique row for two states.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x.re")],
            },
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x.im")],
            },
        ),
        span: Span::DUMMY,
        origin: "shared_derivative_row".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_states_without_assignable_derivative_rows(&mut dae);
    assert_eq!(
        demoted, 1,
        "expected one unmatched state to be demoted when only one derivative row is available"
    );
    assert_eq!(dae.states.len(), 1);
    assert_eq!(dae.algebraics.len(), 1);
}

#[test]
fn test_demote_states_without_assignable_derivative_rows_keeps_matchable_states() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x.re"), Variable::new(VarName::new("x.re")));
    dae.states
        .insert(VarName::new("x.im"), Variable::new(VarName::new("x.im")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x.re")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x_re".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x.im")],
            },
            real(2.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x_im".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_states_without_assignable_derivative_rows(&mut dae);
    assert_eq!(demoted, 0);
    assert_eq!(dae.states.len(), 2);
    assert!(!dae.algebraics.contains_key(&VarName::new("x.re")));
    assert!(!dae.algebraics.contains_key(&VarName::new("x.im")));
}

#[test]
fn test_reorder_equations_for_solver_uses_maximum_matching_for_shared_derivative_rows() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // Shared derivative row appears first; greedy row selection would pick this
    // for x and then fail to find a row for y.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("y")],
            },
        ),
        span: Span::DUMMY,
        origin: "ode_shared_xy".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_x_only".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), var_ref("x")),
        span: Span::DUMMY,
        origin: "alg_z".to_string(),
        scalar_count: 1,
    });

    problem::reorder_equations_for_solver(&mut dae)
        .expect("state-row matching should pick a valid assignment for x and y");

    let ode_rows = &dae.f_x[..2];
    assert!(
        ode_rows
            .iter()
            .any(|eq| problem::expr_contains_der_of(&eq.rhs, &VarName::new("x"))),
        "expected reordered ODE rows to include der(x)"
    );
    assert!(
        ode_rows
            .iter()
            .any(|eq| problem::expr_contains_der_of(&eq.rhs, &VarName::new("y"))),
        "expected reordered ODE rows to include der(y)"
    );
}

#[test]
fn test_promote_then_redemote_keeps_unassignable_derivative_state_demoted() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x.re"), Variable::new(VarName::new("x.re")));
    dae.states
        .insert(VarName::new("x.im"), Variable::new(VarName::new("x.im")));

    // One row references both derivatives; only one of the two states can own
    // this row for ODE ordering.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x.re")],
            },
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x.im")],
            },
        ),
        span: Span::DUMMY,
        origin: "shared_derivative_row".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_states_without_assignable_derivative_rows(&mut dae);
    assert_eq!(demoted, 1);
    assert_eq!(dae.states.len(), 1);
    assert_eq!(dae.algebraics.len(), 1);

    // Re-promotion may bring the coupled state back.
    let before = dae.states.len();
    promote_der_algebraics_to_states(&mut dae);
    assert!(dae.states.len() >= before);

    // The second pass must demote it again so reorder remains well-defined.
    let redemoted = demote_states_without_assignable_derivative_rows(&mut dae);
    assert_eq!(redemoted, 1);
    assert_eq!(dae.states.len(), 1);

    let mut ordered = dae.clone();
    assert!(
        problem::reorder_equations_for_solver(&mut ordered).is_ok(),
        "after re-demotion, reorder should not report MissingStateEquation"
    );
}
