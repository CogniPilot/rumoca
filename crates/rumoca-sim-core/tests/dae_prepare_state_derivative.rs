use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_sim_core::simulation::dae_prepare::expr_contains_der_of;
use rumoca_sim_core::simulation::dae_prepare::{
    demote_alias_states_without_der, demote_coupled_derivative_states,
    index_reduce_missing_state_derivatives, promote_der_algebraics_to_states,
};
use rumoca_sim_core::{
    EquationReorderError, TimeoutBudget, compute_mass_matrix, reorder_equations_for_solver,
};

type Dae = dae::Dae;
type BuiltinFunction = dae::BuiltinFunction;
type Expression = dae::Expression;
type OpBinary = dae::OpBinary;
type VarName = dae::VarName;
type Variable = dae::Variable;

fn var_ref(name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![],
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal(dae::Literal::Real(value))
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}
#[test]
fn test_demote_alias_states_propagates_across_chain() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.states
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

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
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("y")),
        span: Span::DUMMY,
        origin: "alias_xy".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), var_ref("z")),
        span: Span::DUMMY,
        origin: "alias_yz".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_alias_states_without_der(&mut dae);
    assert_eq!(demoted, 2);
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(!dae.states.contains_key(&VarName::new("y")));
    assert!(!dae.states.contains_key(&VarName::new("z")));
}

#[test]
fn test_promote_der_algebraic_to_state_allows_coupled_derivative_row() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("p"), Variable::new(VarName::new("p")));

    // der(x) has its own ODE row.
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

    // y only appears in a coupled-derivative equation with der(x).
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("p"),
            Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![var_ref("x")],
                }),
                rhs: Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![var_ref("y")],
                }),
            },
        ),
        span: Span::DUMMY,
        origin: "coupled_power".to_string(),
        scalar_count: 1,
    });

    promote_der_algebraics_to_states(&mut dae);
    assert!(
        dae.states.contains_key(&VarName::new("y")),
        "y should be promoted when der(y) appears in a coupled derivative row"
    );
    assert!(!dae.algebraics.contains_key(&VarName::new("y")));
}

#[test]
fn test_promote_der_algebraic_to_state_from_isolated_derivative_row() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("y")],
            },
            real(2.0),
        ),
        span: Span::DUMMY,
        origin: "ode_y".to_string(),
        scalar_count: 1,
    });

    promote_der_algebraics_to_states(&mut dae);
    assert!(dae.states.contains_key(&VarName::new("y")));
    assert!(!dae.algebraics.contains_key(&VarName::new("y")));
}

#[test]
fn test_demote_coupled_derivative_states_without_standalone_rows() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.states
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));
    dae.algebraics
        .insert(VarName::new("w"), Variable::new(VarName::new("w")));

    // Coupled derivative row: w = der(a) - der(b)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("w"),
            sub(
                Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![var_ref("a")],
                },
                Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![var_ref("b")],
                },
            ),
        ),
        span: Span::DUMMY,
        origin: "coupled_der".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_coupled_derivative_states(&mut dae);
    assert_eq!(demoted, 0, "coupled derivative states should remain states");
    assert!(dae.states.contains_key(&VarName::new("a")));
    assert!(dae.states.contains_key(&VarName::new("b")));
    assert!(!dae.algebraics.contains_key(&VarName::new("b")));
}

#[test]
fn test_compute_mass_matrix_keeps_coupled_derivative_offdiagonals() {
    fn der(name: &str) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var_ref(name)],
        }
    }
    fn mul(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Mul(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn add(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.states
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            add(mul(real(2.0), der("a")), mul(real(3.0), der("b"))),
            real(0.0),
        ),
        span: Span::DUMMY,
        origin: "ode_ab_0".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(add(mul(real(5.0), der("a")), der("b")), real(0.0)),
        span: Span::DUMMY,
        origin: "ode_ab_1".to_string(),
        scalar_count: 1,
    });

    let budget = TimeoutBudget::new(None);
    let mass = compute_mass_matrix(&dae, 2, &[], &budget).expect("mass matrix");
    assert_eq!(mass.len(), 2);
    assert_eq!(mass[0].len(), 2);
    assert_eq!(mass[1].len(), 2);
    assert!((mass[0][0] - 2.0).abs() < 1e-12);
    assert!((mass[0][1] - 3.0).abs() < 1e-12);
    assert!((mass[1][0] - 5.0).abs() < 1e-12);
    assert!((mass[1][1] - 1.0).abs() < 1e-12);
}

#[test]
fn test_index_reduction_does_nothing_without_state_constraint_candidate() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // No equation references x, so no valid differentiation candidate.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "z_eq_1".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(2.0)),
        span: Span::DUMMY,
        origin: "z_eq_2".to_string(),
        scalar_count: 1,
    });

    let changed = index_reduce_missing_state_derivatives(&mut dae);
    assert_eq!(changed, 0, "no candidate equation should mean no rewrite");

    let mut dae_after = dae.clone();
    assert!(
        matches!(
            reorder_equations_for_solver(&mut dae_after),
            Err(EquationReorderError::MissingStateEquation(name)) if name == "x"
        ),
        "missing der(x) should remain explicit when reduction is not possible"
    );
}

#[test]
fn test_index_reduction_tries_multiple_candidates_for_state() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // First candidate references x but is not symbolically differentiable by our rules.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::FunctionCall {
                name: VarName::new("f"),
                args: vec![var_ref("x")],
                is_constructor: false,
            },
            real(0.0),
        ),
        span: Span::DUMMY,
        origin: "nondiff_candidate".to_string(),
        scalar_count: 1,
    });
    // Second candidate is differentiable and should be selected.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("z")),
        span: Span::DUMMY,
        origin: "diff_candidate".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), var_ref("v")),
        span: Span::DUMMY,
        origin: "def_z".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("v")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_v".to_string(),
        scalar_count: 1,
    });

    let changed = index_reduce_missing_state_derivatives(&mut dae);
    assert_eq!(
        changed, 1,
        "expected to use second differentiable candidate"
    );
    assert!(
        dae.f_x.iter().any(|eq| eq
            .origin
            .contains("diff_candidate|index_reduction:d_dt_for_x")),
        "expected transformed equation to come from second candidate"
    );
    assert!(
        dae.f_x
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("x"))),
        "expected resulting system to contain der(x)"
    );
}

#[test]
fn test_demote_alias_states_without_derivative_row() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // x has a derivative row.
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
    // y is only algebraically tied to x.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), var_ref("x")),
        span: Span::DUMMY,
        origin: "alias_y_x".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_alias_states_without_der(&mut dae);
    assert_eq!(demoted, 1);
    assert!(dae.states.contains_key(&VarName::new("x")));
    assert!(!dae.states.contains_key(&VarName::new("y")));
    assert!(dae.algebraics.contains_key(&VarName::new("y")));

    let mut dae_after = dae.clone();
    assert!(
        reorder_equations_for_solver(&mut dae_after).is_ok(),
        "after demotion, reorder should not require der(y)"
    );
}

#[test]
fn test_demote_alias_state_without_derivative_row_when_aliases_algebraic() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // z has a standalone derivative row.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("z")],
            },
            real(1.0),
        ),
        span: Span::DUMMY,
        origin: "ode_z".to_string(),
        scalar_count: 1,
    });
    // x is only alias-constrained to non-state y.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("y")),
        span: Span::DUMMY,
        origin: "alias_x_y".to_string(),
        scalar_count: 1,
    });
    // y aliases z so the trajectory still has dynamic support.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), var_ref("z")),
        span: Span::DUMMY,
        origin: "alias_y_z".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_alias_states_without_der(&mut dae);
    assert_eq!(demoted, 1);
    assert!(!dae.states.contains_key(&VarName::new("x")));
    assert!(dae.algebraics.contains_key(&VarName::new("x")));
    assert!(dae.states.contains_key(&VarName::new("z")));

    let mut dae_after = dae.clone();
    assert!(
        reorder_equations_for_solver(&mut dae_after).is_ok(),
        "after demoting x, solver reorder should not require der(x)"
    );
}

#[test]
fn test_expr_contains_der_of_matches_field_access_state_reference() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![Expression::FieldAccess {
            base: Box::new(var_ref("x")),
            field: "im".to_string(),
        }],
    };
    assert!(
        expr_contains_der_of(&expr, &VarName::new("x.im")),
        "der(FieldAccess(x, im)) should match scalarized state x.im"
    );
}

#[test]
fn test_expr_contains_der_of_matches_indexed_field_access_state_reference() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![Expression::FieldAccess {
            base: Box::new(Expression::Index {
                base: Box::new(var_ref("x")),
                subscripts: vec![dae::Subscript::Index(2)],
            }),
            field: "im".to_string(),
        }],
    };
    assert!(
        expr_contains_der_of(&expr, &VarName::new("x[2].im")),
        "der(FieldAccess(Index(x,2), im)) should match scalarized state x[2].im"
    );
}
