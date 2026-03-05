use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_sim_core::simulation::dae_prepare::{
    demote_direct_assigned_states, expr_contains_der_of,
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
fn test_demote_direct_assigned_state_without_explicit_time_reference() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // x = z (direct assignment, no explicit time reference)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("z")),
        span: Span::DUMMY,
        origin: "assign_x".to_string(),
        scalar_count: 1,
    });
    // z = 1 (defines z algebraically, so der(z) = 0)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_z".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(demoted, 1, "expected x to be demoted as an algebraic dummy");
    assert!(!dae.states.contains_key(&VarName::new("x")));
    assert!(dae.algebraics.contains_key(&VarName::new("x")));
    assert!(
        !dae.f_x
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("x"))),
        "der(x) should be substituted after demotion"
    );
}

#[test]
fn test_demote_direct_assigned_state_from_explicit_lhs_equation() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // explicit equation: x = z (lhs form, not residual-sub form)
    dae.f_x.push(dae::Equation {
        lhs: Some(VarName::new("x")),
        rhs: var_ref("z"),
        span: Span::DUMMY,
        origin: "assign_x_explicit".to_string(),
        scalar_count: 1,
    });
    // z = 1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_z".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 1,
        "expected x to be demoted from explicit lhs assignment"
    );
    assert!(!dae.states.contains_key(&VarName::new("x")));
    assert!(dae.algebraics.contains_key(&VarName::new("x")));
    assert!(
        !dae.f_x
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("x"))),
        "der(x) should be substituted after explicit-lhs demotion"
    );
}

#[test]
fn test_demote_direct_assigned_state_from_affine_residual_equation() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // residual equation: 0 = x + z  =>  x = -z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "assign_x_affine".to_string(),
        scalar_count: 1,
    });
    // z = 1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_z".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 1,
        "expected x to be demoted from affine residual equation"
    );
    assert!(!dae.states.contains_key(&VarName::new("x")));
    assert!(dae.algebraics.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_state_does_not_use_non_state_lhs_rows() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // y = x - z (this is not a residual row for x)
    dae.f_x.push(dae::Equation {
        lhs: Some(VarName::new("y")),
        rhs: sub(var_ref("x"), var_ref("z")),
        span: Span::DUMMY,
        origin: "assign_y".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_z".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state x must not be demoted based on a non-state lhs assignment row"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_state_skips_with_additional_state_reference_rows() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("w"), Variable::new(VarName::new("w")));

    // der(x) = y
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("y"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // x = y (direct assignment candidate)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("y")),
        span: Span::DUMMY,
        origin: "assign_x".to_string(),
        scalar_count: 1,
    });
    // Additional row referencing x must block direct-assignment demotion.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("w"), var_ref("x")),
        span: Span::DUMMY,
        origin: "read_x_for_output".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_y".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state x must not be demoted when it participates in additional non-derivative rows"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_state_skips_unsliced_vector_alias_expr() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    let mut v = Variable::new(VarName::new("v"));
    v.dims = vec![3];
    dae.algebraics.insert(VarName::new("v"), v);

    // der(x) = 1
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
    // 0 = x + v, with v array-valued and unsliced here.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(var_ref("v")),
        },
        span: Span::DUMMY,
        origin: "assign_x_from_vector_alias".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state x must not be demoted from an unsliced vector alias expression"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_state_skips_flow_sum_equation_origin() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // Connection-style flow sum equation should not drive direct state demotion.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "flow sum equation: x + z = 0".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state x must not be demoted from a flow-sum connection equation"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_state_skips_connection_equation_origin() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // Connection equation aliases an algebraic connector potential to state x.
    // This must not trigger direct-assignment state demotion.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("a"), var_ref("x")),
        span: Span::DUMMY,
        origin: "connection equation: a = x".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_z".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state x must not be demoted from a connection equation alias"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_states_substitutes_derivatives() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("s"), Variable::new(VarName::new("s")));
    dae.states
        .insert(VarName::new("sd"), Variable::new(VarName::new("sd")));
    dae.algebraics
        .insert(VarName::new("sdd"), Variable::new(VarName::new("sdd")));

    // der(s) = sd
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("s")],
            },
            var_ref("sd"),
        ),
        span: Span::DUMMY,
        origin: "ode_s".to_string(),
        scalar_count: 1,
    });
    // der(sd) = sdd
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("sd")],
            },
            var_ref("sdd"),
        ),
        span: Span::DUMMY,
        origin: "ode_sd".to_string(),
        scalar_count: 1,
    });
    // s = if time < 1 then time*time else time
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("s"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(1.0)),
                    },
                    Expression::Binary {
                        op: OpBinary::Mul(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(var_ref("time")),
                    },
                )],
                else_branch: Box::new(var_ref("time")),
            },
        ),
        span: Span::DUMMY,
        origin: "assign_s".to_string(),
        scalar_count: 1,
    });
    // sdd = 0
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("sdd"), real(0.0)),
        span: Span::DUMMY,
        origin: "assign_sdd".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(demoted, 2, "expected s and sd to be demoted");
    assert!(!dae.states.contains_key(&VarName::new("s")));
    assert!(!dae.states.contains_key(&VarName::new("sd")));
    assert!(dae.algebraics.contains_key(&VarName::new("s")));
    assert!(dae.algebraics.contains_key(&VarName::new("sd")));

    assert!(
        !dae.f_x
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("s"))),
        "der(s) should be substituted after demotion"
    );
    assert!(
        !dae.f_x
            .iter()
            .any(|eq| expr_contains_der_of(&eq.rhs, &VarName::new("sd"))),
        "der(sd) should be substituted after demotion"
    );
}

#[test]
fn test_demote_direct_assigned_state_requires_symbolic_derivative() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("s"), Variable::new(VarName::new("s")));
    dae.states
        .insert(VarName::new("sd"), Variable::new(VarName::new("sd")));
    dae.algebraics
        .insert(VarName::new("sdd"), Variable::new(VarName::new("sdd")));

    // der(s) = sd
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("s")],
            },
            var_ref("sd"),
        ),
        span: Span::DUMMY,
        origin: "ode_s".to_string(),
        scalar_count: 1,
    });
    // der(sd) = sdd
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("sd")],
            },
            var_ref("sdd"),
        ),
        span: Span::DUMMY,
        origin: "ode_sd".to_string(),
        scalar_count: 1,
    });
    // s = time^2 (uses exponent op that symbolic differentiation may not support)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("s"),
            Expression::Binary {
                op: OpBinary::Exp(Default::default()),
                lhs: Box::new(var_ref("time")),
                rhs: Box::new(real(2.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "assign_s_exp".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("sdd"), real(0.0)),
        span: Span::DUMMY,
        origin: "assign_sdd".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state s must not be demoted when its defining expression cannot be symbolically differentiated"
    );
    assert!(dae.states.contains_key(&VarName::new("s")));
}

#[test]
fn test_demote_direct_assigned_state_allows_scaled_derivative_row_via_symbolic_path() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));
    let mut n_param = Variable::new(VarName::new("n"));
    n_param.start = Some(real(2.0));
    n_param.fixed = Some(true);
    dae.parameters.insert(VarName::new("n"), n_param);

    // n * der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::Binary {
                op: OpBinary::Mul(Default::default()),
                lhs: Box::new(var_ref("n")),
                rhs: Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![var_ref("x")],
                }),
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x_scaled".to_string(),
        scalar_count: 1,
    });
    // residual equation: 0 = x + z  =>  x = -z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(var_ref("z")),
        },
        span: Span::DUMMY,
        origin: "assign_x_affine".to_string(),
        scalar_count: 1,
    });
    // z = 1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_z".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 1,
        "state x should be demoted when symbolic differentiation can resolve its defining derivative"
    );
    assert!(!dae.states.contains_key(&VarName::new("x")));
    assert!(dae.algebraics.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_state_skips_when_reinit_targets() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // der(x) = z
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("z"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });
    // x = z (would normally be eligible for direct-assignment demotion)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("x"), var_ref("z")),
        span: Span::DUMMY,
        origin: "assign_x".to_string(),
        scalar_count: 1,
    });
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("z"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_z".to_string(),
        scalar_count: 1,
    });

    // Event-partition assignment marks x as an event-updated state target.
    dae.f_z.push(dae::Equation {
        lhs: Some(VarName::new("x")),
        rhs: Expression::If {
            branches: vec![(
                Expression::Binary {
                    op: OpBinary::Le(Default::default()),
                    lhs: Box::new(var_ref("x")),
                    rhs: Box::new(real(0.0)),
                },
                real(0.0),
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![var_ref("x")],
            }),
        },
        span: Span::DUMMY,
        origin: "reinit_x".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "state x should not be demoted when it is assigned in a when-clause"
    );
    assert!(dae.states.contains_key(&VarName::new("x")));
}

#[test]
fn test_demote_direct_assigned_state_skips_function_call_defining_expr() {
    let mut dae = Dae::new();
    dae.states.insert(
        VarName::new("coil.Phi"),
        Variable::new(VarName::new("coil.Phi")),
    );
    dae.algebraics.insert(
        VarName::new("coil.i"),
        Variable::new(VarName::new("coil.i")),
    );
    dae.algebraics.insert(
        VarName::new("source.v"),
        Variable::new(VarName::new("source.v")),
    );

    // der(coil.Phi) = source.v
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("coil.Phi")],
            },
            var_ref("source.v"),
        ),
        span: Span::DUMMY,
        origin: "ode_flux".to_string(),
        scalar_count: 1,
    });
    // coil.Phi = nonlinearFlux(coil.i)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("coil.Phi"),
            Expression::FunctionCall {
                name: VarName::new("nonlinearFlux"),
                args: vec![var_ref("coil.i")],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "assign_flux".to_string(),
        scalar_count: 1,
    });
    // coil.i = time
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("coil.i"), var_ref("time")),
        span: Span::DUMMY,
        origin: "assign_current".to_string(),
        scalar_count: 1,
    });
    // source.v = 1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("source.v"), real(1.0)),
        span: Span::DUMMY,
        origin: "assign_voltage".to_string(),
        scalar_count: 1,
    });

    let demoted = demote_direct_assigned_states(&mut dae);
    assert_eq!(
        demoted, 0,
        "function-call defining expressions must not trigger direct-assignment state demotion"
    );
    assert!(dae.states.contains_key(&VarName::new("coil.Phi")));
}
