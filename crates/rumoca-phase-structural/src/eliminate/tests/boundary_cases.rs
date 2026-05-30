use super::*;

#[test]
fn test_eliminate_bare_varref() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: var_ref("z"),
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert!(dae.continuous.equations.is_empty());
}

// ── Boundary resolution specific tests ──────────────────────────

#[test]
fn test_boundary_zero_unknown_removed() {
    // dae::Equation with no unknowns (parameter-only) should be removed.
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    // eq1: 0 = z - 1.0  (1 unknown, solvable)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });

    // eq2: 0 = 3.0 - 3.0  (0 unknowns, redundant)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(lit(3.0)),
            rhs: Box::new(lit(3.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "eq2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    // Both removed: eq2 has 0 unknowns, eq1 has 1 unknown (z=1.0).
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.continuous.equations.is_empty());
}

#[test]
fn test_boundary_zero_unknown_alias_equation_becomes_substitution() {
    let mut dae = Dae::new();

    let mut state = dae::Variable::new(VarName::new("x"));
    state.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), state);
    dae.variables
        .algebraics
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.variables.inputs.insert(
        VarName::new("alias_local"),
        dae::Variable::new(VarName::new("alias_local")),
    );
    dae.variables.discrete_valued.insert(
        VarName::new("local"),
        dae::Variable::new(VarName::new("local")),
    );

    // 0 = der(x) - y
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref("y")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // 0 = y - if alias_local then 1 else 0
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(var_ref("alias_local"), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "y_alias".to_string(),
        scalar_count: 1,
    });

    // 0 = alias_local - local (no continuous unknowns)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("alias_local")),
            rhs: Box::new(var_ref("local")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "discrete_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    let alias_sub = result
        .substitutions
        .iter()
        .find(|sub| sub.var_name.as_str() == "alias_local")
        .expect("discrete alias equation should be converted to substitution");

    assert!(
        matches!(
            alias_sub.expr,
            Expression::VarRef { ref name, ref subscripts, .. }
                if name.as_str() == "local" && subscripts.is_empty()
        ),
        "alias_local should resolve to local, got {:?}",
        alias_sub.expr
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &VarName::new("alias_local"))),
        "remaining equations should no longer reference alias_local"
    );
}

#[test]
fn test_boundary_cascade_resolution() {
    // a=1 (1 unknown), b=a (2 unknowns initially, 1 after a resolved).
    // Phase A should cascade: resolve a first, then b becomes 1-unknown.
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), dae::Variable::new(VarName::new("b")));

    // eq1: 0 = a - 1.0  (1 unknown)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });

    // eq2: 0 = b - a  (2 unknowns initially)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("b")),
            rhs: Box::new(var_ref("a")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "eq2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.continuous.equations.is_empty());
    assert!(dae.variables.algebraics.is_empty());
}

#[test]
fn test_boundary_skips_ode_equations() {
    // ODE equation should never be eliminated by boundary resolution.
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(Expression::Literal {
        value: Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.states.insert(VarName::new("x"), var_x);

    // ODE: 0 = der(x) - 1.0
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
}

#[test]
fn test_boundary_eliminates_derivative_dependent_output_alias() {
    // Keep true ODE equation and eliminate derivative-dependent output alias.
    let mut dae = Dae::new();

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(Expression::Literal {
        value: Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.states.insert(VarName::new("x"), var_x);
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    // ODE: 0 = der(x) - 1.0
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // Alias output: 0 = y - der(x)
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "y_alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "y");
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(!dae.variables.outputs.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_eliminates_control_flow_solution_equation() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Literal {
                        value: Literal::Boolean(true),
                        span: rumoca_core::Span::DUMMY,
                    },
                    lit(1.0),
                )],
                else_branch: Box::new(lit(2.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "if_expr".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.continuous.equations.len(), 0);
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_eliminates_single_unknown_connection_after_substitution() {
    let mut dae = Dae::new();
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.variables
        .algebraics
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));

    // Source-like equation: y = if time < 0.2 then 1 else 2.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt,
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(lit(0.2)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    lit(1.0),
                )],
                else_branch: Box::new(lit(2.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "source".to_string(),
        scalar_count: 1,
    });

    // Connection equation reduced to one unknown after y substitution.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("u")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: y = u".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    // y is a non-trivial output (if-expression) — preserved in the DAE.
    // u cannot be eliminated because y also remains live in the connection
    // equation, keeping both unknowns alive.
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 2);
    assert!(
        dae.variables.outputs.contains_key(&VarName::new("y")),
        "output y should remain (non-trivial expression)"
    );
    assert!(
        dae.variables.algebraics.contains_key(&VarName::new("u")),
        "u should remain (y not eliminated, connection eq still has two unknowns)"
    );
}

#[test]
fn test_boundary_keeps_connection_eq_touching_runtime_discrete_target() {
    let mut dae = Dae::new();
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.variables
        .inputs
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));

    // Runtime-discrete partition assignment target (f_m/f_z lhs) marks `y`
    // as a runtime-discrete target that must not lose alias edges.
    dae.discrete.valued_updates.push(dae::Equation {
        lhs: Some(VarName::new("y")),
        rhs: Expression::Literal {
            value: Literal::Boolean(false),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "runtime discrete assignment".to_string(),
        scalar_count: 1,
    });

    // Connection equation that would normally be single-live-unknown.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("u")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: y = u".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.outputs.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_keeps_zero_unknown_runtime_discrete_assignment_used_by_f_m() {
    let mut dae = Dae::new();
    dae.variables.discrete_valued.insert(
        VarName::new("Enable.y"),
        dae::Variable::new(VarName::new("Enable.y")),
    );
    dae.variables.discrete_valued.insert(
        VarName::new("Counter.enable"),
        dae::Variable::new(VarName::new("Counter.enable")),
    );

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("Enable.y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Ge,
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(lit(1.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    lit(4.0),
                )],
                else_branch: Box::new(lit(3.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "digital source".to_string(),
        scalar_count: 1,
    });
    dae.discrete.valued_updates.push(dae::Equation {
        lhs: Some(VarName::new("Counter.enable")),
        rhs: var_ref("Enable.y"),
        span: Span::DUMMY,
        origin: "explicit connection equation: Counter.enable = Enable.y".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(
        dae.continuous.equations.len(),
        1,
        "runtime discrete source row must remain live"
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin == "digital source"),
        "time-driven discrete assignment should not be dropped by boundary elimination"
    );
}

#[test]
fn test_eliminate_trivial_accepts_runtime_known_assignment_tail_after_output_alias() {
    let mut dae = Dae::new();
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.variables
        .discrete_reals
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "runtime source".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("u")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "output alias to runtime tail".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert!(
        result.blt_error.is_none(),
        "runtime-known assignment rows should not make BLT lowering fail"
    );
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.outputs.is_empty());
}

#[test]
fn test_eliminate_trivial_keeps_sampled_value_source_unknown() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.variables
        .algebraics
        .insert(VarName::new("u"), dae::Variable::new(VarName::new("u")));
    dae.variables
        .discrete_reals
        .insert(VarName::new("clk"), dae::Variable::new(VarName::new("clk")));
    dae.variables
        .discrete_reals
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("u")),
            rhs: Box::new(var_ref("x")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "u = x".to_string(),
        scalar_count: 1,
    });
    dae.discrete.real_updates.push(dae::Equation {
        lhs: Some(VarName::new("clk")),
        rhs: Expression::FunctionCall {
            name: rumoca_core::Reference::new("Clock"),
            args: vec![lit(0.1)],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "clk".to_string(),
        scalar_count: 1,
    });
    dae.discrete.real_updates.push(dae::Equation {
        lhs: Some(VarName::new("y")),
        rhs: Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var_ref("u"), var_ref("clk")],
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "y = sample(u, clk)".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae);
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(
        dae.variables.algebraics.contains_key(&VarName::new("u")),
        "sampled continuous helper source must stay live for f_z/f_m value reads"
    );
}
