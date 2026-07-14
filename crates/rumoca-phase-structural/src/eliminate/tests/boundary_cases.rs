use super::*;

#[test]
fn test_eliminate_bare_varref() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: var_ref("z"),
        span: Span::DUMMY,
        origin: "eq1".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
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
        .insert(VarName::new("z"), test_dae_variable("z"));

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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    // Both removed: eq2 has 0 unknowns, eq1 has 1 unknown (z=1.0).
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.continuous.equations.is_empty());
}

#[test]
fn test_boundary_zero_unknown_alias_equation_becomes_substitution() {
    let mut dae = Dae::new();

    let mut state = test_dae_variable("x");
    state.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), state);
    dae.variables
        .algebraics
        .insert(VarName::new("y"), test_dae_variable("y"));
    dae.variables.inputs.insert(
        VarName::new("alias_local"),
        test_dae_variable("alias_local"),
    );
    dae.variables
        .discrete_valued
        .insert(VarName::new("local"), test_dae_variable("local"));

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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
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
        .insert(VarName::new("a"), test_dae_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), test_dae_variable("b"));

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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.continuous.equations.is_empty());
    assert!(dae.variables.algebraics.is_empty());
}

#[test]
fn test_boundary_eliminates_indexed_scalar_connection_alias() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("pin[2].v"), test_dae_variable("pin[2].v"));
    dae.variables
        .algebraics
        .insert(VarName::new("node.v"), test_dae_variable("node.v"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("pin[2].v")),
            rhs: Box::new(var_ref("node.v")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: pin[2].v = node.v".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("node.v")),
            rhs: Box::new(lit(0.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ground equation".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(result.blt_error.is_none());
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.continuous.equations.is_empty());
    assert!(dae.variables.algebraics.is_empty());
}

#[test]
fn test_boundary_eliminates_scalar_output_connection_alias() {
    let mut dae = Dae::new();
    dae.variables
        .outputs
        .insert(VarName::new("sensor.y"), test_dae_variable("sensor.y"));
    dae.variables
        .parameters
        .insert(VarName::new("source.y"), test_dae_variable("source.y"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("sensor.y")),
            rhs: Box::new(var_ref("source.y")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: sensor.y = source.y".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(result.blt_error.is_none());
    assert_eq!(result.n_eliminated, 1);
    assert!(dae.continuous.equations.is_empty());
    assert!(
        !dae.variables
            .outputs
            .contains_key(&VarName::new("sensor.y"))
    );
    assert_eq!(result.substitutions[0].var_name.as_str(), "sensor.y");
}

#[test]
fn test_boundary_eliminates_negated_additive_single_unknown() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("n_i"), test_dae_variable("n_i"));
    dae.variables
        .parameters
        .insert(VarName::new("source_i"), test_dae_variable("source_i"));
    let span = test_span();

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Unary {
            op: minus_op(),
            rhs: Box::new(Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(var_ref("source_i")),
                rhs: Box::new(var_ref("n_i")),
                span,
            }),
            span,
        },
        span,
        origin: "current source balance".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 1);
    assert!(dae.continuous.equations.is_empty());
    let substitution = result
        .substitutions
        .iter()
        .find(|sub| sub.var_name.as_str() == "n_i")
        .expect("n_i should be solved from the additive residual");
    assert!(
        matches!(
            &substitution.expr,
            Expression::Unary { op: OpUnary::Minus, rhs, span: expr_span }
                if *expr_span == span
                    && matches!(rhs.as_ref(), Expression::VarRef { name, .. } if name.as_str() == "source_i")
        ),
        "expected n_i = -source_i, got {:?}",
        substitution.expr
    );
}

#[test]
fn test_eliminate_trivial_demotes_state_after_boundary_substitution() {
    let mut dae = Dae::new();
    let span = test_span();
    dae.variables
        .states
        .insert(VarName::new("psi"), test_dae_variable("psi"));
    for name in ["i", "v"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }
    for name in ["source_i", "l"] {
        dae.variables
            .parameters
            .insert(VarName::new(name), test_dae_variable(name));
    }

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("i")),
            rhs: Box::new(var_ref("source_i")),
            span,
        },
        span,
        origin: "source current".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("psi")),
            rhs: Box::new(Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(var_ref("l")),
                rhs: Box::new(var_ref("i")),
                span,
            }),
            span,
        },
        span,
        origin: "flux relation".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("v")),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("psi")],
                span,
            }),
            span,
        },
        span,
        origin: "voltage relation".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 3);
    assert!(dae.continuous.equations.is_empty());
    assert!(!dae.variables.states.contains_key(&VarName::new("psi")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("psi")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("i")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("v")));
}

#[test]
fn test_boundary_skips_ode_equations() {
    // ODE equation should never be eliminated by boundary resolution.
    let mut dae = Dae::new();

    let mut var_x = test_dae_variable("x");
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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
}

#[test]
fn test_boundary_eliminates_derivative_dependent_output_alias() {
    // Keep true ODE equation and eliminate derivative-dependent output alias.
    let mut dae = Dae::new();

    let mut var_x = test_dae_variable("x");
    var_x.start = Some(Expression::Literal {
        value: Literal::Real(0.0),
        span: rumoca_core::Span::DUMMY,
    });
    dae.variables.states.insert(VarName::new("x"), var_x);
    dae.variables
        .outputs
        .insert(VarName::new("y"), test_dae_variable("y"));

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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
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
        .insert(VarName::new("y"), test_dae_variable("y"));

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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.continuous.equations.len(), 0);
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_eliminates_single_unknown_connection_after_substitution() {
    let mut dae = Dae::new();
    dae.variables
        .outputs
        .insert(VarName::new("y"), test_dae_variable("y"));
    dae.variables
        .algebraics
        .insert(VarName::new("u"), test_dae_variable("u"));

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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    // y is a non-trivial output (if-expression), so preserve y and its source
    // equation. The connection alias can still eliminate the scalar sink u.
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(
        dae.variables.outputs.contains_key(&VarName::new("y")),
        "output y should remain (non-trivial expression)"
    );
    assert!(
        !dae.variables.algebraics.contains_key(&VarName::new("u")),
        "connection sink u should be eliminated as an alias of y"
    );
}

#[test]
fn test_boundary_connection_alias_prefers_rhs_sink_element() {
    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        VarName::new("analysatorAC.voltageLine2Line[6].product1.u[2]"),
        test_dae_variable("analysatorAC.voltageLine2Line[6].product1.u[2]"),
    );
    dae.variables.algebraics.insert(
        VarName::new("analysatorAC.voltageLine2Line[6].product2.u[1]"),
        test_dae_variable("analysatorAC.voltageLine2Line[6].product2.u[1]"),
    );

    let connection_rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref("analysatorAC.voltageLine2Line[6].product1.u[2]")),
        rhs: Box::new(var_ref("analysatorAC.voltageLine2Line[6].product2.u[1]")),
        span: rumoca_core::Span::DUMMY,
    };
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: connection_rhs.clone(),
        span: Span::DUMMY,
        origin: "connection equation: analysatorAC.voltageLine2Line[6].product1.u[2] = analysatorAC.voltageLine2Line[6].product2.u[1]".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var_ref("analysatorAC.voltageLine2Line[6].product1.u[2]")),
            rhs: Box::new(var_ref("analysatorAC.voltageLine2Line[6].product2.u[1]")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "product input use".to_string(),
        scalar_count: 1,
    });

    let live = vec![
        VarName::new("analysatorAC.voltageLine2Line[6].product1.u[2]"),
        VarName::new("analysatorAC.voltageLine2Line[6].product2.u[1]"),
    ];
    let direct_definitions = DirectDefinitionIndex::build(&dae);
    let protected = IndexSet::new();
    let choice_ctx = EliminationChoiceContext {
        dae: &dae,
        eq_idx: 0,
        has_state_derivative: false,
        runtime_protected_unknowns: &protected,
        direct_definitions: &direct_definitions,
        allow_multi_live_trivial_alias: true,
    };
    let (var_name, solution) =
        choose_solvable_unknown_for_elimination(&choice_ctx, &connection_rhs, &live)
            .expect("candidate selection should not fail")
            .expect("connection alias should be solvable");

    assert_eq!(
        var_name.as_str(),
        "analysatorAC.voltageLine2Line[6].product2.u[1]"
    );
    assert!(
        matches!(
            solution,
            Expression::VarRef { ref name, ref subscripts, .. }
                if name.as_str() == "analysatorAC.voltageLine2Line[6].product1.u[2]"
                    && subscripts.is_empty()
        ),
        "sink input should resolve to source input, got {:?}",
        solution
    );
}

#[test]
fn test_boundary_connection_policy_accepts_scalar_element_of_aggregate_var() {
    let mut dae = Dae::new();
    let mut product1_u = test_dae_variable("analysatorAC.voltageLine2Line[6].product1.u");
    product1_u.dims = vec![2];
    dae.variables.algebraics.insert(
        VarName::new("analysatorAC.voltageLine2Line[6].product1.u"),
        product1_u,
    );
    let mut product2_u = test_dae_variable("analysatorAC.voltageLine2Line[6].product2.u");
    product2_u.dims = vec![2];
    dae.variables.algebraics.insert(
        VarName::new("analysatorAC.voltageLine2Line[6].product2.u"),
        product2_u,
    );

    let connection_rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref_idx(
            "analysatorAC.voltageLine2Line[6].product1.u",
            2,
        )),
        rhs: Box::new(var_ref_idx(
            "analysatorAC.voltageLine2Line[6].product2.u",
            1,
        )),
        span: rumoca_core::Span::DUMMY,
    };
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: connection_rhs.clone(),
        span: Span::DUMMY,
        origin: "connection equation: analysatorAC.voltageLine2Line[6].product1.u[2] = analysatorAC.voltageLine2Line[6].product2.u[1]".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var_ref_idx(
                "analysatorAC.voltageLine2Line[6].product1.u",
                2,
            )),
            rhs: Box::new(var_ref_idx(
                "analysatorAC.voltageLine2Line[6].product2.u",
                1,
            )),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "product input use".to_string(),
        scalar_count: 1,
    });

    let live = vec![
        VarName::new("analysatorAC.voltageLine2Line[6].product1.u[2]"),
        VarName::new("analysatorAC.voltageLine2Line[6].product2.u[1]"),
    ];

    assert!(
        !should_skip_connection_equation(&dae, &connection_rhs, true, &live, &HashSet::new(),),
        "scalar element aliases of aggregate variables should be eligible for connection elimination"
    );
}

#[test]
fn test_boundary_connection_policy_preserves_aggregate_only_scalar_elements() {
    let mut dae = Dae::new();
    for name in ["block.product1.u", "block.product2.u"] {
        let mut variable = test_dae_variable(name);
        variable.dims = vec![2];
        dae.variables
            .algebraics
            .insert(VarName::new(name), variable);
    }

    let connection_rhs = Expression::Binary {
        op: sub_op(),
        lhs: Box::new(var_ref_idx("block.product1.u", 2)),
        rhs: Box::new(var_ref_idx("block.product2.u", 1)),
        span: test_span(),
    };
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: connection_rhs.clone(),
        span: test_span(),
        origin: "connection equation: block.product1.u[2] = block.product2.u[1]".to_string(),
        scalar_count: 1,
    });
    for (output, aggregate) in [
        ("product1.y", "block.product1.u"),
        ("product2.y", "block.product2.u"),
    ] {
        dae.variables
            .algebraics
            .insert(VarName::new(output), test_dae_variable(output));
        dae.continuous.equations.push(residual(
            var_ref(output),
            builtin(BuiltinFunction::Product, vec![var_ref(aggregate)]),
            1,
            "aggregate-only product input use",
        ));
    }

    let live = vec![
        VarName::new("block.product1.u[2]"),
        VarName::new("block.product2.u[1]"),
    ];

    assert!(
        should_skip_connection_equation(&dae, &connection_rhs, true, &live, &HashSet::new()),
        "a scalar aggregate leaf whose only non-connection use is the materializable aggregate must keep its connection equation because the base vector storage cannot remove one leaf"
    );
}

#[test]
fn test_boundary_eliminates_scalarized_element_with_boundary_known_derivative_use() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_dae_variable("x"));
    let mut v = test_dae_variable("v");
    v.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("v"), v);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref_idx("v", 2)),
            rhs: Box::new(lit(20.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "binding equation for v[2]".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref_idx("v", 2)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "v[2]"),
        "boundary-known scalarized element should be substituted into derivative use"
    );
    assert_eq!(dae.continuous.equations.len(), 1);
    let Expression::Binary { rhs, .. } = &dae.continuous.equations[0].rhs else {
        panic!("remaining ODE should stay as a binary residual");
    };
    assert!(
        matches!(
            rhs.as_ref(),
            Expression::Literal {
                value: Literal::Real(value),
                ..
            } if *value == 20.0
        ),
        "derivative row should receive the boundary-known scalar value, got {:?}",
        dae.continuous.equations[0].rhs
    );
}

#[test]
fn test_boundary_rejects_derivative_alias_backed_only_by_connection_definition() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_dae_variable("x"));
    for (name, causality) in [
        ("mean.u", dae::VariableCausality::Input),
        ("sensor.v", dae::VariableCausality::Output),
    ] {
        let mut variable = test_dae_variable(name);
        variable.causality = causality;
        dae.variables.outputs.insert(VarName::new(name), variable);
    }
    dae.continuous
        .equations
        .push(residual(der(var_ref("x")), var_ref("mean.u"), 1, "ode"));
    dae.continuous.equations.push(residual(
        var_ref("sensor.v"),
        var_ref("mean.u"),
        1,
        "connection equation: sensor.v = mean.u",
    ));

    let direct_definitions = DirectDefinitionIndex::build(&dae);
    let runtime_protected_unknowns = IndexSet::new();
    let ctx = EliminationChoiceContext {
        dae: &dae,
        eq_idx: 0,
        has_state_derivative: true,
        runtime_protected_unknowns: &runtime_protected_unknowns,
        direct_definitions: &direct_definitions,
        allow_multi_live_trivial_alias: false,
    };
    let choice = choose_solvable_unknown_for_elimination(
        &ctx,
        &dae.continuous.equations[0].rhs,
        &[VarName::new("mean.u")],
    )
    .expect("candidate analysis should succeed");

    assert!(
        choice.is_none(),
        "a connection-only future definition is not a proven surviving producer"
    );
}

#[test]
fn test_boundary_keeps_producer_for_derivative_dependent_aggregate_leaf() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_dae_variable("x"));
    dae.variables
        .states
        .insert(VarName::new("rms"), test_dae_variable("rms"));
    for name in ["a", "b"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }
    dae.continuous.equations.push(residual(
        binary(OpBinary::Mul, var_ref("a"), var_ref("b")),
        lit(1.0),
        1,
        "coupled source constraint",
    ));
    dae.continuous.equations.push(residual(
        binary(OpBinary::Add, var_ref("a"), var_ref("b")),
        lit(2.0),
        1,
        "coupled source constraint",
    ));
    let mut inputs = component_var("product.u");
    inputs.dims = vec![2];
    inputs.causality = dae::VariableCausality::Input;
    dae.variables
        .outputs
        .insert(VarName::new("product.u"), inputs);
    for (name, causality) in [
        ("product.y", dae::VariableCausality::Output),
        ("sensor.v", dae::VariableCausality::Output),
        ("mean.u", dae::VariableCausality::Input),
        ("rms.u", dae::VariableCausality::Input),
        ("rms.mean.u", dae::VariableCausality::Input),
    ] {
        let mut variable = component_var(name);
        variable.causality = causality;
        dae.variables.outputs.insert(VarName::new(name), variable);
    }
    dae.continuous.equations.push(residual(
        var_ref("product.y"),
        builtin(BuiltinFunction::Product, vec![var_ref("product.u")]),
        1,
        "aggregate product",
    ));
    dae.continuous.equations.push(residual(
        der(var_ref("rms")),
        var_ref("rms.mean.u"),
        1,
        "aggregate consumer ode",
    ));
    dae.continuous
        .equations
        .push(residual(der(var_ref("x")), var_ref("mean.u"), 1, "ode"));
    dae.continuous.equations.push(residual(
        var_ref("sensor.v"),
        binary(OpBinary::Sub, var_ref("a"), var_ref("b")),
        1,
        "source definition",
    ));
    dae.continuous.equations.push(residual(
        var_ref("product.y"),
        var_ref("rms.mean.u"),
        1,
        "connection equation: product.y = rms.mean.u",
    ));
    dae.continuous.equations.push(residual(
        var_ref("rms.u"),
        Expression::VarRef {
            name: Reference::with_component_reference(
                "product.u[1]",
                component_ref("product.u[1]"),
            ),
            subscripts: Vec::new(),
            span: test_span(),
        },
        1,
        "connection equation: rms.u = product.u[1]",
    ));
    dae.continuous.equations.push(residual(
        Expression::VarRef {
            name: Reference::with_component_reference(
                "product.u[2]",
                component_ref("product.u[2]"),
            ),
            subscripts: Vec::new(),
            span: test_span(),
        },
        var_ref("sensor.v"),
        1,
        "connection equation: product.u[2] = sensor.v",
    ));
    dae.continuous.equations.push(residual(
        var_ref("sensor.v"),
        var_ref("mean.u"),
        1,
        "connection equation: sensor.v = mean.u",
    ));
    dae.continuous.equations.push(residual(
        Expression::VarRef {
            name: Reference::with_component_reference(
                "product.u[1]",
                component_ref("product.u[1]"),
            ),
            subscripts: Vec::new(),
            span: test_span(),
        },
        Expression::VarRef {
            name: Reference::with_component_reference(
                "product.u[2]",
                component_ref("product.u[2]"),
            ),
            subscripts: Vec::new(),
            span: test_span(),
        },
        1,
        "connection equation: product.u[1] = product.u[2]",
    ));

    eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    let state_names = dae.variables.states.keys().cloned().collect::<Vec<_>>();
    let state_derivatives = DerivativeNameMatcher::from_var_names(state_names.iter());
    let ode = dae
        .continuous
        .equations
        .iter()
        .find(|eq| {
            expr_contains_der_of_any(&eq.rhs, &state_derivatives)
                && expr_contains_var(&eq.rhs, &VarName::new("x"))
        })
        .expect("state derivative row must remain");
    assert!(
        expr_contains_var(&ode.rhs, &VarName::new("a"))
            && !expr_contains_var(&ode.rhs, &VarName::new("mean.u"))
            && !expr_contains_var(&ode.rhs, &VarName::new("sensor.v"))
            && !expr_contains_var(&ode.rhs, &VarName::new("product.u[2]")),
        "the derivative must resolve to the canonical physical source, got {:?}",
        ode.rhs
    );
}

#[test]
fn test_boundary_eliminates_scalarized_element_with_static_index_expression_derivative_use() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_dae_variable("x"));
    let mut v = test_dae_variable("v");
    v.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("v"), v);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref_idx("v", 2)),
            rhs: Box::new(lit(20.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "binding equation for v[2]".to_string(),
        scalar_count: 1,
    });
    let static_index = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Mod,
            args: vec![lit(3.0), lit(2.0)],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(lit(1.0)),
        span: rumoca_core::Span::DUMMY,
    };
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref_with_subscript_expr("v", static_index)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode with static subscript expression".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "v[2]"),
        "boundary-known scalarized element should be substituted into derivative use"
    );
    assert_eq!(dae.continuous.equations.len(), 1);
    let Expression::Binary { rhs, .. } = &dae.continuous.equations[0].rhs else {
        panic!("remaining ODE should stay as a binary residual");
    };
    assert!(
        matches!(
            rhs.as_ref(),
            Expression::Literal {
                value: Literal::Real(value),
                ..
            } if *value == 20.0
        ),
        "static-index derivative row should receive the boundary-known scalar value, got {:?}",
        dae.continuous.equations[0].rhs
    );
}

#[test]
fn test_boundary_eliminates_internal_output_nontrivial_definition() {
    let mut dae = Dae::new();
    let mut y = test_dae_variable("block.y");
    y.component_ref =
        rumoca_core::component_reference_from_flat_name(&VarName::new("block.y"), Span::DUMMY);
    dae.variables.outputs.insert(VarName::new("block.y"), y);
    dae.variables
        .algebraics
        .insert(VarName::new("u"), test_dae_variable("u"));
    dae.variables
        .parameters
        .insert(VarName::new("p"), test_dae_variable("p"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("block.y")),
            rhs: Box::new(Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(var_ref("p")),
                rhs: Box::new(lit(1.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "internal output definition".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("block.y")),
            rhs: Box::new(var_ref("u")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: block.y = u".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(result.blt_error.is_none());
    assert_eq!(result.n_eliminated, 2);
    assert!(dae.continuous.equations.is_empty());
    assert!(!dae.variables.outputs.contains_key(&VarName::new("block.y")));
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("u")));
}

#[test]
fn test_boundary_eliminates_internal_output_scalar_reduction_definition() {
    let mut dae = Dae::new();
    let mut y = test_dae_variable("block.y");
    y.component_ref =
        rumoca_core::component_reference_from_flat_name(&VarName::new("block.y"), Span::DUMMY);
    dae.variables.outputs.insert(VarName::new("block.y"), y);

    let mut u = test_dae_variable("block.u");
    u.dims = vec![2];
    dae.variables.algebraics.insert(VarName::new("block.u"), u);
    dae.variables
        .algebraics
        .insert(VarName::new("sink.u"), test_dae_variable("sink.u"));
    dae.variables
        .parameters
        .insert(VarName::new("p1"), test_dae_variable("p1"));
    dae.variables
        .parameters
        .insert(VarName::new("p2"), test_dae_variable("p2"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("block.y")),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Product,
                args: vec![var_ref("block.u")],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "internal reduction output definition".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("block.y")),
            rhs: Box::new(var_ref("sink.u")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: block.y = sink.u".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("block.u[1]")),
            rhs: Box::new(var_ref("p1")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "block.u[1] = p1".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("block.u[2]")),
            rhs: Box::new(var_ref("p2")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "block.u[2] = p2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(result.blt_error.is_none());
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "block.y")
    );
    assert!(!dae.variables.outputs.contains_key(&VarName::new("block.y")));
}

#[test]
fn test_boundary_keeps_connection_eq_touching_runtime_discrete_target() {
    let mut dae = Dae::new();
    dae.variables
        .outputs
        .insert(VarName::new("y"), test_dae_variable("y"));
    dae.variables
        .inputs
        .insert(VarName::new("u"), test_dae_variable("u"));

    // Runtime-discrete partition assignment target (f_m/f_z lhs) marks `y`
    // as a runtime-discrete target that must not lose alias edges.
    dae.discrete.valued_updates.push(dae::Equation {
        lhs: Some(VarName::new("y").into()),
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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.outputs.contains_key(&VarName::new("y")));
}

#[test]
fn test_boundary_keeps_zero_unknown_runtime_discrete_assignment_used_by_f_m() {
    let mut dae = Dae::new();
    dae.variables
        .discrete_valued
        .insert(VarName::new("Enable.y"), test_dae_variable("Enable.y"));
    dae.variables.discrete_valued.insert(
        VarName::new("Counter.enable"),
        test_dae_variable("Counter.enable"),
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
        lhs: Some(VarName::new("Counter.enable").into()),
        rhs: var_ref("Enable.y"),
        span: Span::DUMMY,
        origin: "explicit connection equation: Counter.enable = Enable.y".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
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
        .insert(VarName::new("y"), test_dae_variable("y"));
    dae.variables
        .discrete_reals
        .insert(VarName::new("u"), test_dae_variable("u"));

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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert!(
        result.blt_error.is_none(),
        "runtime-known assignment rows should not make BLT lowering fail"
    );
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.outputs.is_empty());
}

#[test]
fn test_boundary_eliminates_connection_rhs_output_from_source_expression() {
    let mut dae = Dae::new();
    let mut state = test_dae_variable("x");
    state.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), state);
    dae.variables
        .outputs
        .insert(VarName::new("sink.u"), test_dae_variable("sink.u"));
    dae.variables.parameters.insert(
        VarName::new("source.offset"),
        test_dae_variable("source.offset"),
    );
    dae.variables
        .discrete_valued
        .insert(VarName::new("c"), test_dae_variable("c"));

    dae.continuous
        .equations
        .push(residual(der(var_ref("x")), var_ref("sink.u"), 1, "ode"));
    dae.continuous.equations.push(residual(
        binary(
            OpBinary::Add,
            var_ref("source.offset"),
            Expression::If {
                branches: vec![(
                    Expression::VarRef {
                        name: rumoca_core::Reference::generated("c"),
                        subscripts: vec![rumoca_core::Subscript::generated_index(1, test_span())],
                        span: test_span(),
                    },
                    lit(0.0),
                )],
                else_branch: Box::new(lit(1.0)),
                span: test_span(),
            },
        ),
        var_ref("sink.u"),
        1,
        "connection equation: source.y = sink.u",
    ));

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert_eq!(result.n_eliminated, 1);
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "sink.u"),
        "source-driven input port should be substituted"
    );
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(
        !expr_contains_var(&dae.continuous.equations[0].rhs, &VarName::new("sink.u")),
        "remaining ODE should reference the source expression directly"
    );
    assert!(!dae.variables.outputs.contains_key(&VarName::new("sink.u")));
}

#[test]
fn test_boundary_eliminates_input_connected_to_non_dae_source_alias() {
    let mut dae = Dae::new();
    let mut state = test_dae_variable("x");
    state.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), state);
    let mut sink_u = test_dae_variable("sink.u");
    sink_u.causality = dae::VariableCausality::Input;
    dae.variables.outputs.insert(VarName::new("sink.u"), sink_u);

    dae.continuous.equations.push(residual(
        var_ref("source.y"),
        var_ref("sink.u"),
        1,
        "connection equation: source.y = sink.u",
    ));
    dae.continuous
        .equations
        .push(residual(der(var_ref("x")), var_ref("sink.u"), 1, "ode"));

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    let sink_sub = result
        .substitutions
        .iter()
        .find(|sub| sub.var_name.as_str() == "sink.u")
        .expect("input connected to a source alias should be eliminated");

    assert!(
        matches!(
            sink_sub.expr,
            Expression::VarRef { ref name, ref subscripts, .. }
                if name.as_str() == "source.y" && subscripts.is_empty()
        ),
        "input should resolve to the non-DAE source alias, got {:?}",
        sink_sub.expr
    );
}

#[test]
fn test_boundary_prefers_connection_input_over_source_output() {
    let mut dae = Dae::new();
    let mut source_y = test_dae_variable("source.y");
    source_y.causality = dae::VariableCausality::Output;
    dae.variables
        .outputs
        .insert(VarName::new("source.y"), source_y);
    let mut sink_u = test_dae_variable("sink.u");
    sink_u.causality = dae::VariableCausality::Input;
    dae.variables.outputs.insert(VarName::new("sink.u"), sink_u);
    dae.variables
        .algebraics
        .insert(VarName::new("consumer.u"), test_dae_variable("consumer.u"));

    dae.continuous.equations.push(residual(
        var_ref("source.y"),
        var_ref("sink.u"),
        1,
        "connection equation: source.y = sink.u",
    ));
    dae.continuous.equations.push(residual(
        var_ref("consumer.u"),
        var_ref("sink.u"),
        1,
        "consumer equation",
    ));

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    let sink_sub = result
        .substitutions
        .iter()
        .find(|sub| sub.var_name.as_str() == "sink.u")
        .expect("connection input should be eliminated");

    assert!(
        matches!(
            sink_sub.expr,
            Expression::VarRef { ref name, ref subscripts, .. }
                if name.as_str() == "source.y" && subscripts.is_empty()
        ),
        "connection should substitute sink input from source output, got {:?}",
        sink_sub.expr
    );
    assert!(
        result
            .substitutions
            .iter()
            .all(|sub| sub.var_name.as_str() != "source.y"),
        "connection alias should preserve the source output"
    );
}

#[test]
fn test_exact_subscript_index_in_dae_resolves_builtin_integer_expression() {
    let dae = Dae::new();
    let subscript = rumoca_core::Subscript::Expr {
        expr: Box::new(binary(
            OpBinary::Add,
            binary(
                OpBinary::Add,
                builtin(BuiltinFunction::Mod, vec![lit(3.0), lit(2.0)]),
                builtin(
                    BuiltinFunction::Integer,
                    vec![binary(OpBinary::Div, lit(3.0), lit(2.0))],
                ),
            ),
            lit(1.0),
        )),
        span: test_span(),
    };

    assert_eq!(exact_subscript_index_in_dae(&dae, &subscript), Some(3));
}

#[test]
fn test_eliminate_trivial_keeps_sampled_value_source_unknown() {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_dae_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("u"), test_dae_variable("u"));
    dae.variables
        .discrete_reals
        .insert(VarName::new("clk"), test_dae_variable("clk"));
    dae.variables
        .discrete_reals
        .insert(VarName::new("y"), test_dae_variable("y"));

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
        lhs: Some(VarName::new("clk").into()),
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
        lhs: Some(VarName::new("y").into()),
        rhs: Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var_ref("u"), var_ref("clk")],
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "y = sample(u, clk)".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(
        dae.variables.algebraics.contains_key(&VarName::new("u")),
        "sampled continuous helper source must stay live for f_z/f_m value reads"
    );
}
