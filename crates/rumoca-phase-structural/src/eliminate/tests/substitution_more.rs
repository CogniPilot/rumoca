use super::*;

#[test]
fn test_substitute_indexed_use_site_keeps_projected_scalar_replacement() {
    let mut dae = Dae::new();
    let mut u = test_dae_variable("block.u");
    u.dims = vec![1];
    dae.variables.algebraics.insert(VarName::new("block.u"), u);
    let mut y = test_dae_variable("block.y");
    y.dims = vec![3];
    dae.variables.outputs.insert(VarName::new("block.y"), y);

    let substitution =
        substitution_for_var(&dae, VarName::new("block.u"), var_ref_idx("block.y", 2))
            .expect("projected vector output should be a valid scalar replacement");

    let rewritten = resolve_substitutions_in_expr(&var_ref_idx("block.u", 1), &[substitution])
        .expect("substitution should rewrite indexed use site");

    let Expression::VarRef {
        name, subscripts, ..
    } = rewritten
    else {
        panic!("rewritten expression should remain a VarRef");
    };
    assert_eq!(name.as_str(), "block.y");
    assert_eq!(subscripts.len(), 1, "replacement must not become y[2,1]");
    assert!(
        matches!(
            &subscripts[0],
            rumoca_core::Subscript::Index { value: 2, .. }
        ),
        "replacement should preserve the projected physical element"
    );
}

#[test]
fn test_substitute_indexed_use_site_keeps_index_expression_scalar_replacement() {
    let substitution = Substitution {
        var_name: VarName::new("block.u"),
        var_ref: None,
        expr: Expression::Index {
            base: Box::new(var_ref("block.y")),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 2,
                span: Span::DUMMY,
            }],
            span: Span::DUMMY,
        },
        var_dims: vec![1],
        replacement_dims: Vec::new(),
        env_keys: vec!["block.u".to_string()],
    };

    let rewritten = resolve_substitutions_in_expr(&var_ref_idx("block.u", 1), &[substitution])
        .expect("substitution should rewrite indexed use site");

    let Expression::Index {
        base, subscripts, ..
    } = rewritten
    else {
        panic!("rewritten expression should remain the projected scalar Index");
    };
    let Expression::VarRef {
        name,
        subscripts: base_subscripts,
        ..
    } = base.as_ref()
    else {
        panic!("index base should remain a VarRef");
    };
    assert_eq!(name.as_str(), "block.y");
    assert!(base_subscripts.is_empty());
    assert_eq!(subscripts.len(), 1, "replacement must not become y[2,1]");
    assert!(matches!(
        &subscripts[0],
        rumoca_core::Subscript::Index { value: 2, .. }
    ));
}

#[test]
fn test_eliminate_trivial_rejects_multiscalar_array_solution_for_scalar_target() {
    let mut dae = Dae::new();
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("scalar_array_solution.mo"),
        1,
        12,
    );
    let mut y = test_dae_variable("y");
    y.dims = vec![3];
    dae.variables.outputs.insert(VarName::new("y"), y);
    for name in ["a", "b", "c"] {
        dae.variables
            .parameters
            .insert(VarName::new(name), test_dae_variable(name));
    }
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref_idx("y", 2)),
            rhs: Box::new(Expression::Array {
                elements: vec![var_ref("a"), var_ref("b"), var_ref("c")],
                is_matrix: false,
                span,
            }),
            span,
        },
        span,
        origin: "scalar target cannot equal vector".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("elimination should reject invalid solution");

    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.outputs.contains_key(&VarName::new("y")));
}

#[test]
fn test_eliminate_trivial_resolves_scalarized_internal_output_element_alias() {
    let mut dae = Dae::new();
    let mut y = test_dae_variable("block.multiplex.y");
    y.dims = vec![3];
    dae.variables
        .outputs
        .insert(VarName::new("block.multiplex.y"), y);
    dae.variables.algebraics.insert(
        VarName::new("block.multiplex.u3"),
        test_dae_variable("block.multiplex.u3"),
    );
    dae.variables
        .states
        .insert(VarName::new("body.w"), test_dae_variable("body.w"));
    dae.variables
        .states
        .insert(VarName::new("body.phi"), test_dae_variable("body.phi"));

    dae.continuous.equations.push(residual(
        var_ref_idx("block.multiplex.y", 3),
        var_ref("block.multiplex.u3"),
        1,
        "equation from block.multiplex",
    ));
    dae.continuous.equations.push(residual(
        var_ref("body.phi"),
        Expression::FunctionCall {
            name: reference("Move.position"),
            args: vec![
                array(vec![
                    var_ref("body.phi"),
                    var_ref("body.w"),
                    var_ref_idx("block.multiplex.y", 3),
                ]),
                var_ref("time"),
            ],
            is_constructor: false,
            span: Span::DUMMY,
        },
        1,
        "equation from block.move",
    ));
    dae.continuous.equations.push(residual(
        der(var_ref("body.w")),
        var_ref_idx("block.multiplex.y", 3),
        1,
        "connection equation: body.a = block.multiplex.u3[1]",
    ));

    let result = eliminate_trivial(&mut dae)
        .expect("scalarized internal output element should be eligible for substitution");

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "block.multiplex.y[3]"),
        "expected scalarized output element substitution, got {:?}",
        result.substitutions
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &VarName::new("block.multiplex.y[3]"))),
        "remaining equations should not retain the eliminated scalarized output element"
    );
}

#[test]
fn test_eliminate_trivial_resolves_internal_output_torque_definition() {
    let mut dae = Dae::new();
    dae.variables.outputs.insert(
        VarName::new("adapter.tau2"),
        test_dae_variable("adapter.tau2"),
    );
    for name in [
        "spring.c",
        "spring.phi_rel",
        "spring.w_rel",
        "body.J",
        "body.a",
    ] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_dae_variable(name));
    }

    let spring_torque = binary(
        OpBinary::Add,
        binary(
            OpBinary::Mul,
            var_ref("spring.c"),
            var_ref("spring.phi_rel"),
        ),
        binary(OpBinary::Mul, real(100.0), var_ref("spring.w_rel")),
    );
    dae.continuous.equations.push(residual(
        var_ref("adapter.tau2"),
        spring_torque,
        1,
        "equation from adapter.torqueSensor",
    ));
    dae.continuous.equations.push(residual(
        binary(OpBinary::Mul, var_ref("body.J"), var_ref("body.a")),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs: Box::new(var_ref("adapter.tau2")),
            span: Span::DUMMY,
        },
        1,
        "equation from body",
    ));

    let result =
        eliminate_trivial(&mut dae).expect("internal output torque should be substitutable");

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "adapter.tau2"),
        "expected adapter.tau2 substitution, got {:?}",
        result.substitutions
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| !expr_contains_var(&eq.rhs, &VarName::new("adapter.tau2"))),
        "remaining equations should not retain adapter.tau2"
    );
}

#[test]
fn test_scalar_blt_solution_rejects_multiscalar_array_solution_for_scalar_target() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("y"), test_dae_variable("y"));
    for name in ["a", "b", "c"] {
        dae.variables
            .parameters
            .insert(VarName::new(name), test_dae_variable(name));
    }
    dae.continuous.equations.push(residual(
        var_ref("y"),
        array(vec![var_ref("a"), var_ref("b"), var_ref("c")]),
        1,
        "scalar target cannot equal vector through BLT",
    ));

    let state_names = Vec::<VarName>::new();
    let state_derivative_matcher = DerivativeNameMatcher::from_var_names(state_names.iter());
    let result = structural_ok(scalar_blt_solution(
        &dae,
        &EquationRef(0),
        &UnknownId::Variable(VarName::new("y")),
        &IndexSet::new(),
        &HashSet::new(),
        &state_derivative_matcher,
        &[],
    ));

    assert!(
        result.is_none(),
        "BLT scalar elimination must reject scalar-to-vector substitutions"
    );
}

#[test]
fn test_elimination_substitution_differentiates_scalar_alias_in_derivative_call() {
    let mut dae = Dae::new();
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_structural_substitution_more_source_17.mo"),
        10,
        42,
    );
    dae.variables
        .states
        .insert(VarName::new("w2"), test_dae_variable("w2"));
    dae.variables
        .algebraics
        .insert(VarName::new("w1"), test_dae_variable("w1"));
    dae.variables
        .algebraics
        .insert(VarName::new("force"), test_dae_variable("force"));
    dae.variables
        .parameters
        .insert(VarName::new("ratio"), test_dae_variable("ratio"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(sub_op(), der(var_ref("w2")), var_ref("force")),
        span,
        origin: "state derivative".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(sub_op(), der(var_ref("w1")), var_ref("force")),
        span,
        origin: "alias derivative".to_string(),
        scalar_count: 1,
    });

    let substitutions = [Substitution {
        var_name: VarName::new("w1"),
        var_ref: Some(reference("w1")),
        expr: binary(rumoca_core::OpBinary::Mul, var_ref("ratio"), var_ref("w2")),
        var_dims: Vec::new(),
        replacement_dims: Vec::new(),
        env_keys: vec!["w1".to_string()],
    }];

    structural_ok(apply_elimination_substitutions_to_dae(
        &mut dae,
        &substitutions,
    ));

    let rewritten = &dae.continuous.equations[1].rhs;
    assert!(
        !expr_contains_der_of(rewritten, &VarName::new("w1")),
        "eliminated alias derivative should not remain: {rewritten:?}"
    );
    assert!(
        contains_exact_var_ref(rewritten, "ratio") && contains_exact_var_ref(rewritten, "force"),
        "der(w1) should rewrite through d/dt(ratio*w2) and resolve der(w2): {rewritten:?}"
    );
}

#[test]
fn test_scalar_substitution_stamps_source_free_replacement_root() {
    let owner_span =
        Span::from_offsets(rumoca_core::SourceId::from_source_name("visible.mo"), 4, 9);
    let expr = Expression::VarRef {
        name: reference("x"),
        subscripts: Vec::new(),
        span: owner_span,
    };
    let replacement = Expression::Array {
        elements: vec![Expression::Unary {
            op: OpUnary::Minus,
            rhs: Box::new(lit(1.0)),
            span: Span::DUMMY,
        }],
        is_matrix: false,
        span: Span::DUMMY,
    };
    let substitution = Substitution {
        var_name: VarName::new("x"),
        var_ref: Some(reference("x")),
        expr: replacement,
        var_dims: Vec::new(),
        replacement_dims: vec![1],
        env_keys: vec!["x".to_string()],
    };

    let result = structural_ok(apply_substitutions_to_expr(&expr, &[substitution]));

    assert_eq!(result.span(), Some(owner_span));
}

#[test]
fn test_apply_substitutions_rejects_short_eliminated_flags_without_panic()
-> Result<(), Box<dyn std::error::Error>> {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_structural_substitution_more_source_17.mo"),
        50,
        80,
    );
    let mut dae = Dae::new();
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(sub_op(), var_ref("x"), real(1.0)),
        span,
        origin: "short flags regression".to_string(),
        scalar_count: 1,
    });
    let substitutions = [test_substitution("x", real(1.0))];

    match apply_substitutions_to_remaining_once(&mut dae, &[], &substitutions) {
        Err(StructuralError::ContractViolation {
            reason,
            span: err_span,
        }) => {
            assert!(
                reason.contains("eliminated equation flags have length 0"),
                "unexpected reason: {reason}"
            );
            assert_eq!(err_span, span);
            Ok(())
        }
        Err(other) => Err(Box::new(other)),
        Ok(()) => Err(Box::new(std::io::Error::other(
            "short eliminated-equation flags were accepted",
        ))),
    }
}

#[test]
fn test_equation_analysis_expr_preserves_equation_span() -> Result<(), Box<dyn std::error::Error>> {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_structural_substitution_more_source_17.mo"),
        90,
        120,
    );
    let eq = dae::Equation {
        lhs: Some(reference("x")),
        rhs: real(1.0),
        span,
        origin: "analysis span regression".to_string(),
        scalar_count: 1,
    };

    let analysis = equation_analysis_expr(&eq);

    let Expression::Binary {
        lhs,
        span: analysis_span,
        ..
    } = analysis
    else {
        return Err(Box::new(std::io::Error::other(
            "equation analysis did not produce a binary residual",
        )));
    };
    assert_eq!(analysis_span, span);
    let Expression::VarRef { span: lhs_span, .. } = *lhs else {
        return Err(Box::new(std::io::Error::other(
            "equation analysis lhs was not a variable reference",
        )));
    };
    assert_eq!(lhs_span, span);
    Ok(())
}

#[test]
fn test_eliminate_trivial_keeps_runtime_partition_defined_output() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));
    dae.variables
        .outputs
        .insert(VarName::new("y"), test_dae_variable("y"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "output_alias".to_string(),
        scalar_count: 1,
    });

    dae.discrete.real_updates.push(dae::Equation {
        lhs: Some(VarName::new("y").into()),
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("z")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "runtime_partition".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 0);
    assert!(
        result.substitutions.is_empty(),
        "runtime partition dependencies should block trivial elimination"
    );
    assert!(dae.variables.outputs.contains_key(&VarName::new("y")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_keeps_branch_local_analog_helper_unknown() {
    let mut dae = Dae::new();

    let mut node = test_dae_variable("node");
    node.fixed = Some(true);
    node.start = Some(lit(0.0));
    dae.variables.algebraics.insert(VarName::new("node"), node);
    dae.variables
        .algebraics
        .insert(VarName::new("vAK"), test_dae_variable("vAK"));

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("vAK")),
            rhs: Box::new(var_ref("node")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "direct_alias".to_string(),
        scalar_count: 1,
    });

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Smooth,
                args: vec![
                    lit(0.0),
                    Expression::If {
                        branches: vec![(
                            Expression::Binary {
                                op: rumoca_core::OpBinary::Lt,
                                lhs: Box::new(var_ref("vAK")),
                                rhs: Box::new(lit(1.0)),
                                span: rumoca_core::Span::DUMMY,
                            },
                            var_ref("vAK"),
                        )],
                        else_branch: Box::new(lit(1.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref("node")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "smooth_row".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .all(|sub| sub.var_name.as_str() != "vAK"),
        "branch-local smooth/noEvent helper unknown should remain live"
    );
    assert!(dae.variables.algebraics.contains_key(&VarName::new("vAK")));
}

#[test]
fn test_eliminate_trivial_keeps_lhs_homotopy_unknown() {
    let mut dae = Dae::new();

    dae.variables
        .algebraics
        .insert(VarName::new("x"), test_dae_variable("x"));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));

    dae.continuous.equations.push(dae::Equation {
        lhs: Some(VarName::new("z").into()),
        rhs: Expression::BuiltinCall {
            function: BuiltinFunction::Homotopy,
            args: vec![var_ref("x"), lit(1.0)],
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "lhs homotopy row".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: Some(VarName::new("x").into()),
        rhs: lit(2.0),
        span: Span::DUMMY,
        origin: "x assignment".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .all(|sub| sub.var_name.as_str() != "z"),
        "lhs homotopy unknown should remain live for initialization semantics"
    );
    assert!(dae.variables.algebraics.contains_key(&VarName::new("z")));
}

#[test]
fn test_eliminate_trivial_applies_substitutions_to_initial_equations() {
    let mut dae = Dae::new();

    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_dae_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), test_dae_variable("b"));

    dae.continuous.equations.push(dae::Equation {
        lhs: Some(VarName::new("a").into()),
        rhs: lit(1.0),
        span: Span::DUMMY,
        origin: "a assignment".to_string(),
        scalar_count: 1,
    });
    dae.initialization.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("b")),
            rhs: Box::new(var_ref("a")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "initial equation".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "a"),
        "continuous trivial assignment should eliminate a"
    );
    assert!(
        !expr_contains_var(&dae.initialization.equations[0].rhs, &VarName::new("a")),
        "initial equations should be rewritten with structural substitutions"
    );
}

#[test]
fn test_eliminate_trivial_blt_keeps_fixed_alias_unknown_against_state() {
    let mut dae = Dae::new();

    let mut state = test_dae_variable("x");
    state.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), state);

    let mut fixed = test_dae_variable("y");
    fixed.fixed = Some(true);
    fixed.start = Some(lit(0.0));
    dae.variables.algebraics.insert(VarName::new("y"), fixed);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(var_ref("x")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "fixed_alias_to_state".to_string(),
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
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "state_dynamics".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        dae.variables.algebraics.contains_key(&VarName::new("y")),
        "fixed alias unknown must not be eliminated by BLT"
    );
    assert!(
        result
            .substitutions
            .iter()
            .all(|sub| sub.var_name.as_str() != "y"),
        "BLT should not create substitution for fixed unknown y"
    );
}

#[test]
fn test_eliminate_trivial_direct_assignment_with_multiple_live_unknowns() {
    let mut dae = Dae::new();

    let mut var_x = test_dae_variable("x");
    var_x.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), var_x);
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_dae_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));

    // Keep `a` coupled to dynamics so it is not trivially removable.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(var_ref("a")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    // `z` can still be eliminated because this row is a direct assignment.
    // Another live unknown (`a`) remains in the row expression.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(Expression::Binary {
                op: mul_op(),
                lhs: Box::new(var_ref("a")),
                rhs: Box::new(var_ref("a")),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "leaf".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(result.substitutions.len(), 1);
    assert_eq!(result.substitutions[0].var_name.as_str(), "z");
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(!dae.variables.algebraics.contains_key(&VarName::new("z")));
    assert!(dae.variables.algebraics.contains_key(&VarName::new("a")));
}

#[test]
fn test_eliminate_trivial_allows_output_if_assignment() {
    let mut dae = Dae::new();

    dae.variables
        .algebraics
        .insert(VarName::new("x"), test_dae_variable("x"));
    dae.variables
        .outputs
        .insert(VarName::new("y"), test_dae_variable("y"));

    // 0 = x - 1
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "x_assign".to_string(),
        scalar_count: 1,
    });

    // 0 = y - if x > 0 then x else 0
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Gt,
                        lhs: Box::new(var_ref("x")),
                        rhs: Box::new(lit(0.0)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    var_ref("x"),
                )],
                else_branch: Box::new(lit(0.0)),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "y_if_assign".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    // Non-trivial output expressions (if-then-else) should be preserved so
    // they remain visible in codegen output.
    assert!(
        !result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "y"),
        "output with non-trivial if-expression should NOT be eliminated"
    );
    assert!(
        dae.variables.outputs.contains_key(&VarName::new("y")),
        "output y should remain in the DAE"
    );
}

#[test]
fn test_eliminate_trivial_handles_singleton_array_alias_equation() {
    let mut dae = Dae::new();
    let mut aux = test_dae_variable("aux");
    aux.dims = vec![1];
    dae.variables.algebraics.insert(VarName::new("aux"), aux);
    dae.variables
        .algebraics
        .insert(VarName::new("z"), test_dae_variable("z"));

    let mut p = test_dae_variable("p");
    p.start = Some(lit(2.0));
    dae.variables.parameters.insert(VarName::new("p"), p);

    // 0 = aux[1] - p
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("aux[1]")),
            rhs: Box::new(var_ref("p")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "aux_alias".to_string(),
        scalar_count: 1,
    });
    // 0 = z - aux
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("aux")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "z_aux".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 2);
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "aux"),
        "expected canonical aux substitution in elimination result"
    );
    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "z"),
        "expected z substitution in elimination result"
    );
    assert!(
        dae.continuous.equations.is_empty(),
        "all trivial equations should be eliminated"
    );
    assert!(
        !dae.variables.algebraics.contains_key(&VarName::new("aux")),
        "singleton array alias variable should be removed from unknowns"
    );
    assert!(
        !dae.variables.algebraics.contains_key(&VarName::new("z")),
        "dependent alias variable should be removed from unknowns"
    );
}

#[test]
fn test_eliminate_trivial_derstate_kept() {
    let mut dae = Dae::new();

    let mut var_x = test_dae_variable("x");
    var_x.start = Some(Expression::Literal {
        value: Literal::Real(1.0),
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
fn test_eliminate_trivial_eliminates_array_alias_equations() {
    let mut dae = Dae::new();

    let mut arr = test_dae_variable("arr");
    arr.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("arr"), arr);

    let mut pin = test_dae_variable("plug.pin.i");
    pin.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("plug.pin.i"), pin);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("arr")),
            rhs: Box::new(var_ref("plug.pin.i")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "array_alias".to_string(),
        scalar_count: 3,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.continuous.equations.len(), 0);
    assert!(dae.variables.algebraics.contains_key(&VarName::new("arr")));
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("plug.pin.i")),
        "deeper aggregate alias should be eliminated when both sides have identical shape"
    );
}

#[test]
fn test_eliminate_trivial_keeps_discrete_path_connection_array_alias() {
    let mut dae = Dae::new();

    let mut arr = test_dae_variable("arr");
    arr.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("arr"), arr);

    let mut pin = test_dae_variable("plug.pin.i");
    pin.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("plug.pin.i"), pin);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("arr")),
            rhs: Box::new(var_ref("plug.pin.i")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: arr = plug.pin.i".to_string(),
        scalar_count: 3,
    });
    dae.discrete.real_updates.push(dae::Equation {
        lhs: Some(VarName::new("plug.pin.i").into()),
        rhs: var_ref("source"),
        span: Span::DUMMY,
        origin: "when".to_string(),
        scalar_count: 3,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.algebraics.contains_key(&VarName::new("arr")));
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("plug.pin.i"))
    );
}

#[test]
fn test_eliminate_trivial_preserves_event_referenced_array_alias() {
    let mut dae = Dae::new();

    let mut event_signal = test_dae_variable("eventSignal");
    event_signal.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("eventSignal"), event_signal);

    let mut source = test_dae_variable("source");
    source.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("source"), source);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("eventSignal")),
            rhs: Box::new(var_ref("source")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "array_alias".to_string(),
        scalar_count: 2,
    });
    dae.events
        .synthetic_root_conditions
        .push(Expression::Binary {
            op: OpBinary::Gt,
            lhs: Box::new(var_ref_idx("eventSignal", 1)),
            rhs: Box::new(lit(0.0)),
            span: rumoca_core::Span::DUMMY,
        });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("eventSignal")),
        "event root dependencies must not be eliminated"
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("source")),
        "non-event side may still be eliminated"
    );
}

#[test]
fn test_eliminate_trivial_eliminates_output_array_alias_equations() {
    let mut dae = Dae::new();

    let mut source = test_dae_variable("source");
    source.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("source"), source);

    let mut out = test_dae_variable("out");
    out.dims = vec![3];
    dae.variables.outputs.insert(VarName::new("out"), out);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("out")),
            rhs: Box::new(var_ref("source")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "output_array_alias".to_string(),
        scalar_count: 3,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert_eq!(dae.continuous.equations.len(), 0);
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("source"))
    );
    assert!(dae.variables.outputs.contains_key(&VarName::new("out")));
}

#[test]
fn test_eliminate_trivial_drops_unknown_free_record_shell_alias() {
    let mut dae = Dae::new();

    let mut lhs_field = component_var("a.R.w");
    lhs_field.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("a.R.w"), lhs_field);

    let mut rhs_field = component_var("b.R.w");
    rhs_field.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("b.R.w"), rhs_field);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a.R.w")),
            rhs: Box::new(var_ref("b.R.w")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "field alias".to_string(),
        scalar_count: 3,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a.R")),
            rhs: Box::new(var_ref("b.R")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "record shell alias".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 2);
    assert!(
        dae.continuous.equations.is_empty(),
        "record shell alias should be dropped once it has no live DAE variables"
    );
}

#[test]
fn test_eliminate_trivial_keeps_indexed_output_assignment() {
    let mut dae = Dae::new();

    let mut out = component_var("out");
    out.dims = vec![3];
    dae.variables.outputs.insert(VarName::new("out"), out);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref_idx("out", 2)),
            rhs: Box::new(lit(1.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "indexed_output_assignment".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(dae.variables.outputs.contains_key(&VarName::new("out")));
}

#[test]
fn test_eliminate_trivial_preserves_indexed_flow_reference() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("sineVoltage.sineVoltage[1].p.i"),
        component_var("sineVoltage.sineVoltage[1].p.i"),
    );

    let mut array_alias = component_var("sineVoltage.i");
    array_alias.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("sineVoltage.i"), array_alias);

    let mut pin_alias = component_var("sineVoltage.plug_p.pin.i");
    pin_alias.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("sineVoltage.plug_p.pin.i"), pin_alias);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("sineVoltage.i")),
            rhs: Box::new(var_ref("sineVoltage.plug_p.pin.i")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "array_alias".to_string(),
        scalar_count: 3,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("sineVoltage.sineVoltage[1].p.i")),
            rhs: Box::new(var_ref("sineVoltage.plug_p.pin[1].i")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "flow".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(
        result.substitutions[0].var_name.as_str(),
        "sineVoltage.plug_p.pin.i"
    );
    let flow_eq = dae
        .continuous
        .equations
        .iter()
        .find(|eq| eq.origin == "flow")
        .expect("flow equation must be preserved");
    let Expression::Binary { rhs, .. } = &flow_eq.rhs else {
        panic!("flow equation should remain binary");
    };
    let Expression::VarRef {
        name: rhs_name,
        subscripts,
        span,
    } = rhs.as_ref()
    else {
        panic!("flow rhs should remain a VarRef");
    };
    assert_eq!(rhs_name.as_str(), "sineVoltage.i");
    assert_eq!(subscripts.len(), 1);
    assert!(
        !span.is_dummy(),
        "flow rhs substitution should preserve source provenance"
    );
}

#[test]
fn test_eliminate_trivial_rewrites_indexed_component_scalar_in_derivative_rhs() {
    let mut dae = Dae::new();

    let mut omega = component_var("vehicle.motor[1].omega");
    omega.start = Some(lit(0.0));
    dae.variables
        .states
        .insert(VarName::new("vehicle.motor[1].omega"), omega);
    dae.variables.algebraics.insert(
        VarName::new("vehicle.motor[1].tau_inv"),
        component_var("vehicle.motor[1].tau_inv"),
    );
    dae.variables.parameters.insert(
        VarName::new("vehicle.motor[1].tau_inv_mid"),
        component_var("vehicle.motor[1].tau_inv_mid"),
    );
    dae.variables.parameters.insert(
        VarName::new("vehicle.motor[1].omega_error"),
        component_var("vehicle.motor[1].omega_error"),
    );

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("vehicle.motor[1].tau_inv")),
            rhs: Box::new(var_ref("vehicle.motor[1].tau_inv_mid")),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "indexed component scalar assignment".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(der(var_ref("vehicle.motor[1].omega"))),
            rhs: Box::new(Expression::Binary {
                op: mul_op(),
                lhs: Box::new(Expression::VarRef {
                    name: rumoca_core::Reference::new("vehicle.motor[1].tau_inv"),
                    subscripts: vec![],
                    span: Span::DUMMY,
                }),
                rhs: Box::new(var_ref("vehicle.motor[1].omega_error")),
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "indexed component derivative".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .any(|sub| sub.var_name.as_str() == "vehicle.motor[1].tau_inv"),
        "indexed component scalar assignment should be eliminated"
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("vehicle.motor[1].tau_inv")),
        "eliminated indexed component scalar should be removed"
    );
    let ode = dae
        .continuous
        .equations
        .iter()
        .find(|eq| eq.origin == "indexed component derivative")
        .expect("derivative equation should remain");
    assert!(
        !contains_exact_var_ref(&ode.rhs, "vehicle.motor[1].tau_inv"),
        "derivative RHS must not retain an eliminated indexed component scalar"
    );
    assert!(
        contains_exact_var_ref(&ode.rhs, "vehicle.motor[1].tau_inv_mid"),
        "derivative RHS should use the assignment replacement"
    );
}

#[test]
fn test_eliminate_trivial_skips_substitution_to_unsliced_multiscalar_solution() {
    let mut dae = Dae::new();

    dae.variables.algebraics.insert(
        VarName::new("source.pin[1].i"),
        test_dae_variable("source.pin[1].i"),
    );
    dae.variables.algebraics.insert(
        VarName::new("branch.pin.i"),
        test_dae_variable("branch.pin.i"),
    );
    let mut source_pin = test_dae_variable("source.pin.i");
    source_pin.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("source.pin.i"), source_pin);

    // Scalar-to-vector alias row that must not be used for elimination.
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("source.pin[1].i")),
            rhs: Box::new(var_ref("source.pin.i")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "scalar_vector_alias".to_string(),
        scalar_count: 1,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("branch.pin.i")),
            rhs: Box::new(var_ref("source.pin[1].i")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "flow".to_string(),
        scalar_count: 1,
    });

    eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    let flow_eq = dae
        .continuous
        .equations
        .iter()
        .find(|eq| eq.origin == "flow")
        .expect("flow equation must be preserved");
    let Expression::Binary { rhs, .. } = &flow_eq.rhs else {
        panic!("flow equation should remain binary");
    };
    let Expression::VarRef {
        name: rhs_name,
        subscripts,
        span,
    } = rhs.as_ref()
    else {
        panic!("flow rhs should remain a VarRef");
    };
    assert_eq!(rhs_name.as_str(), "source.pin[1].i");
    assert!(subscripts.is_empty());
    assert!(
        !span.is_dummy(),
        "flow rhs substitution should preserve source provenance"
    );
}

#[test]
fn test_scalarized_substitution_projects_array_comprehension_solution() {
    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        VarName::new("ductOut.fluidVolumes[1]"),
        test_dae_variable("ductOut.fluidVolumes[1]"),
    );
    let span = test_span();
    let index_ref = || Expression::VarRef {
        name: reference("i"),
        subscripts: Vec::new(),
        span,
    };
    let indexed_ref = |name: &str| Expression::VarRef {
        name: reference(name),
        subscripts: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(index_ref()),
            span,
        }],
        span,
    };
    let comprehension = Expression::ArrayComprehension {
        expr: Box::new(binary(
            mul_op(),
            indexed_ref("ductOut.crossAreas"),
            indexed_ref("ductOut.lengths"),
        )),
        indices: vec![rumoca_core::ComprehensionIndex {
            name: "i".to_string(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span,
                }),
                step: None,
                end: Box::new(Expression::Literal {
                    value: Literal::Integer(2),
                    span,
                }),
                span,
            },
        }],
        filter: None,
        span,
    };
    let aggregate_solution = binary(mul_op(), comprehension, lit(1.0));

    let substitution = substitution_for_var(
        &dae,
        VarName::new("ductOut.fluidVolumes[1]"),
        aggregate_solution,
    )
    .expect("scalarized substitution should be constructed");

    assert!(
        !contains_array_comprehension_expr(&substitution.expr),
        "scalarized substitution should project the aggregate RHS: {:?}",
        substitution.expr
    );
    assert!(
        contains_indexed_literal_ref(&substitution.expr, "ductOut.crossAreas", 1)
            && contains_indexed_literal_ref(&substitution.expr, "ductOut.lengths", 1),
        "projected substitution should select the first physical duct volume element: {:?}",
        substitution.expr
    );
}

fn contains_array_comprehension_expr(expr: &Expression) -> bool {
    match expr {
        Expression::ArrayComprehension { .. } => true,
        Expression::Binary { lhs, rhs, .. } => {
            contains_array_comprehension_expr(lhs) || contains_array_comprehension_expr(rhs)
        }
        Expression::Unary { rhs, .. } => contains_array_comprehension_expr(rhs),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(contains_array_comprehension_expr)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                contains_array_comprehension_expr(condition)
                    || contains_array_comprehension_expr(value)
            }) || contains_array_comprehension_expr(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().any(contains_array_comprehension_expr)
        }
        Expression::Range {
            start, step, end, ..
        } => {
            contains_array_comprehension_expr(start)
                || step
                    .as_ref()
                    .is_some_and(|step| contains_array_comprehension_expr(step))
                || contains_array_comprehension_expr(end)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            contains_array_comprehension_expr(base)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        contains_array_comprehension_expr(expr)
                    }
                    _ => false,
                })
        }
        Expression::FieldAccess { base, .. } => contains_array_comprehension_expr(base),
        Expression::VarRef { .. } | Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

fn contains_indexed_literal_ref(expr: &Expression, needle: &str, index: i64) -> bool {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            name.as_str() == needle
                && matches!(
                    subscripts.as_slice(),
                    [rumoca_core::Subscript::Expr {
                        expr,
                        ..
                    }] if matches!(
                        expr.as_ref(),
                        Expression::Literal {
                            value: Literal::Integer(value),
                            ..
                        } if *value == index
                    )
                )
        }
        Expression::Binary { lhs, rhs, .. } => {
            contains_indexed_literal_ref(lhs, needle, index)
                || contains_indexed_literal_ref(rhs, needle, index)
        }
        Expression::Unary { rhs, .. } => contains_indexed_literal_ref(rhs, needle, index),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| contains_indexed_literal_ref(arg, needle, index)),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                contains_indexed_literal_ref(condition, needle, index)
                    || contains_indexed_literal_ref(value, needle, index)
            }) || contains_indexed_literal_ref(else_branch, needle, index)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => elements
            .iter()
            .any(|element| contains_indexed_literal_ref(element, needle, index)),
        Expression::Range {
            start, step, end, ..
        } => {
            contains_indexed_literal_ref(start, needle, index)
                || step
                    .as_ref()
                    .is_some_and(|step| contains_indexed_literal_ref(step, needle, index))
                || contains_indexed_literal_ref(end, needle, index)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            contains_indexed_literal_ref(expr, needle, index)
                || indices
                    .iter()
                    .any(|index_def| contains_indexed_literal_ref(&index_def.range, needle, index))
                || filter
                    .as_ref()
                    .is_some_and(|filter| contains_indexed_literal_ref(filter, needle, index))
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            contains_indexed_literal_ref(base, needle, index)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        contains_indexed_literal_ref(expr, needle, index)
                    }
                    _ => false,
                })
        }
        Expression::FieldAccess { base, .. } => contains_indexed_literal_ref(base, needle, index),
        Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

#[test]
fn test_eliminate_trivial_rewrites_eliminated_indexed_record_field_aggregate() {
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Sum,
        args: vec![var_ref("pin.LossPower")],
        span: Span::DUMMY,
    };
    let substitutions = [("pin[1].LossPower", 10.0), ("pin[2].LossPower", 20.0)]
        .into_iter()
        .map(|(name, value)| Substitution {
            var_name: VarName::new(name),
            var_ref: Some(reference(name)),
            expr: lit(value),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: vec![name.to_string()],
        })
        .collect::<Vec<_>>();

    let rewritten = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));
    assert!(
        !contains_exact_var_ref(&rewritten, "pin.LossPower"),
        "aggregate record-field reference should be rewritten after scalar substitutions"
    );
    assert!(
        contains_array_expr(&rewritten),
        "aggregate record-field substitutions should materialize an array expression"
    );
}

#[test]
fn test_eliminate_trivial_rewrites_eliminated_complex_field_parent_ref() {
    let expr = Expression::FieldAccess {
        base: Box::new(Expression::Binary {
            op: add_op(),
            lhs: Box::new(var_ref("z")),
            rhs: Box::new(var_ref("w")),
            span: Span::DUMMY,
        }),
        field: "re".to_string(),
        span: Span::DUMMY,
    };
    let substitutions = [("z.re", 1.0), ("z.im", 2.0)]
        .into_iter()
        .map(|(name, value)| Substitution {
            var_name: VarName::new(name),
            var_ref: Some(reference(name)),
            expr: lit(value),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: vec![name.to_string()],
        })
        .collect::<Vec<_>>();

    let rewritten = structural_ok(apply_substitutions_to_expr(&expr, &substitutions));
    assert!(
        !contains_exact_var_ref(&rewritten, "z"),
        "parent Complex reference should be rewritten when eliminated fields define the full value"
    );
    assert!(
        contains_complex_constructor(&rewritten),
        "complete Complex field substitutions should preserve the parent value structurally"
    );
}

fn dae_with_alias_in_runtime_partitions() -> Dae {
    let mut dae = Dae::new();
    dae.initialization.equations.push(dae::Equation {
        lhs: None,
        rhs: var_ref("alias"),
        span: Span::DUMMY,
        origin: "initial".to_string(),
        scalar_count: 1,
    });
    dae.discrete.real_updates.push(dae::Equation {
        lhs: Some(VarName::new("z").into()),
        rhs: var_ref("alias"),
        span: Span::DUMMY,
        origin: "f_z".to_string(),
        scalar_count: 1,
    });
    dae.discrete.valued_updates.push(dae::Equation {
        lhs: Some(VarName::new("m").into()),
        rhs: var_ref("alias"),
        span: Span::DUMMY,
        origin: "f_m".to_string(),
        scalar_count: 1,
    });
    dae.conditions.equations.push(dae::Equation {
        lhs: Some(VarName::new("c").into()),
        rhs: var_ref("alias"),
        span: Span::DUMMY,
        origin: "f_c".to_string(),
        scalar_count: 1,
    });
    dae.conditions.relations.push(var_ref("alias"));
    dae.events.synthetic_root_conditions.push(var_ref("alias"));
    dae.clocks.constructor_exprs.push(var_ref("alias"));
    dae.clocks.triggered_conditions.push(var_ref("alias"));
    dae.events.event_actions.push(dae::DaeEventAction {
        condition: var_ref("alias"),
        kind: dae::DaeEventActionKind::Terminate {
            message: var_ref("alias"),
        },
        span: Span::DUMMY,
        origin: "assert".to_string(),
    });
    dae.events.event_actions.push(dae::DaeEventAction {
        condition: var_ref("alias"),
        kind: dae::DaeEventActionKind::Assert {
            message: var_ref("alias"),
        },
        span: Span::DUMMY,
        origin: "assert".to_string(),
    });
    dae
}

#[test]
fn test_apply_elimination_substitutions_rewrites_dae_runtime_partitions() {
    let mut dae = dae_with_alias_in_runtime_partitions();
    let substitutions = [Substitution {
        var_name: VarName::new("alias"),
        var_ref: Some(reference("alias")),
        expr: var_ref("source"),
        var_dims: Vec::new(),
        replacement_dims: Vec::new(),
        env_keys: vec!["alias".to_string()],
    }];
    structural_ok(apply_elimination_substitutions_to_dae(
        &mut dae,
        &substitutions,
    ));

    assert!(contains_exact_var_ref(
        &dae.initialization.equations[0].rhs,
        "source"
    ));
    assert!(contains_exact_var_ref(
        &dae.discrete.real_updates[0].rhs,
        "source"
    ));
    assert!(contains_exact_var_ref(
        &dae.discrete.valued_updates[0].rhs,
        "source"
    ));
    assert!(contains_exact_var_ref(
        &dae.conditions.equations[0].rhs,
        "source"
    ));
    assert!(contains_exact_var_ref(
        &dae.conditions.relations[0],
        "source"
    ));
    assert!(contains_exact_var_ref(
        &dae.events.synthetic_root_conditions[0],
        "source"
    ));
    assert!(contains_exact_var_ref(
        &dae.clocks.constructor_exprs[0],
        "source"
    ));
    assert!(contains_exact_var_ref(
        &dae.clocks.triggered_conditions[0],
        "source"
    ));
    assert!(dae.events.event_actions.iter().all(|action| {
        let message = match &action.kind {
            dae::DaeEventActionKind::Assert { message }
            | dae::DaeEventActionKind::Terminate { message } => message,
        };
        contains_exact_var_ref(&action.condition, "source")
            && contains_exact_var_ref(message, "source")
    }));
}

#[test]
fn test_eliminate_structurally_singular_boundary_resolution() {
    // 2 equations both referencing only `a`, `b` unmatched.
    // Phase A resolves a=1.0, then eq2 becomes zero-unknown and is removed.
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("a"), test_dae_variable("a"));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), test_dae_variable("b"));

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
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(lit(2.0)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "eq2".to_string(),
        scalar_count: 1,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    // Phase A: eq1 solves a=1.0 (1 unknown), eq2 becomes 0-unknown → removed.
    assert_eq!(result.n_eliminated, 2);
    assert_eq!(dae.continuous.equations.len(), 0);
}
