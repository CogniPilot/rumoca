use super::*;

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
fn test_eliminate_trivial_drops_exact_runtime_identity() {
    let mut dae = Dae::new();
    dae.continuous.equations.push(residual(
        var_ref("time"),
        var_ref("time"),
        1,
        "exact runtime identity",
    ));

    let result = eliminate_trivial(&mut dae).expect("exact identity should eliminate");

    assert_eq!(result.n_eliminated, 1);
    assert!(dae.continuous.equations.is_empty());
}

#[test]
fn test_arithmetic_identity_keeps_partial_expression_obligation() {
    let partial = binary(rumoca_core::OpBinary::Div, real(1.0), real(0.0));
    let expression = binary(sub_op(), partial.clone(), partial);

    let simplified = simplify_arithmetic_identities(expression);

    assert!(matches!(
        simplified,
        Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            ..
        }
    ));
}

#[test]
fn test_eliminate_trivial_keeps_vector_source_of_generated_pre_slot() {
    let mut dae = Dae::new();

    let mut state = test_dae_variable("state");
    state.dims = vec![3];
    dae.variables.states.insert(VarName::new("state"), state);

    let mut sampled = test_dae_variable("sampled");
    sampled.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("sampled"), sampled);

    let pre_name = rumoca_core::pre_slot_name("sampled");
    let mut pre_sampled = test_dae_variable(pre_name.as_str());
    pre_sampled.dims = vec![3];
    dae.variables.parameters.insert(pre_name, pre_sampled);

    dae.continuous.equations.push(dae::Equation {
        lhs: Some(reference("sampled")),
        rhs: var_ref("state"),
        span: test_span(),
        origin: "sampled vector source".to_string(),
        scalar_count: 3,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");

    assert!(
        result
            .substitutions
            .iter()
            .all(|sub| sub.var_name.as_str() != "sampled"),
        "a generated pre slot requires its continuous source to remain live"
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("sampled"))
    );
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
fn test_eliminate_trivial_eliminates_continuous_connection_array_alias() {
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

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert!(dae.continuous.equations.is_empty());
    assert!(dae.variables.algebraics.contains_key(&VarName::new("arr")));
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("plug.pin.i"))
    );
}

#[test]
fn test_eliminate_trivial_eliminates_exact_shape_connection_array_definition() {
    let mut dae = Dae::new();

    let mut target = test_dae_variable("target");
    target.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("target"), target);

    let mut source = test_dae_variable("source");
    source.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("source"), source);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("target")),
            rhs: Box::new(Expression::Unary {
                op: OpUnary::Minus,
                rhs: Box::new(var_ref("source")),
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: target = -source".to_string(),
        scalar_count: 3,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 1);
    assert!(dae.continuous.equations.is_empty());
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&VarName::new("target"))
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("source"))
    );
}

#[test]
fn test_eliminate_trivial_rejects_equal_cardinality_array_definition_shape_mismatch() {
    let mut dae = Dae::new();

    let mut target = test_dae_variable("target");
    target.dims = vec![2, 2];
    dae.variables
        .algebraics
        .insert(VarName::new("target"), target);

    let mut source = test_dae_variable("source");
    source.dims = vec![4];
    dae.variables
        .algebraics
        .insert(VarName::new("source"), source);

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: sub_op(),
            lhs: Box::new(var_ref("target")),
            rhs: Box::new(Expression::Unary {
                op: OpUnary::Minus,
                rhs: Box::new(var_ref("source")),
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "connection equation: target = -source".to_string(),
        scalar_count: 4,
    });

    let result = eliminate_trivial(&mut dae).expect("structural elimination should succeed");
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 1);
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("target"))
    );
    assert!(
        dae.variables
            .algebraics
            .contains_key(&VarName::new("source"))
    );
}

#[test]
fn eliminate_trivial_validates_compact_tensor_equations_through_a_scalar_view() {
    let mut dae = Dae::new();
    for name in ["a", "b"] {
        let mut variable = component_var(name);
        variable.dims = vec![3];
        dae.variables
            .algebraics
            .insert(VarName::new(name), variable);
    }
    let values = array(vec![lit(1.0), lit(2.0), lit(3.0)]);
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(
            sub_op(),
            binary(OpBinary::Add, var_ref("a"), var_ref("b")),
            values.clone(),
        ),
        span: test_span(),
        origin: "compact tensor sum".to_string(),
        scalar_count: 3,
    });
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: binary(
            sub_op(),
            binary(OpBinary::Sub, var_ref("a"), var_ref("b")),
            values,
        ),
        span: test_span(),
        origin: "compact tensor difference".to_string(),
        scalar_count: 3,
    });

    let result = eliminate_trivial(&mut dae).expect("compact tensor system should validate");

    assert!(result.blt_error.is_none());
    assert_eq!(result.n_eliminated, 0);
    assert_eq!(dae.continuous.equations.len(), 2);
    assert_eq!(dae.variables.algebraics[&VarName::new("a")].dims, [3]);
    assert_eq!(dae.variables.algebraics[&VarName::new("b")].dims, [3]);
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
fn test_eliminate_trivial_rejects_record_shell_alias_without_metadata() {
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

    assert!(
        matches!(
            eliminate_trivial(&mut dae),
            Err(StructuralError::ContractViolation { ref reason, .. })
                if reason.contains("missing DAE variable metadata for `a.R`")
        ),
        "record shells require resolved producer metadata"
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
fn test_eliminate_trivial_rewrites_indexed_component_field_selection() {
    let expr = Expression::FieldAccess {
        base: Box::new(Expression::Index {
            base: Box::new(var_ref("pin")),
            subscripts: vec![rumoca_core::Subscript::generated_colon(test_span())],
            span: test_span(),
        }),
        field: "v".to_string(),
        span: test_span(),
    };
    let substitutions = [("pin[1].v", 10.0), ("pin[2].v", 20.0)]
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
    let Expression::Index {
        base, subscripts, ..
    } = rewritten
    else {
        panic!("component-field selection should remain an indexed tensor expression");
    };
    assert!(matches!(base.as_ref(), Expression::Array { elements, .. } if elements.len() == 2));
    assert!(matches!(
        subscripts.as_slice(),
        [rumoca_core::Subscript::Colon { .. }]
    ));
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

    let constructor =
        Reference::new("Complex").with_resolved_function(rumoca_core::ResolvedFunctionReference {
            instance_id: rumoca_core::FunctionInstanceId::new(17),
            base_part_count: 0,
        });
    let rewritten = structural_ok(apply_substitutions_to_expr_with_derivatives(
        &expr,
        &substitutions,
        Some(&constructor),
        |_| Ok(None),
    ));
    assert!(
        !contains_exact_var_ref(&rewritten, "z"),
        "parent Complex reference should be rewritten when eliminated fields define the full value"
    );
    assert!(
        contains_complex_constructor(&rewritten),
        "complete Complex field substitutions should preserve the parent value structurally"
    );
}

#[test]
fn test_apply_elimination_substitutions_rewrites_dae_runtime_partitions() {
    let mut dae = Dae::new();
    dae.discrete.real_updates.push(dae::Equation {
        lhs: Some(VarName::new("z").into()),
        rhs: var_ref("alias"),
        span: Span::DUMMY,
        origin: "f_z".to_string(),
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
            message: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("stop".to_string()),
                span: Span::DUMMY,
            },
        },
        span: Span::DUMMY,
        origin: "assert".to_string(),
    });

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
        &dae.discrete.real_updates[0].rhs,
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
    assert!(contains_exact_var_ref(
        &dae.events.event_actions[0].condition,
        "source"
    ));
}

#[test]
fn test_eliminate_structurally_singular_boundary_resolution() {
    // Two contradictory equations both reference only `a`; `b` is unmatched.
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

    let error = eliminate_trivial(&mut dae)
        .expect_err("a=1 and a=2 must be diagnosed as an inconsistent equation set");
    assert!(matches!(
        error,
        StructuralError::InconsistentEquation {
            residual: -1.0,
            ref origin,
            ..
        } if origin == "eq2"
    ));
}
