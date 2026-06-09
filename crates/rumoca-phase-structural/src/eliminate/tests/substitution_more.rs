use super::*;

#[test]
fn test_eliminate_trivial_keeps_runtime_partition_defined_output() {
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

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

    let mut node = dae::Variable::new(VarName::new("node"));
    node.fixed = Some(true);
    node.start = Some(lit(0.0));
    dae.variables.algebraics.insert(VarName::new("node"), node);
    dae.variables
        .algebraics
        .insert(VarName::new("vAK"), dae::Variable::new(VarName::new("vAK")));

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
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

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
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), dae::Variable::new(VarName::new("b")));

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

    let mut state = dae::Variable::new(VarName::new("x"));
    state.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), state);

    let mut fixed = dae::Variable::new(VarName::new("y"));
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

    let mut var_x = dae::Variable::new(VarName::new("x"));
    var_x.start = Some(lit(0.0));
    dae.variables.states.insert(VarName::new("x"), var_x);
    dae.variables
        .algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.variables
        .algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

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
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    dae.variables
        .outputs
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

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
    let mut aux = dae::Variable::new(VarName::new("aux"));
    aux.dims = vec![1];
    dae.variables.algebraics.insert(VarName::new("aux"), aux);
    dae.variables
        .algebraics
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

    let mut p = dae::Variable::new(VarName::new("p"));
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

    let mut var_x = dae::Variable::new(VarName::new("x"));
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

    let mut arr = dae::Variable::new(VarName::new("arr"));
    arr.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("arr"), arr);

    let mut pin = dae::Variable::new(VarName::new("plug.pin.i"));
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

    let mut arr = dae::Variable::new(VarName::new("arr"));
    arr.dims = vec![3];
    dae.variables.algebraics.insert(VarName::new("arr"), arr);

    let mut pin = dae::Variable::new(VarName::new("plug.pin.i"));
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

    let mut event_signal = dae::Variable::new(VarName::new("eventSignal"));
    event_signal.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("eventSignal"), event_signal);

    let mut source = dae::Variable::new(VarName::new("source"));
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

    let mut source = dae::Variable::new(VarName::new("source"));
    source.dims = vec![3];
    dae.variables
        .algebraics
        .insert(VarName::new("source"), source);

    let mut out = dae::Variable::new(VarName::new("out"));
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
        span: rumoca_core::Span::DUMMY,
    } = rhs.as_ref()
    else {
        panic!("flow rhs should remain a VarRef");
    };
    assert_eq!(rhs_name.as_str(), "sineVoltage.i");
    assert_eq!(subscripts.len(), 1);
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
        dae::Variable::new(VarName::new("source.pin[1].i")),
    );
    dae.variables.algebraics.insert(
        VarName::new("branch.pin.i"),
        dae::Variable::new(VarName::new("branch.pin.i")),
    );
    let mut source_pin = dae::Variable::new(VarName::new("source.pin.i"));
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
        span: rumoca_core::Span::DUMMY,
    } = rhs.as_ref()
    else {
        panic!("flow rhs should remain a VarRef");
    };
    assert_eq!(rhs_name.as_str(), "source.pin[1].i");
    assert!(subscripts.is_empty());
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

    let rewritten = apply_substitutions_to_expr(&expr, &substitutions);
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

    let rewritten = apply_substitutions_to_expr(&expr, &substitutions);
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
    apply_elimination_substitutions_to_dae(&mut dae, &substitutions);

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
    // 2 equations both referencing only `a`, `b` unmatched.
    // Phase A resolves a=1.0, then eq2 becomes zero-unknown and is removed.
    let mut dae = Dae::new();
    dae.variables
        .algebraics
        .insert(VarName::new("a"), dae::Variable::new(VarName::new("a")));
    dae.variables
        .algebraics
        .insert(VarName::new("b"), dae::Variable::new(VarName::new("b")));

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
