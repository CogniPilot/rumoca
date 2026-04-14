use super::*;
use rumoca_core::Span;
use std::collections::HashSet;

fn test_scalar_var(name: &str) -> dae::Variable {
    dae::Variable {
        name: dae::VarName::new(name),
        ..Default::default()
    }
}

fn test_dae_with_vars() -> dae::Dae {
    let mut dae = dae::Dae::default();
    dae.algebraics
        .insert(dae::VarName::new("x"), test_scalar_var("x"));
    dae.algebraics
        .insert(dae::VarName::new("y"), test_scalar_var("y"));
    dae
}

fn ordered_position(ordered: &[String], name: &str) -> usize {
    ordered
        .iter()
        .position(|value| value == name)
        .unwrap_or_else(|| panic!("missing ordered target {name}"))
}

fn test_var(name: &str) -> dae::Expression {
    dae::Expression::VarRef {
        name: dae::VarName::new(name),
        subscripts: vec![],
    }
}

fn logic_expr(name: &str) -> dae::Expression {
    test_var(format!("Modelica.Electrical.Digital.Interfaces.Logic.'{name}'").as_str())
}

fn real_lit(value: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(value))
}

fn int_lit(value: i64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Integer(value))
}

fn fn_call(name: &str, args: Vec<dae::Expression>) -> dae::Expression {
    dae::Expression::FunctionCall {
        name: dae::VarName::new(name),
        args,
        is_constructor: false,
    }
}

fn simple_time_table_expr() -> dae::Expression {
    dae::Expression::Array {
        elements: vec![
            dae::Expression::Array {
                elements: vec![real_lit(0.0), real_lit(10.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![real_lit(1.0), real_lit(12.0)],
                is_matrix: true,
            },
            dae::Expression::Array {
                elements: vec![real_lit(2.0), real_lit(14.0)],
                is_matrix: true,
            },
        ],
        is_matrix: true,
    }
}

#[test]
fn canonical_var_ref_key_formats_indexed_refs() {
    let key = canonical_var_ref_key(
        &dae::VarName::new("x"),
        &[dae::Subscript::Index(2), dae::Subscript::Index(1)],
    )
    .expect("indexed key");
    assert_eq!(key, "x[2,1]");
}

#[test]
fn extract_direct_assignment_handles_residual_orientation() {
    let expr = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(5.0))),
        rhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("y"),
            subscripts: vec![],
        }),
    };
    let (target, source) = extract_direct_assignment(&expr).expect("direct assignment");
    assert_eq!(target, "y");
    assert!(
        matches!(source, dae::Expression::Literal(dae::Literal::Real(v)) if (*v - 5.0).abs() < 1.0e-12)
    );
}

#[test]
fn direct_assignment_from_equation_prefers_explicit_lhs() {
    let eq = dae::Equation::explicit(
        dae::VarName::new("z"),
        dae::Expression::Literal(dae::Literal::Integer(1)),
        Span::DUMMY,
        "test",
    );
    let (target, source) = direct_assignment_from_equation(&eq).expect("direct assignment");
    assert_eq!(target, "z");
    assert!(matches!(
        source,
        dae::Expression::Literal(dae::Literal::Integer(1))
    ));
}

#[test]
fn apply_seeded_values_to_indices_updates_solver_slice() {
    let mut y = vec![0.0, 0.0, 0.0];
    let mut env = VarEnv::<f64>::new();
    let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
    let indices = vec![1usize, 2usize];
    let values = vec![4.0, 5.0];
    let mut seeded = Vec::new();
    let (changed, updates) = apply_seeded_values_to_indices(
        &mut y,
        &mut env,
        &names,
        &indices,
        &values,
        1,
        |name, value| seeded.push((name.to_string(), value)),
    );
    assert!(changed);
    assert_eq!(updates, 2);
    assert_eq!(y[1], 4.0);
    assert_eq!(y[2], 5.0);
    assert_eq!(seeded.len(), 2);
}

#[test]
fn apply_values_to_indices_updates_solver_and_env_without_partition_gate() {
    let mut y = vec![0.0, 0.0, 0.0];
    let mut env = VarEnv::<f64>::new();
    let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
    let indices = vec![0usize, 2usize];
    let values = vec![4.0, 5.0];
    let (changed, updates) = apply_values_to_indices(&mut y, &mut env, &names, &indices, &values);
    assert!(changed);
    assert_eq!(updates, 2);
    assert_eq!(y[0], 4.0);
    assert_eq!(y[2], 5.0);
    assert_eq!(env.vars.get("x").copied().unwrap_or(0.0), 4.0);
    assert_eq!(env.vars.get("z").copied().unwrap_or(0.0), 5.0);
}

#[test]
fn apply_runtime_values_to_indices_updates_env_even_without_solver_slot() {
    let mut y = vec![0.0, 0.0];
    let mut env = VarEnv::<f64>::new();
    let names = vec!["x".to_string(), "y".to_string(), "z".to_string()];
    let indices = vec![2usize];
    let values = vec![7.0];
    let (changed, updates) =
        apply_runtime_values_to_indices(&mut y, &mut env, &names, &indices, &values, 1);
    assert!(changed);
    assert_eq!(updates, 1);
    assert_eq!(env.vars.get("z").copied().unwrap_or(0.0), 7.0);
}

#[test]
fn pre_assignment_from_initial_equation_extracts_pre_target() {
    let pre_x = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![],
        }],
    };
    let rhs = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(pre_x),
        rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
    };
    let eq = dae::Equation::residual(rhs, Span::DUMMY, "test");
    let (target, source) =
        pre_assignment_from_initial_equation(&eq).expect("pre-assignment must resolve");
    assert_eq!(target, "x");
    assert!(matches!(
        source,
        dae::Expression::Literal(dae::Literal::Real(v)) if (*v - 2.0).abs() < 1.0e-12
    ));
}

#[test]
fn is_known_assignment_name_accepts_indexed_names_for_known_bases() {
    let dae = test_dae_with_vars();
    assert!(is_known_assignment_name(&dae, "x"));
    assert!(is_known_assignment_name(&dae, "x[2]"));
    assert!(!is_known_assignment_name(&dae, "missing"));
}

#[test]
fn collect_direct_assignment_target_stats_counts_alias_and_non_alias_rows() {
    let mut dae = test_dae_with_vars();
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("x"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
        },
        Span::DUMMY,
        "non_alias",
    ));
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("x"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("y"),
                subscripts: vec![],
            }),
        },
        Span::DUMMY,
        "alias",
    ));
    let stats = collect_direct_assignment_target_stats(&dae, 0, false);
    let x_stats = stats.get("x").copied().expect("x target stats");
    assert_eq!(x_stats.total, 2);
    assert_eq!(x_stats.non_alias, 1);
}

#[test]
fn ordered_runtime_assignment_targets_for_seeds_tracks_controller_style_chain() {
    let mut dae = dae::Dae::default();
    for name in ["load.w", "ramp.y", "feedback.y", "PI.u"] {
        dae.algebraics
            .insert(dae::VarName::new(name), test_scalar_var(name));
    }
    for name in [
        "sample1.y",
        "sample2.y",
        "feedback.u1",
        "feedback.u2",
        "PI.x",
        "PI.y",
        "hold1.u",
    ] {
        dae.discrete_reals
            .insert(dae::VarName::new(name), test_scalar_var(name));
    }

    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        test_var("load.w"),
        Span::DUMMY,
        "sample1.y = load.w",
    ));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample2.y"),
        test_var("ramp.y"),
        Span::DUMMY,
        "sample2.y = ramp.y",
    ));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("feedback.u2"),
        test_var("sample1.y"),
        Span::DUMMY,
        "feedback.u2 = sample1.y",
    ));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("feedback.u1"),
        test_var("sample2.y"),
        Span::DUMMY,
        "feedback.u1 = sample2.y",
    ));
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("feedback.y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("feedback.u1")),
            rhs: Box::new(test_var("feedback.u2")),
        },
        Span::DUMMY,
        "feedback.y = feedback.u1 - feedback.u2",
    ));
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("PI.u"),
        test_var("feedback.y"),
        Span::DUMMY,
        "PI.u = feedback.y",
    ));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("PI.x"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            args: vec![test_var("PI.x")],
        },
        Span::DUMMY,
        "PI.x = previous(PI.x)",
    ));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("PI.y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(test_var("PI.x")),
            rhs: Box::new(test_var("PI.u")),
        },
        Span::DUMMY,
        "PI.y = PI.x + PI.u",
    ));
    dae.f_z.push(dae::Equation::explicit(
        dae::VarName::new("hold1.u"),
        test_var("PI.y"),
        Span::DUMMY,
        "hold1.u = PI.y",
    ));

    let ctx = build_runtime_direct_assignment_context(&dae, 0, 0);
    let ordered = ordered_runtime_assignment_targets_for_seeds(
        &ctx,
        &HashSet::from([String::from("hold1.u")]),
    );

    assert!(ordered_position(&ordered, "sample1.y") < ordered_position(&ordered, "feedback.u2"));
    assert!(ordered_position(&ordered, "sample2.y") < ordered_position(&ordered, "feedback.u1"));
    assert!(ordered_position(&ordered, "feedback.u1") < ordered_position(&ordered, "feedback.y"));
    assert!(ordered_position(&ordered, "feedback.u2") < ordered_position(&ordered, "feedback.y"));
    assert!(ordered_position(&ordered, "feedback.y") < ordered_position(&ordered, "PI.u"));
    assert!(ordered_position(&ordered, "PI.u") < ordered_position(&ordered, "PI.y"));
    assert!(ordered_position(&ordered, "PI.x") < ordered_position(&ordered, "PI.y"));
    assert!(ordered_position(&ordered, "PI.y") < ordered_position(&ordered, "hold1.u"));
}

#[test]
fn ordered_discrete_assignment_targets_tracks_controller_style_chain() {
    let mut dae = dae::Dae::default();
    for name in ["load.w", "ramp.y", "feedback.y", "PI.u"] {
        dae.algebraics
            .insert(dae::VarName::new(name), test_scalar_var(name));
    }
    for name in [
        "sample1.y",
        "sample2.y",
        "feedback.u1",
        "feedback.u2",
        "PI.x",
        "PI.y",
        "hold1.u",
    ] {
        dae.discrete_reals
            .insert(dae::VarName::new(name), test_scalar_var(name));
    }

    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("hold1.u"),
        test_var("PI.y"),
        Span::DUMMY,
        "hold1.u = PI.y",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("PI.y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(test_var("PI.x")),
            rhs: Box::new(test_var("PI.u")),
        },
        Span::DUMMY,
        "PI.y = PI.x + PI.u",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("PI.x"),
        test_var("PI.x"),
        Span::DUMMY,
        "PI.x = previous(PI.x)",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("PI.u"),
        test_var("feedback.y"),
        Span::DUMMY,
        "PI.u = feedback.y",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("feedback.y"),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("feedback.u1")),
            rhs: Box::new(test_var("feedback.u2")),
        },
        Span::DUMMY,
        "feedback.y = feedback.u1 - feedback.u2",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("feedback.u2"),
        test_var("sample1.y"),
        Span::DUMMY,
        "feedback.u2 = sample1.y",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("feedback.u1"),
        test_var("sample2.y"),
        Span::DUMMY,
        "feedback.u1 = sample2.y",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        test_var("load.w"),
        Span::DUMMY,
        "sample1.y = load.w",
    ));
    dae.f_m.push(dae::Equation::explicit(
        dae::VarName::new("sample2.y"),
        test_var("ramp.y"),
        Span::DUMMY,
        "sample2.y = ramp.y",
    ));

    let ordered = ordered_discrete_assignment_targets(&dae);

    assert!(ordered_position(&ordered, "sample1.y") < ordered_position(&ordered, "feedback.u2"));
    assert!(ordered_position(&ordered, "sample2.y") < ordered_position(&ordered, "feedback.u1"));
    assert!(ordered_position(&ordered, "feedback.u1") < ordered_position(&ordered, "feedback.y"));
    assert!(ordered_position(&ordered, "feedback.u2") < ordered_position(&ordered, "feedback.y"));
    assert!(ordered_position(&ordered, "feedback.y") < ordered_position(&ordered, "PI.u"));
    assert!(ordered_position(&ordered, "PI.u") < ordered_position(&ordered, "PI.y"));
    assert!(ordered_position(&ordered, "PI.x") < ordered_position(&ordered, "PI.y"));
    assert!(ordered_position(&ordered, "PI.y") < ordered_position(&ordered, "hold1.u"));
}

#[test]
fn direct_assignment_source_is_known_rejects_unsolved_solver_unknown_refs() {
    let dae = test_dae_with_vars();
    let rhs = dae::Expression::VarRef {
        name: dae::VarName::new("y"),
        subscripts: vec![],
    };
    let known = direct_assignment_source_is_known(&dae, &rhs, 1, 3, |target| {
        if target == "y" { Some(1) } else { None }
    });
    assert!(!known);
}

#[test]
fn extract_alias_pair_from_equation_detects_known_var_equalities() {
    let dae = test_dae_with_vars();
    let eq = dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("x"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("y"),
                subscripts: vec![],
            }),
        },
        Span::DUMMY,
        "alias",
    );
    let (lhs, rhs) = extract_alias_pair_from_equation(&dae, &eq).expect("alias pair");
    assert_eq!(lhs, "x");
    assert_eq!(rhs, "y");
}

#[test]
fn evaluate_direct_assignment_values_handles_runtime_table_getter_expression() {
    let mut env = VarEnv::<f64>::new();
    let table_id = rumoca_eval_dae::runtime::eval_expr::<f64>(
        &fn_call(
            "ExternalCombiTimeTable",
            vec![
                real_lit(0.0),
                real_lit(0.0),
                simple_time_table_expr(),
                real_lit(0.0),
                real_lit(2.0),
                int_lit(1),
                int_lit(1),
            ],
        ),
        &env,
    );
    assert!(table_id > 0.0);
    env.set("table_id", table_id);

    let expr = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(real_lit(0.25)),
        rhs: Box::new(fn_call(
            "getTimeTableValueNoDer",
            vec![test_var("table_id"), int_lit(1), real_lit(1.0)],
        )),
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values.len(), 1);
    assert!((values[0] - 12.25).abs() < 1.0e-12);
}

#[test]
fn is_discrete_name_only_matches_discrete_partitions() {
    let mut dae = test_dae_with_vars();
    dae.discrete_reals
        .insert(dae::VarName::new("d"), test_scalar_var("d"));
    assert!(is_discrete_name(&dae, "d"));
    assert!(!is_discrete_name(&dae, "x"));
}

#[test]
fn evaluate_direct_assignment_values_resolves_dynamic_raw_index_name_from_env() {
    let mut env = VarEnv::<f64>::new();
    env.set("i", 3.0);
    env.set("Tbl[3]", 4.0);
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("Tbl[i]"),
        subscripts: vec![],
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![4.0]);
}

#[test]
fn evaluate_direct_assignment_values_handles_enum_indexed_lookup_table_scalar() {
    let mut env = VarEnv::<f64>::new();
    env.enum_literal_ordinals = std::sync::Arc::new(indexmap::IndexMap::from([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
            1,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'X'".to_string(),
            2,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
            4,
        ),
    ]));
    env.set("lhs", 3.0);
    env.set("rhs", 4.0);

    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::Array {
            elements: vec![
                dae::Expression::Array {
                    elements: vec![
                        logic_expr("U"),
                        logic_expr("X"),
                        logic_expr("0"),
                        logic_expr("1"),
                    ],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![
                        logic_expr("X"),
                        logic_expr("0"),
                        logic_expr("1"),
                        logic_expr("U"),
                    ],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![
                        logic_expr("0"),
                        logic_expr("1"),
                        logic_expr("U"),
                        logic_expr("X"),
                    ],
                    is_matrix: false,
                },
                dae::Expression::Array {
                    elements: vec![
                        logic_expr("1"),
                        logic_expr("U"),
                        logic_expr("X"),
                        logic_expr("0"),
                    ],
                    is_matrix: false,
                },
            ],
            is_matrix: true,
        }),
        subscripts: vec![
            dae::Subscript::Expr(Box::new(test_var("lhs"))),
            dae::Subscript::Expr(Box::new(test_var("rhs"))),
        ],
    };

    // MLS §10.5 / §10.6.9: lookup-table style nested array indexing is a
    // scalar expression and must not collapse to the 0.0 default when the
    // fast env-key path cannot resolve it directly.
    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![2.0]);
}

#[test]
fn propagate_runtime_direct_assignments_handles_time_guarded_qualified_enum_parameters() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.extend([
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'X'".to_string(),
            2,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
            3,
        ),
        (
            "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
            4,
        ),
    ]);

    let mut before = dae::Variable::new(dae::VarName::new("Enable.before"));
    before.start = Some(logic_expr("0"));
    dae.parameters
        .insert(dae::VarName::new("Enable.before"), before);

    let mut after = dae::Variable::new(dae::VarName::new("Enable.after"));
    after.start = Some(logic_expr("1"));
    dae.parameters
        .insert(dae::VarName::new("Enable.after"), after);

    let mut step_time = dae::Variable::new(dae::VarName::new("Enable.stepTime"));
    step_time.start = Some(dae::Expression::Literal(dae::Literal::Real(1.0)));
    dae.parameters
        .insert(dae::VarName::new("Enable.stepTime"), step_time);

    dae.outputs.insert(
        dae::VarName::new("Enable.y"),
        dae::Variable::new(dae::VarName::new("Enable.y")),
    );
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("Enable.y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Ge(Default::default()),
                    lhs: Box::new(test_var("time")),
                    rhs: Box::new(test_var("Enable.stepTime")),
                },
                test_var("Enable.after"),
            )],
            else_branch: Box::new(test_var("Enable.before")),
        },
        Span::DUMMY,
        "Enable.y = if time >= Enable.stepTime then Enable.after else Enable.before",
    ));

    let ctx = build_runtime_direct_assignment_context(&dae, 1, 0);

    let mut y = vec![0.0];
    let mut env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(&dae, &[], 0.0);
    let updates =
        propagate_runtime_direct_assignments_from_env_with_context(&ctx, &dae, &mut y, 0, &mut env);
    assert!(updates > 0);
    assert_eq!(env.get("Enable.before"), 3.0);
    assert_eq!(env.get("Enable.after"), 4.0);
    assert_eq!(env.get("Enable.y"), 3.0);
    assert_eq!(y, vec![3.0]);

    let mut y = vec![0.0];
    let mut env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(&dae, &[], 2.0);
    let updates =
        propagate_runtime_direct_assignments_from_env_with_context(&ctx, &dae, &mut y, 0, &mut env);
    assert!(updates > 0);
    assert_eq!(env.get("Enable.y"), 4.0);
    assert_eq!(y, vec![4.0]);
}

#[test]
fn evaluate_direct_assignment_values_expands_scalar_varref_to_indexed_values() {
    let mut env = VarEnv::<f64>::new();
    env.set("a[1]", 1.0);
    env.set("a[2]", 2.0);
    let expr = dae::Expression::VarRef {
        name: dae::VarName::new("a"),
        subscripts: vec![],
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![1.0, 2.0]);
}

#[test]
fn evaluate_direct_assignment_values_handles_nested_indexed_scalar_inside_arithmetic() {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);
    env.set("y_old", 1.0);
    env.set("x", 3.0);
    env.set("tLH", 0.001);
    env.set("tHL", 0.001);

    let delay_table = dae::Expression::Array {
        elements: vec![
            dae::Expression::Array {
                elements: vec![int_lit(0), int_lit(0), int_lit(-1), int_lit(1)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![int_lit(0), int_lit(0), int_lit(-1), int_lit(1)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![int_lit(1), int_lit(1), int_lit(0), int_lit(1)],
                is_matrix: false,
            },
            dae::Expression::Array {
                elements: vec![int_lit(-1), int_lit(-1), int_lit(-1), int_lit(0)],
                is_matrix: false,
            },
        ],
        is_matrix: true,
    };
    let lh = dae::Expression::Index {
        base: Box::new(delay_table),
        subscripts: vec![
            dae::Subscript::Expr(Box::new(test_var("y_old"))),
            dae::Subscript::Expr(Box::new(test_var("x"))),
        ],
    };
    let delay_expr = dae::Expression::If {
        branches: vec![(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                lhs: Box::new(lh.clone()),
                rhs: Box::new(int_lit(0)),
            },
            test_var("tLH"),
        )],
        else_branch: Box::new(dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Lt(Default::default()),
                    lhs: Box::new(lh),
                    rhs: Box::new(int_lit(0)),
                },
                test_var("tHL"),
            )],
            else_branch: Box::new(int_lit(0)),
        }),
    };
    let expr = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(test_var("time")),
        rhs: Box::new(delay_expr),
    };

    // MLS §10.5 / §10.6.9: scalar arithmetic may nest indexed scalar
    // selections. The direct-assignment fast path must still evaluate the
    // full scalar expression instead of collapsing it to 0.0.
    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![0.001]);
}

#[test]
fn propagate_runtime_direct_assignments_updates_env_only_targets_without_solver_slots() {
    let mut dae = dae::Dae::default();
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'0'".to_string(),
        3,
    );
    dae.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    );

    let mut y0 = dae::Variable::new(dae::VarName::new("y0"));
    y0.start = Some(dae::Expression::VarRef {
        name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'0'"),
        subscripts: vec![],
    });
    dae.parameters.insert(dae::VarName::new("y0"), y0);
    let mut x1 = dae::Variable::new(dae::VarName::new("x[1]"));
    x1.start = Some(dae::Expression::VarRef {
        name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'1'"),
        subscripts: vec![],
    });
    dae.parameters.insert(dae::VarName::new("x[1]"), x1);
    let mut t1 = dae::Variable::new(dae::VarName::new("t[1]"));
    t1.start = Some(dae::Expression::Literal(dae::Literal::Real(1.0)));
    dae.parameters.insert(dae::VarName::new("t[1]"), t1);
    dae.discrete_valued.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Ge(Default::default()),
                    lhs: Box::new(test_var("time")),
                    rhs: Box::new(test_var("t[1]")),
                },
                test_var("x[1]"),
            )],
            else_branch: Box::new(test_var("y0")),
        },
        Span::DUMMY,
        "y = if time >= t[1] then x[1] else y0",
    ));

    let ctx = build_runtime_direct_assignment_context(&dae, 0, 0);
    let mut y = vec![];
    let mut env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(&dae, &[], 0.0);
    let updates =
        propagate_runtime_direct_assignments_from_env_with_context(&ctx, &dae, &mut y, 0, &mut env);

    assert!(updates > 0);
    assert_eq!(env.get("y0"), 3.0);
    assert_eq!(env.get("y"), 3.0);
    assert!(y.is_empty());

    let mut env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(&dae, &[], 2.0);
    let updates =
        propagate_runtime_direct_assignments_from_env_with_context(&ctx, &dae, &mut y, 0, &mut env);
    assert!(updates > 0);
    assert_eq!(env.get("y"), 4.0);
}

#[test]
fn evaluate_direct_assignment_values_reads_pre_history_for_arrays() {
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("hist[1]", 3.0);
    rumoca_eval_dae::runtime::set_pre_value("hist[2]", 4.0);
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("hist"),
            subscripts: vec![],
        }],
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![3.0, 4.0]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn evaluate_direct_assignment_values_selects_fast_if_array_branch() {
    let mut env = VarEnv::<f64>::new();
    env.set("flag", 1.0);
    let expr = dae::Expression::If {
        branches: vec![(
            dae::Expression::VarRef {
                name: dae::VarName::new("flag"),
                subscripts: vec![],
            },
            dae::Expression::Array {
                elements: vec![
                    dae::Expression::Literal(dae::Literal::Real(2.0)),
                    dae::Expression::Literal(dae::Literal::Real(3.0)),
                ],
                is_matrix: false,
            },
        )],
        else_branch: Box::new(dae::Expression::Array {
            elements: vec![
                dae::Expression::Literal(dae::Literal::Real(9.0)),
                dae::Expression::Literal(dae::Literal::Real(9.0)),
            ],
            is_matrix: false,
        }),
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![2.0, 3.0]);
}

#[test]
fn evaluate_direct_assignment_values_reads_previous_history_for_arrays() {
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("hist[1]", 5.0);
    rumoca_eval_dae::runtime::set_pre_value("hist[2]", 6.0);
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("previous"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("hist"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![5.0, 6.0]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn evaluate_direct_assignment_values_reads_field_projection_for_arrays() {
    let mut env = VarEnv::<f64>::new();
    env.set("rec[1].im", -1.0);
    env.set("rec[2].im", -2.0);
    let expr = dae::Expression::FieldAccess {
        base: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("rec"),
            subscripts: vec![],
        }),
        field: "im".to_string(),
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![-1.0, -2.0]);
}

#[test]
fn evaluate_direct_assignment_values_reads_pre_history_for_array_field_projection() {
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("rec[1].im", -3.0);
    rumoca_eval_dae::runtime::set_pre_value("rec[2].im", -4.0);
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![dae::Expression::FieldAccess {
            base: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("rec"),
                subscripts: vec![],
            }),
            field: "im".to_string(),
        }],
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![-3.0, -4.0]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_fill_builtin() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Fill,
        args: vec![dae::Expression::Literal(dae::Literal::Real(2.5))],
    };
    let values = evaluate_direct_assignment_values(&expr, &env, 3);
    assert_eq!(values, vec![2.5, 2.5, 2.5]);
}

#[test]
fn propagate_runtime_direct_assignments_reads_singleton_array_source_for_scalar_target() {
    let mut dae = test_dae_with_vars();
    dae.f_x.push(dae::Equation::explicit(
        dae::VarName::new("y"),
        dae::Expression::VarRef {
            name: dae::VarName::new("a"),
            subscripts: vec![],
        },
        Span::DUMMY,
        "y = a",
    ));

    let mut y = vec![0.0, 0.0];
    let mut env = VarEnv::<f64>::new();
    env.set("a[1]", 4.0);

    let updates = propagate_runtime_direct_assignments_from_env(&dae, &mut y, 0, &mut env);
    assert!(updates > 0);
    assert!((y[1] - 4.0).abs() <= 1.0e-12);
    assert!((env.get("y") - 4.0).abs() <= 1.0e-12);
}

#[test]
fn evaluate_direct_assignment_values_reads_singleton_tuple_scalar_fast() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::Tuple {
        elements: vec![dae::Expression::Literal(dae::Literal::Real(4.5))],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![4.5]);
}

#[test]
fn evaluate_direct_assignment_values_broadcasts_singleton_tuple_to_array_target() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::Tuple {
        elements: vec![dae::Expression::Literal(dae::Literal::Real(4.5))],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 3);
    assert_eq!(values, vec![4.5, 4.5, 4.5]);
}

#[test]
fn evaluate_direct_assignment_values_reads_tuple_elements_for_array_target() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::Tuple {
        elements: vec![
            dae::Expression::Literal(dae::Literal::Real(4.5)),
            dae::Expression::Literal(dae::Literal::Real(5.5)),
        ],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![4.5, 5.5]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_wrapper_builtins() {
    let env = VarEnv::<f64>::new();
    let wrapped_fill = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::NoEvent,
        args: vec![dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Fill,
            args: vec![dae::Expression::Literal(dae::Literal::Real(2.5))],
        }],
    };
    let wrapped_smooth = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Smooth,
        args: vec![
            dae::Expression::Literal(dae::Literal::Integer(1)),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Fill,
                args: vec![dae::Expression::Literal(dae::Literal::Real(4.0))],
            },
        ],
    };
    let wrapped_homotopy = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Homotopy,
        args: vec![
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Ones,
                args: vec![],
            },
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Zeros,
                args: vec![],
            },
        ],
    };

    assert_eq!(
        evaluate_direct_assignment_values(&wrapped_fill, &env, 2),
        vec![2.5, 2.5]
    );
    assert_eq!(
        evaluate_direct_assignment_values(&wrapped_smooth, &env, 2),
        vec![4.0, 4.0]
    );
    assert_eq!(
        evaluate_direct_assignment_values(&wrapped_homotopy, &env, 3),
        vec![1.0, 1.0, 1.0]
    );
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_dynamic_range_expression() {
    let mut env = VarEnv::<f64>::new();
    env.set("start", 2.0);
    env.set("stop", 4.0);
    let expr = dae::Expression::Range {
        start: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("start"),
            subscripts: vec![],
        }),
        step: None,
        end: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("stop"),
            subscripts: vec![],
        }),
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 3);
    assert_eq!(values, vec![2.0, 3.0, 4.0]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_cat_builtin() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Cat,
        args: vec![
            dae::Expression::Literal(dae::Literal::Integer(1)),
            dae::Expression::Range {
                start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                step: None,
                end: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
            },
            dae::Expression::Tuple {
                elements: vec![
                    dae::Expression::Literal(dae::Literal::Real(3.0)),
                    dae::Expression::Literal(dae::Literal::Real(4.0)),
                ],
            },
        ],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 4);
    assert_eq!(values, vec![1.0, 2.0, 3.0, 4.0]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_index_expression() {
    let mut env = VarEnv::<f64>::new();
    env.set("slot", 2.0);
    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::Range {
            start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            step: None,
            end: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
        }),
        subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("slot"),
            subscripts: vec![],
        }))],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![2.0]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_colon_slice_expression() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::Tuple {
            elements: vec![
                dae::Expression::Literal(dae::Literal::Real(1.0)),
                dae::Expression::Literal(dae::Literal::Real(2.0)),
                dae::Expression::Literal(dae::Literal::Real(3.0)),
            ],
        }),
        subscripts: vec![dae::Subscript::Colon],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 3);
    assert_eq!(values, vec![1.0, 2.0, 3.0]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_array_comprehension_with_filter() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::ArrayComprehension {
        expr: Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("i"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        }),
        indices: vec![dae::ComprehensionIndex {
            name: "i".to_string(),
            range: dae::Expression::Range {
                start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                step: None,
                end: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
            },
        }],
        filter: Some(Box::new(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Gt(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("i"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        })),
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![3.0, 4.0]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_indexed_field_projection() {
    let mut env = VarEnv::<f64>::new();
    env.set("slot", 2.0);
    env.set("rec[1].im", -1.0);
    env.set("rec[2].im", -2.0);
    let expr = dae::Expression::Index {
        base: Box::new(dae::Expression::FieldAccess {
            base: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("rec"),
                subscripts: vec![],
            }),
            field: "im".to_string(),
        }),
        subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("slot"),
            subscripts: vec![],
        }))],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![-2.0]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_previous_indexed_history() {
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("hist[1]", 5.0);
    rumoca_eval_dae::runtime::set_pre_value("hist[2]", 6.0);
    let mut env = VarEnv::<f64>::new();
    env.set("slot", 2.0);
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("hist".to_string(), vec![2])]));
    let expr = dae::Expression::FunctionCall {
        name: dae::VarName::new("previous"),
        args: vec![dae::Expression::Index {
            base: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("hist"),
                subscripts: vec![],
            }),
            subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("slot"),
                subscripts: vec![],
            }))],
        }],
        is_constructor: false,
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![6.0]);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_binary_array_scalar_broadcast() {
    let mut env = VarEnv::<f64>::new();
    env.set("offset", 10.0);
    let expr = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Add(Default::default()),
        lhs: Box::new(dae::Expression::Range {
            start: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            step: None,
            end: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
        }),
        rhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("offset"),
            subscripts: vec![],
        }),
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 3);
    assert_eq!(values, vec![11.0, 12.0, 13.0]);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_unary_array_minus() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::Unary {
        op: rumoca_ir_core::OpUnary::Minus(Default::default()),
        rhs: Box::new(dae::Expression::Tuple {
            elements: vec![
                dae::Expression::Literal(dae::Literal::Real(1.0)),
                dae::Expression::Literal(dae::Literal::Real(2.0)),
            ],
        }),
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 2);
    assert_eq!(values, vec![-1.0, -2.0]);
}

#[test]
fn extract_active_discrete_assignment_selects_noevent_wrapped_if_branch() {
    let residual = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
        rhs: Box::new(dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::NoEvent,
                    args: vec![dae::Expression::Literal(dae::Literal::Boolean(true))],
                },
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
                },
            )],
            else_branch: Box::new(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
            }),
        }),
    };
    let env = VarEnv::<f64>::new();
    let (target, source) =
        extract_active_discrete_assignment(&residual, &env).expect("active assignment");
    assert_eq!(target, "x");
    assert!(
        matches!(source, dae::Expression::Literal(dae::Literal::Real(v)) if (*v - 2.0).abs() <= 1.0e-12)
    );
}

#[test]
fn extract_active_discrete_assignment_selects_true_if_branch() {
    let mut env = VarEnv::<f64>::new();
    env.set("flag", 1.0);
    let residual = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
        lhs: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
        rhs: Box::new(dae::Expression::If {
            branches: vec![(
                dae::Expression::VarRef {
                    name: dae::VarName::new("flag"),
                    subscripts: vec![],
                },
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
                },
            )],
            else_branch: Box::new(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(3.0))),
            }),
        }),
    };

    let (target, source) =
        extract_active_discrete_assignment(&residual, &env).expect("active assignment");
    assert_eq!(target, "x");
    assert!(matches!(
        source,
        dae::Expression::Literal(dae::Literal::Real(v)) if (*v - 2.0).abs() < 1.0e-12
    ));
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_unary_builtin_array_source() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sin,
        args: vec![dae::Expression::Range {
            start: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
            step: None,
            end: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        }],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 3);
    assert!((values[0] - 0.0).abs() <= 1.0e-12);
    assert!((values[1] - 1.0f64.sin()).abs() <= 1.0e-12);
    assert!((values[2] - 2.0f64.sin()).abs() <= 1.0e-12);
}

#[test]
fn evaluate_direct_assignment_values_fast_paths_linspace() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Linspace,
        args: vec![
            dae::Expression::Literal(dae::Literal::Real(1.0)),
            dae::Expression::Literal(dae::Literal::Real(3.0)),
            dae::Expression::Literal(dae::Literal::Integer(3)),
        ],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 3);
    assert_eq!(values, vec![1.0, 2.0, 3.0]);
}

#[test]
fn evaluate_direct_assignment_values_keeps_scalar_size_builtin_without_scalar_eval_fallback() {
    let mut env = VarEnv::<f64>::new();
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("a".to_string(), vec![3])]));
    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Size,
        args: vec![
            dae::Expression::VarRef {
                name: dae::VarName::new("a"),
                subscripts: vec![],
            },
            dae::Expression::Literal(dae::Literal::Integer(1)),
        ],
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![3.0]);
}

#[test]
fn evaluate_direct_assignment_values_keeps_vector_inner_product_scalar_semantics() {
    let mut env = VarEnv::<f64>::new();
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([
        ("k".to_string(), vec![3]),
        ("u".to_string(), vec![3]),
    ]));
    env.set("k[1]", 1.0);
    env.set("k[2]", 1.0);
    env.set("k[3]", 1.0);
    env.set("u[1]", 0.0);
    env.set("u[2]", 0.0);
    env.set("u[3]", 1.0);
    let expr = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Mul(Default::default()),
        lhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("k"),
            subscripts: vec![],
        }),
        rhs: Box::new(dae::Expression::VarRef {
            name: dae::VarName::new("u"),
            subscripts: vec![],
        }),
    };

    let values = evaluate_direct_assignment_values(&expr, &env, 1);
    assert_eq!(values, vec![1.0]);
}

#[test]
fn evaluate_direct_assignment_values_keeps_scalar_reduction_builtins() {
    let mut env = VarEnv::<f64>::new();
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("u".to_string(), vec![3])]));
    env.set("u[1]", 2.0);
    env.set("u[2]", 3.0);
    env.set("u[3]", 4.0);
    let sum_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sum,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("u"),
            subscripts: vec![],
        }],
    };
    let product_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Product,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("u"),
            subscripts: vec![],
        }],
    };

    assert_eq!(
        evaluate_direct_assignment_values(&sum_expr, &env, 1),
        vec![9.0]
    );
    assert_eq!(
        evaluate_direct_assignment_values(&product_expr, &env, 1),
        vec![24.0]
    );
}

#[test]
fn evaluate_direct_assignment_values_keeps_boolean_vector_helpers_on_scalar_targets() {
    let mut env = VarEnv::<f64>::new();
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("u".to_string(), vec![2])]));
    env.set("u[1]", 1.0);
    env.set("u[2]", 0.0);
    let any_true = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Math.BooleanVectors.anyTrue"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("u"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };
    let and_true = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Math.BooleanVectors.andTrue"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("u"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };
    let one_true = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Math.BooleanVectors.oneTrue"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("u"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };
    let first_true_index = dae::Expression::FunctionCall {
        name: dae::VarName::new("Modelica.Math.BooleanVectors.firstTrueIndex"),
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("u"),
            subscripts: vec![],
        }],
        is_constructor: false,
    };

    assert_eq!(
        evaluate_direct_assignment_values(&any_true, &env, 1),
        vec![1.0]
    );
    assert_eq!(
        evaluate_direct_assignment_values(&and_true, &env, 1),
        vec![0.0]
    );
    assert_eq!(
        evaluate_direct_assignment_values(&one_true, &env, 1),
        vec![1.0]
    );
    assert_eq!(
        evaluate_direct_assignment_values(&first_true_index, &env, 1),
        vec![1.0]
    );
}

#[test]
fn evaluate_direct_assignment_values_keeps_negated_boolean_vector_helpers() {
    let mut env = VarEnv::<f64>::new();
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("u".to_string(), vec![2])]));
    env.set("u[1]", 1.0);
    env.set("u[2]", 0.0);
    let expr = dae::Expression::Unary {
        op: rumoca_ir_core::OpUnary::Not(Default::default()),
        rhs: Box::new(dae::Expression::FunctionCall {
            name: dae::VarName::new("Modelica.Math.BooleanVectors.andTrue"),
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }],
            is_constructor: false,
        }),
    };

    assert_eq!(evaluate_direct_assignment_values(&expr, &env, 1), vec![1.0]);
}

#[test]
fn discrete_assignment_from_equation_prefers_explicit_lhs() {
    let mut env = VarEnv::<f64>::new();
    env.set("cond", 0.0);
    let eq = dae::Equation::explicit(
        dae::VarName::new("z"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::VarRef {
                    name: dae::VarName::new("cond"),
                    subscripts: vec![],
                },
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            )],
            else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        },
        Span::DUMMY,
        "test",
    );

    let (target, source) =
        discrete_assignment_from_equation(&eq, &env).expect("explicit assignment");
    assert_eq!(target, "z");
    assert!(matches!(source, dae::Expression::If { .. }));
}

fn der_var(name: &str) -> dae::Expression {
    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Der,
        args: vec![test_var(name)],
    }
}

#[test]
fn runtime_derivative_alias_closure_prefers_state_backed_component_derivative() {
    let mut dae = dae::Dae::default();
    for name in ["load.phi", "load.w", "speed.flange.phi"] {
        dae.states
            .insert(dae::VarName::new(name), test_scalar_var(name));
    }
    dae.outputs
        .insert(dae::VarName::new("speed.w"), test_scalar_var("speed.w"));
    dae.algebraics
        .insert(dae::VarName::new("sample1.u"), test_scalar_var("sample1.u"));

    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("load.w")),
            rhs: Box::new(der_var("load.phi")),
        },
        Span::DUMMY,
        "load.w = der(load.phi)",
    ));
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("speed.flange.phi")),
            rhs: Box::new(test_var("load.phi")),
        },
        Span::DUMMY,
        "speed.flange.phi = load.phi",
    ));
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("speed.w")),
            rhs: Box::new(der_var("speed.flange.phi")),
        },
        Span::DUMMY,
        "speed.w = der(speed.flange.phi)",
    ));
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("speed.w")),
            rhs: Box::new(test_var("sample1.u")),
        },
        Span::DUMMY,
        "speed.w = sample1.u",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("load.w", 0.11);
    env.set("speed.w", 0.0);
    env.set("sample1.u", 0.0);

    let updates = propagate_runtime_derivative_aliases_from_env(&dae, 1, &mut env);
    assert!(updates >= 2);
    assert!(
        (env.get("der(load.phi)") - 0.11).abs() <= 1.0e-12,
        "state-backed derivative alias should seed the canonical state derivative"
    );
    assert!(
        (env.get("der(speed.flange.phi)") - 0.11).abs() <= 1.0e-12,
        "exact alias state peers should observe the same derivative as the state-backed source"
    );

    let mut y = vec![0.0; 5];
    let direct_updates = propagate_runtime_direct_assignments_from_env(&dae, &mut y, 1, &mut env);
    let alias_updates = crate::runtime::alias::propagate_runtime_alias_components_from_env(
        &dae, &mut y, 1, &mut env,
    );
    assert!(direct_updates >= 1);
    assert!(alias_updates >= 1);
    assert!(
        (env.get("speed.w") - 0.11).abs() <= 1.0e-12,
        "output derivative alias should refresh from propagated der(state)"
    );
    assert!(
        (env.get("sample1.u") - 0.11).abs() <= 1.0e-12,
        "runtime helper chain should materialize downstream sampled inputs"
    );
}

#[test]
fn runtime_derivative_alias_closure_handles_unary_wrapped_residual() {
    let mut dae = dae::Dae::default();
    for name in ["load.phi", "load.w", "speed.flange.phi"] {
        dae.states
            .insert(dae::VarName::new(name), test_scalar_var(name));
    }
    dae.outputs
        .insert(dae::VarName::new("speed.w"), test_scalar_var("speed.w"));
    dae.algebraics
        .insert(dae::VarName::new("sample1.u"), test_scalar_var("sample1.u"));

    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(Default::default()),
            rhs: Box::new(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(test_var("load.w")),
                rhs: Box::new(der_var("load.phi")),
            }),
        },
        Span::DUMMY,
        "-(load.w - der(load.phi))",
    ));
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("speed.flange.phi")),
            rhs: Box::new(test_var("load.phi")),
        },
        Span::DUMMY,
        "speed.flange.phi = load.phi",
    ));
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("speed.w")),
            rhs: Box::new(der_var("speed.flange.phi")),
        },
        Span::DUMMY,
        "speed.w = der(speed.flange.phi)",
    ));
    dae.f_x.push(dae::Equation::residual(
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(test_var("speed.w")),
            rhs: Box::new(test_var("sample1.u")),
        },
        Span::DUMMY,
        "speed.w = sample1.u",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("load.w", 0.11);
    env.set("speed.w", 0.0);
    env.set("sample1.u", 0.0);

    let updates = propagate_runtime_derivative_aliases_from_env(&dae, 1, &mut env);
    assert!(updates >= 2);
    assert!((env.get("der(load.phi)") - 0.11).abs() <= 1.0e-12);
    assert!((env.get("der(speed.flange.phi)") - 0.11).abs() <= 1.0e-12);

    let mut y = vec![0.0; 5];
    let direct_updates = propagate_runtime_direct_assignments_from_env(&dae, &mut y, 1, &mut env);
    let alias_updates = crate::runtime::alias::propagate_runtime_alias_components_from_env(
        &dae, &mut y, 1, &mut env,
    );
    assert!(direct_updates >= 1);
    assert!(alias_updates >= 1);
    assert!((env.get("speed.w") - 0.11).abs() <= 1.0e-12);
    assert!((env.get("sample1.u") - 0.11).abs() <= 1.0e-12);
}
