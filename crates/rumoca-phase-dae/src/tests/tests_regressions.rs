use super::*;

#[test]
fn test_todae_inherits_scalarized_element_start_from_array_base() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("arr"),
        flat::Variable {
            name: VarName::new("arr"),
            dims: vec![2],
            start: Some(make_var_ref(
                "Modelica.Electrical.Digital.Interfaces.Logic.'U'",
            )),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("arr[1]"),
        flat::Variable {
            name: VarName::new("arr[1]"),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.enum_literal_ordinals.insert(
        "Modelica.Electrical.Digital.Interfaces.Logic.'U'".to_string(),
        0,
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("todae should inherit scalarized element starts from array base declaration");

    let inherited = dae
        .algebraics
        .get(&dae::VarName::new("arr[1]"))
        .or_else(|| dae.discrete_reals.get(&dae::VarName::new("arr[1]")))
        .or_else(|| dae.discrete_valued.get(&dae::VarName::new("arr[1]")))
        .and_then(|v| v.start.as_ref())
        .map(|expr| format!("{expr:?}"));
    assert_eq!(
        inherited,
        Some(format!(
            "{:?}",
            make_var_ref("Modelica.Electrical.Digital.Interfaces.Logic.'U'")
        ))
    );
}

#[test]
fn test_todae_keeps_non_primitive_leaf_outputs() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("leafOut"),
        flat::Variable {
            name: VarName::new("leafOut"),
            causality: rumoca_ir_core::Causality::Output(rumoca_ir_core::Token::default()),
            variability: rumoca_ir_core::Variability::Empty,
            is_primitive: false,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("u"),
        flat::Variable {
            name: VarName::new("u"),
            causality: rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
            variability: rumoca_ir_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(make_var_ref("leafOut")),
            rhs: Box::new(make_var_ref("u")),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "leaf".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("todae should keep non-primitive leaf output variables");

    assert!(
        dae.outputs.contains_key(&dae::VarName::new("leafOut")),
        "non-primitive leaf outputs must be preserved in DAE output unknowns"
    );
}

#[test]
fn test_classify_equations_non_linearized_embedded_subscript_keeps_slice_size() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("matrix"),
        flat::Variable {
            name: VarName::new("matrix"),
            dims: vec![2, 3],
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(make_var_ref("matrix[1]")),
            rhs: Box::new(Expression::Array {
                elements: vec![
                    Expression::Literal(Literal::Integer(1)),
                    Expression::Literal(Literal::Integer(2)),
                    Expression::Literal(Literal::Integer(3)),
                ],
                is_matrix: false,
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "slice".to_string(),
        },
        scalar_count: 3,
    });

    let mut dae = Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("matrix"),
        Variable::new(dae::VarName::new("matrix")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(dae.f_x.len(), 1);
    assert_eq!(dae.f_x[0].scalar_count, 3);
}

#[test]
fn test_todae_classifies_clocked_flat_assignment_as_discrete_real_and_routes_to_f_z() {
    let mut flat = Model::new();
    let name = VarName::new("d");
    flat.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(make_var_ref("d")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("previous"),
                args: vec![make_var_ref("d")],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "clocked".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("clocked assignment should convert");

    assert!(
        dae.discrete_reals
            .contains_key(&flat_to_dae_var_name(&name)),
        "clocked assignment target must be discrete"
    );
    assert!(
        !dae.algebraics.contains_key(&flat_to_dae_var_name(&name)),
        "clocked assignment target must not remain algebraic"
    );
    assert!(
        dae.f_x.is_empty(),
        "clocked assignment must leave continuous set"
    );
    assert_eq!(
        dae.f_z.len(),
        1,
        "clocked assignment must be routed to discrete-real updates"
    );
}

#[test]
fn test_todae_routes_if_lhs_clocked_assignment_with_supersample_to_f_z() {
    let mut flat = Model::new();
    for name in ["u", "d"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..Default::default()
            },
        );
    }

    let lhs_if = Expression::If {
        branches: vec![(
            Expression::Literal(Literal::Boolean(true)),
            make_var_ref("d"),
        )],
        else_branch: Box::new(make_var_ref("d")),
    };
    let rhs_if = Expression::If {
        branches: vec![(
            Expression::Literal(Literal::Boolean(true)),
            Expression::FunctionCall {
                name: VarName::new("superSample"),
                args: vec![make_var_ref("u")],
                is_constructor: false,
            },
        )],
        else_branch: Box::new(Expression::FunctionCall {
            name: VarName::new("superSample"),
            args: vec![make_var_ref("u"), Expression::Literal(Literal::Integer(2))],
            is_constructor: false,
        }),
    };
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(lhs_if),
            rhs: Box::new(rhs_if),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "clockedIf".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("if-lhs clocked superSample assignment should convert");

    assert!(
        dae.discrete_reals.contains_key(&dae::VarName::new("d")),
        "clocked if-assignment target must be discrete"
    );
    assert!(
        dae.f_x.is_empty(),
        "clocked if-assignment with superSample must not remain in f_x"
    );
    assert_eq!(dae.f_z.len(), 1, "clocked if-assignment must route to f_z");
}

#[test]
fn test_todae_routes_clocked_binding_out_of_fx_even_without_discrete_type_flag() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("u"),
        flat::Variable {
            name: VarName::new("u"),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("usedFactor"),
        flat::Variable {
            name: VarName::new("usedFactor"),
            binding: Some(Expression::FunctionCall {
                name: VarName::new("superSample"),
                args: vec![make_var_ref("u")],
                is_constructor: false,
            }),
            // Reproduce flatten metadata regression where Integer/Boolean tagging
            // can be missing. ToDae must still keep clocked bindings out of f_x.
            is_discrete_type: false,
            is_primitive: true,
            ..Default::default()
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("clocked binding must not remain in f_x");

    assert!(
        dae.discrete_reals
            .contains_key(&dae::VarName::new("usedFactor")),
        "clocked binding variable should be classified as discrete real"
    );
    assert!(
        dae.f_x
            .iter()
            .all(|eq| !eq.origin.contains("binding equation for usedFactor")),
        "clocked binding must not be emitted as continuous residual in f_x"
    );
    assert!(
        dae.f_z
            .iter()
            .any(|eq| eq.lhs.as_ref() == Some(&dae::VarName::new("usedFactor"))),
        "clocked binding must be routed to discrete-real updates"
    );
}

#[test]
fn test_todae_routes_discrete_valued_clocked_binding_to_fm() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("u"),
        flat::Variable {
            name: VarName::new("u"),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("ticks"),
        flat::Variable {
            name: VarName::new("ticks"),
            binding: Some(Expression::FunctionCall {
                name: VarName::new("superSample"),
                args: vec![make_var_ref("u"), Expression::Literal(Literal::Integer(2))],
                is_constructor: false,
            }),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("clocked discrete-valued binding should convert");

    assert!(
        dae.discrete_valued
            .contains_key(&dae::VarName::new("ticks")),
        "discrete-valued variable must be classified into m partition"
    );
    assert!(
        dae.f_m
            .iter()
            .any(|eq| eq.lhs.as_ref() == Some(&dae::VarName::new("ticks"))),
        "clocked discrete-valued binding must be emitted as explicit f_m assignment"
    );
}

#[test]
fn test_todae_routes_clocked_tuple_assignment_to_f_z() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("noise"),
        flat::Variable {
            name: VarName::new("noise"),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("seedState"),
        flat::Variable {
            name: VarName::new("seedState"),
            dims: vec![3],
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(Expression::Tuple {
                elements: vec![make_var_ref("noise"), make_var_ref("seedState")],
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("hold"),
                args: vec![Expression::FunctionCall {
                    name: VarName::new("previous"),
                    args: vec![make_var_ref("seedState")],
                    is_constructor: false,
                }],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "clocked".to_string(),
        },
        scalar_count: 4,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("clocked tuple assignment should convert");

    assert!(dae.discrete_reals.contains_key(&dae::VarName::new("noise")));
    assert!(
        dae.discrete_valued
            .contains_key(&dae::VarName::new("seedState"))
    );
    assert!(
        dae.f_x.is_empty(),
        "clocked tuple assignment should not remain in f_x"
    );
    assert_eq!(
        dae.f_z.len(),
        1,
        "clocked tuple assignment should be routed to f_z"
    );
    assert_eq!(dae.f_z[0].scalar_count, 4);
}

#[test]
fn test_todae_routes_algorithm_when_sample_assignment_to_f_z() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("samplePeriod"),
        flat::Variable {
            name: VarName::new("samplePeriod"),
            variability: rumoca_ir_core::Variability::Parameter(rumoca_ir_core::Token::default()),
            binding: Some(Expression::Literal(Literal::Real(0.1))),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("r"),
        flat::Variable {
            name: VarName::new("r"),
            causality: rumoca_ir_core::Causality::Output(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );

    let mut algorithm = flat::Algorithm::new(Vec::new(), Span::DUMMY, "algorithm");
    algorithm.outputs.push(VarName::new("r"));
    algorithm
        .statements
        .push(flat::Statement::When(vec![flat::StatementBlock {
            cond: Expression::FunctionCall {
                name: VarName::new("sample"),
                args: vec![
                    Expression::Literal(Literal::Integer(0)),
                    make_var_ref("samplePeriod"),
                ],
                is_constructor: false,
            },
            stmts: vec![flat::Statement::Assignment {
                comp: make_comp_ref("r"),
                value: Expression::Literal(Literal::Real(1.0)),
            }],
        }]));
    flat.algorithms.push(algorithm);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("algorithm when sample assignment should lower to discrete partition");

    assert!(
        dae.discrete_reals.contains_key(&dae::VarName::new("r")),
        "algorithm when-assigned Real output must be discrete"
    );
    assert!(
        dae.f_x.is_empty(),
        "algorithm when-assigned Real output must not remain in f_x"
    );
    assert!(
        dae.f_z
            .iter()
            .any(|eq| eq.lhs.as_ref() == Some(&dae::VarName::new("r"))),
        "algorithm when-assigned Real output must be routed to f_z"
    );
}

#[test]
fn test_todae_merges_sequential_when_statements_for_same_target_in_source_order() {
    let mut flat = Model::new();
    for name in ["c1", "c2", "y"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                is_discrete_type: true,
                is_primitive: true,
                ..Default::default()
            },
        );
    }

    let mut algorithm = flat::Algorithm::new(Vec::new(), Span::DUMMY, "algorithm");
    algorithm.outputs.push(VarName::new("y"));
    algorithm
        .statements
        .push(flat::Statement::When(vec![flat::StatementBlock {
            cond: make_var_ref("c1"),
            stmts: vec![flat::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: Expression::Literal(Literal::Boolean(false)),
            }],
        }]));
    algorithm
        .statements
        .push(flat::Statement::When(vec![flat::StatementBlock {
            cond: make_var_ref("c2"),
            stmts: vec![flat::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: Expression::Literal(Literal::Boolean(true)),
            }],
        }]));
    flat.algorithms.push(algorithm);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("sequential when-statements should lower to one ordered discrete equation");

    let eq = dae
        .f_m
        .iter()
        .find(|eq| eq.lhs.as_ref() == Some(&dae::VarName::new("y")))
        .expect("expected lowered discrete equation for y");
    let dae::Expression::If {
        branches,
        else_branch,
    } = &eq.rhs
    else {
        panic!("expected merged when lowering to an If expression");
    };
    assert_eq!(
        branches.len(),
        2,
        "expected both when-statements to be preserved"
    );
    assert!(
        matches!(
            &branches[0].1,
            dae::Expression::Literal(dae::Literal::Boolean(true))
        ),
        "later when-statement must win when both guards are true"
    );
    assert!(
        matches!(
            &branches[1].1,
            dae::Expression::Literal(dae::Literal::Boolean(false))
        ),
        "earlier when-statement must be preserved as lower-priority branch"
    );
    let dae::Expression::If {
        branches: initial_branches,
        else_branch: inactive_else,
    } = else_branch.as_ref()
    else {
        panic!("ordinary when lowering must preserve the initial-section value before pre(y)");
    };
    assert_eq!(initial_branches.len(), 1);
    assert!(matches!(
        &initial_branches[0].0,
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            ..
        }
    ));
    assert!(matches!(
        &initial_branches[0].1,
        dae::Expression::VarRef { .. }
    ));
    assert!(matches!(
        inactive_else.as_ref(),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Pre,
            ..
        }
    ));
}

fn add_tick_based_discrete_vars(flat: &mut Model) {
    for name in ["counter", "startOutput"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                is_discrete_type: true,
                is_primitive: true,
                ..Default::default()
            },
        );
    }
    flat.add_variable(
        VarName::new("startTick"),
        flat::Variable {
            name: VarName::new("startTick"),
            variability: rumoca_ir_core::Variability::Parameter(rumoca_ir_core::Token::default()),
            binding: Some(Expression::Literal(Literal::Integer(4))),
            is_primitive: true,
            ..Default::default()
        },
    );
}

fn previous_call(name: &str) -> Expression {
    Expression::FunctionCall {
        name: VarName::new("previous"),
        args: vec![make_var_ref(name)],
        is_constructor: false,
    }
}

fn sub_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_ir_flat::OpBinary::Sub(rumoca_ir_core::Token::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn add_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_ir_flat::OpBinary::Add(rumoca_ir_core::Token::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn ge_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_ir_flat::OpBinary::Ge(rumoca_ir_core::Token::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn tick_based_if_residual() -> Expression {
    Expression::If {
        branches: vec![(
            previous_call("startOutput"),
            sub_expr(
                make_var_ref("counter"),
                add_expr(
                    previous_call("counter"),
                    Expression::Literal(Literal::Integer(1)),
                ),
            ),
        )],
        else_branch: Box::new(sub_expr(
            make_var_ref("startOutput"),
            ge_expr(
                previous_call("counter"),
                sub_expr(
                    make_var_ref("startTick"),
                    Expression::Literal(Literal::Integer(1)),
                ),
            ),
        )),
    }
}

fn add_tick_based_equation(flat: &mut Model, residual: Expression) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual,
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "tickBasedDiscrete".to_string(),
        },
        scalar_count: 1,
    });
}

#[test]
fn test_todae_routes_zero_minus_if_discrete_assignments_to_f_m() {
    let orientations = vec![
        (
            "0 - if(...)",
            sub_expr(
                Expression::Literal(Literal::Integer(0)),
                tick_based_if_residual(),
            ),
        ),
        (
            "if(...) - 0",
            sub_expr(
                tick_based_if_residual(),
                Expression::Literal(Literal::Integer(0)),
            ),
        ),
        ("if(...)", tick_based_if_residual()),
    ];

    for (label, residual) in orientations {
        let mut flat = Model::new();
        add_tick_based_discrete_vars(&mut flat);
        add_tick_based_equation(&mut flat, residual);

        let dae = to_dae_with_options(
            &flat,
            ToDaeOptions {
                error_on_unbalanced: false,
            },
        )
        .unwrap_or_else(|err| {
            panic!("clocked if-residual assignment should convert ({label}): {err:?}")
        });

        assert!(
            dae.discrete_valued
                .contains_key(&dae::VarName::new("counter"))
        );
        assert!(
            dae.discrete_valued
                .contains_key(&dae::VarName::new("startOutput"))
        );
        assert_eq!(
            dae.f_m.len(),
            2,
            "orientation {label}: residual should canonicalize to explicit assignments for both discrete targets"
        );
        assert!(
            dae.f_x.is_empty(),
            "orientation {label}: if-residual assignment must not remain in continuous partition"
        );
    }
}

#[test]
fn test_todae_keeps_time_guarded_discrete_output_binding_and_alias_consumer() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("Enable.stepTime"),
        flat::Variable {
            name: VarName::new("Enable.stepTime"),
            variability: rumoca_ir_core::Variability::Parameter(rumoca_ir_core::Token::default()),
            is_primitive: true,
            binding: Some(Expression::Literal(Literal::Real(1.0))),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Enable.y"),
        flat::Variable {
            name: VarName::new("Enable.y"),
            causality: rumoca_ir_core::Causality::Output(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Counter.enable"),
        flat::Variable {
            name: VarName::new("Counter.enable"),
            causality: rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            ..Default::default()
        },
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: sub_expr(
            make_var_ref("Enable.y"),
            Expression::If {
                branches: vec![(
                    ge_expr(make_var_ref("time"), make_var_ref("Enable.stepTime")),
                    Expression::Literal(Literal::Integer(4)),
                )],
                else_branch: Box::new(Expression::Literal(Literal::Integer(3))),
            },
        ),
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "Enable".to_string(),
        },
        scalar_count: 1,
    });
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: sub_expr(make_var_ref("Counter.enable"), make_var_ref("Enable.y")),
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "Counter.enable".to_string(),
            rhs: "Enable.y".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("todae should preserve time-guarded discrete output bindings");

    assert!(
        dae.discrete_valued
            .contains_key(&dae::VarName::new("Enable.y"))
            || dae
                .discrete_reals
                .contains_key(&dae::VarName::new("Enable.y")),
        "discrete output binding target must stay in the discrete variable partition",
    );
    assert!(
        dae.discrete_valued
            .contains_key(&dae::VarName::new("Counter.enable"))
            || dae
                .discrete_reals
                .contains_key(&dae::VarName::new("Counter.enable")),
        "discrete alias consumer must stay in the discrete variable partition",
    );
    assert!(
        dae.f_m.iter().any(|eq| eq
            .lhs
            .as_ref()
            .is_some_and(|lhs| lhs.as_str() == "Enable.y")),
        "time-guarded discrete output binding must be routed to f_m",
    );
    assert!(
        dae.f_m.iter().any(|eq| {
            eq.lhs
                .as_ref()
                .is_some_and(|lhs| lhs.as_str() == "Counter.enable")
        }),
        "discrete alias consumer must stay in f_m",
    );
}

#[test]
fn test_todae_converts_non_primitive_leaf_discrete_binding_to_f_m() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("Enable.stepTime"),
        flat::Variable {
            name: VarName::new("Enable.stepTime"),
            variability: rumoca_ir_core::Variability::Parameter(rumoca_ir_core::Token::default()),
            is_primitive: true,
            binding: Some(Expression::Literal(Literal::Real(1.0))),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Enable.y"),
        flat::Variable {
            name: VarName::new("Enable.y"),
            causality: rumoca_ir_core::Causality::Output(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            binding: Some(Expression::If {
                branches: vec![(
                    ge_expr(make_var_ref("time"), make_var_ref("Enable.stepTime")),
                    Expression::Literal(Literal::Integer(4)),
                )],
                else_branch: Box::new(Expression::Literal(Literal::Integer(3))),
            }),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Counter.enable"),
        flat::Variable {
            name: VarName::new("Counter.enable"),
            causality: rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            ..Default::default()
        },
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: sub_expr(make_var_ref("Counter.enable"), make_var_ref("Enable.y")),
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "Counter.enable".to_string(),
            rhs: "Enable.y".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("todae should keep non-primitive leaf discrete bindings");

    assert!(
        dae.f_m.iter().any(|eq| eq
            .lhs
            .as_ref()
            .is_some_and(|lhs| lhs.as_str() == "Enable.y")),
        "non-primitive leaf discrete bindings must contribute an explicit f_m producer",
    );
    assert!(
        dae.f_m.iter().any(|eq| {
            eq.lhs
                .as_ref()
                .is_some_and(|lhs| lhs.as_str() == "Counter.enable")
        }),
        "the discrete alias consumer must remain in f_m alongside the producer",
    );
}

#[test]
fn test_top_level_connector_members_use_component_anchoring() {
    let mut flat = Model::new();
    flat.top_level_connectors.insert("controlBus".to_string());

    for (name, causality, from_expandable_connector) in [
        ("controlBus.axis1", rumoca_ir_core::Causality::Empty, true),
        (
            "path.controlBus.axis1",
            rumoca_ir_core::Causality::Empty,
            true,
        ),
        (
            "axis.controlBus.axis1",
            rumoca_ir_core::Causality::Empty,
            true,
        ),
        ("controlBus.axis2", rumoca_ir_core::Causality::Empty, true),
        (
            "path.controlBus.axis2",
            rumoca_ir_core::Causality::Empty,
            true,
        ),
        ("controlBus.axis3", rumoca_ir_core::Causality::Empty, true),
        (
            "path.controlBus.axis3",
            rumoca_ir_core::Causality::Empty,
            true,
        ),
        (
            "axis.controlBus.axis3",
            rumoca_ir_core::Causality::Empty,
            true,
        ),
        (
            "sink.u",
            rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
            false,
        ),
        (
            "internal.source",
            rumoca_ir_core::Causality::Output(rumoca_ir_core::Token::default()),
            false,
        ),
    ] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                variability: rumoca_ir_core::Variability::Empty,
                causality,
                is_primitive: true,
                connected: true,
                from_expandable_connector,
                ..Default::default()
            },
        );
    }

    // axis1 has an internal anchor through a non-connection equation chain.
    add_connection_equation(&mut flat, "path.controlBus.axis1", "controlBus.axis1");
    add_connection_equation(&mut flat, "controlBus.axis1", "axis.controlBus.axis1");
    add_component_equation(
        &mut flat,
        "axis.controlBus.axis1",
        make_var_ref("internal.source"),
    );
    add_component_equation(
        &mut flat,
        "internal.source",
        Expression::Literal(Literal::Integer(1)),
    );

    // axis2 is an unanchored pass-through connection set.
    add_connection_equation(&mut flat, "path.controlBus.axis2", "controlBus.axis2");

    // axis3 propagates through connector aliases into an internal input sink.
    // It should still behave as an external input (no internal defining equation).
    add_connection_equation(&mut flat, "path.controlBus.axis3", "controlBus.axis3");
    add_connection_equation(&mut flat, "controlBus.axis3", "axis.controlBus.axis3");
    add_connection_equation(&mut flat, "axis.controlBus.axis3", "sink.u");

    let state_vars: HashSet<VarName> = HashSet::default();
    let connector_inputs = find_top_level_connector_input_members(&flat, &state_vars);
    assert!(
        connector_inputs.contains(&VarName::new("controlBus.axis2")),
        "unanchored top-level connector field should become an interface input"
    );
    assert!(
        connector_inputs.contains(&VarName::new("controlBus.axis3")),
        "top-level connector field that only feeds internal inputs should become an interface input"
    );
    assert!(
        !connector_inputs.contains(&VarName::new("controlBus.axis1")),
        "anchored top-level connector field should remain an internal unknown"
    );

    let dae = to_dae(&flat).expect("to_dae should succeed");
    assert_eq!(
        rumoca_eval_dae::analysis::balance(&dae),
        0,
        "component-anchored and unanchored connector sets should both balance"
    );
}

#[test]
fn test_classify_equations_linearized_embedded_subscript_is_scalarized() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("interp"),
        flat::Variable {
            name: VarName::new("interp"),
            dims: vec![1, 4],
            is_primitive: true,
            ..Default::default()
        },
    );
    for idx in 1..=4 {
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
                lhs: Box::new(make_var_ref(&format!("interp[{idx}]"))),
                rhs: Box::new(Expression::Literal(Literal::Integer(idx.into()))),
            },
            span: Span::DUMMY,
            origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
                component: "linearized".to_string(),
            },
            scalar_count: 4,
        });
    }

    let mut dae = Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("interp"),
        Variable::new(dae::VarName::new("interp")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(dae.f_x.len(), 4);
    assert!(dae.f_x.iter().all(|eq| eq.scalar_count == 1));
    assert_eq!(dae.f_x.iter().map(|eq| eq.scalar_count).sum::<usize>(), 4);
}

#[test]
fn test_connected_discrete_input_alias_keeps_discrete_partition() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("inner.flag"),
        flat::Variable {
            name: VarName::new("inner.flag"),
            causality: rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
            variability: rumoca_ir_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("inner.flagAlias"),
        flat::Variable {
            name: VarName::new("inner.flagAlias"),
            causality: rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
            variability: rumoca_ir_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    add_connection_equation(&mut flat, "inner.flagAlias", "inner.flag");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for connected discrete input aliases");

    for name in ["inner.flag", "inner.flagAlias"] {
        let var = dae::VarName::new(name);
        assert!(
            dae.discrete_valued.contains_key(&var),
            "discrete connected input {name} should be classified to m"
        );
        assert!(
            !dae.algebraics.contains_key(&var),
            "discrete connected input {name} must not be promoted to continuous algebraics"
        );
    }

    assert_eq!(
        dae.f_m.len(),
        1,
        "connected discrete input alias must contribute one discrete-valued equation"
    );
    assert_eq!(
        rumoca_eval_dae::analysis::balance(&dae),
        0,
        "discrete connected input aliases must not affect continuous balance"
    );
}

#[test]
fn test_connected_real_input_propagates_discrete_partition_from_peer() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("inner.clocked"),
        flat::Variable {
            name: VarName::new("inner.clocked"),
            causality: rumoca_ir_core::Causality::Output(rumoca_ir_core::Token::default()),
            variability: rumoca_ir_core::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("inner.u"),
        flat::Variable {
            name: VarName::new("inner.u"),
            causality: rumoca_ir_core::Causality::Input(rumoca_ir_core::Token::default()),
            variability: rumoca_ir_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );

    add_connection_equation(&mut flat, "inner.u", "inner.clocked");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for connected clocked real inputs");

    assert!(
        dae.discrete_reals
            .contains_key(&dae::VarName::new("inner.u")),
        "connected real input should become discrete when tied to a discrete peer"
    );
    assert!(
        !dae.algebraics.contains_key(&dae::VarName::new("inner.u")),
        "connected real input should not remain continuous algebraic"
    );
    assert_eq!(
        rumoca_eval_dae::analysis::balance(&dae),
        0,
        "discrete connection propagation should avoid continuous balance deficits"
    );
}

#[test]
fn test_when_clause_guard_for_var_condition_uses_edge_activation() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("flag"),
        flat::Variable {
            name: VarName::new("flag"),
            variability: rumoca_ir_core::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            start: Some(Expression::Literal(Literal::Boolean(false))),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("x"),
        flat::Variable {
            name: VarName::new("x"),
            variability: rumoca_ir_core::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            start: Some(Expression::Literal(Literal::Real(0.0))),
            ..Default::default()
        },
    );

    let mut when_clause = flat::WhenClause::new(make_var_ref("flag"), Span::DUMMY);
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("x"),
        make_var_ref("time"),
        Span::DUMMY,
        "when assignment",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("when clause should lower to guarded discrete update");

    let guarded = dae
        .f_z
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|name| name.as_str() == "x"))
        .expect("expected guarded when equation for x in f_z");

    let dae::Expression::If { branches, .. } = &guarded.rhs else {
        panic!("guarded when equation should lower to if-expression");
    };
    assert_eq!(branches.len(), 1);
    let cond = &branches[0].0;
    let dae::Expression::BuiltinCall { function, args } = cond else {
        panic!("guard condition should be lowered to edge(...)");
    };
    assert_eq!(*function, dae::BuiltinFunction::Edge);
    assert_eq!(args.len(), 1);
    match &args[0] {
        dae::Expression::VarRef { name, subscripts } => {
            assert_eq!(name.as_str(), "flag");
            assert!(subscripts.is_empty());
        }
        other => panic!("expected edge argument to be VarRef(flag), got {other:?}"),
    }
}

#[test]
fn test_when_clause_guard_for_vector_var_conditions_uses_edge_activation_per_element() {
    let mut flat = Model::new();
    for name in ["trigger", "reset"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                variability: rumoca_ir_core::Variability::Discrete(
                    rumoca_ir_core::Token::default(),
                ),
                is_discrete_type: true,
                is_primitive: true,
                start: Some(Expression::Literal(Literal::Boolean(false))),
                ..Default::default()
            },
        );
    }
    flat.add_variable(
        VarName::new("y"),
        flat::Variable {
            name: VarName::new("y"),
            variability: rumoca_ir_core::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_primitive: true,
            start: Some(Expression::Literal(Literal::Integer(0))),
            ..Default::default()
        },
    );

    let mut when_clause = flat::WhenClause::new(
        Expression::Array {
            elements: vec![make_var_ref("trigger"), make_var_ref("reset")],
            is_matrix: false,
        },
        Span::DUMMY,
    );
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("y"),
        Expression::Literal(Literal::Integer(1)),
        Span::DUMMY,
        "when vector assignment",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("vector when clause should lower to guarded discrete update");

    let guarded = dae
        .f_m
        .iter()
        .chain(dae.f_z.iter())
        .find(|eq| eq.lhs.as_ref().is_some_and(|name| name.as_str() == "y"))
        .expect("expected guarded when equation for y in discrete partitions");

    let dae::Expression::If { branches, .. } = &guarded.rhs else {
        panic!("guarded when equation should lower to if-expression");
    };
    assert_eq!(branches.len(), 1);
    let dae::Expression::Array { elements, .. } = &branches[0].0 else {
        panic!("expected vectorized guard condition");
    };
    assert_eq!(elements.len(), 2);
    for (element, expected) in elements.iter().zip(["trigger", "reset"]) {
        let dae::Expression::BuiltinCall { function, args } = element else {
            panic!("each vectorized guard element should be lowered to edge(...)");
        };
        assert_eq!(*function, dae::BuiltinFunction::Edge);
        assert_eq!(args.len(), 1);
        match &args[0] {
            dae::Expression::VarRef { name, subscripts } => {
                assert_eq!(name.as_str(), expected);
                assert!(subscripts.is_empty());
            }
            other => panic!("expected edge argument to be VarRef({expected}), got {other:?}"),
        }
    }
}

fn make_lt_expr(lhs: &str, rhs: i64) -> Expression {
    Expression::Binary {
        op: rumoca_ir_core::OpBinary::Lt(rumoca_ir_core::Token::default()),
        lhs: Box::new(make_var_ref(lhs)),
        rhs: Box::new(Expression::Literal(Literal::Integer(rhs))),
    }
}

fn build_when_condition_alias_model(use_alias_guard: bool) -> Model {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("x"),
        flat::Variable {
            name: VarName::new("x"),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("belowGround"),
        flat::Variable {
            name: VarName::new("belowGround"),
            variability: rumoca_ir_core::Variability::Discrete(rumoca_ir_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("z"),
        flat::Variable {
            name: VarName::new("z"),
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(rumoca_ir_core::Token::default()),
            lhs: Box::new(make_var_ref("belowGround")),
            rhs: Box::new(make_lt_expr("x", 0)),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "below_ground_alias".to_string(),
        },
        scalar_count: 1,
    });

    let guard = if use_alias_guard {
        make_var_ref("belowGround")
    } else {
        make_lt_expr("x", 0)
    };
    let mut when_clause = flat::WhenClause::new(guard, Span::DUMMY);
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("z"),
        Expression::Literal(Literal::Integer(1)),
        Span::DUMMY,
        "when assignment",
    ));
    flat.when_clauses.push(when_clause);
    flat
}

fn extract_guard_expr_for_lhs<'a>(dae: &'a Dae, lhs: &str) -> &'a dae::Expression {
    let guarded = dae
        .f_z
        .iter()
        .chain(dae.f_m.iter())
        .find(|eq| eq.lhs.as_ref().is_some_and(|name| name.as_str() == lhs))
        .expect("expected guarded equation target");

    let dae::Expression::If { branches, .. } = &guarded.rhs else {
        panic!("guarded equation should lower to if-expression");
    };
    branches
        .first()
        .map(|(condition, _)| condition)
        .expect("guarded equation should contain one condition branch")
}

#[test]
fn test_when_boolean_alias_guard_matches_inline_relational_guard() {
    let direct_flat = build_when_condition_alias_model(false);
    let alias_flat = build_when_condition_alias_model(true);

    let direct_dae = to_dae_with_options(
        &direct_flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("direct-guard model should lower");
    let alias_dae = to_dae_with_options(
        &alias_flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("alias-guard model should lower");

    let direct_guard = extract_guard_expr_for_lhs(&direct_dae, "z");
    let alias_guard = extract_guard_expr_for_lhs(&alias_dae, "z");
    let expected_guard = make_lt_expr("x", 0);

    let expected_edge_guard = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args: vec![flat_to_dae_expression(&expected_guard)],
    };
    assert_eq!(
        format!("{direct_guard:?}"),
        format!("{expected_edge_guard:?}"),
        "MLS §8.3.5.1: direct relational when-guards should fire on false->true edges"
    );
    assert_eq!(
        format!("{alias_guard:?}"),
        format!("{expected_edge_guard:?}"),
        "MLS §8.3.5.1: boolean alias guards should lower to the same edge-wrapped relation"
    );

    let edge_alias_condition = format!(
        "{:?}",
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Edge,
            args: vec![flat_to_dae_expression(&make_var_ref("belowGround"))]
        }
    );
    let relation_set = alias_dae
        .relation
        .iter()
        .map(|expr| format!("{expr:?}"))
        .collect::<std::collections::HashSet<_>>();
    assert!(
        relation_set.contains(&format!("{expected_guard:?}")),
        "alias model should expose the relational guard to canonical condition roots"
    );
    assert!(
        !relation_set.contains(&edge_alias_condition),
        "alias model should not lower when-guard to edge(belowGround)"
    );
}

mod regression_more_tests;
