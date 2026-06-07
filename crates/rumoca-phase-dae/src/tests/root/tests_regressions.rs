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
        .variables
        .algebraics
        .get(&rumoca_core::VarName::new("arr[1]"))
        .or_else(|| {
            dae.variables
                .discrete_reals
                .get(&rumoca_core::VarName::new("arr[1]"))
        })
        .or_else(|| {
            dae.variables
                .discrete_valued
                .get(&rumoca_core::VarName::new("arr[1]"))
        })
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
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: false,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("u"),
        flat::Variable {
            name: VarName::new("u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("leafOut")),
            rhs: Box::new(make_var_ref("u")),
            span: rumoca_core::Span::DUMMY,
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
        dae.variables
            .outputs
            .contains_key(&rumoca_core::VarName::new("leafOut")),
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
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("matrix[1]")),
            rhs: Box::new(Expression::Array {
                elements: vec![
                    Expression::Literal {
                        value: Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    },
                    Expression::Literal {
                        value: Literal::Integer(2),
                        span: rumoca_core::Span::DUMMY,
                    },
                    Expression::Literal {
                        value: Literal::Integer(3),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "slice".to_string(),
        },
        scalar_count: 3,
    });

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("matrix"),
        Variable::new(rumoca_core::VarName::new("matrix")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(dae.continuous.equations[0].scalar_count, 3);
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
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("d")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("previous").into(),
                args: vec![make_var_ref("d")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
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
        dae.variables
            .discrete_reals
            .contains_key(&flat_to_dae_var_name(&name)),
        "clocked assignment target must be discrete"
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&flat_to_dae_var_name(&name)),
        "clocked assignment target must not remain algebraic"
    );
    assert!(
        dae.continuous.equations.is_empty(),
        "clocked assignment must leave continuous set"
    );
    assert_eq!(
        dae.discrete.real_updates.len(),
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
            Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            make_var_ref("d"),
        )],
        else_branch: Box::new(make_var_ref("d")),
        span: rumoca_core::Span::DUMMY,
    };
    let rhs_if = Expression::If {
        branches: vec![(
            Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            Expression::FunctionCall {
                name: VarName::new("superSample").into(),
                args: vec![make_var_ref("u")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        )],
        else_branch: Box::new(Expression::FunctionCall {
            name: VarName::new("superSample").into(),
            args: vec![
                make_var_ref("u"),
                Expression::Literal {
                    value: Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs_if),
            rhs: Box::new(rhs_if),
            span: rumoca_core::Span::DUMMY,
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
        dae.variables
            .discrete_reals
            .contains_key(&rumoca_core::VarName::new("d")),
        "clocked if-assignment target must be discrete"
    );
    assert!(
        dae.continuous.equations.is_empty(),
        "clocked if-assignment with superSample must not remain in f_x"
    );
    assert_eq!(
        dae.discrete.real_updates.len(),
        1,
        "clocked if-assignment must route to f_z"
    );
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
                name: VarName::new("superSample").into(),
                args: vec![make_var_ref("u")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
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
        dae.variables
            .discrete_reals
            .contains_key(&rumoca_core::VarName::new("usedFactor")),
        "clocked binding variable should be classified as discrete real"
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| !eq.origin.contains("binding equation for usedFactor")),
        "clocked binding must not be emitted as continuous residual in f_x"
    );
    assert!(
        dae.discrete
            .real_updates
            .iter()
            .any(|eq| eq.lhs.as_ref() == Some(&rumoca_core::VarName::new("usedFactor"))),
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
                name: VarName::new("superSample").into(),
                args: vec![
                    make_var_ref("u"),
                    Expression::Literal {
                        value: Literal::Integer(2),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
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
        dae.variables
            .discrete_valued
            .contains_key(&rumoca_core::VarName::new("ticks")),
        "discrete-valued variable must be classified into m partition"
    );
    assert!(
        dae.discrete
            .valued_updates
            .iter()
            .any(|eq| eq.lhs.as_ref() == Some(&rumoca_core::VarName::new("ticks"))),
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
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Tuple {
                elements: vec![make_var_ref("noise"), make_var_ref("seedState")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("hold").into(),
                args: vec![Expression::FunctionCall {
                    name: VarName::new("previous").into(),
                    args: vec![make_var_ref("seedState")],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
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

    assert!(
        dae.variables
            .discrete_reals
            .contains_key(&rumoca_core::VarName::new("noise"))
    );
    assert!(
        dae.variables
            .discrete_valued
            .contains_key(&rumoca_core::VarName::new("seedState"))
    );
    assert!(
        dae.continuous.equations.is_empty(),
        "clocked tuple assignment should not remain in f_x"
    );
    assert_eq!(
        dae.discrete.real_updates.len(),
        1,
        "clocked tuple assignment should be routed to f_z"
    );
    assert_eq!(dae.discrete.real_updates[0].scalar_count, 4);
}

#[test]
fn test_todae_routes_algorithm_when_sample_assignment_to_f_z() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("samplePeriod"),
        flat::Variable {
            name: VarName::new("samplePeriod"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Real(0.1),
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("r"),
        flat::Variable {
            name: VarName::new("r"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );

    let mut algorithm = flat::Algorithm::new(Vec::new(), Span::DUMMY, "algorithm");
    algorithm.outputs.push(VarName::new("r").into());
    algorithm.statements.push(rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: Expression::FunctionCall {
                name: VarName::new("sample").into(),
                args: vec![
                    Expression::Literal {
                        value: Literal::Integer(0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    make_var_ref("samplePeriod"),
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("r"),
                value: Expression::Literal {
                    value: Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],
        }],
        span: rumoca_core::Span::DUMMY,
    });
    flat.algorithms.push(algorithm);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("algorithm when sample assignment should lower to discrete partition");

    assert!(
        dae.variables
            .discrete_reals
            .contains_key(&rumoca_core::VarName::new("r")),
        "algorithm when-assigned Real output must be discrete"
    );
    assert!(
        dae.continuous.equations.is_empty(),
        "algorithm when-assigned Real output must not remain in f_x"
    );
    assert!(
        dae.discrete
            .real_updates
            .iter()
            .any(|eq| eq.lhs.as_ref() == Some(&rumoca_core::VarName::new("r"))),
        "algorithm when-assigned Real output must be routed to f_z"
    );
    assert_eq!(
        dae.clocks.schedules.len(),
        1,
        "algorithm when sample assignment must produce a periodic runtime schedule"
    );
    assert!((dae.clocks.schedules[0].period_seconds - 0.1).abs() <= 1e-12);
    assert!(dae.clocks.schedules[0].phase_seconds.abs() <= 1e-12);
}

fn sequential_when_same_target_model() -> Model {
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
    algorithm.outputs.push(VarName::new("y").into());
    algorithm.statements.push(rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: make_var_ref("c1"),
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: Expression::Literal {
                    value: Literal::Boolean(false),
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],
        }],
        span: rumoca_core::Span::DUMMY,
    });
    algorithm.statements.push(rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: make_var_ref("c2"),
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],
        }],
        span: rumoca_core::Span::DUMMY,
    });
    flat.algorithms.push(algorithm);
    flat
}

#[test]
fn test_todae_merges_sequential_when_statements_for_same_target_in_source_order() {
    let flat = sequential_when_same_target_model();
    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("sequential when-statements should lower to one ordered discrete equation");

    let eq = dae
        .discrete
        .valued_updates
        .iter()
        .find(|eq| eq.lhs.as_ref() == Some(&rumoca_core::VarName::new("y")))
        .expect("expected lowered discrete equation for y");
    let rumoca_core::Expression::If {
        branches,
        else_branch,
        ..
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
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY
            }
        ),
        "later when-statement must win when both guards are true"
    );
    assert!(
        matches!(
            &branches[1].1,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY
            }
        ),
        "earlier when-statement must be preserved as lower-priority branch"
    );
    let rumoca_core::Expression::If {
        branches: initial_branches,
        else_branch: inactive_else,
        span: rumoca_core::Span::DUMMY,
    } = else_branch.as_ref()
    else {
        panic!("algorithm when lowering must preserve the initial-section value before pre(y)");
    };
    assert_eq!(initial_branches.len(), 1);
    assert!(matches!(
        &initial_branches[0].0,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            ..
        }
    ));
    assert!(matches!(
        &initial_branches[0].1,
        rumoca_core::Expression::VarRef { .. }
    ));
    // SPEC_0007 Stage 3 Contract: pre() is eliminated in every partition by
    // phase-dae::pre_lowering. The inactive-else branch carries a reference
    // to the __pre__.* parameter slot, not a BuiltinCall { Pre }.
    let rumoca_core::Expression::VarRef { name, .. } = inactive_else.as_ref() else {
        panic!("inactive-else branch must be a __pre__.* parameter reference");
    };
    assert!(
        name.as_str().starts_with("__pre__."),
        "expected __pre__.* reference, got {}",
        name.as_str(),
    );
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
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(4),
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..Default::default()
        },
    );
}

fn previous_call(name: &str) -> Expression {
    Expression::FunctionCall {
        name: VarName::new("previous").into(),
        args: vec![make_var_ref(name)],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn sub_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn ge_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
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
                    Expression::Literal {
                        value: Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            ),
        )],
        else_branch: Box::new(sub_expr(
            make_var_ref("startOutput"),
            ge_expr(
                previous_call("counter"),
                sub_expr(
                    make_var_ref("startTick"),
                    Expression::Literal {
                        value: Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            ),
        )),
        span: rumoca_core::Span::DUMMY,
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
                Expression::Literal {
                    value: Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                tick_based_if_residual(),
            ),
        ),
        (
            "if(...) - 0",
            sub_expr(
                tick_based_if_residual(),
                Expression::Literal {
                    value: Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
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
            dae.variables
                .discrete_valued
                .contains_key(&rumoca_core::VarName::new("counter"))
        );
        assert!(
            dae.variables
                .discrete_valued
                .contains_key(&rumoca_core::VarName::new("startOutput"))
        );
        assert_eq!(
            dae.discrete.valued_updates.len(),
            2,
            "orientation {label}: residual should canonicalize to explicit assignments for both discrete targets"
        );
        assert!(
            dae.continuous.equations.is_empty(),
            "orientation {label}: if-residual assignment must not remain in continuous partition"
        );
    }
}

#[test]
fn test_todae_merges_branch_split_discrete_if_assignments_to_f_m() {
    let mut flat = Model::new();
    add_tick_based_discrete_vars(&mut flat);
    add_tick_based_equation(&mut flat, tick_based_if_residual());
    add_tick_based_equation(
        &mut flat,
        Expression::If {
            branches: vec![(
                previous_call("startOutput"),
                sub_expr(make_var_ref("startOutput"), previous_call("startOutput")),
            )],
            else_branch: Box::new(sub_expr(
                make_var_ref("counter"),
                add_expr(
                    previous_call("counter"),
                    Expression::Literal {
                        value: Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    },
                ),
            )),
            span: rumoca_core::Span::DUMMY,
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("branch-split if-equation assignments should merge into solved f_m updates");

    let counter_updates = dae
        .discrete
        .valued_updates
        .iter()
        .filter(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "counter"))
        .count();
    let start_output_updates = dae
        .discrete
        .valued_updates
        .iter()
        .filter(|eq| {
            eq.lhs
                .as_ref()
                .is_some_and(|lhs| lhs.as_str() == "startOutput")
        })
        .count();
    assert_eq!(counter_updates, 1);
    assert_eq!(start_output_updates, 1);
}

#[test]
// SPEC_0021: Exception - single regression fixture for time-guarded discrete output aliasing.
#[allow(clippy::too_many_lines)]
fn test_todae_keeps_time_guarded_discrete_output_binding_and_alias_consumer() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("Enable.stepTime"),
        flat::Variable {
            name: VarName::new("Enable.stepTime"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            binding: Some(Expression::Literal {
                value: Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Enable.y"),
        flat::Variable {
            name: VarName::new("Enable.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Counter.enable"),
        flat::Variable {
            name: VarName::new("Counter.enable"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
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
                    Expression::Literal {
                        value: Literal::Integer(4),
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(Expression::Literal {
                    value: Literal::Integer(3),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
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
        dae.variables
            .discrete_valued
            .contains_key(&rumoca_core::VarName::new("Enable.y"))
            || dae
                .variables
                .discrete_reals
                .contains_key(&rumoca_core::VarName::new("Enable.y")),
        "discrete output binding target must stay in the discrete variable partition",
    );
    assert!(
        dae.variables
            .discrete_valued
            .contains_key(&rumoca_core::VarName::new("Counter.enable"))
            || dae
                .variables
                .discrete_reals
                .contains_key(&rumoca_core::VarName::new("Counter.enable")),
        "discrete alias consumer must stay in the discrete variable partition",
    );
    assert!(
        dae.discrete.valued_updates.iter().any(|eq| eq
            .lhs
            .as_ref()
            .is_some_and(|lhs| lhs.as_str() == "Enable.y")),
        "time-guarded discrete output binding must be routed to f_m",
    );
    assert!(
        dae.discrete.valued_updates.iter().any(|eq| {
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
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            binding: Some(Expression::Literal {
                value: Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Enable.y"),
        flat::Variable {
            name: VarName::new("Enable.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            binding: Some(Expression::If {
                branches: vec![(
                    ge_expr(make_var_ref("time"), make_var_ref("Enable.stepTime")),
                    Expression::Literal {
                        value: Literal::Integer(4),
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(Expression::Literal {
                    value: Literal::Integer(3),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("Counter.enable"),
        flat::Variable {
            name: VarName::new("Counter.enable"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
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
        dae.discrete.valued_updates.iter().any(|eq| eq
            .lhs
            .as_ref()
            .is_some_and(|lhs| lhs.as_str() == "Enable.y")),
        "non-primitive leaf discrete bindings must contribute an explicit f_m producer",
    );
    assert!(
        dae.discrete.valued_updates.iter().any(|eq| {
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
        ("controlBus.axis1", rumoca_core::Causality::Empty, true),
        ("path.controlBus.axis1", rumoca_core::Causality::Empty, true),
        ("axis.controlBus.axis1", rumoca_core::Causality::Empty, true),
        ("controlBus.axis2", rumoca_core::Causality::Empty, true),
        ("path.controlBus.axis2", rumoca_core::Causality::Empty, true),
        ("controlBus.axis3", rumoca_core::Causality::Empty, true),
        ("path.controlBus.axis3", rumoca_core::Causality::Empty, true),
        ("axis.controlBus.axis3", rumoca_core::Causality::Empty, true),
        (
            "sink.u",
            rumoca_core::Causality::Input(rumoca_core::Token::default()),
            false,
        ),
        (
            "internal.source",
            rumoca_core::Causality::Output(rumoca_core::Token::default()),
            false,
        ),
    ] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                variability: rumoca_core::Variability::Empty,
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
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
    );

    // axis2 is an unanchored pass-through connection set.
    add_connection_equation(&mut flat, "path.controlBus.axis2", "controlBus.axis2");

    // axis3 propagates through connector aliases into an internal input sink.
    // It should still behave as an external input (no internal defining equation).
    add_connection_equation(&mut flat, "path.controlBus.axis3", "controlBus.axis3");
    add_connection_equation(&mut flat, "controlBus.axis3", "axis.controlBus.axis3");
    add_connection_equation(&mut flat, "axis.controlBus.axis3", "sink.u");

    let state_vars: indexmap::IndexSet<VarName> = indexmap::IndexSet::new();
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
        crate::balance::balance(&dae),
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
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(make_var_ref(&format!("interp[{idx}]"))),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(idx.into()),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
            origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
                component: "linearized".to_string(),
            },
            scalar_count: 4,
        });
    }

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("interp"),
        Variable::new(rumoca_core::VarName::new("interp")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts).unwrap();

    assert_eq!(dae.continuous.equations.len(), 4);
    assert!(
        dae.continuous
            .equations
            .iter()
            .all(|eq| eq.scalar_count == 1)
    );
    assert_eq!(
        dae.continuous
            .equations
            .iter()
            .map(|eq| eq.scalar_count)
            .sum::<usize>(),
        4
    );
}

#[test]
fn test_connected_discrete_input_alias_keeps_discrete_partition() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("inner.flag"),
        flat::Variable {
            name: VarName::new("inner.flag"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("inner.flagAlias"),
        flat::Variable {
            name: VarName::new("inner.flagAlias"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
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
        let var = rumoca_core::VarName::new(name);
        assert!(
            dae.variables.discrete_valued.contains_key(&var),
            "discrete connected input {name} should be classified to m"
        );
        assert!(
            !dae.variables.algebraics.contains_key(&var),
            "discrete connected input {name} must not be promoted to continuous algebraics"
        );
    }

    assert_eq!(
        dae.discrete.valued_updates.len(),
        1,
        "connected discrete input alias must contribute one discrete-valued equation"
    );
    assert_eq!(
        crate::balance::balance(&dae),
        0,
        "discrete connected input aliases must not affect continuous balance"
    );
}

#[test]
fn test_discrete_input_connected_to_local_output_counts_as_local_unknown() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("source.y"),
        flat::Variable {
            name: VarName::new("source.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("sink.u"),
        flat::Variable {
            name: VarName::new("sink.u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    let mut algorithm = flat::Algorithm::new(Vec::new(), Span::DUMMY, "algorithm");
    algorithm.outputs.push(VarName::new("source.y").into());
    algorithm
        .statements
        .push(rumoca_core::Statement::Assignment {
            comp: make_comp_ref("source.y"),
            value: Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
        });
    flat.algorithms.push(algorithm);
    add_connection_equation(&mut flat, "sink.u", "source.y");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for locally driven discrete input");

    assert!(
        dae.variables
            .discrete_valued
            .contains_key(&rumoca_core::VarName::new("sink.u")),
        "locally driven discrete input should remain in m"
    );
    assert_eq!(
        crate::balance::balance(&dae),
        0,
        "a local output-to-input discrete connection should count both the input unknown and its connection row"
    );
}

#[test]
fn test_discrete_input_alias_chain_to_local_output_counts_as_local_unknown() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("source.y"),
        flat::Variable {
            name: VarName::new("source.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    for name in ["relay.u", "sink.u"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
                variability: rumoca_core::Variability::Empty,
                is_discrete_type: true,
                is_primitive: true,
                ..Default::default()
            },
        );
    }

    let mut algorithm = flat::Algorithm::new(Vec::new(), Span::DUMMY, "algorithm");
    algorithm.outputs.push(VarName::new("source.y").into());
    algorithm
        .statements
        .push(rumoca_core::Statement::Assignment {
            comp: make_comp_ref("source.y"),
            value: Expression::Literal {
                value: Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
        });
    flat.algorithms.push(algorithm);
    add_connection_equation(&mut flat, "relay.u", "source.y");
    add_connection_equation(&mut flat, "sink.u", "relay.u");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for locally driven discrete input alias chain");

    assert_eq!(
        dae.metadata.discrete_input_names,
        Vec::<String>::new(),
        "a transitive local-output alias set must not be marked input-only"
    );
    assert_eq!(
        crate::balance::balance(&dae),
        0,
        "all discrete inputs in a local-output alias chain should count as local unknowns"
    );
}

#[test]
fn test_connected_real_input_propagates_discrete_partition_from_peer() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("inner.clocked"),
        flat::Variable {
            name: VarName::new("inner.clocked"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("inner.u"),
        flat::Variable {
            name: VarName::new("inner.u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
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
        dae.variables
            .discrete_reals
            .contains_key(&rumoca_core::VarName::new("inner.u")),
        "connected real input should become discrete when tied to a discrete peer"
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&rumoca_core::VarName::new("inner.u")),
        "connected real input should not remain continuous algebraic"
    );
    assert_eq!(
        crate::balance::balance(&dae),
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
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("x"),
        flat::Variable {
            name: VarName::new("x"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
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
        .discrete
        .real_updates
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|name| name.as_str() == "x"))
        .expect("expected guarded when equation for x in f_z");

    let rumoca_core::Expression::If { branches, .. } = &guarded.rhs else {
        panic!("guarded when equation should lower to if-expression");
    };
    assert_eq!(branches.len(), 1);
    let cond = &branches[0].0;
    let mut edge_names = Vec::new();
    collect_edge_guard_names(cond, &mut edge_names);
    assert_eq!(edge_names, vec!["flag".to_string()]);
}

#[test]
fn test_when_clause_guard_for_clock_condition_uses_clock_tick_directly() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("y2"),
        flat::Variable {
            name: VarName::new("y2"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );

    let clock_call = Expression::FunctionCall {
        name: VarName::new("Clock").into(),
        args: vec![],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let previous_y2 = Expression::FunctionCall {
        name: VarName::new("previous").into(),
        args: vec![make_var_ref("y2")],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };
    let mut when_clause = flat::WhenClause::new(clock_call, Span::DUMMY);
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("y2"),
        Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(previous_y2),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "when Clock assignment",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("when Clock clause should lower to guarded discrete update");

    let guarded = dae
        .discrete
        .valued_updates
        .iter()
        .find(|eq| eq.lhs.as_ref().is_some_and(|name| name.as_str() == "y2"))
        .expect("expected guarded when equation for y2 in f_m");

    let Expression::If { branches, .. } = &guarded.rhs else {
        panic!("guarded when equation should lower to if-expression");
    };
    assert_eq!(branches.len(), 1);
    assert!(
        matches!(&branches[0].0, Expression::FunctionCall { name, .. } if name.last_segment() == "Clock"),
        "when Clock() guard must use the clock tick directly, got {:?}",
        branches[0].0
    );
}

#[test]
fn test_when_clause_guard_for_vector_var_conditions_uses_edge_activation_per_element() {
    let mut flat = Model::new();
    for name in ["trigger", "reset"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
                is_discrete_type: true,
                is_primitive: true,
                start: Some(Expression::Literal {
                    value: Literal::Boolean(false),
                    span: rumoca_core::Span::DUMMY,
                }),
                ..Default::default()
            },
        );
    }
    flat.add_variable(
        VarName::new("y"),
        flat::Variable {
            name: VarName::new("y"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Integer(0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );

    let mut when_clause = flat::WhenClause::new(
        Expression::Array {
            elements: vec![make_var_ref("trigger"), make_var_ref("reset")],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
    );
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("y"),
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
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
        .discrete
        .valued_updates
        .iter()
        .chain(dae.discrete.real_updates.iter())
        .find(|eq| eq.lhs.as_ref().is_some_and(|name| name.as_str() == "y"))
        .expect("expected guarded when equation for y in discrete partitions");

    let rumoca_core::Expression::If { branches, .. } = &guarded.rhs else {
        panic!("guarded when equation should lower to if-expression");
    };
    assert_eq!(branches.len(), 1);
    let mut edge_names = Vec::new();
    collect_edge_guard_names(&branches[0].0, &mut edge_names);
    edge_names.sort();
    assert_eq!(edge_names, vec!["reset".to_string(), "trigger".to_string()]);
    assert!(
        !matches!(branches[0].0, rumoca_core::Expression::Array { .. }),
        "MLS §8.3.5: a vector when-condition must lower to a scalar activation guard"
    );
}

fn collect_edge_guard_names(expr: &rumoca_core::Expression, names: &mut Vec<String>) {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            collect_edge_guard_names(lhs, names);
            collect_edge_guard_names(rhs, names);
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            assert_eq!(*function, rumoca_core::BuiltinFunction::Edge);
            assert_eq!(args.len(), 1);
            if let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = &args[0]
            {
                assert!(subscripts.is_empty());
                names.push(name.as_str().to_string());
            }
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs,
            rhs,
            ..
        } => {
            if let (
                rumoca_core::Expression::VarRef {
                    name, subscripts, ..
                },
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Not,
                    rhs,
                    ..
                },
            ) = (&**lhs, &**rhs)
                && let rumoca_core::Expression::VarRef {
                    name: pre_name,
                    subscripts: pre_subscripts,
                    ..
                } = &**rhs
                && subscripts.is_empty()
                && pre_subscripts.is_empty()
                && pre_name.as_str() == format!("__pre__.{}", name.as_str())
            {
                names.push(name.as_str().to_string());
            }
        }
        _ => {}
    }
}

fn make_lt_expr(lhs: &str, rhs: i64) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(make_var_ref(lhs)),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(rhs),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
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
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
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
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("belowGround")),
            rhs: Box::new(make_lt_expr("x", 0)),
            span: rumoca_core::Span::DUMMY,
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
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "when assignment",
    ));
    flat.when_clauses.push(when_clause);
    flat
}

fn extract_guard_expr_for_lhs<'a>(dae: &'a Dae, lhs: &str) -> &'a rumoca_core::Expression {
    let guarded = dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
        .find(|eq| eq.lhs.as_ref().is_some_and(|name| name.as_str() == lhs))
        .expect("expected guarded equation target");

    let rumoca_core::Expression::If { branches, .. } = &guarded.rhs else {
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

    let expected_edge_guard = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::And,
        lhs: Box::new(rumoca_core::Expression::VarRef {
            name: VarName::new("c").into(),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("__pre__.c").into(),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
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
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge,
            args: vec![flat_to_dae_expression(&make_var_ref("belowGround"))],
            span: rumoca_core::Span::DUMMY,
        }
    );
    let relation_set = alias_dae
        .conditions
        .relations
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
