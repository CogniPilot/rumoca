use super::*;

mod clocked_tuple_tests;
mod regression_more_tests;
mod when_inactive_tests;
mod when_lowering_tests;

#[test]
fn test_todae_inherits_scalarized_element_start_from_array_base() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("arr"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("arr"),
            dims: vec![2],
            start: Some(make_var_ref(
                "Modelica.Electrical.Digital.Interfaces.Logic.'U'",
            )),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("arr[1]"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("arr[1]"),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
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
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("leafOut"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: false,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("leafOut")),
            rhs: Box::new(make_var_ref("u")),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
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
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("matrix"),
            dims: vec![2, 3],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("matrix[1]")),
            rhs: Box::new(Expression::Array {
                elements: vec![
                    Expression::Literal {
                        value: Literal::Integer(1),
                        span: crate::test_support::test_span(),
                    },
                    Expression::Literal {
                        value: Literal::Integer(2),
                        span: crate::test_support::test_span(),
                    },
                    Expression::Literal {
                        value: Literal::Integer(3),
                        span: crate::test_support::test_span(),
                    },
                ],
                is_matrix: false,
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "slice".to_string(),
        },
        scalar_count: 3,
    });

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("matrix"),
        Variable::new(
            rumoca_core::VarName::new("matrix"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
        crate::test_support::with_component_ref(flat::Variable {
            name: name.clone(),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("d")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("previous").into(),
                args: vec![make_var_ref("d")],
                is_constructor: false,
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
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
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let lhs_if = Expression::If {
        branches: vec![(
            Expression::Literal {
                value: Literal::Boolean(true),
                span: crate::test_support::test_span(),
            },
            make_var_ref("d"),
        )],
        else_branch: Box::new(make_var_ref("d")),
        span: crate::test_support::test_span(),
    };
    let rhs_if = Expression::If {
        branches: vec![(
            Expression::Literal {
                value: Literal::Boolean(true),
                span: crate::test_support::test_span(),
            },
            Expression::FunctionCall {
                name: VarName::new("superSample").into(),
                args: vec![make_var_ref("u")],
                is_constructor: false,
                span: crate::test_support::test_span(),
            },
        )],
        else_branch: Box::new(Expression::FunctionCall {
            name: VarName::new("superSample").into(),
            args: vec![
                make_var_ref("u"),
                Expression::Literal {
                    value: Literal::Integer(2),
                    span: crate::test_support::test_span(),
                },
            ],
            is_constructor: false,
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs_if),
            rhs: Box::new(rhs_if),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
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
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("u"),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("usedFactor"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("usedFactor"),
            binding: Some(Expression::FunctionCall {
                name: VarName::new("superSample").into(),
                args: vec![make_var_ref("u")],
                is_constructor: false,
                span: crate::test_support::test_span(),
            }),
            // Reproduce flatten metadata regression where Integer/Boolean tagging
            // can be missing. ToDae must still keep clocked bindings out of f_x.
            is_discrete_type: false,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
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
        dae.discrete.real_updates.iter().any(|eq| eq
            .lhs
            .as_ref()
            .is_some_and(|lhs| lhs.as_str() == "usedFactor")),
        "clocked binding must be routed to discrete-real updates"
    );
}

#[test]
fn test_todae_routes_discrete_valued_clocked_binding_to_fm() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("u"),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("ticks"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("ticks"),
            binding: Some(Expression::FunctionCall {
                name: VarName::new("superSample").into(),
                args: vec![
                    make_var_ref("u"),
                    Expression::Literal {
                        value: Literal::Integer(2),
                        span: crate::test_support::test_span(),
                    },
                ],
                is_constructor: false,
                span: crate::test_support::test_span(),
            }),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
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
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "ticks")),
        "clocked discrete-valued binding must be emitted as explicit f_m assignment"
    );
}

#[test]
fn test_todae_routes_algorithm_when_sample_assignment_to_f_z() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("samplePeriod"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("samplePeriod"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Real(0.1),
                span: crate::test_support::test_span(),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("r"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("r"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let mut algorithm =
        flat::Algorithm::new(Vec::new(), crate::test_support::test_span(), "algorithm");
    algorithm.outputs.push(VarName::new("r").into());
    algorithm.statements.push(rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: Expression::FunctionCall {
                name: VarName::new("sample").into(),
                args: vec![
                    Expression::Literal {
                        value: Literal::Integer(0),
                        span: crate::test_support::test_span(),
                    },
                    make_var_ref("samplePeriod"),
                ],
                is_constructor: false,
                span: crate::test_support::test_span(),
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("r"),
                value: Expression::Literal {
                    value: Literal::Real(1.0),
                    span: crate::test_support::test_span(),
                },
                span: crate::test_support::test_span(),
            }],
        }],
        span: crate::test_support::test_span(),
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
            .any(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "r")),
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

fn add_tick_based_discrete_vars(flat: &mut Model) {
    for name in ["counter", "startOutput"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                is_discrete_type: true,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }
    flat.add_variable(
        VarName::new("startTick"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("startTick"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::Literal {
                value: Literal::Integer(4),
                span: crate::test_support::test_span(),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
}

fn previous_call(name: &str) -> Expression {
    Expression::FunctionCall {
        name: VarName::new("previous").into(),
        args: vec![make_var_ref(name)],
        is_constructor: false,
        span: crate::test_support::test_span(),
    }
}

fn sub_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    }
}

fn add_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
    }
}

fn ge_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Ge,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: crate::test_support::test_span(),
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
                        span: crate::test_support::test_span(),
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
                        span: crate::test_support::test_span(),
                    },
                ),
            ),
        )),
        span: crate::test_support::test_span(),
    }
}

fn add_tick_based_equation(flat: &mut Model, residual: Expression) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual,
        span: crate::test_support::test_span(),
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
                    span: crate::test_support::test_span(),
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
                    span: crate::test_support::test_span(),
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
                        span: crate::test_support::test_span(),
                    },
                ),
            )),
            span: crate::test_support::test_span(),
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
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Enable.stepTime"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            binding: Some(Expression::Literal {
                value: Literal::Real(1.0),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("Enable.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Enable.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("Counter.enable"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Counter.enable"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: sub_expr(
            make_var_ref("Enable.y"),
            Expression::If {
                branches: vec![(
                    ge_expr(make_var_ref("time"), make_var_ref("Enable.stepTime")),
                    Expression::Literal {
                        value: Literal::Integer(4),
                        span: crate::test_support::test_span(),
                    },
                )],
                else_branch: Box::new(Expression::Literal {
                    value: Literal::Integer(3),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
        ),
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "Enable".to_string(),
        },
        scalar_count: 1,
    });
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: sub_expr(make_var_ref("Counter.enable"), make_var_ref("Enable.y")),
        span: crate::test_support::test_span(),
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
fn test_todae_preserves_discrete_output_binding_as_relation_anchor() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("root.suspend"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("root.suspend"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_discrete_type: true,
            binding: Some(Expression::Literal {
                value: Literal::Boolean(false),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(crate::test_support::test_span())
        }),
    );
    flat.add_variable(
        VarName::new("root.subgraph.suspend"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("root.subgraph.suspend"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_discrete_type: true,
            ..rumoca_ir_flat::Variable::empty_with_span(crate::test_support::test_span())
        }),
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: sub_expr(
            make_var_ref("root.suspend"),
            make_var_ref("root.subgraph.suspend"),
        ),
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "root".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("todae should preserve discrete output binding relation anchors");

    assert!(
        dae.discrete.valued_updates.iter().any(|eq| {
            eq.origin.contains("binding equation for root.suspend")
                && eq
                    .lhs
                    .as_ref()
                    .is_some_and(|lhs| lhs.as_str() == "root.suspend")
        }),
        "constant discrete output binding must remain as the value anchor"
    );
    assert_eq!(
        crate::balance::balance(&dae).expect("fixture balance should be computable"),
        0,
        "binding anchor plus relation equation should balance both discrete variables"
    );
}

#[test]
fn test_todae_converts_non_primitive_leaf_discrete_binding_to_f_m() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("Enable.stepTime"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Enable.stepTime"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            binding: Some(Expression::Literal {
                value: Literal::Real(1.0),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("Enable.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Enable.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            binding: Some(Expression::If {
                branches: vec![(
                    ge_expr(make_var_ref("time"), make_var_ref("Enable.stepTime")),
                    Expression::Literal {
                        value: Literal::Integer(4),
                        span: crate::test_support::test_span(),
                    },
                )],
                else_branch: Box::new(Expression::Literal {
                    value: Literal::Integer(3),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("Counter.enable"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("Counter.enable"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: false,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: sub_expr(make_var_ref("Counter.enable"), make_var_ref("Enable.y")),
        span: crate::test_support::test_span(),
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
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                variability: rumoca_core::Variability::Empty,
                causality,
                is_primitive: true,
                connected: true,
                from_expandable_connector,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
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
            span: crate::test_support::test_span(),
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
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0,
        "component-anchored and unanchored connector sets should both balance"
    );
}

#[test]
fn test_classify_equations_linearized_embedded_subscript_is_scalarized() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("interp"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("interp"),
            dims: vec![1, 4],
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    for idx in 1..=4 {
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(make_var_ref(&format!("interp[{idx}]"))),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(idx.into()),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
            span: crate::test_support::test_span(),
            origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
                component: "linearized".to_string(),
            },
            scalar_count: 4,
        });
    }

    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("interp"),
        Variable::new(
            rumoca_core::VarName::new("interp"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
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
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("inner.flag"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("inner.flagAlias"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("inner.flagAlias"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
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
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0,
        "discrete connected input aliases must not affect continuous balance"
    );
}

#[test]
fn test_discrete_input_connected_to_local_output_counts_as_local_unknown() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("source.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("source.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("sink.u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("sink.u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let mut algorithm =
        flat::Algorithm::new(Vec::new(), crate::test_support::test_span(), "algorithm");
    algorithm.outputs.push(VarName::new("source.y").into());
    algorithm
        .statements
        .push(rumoca_core::Statement::Assignment {
            comp: make_comp_ref("source.y"),
            value: Expression::Literal {
                value: Literal::Boolean(true),
                span: crate::test_support::test_span(),
            },
            span: crate::test_support::test_span(),
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
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0,
        "a local output-to-input discrete connection should count both the input unknown and its connection row"
    );
}

#[test]
fn test_discrete_input_alias_chain_to_local_output_counts_as_local_unknown() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("source.y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("source.y"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    for name in ["relay.u", "sink.u"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
                variability: rumoca_core::Variability::Empty,
                is_discrete_type: true,
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let mut algorithm =
        flat::Algorithm::new(Vec::new(), crate::test_support::test_span(), "algorithm");
    algorithm.outputs.push(VarName::new("source.y").into());
    algorithm
        .statements
        .push(rumoca_core::Statement::Assignment {
            comp: make_comp_ref("source.y"),
            value: Expression::Literal {
                value: Literal::Boolean(true),
                span: crate::test_support::test_span(),
            },
            span: crate::test_support::test_span(),
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
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0,
        "all discrete inputs in a local-output alias chain should count as local unknowns"
    );
}

#[test]
fn test_connected_real_input_propagates_discrete_partition_from_peer() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("inner.clocked"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("inner.clocked"),
            causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("inner.u"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("inner.u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
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
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0,
        "discrete connection propagation should avoid continuous balance deficits"
    );
}

#[test]
fn test_when_clause_guard_for_var_condition_uses_edge_activation() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("flag"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("flag"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Boolean(false),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Real(0.0),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let mut when_clause =
        flat::WhenClause::new(make_var_ref("flag"), crate::test_support::test_span());
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("x"),
        make_var_ref("time"),
        crate::test_support::test_span(),
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
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("y2"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Boolean(false),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let clock_call = Expression::FunctionCall {
        name: VarName::new("Clock").into(),
        args: vec![],
        is_constructor: false,
        span: crate::test_support::test_span(),
    };
    let previous_y2 = Expression::FunctionCall {
        name: VarName::new("previous").into(),
        args: vec![make_var_ref("y2")],
        is_constructor: false,
        span: crate::test_support::test_span(),
    };
    let mut when_clause = flat::WhenClause::new(clock_call, crate::test_support::test_span());
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("y2"),
        Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(previous_y2),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
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
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
                is_discrete_type: true,
                is_primitive: true,
                start: Some(Expression::Literal {
                    value: Literal::Boolean(false),
                    span: crate::test_support::test_span(),
                }),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }
    flat.add_variable(
        VarName::new("y"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("y"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            start: Some(Expression::Literal {
                value: Literal::Integer(0),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let mut when_clause = flat::WhenClause::new(
        Expression::Array {
            elements: vec![make_var_ref("trigger"), make_var_ref("reset")],
            is_matrix: false,
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
    );
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("y"),
        Expression::Literal {
            value: Literal::Integer(1),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
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

/// A scalar parameter binding that references a known variable must keep
/// that reference; the record-field start alias selection used to graft the
/// LHS leaf onto the RHS and suffix-resolve to an unrelated variable
/// (`resistor.m = multiStar.mBasic` became the top-level `m`, so MSL
/// PowerConverters models evaluated `fill(300.15, m)` with the wrong phase
/// count).
#[test]
fn test_scalar_binding_to_known_variable_keeps_reference() {
    let mut flat = Model::new();
    for (name, value) in [("m", 3), ("multiStar.mBasic", 1)] {
        let var_name = VarName::new(name);
        flat.add_variable(
            var_name.clone(),
            crate::test_support::with_component_ref(flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    span: crate::test_support::test_span(),
                }),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }
    let target = VarName::new("multiStar.resistor.m");
    flat.add_variable(
        target.clone(),
        crate::test_support::with_component_ref(flat::Variable {
            name: target.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(Expression::VarRef {
                name: rumoca_core::Reference::from_component_reference(
                    rumoca_core::component_reference_from_flat_name(
                        &VarName::new("multiStar.mBasic"),
                        crate::test_support::test_span(),
                    )
                    .expect("fixture name must form a component reference"),
                ),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("scalar parameter bindings should convert");

    let var = dae
        .variables
        .parameters
        .get(&target)
        .expect("target parameter should exist in DAE");
    let start = var.start.as_ref().expect("binding becomes parameter start");
    let Expression::VarRef { name, .. } = start else {
        panic!("expected a variable reference start, got {start:?}");
    };
    assert_eq!(
        name.var_name().as_str(),
        "multiStar.mBasic",
        "binding reference must not be grafted onto an unrelated variable"
    );
}
