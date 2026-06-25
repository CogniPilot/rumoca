use super::*;

#[test]
fn test_should_skip_binding_for_explicit_var_keeps_record_prefix_unknown_binding() {
    let name = VarName::new("core.V_m.re");
    let var = flat::Variable {
        name: name.clone(),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("core.port_p.V_m")),
            rhs: Box::new(make_var_ref("core.port_n.V_m")),
            span: crate::test_support::test_span(),
        }),
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };

    let unknowns: HashSet<VarName> = [
        VarName::new("core.V_m.re"),
        VarName::new("core.port_p.V_m.re"),
        VarName::new("core.port_p.V_m.im"),
        VarName::new("core.port_n.V_m.re"),
        VarName::new("core.port_n.V_m.im"),
    ]
    .into_iter()
    .collect();
    let unknown_prefix_children = build_unknown_prefix_children(&unknowns);

    assert!(
        !should_skip_binding_for_explicit_var(&name, &var, &unknowns, &unknown_prefix_children),
        "record-prefix bindings that reference other unknown fields must be kept"
    );
}

#[test]
fn test_should_keep_connected_input_binding_for_connected_input_with_binding() {
    let name = VarName::new("u");
    let var = flat::Variable {
        name: name.clone(),
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Literal {
            value: Literal::Real(1.0),
            span: crate::test_support::test_span(),
        }),
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };
    let mut connected_input_only = HashSet::default();
    connected_input_only.insert(name.clone());

    assert!(should_keep_connected_input_binding(
        &VariableKind::Input,
        &name,
        &var,
        &connected_input_only
    ));
}

#[test]
fn test_should_keep_connected_input_binding_rejects_missing_binding() {
    let name = VarName::new("u");
    let var = flat::Variable {
        name: name.clone(),
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        is_primitive: true,
        binding: None,
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };
    let mut connected_input_only = HashSet::default();
    connected_input_only.insert(name.clone());

    assert!(!should_keep_connected_input_binding(
        &VariableKind::Input,
        &name,
        &var,
        &connected_input_only
    ));
}

#[test]
fn test_should_keep_connected_input_binding_rejects_non_input_kind() {
    let name = VarName::new("x");
    let var = flat::Variable {
        name: name.clone(),
        causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Literal {
            value: Literal::Real(1.0),
            span: crate::test_support::test_span(),
        }),
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };
    let mut connected_input_only = HashSet::default();
    connected_input_only.insert(name.clone());

    assert!(!should_keep_connected_input_binding(
        &VariableKind::Algebraic,
        &name,
        &var,
        &connected_input_only
    ));
}

#[test]
fn test_should_skip_binding_for_explicit_var_skips_constant_binding() {
    let name = VarName::new("x");
    let var = flat::Variable {
        name: name.clone(),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Literal {
            value: Literal::Integer(0),
            span: crate::test_support::test_span(),
        }),
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };
    let unknowns: HashSet<VarName> = [name.clone()].into_iter().collect();
    let unknown_prefix_children = build_unknown_prefix_children(&unknowns);

    assert!(
        should_skip_binding_for_explicit_var(&name, &var, &unknowns, &unknown_prefix_children),
        "constant bindings with no other unknown refs should be skipped when explicit equations exist"
    );
}

#[test]
fn test_collect_vars_with_unknown_rhs_resolves_collapsed_array_member_refs() {
    let mut flat = Model::new();
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("ht.Ts")),
            rhs: Box::new(make_var_ref("ht.heatPorts.T")),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "ht".to_string(),
        },
        scalar_count: 1,
    });

    let unknowns: HashSet<VarName> = [
        VarName::new("ht.Ts"),
        VarName::new("ht.heatPorts[1].T"),
        VarName::new("ht.heatPorts[1].Q_flow"),
    ]
    .into_iter()
    .collect();

    let defined = collect_vars_with_unknown_rhs(&flat, &unknowns);
    assert!(
        defined.contains(&VarName::new("ht.Ts")),
        "collapsed array-member RHS refs must mark LHS as unknown-related"
    );
}

#[test]
fn test_empty_model() {
    let flat = Model::new();
    let dae = to_dae(&flat).unwrap();
    assert!(crate::balance::is_balanced(&dae).expect("valid DAE balance fixture"));
    assert_eq!(
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0
    );
}

#[test]
fn test_internal_input_with_der_becomes_state() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("medium.p"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("medium.p"),
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
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("medium.p")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae(&flat).expect("internal input der-equation should compile");
    assert!(
        dae.variables
            .states
            .contains_key(&rumoca_core::VarName::new("medium.p")),
        "internal input with der() must become a state unknown"
    );
    assert!(
        !dae.variables
            .inputs
            .contains_key(&rumoca_core::VarName::new("medium.p")),
        "internal input with der() must not remain an external input"
    );
    assert_eq!(
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0
    );
}

#[test]
fn test_top_level_input_with_der_remains_input() {
    let mut flat = Model::new();
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
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("u")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("top-level input der-equation should compile");
    assert!(
        dae.variables
            .inputs
            .contains_key(&rumoca_core::VarName::new("u")),
        "external top-level input with der() must remain an input"
    );
    assert!(
        !dae.variables
            .states
            .contains_key(&rumoca_core::VarName::new("u")),
        "external top-level input with der() must not become a state"
    );
}

#[test]
fn test_component_ref_to_var_name() {
    let comp = make_comp_ref("myVar");
    let name = comp.to_var_name();
    assert_eq!(name.as_str(), "myVar");
}

#[test]
fn test_component_ref_to_var_name_qualified() {
    let comp = rumoca_core::ComponentReference {
        local: false,
        span: crate::test_support::test_span(),
        parts: vec![
            rumoca_core::ComponentRefPart {
                ident: "comp".to_string(),
                span: crate::test_support::test_span(),
                subs: vec![],
            },
            rumoca_core::ComponentRefPart {
                ident: "var".to_string(),
                span: crate::test_support::test_span(),
                subs: vec![],
            },
        ],
        def_id: None,
    };
    let name = comp.to_var_name();
    assert_eq!(name.as_str(), "comp.var");
}

#[test]
fn test_collect_when_statement_targets_simple() {
    // Test: when statements should collect their targets
    let stmts = vec![make_when_stmt(&["x", "y"])];
    let mut targets = HashSet::default();
    collect_when_statement_targets(&stmts, &mut targets);

    assert_eq!(targets.len(), 2);
    assert!(targets.contains(&VarName::new("x")));
    assert!(targets.contains(&VarName::new("y")));
}

#[test]
fn test_collect_when_statement_targets_nested_in_if() {
    // Test: when statements inside if should be found
    let when_stmt = make_when_stmt(&["discrete_var"]);
    let if_stmt = rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Empty {
                span: crate::test_support::test_span(),
            },
            stmts: vec![when_stmt],
        }],
        else_block: None,

        span: crate::test_support::test_span(),
    };

    let mut targets = HashSet::default();
    collect_when_statement_targets(&[if_stmt], &mut targets);

    assert_eq!(targets.len(), 1);
    assert!(targets.contains(&VarName::new("discrete_var")));
}

#[test]
fn test_collect_when_statement_targets_ignores_non_when() {
    // Test: regular assignments outside when should not be collected
    let stmts = vec![make_assignment("continuous_var")];
    let mut targets = HashSet::default();
    collect_when_statement_targets(&stmts, &mut targets);

    assert!(targets.is_empty());
}

#[test]
fn test_is_input_input_connection_true() {
    // Test: connection between two inputs should return true

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("a"),
        Variable::new(
            rumoca_core::VarName::new("a"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("b"),
        Variable::new(
            rumoca_core::VarName::new("b"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("a").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "a".to_string(),
            rhs: "b".to_string(),
        },
        scalar_count: 1,
    };

    assert!(is_input_input_connection(&eq, &dae));
}

#[test]
fn test_is_input_input_connection_false_one_algebraic() {
    // Test: connection between input and algebraic should return false

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("a"),
        Variable::new(
            rumoca_core::VarName::new("a"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("b"),
        Variable::new(
            rumoca_core::VarName::new("b"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("a").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "a".to_string(),
            rhs: "b".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_input_connection(&eq, &dae));
}

#[test]
fn test_is_input_input_connection_false_not_connection() {
    // Test: non-connection equations should return false

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("a"),
        Variable::new(
            rumoca_core::VarName::new("a"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("b"),
        Variable::new(
            rumoca_core::VarName::new("b"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("a").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("b").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_input_connection(&eq, &dae));
}

#[test]
fn test_is_input_default_equation_true_for_parameter_rhs() {
    let mut flat = Model::new();
    flat.top_level_input_components.insert("x_in".to_string());

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("x_in"),
        Variable::new(
            rumoca_core::VarName::new("x_in"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("x_param"),
        Variable::new(
            rumoca_core::VarName::new("x_param"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_in").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_param").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_is_input_default_equation_false_for_unknown_rhs() {
    let mut flat = Model::new();
    flat.top_level_input_components.insert("x_in".to_string());

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("x_in"),
        Variable::new(
            rumoca_core::VarName::new("x_in"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("x_unknown"),
        Variable::new(
            rumoca_core::VarName::new("x_unknown"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_in").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_unknown").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_is_input_default_equation_false_for_rhs_input_alias() {
    let mut flat = Model::new();
    flat.top_level_input_components.insert("x_in".to_string());
    flat.top_level_input_components.insert("y_in".to_string());

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("x_in"),
        Variable::new(
            rumoca_core::VarName::new("x_in"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("y_in"),
        Variable::new(
            rumoca_core::VarName::new("y_in"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_in").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("y_in").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_is_input_default_equation_false_for_internal_input_default() {
    let flat = Model::new();

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("transition1.condition"),
        Variable::new(
            rumoca_core::VarName::new("transition1.condition"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("alwaysTrue"),
        Variable::new(
            rumoca_core::VarName::new("alwaysTrue"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("transition1.condition").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("alwaysTrue").into(),
                subscripts: vec![],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "transition1".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_connected_input_binding_kept_for_input_only_connection_alias() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("inner.p"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("inner.p"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            binding: Some(rumoca_core::Expression::Literal {
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
        VarName::new("inner.q"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("inner.q"),
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

    add_connection_equation(&mut flat, "inner.q", "inner.p");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for connected internal input alias");

    assert!(
        dae.variables
            .algebraics
            .contains_key(&rumoca_core::VarName::new("inner.p"))
            && dae
                .variables
                .algebraics
                .contains_key(&rumoca_core::VarName::new("inner.q")),
        "connected internal inputs should be promoted to algebraics"
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin.contains("binding equation for inner.p")),
        "binding equation for connected input should be kept for input-only alias set"
    );
    assert_eq!(
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0,
        "input-only connection aliases with a binding must stay balanced"
    );
}

#[test]
fn test_connected_input_alias_keeps_single_binding_anchor() {
    let mut flat = Model::new();
    for (name, value) in [("inner.p", 1.0), ("inner.q", 2.0)] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
                variability: rumoca_core::Variability::Empty,
                is_primitive: true,
                binding: Some(rumoca_core::Expression::Literal {
                    value: Literal::Real(value),
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

    add_connection_equation(&mut flat, "inner.q", "inner.p");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for connected internal input aliases");

    let binding_equations = dae
        .continuous
        .equations
        .iter()
        .filter(|eq| eq.origin.starts_with("binding equation for inner."))
        .count();
    assert_eq!(
        binding_equations, 1,
        "one input-only alias component should keep exactly one binding anchor"
    );
    assert_eq!(
        crate::balance::balance(&dae).expect("valid DAE balance fixture"),
        0,
        "multiple default bindings in one input-only alias set must not over-constrain balance"
    );
}

#[test]
fn test_connected_input_alias_with_multilayer_subscripts_promotes_internal_inputs() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.signal"),
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
    flat.add_variable(
        VarName::new("bus.target"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("bus.target"),
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

    add_connection_equation(&mut flat, "bus[1].signal[2]", "bus[1].target[3]");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for multi-layer indexed input aliases");

    for name in ["bus.signal", "bus.target"] {
        let n = rumoca_core::VarName::new(name);
        assert!(
            dae.variables.algebraics.contains_key(&n),
            "internal input {name} should be promoted through multi-layer subscript fallback"
        );
        assert!(
            !dae.variables.inputs.contains_key(&n),
            "internal input {name} should not remain classified as input after promotion"
        );
    }
}
