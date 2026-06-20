use super::*;

fn sequential_when_same_target_model() -> Model {
    let mut flat = Model::new();
    let first_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("when_lowering_fixture.mo"),
        10,
        20,
    );
    let second_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("when_lowering_fixture.mo"),
        30,
        40,
    );
    for name in ["c1", "c2", "y"] {
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

    let mut algorithm =
        flat::Algorithm::new(Vec::new(), crate::test_support::test_span(), "algorithm");
    algorithm.outputs.push(VarName::new("y").into());
    algorithm.statements.push(rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: make_var_ref("c1"),
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: Expression::Literal {
                    value: Literal::Boolean(false),
                    span: first_span,
                },
                span: first_span,
            }],
        }],
        span: first_span,
    });
    algorithm.statements.push(rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: make_var_ref("c2"),
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: second_span,
                },
                span: second_span,
            }],
        }],
        span: second_span,
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
        .find(|eq| eq.lhs.as_ref().is_some_and(|lhs| lhs.as_str() == "y"))
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
                ..
            }
        ),
        "later when-statement must win when both guards are true"
    );
    assert!(
        matches!(
            &branches[1].1,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                ..
            }
        ),
        "earlier when-statement must be preserved as lower-priority branch"
    );
    let rumoca_core::Expression::If {
        branches: initial_branches,
        else_branch: inactive_else,
        span: inactive_span,
    } = else_branch.as_ref()
    else {
        panic!("algorithm when lowering must preserve the initial-section value before pre(y)");
    };
    let expected_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("when_lowering_fixture.mo"),
        30,
        40,
    );
    assert_eq!(
        *inactive_span, expected_span,
        "generated inactive initial/pre guard must retain the assignment source span"
    );
    assert_eq!(initial_branches.len(), 1);
    assert!(matches!(
        &initial_branches[0].0,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            span,
            ..
        } if *span == expected_span
    ));
    assert!(matches!(
        &initial_branches[0].1,
        rumoca_core::Expression::VarRef {
            span,
            ..
        } if *span == expected_span
    ));
    // SPEC_0007 Stage 3 Contract: pre() is eliminated in every partition by
    // phase-dae::pre_lowering. The inactive-else branch carries a reference
    // to the __pre__.* parameter slot, not a BuiltinCall { Pre }.
    let rumoca_core::Expression::VarRef {
        name,
        span: inactive_else_span,
        ..
    } = inactive_else.as_ref()
    else {
        panic!("inactive-else branch must be a __pre__.* parameter reference");
    };
    assert_eq!(
        *inactive_else_span, expected_span,
        "generated inactive pre reference must retain the assignment source span"
    );
    assert!(
        name.as_str().starts_with("__pre__."),
        "expected __pre__.* reference, got {}",
        name.as_str(),
    );
}

fn make_lt_expr(lhs: &str, rhs: i64) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(make_var_ref(lhs)),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(rhs),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    }
}

fn build_when_condition_alias_model(use_alias_guard: bool) -> Model {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("x"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("x"),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.add_variable(
        VarName::new("belowGround"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("belowGround"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
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
        VarName::new("z"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("z"),
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
            lhs: Box::new(make_var_ref("belowGround")),
            rhs: Box::new(make_lt_expr("x", 0)),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
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
    let mut when_clause = flat::WhenClause::new(guard, crate::test_support::test_span());
    when_clause.add_equation(flat::WhenEquation::assign(
        VarName::new("z"),
        Expression::Literal {
            value: Literal::Integer(1),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
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
                crate::test_support::test_span(),
            )],
            span: crate::test_support::test_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("__pre__.c").into(),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    crate::test_support::test_span(),
                )],
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        }),
        span: crate::test_support::test_span(),
    };
    assert!(
        rumoca_core::expressions_semantically_equal(direct_guard, &expected_edge_guard),
        "MLS §8.3.5.1: direct relational when-guards should fire on false->true edges"
    );
    assert!(
        rumoca_core::expressions_semantically_equal(alias_guard, &expected_edge_guard),
        "MLS §8.3.5.1: boolean alias guards should lower to the same edge-wrapped relation"
    );

    let edge_alias_condition = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Edge,
        args: vec![flat_to_dae_expression(&make_var_ref("belowGround"))],
        span: crate::test_support::test_span(),
    };
    assert!(
        alias_dae
            .conditions
            .relations
            .iter()
            .any(|relation| rumoca_core::expressions_semantically_equal(relation, &expected_guard)),
        "alias model should expose the relational guard to canonical condition roots"
    );
    assert!(
        !alias_dae.conditions.relations.iter().any(|relation| {
            rumoca_core::expressions_semantically_equal(relation, &edge_alias_condition)
        }),
        "alias model should not lower when-guard to edge(belowGround)"
    );
}
