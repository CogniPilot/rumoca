use super::*;

fn test_comp_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![],
        }],
        def_id: None,
    }
}

fn explicit(lhs: &str, rhs: Expression, origin: &str) -> rumoca_ir_dae::Equation {
    let lhs = VarName::new(lhs);
    rumoca_ir_dae::Equation::explicit(
        flat_to_dae_var_name(&lhs),
        flat_to_dae_expression(&rhs),
        Span::DUMMY,
        origin.to_string(),
    )
}

fn reaches_source_alias_chain(
    start: &str,
    aliases: &std::collections::HashMap<String, String>,
) -> bool {
    let mut cur = start.to_string();
    let mut visited = std::collections::HashSet::<String>::new();
    for _ in 0..16 {
        if cur == "src" || !visited.insert(cur.clone()) {
            return cur == "src";
        }
        let next = match aliases.get(&cur) {
            Some(next) => next,
            None => return false,
        };
        cur = next.clone();
    }
    false
}

#[test]
fn lower_algorithm_uses_statement_span_for_main_assignment_equations() {
    let algorithm_span = Span::new(
        rumoca_core::SourceId(7),
        rumoca_core::BytePos(11),
        rumoca_core::BytePos(23),
    );
    let statement_span = Span::new(
        rumoca_core::SourceId(7),
        rumoca_core::BytePos(17),
        rumoca_core::BytePos(21),
    );
    let flat = flat::Model::new();
    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        VarName::new("x"),
        rumoca_ir_dae::Variable::new(VarName::new("x")),
    );
    let algorithm = flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: test_comp_ref("x"),
                value: Expression::Literal {
                    value: Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }
            .with_span(statement_span),
        ],
        algorithm_span,
        "algorithm",
    );

    let lowered = lower_algorithm_to_equations(&dae, &flat, &algorithm).expect("lower algorithm");

    assert_eq!(lowered.main.len(), 1);
    assert_eq!(lowered.main[0].span, statement_span);
}

#[test]
fn lower_algorithm_uses_statement_span_for_when_assignment_equations() {
    let algorithm_span = Span::new(
        rumoca_core::SourceId(8),
        rumoca_core::BytePos(31),
        rumoca_core::BytePos(47),
    );
    let statement_span = Span::new(
        rumoca_core::SourceId(8),
        rumoca_core::BytePos(35),
        rumoca_core::BytePos(43),
    );
    let flat = flat::Model::new();
    let mut dae = Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y")),
    );
    let algorithm = flat::Algorithm::new(
        vec![
            rumoca_core::Statement::When {
                blocks: vec![rumoca_core::StatementBlock {
                    cond: Expression::Literal {
                        value: Literal::Boolean(true),
                        span: rumoca_core::Span::DUMMY,
                    },
                    stmts: vec![rumoca_core::Statement::Assignment {
                        comp: test_comp_ref("y"),
                        value: Expression::Literal {
                            value: Literal::Boolean(false),
                            span: rumoca_core::Span::DUMMY,
                        },
                        span: rumoca_core::Span::DUMMY,
                    }],
                }],
                span: rumoca_core::Span::DUMMY,
            }
            .with_span(statement_span),
        ],
        algorithm_span,
        "algorithm",
    );

    let lowered = lower_algorithm_to_equations(&dae, &flat, &algorithm).expect("lower algorithm");

    assert_eq!(lowered.f_m.len(), 1);
    assert_eq!(lowered.f_m[0].span, statement_span);
}

#[test]
fn lower_algorithm_rejects_reinit_outside_when_statement() {
    let flat = flat::Model::new();
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), dae::Variable::new(VarName::new("x")));
    let algorithm = flat::Algorithm::new(
        vec![rumoca_core::Statement::Reinit {
            variable: test_comp_ref("x"),
            value: Expression::Literal {
                value: Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
        }],
        Span::DUMMY,
        "algorithm",
    );

    let err = match lower_algorithm_to_equations(&dae, &flat, &algorithm) {
        Ok(_) => panic!("bare reinit() must be rejected"),
        Err(err) => err,
    };
    assert!(err.contains("Reinit"));
}

#[test]
fn guarded_expr_prepends_to_existing_if_without_nesting_fallback() {
    let first = guarded_expr(
        Expression::VarRef {
            name: VarName::new("g1").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
        Expression::Literal {
            value: Literal::Integer(0),
            span: rumoca_core::Span::DUMMY,
        },
    );
    let merged = guarded_expr(
        Expression::VarRef {
            name: VarName::new("g2").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        Expression::Literal {
            value: Literal::Integer(2),
            span: rumoca_core::Span::DUMMY,
        },
        first,
    );

    let Expression::If {
        branches,
        else_branch,
        ..
    } = merged
    else {
        panic!("guarded expression should lower to an if-expression");
    };

    assert_eq!(branches.len(), 2);
    assert!(matches!(
        branches[0].1,
        Expression::Literal {
            value: Literal::Integer(2),
            span: rumoca_core::Span::DUMMY
        }
    ));
    assert!(matches!(
        branches[1].1,
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY
        }
    ));
    assert!(matches!(
        *else_branch,
        Expression::Literal {
            value: Literal::Integer(0),
            span: rumoca_core::Span::DUMMY
        }
    ));
}

#[test]
fn expression_exceeds_node_budget_counts_nested_expression_nodes() {
    let expr = Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        }),
        rhs: Box::new(Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs: Box::new(Expression::Literal {
                value: Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };

    assert!(expression_exceeds_node_budget(&expr, 3));
    assert!(!expression_exceeds_node_budget(&expr, 4));
}

#[test]
fn lower_for_if_branches_do_not_share_current_value_state() {
    let mut dae = Dae::new();
    dae.variables
        .discrete_valued
        .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));
    let flat = flat::Model::new();
    let statements = vec![rumoca_core::Statement::If {
        cond_blocks: vec![
            rumoca_core::StatementBlock {
                cond: Expression::VarRef {
                    name: VarName::new("c1").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: test_comp_ref("z"),
                    value: Expression::Literal {
                        value: Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    },
                    span: rumoca_core::Span::DUMMY,
                }],
            },
            rumoca_core::StatementBlock {
                cond: Expression::VarRef {
                    name: VarName::new("c2").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                },
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: test_comp_ref("z"),
                    value: Expression::Binary {
                        op: rumoca_core::OpBinary::Add,
                        lhs: Box::new(Expression::VarRef {
                            name: VarName::new("z").into(),
                            subscripts: vec![],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        rhs: Box::new(Expression::Literal {
                            value: Literal::Integer(2),
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                    span: rumoca_core::Span::DUMMY,
                }],
            },
        ],
        else_block: None,
        span: rumoca_core::Span::DUMMY,
    }];
    let indices = vec![rumoca_core::ForIndex {
        ident: "i".to_string(),
        range: Expression::Range {
            start: Box::new(Expression::Literal {
                value: Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            }),
            step: None,
            end: Box::new(Expression::Literal {
                value: Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
    }];

    let lowered = lower_for_statement_assignments(
        &dae,
        &flat,
        &indices,
        &statements,
        &IndexMap::new(),
        &HashSet::new(),
    )
    .expect("for-if should lower");
    let (_, rhs, _, _) = lowered
        .iter()
        .find(|(target, _, _, _)| target.as_str() == "z")
        .expect("missing lowered z assignment");
    let Expression::If { branches, .. } = rhs else {
        panic!("expected merged branch expression, got {rhs:?}");
    };
    assert_eq!(branches.len(), 2, "expected if/elseif branches");
    let second_branch_rhs = format!("{:?}", branches[1].1);
    assert!(
        second_branch_rhs.contains("BuiltinCall")
            && second_branch_rhs.contains("Pre")
            && second_branch_rhs.contains("Integer(2)"),
        "second branch should read event-entry z, got {second_branch_rhs}"
    );
    assert!(
        !second_branch_rhs.contains("Integer(1)"),
        "second branch must not inherit the first sibling branch assignment, got {second_branch_rhs}"
    );
}

fn integer_literal(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn real_literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn array_literal(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn parameter_array(name: &str, elements: Vec<Expression>) -> dae::Variable {
    dae::Variable {
        name: VarName::new(name),
        dims: vec![4],
        start: Some(array_literal(elements)),
        ..Default::default()
    }
}

fn indexed_var_ref(name: &str, index_name: &str) -> Expression {
    Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![Subscript::generated_expr(Box::new(Expression::VarRef {
            name: VarName::new(index_name).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }))],
        span: rumoca_core::Span::DUMMY,
    }
}

fn table_pattern_if_statement() -> rumoca_core::Statement {
    rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: Expression::Binary {
                op: rumoca_core::OpBinary::Ge,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("time").into(),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(indexed_var_ref("t", "i")),
                span: rumoca_core::Span::DUMMY,
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: test_comp_ref("y"),
                value: indexed_var_ref("x", "i"),
                span: rumoca_core::Span::DUMMY,
            }],
        }],
        else_block: None,
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn lower_for_table_pattern_folds_static_parameter_array_indices() {
    let mut dae = Dae::new();
    dae.variables
        .discrete_valued
        .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
    dae.variables.parameters.insert(
        VarName::new("x"),
        parameter_array(
            "x",
            vec![
                integer_literal(1),
                integer_literal(2),
                integer_literal(3),
                integer_literal(4),
            ],
        ),
    );
    dae.variables.parameters.insert(
        VarName::new("t"),
        parameter_array(
            "t",
            vec![
                real_literal(1.0),
                real_literal(2.0),
                real_literal(3.0),
                real_literal(4.0),
            ],
        ),
    );
    let flat = flat::Model::new();
    let statements = vec![table_pattern_if_statement()];
    let indices = vec![rumoca_core::ForIndex {
        ident: "i".to_string(),
        range: Expression::Range {
            start: Box::new(integer_literal(1)),
            step: None,
            end: Box::new(integer_literal(4)),
            span: rumoca_core::Span::DUMMY,
        },
    }];

    let lowered = lower_for_statement_assignments(
        &dae,
        &flat,
        &indices,
        &statements,
        &IndexMap::new(),
        &HashSet::new(),
    )
    .expect("table-like for loop should lower");
    let (_, rhs, _, _) = lowered
        .iter()
        .find(|(target, _, _, _)| target.as_str() == "y")
        .expect("missing lowered y assignment");

    assert!(
        expression_node_count(rhs) < 80,
        "static parameter array indices should be folded, got {rhs:?}"
    );
    assert!(
        !format!("{rhs:?}").contains("Array"),
        "guarded table expression should not clone full parameter arrays"
    );
}

#[test]
fn canonicalize_discrete_assignments_reroutes_connection_aliases_for_defined_target() {
    let mut dae = Dae::new();
    for name in ["y", "u", "v"] {
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(rumoca_core::VarName::new(name)),
        );
    }

    dae.discrete.valued_updates.push(explicit(
        "y",
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
        "explicit equation from source",
    ));
    dae.discrete.valued_updates.push(explicit(
        "y",
        Expression::VarRef {
            name: VarName::new("u").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        "connection equation: y = u",
    ));
    dae.discrete.valued_updates.push(explicit(
        "y",
        Expression::VarRef {
            name: VarName::new("v").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        "connection equation: y = v",
    ));

    canonicalize_discrete_assignment_equations(&mut dae)
        .expect("canonicalize discrete assignments");

    let mut has_source = false;
    let mut has_u_alias = false;
    let mut has_v_alias = false;
    for eq in &dae.discrete.valued_updates {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        match lhs.as_str() {
            "y" => {
                has_source |= matches!(
                    eq.rhs,
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY
                    }
                );
                assert!(
                    !is_connection_equation_origin(&eq.origin),
                    "y should keep only non-connection defining equation"
                );
            }
            "u" => {
                has_u_alias |= matches!(
                    eq.rhs,
                    rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
                        if name.as_str() == "y" && subscripts.is_empty()
                );
            }
            "v" => {
                has_v_alias |= matches!(
                    eq.rhs,
                    rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
                        if name.as_str() == "y" && subscripts.is_empty()
                );
            }
            _ => {}
        }
    }

    assert!(has_source, "expected source definition for y");
    assert!(has_u_alias, "expected rerouted alias u = y");
    assert!(has_v_alias, "expected rerouted alias v = y");
}

#[test]
fn canonicalize_discrete_assignments_resolves_reroute_target_collisions() {
    let mut dae = Dae::new();
    for name in ["y", "u", "v"] {
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(rumoca_core::VarName::new(name)),
        );
    }

    dae.discrete.valued_updates.push(explicit(
        "y",
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
        "explicit equation from source",
    ));
    dae.discrete.valued_updates.push(explicit(
        "y",
        Expression::VarRef {
            name: VarName::new("u").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        "connection equation: y = u",
    ));
    dae.discrete.valued_updates.push(explicit(
        "u",
        Expression::VarRef {
            name: VarName::new("v").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        "connection equation: u = v",
    ));

    canonicalize_discrete_assignment_equations(&mut dae)
        .expect("canonicalize discrete assignments");

    let mut lhs_counts: IndexMap<String, usize> = IndexMap::new();
    let mut has_u_from_y = false;
    let mut has_v_from_u = false;
    let mut has_y_source = false;

    for eq in &dae.discrete.valued_updates {
        if let Some(lhs) = eq.lhs.as_ref() {
            *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
        }
        match eq.lhs.as_ref().map(|name| name.as_str()) {
            Some("y") => {
                has_y_source |= matches!(
                    eq.rhs,
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY
                    }
                );
            }
            Some("u") => {
                has_u_from_y |= matches!(
                    eq.rhs,
                    rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
                        if name.as_str() == "y" && subscripts.is_empty()
                );
            }
            Some("v") => {
                has_v_from_u |= matches!(
                    eq.rhs,
                    rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
                        if name.as_str() == "u" && subscripts.is_empty()
                );
            }
            _ => {}
        }
    }

    for (lhs, count) in lhs_counts {
        assert!(
            count <= 1,
            "duplicate discrete assignment target after canonicalize: {lhs} ({count})"
        );
    }
    assert!(has_y_source, "expected source definition for y");
    assert!(has_u_from_y, "expected rerouted alias u = y");
    assert!(has_v_from_u, "expected collision resolution alias v = u");
}

#[test]
fn canonicalize_discrete_assignments_preserves_chain_connectivity_to_source() {
    let mut dae = Dae::new();
    for name in ["src", "a", "b", "c"] {
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(rumoca_core::VarName::new(name)),
        );
    }

    dae.discrete.valued_updates.push(explicit(
        "src",
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
        "explicit equation from source",
    ));
    dae.discrete.valued_updates.push(explicit(
        "src",
        Expression::VarRef {
            name: VarName::new("a").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        "connection equation: src = a",
    ));
    dae.discrete.valued_updates.push(explicit(
        "a",
        Expression::VarRef {
            name: VarName::new("b").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        "connection equation: a = b",
    ));
    dae.discrete.valued_updates.push(explicit(
        "b",
        Expression::VarRef {
            name: VarName::new("c").into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        "connection equation: b = c",
    ));

    canonicalize_discrete_assignment_equations(&mut dae)
        .expect("canonicalize discrete assignments");

    let mut lhs_counts: IndexMap<String, usize> = IndexMap::new();
    let mut aliases = std::collections::HashMap::<String, String>::new();
    let mut has_source = false;
    for eq in &dae.discrete.valued_updates {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        *lhs_counts.entry(lhs.as_str().to_string()).or_default() += 1;
        if lhs.as_str() == "src" {
            has_source |= matches!(
                eq.rhs,
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY
                }
            );
        }
        if let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = &eq.rhs
            && subscripts.is_empty()
        {
            aliases.insert(lhs.as_str().to_string(), name.as_str().to_string());
        }
    }

    for (lhs, count) in lhs_counts {
        assert!(
            count <= 1,
            "duplicate discrete assignment target after canonicalize: {lhs} ({count})"
        );
    }
    assert!(has_source, "expected source definition for src");

    assert!(
        reaches_source_alias_chain("a", &aliases),
        "expected a to remain connected to src, aliases={aliases:?}"
    );
    assert!(
        reaches_source_alias_chain("b", &aliases),
        "expected b to remain connected to src, aliases={aliases:?}"
    );
    assert!(
        reaches_source_alias_chain("c", &aliases),
        "expected c to remain connected to src, aliases={aliases:?}"
    );
}

#[test]
fn canonicalize_discrete_assignments_preserves_conflicting_same_target_rows() {
    let mut dae = Dae::new();
    dae.variables.discrete_valued.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y")),
    );

    dae.discrete.valued_updates.push(explicit(
        "y",
        Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
        "explicit equation from first source",
    ));
    dae.discrete.valued_updates.push(explicit(
        "y",
        Expression::Literal {
            value: Literal::Integer(2),
            span: rumoca_core::Span::DUMMY,
        },
        "explicit equation from second source",
    ));

    canonicalize_discrete_assignment_equations(&mut dae)
        .expect("canonicalize discrete assignments");

    assert_eq!(
        dae.discrete.valued_updates.len(),
        2,
        "canonicalization should not hide repeated non-connection assignments; validation owns the error"
    );
}
