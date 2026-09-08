use super::*;

const TEST_FILE: &str = "connections.mo";

fn test_source_map() -> SourceMap {
    let mut source_map = SourceMap::new();
    source_map.add(TEST_FILE, "connect(a.p, b.n); for i in 1:2 loop end for;");
    source_map
}

fn make_token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: std::sync::Arc::from(text),
        location: rumoca_core::Location {
            start_line: 1,
            start_column: 1,
            end_line: 1,
            end_column: 2,
            start: 0,
            end: 1,
            source: rumoca_core::SourceId::from_source_name(TEST_FILE),
        },
        token_number: 0,
        token_type: 0,
    }
}

fn make_comp_ref(names: &[&str]) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|name| ast::ComponentRefPart {
                ident: make_token(name),
                subs: None,
                def_id: None,
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    }
}

fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
    ast::Expression::ComponentReference(make_comp_ref(names))
}

fn make_integer_terminal(value: &str) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: make_token(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_connect(lhs: &[&str], rhs: &[&str]) -> ast::Equation {
    ast::Equation::Connect {
        lhs: make_comp_ref(lhs),
        rhs: make_comp_ref(rhs),
    }
}

fn scalar_connection(connection: &ast::InstanceConnection) -> &ast::InstanceScalarConnection {
    connection
        .as_scalar()
        .expect("fixture must produce a scalar connection")
}

fn connection_family(connection: &ast::InstanceConnection) -> &ast::InstanceConnectionFamily {
    connection
        .as_family()
        .expect("fixture must produce a compact connection family")
}

fn make_range_expr(start: ast::Expression, end: ast::Expression) -> ast::Expression {
    ast::Expression::Range {
        start: std::sync::Arc::new(start),
        step: None,
        end: std::sync::Arc::new(end),
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_comp_ref_with_sub(expr: ast::Expression, names: &[&str]) -> ast::ComponentReference {
    make_comp_ref_with_sub_at(expr, names, 0)
}

fn make_comp_ref_with_sub_at(
    expr: ast::Expression,
    names: &[&str],
    sub_part_index: usize,
) -> ast::ComponentReference {
    let mut parts = Vec::new();
    for (i, name) in names.iter().enumerate() {
        parts.push(ast::ComponentRefPart {
            ident: make_token(name),
            subs: if i == sub_part_index {
                Some(vec![ast::Subscript::Expression(expr.clone())])
            } else {
                None
            },
            def_id: None,
        });
    }
    ast::ComponentReference {
        local: false,
        parts,
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    }
}

#[test]
fn test_extract_connection() {
    let eq = ast::Equation::Connect {
        lhs: make_comp_ref(&["a", "p"]),
        rhs: make_comp_ref(&["b", "n"]),
    };

    let prefix = ast::QualifiedName::new();
    let source_map = test_source_map();
    let connections =
        extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map).unwrap();

    assert_eq!(connections.len(), 1);
    assert_eq!(
        scalar_connection(&connections[0]).a().to_flat_string(),
        "a.p"
    );
    assert_eq!(
        scalar_connection(&connections[0]).b().to_flat_string(),
        "b.n"
    );
}

#[test]
fn an_undecidable_conditional_connection_fails_instead_of_unioning_branches() {
    let equation = ast::Equation::If {
        cond_blocks: vec![ast::EquationBlock {
            cond: make_comp_ref_expr(&["runtime_enabled"]),
            eqs: vec![make_connect(&["a"], &["b"])],
        }],
        else_block: Some(vec![make_connect(&["c"], &["d"])]),
    };

    let error = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect_err("an unknown structural branch must never fabricate both connection sets");

    assert!(matches!(
        *error,
        InstantiateError::StructuralParamError { ref name, ref msg, .. }
            if name == "runtime_enabled"
                && msg.contains("cannot decide a connection if-equation branch")
    ));
}

#[test]
fn a_qualified_nested_boolean_selects_exactly_one_connection_branch() {
    let equation = ast::Equation::If {
        cond_blocks: vec![ast::EquationBlock {
            cond: make_comp_ref_expr(&["settings", "enabled"]),
            eqs: vec![make_connect(&["a"], &["b"])],
        }],
        else_block: Some(vec![make_connect(&["c"], &["d"])]),
    };
    let mut params = ConnectionParams::new();
    params.bools.insert("settings.enabled".to_string(), false);

    let connections = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &params,
        &test_source_map(),
    )
    .expect("the qualified structural Boolean is known");

    assert_eq!(connections.len(), 1);
    assert_eq!(scalar_connection(&connections[0]).a().to_flat_string(), "c");
    assert_eq!(scalar_connection(&connections[0]).b().to_flat_string(), "d");
}

/// MLS §4.4.5 removes a connection owned by a false conditional component
/// before flattening asks for endpoint declaration/type evidence.  The names
/// deliberately have no declarations in this fixture: retaining this connect
/// would make the later fail-closed connection boundary reject a legal inactive
/// branch instead of pruning it.
#[test]
fn false_conditional_connection_is_pruned_before_endpoint_evidence_is_required() {
    let equation = ast::Equation::If {
        cond_blocks: vec![ast::EquationBlock {
            cond: make_comp_ref_expr(&["enabled"]),
            eqs: vec![make_connect(
                &["inactive", "missing_left"],
                &["inactive", "missing_right"],
            )],
        }],
        else_block: None,
    };
    let mut params = ConnectionParams::new();
    params.bools.insert("enabled".to_string(), false);

    let connections = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &params,
        &test_source_map(),
    )
    .expect("a false structural branch is removed before endpoint validation");

    assert!(connections.is_empty());
}

#[test]
fn connection_subscripts_refuse_unpreserved_colon_and_recovery_shapes() {
    let colon = ast::Subscript::Range {
        token: make_token(":"),
    };
    let end = ast::Subscript::Expression(ast::Expression::Terminal {
        terminal_type: ast::TerminalType::End,
        token: make_token("end"),
        span: Span::DUMMY,
    });
    let fixtures = [
        (colon, "not yet supported"),
        (end, "end"),
        (ast::Subscript::Empty, "Subscript::Empty"),
    ];
    for (subscript, expected) in fixtures {
        let mut lhs = make_comp_ref(&["a"]);
        lhs.parts[0].subs = Some(vec![subscript]);
        let error = extract_connections(
            &[ast::Equation::Connect {
                lhs,
                rhs: make_comp_ref(&["b"]),
            }],
            &ast::QualifiedName::new(),
            &ConnectionParams::new(),
            &test_source_map(),
        )
        .expect_err("unsupported/recovery subscripts must not be erased into connection `a`");
        assert!(matches!(
            *error,
        InstantiateError::UnsupportedConnectionSubscript { ref selector, ref reason, .. }
                if selector.contains(expected) || reason.contains(expected)
        ));
    }
}

#[test]
fn connection_scalar_subscripts_never_launder_unknown_or_invalid_slots() {
    let unknown = ast::Subscript::Expression(make_comp_ref_expr(&["unknown"]));
    let nested_recovery = ast::Subscript::Expression(ast::Expression::Parenthesized {
        inner: std::sync::Arc::new(ast::Expression::Empty { span: Span::DUMMY }),
        span: Span::DUMMY,
    });
    let mixed = vec![
        ast::Subscript::Expression(make_integer_terminal("1")),
        unknown.clone(),
    ];
    for subscripts in [vec![unknown], vec![nested_recovery], mixed] {
        let mut lhs = make_comp_ref(&["a"]);
        lhs.parts[0].subs = Some(subscripts);
        let error = extract_connections(
            &[ast::Equation::Connect {
                lhs,
                rhs: make_comp_ref(&["b"]),
            }],
            &ast::QualifiedName::new(),
            &ConnectionParams::new(),
            &test_source_map(),
        )
        .expect_err("every supplied selector must produce exactly one endpoint index");
        assert!(
            matches!(
                *error,
                InstantiateError::UnsupportedConnectionSubscript { .. }
            ),
            "unknown, nested-invalid, and mixed selectors must fail with EI035: {error}"
        );
    }
}

#[test]
fn test_extract_connection_expands_range_on_non_first_part() {
    // Regression: connect(mux2.y, mux5.u[1:2]) must expand even when
    // the range subscript is on the second component-reference part.
    let range = ast::Expression::Range {
        start: std::sync::Arc::new(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("1"),
            span: rumoca_core::Span::DUMMY,
        }),
        step: None,
        end: std::sync::Arc::new(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token("2"),
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let eq = ast::Equation::Connect {
        lhs: make_comp_ref(&["mux2", "y"]),
        rhs: make_comp_ref_with_sub_at(range, &["mux5", "u"], 1),
    };

    let prefix = ast::QualifiedName::new();
    let source_map = test_source_map();
    let connections =
        extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map).unwrap();

    assert_eq!(connections.len(), 1);
    assert!(connections[0].as_family().is_some());
    let mut got: Vec<(String, String)> =
        rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
            .expect("valid range connection family")
            .into_iter()
            .map(|connection| {
                (
                    connection.a().to_flat_string(),
                    connection.b().to_flat_string(),
                )
            })
            .collect();
    got.sort();

    assert_eq!(
        got,
        vec![
            ("mux2.y[1]".to_string(), "mux5.u[1]".to_string()),
            ("mux2.y[2]".to_string(), "mux5.u[2]".to_string()),
        ]
    );
}

#[test]
fn test_extract_connection_preserves_multidimensional_ranges() {
    let mut lhs = make_comp_ref(&["a"]);
    lhs.parts[0].subs = Some(vec![
        ast::Subscript::Expression(make_range_expr(
            make_integer_terminal("1"),
            make_integer_terminal("2"),
        )),
        ast::Subscript::Expression(make_range_expr(
            make_integer_terminal("4"),
            make_integer_terminal("5"),
        )),
    ]);
    let mut rhs = make_comp_ref(&["b"]);
    rhs.parts[0].subs = Some(vec![
        ast::Subscript::Expression(make_range_expr(
            make_integer_terminal("7"),
            make_integer_terminal("8"),
        )),
        ast::Subscript::Expression(make_range_expr(
            make_integer_terminal("9"),
            make_integer_terminal("10"),
        )),
    ]);

    let connections = extract_connections(
        &[ast::Equation::Connect { lhs, rhs }],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect("multidimensional range connection should instantiate");

    assert_eq!(connections.len(), 1);
    let family = connection_family(&connections[0]);
    assert_eq!(family.domain().extents(), Ok(vec![2, 2]));
    let members = rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
        .expect("multidimensional connection family should evaluate")
        .into_iter()
        .map(|member| (member.a().to_flat_string(), member.b().to_flat_string()))
        .collect::<Vec<_>>();
    assert_eq!(
        members,
        vec![
            ("a[1,4]".to_string(), "b[7,9]".to_string()),
            ("a[1,5]".to_string(), "b[7,10]".to_string()),
            ("a[2,4]".to_string(), "b[8,9]".to_string()),
            ("a[2,5]".to_string(), "b[8,10]".to_string()),
        ]
    );
}

#[test]
fn empty_connection_ranges_produce_empty_scalar_views() {
    let empty_range = make_range_expr(make_integer_terminal("1"), make_integer_terminal("0"));
    let ForRangeExpansion::Values(values) = expand_for_range(
        &empty_range,
        &rustc_hash::FxHashMap::default(),
        &ast::QualifiedName::new(),
        &mut ConnectionExpansionBudget::new(),
    ) else {
        panic!("an empty range is evaluable without materialization");
    };
    assert!(values.is_empty());

    let mut lhs = make_comp_ref(&["a"]);
    lhs.parts[0].subs = Some(vec![ast::Subscript::Expression(empty_range.clone())]);
    let mut rhs = make_comp_ref(&["b"]);
    rhs.parts[0].subs = Some(vec![ast::Subscript::Expression(empty_range)]);
    let connections = extract_connections(
        &[ast::Equation::Connect { lhs, rhs }],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect("an empty range connection is valid");

    assert_eq!(connections.len(), 1);
    assert_eq!(
        connection_family(&connections[0]).domain().scalar_count(),
        Ok(0)
    );
    assert!(
        rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
            .expect("empty family has a valid derived view")
            .is_empty()
    );
}

#[test]
fn test_extract_connections_nested_for_range_depends_on_outer_index() {
    let eq = nested_dependent_for_connection_eq();
    let prefix = ast::QualifiedName::new();
    let source_map = test_source_map();
    let params = ConnectionParams::new();
    let conns = extract_connections(&[eq], &prefix, &params, &source_map).unwrap();

    let mut got: Vec<(String, String)> = conns
        .iter()
        .flat_map(|connection| {
            rumoca_eval_ast::connection::scalar_connection_members(connection)
                .expect("valid structured connection")
        })
        .map(|connection| {
            (
                connection.a().to_flat_string(),
                connection.b().to_flat_string(),
            )
        })
        .collect();
    got.sort();

    let expected = vec![
        ("a[1]".to_string(), "b[2]".to_string()),
        ("a[1]".to_string(), "b[3]".to_string()),
        ("a[2]".to_string(), "b[3]".to_string()),
    ];
    assert_eq!(got, expected);
}

#[test]
fn test_extract_connections_multi_index_range_depends_on_prior_index() {
    let eq = multi_index_dependent_for_connection_eq();
    let prefix = ast::QualifiedName::new();
    let source_map = test_source_map();
    let params = ConnectionParams::new();
    let conns = extract_connections(&[eq], &prefix, &params, &source_map).unwrap();

    let mut got: Vec<(String, String)> = conns
        .iter()
        .flat_map(|connection| {
            rumoca_eval_ast::connection::scalar_connection_members(connection)
                .expect("valid structured connection")
        })
        .map(|connection| {
            (
                connection.a().to_flat_string(),
                connection.b().to_flat_string(),
            )
        })
        .collect();
    got.sort();

    let expected = vec![
        ("a[1]".to_string(), "b[2]".to_string()),
        ("a[1]".to_string(), "b[3]".to_string()),
        ("a[2]".to_string(), "b[3]".to_string()),
    ];
    assert_eq!(got, expected);
}

#[test]
fn test_extract_connections_skips_non_connection_for_equation_range() {
    let eq = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: ast::Expression::ComponentReference(make_comp_ref(&["nout"])),
        }],
        equations: vec![ast::Equation::Simple {
            lhs: make_comp_ref_expr(&["aux", "i"]),
            rhs: make_integer_terminal("0"),
        }],
    };

    let prefix = ast::QualifiedName::new();
    let source_map = SourceMap::new();
    let connections =
        extract_connections(&[eq], &prefix, &ConnectionParams::new(), &source_map).unwrap();

    assert!(connections.is_empty());
}

#[test]
fn test_extract_regular_for_connection_preserves_one_symbolic_family() {
    let eq = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("3")),
        }],
        equations: vec![ast::Equation::Connect {
            lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["a"]),
            rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["b"]),
        }],
    };

    let connections = extract_connections(
        &[eq],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect("regular vectorized connection should instantiate");

    assert_eq!(connections.len(), 1);
    let family = connection_family(&connections[0]);
    assert_eq!(family.domain().scalar_count(), Ok(3));
    let members = rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
        .expect("symbolic connection must expose a valid derived scalar view")
        .into_iter()
        .map(|member| (member.a().to_flat_string(), member.b().to_flat_string()))
        .collect::<Vec<_>>();
    assert_eq!(
        members,
        vec![
            ("a[1]".to_string(), "b[1]".to_string()),
            ("a[2]".to_string(), "b[2]".to_string()),
            ("a[3]".to_string(), "b[3]".to_string()),
        ]
    );
}

#[test]
fn empty_regular_for_connection_preserves_a_zero_cardinality_family() {
    let equation = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("0")),
        }],
        equations: vec![ast::Equation::Connect {
            lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["a"]),
            rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["b"]),
        }],
    };

    let connections = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect("an empty regular connection family is valid");

    assert_eq!(connections.len(), 1);
    assert_eq!(
        connection_family(&connections[0]).domain().scalar_count(),
        Ok(0)
    );
    assert!(
        rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
            .expect("zero-cardinality family has an exact empty scalar view")
            .is_empty()
    );
}

#[test]
fn huge_loop_selects_invariant_false_branch_before_materialization() {
    let equation = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(
                make_integer_terminal("1"),
                make_integer_terminal("1000000000"),
            ),
        }],
        equations: vec![ast::Equation::If {
            cond_blocks: vec![rumoca_ir_ast::EquationBlock {
                cond: make_comp_ref_expr(&["enabled"]),
                eqs: vec![make_connect(&["a"], &["b"])],
            }],
            else_block: None,
        }],
    };
    let mut params = ConnectionParams::new();
    params.bools.insert("enabled".to_string(), false);

    let connections = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &params,
        &test_source_map(),
    )
    .expect("an invariant false branch must be selected before range expansion");
    assert!(connections.is_empty());
}

#[test]
fn huge_binder_dependent_fallback_is_refused_before_materialization() {
    let equation = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(
                make_integer_terminal("1"),
                make_integer_terminal("1000000000"),
            ),
        }],
        equations: vec![ast::Equation::If {
            cond_blocks: vec![rumoca_ir_ast::EquationBlock {
                cond: ast::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: std::sync::Arc::new(make_comp_ref_expr(&["i"])),
                    rhs: std::sync::Arc::new(make_integer_terminal("0")),
                    span: Span::DUMMY,
                },
                eqs: vec![make_connect(&["a"], &["b"])],
            }],
            else_block: None,
        }],
    };

    let error = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect_err("a non-compact billion-row fallback must fail early");
    assert!(error.to_string().contains("would materialize 1000000000"));
}

#[test]
fn scalar_for_equation_range_is_not_reinterpreted_as_one_to_n() {
    let equation = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_integer_terminal("3"),
        }],
        equations: vec![ast::Equation::Connect {
            lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["a"]),
            rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["b"]),
        }],
    };

    let error = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect_err("MLS §8.3.2 requires a vector iterator expression");
    assert!(matches!(
        *error,
        InstantiateError::StructuralParamError { .. }
    ));
    assert!(
        error
            .to_string()
            .contains("cannot evaluate connection for-equation range")
    );
}

#[test]
fn nested_noncompact_ranges_share_one_materialization_budget() {
    let dependent_if = ast::Equation::If {
        cond_blocks: vec![rumoca_ir_ast::EquationBlock {
            cond: ast::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                lhs: std::sync::Arc::new(make_comp_ref_expr(&["j"])),
                rhs: std::sync::Arc::new(make_integer_terminal("0")),
                span: Span::DUMMY,
            },
            eqs: vec![make_connect(&["a"], &["b"])],
        }],
        else_block: None,
    };
    let inner = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("j"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("1000")),
        }],
        equations: vec![dependent_if],
    };
    let outer = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("1001")),
        }],
        equations: vec![inner],
    };

    let error = extract_connections(
        &[outer],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect_err("nested fallbacks must share a whole-extraction work budget");
    assert!(error.to_string().contains("requires 1001000 iterations"));
}

#[test]
fn nested_for_binder_shadows_outer_index_only_after_its_own_range() {
    let inner_condition = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: std::sync::Arc::new(make_comp_ref_expr(&["i"])),
        rhs: std::sync::Arc::new(make_integer_terminal("2")),
        span: Span::DUMMY,
    };
    let inner = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("2")),
        }],
        equations: vec![ast::Equation::If {
            cond_blocks: vec![rumoca_ir_ast::EquationBlock {
                cond: inner_condition,
                eqs: vec![make_connect(&["c"], &["d"])],
            }],
            else_block: Some(vec![make_connect(&["a"], &["b"])]),
        }],
    };
    let outer = ast::Equation::For {
        indices: vec![rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("1")),
        }],
        equations: vec![inner],
    };

    let connections = extract_connections(
        &[outer],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect("the inner binder must retain its own values");
    let pairs = connections
        .iter()
        .map(|connection| {
            let connection = scalar_connection(connection);
            (
                connection.a().to_flat_string(),
                connection.b().to_flat_string(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        pairs,
        vec![
            ("a".to_string(), "b".to_string()),
            ("c".to_string(), "d".to_string()),
        ]
    );
}

#[test]
fn same_clause_binder_shadows_outer_only_after_its_own_range() {
    let indices = vec![
        rumoca_ir_ast::ForIndex {
            ident: make_token("i"),
            range: make_range_expr(make_integer_terminal("1"), make_comp_ref_expr(&["i"])),
        },
        rumoca_ir_ast::ForIndex {
            ident: make_token("j"),
            range: make_range_expr(make_comp_ref_expr(&["i"]), make_comp_ref_expr(&["i"])),
        },
    ];

    let (substituted, shadowed) = substitute_index_in_for_indices(&indices, "i", 7);
    assert!(shadowed);
    assert_eq!(
        substituted[0].range,
        make_range_expr(make_integer_terminal("1"), make_integer_terminal("7")),
        "the new binder is not in scope in its own range"
    );
    assert_eq!(
        substituted[1].range, indices[1].range,
        "the new binder must be in scope in each later index range"
    );
}

#[test]
fn same_clause_shadowing_controls_later_ranges_and_the_connection_body() {
    let equation = ast::Equation::For {
        indices: vec![
            rumoca_ir_ast::ForIndex {
                ident: make_token("i"),
                range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("1")),
            },
            rumoca_ir_ast::ForIndex {
                ident: make_token("i"),
                range: make_range_expr(make_integer_terminal("2"), make_integer_terminal("2")),
            },
            rumoca_ir_ast::ForIndex {
                ident: make_token("j"),
                range: make_range_expr(make_comp_ref_expr(&["i"]), make_comp_ref_expr(&["i"])),
            },
        ],
        equations: vec![ast::Equation::Connect {
            lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["j"]), &["a"]),
            rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["b"]),
        }],
    };

    let connections = extract_connections(
        &[equation],
        &ast::QualifiedName::new(),
        &ConnectionParams::new(),
        &test_source_map(),
    )
    .expect("same-clause shadowing must preserve the inner binder");

    assert_eq!(connections.len(), 1);
    let members = rumoca_eval_ast::connection::scalar_connection_members(&connections[0])
        .expect("the compact or scalar representation must have an exact scalar view");
    assert_eq!(members.len(), 1);
    let connection = &members[0];
    assert_eq!(connection.a().to_flat_string(), "a[2]");
    assert_eq!(connection.b().to_flat_string(), "b[2]");
}

#[test]
fn connection_integer_folding_declines_overflow_without_panicking() {
    assert_eq!(
        rumoca_core::eval_ast_integer_binary(&rumoca_core::OpBinary::Div, i64::MIN, -1),
        None
    );
    assert_eq!(
        rumoca_core::eval_ast_integer_binary(&rumoca_core::OpBinary::Add, i64::MAX, 1),
        None
    );
    assert_eq!(eval_unary_i64(&rumoca_core::OpUnary::Minus, i64::MIN), None);
    assert_eq!(
        checked_divide_affine(
            &rumoca_core::AffineForm {
                constant: i64::MIN,
                coeffs: vec![0],
            },
            -1,
        ),
        None
    );
}

fn nested_dependent_for_connection_eq() -> ast::Equation {
    let outer_idx = rumoca_ir_ast::ForIndex {
        ident: make_token("j"),
        range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("2")),
    };
    let inner_idx = rumoca_ir_ast::ForIndex {
        ident: make_token("i"),
        range: make_range_expr(j_plus_one_expr(), make_integer_terminal("3")),
    };
    ast::Equation::For {
        indices: vec![outer_idx],
        equations: vec![ast::Equation::For {
            indices: vec![inner_idx],
            equations: vec![ast::Equation::Connect {
                lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["j"]), &["a"]),
                rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["b"]),
            }],
        }],
    }
}

fn multi_index_dependent_for_connection_eq() -> ast::Equation {
    let prior_idx = rumoca_ir_ast::ForIndex {
        ident: make_token("i"),
        range: make_range_expr(make_integer_terminal("1"), make_integer_terminal("2")),
    };
    let dependent_idx = rumoca_ir_ast::ForIndex {
        ident: make_token("j"),
        range: make_range_expr(i_plus_one_expr(), make_integer_terminal("3")),
    };
    ast::Equation::For {
        indices: vec![prior_idx, dependent_idx],
        equations: vec![ast::Equation::Connect {
            lhs: make_comp_ref_with_sub(make_comp_ref_expr(&["i"]), &["a"]),
            rhs: make_comp_ref_with_sub(make_comp_ref_expr(&["j"]), &["b"]),
        }],
    }
}

fn i_plus_one_expr() -> ast::Expression {
    ast::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: std::sync::Arc::new(make_comp_ref_expr(&["i"])),
        rhs: std::sync::Arc::new(make_integer_terminal("1")),
        span: rumoca_core::Span::DUMMY,
    }
}

fn j_plus_one_expr() -> ast::Expression {
    ast::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: std::sync::Arc::new(make_comp_ref_expr(&["j"])),
        rhs: std::sync::Arc::new(make_integer_terminal("1")),
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn test_component_ref_subscript_resolves_leaf_integer_param_key() {
    // resistor[cellData.nRC].n should keep the subscript when only leaf key
    // The full component-reference path is available in int_params.
    let sub_expr = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![
            ast::ComponentRefPart {
                ident: make_token("cellData"),
                subs: None,
                def_id: None,
            },
            ast::ComponentRefPart {
                ident: make_token("nRC"),
                subs: None,
                def_id: None,
            },
        ],
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    });
    let cr = make_comp_ref_with_sub(sub_expr, &["resistor", "n"]);
    let prefix = ast::QualifiedName::new();
    let mut int_params = rustc_hash::FxHashMap::default();
    int_params.insert("cellData.nRC".to_string(), 2);

    let qn = component_ref_to_qualified_name(&cr, &prefix, &int_params, Span::DUMMY)
        .expect("the parameter selector is evaluable");
    assert_eq!(qn.to_flat_string(), "resistor[2].n");
}

#[test]
fn test_component_ref_subscript_resolves_scoped_dotted_param_key() {
    // cellData.nRC resolves from cell.cellData.nRC only when the instance
    // scope is cell.
    let sub_expr = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![
            ast::ComponentRefPart {
                ident: make_token("cellData"),
                subs: None,
                def_id: None,
            },
            ast::ComponentRefPart {
                ident: make_token("nRC"),
                subs: None,
                def_id: None,
            },
        ],
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    });
    let cr = make_comp_ref_with_sub(sub_expr, &["resistor", "n"]);
    let prefix = ast::QualifiedName::from_dotted("cell");
    let mut int_params = rustc_hash::FxHashMap::default();
    int_params.insert("cell.cellData.nRC".to_string(), 2);

    let qn = component_ref_to_qualified_name(&cr, &prefix, &int_params, Span::DUMMY)
        .expect("the scoped parameter selector is evaluable");
    assert_eq!(qn.to_flat_string(), "cell.resistor[2].n");
}

#[test]
fn test_component_ref_subscript_does_not_scan_suffix_param_keys() {
    let cr = ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: make_token("nRC"),
            subs: None,
            def_id: None,
        }],
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    };
    let mut int_params = rustc_hash::FxHashMap::default();
    int_params.insert("cellData.fake_nRC".to_string(), 4);
    int_params.insert("cellData.real.nRC".to_string(), 2);

    let scope = ast::QualifiedName::new();
    assert_eq!(resolve_int_param_ref(&cr, &int_params, &scope), None);
}
