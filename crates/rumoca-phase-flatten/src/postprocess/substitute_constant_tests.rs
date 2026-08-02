use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_flatten_substitute_constant_source_91.mo"),
        10,
        22,
    )
}

fn fixture_def_id(name: &str) -> rumoca_core::DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(hash.max(1))
}

fn component_ref(path: &str) -> rumoca_core::ComponentReference {
    let parts = rumoca_core::ComponentPath::from_flat_path(path)
        .parts()
        .iter()
        .map(|ident| rumoca_core::ComponentRefPart {
            ident: ident.clone(),
            span: test_span(),
            subs: Vec::new(),
            def_id: fixture_def_id(ident),
        })
        .collect();
    rumoca_core::ComponentReference::construct(false, test_span(), parts)
        .expect("fixture reference has an exact identity for every part")
}

fn component_ref_with_target(
    path: &str,
    target: rumoca_core::DefId,
) -> rumoca_core::ComponentReference {
    let path = rumoca_core::ComponentPath::from_flat_path(path);
    let last = path.len().checked_sub(1).expect("fixture path is nonempty");
    let parts = path
        .parts()
        .iter()
        .enumerate()
        .map(|(index, ident)| rumoca_core::ComponentRefPart {
            ident: ident.clone(),
            span: test_span(),
            subs: Vec::new(),
            def_id: if index == last {
                target
            } else {
                fixture_def_id(ident)
            },
        })
        .collect();
    rumoca_core::ComponentReference::construct(false, test_span(), parts)
        .expect("fixture reference has an exact identity for every part")
}

fn simple_assignment(value: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value,
        span: test_span(),
    }
}

fn generated_var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::generated(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn spanned_var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref(name)),
        subscripts: vec![],
        span: test_span(),
    }
}

fn source_var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref(name)),
        subscripts: vec![],
        span: test_span(),
    }
}

fn source_var_ref_with_target(name: &str, target: rumoca_core::DefId) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            name,
            component_ref_with_target(name, target),
        ),
        subscripts: vec![],
        span: test_span(),
    }
}

fn insert_source_constant(ctx: &mut Context, reference: &str, value: rumoca_core::Expression) {
    ctx.constant_values_by_def_id
        .insert(component_ref(reference).target_def_id(), value);
}

fn int_literal(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn bool_literal(value: bool, span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(value),
        span,
    }
}

#[test]
fn substitution_removes_a_dead_specialized_conditional_branch() {
    let source = rumoca_core::SourceId::from_source_name("dead_specialized_branch.mo");
    let if_span = Span::from_offsets(source, 0, 48);
    let selected_span = Span::from_offsets(source, 15, 23);
    let dead_span = Span::from_offsets(source, 29, 47);
    let condition = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(int_literal(1)),
        rhs: Box::new(int_literal(1)),
        span: Span::from_offsets(source, 3, 9),
    };
    let selected = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(7),
        span: selected_span,
    };
    let dead = rumoca_core::Expression::Index {
        base: Box::new(spanned_var_ref("inPort")),
        subscripts: vec![rumoca_core::Subscript::Index {
            value: 0,
            span: dead_span,
        }],
        span: dead_span,
    };
    let expression = rumoca_core::Expression::If {
        branches: vec![(condition, selected)],
        else_branch: Box::new(dead),
        span: if_span,
    };

    let rewritten = substitute_known_constants_expr(
        expression,
        &Context::new(),
        &rustc_hash::FxHashSet::default(),
        &HashSet::new(),
        "",
    )
    .expect("a settled condition selects exactly its live branch");

    assert!(matches!(
        rewritten,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(7),
            span,
        } if span == selected_span
    ));
}

#[test]
fn substitution_preserves_unknown_condition_order_and_provenance() {
    let source = rumoca_core::SourceId::from_source_name("partly_static_conditional.mo");
    let if_span = Span::from_offsets(source, 0, 64);
    let unknown_span = Span::from_offsets(source, 3, 10);
    let value_span = Span::from_offsets(source, 16, 21);
    let fallback_span = Span::from_offsets(source, 42, 50);
    let unknown = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref("active")),
        subscripts: Vec::new(),
        span: unknown_span,
    };
    let expression = rumoca_core::Expression::If {
        branches: vec![
            (
                unknown,
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: value_span,
                },
            ),
            (
                bool_literal(false, Span::from_offsets(source, 24, 29)),
                int_literal(2),
            ),
            (
                bool_literal(true, Span::from_offsets(source, 33, 37)),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: fallback_span,
                },
            ),
        ],
        else_branch: Box::new(int_literal(4)),
        span: if_span,
    };

    let rewritten = substitute_known_constants_expr(
        expression,
        &Context::new(),
        &rustc_hash::FxHashSet::default(),
        &HashSet::new(),
        "",
    )
    .expect("unknown branches remain ahead of a settled fallback");

    assert!(matches!(
        rewritten,
        rumoca_core::Expression::If { branches, else_branch, span }
            if span == if_span
                && matches!(branches.as_slice(),
                    [(rumoca_core::Expression::VarRef { span, .. },
                      rumoca_core::Expression::Literal {
                          value: rumoca_core::Literal::Integer(1),
                          span: value,
                      })] if *span == unknown_span && *value == value_span)
                && matches!(else_branch.as_ref(),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(3),
                        span,
                    } if *span == fallback_span)
    ));
}

fn reference_x_fill_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Div,
                lhs: Box::new(int_literal(1)),
                rhs: Box::new(generated_var_ref("nS")),
                span: rumoca_core::Span::DUMMY,
            },
            generated_var_ref("nS"),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn source_reference_x_fill_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Div,
                lhs: Box::new(int_literal(1)),
                rhs: Box::new(source_var_ref("nS")),
                span: rumoca_core::Span::DUMMY,
            },
            source_var_ref("nS"),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_primitive_variable(model: &mut flat::Model, name: &str) {
    model.add_variable(
        rumoca_core::VarName::new(name),
        flat::Variable {
            instance_id: rumoca_core::InstanceId::new(fixture_def_id(name).index()),
            name: rumoca_core::VarName::new(name),
            component_ref: Some(component_ref(name)),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
}

#[test]
fn collapse_index_refs_collapses_indexed_field_access_to_known_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("port_a[1].Q_flow"),
        flat::Variable {
            instance_id: rumoca_core::InstanceId::new(fixture_def_id("port_a[1].Q_flow").index()),
            name: rumoca_core::VarName::new("port_a[1].Q_flow"),
            component_ref: Some(component_ref("port_a[1].Q_flow")),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::generated("port_a"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "Q_flow".to_string(),
            field_def_id: fixture_def_id("Q_flow"),
            span: test_span(),
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "port_a[1].Q_flow" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_indexed_var_ref_to_known_scalar_var() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "arr[1]");
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::generated("arr"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "arr[1]" && subscripts.is_empty()
    ));
}

#[test]
fn substitutes_late_scoped_constant_inside_array_subscript() {
    let mut model = flat::Model::new();
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("medium.X"),
            subscripts: vec![rumoca_core::Subscript::Expr {
                expr: Box::new(generated_var_ref("nS")),
                span: test_span(),
            }],
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
    ));
    let mut ctx = Context::new();
    ctx.parameter_values.insert("medium.nS".to_string(), 2);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::VarRef { subscripts, .. } = &model.equations[0].residual else {
        panic!("expected indexed variable reference");
    };
    assert!(matches!(
        &subscripts[0],
        rumoca_core::Subscript::Expr { expr, .. }
            if matches!(
                expr.as_ref(),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    ..
                }
            )
    ));
}

#[test]
fn substitutes_known_constants_inside_function_defaults_and_body() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        crate::test_support::real_param("u", Vec::new(), test_span())
            .with_default(source_var_ref("Pkg.Constants.k")),
    );
    function
        .body
        .push(simple_assignment(source_var_ref("Pkg.Constants.k")));
    model.add_function(function);

    let mut ctx = Context::new();
    insert_source_constant(
        &mut ctx,
        "Pkg.Constants.k",
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(42.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    assert!(matches!(
        function.inputs[0].default,
        Some(rumoca_core::Expression::Literal { value: rumoca_core::Literal::Real(v), span }) if (v - 42.0).abs() < f64::EPSILON && span == test_span()
    ));
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::Literal { value: rumoca_core::Literal::Real(v), .. } if (*v - 42.0).abs() < f64::EPSILON
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn exact_source_constants_ignore_same_spelling_generated_lookup() {
    let first = rumoca_core::DefId::new(91_001);
    let second = rumoca_core::DefId::new(91_002);
    let mut ctx = Context::new();
    ctx.constant_values_by_def_id.insert(first, int_literal(11));
    ctx.constant_values_by_def_id
        .insert(second, int_literal(22));
    ctx.constant_values
        .insert("Pkg.k".to_string(), int_literal(99));

    let substitute = |expression| {
        substitute_known_constants_expr(
            expression,
            &ctx,
            &rustc_hash::FxHashSet::default(),
            &HashSet::new(),
            "",
        )
        .expect("fixture constant should substitute")
    };

    assert!(matches!(
        substitute(source_var_ref_with_target("Pkg.k", first)),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(11),
            ..
        }
    ));
    assert!(matches!(
        substitute(source_var_ref_with_target("Pkg.k", second)),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(22),
            ..
        }
    ));
    assert!(matches!(
        substitute(generated_var_ref("Pkg.k")),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(99),
            ..
        }
    ));
}

#[test]
fn unstructured_non_generated_constant_reference_is_rejected() {
    let mut ctx = Context::new();
    ctx.constant_values
        .insert("Pkg.k".to_string(), int_literal(99));
    let expression = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("Pkg.k"),
        subscripts: Vec::new(),
        span: test_span(),
    };

    let error = substitute_known_constants_expr(
        expression,
        &ctx,
        &rustc_hash::FxHashSet::default(),
        &HashSet::new(),
        "",
    )
    .expect_err("source-like reference without exact identity must fail early");
    assert!(matches!(
        error,
        FlattenError::MissingSourceContext { reason }
            if reason.contains("no resolved declaration identity")
    ));
}

#[test]
fn substitutes_scoped_relative_constant_alias_field() {
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "medium.steam".to_string(),
        generated_var_ref("Utilities.Water95_Utilities.Constants"),
    );
    ctx.real_parameter_values.insert(
        "medium.Utilities.Water95_Utilities.Constants.R_s".to_string(),
        461.526,
    );

    let substituted = substitute_known_constants_expr(
        generated_var_ref("steam.R_s"),
        &ctx,
        &rustc_hash::FxHashSet::default(),
        &HashSet::new(),
        "medium",
    )
    .unwrap();

    assert!(matches!(
        substituted,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } if (v - 461.526).abs() < f64::EPSILON
    ));
}

#[test]
fn substitutes_assert_condition_with_component_origin_scope() {
    let mut model = flat::Model::new();
    model.assert_equations.push(flat::AssertEquation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(generated_var_ref("TD")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        },
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("F or TD has to be positive".to_string()),
            span: test_span(),
        },
        None,
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "line".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.real_parameter_values.insert("TD".to_string(), 0.0);
    ctx.real_parameter_values
        .insert("line.TD".to_string(), 0.001);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let condition = &model.assert_equations[0].condition;
    let condition_uses_scoped_td = match condition {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(true),
            ..
        } => true,
        rumoca_core::Expression::Binary { lhs, .. } => matches!(
            lhs.as_ref(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if (*value - 0.001).abs() < f64::EPSILON
        ),
        _ => false,
    };
    assert!(
        condition_uses_scoped_td,
        "unexpected condition: {condition:?}"
    );
}

#[test]
fn substituted_assert_condition_prefers_evaluated_scalar_over_stale_default() {
    let mut model = flat::Model::new();
    let condition = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(generated_var_ref("F")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(generated_var_ref("TD")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        }),
        span: test_span(),
    };
    model.assert_equations.push(flat::AssertEquation::new(
        condition,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("F or TD has to be positive".to_string()),
            span: test_span(),
        },
        None,
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "line".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("line.F".to_string(), int_literal(0));
    ctx.constant_values
        .insert("line.TD".to_string(), int_literal(0));
    ctx.real_parameter_values.insert("line.F".to_string(), 0.0);
    ctx.real_parameter_values
        .insert("line.TD".to_string(), 0.001);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::Binary { rhs, .. } = &model.assert_equations[0].condition else {
        panic!(
            "unexpected condition: {:?}",
            model.assert_equations[0].condition
        );
    };
    let rumoca_core::Expression::Binary { lhs, .. } = rhs.as_ref() else {
        panic!("unexpected TD comparison: {rhs:?}");
    };
    assert!(matches!(
        lhs.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if (*value - 0.001).abs() < f64::EPSILON
    ));
}

#[test]
fn substitutes_function_scope_constants_inside_defaults_and_body() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        crate::test_support::real_param("u", Vec::new(), test_span()).with_default(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::generated("reference_X"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::generated("reference_X"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let reference_x = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let mut ctx = Context::new();
    ctx.constant_values
        .insert("Pkg.reference_X".to_string(), reference_x.clone());

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    assert_eq!(function.inputs[0].default, Some(reference_x.clone()));
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => {
            assert_eq!(value, &reference_x);
        }
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_record_array_field_projection_from_flat_var_ref() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        crate::test_support::real_param("u", Vec::new(), test_span())
            .with_default(generated_var_ref("ConcreteMedium.data.MM")),
    );
    model.add_function(function);

    let record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("DataRecord"),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("__rumoca_named_arg__.MM"),
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(28.0),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "ConcreteMedium.data".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![record],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );
    ctx.constant_values.insert(
        "ConcreteMedium.data_alias".to_string(),
        generated_var_ref("PartialMedium.data"),
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    let Some(rumoca_core::Expression::Array { elements, .. }) = &function.inputs[0].default else {
        panic!("expected projected array default");
    };
    assert!(matches!(
        elements.as_slice(),
        [rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        }] if (*value - 28.0).abs() < f64::EPSILON
    ));
}

#[test]
fn does_not_substitute_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.g", Span::DUMMY);
    function.add_input(crate::test_support::real_param(
        "k",
        Vec::new(),
        test_span(),
    ));
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("k"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "k".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(7.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.g"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "k"
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_substitute_indexed_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.g_indexed", Span::DUMMY);
    function.add_input(crate::test_support::real_param(
        "table",
        vec![7, 2],
        test_span(),
    ));
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("table"),
            subscripts: vec![
                rumoca_core::Subscript::generated_expr(
                    Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("next"),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rumoca_core::Span::DUMMY,
                ),
                rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
            ],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.g_indexed"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => match value {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                assert_eq!(name.as_str(), "table");
                assert_eq!(subscripts.len(), 2);
            }
            other => panic!("expected table varref, got {other:?}"),
        },
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_inline_multi_indexed_constant_varref_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.h", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::generated(
                "Modelica.Blocks.Sources.IntegerTable.table[1,1]",
            ),
            subscripts: vec![],
            span: test_span(),
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Modelica.Blocks.Sources.IntegerTable.table".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.h"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => match value {
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => {
                assert!(matches!(
                    base.as_ref(),
                    rumoca_core::Expression::Array { elements, is_matrix, .. }
                        if !*is_matrix && elements.len() == 2
                ));
                assert_eq!(subscripts.len(), 2);
                assert!(matches!(
                    &subscripts[0],
                    rumoca_core::Subscript::Expr { expr, .. }
                        if matches!(expr.as_ref(), rumoca_core::Expression::Literal { value: rumoca_core::Literal::Integer(1), .. })
                ));
                assert!(matches!(
                    &subscripts[1],
                    rumoca_core::Subscript::Expr { expr, .. }
                        if matches!(expr.as_ref(), rumoca_core::Expression::Literal { value: rumoca_core::Literal::Integer(1), .. })
                ));
            }
            other => panic!("expected indexed expression, got {other:?}"),
        },
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn rejects_unspanned_inline_indexed_constant_varref_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.unspanned_inline", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::generated("Pkg.table[1]"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Pkg.table".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![int_literal(1)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );

    match substitute_known_constants_in_flat(&mut model, &ctx) {
        Err(FlattenError::MissingSourceContext { reason }) => assert!(
            reason.contains("flatten inline indexed constant"),
            "unexpected reason: {reason}"
        ),
        other => panic!("expected missing-source-context error, got {other:?}"),
    }
}

#[test]
fn inline_indexed_name_uses_structured_scalar_name_parser() {
    assert_eq!(
        split_inline_indexed_name("table[1, 2]"),
        Some(("table", vec![1, 2]))
    );
    assert_eq!(
        split_inline_indexed_name("pkg.table[index.with.dot].value[3]"),
        Some(("pkg.table[index.with.dot].value", vec![3]))
    );
    assert!(split_inline_indexed_name("table").is_none());
    assert!(split_inline_indexed_name("table[1").is_none());
    assert!(split_inline_indexed_name("[1]").is_none());
    assert!(split_inline_indexed_name("table[index.with.dot]").is_none());
}

#[test]
fn does_not_substitute_inline_indexed_varref_when_base_is_local() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.inline_local", Span::DUMMY);
    function.add_input(crate::test_support::real_param(
        "table",
        vec![7, 2],
        test_span(),
    ));
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("table[1,1]"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.inline_local"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "table[1,1]" && subscripts.is_empty()
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_variable_attribute_constants_in_variable_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.medium.X");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("tank.medium.X"))
        .expect("variable should exist")
        .start = Some(generated_var_ref("reference_X"));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "tank.medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("tank.medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let start = model
        .variables
        .get(&rumoca_core::VarName::new("tank.medium.X"))
        .expect("variable should exist")
        .start
        .as_ref()
        .expect("start attribute should remain");
    assert!(!expr_contains_var_ref(start, "reference_X"));
    assert!(!expr_contains_var_ref(start, "nS"));
}

#[test]
fn substitutes_component_equation_constants_in_origin_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.medium.X");
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(generated_var_ref("tank.medium.X")),
            rhs: Box::new(generated_var_ref("reference_X")),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank.medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "tank.medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("tank.medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let residual = &model.equations[0].residual;
    assert!(!expr_contains_var_ref(residual, "reference_X"));
    assert!(!expr_contains_var_ref(residual, "nS"));
}

#[test]
fn substitutes_package_constant_in_structured_template_and_preserves_binder() {
    let table_name = "Pkg.Tables.LogicTable";
    let table_index = rumoca_core::Expression::Index {
        base: Box::new(generated_var_ref(table_name)),
        subscripts: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(generated_var_ref("i")),
            span: test_span(),
        }],
        span: test_span(),
    };
    let origin = flat::EquationOrigin::ComponentEquation {
        component: "gate".to_string(),
    };
    let mut model = flat::Model::new();
    model.add_equation(flat::Equation::new(
        table_index.clone(),
        test_span(),
        origin.clone(),
    ));
    model.add_structured_equation(flat::StructuredEquationFamily {
        domain: rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span: test_span(),
        origin,
        regular: None,
        template: Some(rumoca_core::ComprehensionTemplate {
            body: vec![table_index],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        }),
        interiors_materialized: true,
    });
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        table_name.to_string(),
        rumoca_core::Expression::Array {
            elements: vec![int_literal(11), int_literal(12)],
            is_matrix: false,
            span: test_span(),
        },
    );
    ctx.array_dimensions.insert(table_name.to_string(), vec![2]);

    substitute_known_constants_in_flat(&mut model, &ctx)
        .expect("structured templates fold translation-time package constants");

    let body = &model.structured_equations[0]
        .template
        .as_ref()
        .expect("fixture retains its compact owner")
        .body[0];
    assert!(!expr_contains_var_ref(body, table_name));
    assert!(expr_contains_var_ref(body, "i"));
    assert!(matches!(
        body,
        rumoca_core::Expression::Index { base, .. }
            if matches!(base.as_ref(), rumoca_core::Expression::Array { elements, .. }
                if elements.len() == 2)
    ));
}

#[test]
fn substitutes_fully_qualified_constant_alias_in_declaration_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.X_start");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("tank.X_start"))
        .expect("variable should exist")
        .start = Some(source_var_ref("Pkg.Medium.X_default"));

    let mut ctx = Context::new();
    insert_source_constant(
        &mut ctx,
        "Pkg.Medium.X_default",
        source_var_ref("reference_X"),
    );
    insert_source_constant(&mut ctx, "reference_X", source_reference_x_fill_expr());
    insert_source_constant(&mut ctx, "nS", int_literal(1));

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let start = model
        .variables
        .get(&rumoca_core::VarName::new("tank.X_start"))
        .expect("variable should exist")
        .start
        .as_ref()
        .expect("start attribute should remain");
    assert!(!expr_contains_var_ref(start, "Pkg.Medium.X_default"));
    assert!(!expr_contains_var_ref(start, "reference_X"));
    assert!(!expr_contains_var_ref(start, "nS"));
}

#[test]
fn does_not_substitute_array_shaped_scalar_parameter_ref() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        spanned_var_ref("CriticalDamping.c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.real_parameter_values
        .insert("CriticalDamping.c0".to_string(), 0.0);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "CriticalDamping.c0" && subscripts.is_empty()
    ));
}

#[test]
fn does_not_substitute_scoped_zero_length_array_parameter_ref() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        generated_var_ref("c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.real_parameter_values
        .insert("CriticalDamping.c0".to_string(), 0.0);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "c0" && subscripts.is_empty()
    ));
}

#[test]
fn does_not_substitute_array_shaped_scalar_constant_expr() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        generated_var_ref("c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.constant_values.insert(
        "CriticalDamping.c0".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "c0" && subscripts.is_empty()
    ));
}

#[test]
fn materializes_referenced_zero_sized_array_declaration() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        spanned_var_ref("CriticalDamping.c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let var = model
        .variables
        .get(&rumoca_core::VarName::new("CriticalDamping.c0"))
        .expect("zero-sized referenced array should have a Flat declaration");
    assert_eq!(var.dims, vec![0]);
    assert!(var.is_primitive);
}

#[test]
fn substitutes_field_access_on_zero_arg_constructor_constants() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.k", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(
                    "Modelica.Electrical.Batteries.ParameterRecords.ExampleData",
                ),
                args: vec![],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            }),
            field: "useLinearSOCDependency".to_string(),
            field_def_id: fixture_def_id("useLinearSOCDependency"),
            span: test_span(),
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.boolean_parameter_values.insert(
        "Modelica.Electrical.Batteries.ParameterRecords.ExampleData.useLinearSOCDependency"
            .to_string(),
        false,
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.k"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span
            }
            if *span == test_span()
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_resolve_function_local_record_root_through_constant_alias() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_output(crate::test_support::aggregate_param(
        "g",
        "Common.GibbsDerivs",
        Vec::new(),
        test_span(),
    ));
    function
        .body
        .push(simple_assignment(generated_var_ref("g.tau")));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("g".to_string(), generated_var_ref("Modelica.Constants.g_n"));

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "g.tau"
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

fn expr_contains_var_ref(expr: &rumoca_core::Expression, needle: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name.as_str() == needle,
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var_ref(lhs, needle) || expr_contains_var_ref(rhs, needle)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_contains_var_ref(rhs, needle),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            args.iter().any(|arg| expr_contains_var_ref(arg, needle))
        }
        rumoca_core::Expression::StringConversion { value, format, .. } => {
            expr_contains_var_ref(value, needle)
                || format
                    .operands()
                    .any(|operand| expr_contains_var_ref(operand, needle))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_var_ref(condition, needle) || expr_contains_var_ref(value, needle)
            }) || expr_contains_var_ref(else_branch, needle)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_contains_var_ref(start, needle)
                || step
                    .as_ref()
                    .is_some_and(|step| expr_contains_var_ref(step, needle))
                || expr_contains_var_ref(end, needle)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expr_contains_var_ref(expr, needle)
                || indices
                    .iter()
                    .any(|index| expr_contains_var_ref(&index.range, needle))
                || filter
                    .as_ref()
                    .is_some_and(|filter| expr_contains_var_ref(filter, needle))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expr_contains_var_ref(base, needle)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        expr_contains_var_ref(expr, needle)
                    }
                    rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => {
                        false
                    }
                })
        }
        rumoca_core::Expression::FieldAccess { base, .. } => expr_contains_var_ref(base, needle),
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}

#[test]
fn reports_self_referential_constant_binding_instead_of_recursing() {
    // A short class definition such as `function f = base(arg = arg)` used to
    // leave `f.arg` bound to a reference that resolves straight back to `f.arg`.
    // Folding that binding must produce a spanned diagnostic; before cycle
    // detection it recursed until the stack overflowed.
    let mut ctx = Context::new();
    insert_source_constant(&mut ctx, "a.k", source_var_ref("k"));

    let error = substitute_known_constants_expr(
        source_var_ref("a.k"),
        &ctx,
        &rustc_hash::FxHashSet::default(),
        &HashSet::new(),
        "",
    )
    .expect_err("self-referential constant binding must be reported");

    match &error {
        FlattenError::CyclicConstantBinding { name, cycle, .. } => {
            assert_eq!(name, "a.k");
            assert!(
                cycle.starts_with("a.k -> "),
                "cycle should list the expansion chain, got {cycle}"
            );
        }
        other => panic!("expected CyclicConstantBinding, got {other:?}"),
    }
}

/// `Complex(re = 5e-3, im = 0)` on a record parameter must survive folding.
///
/// The declaration default recorded from the class body (`Complex(1, 0)`) is
/// keyed on the whole-record path `src.Phi`, which is never a flat variable —
/// only its members `src.Phi.re` / `src.Phi.im` are, and those already carry the
/// modification. Folding the default over the reference dropped the modifier
/// (MLS §7.2.4).
#[test]
fn keeps_expanded_record_component_reference_symbolic() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "src.Phi.re");
    add_primitive_variable(&mut model, "src.Phi.im");
    add_primitive_variable(&mut model, "src.port_p.Phi.re");
    add_primitive_variable(&mut model, "src.port_p.Phi.im");
    model.add_equation(flat::Equation::new(
        generated_var_ref("src.Phi"),
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "src".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "src.Phi".to_string(),
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Complex"),
            args: vec![int_literal(1), int_literal(0)],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
    );
    ctx.seed_expanded_component_keys(&model);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert_eq!(model.equations[0].residual, generated_var_ref("src.Phi"));
}

/// A record path with no flat members is still a foldable constant.
///
/// Package-level record constants such as `Medium.data` never appear in the flat
/// variable set, so the guard above must not stop them from folding.
#[test]
fn still_folds_record_constant_without_flat_members() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "src.y");
    let constructor = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Complex"),
        args: vec![int_literal(1), int_literal(0)],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    model.add_equation(flat::Equation::new(
        source_var_ref("Pkg.phasor"),
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "src".to_string(),
        },
    ));

    let mut ctx = Context::new();
    insert_source_constant(&mut ctx, "Pkg.phasor", constructor.clone());
    ctx.seed_expanded_component_keys(&model);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(
        model.equations[0]
            .residual
            .semantically_eq_ignoring_spans(&constructor)
    );
    assert_eq!(model.equations[0].residual.span(), Some(test_span()));
}

/// Field access resolves through the class constant only when the flat model has
/// no variable of that name.
///
/// `medium.state.reference_X` is a medium constant with no flat variable and must
/// still fold, while `medium.state.X` is instantiated and must stay symbolic.
#[test]
fn folds_class_constant_field_but_not_instantiated_member() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "medium.state.X");
    let field = |base: &str, name: &str| rumoca_core::Expression::FieldAccess {
        base: Box::new(generated_var_ref(base)),
        field: name.to_string(),
        field_def_id: fixture_def_id(name),
        span: test_span(),
    };
    model.add_equation(flat::Equation::new(
        field("medium.state", "reference_X"),
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
    ));
    model.add_equation(flat::Equation::new(
        field("medium.state", "X"),
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("medium.state.reference_X".to_string(), int_literal(7));
    ctx.constant_values
        .insert("medium.state.X".to_string(), int_literal(9));
    ctx.seed_expanded_component_keys(&model);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(7),
            ..
        }
    ));
    assert_eq!(model.equations[1].residual, field("medium.state", "X"));
}
