use super::*;

fn reference(name: &str) -> Expression {
    Expression::ComponentReference(component_reference(name))
}

fn component_reference(name: &str) -> rumoca_ir_ast::ComponentReference {
    rumoca_ir_ast::ComponentReference {
        local: false,
        parts: vec![rumoca_ir_ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: name.into(),
                ..rumoca_core::Token::default()
            },
            subs: None,
            def_id: None,
        }],
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
    }
}

fn component_reference_with_id(
    name: &str,
    identity: rumoca_core::DefId,
) -> rumoca_ir_ast::ComponentReference {
    let mut reference = component_reference(name);
    reference.parts[0].def_id = Some(identity);
    reference
}

fn call_with_id(name: &str, identity: rumoca_core::DefId, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall {
        comp: component_reference_with_id(name, identity),
        args,
        is_partial_application: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn integer(value: i64) -> Expression {
    Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: value.to_string().into(),
            ..rumoca_core::Token::default()
        },
        span: rumoca_core::Span::DUMMY,
    }
}

fn scalar_user_function(name: &str, identity: rumoca_core::DefId, arity: usize) -> ClassDef {
    let mut function = ClassDef {
        def_id: Some(identity),
        name: rumoca_core::Token {
            text: name.into(),
            ..rumoca_core::Token::default()
        },
        class_type: ClassType::Function,
        pure: true,
        ..ClassDef::default()
    };
    for index in 0..arity {
        let name = format!("x{index}");
        function.components.insert(
            name.clone(),
            rumoca_ir_ast::Component {
                name,
                causality: Causality::Input(rumoca_core::Token::default()),
                ..rumoca_ir_ast::Component::empty_with_span(rumoca_core::Span::DUMMY)
            },
        );
    }
    function.components.insert(
        "y".to_string(),
        rumoca_ir_ast::Component {
            name: "y".to_string(),
            causality: Causality::Output(rumoca_core::Token::default()),
            ..rumoca_ir_ast::Component::empty_with_span(rumoca_core::Span::DUMMY)
        },
    );
    function
}

#[test]
fn array_literal_requires_uniform_inner_shapes() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let ragged = array(vec![
        array(vec![integer(1), integer(2)]),
        array(vec![integer(3)]),
    ]);
    assert_eq!(infer_dimensions_from_binding(&ragged, &ctx), None);

    let rectangular = array(vec![
        array(vec![integer(1), integer(2)]),
        array(vec![integer(3), integer(4)]),
    ]);
    assert_eq!(
        infer_dimensions_from_binding(&rectangular, &ctx),
        Some(vec![2, 2])
    );
    assert_eq!(
        infer_dims_from_func_with_scope(
            &component_reference("array"),
            &[array(vec![integer(1), integer(2)]), array(vec![integer(3)]),],
            &ctx,
            "",
        ),
        None,
        "the array constructor must enforce the same rectangularity contract"
    );
}

#[test]
fn shape_arithmetic_refuses_usize_overflow() {
    let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
    ctx.dimensions.insert("huge".to_string(), vec![usize::MAX]);
    ctx.dimensions.insert("one".to_string(), vec![1]);
    ctx.dimensions
        .insert("wide".to_string(), vec![usize::MAX, 2]);

    assert_eq!(
        infer_dims_from_func_with_scope(
            &component_reference("cat"),
            &[integer(1), reference("huge"), reference("one")],
            &ctx,
            ""
        ),
        None
    );
    assert_eq!(
        infer_dims_from_func_with_scope(
            &component_reference("vector"),
            &[reference("wide")],
            &ctx,
            ""
        ),
        None
    );
    assert_eq!(
        infer_matrix_row_dims(&[reference("huge"), reference("one")], &ctx, ""),
        None
    );
}

#[test]
fn range_cardinality_handles_extremes_and_refuses_huge_real_counts() {
    assert_eq!(compute_range_len(i64::MIN, 1, i64::MAX), None);
    assert_eq!(compute_range_len(i64::MAX, -1, i64::MIN), None);
    assert_eq!(compute_range_len(i64::MIN, i64::MAX, i64::MAX), Some(3));
    assert_eq!(compute_range_len(i64::MAX, i64::MIN, i64::MIN), Some(2));
    assert_eq!(compute_range_len_real(0.0, 1.0, f64::MAX), None);
    assert_eq!(compute_range_len_real(0.0, 1.0e-15, 2.0e-15), Some(3));
    assert_eq!(compute_range_len_real(1.0e15, 1.0, 1.0e15 + 0.5), Some(1));
}

#[test]
fn array_constructor_shapes_require_predefined_identity() {
    let cases = [
        ("zeros", vec![integer(4)], Some(vec![]), Some(vec![4])),
        (
            "fill",
            vec![integer(9), integer(4)],
            Some(vec![]),
            Some(vec![4]),
        ),
        (
            "cat",
            vec![integer(1), array(vec![integer(1)]), array(vec![integer(2)])],
            None,
            Some(vec![2]),
        ),
    ];
    for (offset, (name, args, user_shape, builtin_shape)) in cases.into_iter().enumerate() {
        let user_id = rumoca_core::DefId::new(600 + offset as u32);
        let builtin_id = rumoca_core::DefId::new(610 + offset as u32);
        let function = scalar_user_function(name, user_id, args.len());
        let mut functions = FxHashMap::default();
        functions.insert(name.to_string(), function);
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.functions = Arc::new(functions);
        ctx.set_predefined_functions([(name.to_string(), builtin_id)]);

        assert_eq!(
            infer_dimensions_from_binding(&call_with_id(name, user_id, args.clone()), &ctx),
            user_shape,
            "a user declaration named {name} must not acquire constructor shape semantics",
        );
        assert_eq!(
            infer_dimensions_from_binding(&call_with_id(name, builtin_id, args.clone()), &ctx),
            builtin_shape,
            "the registered predefined {name} must retain its constructor shape",
        );
        assert_eq!(
            infer_dimensions_from_binding(
                &call_with_id(name, rumoca_core::DefId::new(699), args),
                &ctx,
            ),
            None,
        );
    }
}

fn comprehension(
    body: Expression,
    indices: Vec<rumoca_ir_ast::ForIndex>,
    filter: Option<Expression>,
) -> Expression {
    Expression::ArrayComprehension {
        expr: Arc::new(body),
        indices,
        filter: filter.map(Arc::new),
        span: rumoca_core::Span::DUMMY,
    }
}

fn comprehension_index(name: &str) -> rumoca_ir_ast::ForIndex {
    rumoca_ir_ast::ForIndex {
        ident: rumoca_core::Token {
            text: name.into(),
            ..rumoca_core::Token::default()
        },
        range: Expression::Range {
            start: Arc::new(integer(1)),
            step: None,
            end: Arc::new(integer(3)),
            span: rumoca_core::Span::DUMMY,
        },
    }
}

#[test]
fn comprehension_shape_requires_one_unfiltered_index_and_proven_body_shape() {
    let ctx = TypeCheckEvalContext::for_pre_identity_structural();
    let index = comprehension_index("i");
    assert_eq!(
        infer_dimensions_from_binding(&comprehension(integer(1), vec![index.clone()], None), &ctx,),
        Some(vec![3]),
    );
    assert_eq!(
        infer_dimensions_from_binding(
            &comprehension(reference("unknown"), vec![index.clone()], None),
            &ctx,
        ),
        None,
    );
    assert_eq!(
        infer_dimensions_from_binding(
            &comprehension(
                integer(1),
                vec![index.clone()],
                Some(Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::Bool,
                    token: rumoca_core::Token {
                        text: "true".into(),
                        ..rumoca_core::Token::default()
                    },
                    span: rumoca_core::Span::DUMMY,
                }),
            ),
            &ctx,
        ),
        None,
    );
    assert_eq!(
        infer_dimensions_from_binding(
            &comprehension(integer(1), vec![index, comprehension_index("j")], None),
            &ctx,
        ),
        None,
    );
}
