use super::*;
use std::sync::Arc;

fn test_span() -> Span {
    span_at(1, 2)
}

fn span_at(start: usize, end: usize) -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("ast_lower_test.mo"),
        start,
        end,
    )
}

fn test_def_id(name: &str) -> DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    DefId::new(hash.max(1))
}

fn part(name: &str) -> ast::ComponentRefPart {
    ast::ComponentRefPart {
        ident: rumoca_core::Token {
            text: Arc::from(name),
            ..rumoca_core::Token::default()
        },
        subs: None,
        def_id: Some(test_def_id(name)),
    }
}

fn component_ref(names: &[&str]) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: names.iter().map(|name| part(name)).collect(),
        span: test_span(),
        qualified_display_name: None,
    }
}

fn ast_var(name: &str) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![part(name)],
        span: test_span(),
        qualified_display_name: None,
    })
}

fn ast_var_with_span(name: &str, span: Span) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![part(name)],
        span,
        qualified_display_name: None,
    })
}

#[test]
fn direct_statement_lowering_rejects_empty_recovery_node_at_owner_span() {
    let span = span_at(17, 23);
    let error = statement_from_ast_with_span(
        &ast::Statement::Empty,
        LoweringContext::default(),
        None,
        span,
    )
    .expect_err("direct statement lowering must not preserve Statement::Empty");

    assert!(matches!(
        error,
        crate::FlattenError::InvalidAstRecovery {
            span: error_span,
            ..
        } if error_span == span
    ));
}

#[test]
fn direct_expression_lowering_rejects_recovery_nodes_at_their_spans() {
    let span = span_at(31, 37);
    let terminal = || ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: Arc::from("1"),
            ..rumoca_core::Token::default()
        },
        span,
    };
    let invalid = [
        ast::Expression::Empty { span },
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Empty,
            rhs: Arc::new(terminal()),
            span,
        },
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Empty,
            lhs: Arc::new(terminal()),
            rhs: Arc::new(terminal()),
            span,
        },
    ];

    for expression in invalid {
        let error = expression_from_ast(&expression)
            .expect_err("direct expression lowering must reject recovery syntax");
        assert!(matches!(
            error,
            crate::FlattenError::InvalidAstRecovery {
                span: error_span,
                ..
            } if error_span == span
        ));
    }
}

#[test]
fn direct_expression_lowering_rejects_context_only_call_carriers() {
    let span = span_at(41, 49);
    let value = ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: Arc::from("1"),
            ..rumoca_core::Token::default()
        },
        span,
    };
    let invalid = [
        ast::Expression::NamedArgument {
            name: rumoca_core::Token::default(),
            value: Arc::new(value.clone()),
            span,
        },
        ast::Expression::Modification {
            target: component_ref(&["x"]),
            value: Some(Arc::new(value)),
            span,
        },
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs: Arc::new(ast_var("x")),
            rhs: Arc::new(ast_var("y")),
            span,
        },
    ];

    for expression in invalid {
        let error = expression_from_ast(&expression)
            .expect_err("a context-only carrier must not silently become only its value");
        assert!(matches!(
            error,
            crate::FlattenError::InvalidAstRecovery {
                span: error_span,
                ..
            } if error_span == span
        ));
    }
}

fn function_ref(name: &str) -> ast::ComponentReference {
    component_ref(&[name])
}

fn resolved_function_ref(name: &str, target: DefId) -> ast::ComponentReference {
    let mut reference = function_ref(name);
    reference.set_target_def_id(Some(target));
    reference
}

fn integer(value: i64, span: Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: Arc::from(value.to_string()),
            ..rumoca_core::Token::default()
        },
        span,
    }
}

fn string(value: &str, span: Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::String,
        token: rumoca_core::Token {
            text: Arc::from(format!("\"{value}\"")),
            ..rumoca_core::Token::default()
        },
        span,
    }
}

#[test]
fn scalar_lowering_preserves_identity_and_each_source_span() {
    let reference_span = span_at(3, 4);
    let literal_span = span_at(7, 9);
    let binary_span = span_at(3, 9);
    let reference = ast_var_with_span("x", reference_span);
    let expected_id = match &reference {
        ast::Expression::ComponentReference(reference) => reference.target_def_id().unwrap(),
        _ => unreachable!(),
    };
    let expression = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Arc::new(reference),
        rhs: Arc::new(integer(2, literal_span)),
        span: binary_span,
    };

    let lowered = expression_from_ast(&expression).unwrap();
    let rumoca_core::Expression::Binary { lhs, rhs, span, .. } = lowered else {
        panic!("expected binary expression");
    };
    assert_eq!(span, binary_span);
    let rumoca_core::Expression::VarRef { name, span, .. } = lhs.as_ref() else {
        panic!("expected exact variable reference");
    };
    assert_eq!(*span, reference_span);
    assert_eq!(
        name.component_ref().map(ComponentReference::target_def_id),
        Some(expected_id)
    );
    assert!(matches!(
        rhs.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            span,
        } if *span == literal_span
    ));
}

#[test]
fn empty_terminal_rejects_with_source_span() {
    let span = test_span();
    let error = convert_terminal(
        &ast::TerminalType::Empty,
        &rumoca_core::Token::default(),
        span,
    )
    .expect_err("an empty terminal is not a numeric literal");

    assert!(matches!(
        error,
        FlattenError::InvalidLiteralTerminal {
            span: error_span,
            ..
        } if error_span == span
    ));

    let token = rumoca_core::Token {
        text: Arc::from("7"),
        ..rumoca_core::Token::default()
    };
    assert_eq!(
        convert_terminal(&ast::TerminalType::UnsignedInteger, &token, span)
            .expect("an unsigned integer terminal remains a legal literal"),
        rumoca_core::Literal::Integer(7)
    );
}

#[test]
fn derivative_lowering_is_structurally_discoverable() {
    let derivative_span = span_at(10, 16);
    let call = ast::Expression::DerivativeCall {
        args: vec![ast_var_with_span("x", span_at(14, 15))],
        span: derivative_span,
    };

    let lowered = expression_from_ast(&call).unwrap();
    assert_eq!(lowered.span(), Some(derivative_span));
    assert!(lowered.contains_subexpression(|expression| matches!(
        expression,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            ..
        } if matches!(args.first(), Some(rumoca_core::Expression::VarRef { name, .. }) if name.as_str() == "x")
    )));
    let mut states = Vec::new();
    lowered.collect_state_variables(&mut states);
    assert_eq!(states, vec![rumoca_core::VarName::new("x")]);
}

#[test]
fn ordinary_function_reference_spelled_der_cannot_mint_derivative_identity() {
    let call = ast::Expression::FunctionCall {
        comp: function_ref("der"),
        args: vec![ast_var_with_span("x", span_at(14, 15))],
        is_partial_application: false,
        span: span_at(10, 16),
    };

    let lowered = expression_from_ast(&call).expect("ordinary calls remain ordinary calls");
    assert!(matches!(
        lowered,
        rumoca_core::Expression::FunctionCall { .. }
    ));
}

#[test]
fn constructor_lowering_requires_identity_and_preserves_named_argument_span() {
    let constructor_span = span_at(20, 42);
    let argument_span = span_at(31, 41);
    let mut target = component_ref(&["Alias", "Record"]);
    let target_id = DefId::new(77);
    target.set_target_def_id(Some(target_id));
    target.span = constructor_span;
    target.set_qualified_display_name("Pkg.Record");
    let expression = ast::Expression::ClassModification {
        target,
        modifications: vec![ast::Expression::NamedArgument {
            name: rumoca_core::Token {
                text: Arc::from("value"),
                ..rumoca_core::Token::default()
            },
            value: Arc::new(integer(3, span_at(39, 40))),
            span: argument_span,
        }],
        each_flags: vec![false],
        final_flags: vec![false],
        redeclare_flags: vec![false],
        span: constructor_span,
    };

    let lowered = expression_from_ast(&expression).unwrap();
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        span,
        ..
    } = lowered
    else {
        panic!("expected constructor call");
    };
    assert!(is_constructor);
    assert_eq!(span, constructor_span);
    assert_eq!(name.as_str(), "Pkg.Record");
    assert_eq!(
        name.component_ref().map(ComponentReference::target_def_id),
        Some(target_id)
    );
    let [
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span,
        },
    ] = args.as_slice()
    else {
        panic!("expected generated named-argument wrapper");
    };
    assert_eq!(
        name.as_str(),
        format!("{}value", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    );
    assert!(name.is_generated());
    assert_eq!(*span, argument_span);
    assert!(matches!(
        args.as_slice(),
        [rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            ..
        }]
    ));

    let mut missing = component_ref(&["Missing"]);
    missing.set_target_def_id(None);
    let error = expression_from_ast(&ast::Expression::ClassModification {
        target: missing,
        modifications: Vec::new(),
        each_flags: Vec::new(),
        final_flags: Vec::new(),
        redeclare_flags: Vec::new(),
        span: constructor_span,
    })
    .unwrap_err();
    assert!(matches!(
        error,
        FlattenError::MissingFlatVariableIdentity { .. }
    ));

    let error = expression_from_ast(&ast::Expression::ComponentReference(
        ast::ComponentReference {
            local: false,
            parts: Vec::new(),
            span: constructor_span,
            qualified_display_name: None,
        },
    ))
    .unwrap_err();
    assert!(matches!(
        error,
        FlattenError::MissingFlatVariableIdentity { ref name, span }
            if name.contains("requires at least one identity-bearing part")
                && span == constructor_span
    ));
}

#[test]
fn comprehension_lowering_preserves_structure_and_owner_provenance() {
    let owner_span = span_at(50, 80);
    let body_span = span_at(51, 52);
    let range_span = span_at(60, 64);
    let filter_span = span_at(70, 75);
    let expression = ast::Expression::ArrayComprehension {
        expr: Arc::new(ast_var_with_span("x", body_span)),
        indices: vec![ast::ForIndex {
            ident: rumoca_core::Token {
                text: Arc::from("i"),
                ..rumoca_core::Token::default()
            },
            range: ast::Expression::Range {
                start: Arc::new(integer(1, span_at(60, 61))),
                step: None,
                end: Arc::new(integer(4, span_at(63, 64))),
                span: range_span,
            },
        }],
        filter: Some(Arc::new(ast_var_with_span("enabled", filter_span))),
        span: owner_span,
    };

    let lowered = expression_from_ast(&expression).unwrap();
    let rumoca_core::Expression::ArrayComprehension {
        expr,
        indices,
        filter,
        span,
    } = lowered
    else {
        panic!("expected array comprehension");
    };
    assert_eq!(span, owner_span);
    assert_eq!(expr.span(), Some(body_span));
    assert_eq!(indices.len(), 1);
    assert_eq!(indices[0].name, "i");
    assert_eq!(indices[0].range.span(), Some(range_span));
    assert_eq!(filter.expect("filter").span(), Some(filter_span));
}

#[test]
fn subscript_lowering_folds_arithmetic_and_retains_dynamic_identity() {
    let arithmetic = ast::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Arc::new(integer(2, span_at(82, 83))),
        rhs: Arc::new(integer(3, span_at(84, 85))),
        span: span_at(82, 85),
    };
    let i = ast_var_with_span("i", span_at(87, 88));
    let j = ast_var_with_span("j", span_at(90, 91));
    let expected_dynamic_ids = [&i, &j].map(|expression| match expression {
        ast::Expression::ComponentReference(reference) => reference.target_def_id().unwrap(),
        _ => unreachable!(),
    });
    let mut indexed = part("a");
    indexed.subs = Some(vec![
        ast::Subscript::Expression(arithmetic),
        ast::Subscript::Expression(i),
        ast::Subscript::Expression(j),
    ]);
    let expression = ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![indexed],
        span: span_at(81, 92),
        qualified_display_name: None,
    });

    let lowered = expression_from_ast(&expression).unwrap();
    let rumoca_core::Expression::Index { subscripts, .. } = lowered else {
        panic!("expected indexed reference");
    };
    assert!(matches!(
        subscripts.first(),
        Some(rumoca_core::Subscript::Index { value: 5, .. })
    ));
    for (subscript, expected_id) in subscripts[1..].iter().zip(expected_dynamic_ids) {
        let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
            panic!("expected dynamic subscript");
        };
        let rumoca_core::Expression::VarRef { name, .. } = expr.as_ref() else {
            panic!("expected dynamic exact reference");
        };
        assert_eq!(
            name.component_ref().map(ComponentReference::target_def_id),
            Some(expected_id)
        );
    }
}

#[test]
fn interval_requires_the_exact_predefined_declaration_identity() {
    let predefined_interval = DefId::new(40);
    let shadowed_interval = DefId::new(41);
    let mut identities = [None; rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED.len()];
    let interval = rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED
        .iter()
        .position(|builtin| *builtin == rumoca_core::BuiltinFunction::Interval)
        .expect("Interval requires predefined identity");
    identities[interval] = Some(predefined_interval);
    let context = LoweringContext {
        predefined_intrinsics: PredefinedIntrinsicIds {
            identities,
            assertion: None,
        },
        ..LoweringContext::default()
    };

    let predefined = convert_function_call_with_context(
        &resolved_function_ref("interval", predefined_interval),
        &[ast_var("u")],
        rumoca_core::FunctionCallKind::Invocation,
        test_span(),
        context,
    )
    .unwrap();
    assert!(matches!(
        predefined,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Interval,
            ..
        }
    ));

    let shadowed = convert_function_call_with_context(
        &resolved_function_ref("interval", shadowed_interval),
        &[ast_var("u")],
        rumoca_core::FunctionCallKind::Invocation,
        test_span(),
        context,
    )
    .unwrap();
    assert!(matches!(
        shadowed,
        rumoca_core::Expression::FunctionCall { .. }
    ));

    let mut indexed = part("a");
    indexed.subs = Some(vec![ast::Subscript::Expression(
        ast::Expression::FunctionCall {
            comp: resolved_function_ref("interval", predefined_interval),
            args: vec![ast_var("u")],
            is_partial_application: false,
            span: test_span(),
        },
    )]);
    let indexed = expression_from_component_ref_with_context(
        &ast::ComponentReference {
            local: false,
            parts: vec![indexed],
            span: test_span(),
            qualified_display_name: None,
        },
        context,
    )
    .unwrap();
    let rumoca_core::Expression::Index { subscripts, .. } = indexed else {
        panic!("expected indexed expression");
    };
    assert!(matches!(
        &subscripts[0],
        rumoca_core::Subscript::Expr { expr, .. }
            if matches!(
                expr.as_ref(),
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Interval,
                    ..
                }
            )
    ));
}

#[test]
fn resolved_product_declaration_is_not_lowered_as_reduction_builtin() {
    let product = DefId::new(42);
    let lowered = convert_function_call_with_context(
        &resolved_function_ref("product", product),
        &[ast_var("left"), ast_var("right")],
        rumoca_core::FunctionCallKind::Invocation,
        test_span(),
        LoweringContext::default(),
    )
    .expect("resolved package-local product is a user function");

    assert!(matches!(
        lowered,
        rumoca_core::Expression::FunctionCall { name, args, .. }
            if name.target_def_id() == Some(product) && args.len() == 2
    ));
}

#[test]
fn algorithm_assert_requires_the_exact_predefined_declaration_identity() {
    let predefined_assert = DefId::new(50);
    let shadowed_assert = DefId::new(51);
    let context = LoweringContext {
        predefined_intrinsics: PredefinedIntrinsicIds {
            assertion: Some(predefined_assert),
            ..PredefinedIntrinsicIds::default()
        },
        ..LoweringContext::default()
    };
    let statement = |target| ast::Statement::FunctionCall {
        comp: resolved_function_ref("assert", target),
        args: vec![
            ast_var("condition"),
            string("assertion message", test_span()),
        ],
        outputs: Vec::new(),
    };

    let predefined = statement_from_ast_with_context_and_source_map(
        &statement(predefined_assert),
        context,
        None,
    )
    .expect("predefined assert lowers as a typed statement");
    assert!(matches!(predefined, rumoca_core::Statement::Assert { .. }));

    let shadowed =
        statement_from_ast_with_context_and_source_map(&statement(shadowed_assert), context, None)
            .expect("same-spelling user call remains an ordinary call");
    assert!(matches!(
        shadowed,
        rumoca_core::Statement::FunctionCall { ref outputs, .. } if outputs.is_empty()
    ));
}

#[test]
fn function_call_output_lowering_preserves_legal_slots_and_rejects_other_expressions() {
    let call_span = span_at(70, 92);
    let omitted_span = span_at(71, 72);
    let target_span = span_at(74, 75);
    let invalid_span = span_at(77, 82);
    let mut target = component_ref(&["y"]);
    target.span = target_span;

    let lowered = lower_function_call_statement(
        &function_ref("f"),
        &[],
        &[
            ast::Expression::Empty { span: omitted_span },
            ast::Expression::ComponentReference(target.clone()),
        ],
        LoweringContext::default(),
        call_span,
    )
    .expect("omitted and component-reference output slots are legal");
    assert!(matches!(
        lowered,
        rumoca_core::Statement::FunctionCall { outputs, .. }
            if matches!(outputs.as_slice(), [None, Some(reference)] if Some(reference.target_def_id()) == target.target_def_id())
    ));

    let invalid = [
        integer(1, invalid_span),
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Arc::new(integer(1, invalid_span)),
            rhs: Arc::new(integer(2, invalid_span)),
            span: invalid_span,
        },
    ];
    for output in invalid {
        let error = lower_function_call_statement(
            &function_ref("f"),
            &[],
            &[output],
            LoweringContext::default(),
            call_span,
        )
        .expect_err("non-reference output expressions must not become omitted slots");
        assert!(matches!(
            error,
            crate::FlattenError::InvalidFunctionCallOutput {
                span: error_span,
                ..
            } if error_span == invalid_span
        ));
    }

    let dummy_output = integer(1, Span::DUMMY);
    let owner_error = lower_function_call_statement(
        &function_ref("f"),
        &[],
        std::slice::from_ref(&dummy_output),
        LoweringContext::default(),
        call_span,
    )
    .expect_err("a valid call owner supplies missing child provenance");
    assert!(matches!(
        owner_error,
        crate::FlattenError::InvalidFunctionCallOutput { span, .. } if span == call_span
    ));

    let missing = lower_function_call_statement(
        &function_ref("f"),
        &[],
        &[dummy_output],
        LoweringContext::default(),
        Span::DUMMY,
    )
    .expect_err("a diagnostic cannot be minted with dummy child and owner spans");
    assert!(matches!(
        missing,
        crate::FlattenError::MissingSourceContext { .. }
    ));
}

#[test]
fn multi_output_call_statement_projects_named_actuals_to_markers() {
    // A multi-output tuple assignment such as `(q, r) := h2(a = p, b = 2)`
    // carries the same positional-and-named actual layout as an expression-position
    // call. Its actuals must pass through the shared named-argument marker projection
    // so no `Expression::NamedArgument` survives into Flat construction, where the
    // construction boundary rejects it unconditionally. This guards the tuple-call
    // lowering path against regressing back to a plain-expression conversion, which
    // stranded the named marker and produced an invalid-AST-recovery refusal.
    let call_span = span_at(70, 92);
    let named_a_span = span_at(72, 77);
    let named_b_span = span_at(79, 84);
    let named = |text: &str, value: ast::Expression, span: Span| ast::Expression::NamedArgument {
        name: rumoca_core::Token {
            text: Arc::from(text),
            ..rumoca_core::Token::default()
        },
        value: Arc::new(value),
        span,
    };

    let named_form = lower_function_call_statement(
        &function_ref("h2"),
        &[
            named("a", ast_var("p"), named_a_span),
            named("b", integer(2, named_b_span), named_b_span),
        ],
        &[
            ast::Expression::ComponentReference(component_ref(&["q"])),
            ast::Expression::ComponentReference(component_ref(&["r"])),
        ],
        LoweringContext::default(),
        call_span,
    )
    .expect("named actuals in a multi-output call statement must lower cleanly");

    let rumoca_core::Statement::FunctionCall { args, outputs, .. } = named_form else {
        panic!("expected a lowered function-call statement");
    };
    // Both tuple output slots survive; a multi-output call is not a scalar assignment.
    assert_eq!(outputs.len(), 2);
    // Each named actual is projected to a generated marker rather than an
    // `Expression::NamedArgument`; the marker name carries the shared prefix and the
    // original argument span so slot resolution can report against the source.
    let [
        rumoca_core::Expression::FunctionCall {
            name: name_a,
            span: span_a,
            ..
        },
        rumoca_core::Expression::FunctionCall {
            name: name_b,
            span: span_b,
            ..
        },
    ] = args.as_slice()
    else {
        panic!("expected two generated named-argument markers");
    };
    assert_eq!(
        name_a.as_str(),
        format!("{}a", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    );
    assert!(name_a.is_generated());
    assert_eq!(*span_a, named_a_span);
    assert_eq!(
        name_b.as_str(),
        format!("{}b", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    );
    assert!(name_b.is_generated());
    assert_eq!(*span_b, named_b_span);
}

#[test]
fn multi_output_call_statement_keeps_positional_actuals_unwrapped() {
    // The positional spelling `(q, r) := h2(p, 2)` is the differential partner of the
    // named form: its actuals must lower to plain expressions with no marker wrapping,
    // so that after slot resolution the two spellings resolve to the same argument
    // vector. A marker minted for a positional actual would corrupt that equivalence.
    let call_span = span_at(70, 92);
    let positional_form = lower_function_call_statement(
        &function_ref("h2"),
        &[ast_var("p"), integer(2, span_at(79, 80))],
        &[
            ast::Expression::ComponentReference(component_ref(&["q"])),
            ast::Expression::ComponentReference(component_ref(&["r"])),
        ],
        LoweringContext::default(),
        call_span,
    )
    .expect("positional actuals in a multi-output call statement must lower cleanly");

    let rumoca_core::Statement::FunctionCall { args, .. } = positional_form else {
        panic!("expected a lowered function-call statement");
    };
    // A positional component reference stays a variable reference and a positional
    // literal stays a literal; neither is disguised as a named-argument marker.
    assert!(matches!(
        args.as_slice(),
        [
            rumoca_core::Expression::VarRef { .. },
            rumoca_core::Expression::Literal { .. },
        ]
    ));
}

#[test]
fn unresolved_interval_spelling_never_mints_a_predefined_intrinsic() {
    let lowered = convert_function_call_with_context(
        &function_ref("interval"),
        &[ast_var("u")],
        rumoca_core::FunctionCallKind::Invocation,
        test_span(),
        LoweringContext::default(),
    )
    .unwrap();

    assert!(matches!(
        lowered,
        rumoca_core::Expression::FunctionCall { .. }
    ));
}

#[test]
fn get_instance_name_lowers_to_instance_string_literal() {
    let expr = convert_function_call_with_context(
        &function_ref("getInstanceName"),
        &[],
        rumoca_core::FunctionCallKind::Invocation,
        test_span(),
        LoweringContext {
            instance_name: Some("Vehicle.engine.controller"),
            predefined_string_declaration: None,
            predefined_intrinsics: PredefinedIntrinsicIds::default(),
        },
    )
    .unwrap();

    let rumoca_core::Expression::Literal { value, .. } = expr else {
        panic!("expected literal");
    };
    assert_eq!(
        value,
        rumoca_core::Literal::String("Vehicle.engine.controller".to_string())
    );
}

#[test]
fn function_call_lowering_preserves_the_ast_call_span() {
    let call_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("ast_lower_test.mo"),
        10,
        24,
    );
    let expression = ast::Expression::FunctionCall {
        comp: function_ref("previous"),
        args: vec![ast_var("x")],
        is_partial_application: false,
        span: call_span,
    };
    let lowered = expression_from_ast(&expression).unwrap();
    assert_eq!(lowered.span(), Some(call_span));
}

#[test]
fn function_call_lowering_preserves_invocation_vs_partial_application_identity() {
    let named_argument = ast::Expression::NamedArgument {
        name: rumoca_core::Token {
            text: Arc::from("gain"),
            ..Default::default()
        },
        value: Arc::new(integer(2, span_at(20, 21))),
        span: span_at(13, 21),
    };
    let lower = |is_partial_application| {
        expression_from_ast(&ast::Expression::FunctionCall {
            comp: function_ref("scale"),
            args: vec![named_argument.clone()],
            is_partial_application,
            span: span_at(5, 22),
        })
        .expect("resolved call syntax lowers")
    };

    let rumoca_core::Expression::FunctionCall {
        call_kind: invocation_kind,
        args: invocation_args,
        ..
    } = lower(false)
    else {
        panic!("expected ordinary invocation");
    };
    assert_eq!(
        invocation_kind,
        rumoca_core::FunctionCallKind::Invocation,
        "ordinary named arguments do not imply a function value"
    );
    assert!(matches!(
        invocation_args.as_slice(),
        [rumoca_core::Expression::FunctionCall {
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            ..
        }]
    ));

    let rumoca_core::Expression::FunctionCall {
        call_kind: partial_kind,
        ..
    } = lower(true)
    else {
        panic!("expected partial application");
    };
    assert_eq!(
        partial_kind,
        rumoca_core::FunctionCallKind::PartialApplication
    );
}

#[test]
fn get_instance_name_requires_instance_scope() {
    let err = convert_function_call_with_context(
        &function_ref("getInstanceName"),
        &[],
        rumoca_core::FunctionCallKind::Invocation,
        test_span(),
        LoweringContext::default(),
    )
    .unwrap_err();

    assert!(
        err.to_string()
            .contains("requires a model/block instance scope")
    );
}

#[test]
fn get_instance_name_rejects_arguments() {
    let err = convert_function_call_with_context(
        &function_ref("getInstanceName"),
        &[ast_var("x")],
        rumoca_core::FunctionCallKind::Invocation,
        test_span(),
        LoweringContext {
            instance_name: Some("Vehicle.engine.controller"),
            predefined_string_declaration: None,
            predefined_intrinsics: PredefinedIntrinsicIds::default(),
        },
    )
    .unwrap_err();

    assert!(err.to_string().contains("takes no arguments"));
}

#[test]
fn function_call_lowering_keeps_exact_member_path_and_identity() {
    let receiver_def = DefId::new(1);
    let member_def = DefId::new(2);
    let mut comp = component_ref(&["receiver", "member"]);
    comp.set_root_def_id(Some(receiver_def));
    comp.set_target_def_id(Some(member_def));

    let expr = convert_function_call(&comp, &[]).unwrap();
    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "receiver.member");
    let reference = name.component_ref().expect("structured function reference");
    assert_eq!(reference.root_def_id(), receiver_def);
    assert_eq!(reference.target_def_id(), member_def);
}

#[test]
fn function_call_display_name_does_not_replace_structured_identity() {
    let function_def = DefId::new(2);
    let mut comp = component_ref(&["Receiver", "member"]);
    comp.set_target_def_id(Some(function_def));
    comp.set_qualified_display_name("Pkg.Receiver.member");

    let expr = convert_function_call(&comp, &[]).unwrap();
    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(name.as_str(), "Pkg.Receiver.member");
    let reference = name.component_ref().expect("structured function reference");
    assert_eq!(reference.target_def_id(), function_def);
    assert_eq!(reference.parts()[0].ident.as_str(), "Receiver");
}

#[test]
fn function_call_qualified_display_preserves_use_site_parts() {
    let function_def = DefId::new(4);
    let mut comp = component_ref(&["world", "gravityAcceleration"]);
    comp.set_target_def_id(Some(function_def));
    comp.set_qualified_display_name("Modelica.Mechanics.MultiBody.World.gravityAcceleration");

    let expr = convert_function_call(&comp, &[]).unwrap();
    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Mechanics.MultiBody.World.gravityAcceleration"
    );
    let reference = name.component_ref().expect("structured function reference");
    assert_eq!(reference.parts()[0].ident.as_str(), "world");
    assert_eq!(reference.target_def_id(), function_def);
}

#[test]
fn statement_lowering_uses_ast_assignment_span_without_source_map() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("algorithm_assignment_span.mo"),
        8,
        14,
    );
    let stmt = ast::Statement::Assignment {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![part("x")],
            span,
            qualified_display_name: None,
        },
        value: ast_var("y"),
    };

    let lowered = statement_from_ast(&stmt).unwrap();
    assert_eq!(lowered.source_span(), Some(span));
}

#[test]
fn statement_lowering_uses_reference_span_when_prefix_token_location_is_generated() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("algorithm_prefixed_assignment_span.mo"),
        8,
        14,
    );
    let stmt = ast::Statement::Assignment {
        comp: ast::ComponentReference {
            local: false,
            parts: vec![part("Model"), part("x")],
            span,
            qualified_display_name: None,
        },
        value: ast_var_with_span("y", span),
    };

    let lowered = statement_from_ast(&stmt).unwrap();
    assert_eq!(lowered.source_span(), Some(span));
}

#[test]
fn statement_lowering_uses_ast_if_condition_span_without_source_map() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("algorithm_if_span.mo"),
        3,
        12,
    );
    let stmt = ast::Statement::If {
        cond_blocks: vec![ast::StatementBlock {
            cond: ast_var_with_span("condition", span),
            stmts: Vec::new(),
        }],
        else_block: None,
    };

    let lowered = statement_from_ast(&stmt).unwrap();
    assert_eq!(lowered.source_span(), Some(span));
}

#[test]
fn function_call_lowering_keeps_concrete_path_when_def_id_names_constraint() {
    let partial_function_def = DefId::new(3);
    let mut comp = component_ref(&[
        "Modelica",
        "Media",
        "Air",
        "ReferenceAir",
        "Air_pT",
        "specificEnthalpy",
    ]);
    comp.set_target_def_id(Some(partial_function_def));

    let expr = convert_function_call(&comp, &[]).unwrap();
    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Media.Air.ReferenceAir.Air_pT.specificEnthalpy"
    );
}

#[test]
fn dynamic_final_subscript_keeps_local_index_base() {
    let variable_def = DefId::new(3);
    let comp = ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from("leg_v_b"),
                ..rumoca_core::Token::default()
            },
            subs: Some(vec![
                ast::Subscript::Range {
                    token: rumoca_core::Token {
                        text: Arc::from(":"),
                        ..rumoca_core::Token::default()
                    },
                },
                ast::Subscript::Expression(ast_var("i")),
            ]),
            def_id: Some(variable_def),
        }],
        span: test_span(),
        qualified_display_name: None,
    };

    let expr =
        expression_from_component_ref_with_context(&comp, LoweringContext::default()).unwrap();
    let rumoca_core::Expression::Index {
        base, subscripts, ..
    } = expr
    else {
        panic!("expected dynamic final subscript to lower as structured index");
    };
    let rumoca_core::Expression::VarRef { name, .. } = base.as_ref() else {
        panic!("expected indexed base to remain a variable reference");
    };

    assert_eq!(name.as_str(), "leg_v_b");
    assert_eq!(subscripts.len(), 2);
}

#[test]
fn empty_component_subscript_rejects_at_component_span() {
    let owner_span = span_at(40, 49);
    let comp = ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from("value"),
                ..rumoca_core::Token::default()
            },
            subs: Some(vec![ast::Subscript::Empty]),
            def_id: Some(DefId::new(3)),
        }],
        span: owner_span,
        qualified_display_name: None,
    };

    let error = expression_from_component_ref_with_context(&comp, LoweringContext::default())
        .expect_err("a recovery subscript cannot become a whole-dimension selector");
    assert!(matches!(
        error,
        FlattenError::InvalidAstSubscript { span, .. } if span == owner_span
    ));
}

#[test]
fn end_subscript_lowers_to_size_of_selected_base_dimension() {
    let mut indexed = part("v");
    indexed.subs = Some(vec![ast::Subscript::Expression(
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            token: rumoca_core::Token {
                text: Arc::from("end"),
                ..rumoca_core::Token::default()
            },
            span: test_span(),
        },
    )]);
    let comp = ast::ComponentReference {
        local: false,
        parts: vec![indexed],
        span: test_span(),
        qualified_display_name: None,
    };

    let lowered = expression_from_component_ref_with_context(&comp, LoweringContext::default())
        .expect("end should lower in a valid subscript context");
    let rumoca_core::Expression::Index {
        base, subscripts, ..
    } = lowered
    else {
        panic!("expected indexed expression");
    };
    let [rumoca_core::Subscript::Expr { expr, .. }] = subscripts.as_slice() else {
        panic!("expected expression subscript");
    };
    let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args,
        ..
    } = expr.as_ref()
    else {
        panic!("end should become size(base, dimension)");
    };
    assert_eq!(args[0], *base);
    assert!(matches!(
        args[1],
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            ..
        }
    ));
}

#[test]
fn end_outside_subscript_is_rejected_instead_of_becoming_zero() {
    let expr = ast::Expression::Terminal {
        terminal_type: ast::TerminalType::End,
        token: rumoca_core::Token {
            text: Arc::from("end"),
            ..rumoca_core::Token::default()
        },
        span: test_span(),
    };

    let err = expression_from_ast(&expr).expect_err("bare end is invalid");
    assert!(
        err.to_string()
            .contains("only valid inside an array subscript"),
        "unexpected error: {err}"
    );
}

#[test]
fn structured_subscript_base_carries_exact_final_target_for_flat_projection() {
    let mut fluid_constants = part("fluidConstants");
    let fluid_constants_def_id = fluid_constants.def_id.unwrap();
    fluid_constants.subs = Some(vec![ast::Subscript::Expression(
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: Arc::from("1"),
                ..rumoca_core::Token::default()
            },
            span: test_span(),
        },
    )]);
    let mut comp = ast::ComponentReference {
        local: false,
        parts: vec![
            part("source"),
            part("medium"),
            fluid_constants,
            part("criticalTemperature"),
        ],
        span: test_span(),
        qualified_display_name: None,
    };
    comp.set_target_def_id(Some(DefId::new(77)));

    let expr =
        expression_from_component_ref_with_context(&comp, LoweringContext::default()).unwrap();
    let rumoca_core::Expression::FieldAccess { base, .. } = expr else {
        panic!("expected field access after indexed package constant");
    };
    let rumoca_core::Expression::Index { base, .. } = base.as_ref() else {
        panic!("expected indexed package constant base");
    };
    let rumoca_core::Expression::VarRef { name, .. } = base.as_ref() else {
        panic!("expected indexed base to be a variable reference");
    };

    assert_eq!(name.as_str(), "source.medium.fluidConstants");
    assert_eq!(name.target_def_id(), Some(fluid_constants_def_id));
}

fn named_arg(name: &str, value: ast::Expression, span: Span) -> ast::Expression {
    ast::Expression::NamedArgument {
        name: rumoca_core::Token {
            text: Arc::from(name),
            ..rumoca_core::Token::default()
        },
        value: Arc::new(value),
        span,
    }
}

fn builtin_call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
    ast::Expression::FunctionCall {
        comp: function_ref(name),
        args,
        is_partial_application: false,
        span: test_span(),
    }
}

fn homotopy_positional_args(expression: &rumoca_core::Expression) -> &[rumoca_core::Expression] {
    match expression {
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Homotopy,
            args,
            ..
        } => args,
        other => panic!("expected a homotopy BuiltinCall, found {other:?}"),
    }
}

#[test]
fn homotopy_named_arguments_lower_to_positional_vector() {
    let named = expression_from_ast(&builtin_call(
        "homotopy",
        vec![
            named_arg("actual", ast_var("a"), test_span()),
            named_arg("simplified", ast_var("b"), test_span()),
        ],
    ))
    .expect("named homotopy actuals lower cleanly");
    let positional =
        expression_from_ast(&builtin_call("homotopy", vec![ast_var("a"), ast_var("b")]))
            .expect("positional homotopy actuals lower cleanly");

    assert_eq!(
        homotopy_positional_args(&named),
        homotopy_positional_args(&positional),
    );
}

#[test]
fn homotopy_reordered_named_arguments_bind_by_name() {
    // The named spelling deliberately reverses source order; binding by name must
    // still place `actual` first and `simplified` second, matching `homotopy(a, b)`.
    let reordered = expression_from_ast(&builtin_call(
        "homotopy",
        vec![
            named_arg("simplified", ast_var("b"), test_span()),
            named_arg("actual", ast_var("a"), test_span()),
        ],
    ))
    .expect("reordered named homotopy actuals lower cleanly");

    let args = homotopy_positional_args(&reordered);
    assert!(matches!(
        &args[0],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "a"
    ));
    assert!(matches!(
        &args[1],
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "b"
    ));
}

#[test]
fn homotopy_unknown_named_formal_is_refused() {
    let error = expression_from_ast(&builtin_call(
        "homotopy",
        vec![
            named_arg("bogus", ast_var("a"), test_span()),
            named_arg("simplified", ast_var("b"), test_span()),
        ],
    ))
    .expect_err("an unknown formal name has no slot to bind");
    assert!(matches!(
        error,
        crate::FlattenError::InvalidFunctionCallArgs { ref function, .. } if function == "homotopy"
    ));
}

#[test]
fn homotopy_duplicate_named_formal_is_refused() {
    let error = expression_from_ast(&builtin_call(
        "homotopy",
        vec![
            named_arg("actual", ast_var("a"), test_span()),
            named_arg("actual", ast_var("b"), test_span()),
        ],
    ))
    .expect_err("a formal filled twice is ambiguous");
    assert!(matches!(
        error,
        crate::FlattenError::InvalidFunctionCallArgs { ref function, .. } if function == "homotopy"
    ));
}

#[test]
fn homotopy_positional_after_named_is_refused() {
    // The grammar cannot produce this order, but the binder must still refuse a
    // hand-built AST that trails a positional actual after a named one.
    let error = expression_from_ast(&builtin_call(
        "homotopy",
        vec![named_arg("actual", ast_var("a"), test_span()), ast_var("b")],
    ))
    .expect_err("a positional actual may not follow a named actual");
    assert!(matches!(
        error,
        crate::FlattenError::InvalidFunctionCallArgs { ref function, .. } if function == "homotopy"
    ));
}

#[test]
fn homotopy_named_gap_before_filled_slot_is_refused() {
    // `simplified` fills slot 1 while slot 0 (`actual`) has no argument; a builtin
    // owns no defaults, so the hole cannot be filled and the call is refused.
    let error = expression_from_ast(&builtin_call(
        "homotopy",
        vec![named_arg("simplified", ast_var("b"), test_span())],
    ))
    .expect_err("an unfilled earlier slot is a gap a builtin cannot default");
    assert!(matches!(
        error,
        crate::FlattenError::InvalidFunctionCallArgs { ref function, .. } if function == "homotopy"
    ));
}

#[test]
fn named_argument_to_operator_without_named_formals_is_refused() {
    // `sin` takes a single positional argument and defines no named formals, so a
    // named actual is a typed refusal rather than a stranded NamedArgument node.
    let error = expression_from_ast(&builtin_call(
        "sin",
        vec![named_arg("x", ast_var("a"), test_span())],
    ))
    .expect_err("a builtin with no named formals rejects a named actual");
    assert!(matches!(
        error,
        crate::FlattenError::InvalidFunctionCallArgs { ref function, .. } if function == "sin"
    ));
}
