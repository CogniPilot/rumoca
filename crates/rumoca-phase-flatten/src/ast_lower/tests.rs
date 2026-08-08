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
fn derivative_lowering_is_structurally_discoverable() {
    let derivative_span = span_at(10, 16);
    let call = ast::Expression::FunctionCall {
        comp: function_ref("der"),
        args: vec![ast_var_with_span("x", span_at(14, 15))],
        is_partial_application: false,
        span: derivative_span,
    };

    let lowered = expression_from_ast(&call).unwrap();
    assert_eq!(lowered.span(), Some(derivative_span));
    assert!(lowered.contains_der());
    assert_eq!(
        lowered.get_der_variable().map(|name| name.as_str()),
        Some("x")
    );
    let mut states = Vec::new();
    lowered.collect_state_variables(&mut states);
    assert_eq!(states, vec![rumoca_core::VarName::new("x")]);
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
            span,
        },
    ] = args.as_slice()
    else {
        panic!("expected generated named-argument wrapper");
    };
    assert_eq!(name.as_str(), "__rumoca_named_arg__.value");
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
fn unresolved_interval_spelling_never_mints_a_predefined_intrinsic() {
    let lowered = convert_function_call_with_context(
        &function_ref("interval"),
        &[ast_var("u")],
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
fn get_instance_name_requires_instance_scope() {
    let err = convert_function_call_with_context(
        &function_ref("getInstanceName"),
        &[],
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
                ast::Subscript::Empty,
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
