use super::{
    collect_when_eq_targets, extract_assignment_target, flatten_when_blocks,
    flatten_when_body_equation, flatten_when_for_equation, flatten_when_function_call,
    flatten_when_if_equation, is_known_streams_side_effect_call,
    validate_when_equation_definitions,
};
use crate::errors::FlattenError;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::TerminalType;
use rumoca_ir_flat as flat;
use std::collections::BTreeSet;
use std::sync::Arc;

fn token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text.to_string()),
        ..rumoca_core::Token::default()
    }
}

fn comp_ref(path: &str) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: rumoca_core::ComponentPath::from_flat_path(path)
            .into_parts()
            .into_iter()
            .map(|part| ast::ComponentRefPart {
                ident: token(&part),
                subs: None,
            })
            .collect(),
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_expr(value: i64) -> ast::Expression {
    int_expr_with_span(value, rumoca_core::Span::DUMMY)
}

fn int_expr_with_span(value: i64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: token(&value.to_string()),
        span,
    }
}

fn bool_expr(value: bool, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: TerminalType::Bool,
        token: token(if value { "true" } else { "false" }),
        span,
    }
}

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("when_equations_test.mo"),
        10,
        14,
    )
}

fn range_expr(start: i64, end: i64) -> ast::Expression {
    ast::Expression::Range {
        start: Arc::new(int_expr(start)),
        step: None,
        end: Arc::new(int_expr(end)),
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_expr(name: &str) -> ast::Expression {
    ast::Expression::ComponentReference(comp_ref(name))
}

fn var_expr_with_span(name: &str, span: rumoca_core::Span) -> ast::Expression {
    let mut reference = comp_ref(name);
    reference.span = span;
    ast::Expression::ComponentReference(reference)
}

fn for_index(name: &str, start: i64, end: i64) -> ast::ForIndex {
    ast::ForIndex {
        ident: token(name),
        range: range_expr(start, end),
    }
}

fn indexed_var_expr(name: &str, subscripts: &[&str]) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: token(name),
            subs: Some(
                subscripts
                    .iter()
                    .map(|name| ast::Subscript::Expression(var_expr(name)))
                    .collect(),
            ),
        }],
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    })
}

fn assignment(target: &str, value: i64) -> ast::Equation {
    ast::Equation::Simple {
        lhs: var_expr(target),
        rhs: int_expr(value),
    }
}

fn flat_assignment(target: &str, span: rumoca_core::Span) -> flat::WhenEquation {
    flat::WhenEquation::assign(
        rumoca_core::VarName::new(target),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span,
        },
        span,
        format!("assignment to {target}"),
    )
}

fn flat_condition(span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Boolean(true),
        span,
    }
}

fn named_argument(name: &str, value: ast::Expression, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::NamedArgument {
        name: token(name),
        value: Arc::new(value),
        span,
    }
}

#[test]
fn when_elsewhen_retains_one_owner_and_ordered_branch_spans() {
    let source = rumoca_core::SourceId::from_source_name("when_chain_test.mo");
    let owner_span = rumoca_core::Span::from_offsets(source, 5, 80);
    let first_span = rumoca_core::Span::from_offsets(source, 10, 14);
    let second_span = rumoca_core::Span::from_offsets(source, 40, 45);
    let blocks = vec![
        ast::EquationBlock {
            cond: bool_expr(true, first_span),
            eqs: vec![ast::Equation::Simple {
                lhs: var_expr("m"),
                rhs: int_expr(1),
            }],
        },
        ast::EquationBlock {
            cond: bool_expr(false, second_span),
            eqs: vec![ast::Equation::Simple {
                lhs: var_expr("m"),
                rhs: int_expr(2),
            }],
        },
    ];

    let chain = flatten_when_blocks(
        &crate::Context::default(),
        &blocks,
        &ast::QualifiedName::new(),
        owner_span,
        None,
    )
    .expect("flatten one when/elsewhen owner");

    assert_eq!(chain.span(), owner_span);
    assert_eq!(chain.branch_count(), 2);
    let branches = chain.branches().collect::<Vec<_>>();
    assert_eq!(branches[0].span, first_span);
    assert_eq!(branches[1].span, second_span);
    assert!(matches!(
        &branches[0].condition,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(true),
            ..
        }
    ));
    assert!(matches!(
        &branches[1].condition,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(false),
            ..
        }
    ));
}

#[test]
fn when_producer_rejects_an_empty_branch_list() {
    let span = test_span();
    let error = flatten_when_blocks(
        &crate::Context::default(),
        &[],
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect_err("source when owner requires its first branch");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation {
            description,
            span: error_span,
        } if description == "when-equation requires a first branch" && error_span == span
    ));
}

#[test]
fn when_chain_still_rejects_mismatched_dynamic_branch_targets() {
    let source = rumoca_core::SourceId::from_source_name("when_chain_targets_test.mo");
    let owner_span = rumoca_core::Span::from_offsets(source, 5, 80);
    let blocks = vec![
        ast::EquationBlock {
            cond: var_expr_with_span(
                "first_trigger",
                rumoca_core::Span::from_offsets(source, 10, 23),
            ),
            eqs: vec![ast::Equation::Simple {
                lhs: var_expr("first_target"),
                rhs: int_expr(1),
            }],
        },
        ast::EquationBlock {
            cond: var_expr_with_span(
                "second_trigger",
                rumoca_core::Span::from_offsets(source, 40, 54),
            ),
            eqs: vec![ast::Equation::Simple {
                lhs: var_expr("second_target"),
                rhs: int_expr(2),
            }],
        },
    ];

    let error = flatten_when_blocks(
        &crate::Context::default(),
        &blocks,
        &ast::QualifiedName::new(),
        owner_span,
        None,
    )
    .expect_err("dynamic branches with different targets violate EQN-013");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { span, .. } if span == owner_span
    ));
}

#[test]
fn mutually_exclusive_when_if_branches_may_define_the_same_target() {
    let span = test_span();
    let blocks = [ast::EquationBlock {
        cond: bool_expr(true, span),
        eqs: vec![ast::Equation::If {
            cond_blocks: vec![
                ast::EquationBlock {
                    cond: var_expr_with_span("firstChoice", span),
                    eqs: vec![assignment("target", 1)],
                },
                ast::EquationBlock {
                    cond: var_expr_with_span("secondChoice", span),
                    eqs: vec![assignment("target", 2)],
                },
            ],
            else_block: Some(vec![assignment("target", 3)]),
        }],
    }];

    flatten_when_blocks(
        &crate::Context::default(),
        &blocks,
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect("alternative conditional branches contribute one target definition");
}

#[test]
fn structural_false_when_if_ignores_inactive_duplicate_target() {
    let span = test_span();
    let mut ctx = crate::Context::default();
    ctx.structural_params.insert("p".to_string());
    ctx.boolean_parameter_values.insert("p".to_string(), false);
    let blocks = [ast::EquationBlock {
        cond: bool_expr(true, span),
        eqs: vec![
            ast::Equation::If {
                cond_blocks: vec![ast::EquationBlock {
                    cond: var_expr_with_span("p", span),
                    eqs: vec![assignment("x", 1)],
                }],
                else_block: Some(vec![assignment("y", 1)]),
            },
            assignment("x", 2),
        ],
    }];

    let chain = flatten_when_blocks(&ctx, &blocks, &ast::QualifiedName::new(), span, None)
        .expect("the inactive structural x definition must not collide");
    let targets = collect_when_eq_targets(&chain.first().equations);
    assert_eq!(
        targets,
        BTreeSet::from([
            rumoca_core::VarName::new("x"),
            rumoca_core::VarName::new("y"),
        ])
    );
}

#[test]
fn duplicate_flat_summary_reports_second_span_and_producer_rejects() {
    let source = rumoca_core::SourceId::from_source_name("duplicate_when_direct.mo");
    let first = rumoca_core::Span::from_offsets(source, 20, 25);
    let second = rumoca_core::Span::from_offsets(source, 30, 35);
    let error = validate_when_equation_definitions(&[
        flat_assignment("duplicate", first),
        flat_assignment("duplicate", second),
    ])
    .expect_err("one sequential branch cannot define a target twice");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { description, span }
            if span == second && description.contains("`duplicate`")
    ));

    let blocks = [ast::EquationBlock {
        cond: bool_expr(true, first),
        eqs: vec![assignment("duplicate", 1), assignment("duplicate", 2)],
    }];
    let error = flatten_when_blocks(
        &crate::Context::default(),
        &blocks,
        &ast::QualifiedName::new(),
        second,
        None,
    )
    .expect_err("the Flat producer validates each completed branch");
    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { description, .. }
            if description.contains("`duplicate`")
    ));
}

#[test]
fn nested_duplicate_reports_first_collision_in_source_order() {
    let source = rumoca_core::SourceId::from_source_name("duplicate_when_order.mo");
    let first_a = rumoca_core::Span::from_offsets(source, 10, 11);
    let first_b = rumoca_core::Span::from_offsets(source, 20, 21);
    let second_b = rumoca_core::Span::from_offsets(source, 30, 31);
    let second_a = rumoca_core::Span::from_offsets(source, 40, 41);
    let conditional = flat::WhenEquation::conditional(
        vec![(
            flat_condition(second_b),
            vec![
                flat_assignment("b", second_b),
                flat_assignment("a", second_a),
            ],
        )],
        None,
        second_b,
        "source-ordered nested definitions",
    );
    let error = validate_when_equation_definitions(&[
        flat_assignment("a", first_a),
        flat_assignment("b", first_b),
        conditional,
    ])
    .expect_err("the first source-ordered nested collision must be reported");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { description, span }
            if span == second_b && description.contains("`b`")
    ));
}

#[test]
fn assignments_before_and_after_nested_conditionals_are_duplicate_definitions() {
    let source = rumoca_core::SourceId::from_source_name("duplicate_when_nested.mo");
    let first = rumoca_core::Span::from_offsets(source, 20, 25);
    let second = rumoca_core::Span::from_offsets(source, 40, 45);
    let conditional = flat::WhenEquation::conditional(
        vec![(
            flat_condition(second),
            vec![flat_assignment("duplicate", second)],
        )],
        None,
        second,
        "nested duplicate",
    );
    let error =
        validate_when_equation_definitions(&[flat_assignment("duplicate", first), conditional])
            .expect_err("a conditional target contributes once to its enclosing sequence");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { description, span }
            if span == second && description.contains("`duplicate`")
    ));

    let conditional = flat::WhenEquation::conditional(
        vec![(
            flat_condition(first),
            vec![flat_assignment("duplicate", first)],
        )],
        None,
        first,
        "nested first definition",
    );
    let error =
        validate_when_equation_definitions(&[conditional, flat_assignment("duplicate", second)])
            .expect_err("a later assignment collides with a conditional target");
    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { description, span }
            if span == second && description.contains("`duplicate`")
    ));
}

#[test]
fn for_expansion_cannot_define_one_scalar_target_twice() {
    let span = test_span();
    let blocks = [ast::EquationBlock {
        cond: bool_expr(true, span),
        eqs: vec![ast::Equation::For {
            indices: vec![for_index("i", 1, 2)],
            equations: vec![assignment("duplicate", 1)],
        }],
    }];
    let error = flatten_when_blocks(
        &crate::Context::default(),
        &blocks,
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect_err("expanded iterations cannot repeatedly define one scalar target");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { description, span: found }
            if found == span && description.contains("`duplicate`")
    ));
}

#[test]
fn function_call_outputs_must_be_unique_within_one_tuple() {
    let span = test_span();
    let mut function = comp_ref("multi_output_call");
    function.span = span;
    let blocks = [ast::EquationBlock {
        cond: bool_expr(true, span),
        eqs: vec![ast::Equation::Simple {
            lhs: ast::Expression::Tuple {
                elements: vec![var_expr("duplicate"), var_expr("duplicate")],
                span,
            },
            rhs: ast::Expression::FunctionCall {
                comp: function,
                args: Vec::new(),
                is_partial_application: false,
                span,
            },
        }],
    }];
    let error = flatten_when_blocks(
        &crate::Context::default(),
        &blocks,
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect_err("one tuple cannot name the same output target twice");

    assert!(
        matches!(
            &error,
            FlattenError::UnsupportedEquation { description, span: found }
                if *found == span && description.contains("`duplicate`")
        ),
        "unexpected duplicate-output error: {error:?}"
    );
}

#[test]
fn when_assert_preserves_optional_level() {
    let span = test_span();
    let level_span = rumoca_core::Span::from_offsets(span.source, 30, 31);
    let equation = ast::Equation::Assert {
        condition: bool_expr(true, span),
        message: int_expr(1),
        level: Some(int_expr_with_span(2, level_span)),
    };

    let equations = flatten_when_body_equation(
        &crate::Context::default(),
        &equation,
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect("source assert is a checked when action");

    let [
        flat::WhenEquation::Assert {
            level: Some(level),
            span: found,
            ..
        },
    ] = equations.as_slice()
    else {
        panic!("optional assertion level must remain in Flat");
    };
    assert_eq!(*found, span);
    assert_eq!(level.span(), Some(level_span));
    assert!(matches!(
        level.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(2),
            ..
        }
    ));
}

#[test]
fn when_assert_and_terminate_reject_extra_or_missing_arguments() {
    let ctx = crate::Context::default();
    let prefix = ast::QualifiedName::new();
    let span = test_span();
    for count in [1, 4] {
        let args = (0..count).map(int_expr).collect::<Vec<_>>();
        let error = flatten_when_function_call(
            &ctx,
            &comp_ref("assert"),
            &args,
            &prefix,
            span,
            &ctx.current_imports,
            None,
        )
        .expect_err("assert arity must fail at its source owner");
        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation { span: found, .. } if found == span
        ));
    }
    for count in [0, 2] {
        let args = (0..count).map(int_expr).collect::<Vec<_>>();
        let error = flatten_when_function_call(
            &ctx,
            &comp_ref("terminate"),
            &args,
            &prefix,
            span,
            &ctx.current_imports,
            None,
        )
        .expect_err("terminate arity must fail at its source owner");
        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation { span: found, .. } if found == span
        ));
    }
}

#[test]
fn when_assert_decoder_rejects_duplicate_and_unknown_named_arguments() {
    let ctx = crate::Context::default();
    let prefix = ast::QualifiedName::new();
    let span = test_span();
    let duplicate = vec![
        bool_expr(true, span),
        int_expr(1),
        named_argument("message", int_expr(2), span),
    ];
    let unknown = vec![
        bool_expr(true, span),
        int_expr(1),
        named_argument("severity", int_expr(2), span),
    ];
    for args in [&duplicate, &unknown] {
        let error = flatten_when_function_call(
            &ctx,
            &comp_ref("assert"),
            args,
            &prefix,
            span,
            &ctx.current_imports,
            None,
        )
        .expect_err("every assert slot must be known and filled once");
        assert!(matches!(
            error,
            FlattenError::UnsupportedEquation { span: found, .. } if found == span
        ));
    }
}

#[test]
fn when_terminate_decoder_unwraps_only_named_message() {
    let ctx = crate::Context::default();
    let prefix = ast::QualifiedName::new();
    let span = test_span();
    let decoded = flatten_when_function_call(
        &ctx,
        &comp_ref("terminate"),
        &[named_argument("message", int_expr_with_span(7, span), span)],
        &prefix,
        span,
        &ctx.current_imports,
        None,
    )
    .expect("named terminate message is a single checked slot")
    .expect("terminate remains an event action");
    assert!(matches!(
        decoded,
        flat::WhenEquation::Terminate {
            message: rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(7),
                ..
            },
            ..
        }
    ));

    let error = flatten_when_function_call(
        &ctx,
        &comp_ref("terminate"),
        &[named_argument("text", int_expr(1), span)],
        &prefix,
        span,
        &ctx.current_imports,
        None,
    )
    .expect_err("unknown terminate slot must fail at the call");
    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { span: found, .. } if found == span
    ));
}

#[test]
fn nested_when_if_rejects_mismatched_explicit_else_target() {
    let span = test_span();
    let blocks = vec![ast::EquationBlock {
        cond: var_expr_with_span("active", span),
        eqs: vec![assignment("first", 1)],
    }];
    let error = flatten_when_if_equation(
        &crate::Context::default(),
        &blocks,
        &Some(vec![assignment("second", 2)]),
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect_err("one if plus else must compare target sets");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { span: found, .. } if found == span
    ));
}

#[test]
fn nested_when_if_rejects_missing_first_condition_block() {
    let span = test_span();
    let error = flatten_when_if_equation(
        &crate::Context::default(),
        &[],
        &Some(vec![assignment("target", 1)]),
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect_err("malformed if-equation cannot select an else without a first condition");

    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { description, span: found }
            if found == span && description.contains("at least one conditional branch")
    ));
}

#[test]
fn nested_when_if_distinguishes_absent_from_explicit_empty_else() {
    let span = test_span();
    let blocks = vec![ast::EquationBlock {
        cond: var_expr_with_span("active", span),
        eqs: vec![assignment("target", 1)],
    }];
    let absent = flatten_when_if_equation(
        &crate::Context::default(),
        &blocks,
        &None,
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect("an absent else remains absent");
    let [absent] = absent.as_slice() else {
        panic!("one conditional owner must be retained");
    };
    assert!(matches!(
        absent,
        flat::WhenEquation::Conditional {
            else_branch: None,
            ..
        }
    ));

    let error = flatten_when_if_equation(
        &crate::Context::default(),
        &blocks,
        &Some(Vec::new()),
        &ast::QualifiedName::new(),
        span,
        None,
    )
    .expect_err("an explicit empty else has an empty target set");
    assert!(matches!(
        error,
        FlattenError::UnsupportedEquation { span: found, .. } if found == span
    ));
}

#[test]
fn streams_error_in_when_fails_instead_of_disappearing() {
    let ctx = crate::Context::default();
    let span = test_span();
    let error = flatten_when_function_call(
        &ctx,
        &comp_ref("Modelica.Utilities.Streams.error"),
        &[int_expr(1)],
        &ast::QualifiedName::new(),
        span,
        &ctx.current_imports,
        None,
    )
    .expect_err("unrepresented Streams.error must never become an empty branch");

    let FlattenError::UnsupportedEquation {
        description,
        span: found,
    } = error
    else {
        panic!("Streams.error must report a source-owned unsupported equation");
    };
    assert_eq!(found, span);
    assert!(description.contains("typed checked event-call owner"));
}

#[test]
fn assignment_target_error_uses_invalid_lhs_span() {
    let span = test_span();
    let lhs = ast::Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: token("1"),
        span,
    };

    let err = extract_assignment_target(&lhs, &ast::QualifiedName::new())
        .expect_err("non-reference LHS should fail");

    assert!(
        matches!(
            err,
            FlattenError::UnsupportedEquation { span: error_span, .. }
                if error_span == span
        ),
        "invalid LHS diagnostic should use the LHS source span: {err:?}"
    );
}

#[test]
fn streams_side_effect_matching_uses_structured_parts() {
    assert!(is_known_streams_side_effect_call(&comp_ref(
        "Streams.print"
    )));
    assert!(is_known_streams_side_effect_call(&comp_ref(
        "Modelica.Utilities.Streams.close"
    )));
    assert!(is_known_streams_side_effect_call(&comp_ref(
        "Modelica.Utilities.Streams.error"
    )));
    assert!(!is_known_streams_side_effect_call(&comp_ref(
        "Modelica.Utilities.FakeStreams.print"
    )));
    assert!(!is_known_streams_side_effect_call(&comp_ref(
        "Modelica.Utilities.Streams.myprint"
    )));
}

#[test]
fn when_for_equation_expands_all_index_ranges() {
    let ctx = crate::Context::default();
    let indices = vec![for_index("i", 1, 2), for_index("j", 1, 2)];
    let equations = vec![ast::Equation::Simple {
        lhs: indexed_var_expr("y", &["i", "j"]),
        rhs: ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Arc::new(var_expr("i")),
            rhs: Arc::new(var_expr("j")),
            span: rumoca_core::Span::DUMMY,
        },
    }];

    let expanded = flatten_when_for_equation(
        &ctx,
        &indices,
        &equations,
        &ast::QualifiedName::new(),
        rumoca_core::Span::DUMMY,
        None,
    )
    .unwrap();

    let targets = expanded
        .iter()
        .map(|eq| match eq {
            flat::WhenEquation::Assign { target, .. } => target.as_str().to_string(),
            other => panic!("expected assignment, got {other:?}"),
        })
        .collect::<std::collections::HashSet<_>>();

    assert_eq!(targets.len(), 4);
    assert!(targets.contains("y[1,1]"));
    assert!(targets.contains("y[1,2]"));
    assert!(targets.contains("y[2,1]"));
    assert!(targets.contains("y[2,2]"));
}
