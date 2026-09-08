use super::arrays_and_declarations::*;
use super::functions_and_defaults::bool_literal;
use super::*;
use rumoca_core::Reference;

#[test]
fn test_simple_function() {
    let func = make_simple_function();
    let ctx = EvalContext::structural_preidentity();

    let result = eval_function(
        &func,
        vec![Value::Real(5.0)],
        &ctx,
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .unwrap();

    assert!((result.to_real().unwrap() - 10.0).abs() < 1e-10);
}

#[test]
fn empty_expression_in_function_body_is_a_nonrecoverable_ir_error() {
    let span = test_span();
    let mut func = Function::new("test.empty", rumoca_core::DefId::new(101), span);
    func.add_output(integer_param("y"));
    func.pure = true;
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: rumoca_core::Expression::Empty { span },
        span,
    }];

    let error = eval_function(
        &func,
        Vec::new(),
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        span,
    )
    .expect_err("empty function-body IR must not be deferred to runtime");

    assert!(matches!(&error, EvalError::InvalidSemanticIr { .. }));
    assert_eq!(error.span(), Some(span));
    assert_eq!(error.runtime_dependent_reason(), None);

    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(7),
            span,
        },
        span,
    }];
    assert_eq!(
        eval_function(
            &func,
            Vec::new(),
            &EvalContext::structural_preidentity(),
            &EvalLimits::default(),
            0,
            span,
        )
        .expect("a spanned literal assignment remains a legal function body"),
        Value::Integer(7)
    );
}

#[test]
fn empty_statement_and_dead_branch_ir_never_enter_execution() {
    let span = test_span();
    let mut func = Function::new("test.invalidStatement", rumoca_core::DefId::new(102), span);
    func.add_output(integer_param("y"));
    func.pure = true;
    func.body = vec![Statement::Empty { span }];
    assert!(matches!(
        fold(&func),
        Err(EvalError::InvalidSemanticIr { .. })
    ));

    func.body = vec![Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: Expression::Literal {
                value: Literal::Boolean(false),
                span,
            },
            stmts: vec![Statement::Empty { span }],
        }],
        else_block: Some(vec![assign(component_reference("y"), integer_literal(1))]),
        span,
    }];
    assert!(matches!(
        fold(&func),
        Err(EvalError::InvalidSemanticIr { .. })
    ));

    func.body.clear();
    assert_eq!(
        fold(&func).expect("an actually empty function body retains output initialization"),
        Value::Integer(0)
    );
}

#[test]
fn known_dead_function_branch_cannot_hide_malformed_builtin_arity() {
    let span = test_span();
    let call_span = Span::from_offsets(span.source, 20, 30);
    let mut func = Function::new(
        "test.invalidBuiltinArity",
        rumoca_core::DefId::new(103),
        span,
    );
    func.add_output(integer_param("y"));
    func.pure = true;
    func.body = vec![Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: Expression::Literal {
                value: Literal::Boolean(false),
                span,
            },
            stmts: vec![assign(
                component_reference("y"),
                Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Smooth,
                    args: vec![integer_literal(1)],
                    span: call_span,
                },
            )],
        }],
        else_block: Some(vec![assign(component_reference("y"), integer_literal(7))]),
        span,
    }];

    let error = fold(&func)
        .expect_err("whole-function validation must inspect a known-dead statement branch");
    assert!(matches!(error, EvalError::WrongArgCount { .. }));
    assert_eq!(error.span(), Some(call_span));
}

#[test]
fn known_dead_function_branch_cannot_hide_retired_named_string_representation() {
    let span = test_span();
    let call_span = Span::from_offsets(span.source, 30, 40);
    let mut func = Function::new(
        "test.invalidNamedStringArity",
        rumoca_core::DefId::new(104),
        span,
    );
    func.add_output(integer_param("y"));
    func.pure = true;
    func.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: bool_literal(false),
            stmts: vec![assign(
                component_reference("y"),
                Expression::FunctionCall {
                    name: Reference::new("String"),
                    args: vec![integer_literal(1), integer_literal(2)],
                    is_constructor: false,
                    call_kind: rumoca_core::FunctionCallKind::Invocation,
                    span: call_span,
                },
            )],
        }],
        else_block: Some(vec![assign(component_reference("y"), integer_literal(7))]),
        span,
    }];

    let error = fold(&func)
        .expect_err("retired named String representation is invalid independent of reachability");
    assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
    assert_eq!(error.span(), Some(call_span));
}

fn named_call_argument(name: &str, value: Expression) -> Expression {
    Expression::FunctionCall {
        name: Reference::generated(format!("{}{name}", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)),
        args: vec![value],
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    }
}

fn malformed_named_call_argument(
    values: Vec<Expression>,
    call_kind: rumoca_core::FunctionCallKind,
) -> Expression {
    malformed_named_call_argument_with_shape("a", values, true, call_kind)
}

fn malformed_named_call_argument_with_shape(
    suffix: &str,
    values: Vec<Expression>,
    is_constructor: bool,
    call_kind: rumoca_core::FunctionCallKind,
) -> Expression {
    Expression::FunctionCall {
        name: Reference::generated(format!(
            "{}{suffix}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: values,
        is_constructor,
        call_kind,
        span: test_span(),
    }
}

fn registered_call(args: Vec<Expression>, span: Span) -> Expression {
    Expression::FunctionCall {
        name: exact_reference("test.namedShape", rumoca_core::DefId::new(105)),
        args,
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    }
}

fn named_shape_callee() -> Function {
    let mut function = Function::new("test.namedShape", rumoca_core::DefId::new(105), test_span());
    function.add_input(integer_param("a"));
    let mut optional = integer_param("b");
    optional.default = Some(integer_literal(9));
    function.add_input(optional);
    function.add_output(integer_param("y"));
    function.pure = true;
    function
}

fn call_shape_caller(call: Expression, short_circuit: bool) -> Function {
    let span = test_span();
    let mut caller = Function::new("test.callShapeCaller", rumoca_core::DefId::new(106), span);
    caller.add_output(integer_param("y"));
    caller.pure = true;
    let false_condition = bool_literal(false);
    let condition = if short_circuit {
        Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs: Box::new(false_condition),
            rhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Eq,
                lhs: Box::new(call.clone()),
                rhs: Box::new(integer_literal(0)),
                span,
            }),
            span,
        }
    } else {
        false_condition
    };
    let hidden = if short_circuit {
        assign(component_reference("y"), integer_literal(1))
    } else {
        assign(component_reference("y"), call)
    };
    caller.body = vec![Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: condition,
            stmts: vec![hidden],
        }],
        else_block: Some(vec![assign(component_reference("y"), integer_literal(7))]),
        span,
    }];
    caller
}

#[test]
fn registered_named_call_shape_is_validated_in_dead_and_short_circuit_branches() {
    let span = test_span();
    let cases = [
        (
            "missing required argument a",
            vec![named_call_argument("b", integer_literal(2))],
        ),
        (
            "unknown named argument ghost",
            vec![named_call_argument("ghost", integer_literal(2))],
        ),
        (
            "duplicate argument a",
            vec![
                integer_literal(1),
                named_call_argument("a", integer_literal(2)),
            ],
        ),
        (
            "positional argument after named argument",
            vec![
                named_call_argument("a", integer_literal(1)),
                integer_literal(2),
            ],
        ),
    ];
    for (expected, args) in cases {
        for short_circuit in [false, true] {
            let call = registered_call(args.clone(), span);
            let caller = call_shape_caller(call, short_circuit);
            let mut ctx = EvalContext::structural_preidentity();
            ctx.insert_direct_function_fixture(named_shape_callee());
            let error = eval_function(&caller, Vec::new(), &ctx, &EvalLimits::default(), 0, span)
                .expect_err("static validation must inspect unreachable registered calls");
            let EvalError::FunctionError { message, .. } = error else {
                panic!("expected call-shape FunctionError, got {error:?}");
            };
            assert!(
                message.contains(expected),
                "expected `{expected}` in `{message}` (short_circuit={short_circuit})",
            );
        }
    }
}

#[test]
fn malformed_named_argument_markers_are_rejected_before_dead_code_elimination() {
    let span = test_span();
    let markers = [
        malformed_named_call_argument(Vec::new(), rumoca_core::FunctionCallKind::Invocation),
        malformed_named_call_argument(
            vec![integer_literal(1), integer_literal(2)],
            rumoca_core::FunctionCallKind::Invocation,
        ),
        malformed_named_call_argument(
            vec![integer_literal(1)],
            rumoca_core::FunctionCallKind::PartialApplication,
        ),
        malformed_named_call_argument_with_shape(
            "a",
            vec![integer_literal(1)],
            false,
            rumoca_core::FunctionCallKind::Invocation,
        ),
        malformed_named_call_argument_with_shape(
            "",
            vec![integer_literal(1)],
            true,
            rumoca_core::FunctionCallKind::Invocation,
        ),
    ];
    for marker in markers {
        for short_circuit in [false, true] {
            let caller =
                call_shape_caller(registered_call(vec![marker.clone()], span), short_circuit);
            let mut ctx = EvalContext::structural_preidentity();
            ctx.insert_direct_function_fixture(named_shape_callee());
            let error = eval_function(&caller, Vec::new(), &ctx, &EvalLimits::default(), 0, span)
                .expect_err("malformed internal named-argument markers must fail statically");
            assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
        }
    }
}

#[test]
fn user_owned_reserved_prefix_call_remains_an_ordinary_positional_expression() {
    let span = test_span();
    let user_call = Expression::FunctionCall {
        name: Reference::new(format!("{}a", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)),
        args: vec![integer_literal(1)],
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    };
    let caller = call_shape_caller(registered_call(vec![user_call], span), false);
    let mut ctx = EvalContext::structural_preidentity();
    ctx.insert_direct_function_fixture(named_shape_callee());

    assert_eq!(
        eval_function(&caller, Vec::new(), &ctx, &EvalLimits::default(), 0, span)
            .expect("dead user-owned reserved-prefix call is an ordinary expression"),
        Value::Integer(7)
    );
}

#[test]
fn test_named_argument_binding() {
    let mut func = Function::new("test.third", rumoca_core::DefId::new(107), Span::DUMMY);
    func.add_input(integer_param("x"));
    func.add_input(integer_param("y"));
    func.add_input(integer_param("z"));
    func.add_output(integer_param("result"));
    func.pure = true;
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("result"),
        value: rumoca_core::Expression::VarRef {
            name: fixture_reference("z"),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    }];

    let result = eval_function_with_call_args(
        &func,
        vec![
            FunctionCallArg::positional(Value::Integer(1)),
            FunctionCallArg::positional(Value::Integer(2)),
            FunctionCallArg::named("z".to_string(), Value::Integer(7)),
        ],
        &EvalContext::structural_preidentity(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
    .unwrap();

    assert_eq!(result.as_integer(), Some(7));
}

#[test]
fn eval_var_ref_requires_registered_enum_identity() {
    let env = FunctionEnv {
        declarations: Vec::new(),
        formal_extents: Vec::new(),
        loop_bindings: Vec::new(),
        inputs: IndexMap::new(),
        outputs: IndexMap::new(),
        locals: IndexMap::new(),
        declared_outputs: IndexSet::new(),
        declared_locals: IndexSet::new(),
    };
    let enum_declaration = rumoca_core::DefId::new(71);
    let catalog = crate::constant::ResolvedEnumCatalog::try_from_declarations(vec![
        crate::constant::ResolvedEnumDeclaration {
            declaration: enum_declaration,
            type_name: "Modelica.Types.Color".to_string(),
            literals: vec!["red".to_string()],
        },
    ])
    .unwrap();
    let ctx = EvalContext::structural_preidentity_with_catalog(0, 0, catalog);
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &ctx,
        limits: &limits,
        depth: 0,
        span: Span::DUMMY,
    };

    let Ok(enum_reference) = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![
            rumoca_core::ComponentRefPart {
                ident: "Modelica.Types.Color".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: enum_declaration,
            },
            rumoca_core::ComponentRefPart {
                ident: "red".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(72),
            },
        ],
    ) else {
        panic!("fixture enum reference must be resolved");
    };
    let enum_value = eval_var_ref(
        &rumoca_core::Reference::from_component_reference(enum_reference),
        &[],
        &env,
        &eval,
    )
    .expect("registered enum literal");
    assert!(matches!(
        enum_value,
        Value::ResolvedEnum(value)
            if value.declaration() == enum_declaration
                && value.literal() == "red"
    ));

    assert!(
        eval_var_ref(
            &rumoca_core::Reference::new("data[index.with.dot]"),
            &[],
            &env,
            &eval
        )
        .is_err()
    );

    let error = eval_var_ref(
        &rumoca_core::Reference::new("unresolved.package.constant"),
        &[],
        &env,
        &eval,
    )
    .expect_err("a dotted spelling must not invent an enumeration value");
    assert!(matches!(error, EvalError::UnknownVariable { .. }));
}

#[test]
fn optional_evaluation_does_not_defer_a_malformed_function_call() {
    let mut ctx = EvalContext::structural_preidentity();
    ctx.insert_direct_function_fixture(make_simple_function());
    let expression = Expression::FunctionCall {
        name: exact_reference("test.f", rumoca_core::DefId::new(10_001)),
        args: Vec::new(),
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };

    let error = crate::constant::eval_optional(&expression, &ctx)
        .expect_err("a missing required argument is a model error, not runtime dependence");
    assert!(matches!(error, EvalError::FunctionError { .. }));
    assert_eq!(error.span(), Some(test_span()));
}
