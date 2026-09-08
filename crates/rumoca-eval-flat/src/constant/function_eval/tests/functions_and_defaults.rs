use super::super::assignment::set_array_selection;
use super::arrays_and_declarations::*;
use super::*;
use crate::constant::{
    ResolvedEnumCatalog, ResolvedIdentityInventory, ResolvedOccurrenceKey, ResolvedValueBinding,
};
use rumoca_core::Reference;

#[test]
fn test_recursion_limit() {
    let mut func = Function::new("test.recurse", rumoca_core::DefId::new(201), Span::DUMMY);
    func.add_input(integer_param("n"));
    func.add_output(integer_param("y"));
    func.pure = true;

    // Simple function that always returns 0 (to test limit checking)
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];

    let ctx = EvalContext::structural_preidentity();
    let limits = EvalLimits {
        recursion_depth: 5,
        max_iterations: 1000,
    };

    // This should succeed at depth 5
    let result = eval_function(
        &func,
        vec![Value::Integer(1)],
        &ctx,
        &limits,
        5,
        Span::DUMMY,
    );
    assert!(result.is_ok());

    // This should fail at depth 6
    let result = eval_function(
        &func,
        vec![Value::Integer(1)],
        &ctx,
        &limits,
        6,
        Span::DUMMY,
    );
    let error = result.expect_err("the interpreter must enforce its recursion budget");
    assert!(matches!(error, EvalError::UnsupportedExpression { .. }));
    assert_eq!(
        error.runtime_dependent_reason(),
        Some(crate::constant::RuntimeDependentReason::UnimplementedForm)
    );
}

#[test]
fn test_while_loop() {
    // Test: function countTo4() output Integer count; algorithm count := 0; while count < 4 loop count := count + 1; end while; end countTo4;
    let mut func = Function::new("test.countTo4", rumoca_core::DefId::new(202), Span::DUMMY);
    func.add_output(integer_param("count"));
    func.pure = true;

    // count := 0
    let init_stmt = rumoca_core::Statement::Assignment {
        comp: component_reference("count"),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    };

    // while count < 4 loop count := count + 1; end while
    let while_stmt = rumoca_core::Statement::While {
        block: rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Lt,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: fixture_reference("count"),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(4),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            stmts: vec![rumoca_core::Statement::Assignment {
                comp: component_reference("count"),
                value: rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(rumoca_core::Expression::VarRef {
                        name: fixture_reference("count"),
                        subscripts: Vec::new(),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rhs: Box::new(rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
                span: rumoca_core::Span::DUMMY,
            }],
        },
        span: rumoca_core::Span::DUMMY,
    };

    func.body = vec![init_stmt, while_stmt];

    let ctx = EvalContext::structural_preidentity();
    let result =
        eval_function(&func, vec![], &ctx, &EvalLimits::default(), 0, Span::DUMMY).unwrap();

    assert_eq!(result.as_integer(), Some(4), "while loop should count to 4");
}

#[test]
fn resolved_short_and_qualified_calls_share_one_catalog_entry() {
    let mut func = Function::new("Pkg.isEqual", rumoca_core::DefId::new(203), Span::DUMMY);
    func.def_id = Some(rumoca_core::DefId::new(203));
    func.instance_id = Some(rumoca_core::FunctionInstanceId::new(203));
    func.add_input(real_param("a"));
    func.add_input(real_param("b"));
    func.add_output(real_param("y"));
    func.pure = true;
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: fixture_reference("a"),
                subscripts: Vec::new(),
                span: Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    }];

    let mut ctx = EvalContext::resolved_empty();
    ctx.try_add_function(func).unwrap();
    assert_eq!(ctx.finalized_function_count(), 1);
    assert!(
        ctx.finalized_function_by_instance(rumoca_core::FunctionInstanceId::new(203))
            .is_some()
    );

    for spelling in ["isEqual", "Pkg.isEqual"] {
        let call = Expression::FunctionCall {
            name: rumoca_core::Reference::new(spelling).with_resolved_function(
                rumoca_core::ResolvedFunctionReference {
                    instance_id: rumoca_core::FunctionInstanceId::new(203),
                    base_part_count: spelling.split('.').count(),
                    transitively_non_replaceable: false,
                },
            ),
            args: vec![
                Expression::Literal {
                    value: Literal::Real(3.0),
                    span: Span::DUMMY,
                },
                Expression::Literal {
                    value: Literal::Real(3.0),
                    span: Span::DUMMY,
                },
            ],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: Span::DUMMY,
        };
        let result = crate::constant::eval_expr_with_span(&call, &ctx, Span::DUMMY).unwrap();
        assert_eq!(result, Value::Real(6.0));
    }

    let wrong_identity = Expression::FunctionCall {
        name: rumoca_core::Reference::new("isEqual").with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(204),
                base_part_count: 1,
                transitively_non_replaceable: false,
            },
        ),
        args: Vec::new(),
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: Span::DUMMY,
    };
    assert!(crate::constant::eval_expr_with_span(&wrong_identity, &ctx, Span::DUMMY).is_err());
}

#[test]
fn function_catalog_keys_specializations_by_exact_instance_identity() {
    let mut canonical = Function::new("Pkg.f", rumoca_core::DefId::new(20_301), Span::DUMMY);
    canonical.def_id = Some(rumoca_core::DefId::new(20_301));
    canonical.instance_id = Some(rumoca_core::FunctionInstanceId::new(20_301));

    let mut ctx = EvalContext::resolved_empty();
    ctx.try_add_function(canonical.clone()).unwrap();

    let mut second_specialization = canonical.clone();
    second_specialization.instance_id = Some(rumoca_core::FunctionInstanceId::new(20_302));
    second_specialization.def_id = Some(rumoca_core::DefId::new(20_302));
    second_specialization.exposure_def_id = rumoca_core::DefId::new(20_302);
    ctx.try_add_function(second_specialization).unwrap();
    assert_eq!(ctx.finalized_function_count(), 2);

    let mut duplicate_identity = canonical;
    duplicate_identity.name = rumoca_core::VarName::new("Alias.f");
    assert!(ctx.try_add_function(duplicate_identity).is_err());
    assert_eq!(ctx.finalized_function_count(), 2);
    assert_eq!(
        ctx.finalized_function_by_instance(rumoca_core::FunctionInstanceId::new(20_301))
            .map(|function| function.name.as_str()),
        Some("Pkg.f")
    );
    assert_eq!(
        ctx.finalized_function_by_instance(rumoca_core::FunctionInstanceId::new(20_302))
            .map(|function| function.name.as_str()),
        Some("Pkg.f")
    );
}

#[test]
fn mixed_callable_authority_selects_exact_finalized_and_refuses_pending() {
    let call_span = test_span();
    for pending_first in [true, false] {
        let pending = Function::new("Pkg.pending", rumoca_core::DefId::new(20_310), call_span);
        let mut finalized =
            Function::new("Pkg.finalized", rumoca_core::DefId::new(20_311), call_span);
        finalized.def_id = Some(rumoca_core::DefId::new(20_311));
        finalized.instance_id = Some(rumoca_core::FunctionInstanceId::new(20_311));
        finalized.add_output(real_param("y"));
        finalized.body = vec![rumoca_core::Statement::Assignment {
            comp: component_reference("y"),
            value: Expression::Literal {
                value: Literal::Real(7.0),
                span: call_span,
            },
            span: call_span,
        }];

        let mut ctx = EvalContext::resolved_empty();
        if pending_first {
            ctx.try_add_function(pending.clone()).unwrap();
            ctx.try_add_function(finalized.clone()).unwrap();
        } else {
            ctx.try_add_function(finalized.clone()).unwrap();
            ctx.try_add_function(pending.clone()).unwrap();
        }
        assert_eq!(ctx.pending_function_count(), Some(1));
        assert_eq!(ctx.finalized_function_count(), 1);

        let exact_call = Expression::FunctionCall {
            name: Reference::new("Pkg.finalized").with_resolved_function(
                rumoca_core::ResolvedFunctionReference {
                    instance_id: rumoca_core::FunctionInstanceId::new(20_311),
                    base_part_count: 2,
                    transitively_non_replaceable: false,
                },
            ),
            args: Vec::new(),
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        };
        assert_eq!(
            crate::constant::eval_expr(&exact_call, &ctx).unwrap(),
            Value::Real(7.0)
        );

        let pending_call = Expression::FunctionCall {
            name: Reference::new("Pkg.pending"),
            args: Vec::new(),
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        };
        assert!(matches!(
            crate::constant::eval_expr(&pending_call, &ctx),
            Err(EvalError::PendingCallableIdentity { span, .. }) if span == call_span
        ));

        let mut duplicate = finalized.clone();
        duplicate.name = rumoca_core::VarName::new("Pkg.alias");
        assert!(ctx.try_add_function(duplicate).is_err());
        assert_eq!(ctx.pending_function_count(), Some(1));
        assert_eq!(ctx.finalized_function_count(), 1);
    }
}

#[test]
fn builtin_spelling_cannot_bypass_user_function_identity() {
    let call_span = test_span();
    let mut user_abs = Function::new("abs", rumoca_core::DefId::new(20_320), call_span);
    user_abs.def_id = Some(rumoca_core::DefId::new(20_320));
    user_abs.instance_id = Some(rumoca_core::FunctionInstanceId::new(20_320));
    user_abs.add_input(real_param("u"));
    user_abs.add_output(real_param("y"));
    user_abs.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: Expression::Literal {
            value: Literal::Real(42.0),
            span: call_span,
        },
        span: call_span,
    }];

    let mut finalized = EvalContext::resolved_empty();
    finalized.try_add_function(user_abs).unwrap();
    let exact_user_call = Expression::FunctionCall {
        name: Reference::new("abs").with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id: rumoca_core::FunctionInstanceId::new(20_320),
                base_part_count: 1,
                transitively_non_replaceable: false,
            },
        ),
        args: vec![Expression::Literal {
            value: Literal::Real(-2.0),
            span: call_span,
        }],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: call_span,
    };
    assert_eq!(
        crate::constant::eval_expr(&exact_user_call, &finalized).unwrap(),
        Value::Real(42.0),
        "FunctionCall selects the exact user instance; only BuiltinCall selects abs"
    );

    let mut pending = EvalContext::structural_preidentity();
    pending
        .try_add_function(Function::new(
            "abs",
            rumoca_core::DefId::new(20_321),
            call_span,
        ))
        .unwrap();
    let pending_call = Expression::FunctionCall {
        name: Reference::new("abs"),
        args: vec![Expression::Literal {
            value: Literal::Real(-2.0),
            span: call_span,
        }],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: call_span,
    };
    assert!(matches!(
        crate::constant::eval_expr(&pending_call, &pending),
        Err(EvalError::PendingCallableIdentity { span, .. }) if span == call_span
    ));
}

#[test]
fn function_catalog_retains_pending_and_rejects_reserved_identity_without_mutation() {
    let mut pending_ctx = EvalContext::structural_preidentity();
    let missing = Function::new("Pkg.missing", rumoca_core::DefId::new(20_303), Span::DUMMY);
    assert!(pending_ctx.try_add_function(missing.clone()).is_ok());
    assert!(pending_ctx.try_add_function(missing).is_ok());
    assert_eq!(pending_ctx.finalized_function_count(), 0);

    let mut ctx = EvalContext::resolved_empty();
    let mut reserved = Function::new("Pkg.reserved", rumoca_core::DefId::new(0), Span::DUMMY);
    reserved.def_id = Some(rumoca_core::DefId::new(0));
    reserved.instance_id = Some(rumoca_core::FunctionInstanceId::new(20_304));
    assert!(ctx.try_add_function(reserved).is_err());
    assert_eq!(ctx.finalized_function_count(), 0);
}

/// The actual MSL 4.1.0 `Modelica.Math.Vectors.isEqual` body, verbatim in
/// structure: protected `n = size(v1,1)`, `i = 1`, an if over the length
/// match, a bounded while with an early-exit rewrite of `i`, indexed reads,
/// and `abs`. This is the body the general evaluator must interpret now that
/// no emulation exists for the spelling.
pub(super) fn boolean_param(name: &str) -> rumoca_core::FunctionParam {
    function_param(name, "Boolean", rumoca_core::TypeId::new(3))
}

pub(super) fn unknown_vector_param(name: &str) -> rumoca_core::FunctionParam {
    let real = rumoca_core::TypeId::new(1);
    let effective_type = rumoca_core::EffectiveType::new(real, real, vec![0]).expect("vector type");
    rumoca_core::FunctionParam::new(name, "Real", effective_type, test_span())
        .with_def_id(fixture_def_id(name))
}

fn builtin_call(function: rumoca_core::BuiltinFunction, args: Vec<Expression>) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span: Span::DUMMY,
    }
}

pub(super) fn bool_literal(value: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(value),
        span: Span::DUMMY,
    }
}

pub(super) fn binary(op: rumoca_core::OpBinary, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: Span::DUMMY,
    }
}

pub(super) fn indexed(name: &str, index: &str) -> Expression {
    Expression::VarRef {
        name: fixture_reference(name),
        subscripts: vec![Subscript::Expr {
            expr: Box::new(var_ref(index)),
            span: Span::DUMMY,
        }],
        span: Span::DUMMY,
    }
}

/// The early-exit element scan of the real `isEqual` body: `while i <= n
/// loop if abs(v1[i] - v2[i]) > eps then result := false; i := n; end if;
/// i := i + 1; end while`.
pub(super) fn is_equal_element_scan() -> Statement {
    let early_exit = rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: binary(
                rumoca_core::OpBinary::Gt,
                builtin_call(
                    rumoca_core::BuiltinFunction::Abs,
                    vec![binary(
                        rumoca_core::OpBinary::Sub,
                        indexed("v1", "i"),
                        indexed("v2", "i"),
                    )],
                ),
                var_ref("eps"),
            ),
            stmts: vec![
                assign(component_reference("result"), bool_literal(false)),
                assign(component_reference("i"), var_ref("n")),
            ],
        }],
        else_block: None,
        span: Span::DUMMY,
    };
    let bump = assign(
        component_reference("i"),
        binary(rumoca_core::OpBinary::Add, var_ref("i"), integer_literal(1)),
    );
    rumoca_core::Statement::While {
        block: rumoca_core::StatementBlock {
            cond: binary(rumoca_core::OpBinary::Le, var_ref("i"), var_ref("n")),
            stmts: vec![early_exit, bump],
        },
        span: Span::DUMMY,
    }
}

pub(super) fn real_msl_vectors_is_equal() -> Function {
    let mut func = Function::new(
        "Modelica.Math.Vectors.isEqual",
        rumoca_core::DefId::new(41),
        Span::DUMMY,
    );
    func.def_id = Some(rumoca_core::DefId::new(41));
    func.pure = true;
    func.add_input(unknown_vector_param("v1"));
    func.add_input(unknown_vector_param("v2"));
    func.add_input(real_param("eps").with_default(real_literal(0.0)));
    func.add_output(boolean_param("result"));
    func.add_local(integer_param("n").with_default(builtin_call(
        rumoca_core::BuiltinFunction::Size,
        vec![var_ref("v1"), integer_literal(1)],
    )));
    func.add_local(integer_param("i").with_default(integer_literal(1)));

    func.body = vec![
        assign(component_reference("result"), bool_literal(false)),
        rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: binary(
                    rumoca_core::OpBinary::Eq,
                    builtin_call(
                        rumoca_core::BuiltinFunction::Size,
                        vec![var_ref("v2"), integer_literal(1)],
                    ),
                    var_ref("n"),
                ),
                stmts: vec![
                    assign(component_reference("result"), bool_literal(true)),
                    is_equal_element_scan(),
                ],
            }],
            else_block: None,
            span: Span::DUMMY,
        },
    ];
    func
}

#[test]
fn real_msl_is_equal_body_interprets_through_the_general_evaluator() {
    let ctx = EvalContext::structural_preidentity();
    let function = real_msl_vectors_is_equal();
    let limits = EvalLimits::default();

    let vec_of =
        |values: &[f64]| Value::Array(values.iter().map(|value| Value::Real(*value)).collect());
    let evaluate = |args| eval_function(&function, args, &ctx, &limits, 0, Span::DUMMY);
    let equal = evaluate(vec![vec_of(&[1.0, 2.0]), vec_of(&[1.0, 2.0])]).unwrap();
    assert_eq!(equal, Value::Bool(true));

    let unequal = evaluate(vec![vec_of(&[1.0, 2.0]), vec_of(&[1.0, 3.0])]).unwrap();
    assert_eq!(unequal, Value::Bool(false));

    let length_mismatch = evaluate(vec![vec_of(&[1.0, 2.0]), vec_of(&[1.0])]).unwrap();
    assert_eq!(length_mismatch, Value::Bool(false));

    let with_eps = evaluate(vec![
        vec_of(&[1.0, 2.0]),
        vec_of(&[1.0005, 2.0]),
        Value::Real(0.01),
    ])
    .unwrap();
    assert_eq!(with_eps, Value::Bool(true));
}

#[test]
fn omitted_input_defaults_follow_declaration_semantics() {
    // function f(input Integer a, input Integer b = a + 1,
    //            input Integer c = b * 2) output Integer y;
    // algorithm y := c; end f;
    // MLS §12.4.1: omitted inputs take their declared defaults; §12.4.4's
    // ordering rule lets b's default read a and c's default read b.
    let mut func = Function::new("test.defaults", rumoca_core::DefId::new(204), Span::DUMMY);
    func.pure = true;
    func.add_input(integer_param("a"));
    func.add_input(integer_param("b").with_default(Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(var_ref("a")),
        rhs: Box::new(integer_literal(1)),
        span: Span::DUMMY,
    }));
    func.add_input(integer_param("c").with_default(Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Box::new(var_ref("b")),
        rhs: Box::new(integer_literal(2)),
        span: Span::DUMMY,
    }));
    func.add_output(integer_param("y"));
    func.body = vec![assign(component_reference("y"), var_ref("c"))];

    let ctx = EvalContext::structural_preidentity();
    let limits = EvalLimits::default();

    // Positional omission: f(4) -> b = 5, c = 10.
    let result = eval_function(
        &func,
        vec![Value::Integer(4)],
        &ctx,
        &limits,
        0,
        Span::DUMMY,
    )
    .unwrap();
    assert_eq!(result.as_integer(), Some(10));

    // Named omission skipping the middle formal: f(a = 4, c = 7) -> 7, and
    // b's default still binds (readable, unused).
    let result = eval_function_with_call_args(
        &func,
        vec![
            FunctionCallArg::named("a".to_string(), Value::Integer(4)),
            FunctionCallArg::named("c".to_string(), Value::Integer(7)),
        ],
        &ctx,
        &limits,
        0,
        Span::DUMMY,
    )
    .unwrap();
    assert_eq!(result.as_integer(), Some(7));

    // Explicit b overrides its default: f(4, 10) -> c = 20.
    let result = eval_function(
        &func,
        vec![Value::Integer(4), Value::Integer(10)],
        &ctx,
        &limits,
        0,
        Span::DUMMY,
    )
    .unwrap();
    assert_eq!(result.as_integer(), Some(20));
}

#[test]
fn cyclic_or_missing_input_defaults_fail_closed() {
    // Cyclic defaults have no §12.4.4 order and must error, never take a
    // type zero.
    let mut cyclic = Function::new("test.cyclic", rumoca_core::DefId::new(205), Span::DUMMY);
    cyclic.pure = true;
    cyclic.add_input(integer_param("a").with_default(var_ref("b")));
    cyclic.add_input(integer_param("b").with_default(var_ref("a")));
    cyclic.add_output(integer_param("y"));
    cyclic.body = vec![assign(component_reference("y"), var_ref("a"))];

    let ctx = EvalContext::structural_preidentity();
    let limits = EvalLimits::default();
    assert!(eval_function(&cyclic, vec![], &ctx, &limits, 0, Span::DUMMY).is_err());

    // A required formal without a default stays an error when omitted.
    let mut required = Function::new("test.required", rumoca_core::DefId::new(206), Span::DUMMY);
    required.pure = true;
    required.add_input(integer_param("a"));
    required.add_output(integer_param("y"));
    required.body = vec![assign(component_reference("y"), var_ref("a"))];
    assert!(eval_function(&required, vec![], &ctx, &limits, 0, Span::DUMMY).is_err());
}

#[test]
fn foreign_resolved_default_does_not_alias_same_spelling_formal_value() {
    fn resolved_reference(name: &str, def_id: u32, instance_id: u32) -> Expression {
        let Ok(component_ref) = rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(def_id),
            }],
        ) else {
            panic!("fixture reference must be exact");
        };
        let reference = rumoca_core::Reference::from_component_reference(component_ref)
            .with_instance_id(rumoca_core::InstanceId::new(instance_id));
        Expression::VarRef {
            name: reference,
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    let mut a = integer_param("a");
    a.def_id = Some(rumoca_core::DefId::new(1));
    let outer_reference = resolved_reference("b", 99, 990);
    a.default = Some(outer_reference.clone());
    let mut b = integer_param("b");
    b.def_id = Some(rumoca_core::DefId::new(2));
    let mut y = integer_param("y");
    y.def_id = Some(rumoca_core::DefId::new(3));
    let mut func = Function::new(
        "test.identity_default",
        rumoca_core::DefId::new(207),
        test_span(),
    );
    func.add_input(a);
    func.add_input(b);
    func.add_output(y);
    func.body = vec![assign(
        component_reference("y"),
        resolved_reference("a", 1, 1),
    )];

    let Ok(inventory) = ResolvedIdentityInventory::try_from_bindings(
        vec![ResolvedValueBinding {
            identity: ResolvedOccurrenceKey {
                instance_id: rumoca_core::InstanceId::new(990),
                root_def_id: rumoca_core::DefId::new(99),
            },
            value: Value::Integer(99),
        }],
        Vec::new(),
    ) else {
        panic!("fixture identity inventory must be unique");
    };
    let mut ctx = EvalContext::resolved(1, 0, inventory, ResolvedEnumCatalog::empty());
    ctx.add_parameter("b".to_string(), Value::Integer(41));
    let Expression::VarRef {
        name: exact_outer, ..
    } = &outer_reference
    else {
        panic!("fixture outer reference must remain exact");
    };
    assert_eq!(ctx.get_reference(exact_outer), Some(&Value::Integer(99)));
    let unknown_outer = resolved_reference("b", 98, 991);
    let Expression::VarRef {
        name: unknown_exact_outer,
        ..
    } = &unknown_outer
    else {
        panic!("fixture unknown reference must remain exact");
    };
    assert_eq!(ctx.get_reference(unknown_exact_outer), None);
    let Ok(result) = eval_function_with_call_args(
        &func,
        vec![FunctionCallArg::named("b".to_string(), Value::Integer(2))],
        &ctx,
        &EvalLimits::default(),
        0,
        test_span(),
    ) else {
        panic!("foreign resolved default must read the outer value");
    };

    assert_eq!(result, Value::Integer(99));
}

#[test]
fn enumeration_ranges_inside_functions_defer_to_typed_lowering() {
    let error = crate::constant::range_eval::eval_value_range(
        &Value::Enum("Color".to_string(), "red".to_string()),
        None,
        &Value::Enum("Color".to_string(), "blue".to_string()),
        test_span(),
    )
    .expect_err("the untyped function interpreter must preserve enum-range ownership");
    assert!(matches!(error, EvalError::UnsupportedExpression { .. }));
}

fn empty_function_environment() -> FunctionEnv {
    FunctionEnv {
        declarations: Vec::new(),
        formal_extents: Vec::new(),
        loop_bindings: Vec::new(),
        inputs: IndexMap::new(),
        outputs: IndexMap::new(),
        locals: IndexMap::new(),
        declared_outputs: IndexSet::new(),
        declared_locals: IndexSet::new(),
    }
}

#[test]
fn noninteger_range_selectors_defer_for_function_reads_and_writes() {
    let span = test_span();
    let mut context = EvalContext::structural_preidentity();
    context.add_parameter(
        "first_mode",
        Value::Enum("Mode".to_string(), "off".to_string()),
    );
    context.add_parameter(
        "last_mode",
        Value::Enum("Mode".to_string(), "on".to_string()),
    );
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &context,
        limits: &limits,
        depth: 0,
        span,
    };
    let env = empty_function_environment();
    let boolean_range = Subscript::Expr {
        expr: Box::new(Expression::Range {
            start: Box::new(Expression::Literal {
                value: Literal::Boolean(false),
                span,
            }),
            step: None,
            end: Box::new(Expression::Literal {
                value: Literal::Boolean(true),
                span,
            }),
            span,
        }),
        span,
    };
    let enum_bound = |name: &str| Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span,
    };
    let enumeration_range = Subscript::Expr {
        expr: Box::new(Expression::Range {
            start: Box::new(enum_bound("first_mode")),
            step: None,
            end: Box::new(enum_bound("last_mode")),
            span,
        }),
        span,
    };
    let target = Value::Array(vec![Value::Integer(10), Value::Integer(20)]);
    let assigned = Value::Array(vec![Value::Integer(1), Value::Integer(2)]);

    for subscript in [boolean_range, enumeration_range] {
        assert!(matches!(
            apply_subscripts_flat(
                target.clone(),
                std::slice::from_ref(&subscript),
                &env,
                &eval
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
        assert!(matches!(
            set_array_selection(
                target.clone(),
                std::slice::from_ref(&subscript),
                assigned.clone(),
                &env,
                &eval,
            ),
            Err(EvalError::UnsupportedExpression { .. })
        ));
    }

    let integer_range = range_subscript(1, None, 2);
    assert_eq!(
        apply_subscripts_flat(
            target.clone(),
            std::slice::from_ref(&integer_range),
            &env,
            &eval,
        )
        .unwrap(),
        target
    );
    assert_eq!(
        set_array_selection(
            Value::Array(vec![Value::Integer(0), Value::Integer(0)]),
            std::slice::from_ref(&integer_range),
            assigned.clone(),
            &env,
            &eval,
        )
        .unwrap(),
        assigned
    );
}
