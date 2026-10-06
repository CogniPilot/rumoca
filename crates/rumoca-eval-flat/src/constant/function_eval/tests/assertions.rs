use super::*;

fn literal(value: Literal) -> Expression {
    Expression::Literal {
        value,
        span: test_span(),
    }
}

fn assertion(condition: Expression, message: Expression) -> Statement {
    Statement::Assert {
        condition,
        message: Box::new(message),
        level: None,
        span: assertion_span(),
    }
}

fn assertion_span() -> Span {
    Span::from_offsets(test_span().source, 8, 9)
}

fn function_with(statements: Vec<Statement>) -> Function {
    let mut function = Function::new("asserted", test_span());
    function.outputs.push(integer_param("result"));
    function.body = statements;
    let comp = ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: "result".into(),
            span: test_span(),
            subs: vec![],
            def_id: rumoca_core::DefId::new(17),
        }],
    )
    .unwrap();
    function.body.push(Statement::Assignment {
        comp,
        value: literal(Literal::Integer(7)),
        span: test_span(),
    });
    function
}

fn evaluate(function: &Function) -> Result<Value, EvalError> {
    eval_function(
        function,
        vec![],
        &EvalContext::new(),
        &EvalLimits::default(),
        0,
        test_span(),
    )
}

fn false_assertion() -> Statement {
    assertion(
        literal(Literal::Boolean(false)),
        literal(Literal::String("selected failure".into())),
    )
}

#[test]
fn false_default_assertion_aborts_with_its_message_and_statement_provenance() {
    let error = evaluate(&function_with(vec![false_assertion()])).unwrap_err();
    assert!(
        matches!(&error, EvalError::FunctionError { message, span } if message == "selected failure" && *span == assertion_span())
    );
}

#[test]
fn true_assertion_does_not_evaluate_its_message() {
    let invalid_message = Expression::VarRef {
        name: exact_reference("unboundMessage", rumoca_core::DefId::new(18)),
        subscripts: vec![],
        span: test_span(),
    };
    assert_eq!(
        evaluate(&function_with(vec![assertion(
            literal(Literal::Boolean(true)),
            invalid_message
        )]))
        .unwrap(),
        Value::Integer(7)
    );
}

#[test]
fn false_assertion_inside_an_inactive_branch_is_not_evaluated() {
    let skipped = Statement::If {
        cond_blocks: vec![StatementBlock {
            cond: literal(Literal::Boolean(false)),
            stmts: vec![false_assertion()],
        }],
        else_block: None,
        span: test_span(),
    };
    assert_eq!(
        evaluate(&function_with(vec![skipped])).unwrap(),
        Value::Integer(7)
    );
}

#[test]
fn false_assertion_inside_an_executed_loop_aborts() {
    let loop_statement = Statement::For {
        indices: vec![ForIndex {
            ident: "i".into(),
            range: Expression::Range {
                start: Box::new(literal(Literal::Integer(1))),
                step: None,
                end: Box::new(literal(Literal::Integer(2))),
                span: test_span(),
            },
        }],
        equations: vec![false_assertion()],
        span: test_span(),
    };
    assert!(
        matches!(evaluate(&function_with(vec![loop_statement])), Err(EvalError::FunctionError { message, .. }) if message == "selected failure")
    );
}

#[test]
fn call_spelling_has_the_same_error_owner_and_lazy_message_semantics() {
    let call = |condition, message| Statement::FunctionCall {
        comp: Reference::new("assert"),
        args: vec![condition, message],
        outputs: vec![],
        span: assertion_span(),
    };
    let error = evaluate(&function_with(vec![call(
        literal(Literal::Boolean(false)),
        literal(Literal::String("call failure".into())),
    )]))
    .unwrap_err();
    assert!(
        matches!(error, EvalError::FunctionError { message, span } if message == "call failure" && span == assertion_span())
    );
    let invalid = Expression::VarRef {
        name: exact_reference("unbound", rumoca_core::DefId::new(18)),
        subscripts: vec![],
        span: test_span(),
    };
    assert_eq!(
        evaluate(&function_with(vec![call(
            literal(Literal::Boolean(true)),
            invalid
        )]))
        .unwrap(),
        Value::Integer(7)
    );
}

fn level_function(level: &str, call_spelling: bool) -> (Function, Value) {
    let mut function = function_with(vec![]);
    function.inputs.push(function_param(
        "level",
        "AssertionLevel",
        rumoca_core::TypeId::new(3),
    ));
    let condition = literal(Literal::Boolean(false));
    let message = literal(Literal::String("level failure".into()));
    let level_expression = Expression::VarRef {
        name: exact_reference("level", rumoca_core::DefId::new(19)),
        subscripts: vec![],
        span: test_span(),
    };
    let statement = if call_spelling {
        Statement::FunctionCall {
            comp: Reference::new("assert"),
            args: vec![condition, message, level_expression],
            outputs: vec![],
            span: assertion_span(),
        }
    } else {
        Statement::Assert {
            condition,
            message: Box::new(message),
            level: Some(Box::new(level_expression)),
            span: assertion_span(),
        }
    };
    function.body.insert(0, statement);
    (function, Value::Enum("AssertionLevel".into(), level.into()))
}

#[test]
fn explicit_error_level_is_not_ignored_in_either_spelling() {
    for call in [false, true] {
        let (function, argument) = level_function("error", call);
        let result = eval_function(
            &function,
            vec![argument],
            &EvalContext::new(),
            &EvalLimits::default(),
            0,
            test_span(),
        );
        assert!(
            matches!(result, Err(EvalError::FunctionError { message, .. }) if message == "level failure")
        );
    }
}

#[test]
fn warning_actions_are_deferred_instead_of_silently_disappearing_in_a_constant() {
    for call in [false, true] {
        let (function, argument) = level_function("warning", call);
        let result = eval_function(
            &function,
            vec![argument],
            &EvalContext::new(),
            &EvalLimits::default(),
            0,
            test_span(),
        );
        assert!(
            matches!(result, Err(EvalError::NotConstant { reason, .. }) if reason.contains("reporting action"))
        );
    }
}

#[test]
fn assertion_operands_and_receiving_slots_keep_strict_type_checks() {
    let bad_condition = assertion(
        literal(Literal::Integer(1)),
        literal(Literal::String("invalid".into())),
    );
    assert!(
        matches!(evaluate(&function_with(vec![bad_condition])), Err(EvalError::TypeMismatch { expected, .. }) if expected == "Boolean")
    );
    let bad_message = assertion(
        literal(Literal::Boolean(false)),
        literal(Literal::Integer(1)),
    );
    assert!(
        matches!(evaluate(&function_with(vec![bad_message])), Err(EvalError::TypeMismatch { expected, .. }) if expected == "String")
    );
    let (function, _) = level_function("error", false);
    let result = eval_function(
        &function,
        vec![Value::Integer(1)],
        &EvalContext::new(),
        &EvalLimits::default(),
        0,
        test_span(),
    );
    assert!(
        matches!(result, Err(EvalError::TypeMismatch { expected, .. }) if expected == "AssertionLevel")
    );
    let call = Statement::FunctionCall {
        comp: Reference::new("assert"),
        args: vec![
            literal(Literal::Boolean(true)),
            literal(Literal::String("".into())),
        ],
        outputs: vec![None],
        span: assertion_span(),
    };
    assert!(
        matches!(evaluate(&function_with(vec![call])), Err(EvalError::FunctionError { message, .. }) if message == "assert has no result")
    );
}
