use super::*;

#[test]
fn function_comprehension_inactive_source_id_cannot_read_same_name_input() {
    let ctx = EvalContext::new();
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &ctx,
        limits: &limits,
        depth: 0,
        span: test_span(),
    };
    let id = rumoca_core::DefId::new(42);
    let env = FunctionEnv {
        inputs: IndexMap::from([("i".to_string(), Value::Integer(9))]),
        outputs: IndexMap::new(),
        locals: IndexMap::new(),
        source_iterators: Vec::new(),
        source_iterator_declarations: std::collections::HashSet::new(),
    };
    let own_ref = Expression::VarRef {
        name: exact_reference("i", id),
        subscripts: Vec::new(),
        span: test_span(),
    };
    let index = ComprehensionIndex {
        binder: rumoca_core::IndexBinderProvenance::Source(id),
        name: "i".into(),
        range: own_ref.clone(),
    };
    let error = eval_array_comprehension(&own_ref, &[index], &None, &env, &eval).unwrap_err();
    assert!(
        matches!(error, EvalError::UnknownVariable { name, span } if name == "i" && span == test_span())
    );
    assert_eq!(env.inputs["i"], Value::Integer(9));
}

#[test]
fn function_comprehension_source_values_keep_same_name_input_and_iterator_separate() {
    let ctx = EvalContext::new();
    let limits = EvalLimits::default();
    let eval = EvalState {
        ctx: &ctx,
        limits: &limits,
        depth: 0,
        span: test_span(),
    };
    let env = FunctionEnv {
        inputs: IndexMap::from([("i".to_string(), Value::Integer(9))]),
        outputs: IndexMap::new(),
        locals: IndexMap::new(),
        source_iterators: Vec::new(),
        source_iterator_declarations: std::collections::HashSet::new(),
    };
    let index = ComprehensionIndex {
        binder: rumoca_core::IndexBinderProvenance::Source(rumoca_core::DefId::new(42)),
        name: "i".into(),
        range: Expression::Range {
            start: Box::new(Expression::Literal {
                value: Literal::Integer(1),
                span: test_span(),
            }),
            step: None,
            end: Box::new(Expression::Literal {
                value: Literal::Integer(2),
                span: test_span(),
            }),
            span: test_span(),
        },
    };
    for (id, expected) in [
        (41, vec![Value::Integer(9), Value::Integer(9)]),
        (42, vec![Value::Integer(1), Value::Integer(2)]),
    ] {
        let expression = Expression::VarRef {
            name: exact_reference("i", rumoca_core::DefId::new(id)),
            subscripts: Vec::new(),
            span: test_span(),
        };
        assert_eq!(
            eval_array_comprehension(
                &expression,
                std::slice::from_ref(&index),
                &None,
                &env,
                &eval
            )
            .unwrap(),
            Value::Array(expected)
        );
    }
}
