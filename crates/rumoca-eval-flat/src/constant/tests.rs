//! Unit tests for the constant expression evaluator.

use rumoca_core::EvalLookup;

use super::range_eval::{collect_int_range, collect_real_range};
use super::*;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("eval_flat_constant_source_7.mo"),
        0,
        1,
    )
}

fn make_int(v: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(v),
        span: test_span(),
    }
}

fn make_real(v: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(v),
        span: test_span(),
    }
}

fn make_bool(v: bool) -> Expression {
    Expression::Literal {
        value: Literal::Boolean(v),
        span: test_span(),
    }
}

fn required_input_function() -> Function {
    let real = rumoca_core::TypeId::new(1);
    let effective = rumoca_core::EffectiveType::new(real, real, Vec::new())
        .expect("fixture scalar type is valid");
    let mut function = Function::new(
        "test.required",
        rumoca_core::DefId::new(11_001),
        test_span(),
    );
    function.pure = true;
    function.add_input(rumoca_core::FunctionParam::new(
        "x",
        "Real",
        effective,
        test_span(),
    ));
    function
}

fn make_vector(values: &[i64]) -> Expression {
    Expression::Array {
        elements: values.iter().map(|v| make_int(*v)).collect(),
        is_matrix: false,
        span: test_span(),
    }
}

fn make_matrix(rows: &[&[i64]]) -> Expression {
    Expression::Array {
        elements: rows
            .iter()
            .map(|row| Expression::Array {
                elements: row.iter().map(|v| make_int(*v)).collect(),
                is_matrix: false,
                span: test_span(),
            })
            .collect(),
        is_matrix: true,
        span: test_span(),
    }
}

fn enum_reference(
    rendered: &str,
    qualifier: &str,
    qualifier_id: rumoca_core::DefId,
    literal: &str,
    enum_id: rumoca_core::DefId,
) -> rumoca_core::Reference {
    rumoca_core::Reference::with_component_reference(
        rendered,
        rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![
                rumoca_core::ComponentRefPart {
                    ident: qualifier.to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: qualifier_id,
                },
                rumoca_core::ComponentRefPart {
                    ident: literal.to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: enum_id,
                },
            ],
        )
        .expect("fixture enum reference has exact identity"),
    )
}

#[test]
fn test_eval_literal() {
    let ctx = EvalContext::structural_preidentity();

    let expr = make_int(42);
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(42));

    let expr = make_real(2.5);
    let result = eval_expr(&expr, &ctx).unwrap();
    assert!((result.as_real().unwrap() - 2.5).abs() < 1e-10);

    let expr = make_bool(true);
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_bool(), Some(true));
}

#[test]
fn test_eval_expr_requires_source_provenance() {
    let ctx = EvalContext::structural_preidentity();
    let expr = Expression::Literal {
        value: Literal::Integer(42),
        span: rumoca_core::Span::DUMMY,
    };

    let err = eval_expr(&expr, &ctx).expect_err("unspanned constants should fail fast");
    assert!(matches!(err, EvalError::MissingSourceContext { .. }));
}

#[test]
fn test_eval_expr_rejects_empty_expression_with_source_span() {
    let ctx = EvalContext::structural_preidentity();
    let span = test_span();
    let expr = Expression::Empty { span };

    let error = eval_expr(&expr, &ctx).expect_err("empty semantic IR must fail closed");
    assert!(matches!(&error, EvalError::InvalidSemanticIr { .. }));
    assert_eq!(error.span(), Some(span));

    let literal = Expression::Literal {
        value: Literal::Integer(7),
        span,
    };
    assert_eq!(
        eval_expr(&literal, &ctx).expect("a spanned literal remains a legal constant"),
        Value::Integer(7)
    );
}

#[test]
fn structural_sentinels_fail_even_in_dead_expression_paths() {
    let span = test_span();
    let empty = || Expression::Empty { span };
    let binary = |op, lhs, rhs| Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    };
    let expressions = [
        Expression::Unary {
            op: OpUnary::Empty,
            rhs: Box::new(make_int(1)),
            span,
        },
        binary(OpBinary::Empty, make_int(1), make_int(2)),
        binary(OpBinary::Assign, make_int(1), make_int(2)),
        binary(OpBinary::And, make_bool(false), empty()),
        binary(OpBinary::Or, make_bool(true), empty()),
        Expression::If {
            branches: vec![(make_bool(true), make_int(1))],
            else_branch: Box::new(empty()),
            span,
        },
    ];

    for expression in expressions {
        assert!(
            matches!(
                eval_optional(&expression, &EvalContext::structural_preidentity()),
                Err(EvalError::InvalidSemanticIr { .. })
            ),
            "invalid structural IR must not be hidden by control flow: {expression:?}"
        );
    }
}

#[test]
fn test_eval_binary() {
    let ctx = EvalContext::structural_preidentity();

    // 3 + 4 = 7
    let expr = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(make_int(3)),
        rhs: Box::new(make_int(4)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(7));

    // 10 - 3 = 7
    let expr = Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(make_int(10)),
        rhs: Box::new(make_int(3)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(7));

    // 3 * 4 = 12
    let expr = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(make_int(3)),
        rhs: Box::new(make_int(4)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(12));

    // 10 / 4 = 2.5 (Real result in Modelica)
    let expr = Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(make_int(10)),
        rhs: Box::new(make_int(4)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert!((result.as_real().unwrap() - 2.5).abs() < 1e-10);
}

#[test]
fn test_eval_binary_integer_overflow_returns_error() {
    let ctx = EvalContext::structural_preidentity();
    let expr = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(make_int(i64::MAX)),
        rhs: Box::new(make_int(1)),
        span: test_span(),
    };
    let err = eval_expr(&expr, &ctx).unwrap_err();
    assert!(
        err.to_string()
            .contains("compile-time integer overflow while evaluating integer addition")
    );
}

#[test]
fn test_eval_mul_vs_mul_elem_vector_semantics() {
    let ctx = EvalContext::structural_preidentity();
    let lhs = make_vector(&[1, 2, 3]);
    let rhs = make_vector(&[4, 5, 6]);

    // `*` performs dot-product on vectors.
    let mul_expr = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs.clone()),
        rhs: Box::new(rhs.clone()),
        span: test_span(),
    };
    let mul_result = eval_expr(&mul_expr, &ctx).unwrap();
    assert_eq!(mul_result, Value::Integer(32));

    // `.*` keeps element-wise vector semantics.
    let mul_elem_expr = Expression::Binary {
        op: OpBinary::MulElem,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    };
    let mul_elem_result = eval_expr(&mul_elem_expr, &ctx).unwrap();
    assert_eq!(
        mul_elem_result,
        Value::Array(vec![
            Value::Integer(4),
            Value::Integer(10),
            Value::Integer(18)
        ])
    );
}

#[test]
fn test_eval_matrix_multiplication_semantics() {
    let ctx = EvalContext::structural_preidentity();

    // [[1,2],[3,4]] * [[5,6],[7,8]] = [[19,22],[43,50]]
    let lhs_matrix = make_matrix(&[&[1, 2], &[3, 4]]);
    let rhs_matrix = make_matrix(&[&[5, 6], &[7, 8]]);
    let matrix_mul_expr = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs_matrix.clone()),
        rhs: Box::new(rhs_matrix.clone()),
        span: test_span(),
    };
    let matrix_mul_result = eval_expr(&matrix_mul_expr, &ctx).unwrap();
    assert_eq!(
        matrix_mul_result,
        Value::Array(vec![
            Value::Array(vec![Value::Integer(19), Value::Integer(22)]),
            Value::Array(vec![Value::Integer(43), Value::Integer(50)])
        ])
    );

    // Element-wise matrix multiply remains shape-preserving.
    let matrix_mul_elem_expr = Expression::Binary {
        op: OpBinary::MulElem,
        lhs: Box::new(lhs_matrix),
        rhs: Box::new(rhs_matrix),
        span: test_span(),
    };
    let matrix_mul_elem_result = eval_expr(&matrix_mul_elem_expr, &ctx).unwrap();
    assert_eq!(
        matrix_mul_elem_result,
        Value::Array(vec![
            Value::Array(vec![Value::Integer(5), Value::Integer(12)]),
            Value::Array(vec![Value::Integer(21), Value::Integer(32)])
        ])
    );
}

#[test]
fn test_eval_comparison() {
    let ctx = EvalContext::structural_preidentity();

    // 3 < 4 = true
    let expr = Expression::Binary {
        op: OpBinary::Lt,
        lhs: Box::new(make_int(3)),
        rhs: Box::new(make_int(4)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_bool(), Some(true));

    // 3 == 3 = true
    let expr = Expression::Binary {
        op: OpBinary::Eq,
        lhs: Box::new(make_int(3)),
        rhs: Box::new(make_int(3)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_bool(), Some(true));
}

#[test]
fn test_eval_unary() {
    let ctx = EvalContext::structural_preidentity();

    // -5
    let expr = Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(make_int(5)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(-5));

    // not true = false
    let expr = Expression::Unary {
        op: OpUnary::Not,
        rhs: Box::new(make_bool(true)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_bool(), Some(false));
}

#[test]
fn test_eval_array() {
    let ctx = EvalContext::structural_preidentity();

    let expr = Expression::Array {
        elements: vec![make_int(1), make_int(2), make_int(3)],
        is_matrix: false,
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_integer(), Some(1));
    assert_eq!(arr[2].as_integer(), Some(3));
}

#[test]
fn test_eval_range() {
    let ctx = EvalContext::structural_preidentity();

    // 1:5 = {1, 2, 3, 4, 5}
    let expr = Expression::Range {
        start: Box::new(make_int(1)),
        step: None,
        end: Box::new(make_int(5)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 5);
    assert_eq!(arr[0].as_integer(), Some(1));
    assert_eq!(arr[4].as_integer(), Some(5));

    // 1:2:5 = {1, 3, 5}
    let expr = Expression::Range {
        start: Box::new(make_int(1)),
        step: Some(Box::new(make_int(2))),
        end: Box::new(make_int(5)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    let arr = result.as_array().unwrap();
    assert_eq!(arr.len(), 3);
    assert_eq!(arr[0].as_integer(), Some(1));
    assert_eq!(arr[1].as_integer(), Some(3));
    assert_eq!(arr[2].as_integer(), Some(5));
}

#[test]
fn test_integer_range_stops_cleanly_at_machine_bounds() {
    assert_eq!(
        collect_int_range(i64::MAX, i64::MAX, 1, test_span()).unwrap(),
        vec![Value::Integer(i64::MAX)]
    );
    assert_eq!(
        collect_int_range(i64::MIN, i64::MIN, -1, test_span()).unwrap(),
        vec![Value::Integer(i64::MIN)]
    );
}

#[test]
fn integer_range_materialization_obeys_the_evaluation_budget() {
    assert!(matches!(
        collect_int_range(0, DEFAULT_EVAL_BUDGET as i64, 1, test_span()),
        Err(EvalError::UnsupportedExpression { .. })
    ));
}

#[test]
fn enumeration_range_defers_to_its_typed_semantic_owner() {
    let span = test_span();
    let mut context = EvalContext::structural_preidentity();
    context.add_parameter(
        "first",
        Value::Enum("Pkg.Level".to_string(), "low".to_string()),
    );
    context.add_parameter(
        "last",
        Value::Enum("Pkg.Level".to_string(), "high".to_string()),
    );
    let range = Expression::Range {
        start: Box::new(Expression::VarRef {
            name: "first".into(),
            subscripts: Vec::new(),
            span,
        }),
        step: None,
        end: Box::new(Expression::VarRef {
            name: "last".into(),
            subscripts: Vec::new(),
            span,
        }),
        span,
    };

    assert!(matches!(
        eval_expr(&range, &context),
        Err(EvalError::UnsupportedExpression { .. })
    ));
    assert_eq!(eval_optional(&range, &context).unwrap(), None);
}

#[test]
fn test_real_range_allows_repeated_binary64_values() {
    let start = 1.0e20_f64;
    let end = f64::from_bits(start.to_bits() + 1);

    let values = collect_real_range(start, end, 1.0, test_span()).unwrap();
    assert_eq!(values.len(), 16_385);
    assert_eq!(values.first(), Some(&Value::Real(start)));
    assert_eq!(values.last(), Some(&Value::Real(end)));
}

#[test]
fn test_eval_if() {
    let ctx = EvalContext::structural_preidentity();

    // if true then 1 else 2
    let expr = Expression::If {
        branches: vec![(make_bool(true), make_int(1))],
        else_branch: Box::new(make_int(2)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(1));

    // if false then 1 else 2
    let expr = Expression::If {
        branches: vec![(make_bool(false), make_int(1))],
        else_branch: Box::new(make_int(2)),
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(2));
}

#[test]
fn test_eval_if_does_not_ignore_unknown_earlier_branch() {
    let ctx = EvalContext::structural_preidentity();
    let unknown = Expression::VarRef {
        name: "unknown".into(),
        subscripts: vec![],
        span: test_span(),
    };
    let expr = Expression::If {
        branches: vec![(unknown, make_int(1)), (make_bool(true), make_int(2))],
        else_branch: Box::new(make_int(2)),
        span: test_span(),
    };

    assert!(matches!(
        eval_expr(&expr, &ctx),
        Err(EvalError::NotConstant { .. })
    ));
}

#[test]
fn test_eval_if_folds_equal_unknown_and_selected_outcomes() {
    let ctx = EvalContext::structural_preidentity();
    let unknown = Expression::VarRef {
        name: "unknown".into(),
        subscripts: vec![],
        span: test_span(),
    };
    let expr = Expression::If {
        branches: vec![(unknown, make_int(2)), (make_bool(true), make_int(2))],
        else_branch: Box::new(make_int(3)),
        span: test_span(),
    };

    assert_eq!(eval_expr(&expr, &ctx).unwrap(), Value::Integer(2));
}

#[test]
fn unknown_earlier_if_control_dominates_runtime_errors_in_later_paths() {
    let span = test_span();
    let unknown = Expression::VarRef {
        name: "runtime_condition".into(),
        subscripts: Vec::new(),
        span,
    };
    let division_by_zero = || Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(make_int(1)),
        rhs: Box::new(make_int(0)),
        span,
    };
    let selected_branch_error = Expression::If {
        branches: vec![
            (unknown.clone(), make_int(1)),
            (make_bool(true), division_by_zero()),
        ],
        else_branch: Box::new(make_int(0)),
        span,
    };
    let later_condition_error = Expression::If {
        branches: vec![(unknown, make_int(1)), (division_by_zero(), make_int(2))],
        else_branch: Box::new(make_int(0)),
        span,
    };

    for expression in [selected_branch_error, later_condition_error] {
        assert!(matches!(
            eval_expr(&expression, &EvalContext::structural_preidentity()),
            Err(EvalError::UnknownVariable { name, .. }) if name == "runtime_condition"
        ));
    }
}

#[test]
fn test_eval_if_requires_exactly_equal_real_outcomes() {
    let ctx = EvalContext::structural_preidentity();
    let unknown = Expression::VarRef {
        name: "unknown".into(),
        subscripts: vec![],
        span: test_span(),
    };
    let next_real = f64::from_bits(1.0f64.to_bits() + 1);
    let expr = Expression::If {
        branches: vec![(unknown, make_real(1.0))],
        else_branch: Box::new(make_real(next_real)),
        span: test_span(),
    };

    assert!(matches!(
        eval_expr(&expr, &ctx),
        Err(EvalError::NotConstant { .. })
    ));
}

#[test]
fn test_logical_operators_fold_determining_value_with_unknown_operand() {
    let ctx = EvalContext::structural_preidentity();
    let unknown = || Expression::VarRef {
        name: "unknown".into(),
        subscripts: vec![],
        span: test_span(),
    };
    let binary = |op, lhs, rhs| Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    };

    for expression in [
        binary(OpBinary::And, make_bool(false), unknown()),
        binary(OpBinary::And, unknown(), make_bool(false)),
    ] {
        assert_eq!(eval_expr(&expression, &ctx).unwrap(), Value::Bool(false));
    }
    for expression in [
        binary(OpBinary::Or, make_bool(true), unknown()),
        binary(OpBinary::Or, unknown(), make_bool(true)),
    ] {
        assert_eq!(eval_expr(&expression, &ctx).unwrap(), Value::Bool(true));
    }
}

#[test]
fn logical_operators_never_hide_hard_rhs_errors_behind_an_unknown_lhs() {
    let ctx = EvalContext::structural_preidentity();
    let lhs_span = test_span();
    let rhs_span = rumoca_core::Span::from_offsets(lhs_span.source, 4, 5);
    let unknown = || Expression::VarRef {
        name: "runtime_value".into(),
        subscripts: vec![],
        span: lhs_span,
    };
    let invalid = || Expression::Empty { span: rhs_span };

    for op in [OpBinary::And, OpBinary::Or] {
        let expression = Expression::Binary {
            op,
            lhs: Box::new(unknown()),
            rhs: Box::new(invalid()),
            span: lhs_span,
        };
        let error = eval_optional(&expression, &ctx)
            .expect_err("a hard RHS error must outrank an undetermined LHS");
        assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
        assert_eq!(error.span(), Some(rhs_span));
    }
}

#[test]
fn unknown_control_never_hides_malformed_calls() {
    let span = test_span();
    let call_span = rumoca_core::Span::from_offsets(span.source, 8, 12);
    let unknown = || Expression::VarRef {
        name: "runtime_value".into(),
        subscripts: vec![],
        span,
    };
    let mut ctx = EvalContext::structural_preidentity();
    ctx.insert_direct_function_fixture(required_input_function());

    for user_function in [true, false] {
        let malformed = || {
            if user_function {
                Expression::FunctionCall {
                    name: "test.required".into(),
                    args: Vec::new(),
                    is_constructor: false,
                    call_kind: rumoca_core::FunctionCallKind::Invocation,
                    span: call_span,
                }
            } else {
                Expression::BuiltinCall {
                    function: BuiltinFunction::Size,
                    args: Vec::new(),
                    span: call_span,
                }
            }
        };
        for op in [OpBinary::And, OpBinary::Or] {
            let expression = Expression::Binary {
                op,
                lhs: Box::new(unknown()),
                rhs: Box::new(malformed()),
                span,
            };
            let error = eval_optional(&expression, &ctx)
                .expect_err("a malformed call must outrank unknown logical control");
            assert_eq!(error.span(), Some(call_span));
        }

        for expression in [
            Expression::If {
                branches: vec![(unknown(), malformed())],
                else_branch: Box::new(make_int(7)),
                span,
            },
            Expression::If {
                branches: vec![(unknown(), make_int(7))],
                else_branch: Box::new(malformed()),
                span,
            },
        ] {
            let error = eval_optional(&expression, &ctx)
                .expect_err("a malformed call must outrank unknown conditional control");
            assert_eq!(error.span(), Some(call_span));
        }
    }
}

#[test]
fn cat_dimension_errors_never_become_optional_success() {
    for dimension in [-1, 0] {
        let expression = Expression::BuiltinCall {
            function: BuiltinFunction::Cat,
            args: vec![
                make_int(dimension),
                Expression::Array {
                    elements: Vec::new(),
                    is_matrix: false,
                    span: test_span(),
                },
                Expression::Array {
                    elements: Vec::new(),
                    is_matrix: false,
                    span: test_span(),
                },
            ],
            span: test_span(),
        };
        assert!(matches!(
            eval_optional(&expression, &EvalContext::structural_preidentity()),
            Err(EvalError::RangeError { .. })
        ));
    }
}

#[test]
fn unknown_control_defers_branch_local_model_errors() {
    let span = test_span();
    let unknown = || Expression::VarRef {
        name: "runtime_value".into(),
        subscripts: vec![],
        span,
    };
    let divide_by_zero = || Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(make_int(1)),
        rhs: Box::new(make_int(0)),
        span,
    };

    let conditional = Expression::If {
        branches: vec![(unknown(), divide_by_zero())],
        else_branch: Box::new(make_int(7)),
        span,
    };
    assert_eq!(
        eval_optional(&conditional, &EvalContext::structural_preidentity()).unwrap(),
        None,
        "a domain error in an unselected runtime branch is not a compile-time model error"
    );

    for op in [OpBinary::And, OpBinary::Or] {
        let logical = Expression::Binary {
            op,
            lhs: Box::new(unknown()),
            rhs: Box::new(divide_by_zero()),
            span,
        };
        assert_eq!(
            eval_optional(&logical, &EvalContext::structural_preidentity()).unwrap(),
            None
        );
    }
}

#[test]
fn test_eval_parameter() {
    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("n", Value::Integer(10));
    ctx.add_parameter("x", Value::Real(2.5));

    let expr = Expression::VarRef {
        name: "n".into(),
        subscripts: vec![],
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(10));

    let expr = Expression::VarRef {
        name: "x".into(),
        subscripts: vec![],
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert!((result.as_real().unwrap() - 2.5).abs() < 1e-10);
}

#[test]
fn test_eval_builtin_call() {
    let ctx = EvalContext::structural_preidentity();

    // abs(-5) = 5
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Abs,
        args: vec![make_int(-5)],
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert_eq!(result.as_integer(), Some(5));

    // sqrt(4.0) = 2.0
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Sqrt,
        args: vec![make_real(4.0)],
        span: test_span(),
    };
    let result = eval_expr(&expr, &ctx).unwrap();
    assert!((result.as_real().unwrap() - 2.0).abs() < 1e-10);
}

#[test]
fn passthrough_builtins_use_the_modelica_argument_contract() {
    let span = test_span();
    let ctx = EvalContext::structural_preidentity();
    let cases = [
        (BuiltinFunction::NoEvent, vec![make_int(11)], 11),
        (BuiltinFunction::Smooth, vec![make_int(1), make_int(12)], 12),
        (
            BuiltinFunction::Homotopy,
            vec![make_int(13), make_int(99)],
            13,
        ),
        (BuiltinFunction::Delay, vec![make_int(14), make_int(1)], 14),
        (
            BuiltinFunction::Delay,
            vec![make_int(15), make_int(1), make_int(2)],
            15,
        ),
    ];

    for (function, args, expected) in cases {
        let expression = Expression::BuiltinCall {
            function,
            args,
            span,
        };
        assert_eq!(
            eval_expr(&expression, &ctx).expect("a valid pass-through call folds"),
            Value::Integer(expected),
            "{} returned the wrong operand",
            function.name()
        );
    }
}

#[test]
fn passthrough_builtin_arity_is_rejected_before_dispatch() {
    let span = test_span();
    let call_span = rumoca_core::Span::from_offsets(span.source, 20, 30);
    let cases = [
        (BuiltinFunction::NoEvent, Vec::new()),
        (BuiltinFunction::NoEvent, vec![make_int(1), make_int(2)]),
        (BuiltinFunction::Smooth, vec![make_int(1)]),
        (
            BuiltinFunction::Smooth,
            vec![make_int(1), make_int(2), make_int(3)],
        ),
        (BuiltinFunction::Homotopy, vec![make_int(1)]),
        (
            BuiltinFunction::Homotopy,
            vec![make_int(1), make_int(2), make_int(3)],
        ),
        (BuiltinFunction::Delay, vec![make_int(1)]),
        (
            BuiltinFunction::Delay,
            vec![make_int(1), make_int(2), make_int(3), make_int(4)],
        ),
    ];

    for (function, args) in cases {
        let expression = Expression::BuiltinCall {
            function,
            args,
            span: call_span,
        };
        let error = eval_expr(&expression, &EvalContext::structural_preidentity())
            .expect_err("an invalid builtin signature must fail closed");
        assert!(
            matches!(error, EvalError::WrongArgCount { .. }),
            "{} returned {error:?}",
            function.name()
        );
        assert_eq!(error.span(), Some(call_span));
    }
}

#[test]
fn known_dead_expression_branch_cannot_hide_malformed_builtin_arity() {
    let span = test_span();
    let call_span = rumoca_core::Span::from_offsets(span.source, 40, 50);
    let expression = Expression::If {
        branches: vec![(make_bool(true), make_int(7))],
        else_branch: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Homotopy,
            args: vec![make_int(1)],
            span: call_span,
        }),
        span,
    };

    let error = eval_expr(&expression, &EvalContext::structural_preidentity())
        .expect_err("whole-expression validation must inspect known-dead branches");
    assert!(matches!(error, EvalError::WrongArgCount { .. }));
    assert_eq!(error.span(), Some(call_span));
}

#[test]
fn cat_and_optional_subclock_factor_arity_is_enforced() {
    let span = test_span();
    let array = || Expression::Array {
        elements: vec![make_int(1)],
        is_matrix: false,
        span,
    };
    let malformed = [
        (BuiltinFunction::Cat, vec![make_int(1), array()]),
        (BuiltinFunction::SubSample, Vec::new()),
        (
            BuiltinFunction::SubSample,
            vec![make_int(1), make_int(2), make_int(3)],
        ),
        (BuiltinFunction::SuperSample, Vec::new()),
        (
            BuiltinFunction::SuperSample,
            vec![make_int(1), make_int(2), make_int(3)],
        ),
    ];
    for (function, args) in malformed {
        let error = eval_expr(
            &Expression::BuiltinCall {
                function,
                args,
                span,
            },
            &EvalContext::structural_preidentity(),
        )
        .expect_err("malformed builtin arity must fail closed");
        assert!(matches!(error, EvalError::WrongArgCount { .. }));
    }

    for (function, args) in [
        (BuiltinFunction::SubSample, vec![make_int(1)]),
        (BuiltinFunction::SuperSample, vec![make_int(1), make_int(2)]),
    ] {
        let error = eval_expr(
            &Expression::BuiltinCall {
                function,
                args,
                span,
            },
            &EvalContext::structural_preidentity(),
        )
        .expect_err("runtime-only synchronous calls do not constant fold");
        assert!(
            matches!(error, EvalError::NotConstant { .. }),
            "a valid {} signature reached the wrong route: {error:?}",
            function.name()
        );
    }
}

/// MLS 3.6 §12.4.6: "if `A` is a vector of reals, then `sin(A)` is a vector
/// where each element is the result of applying the function `sin` to the
/// corresponding element in `A`".
///
/// The expected values are what OMC 4.1.0 returns for
/// `cos({0.0, 2.0, 4.0})`. This is the fold
/// `Modelica.Electrical.Machines.SpacePhasors.Blocks.ToSpacePhasor` needs for
/// `TransformationMatrix[2, m] = 2/m*{cos(phi), sin(phi)}`, where `phi` is the
/// `SI.Angle phi[m]` vector.
#[test]
fn scalar_builtin_applies_element_wise_to_an_array_argument() {
    let ctx = EvalContext::structural_preidentity();
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Cos,
        args: vec![Expression::Array {
            elements: vec![make_real(0.0), make_real(2.0), make_real(4.0)],
            is_matrix: false,
            span: test_span(),
        }],
        span: test_span(),
    };

    let result = eval_expr(&expr, &ctx).expect("cos over a vector folds");
    let elements = result.as_array().expect("vector result");
    let expected = [1.0, -0.4161468365471424, -0.6536436208636119];
    assert_eq!(elements.len(), expected.len());
    for (element, expected) in elements.iter().zip(expected) {
        assert!(
            (element.as_real().expect("Real element") - expected).abs() < 1e-15,
            "{element} != {expected}"
        );
    }
}

/// The same rule carried through a matrix row by row, and the `2/m*{…}` scaling
/// that reads it: MLS §10.6.3 scales every element by the numeric scalar.
#[test]
fn scalar_builtin_applies_element_wise_through_matrix_rows() {
    let ctx = EvalContext::structural_preidentity();
    let rows = Expression::Array {
        elements: vec![
            Expression::Array {
                elements: vec![make_real(0.0), make_real(0.0)],
                is_matrix: false,
                span: test_span(),
            },
            Expression::Array {
                elements: vec![make_real(0.0), make_real(0.0)],
                is_matrix: false,
                span: test_span(),
            },
        ],
        is_matrix: true,
        span: test_span(),
    };
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Cos,
        args: vec![rows],
        span: test_span(),
    };

    let result = eval_expr(&expr, &ctx).expect("cos over a matrix folds");
    assert_eq!(
        result,
        Value::Array(vec![
            Value::Array(vec![Value::Real(1.0), Value::Real(1.0)]),
            Value::Array(vec![Value::Real(1.0), Value::Real(1.0)]),
        ])
    );
}

/// `sum`, `product`, `size` and the other builtins that declare array formals
/// keep their reduction meaning: MLS §12.4.6 only makes an array actual a
/// *foreach* argument where the formal parameter is a scalar.
#[test]
fn array_formal_builtins_are_not_vectorized() {
    let ctx = EvalContext::structural_preidentity();
    let vector = Expression::Array {
        elements: vec![make_int(1), make_int(2), make_int(3)],
        is_matrix: false,
        span: test_span(),
    };
    for (function, expected) in [
        (BuiltinFunction::Sum, Value::Integer(6)),
        (BuiltinFunction::Product, Value::Integer(6)),
        (BuiltinFunction::Size, Value::Array(vec![Value::Integer(3)])),
    ] {
        let expr = Expression::BuiltinCall {
            function,
            args: vec![vector.clone()],
            span: test_span(),
        };
        let result = eval_expr(&expr, &ctx).expect("reduction folds");
        assert_eq!(
            result, expected,
            "{function:?} must apply once to the array"
        );
    }
}

/// MLS 3.6 §10.6.5 "Division by Numeric Scalars": `a / s` divides every element
/// of the numeric array by the scalar.
#[test]
fn array_divided_by_numeric_scalar_folds_element_wise() {
    let ctx = EvalContext::structural_preidentity();
    let expr = Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(Expression::Array {
            elements: vec![make_real(1.0), make_real(2.0)],
            is_matrix: false,
            span: test_span(),
        }),
        rhs: Box::new(make_real(4.0)),
        span: test_span(),
    };

    let result = eval_expr(&expr, &ctx).expect("array / scalar folds");
    assert_eq!(
        result,
        Value::Array(vec![Value::Real(0.25), Value::Real(0.5)])
    );
}

/// MLS 3.6 §14 defines arithmetic over an operator record only through the
/// operator functions the record declares, and this evaluator does not resolve
/// that overload. The failure is therefore an unimplemented form, so a caller
/// folding parameter bindings leaves the value for the runtime instead of
/// rejecting the model — `Real * Complex` is not a defect in the model.
#[test]
fn record_operand_arithmetic_is_unimplemented_not_a_defect() {
    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter(
        "z",
        Value::Record(
            [
                ("re".to_string(), Value::Real(1.0)),
                ("im".to_string(), Value::Real(2.0)),
            ]
            .into_iter()
            .collect(),
        ),
    );
    let expr = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(make_real(5.0)),
        rhs: Box::new(Expression::VarRef {
            name: "z".into(),
            subscripts: vec![],
            span: test_span(),
        }),
        span: test_span(),
    };

    let error = eval_expr(&expr, &ctx).expect_err("an unresolved overload does not fold");
    assert_eq!(
        error.runtime_dependent_reason(),
        Some(RuntimeDependentReason::UnimplementedForm),
        "{error}"
    );
}

#[test]
fn test_eval_builtin_integer_overflow_returns_error() {
    let ctx = EvalContext::structural_preidentity();
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Integer,
        args: vec![make_real(-1e40)],
        span: test_span(),
    };
    let err = eval_expr(&expr, &ctx).unwrap_err();
    assert!(
        err.to_string()
            .contains("outside i64 range while evaluating integer(...)")
    );
}

fn resolved_mode_context(
    enum_id: rumoca_core::DefId,
    foreign_enum_id: rumoca_core::DefId,
) -> EvalContext {
    let catalog = ResolvedEnumCatalog::try_from_declarations(vec![
        ResolvedEnumDeclaration {
            declaration: enum_id,
            type_name: "Pkg.Mode".to_string(),
            literals: vec!["Off".to_string(), "On".to_string()],
        },
        ResolvedEnumDeclaration {
            declaration: foreign_enum_id,
            type_name: "Pkg.Mode".to_string(),
            literals: vec!["On".to_string()],
        },
    ])
    .expect("fixture enum declarations have exact dense ordinals");
    let inventory = ResolvedIdentityInventory::try_from_bindings(Vec::new(), Vec::new())
        .expect("empty occurrence inventory is exact");
    EvalContext::resolved(0, 0, inventory, catalog)
}

fn assert_resolved_mode(value: &Value, declaration: rumoca_core::DefId, literal: &str) {
    assert!(matches!(
        value,
        Value::ResolvedEnum(value)
            if value.declaration() == declaration
                && value.display_type() == "Pkg.Mode"
                && value.literal() == literal
    ));
}

#[test]
fn enum_literal_lookup_and_integer_conversion_use_resolved_identity() {
    let enum_id = rumoca_core::DefId::new(301);
    let foreign_enum_id = rumoca_core::DefId::new(305);
    let ctx = resolved_mode_context(enum_id, foreign_enum_id);

    let alias = Expression::VarRef {
        name: enum_reference(
            "Imported.On",
            "Imported",
            enum_id,
            "On",
            rumoca_core::DefId::new(302),
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };
    let alias_value =
        eval_expr(&alias, &ctx).expect("alias with the same declaration identity folds");
    assert_resolved_mode(&alias_value, enum_id, "On");

    let integer = Expression::BuiltinCall {
        function: BuiltinFunction::Integer,
        args: vec![alias.clone()],
        span: test_span(),
    };
    assert_eq!(eval_expr(&integer, &ctx).unwrap(), Value::Integer(2));
    let off = Expression::VarRef {
        name: enum_reference(
            "Pkg.Mode.Off",
            "Pkg.Mode",
            enum_id,
            "Off",
            rumoca_core::DefId::new(303),
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };
    assert_eq!(
        eval_expr(
            &Expression::Binary {
                op: OpBinary::Lt,
                lhs: Box::new(off),
                rhs: Box::new(alias.clone()),
                span: test_span(),
            },
            &ctx,
        )
        .unwrap(),
        Value::Bool(true)
    );

    let foreign = Expression::VarRef {
        name: enum_reference(
            "Pkg.Mode.On",
            "Pkg.Mode",
            foreign_enum_id,
            "On",
            rumoca_core::DefId::new(306),
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };
    let foreign_value =
        eval_expr(&foreign, &ctx).expect("the foreign declaration is independently registered");
    assert_ne!(alias_value, foreign_value);
    let foreign_integer = Expression::BuiltinCall {
        function: BuiltinFunction::Integer,
        args: vec![foreign.clone()],
        span: test_span(),
    };
    assert_eq!(
        eval_expr(&foreign_integer, &ctx).unwrap(),
        Value::Integer(1)
    );
    assert!(matches!(
        eval_expr(
            &Expression::Binary {
                op: OpBinary::Lt,
                lhs: Box::new(alias),
                rhs: Box::new(foreign),
                span: test_span(),
            },
            &ctx,
        ),
        Err(EvalError::TypeMismatch { .. })
    ));

    let impostor = Expression::VarRef {
        name: enum_reference(
            "Pkg.Mode.On",
            "Pkg.Mode",
            rumoca_core::DefId::new(304),
            "On",
            rumoca_core::DefId::new(307),
        ),
        subscripts: Vec::new(),
        span: test_span(),
    };
    assert!(matches!(
        eval_expr(&impostor, &ctx),
        Err(EvalError::InvalidSemanticIr { .. })
    ));
}

#[test]
fn integer_conversion_does_not_guess_an_unregistered_enum_ordinal() {
    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter(
        "resolution",
        Value::Enum("Resolution".to_string(), "y".to_string()),
    );
    let expr = Expression::BuiltinCall {
        function: BuiltinFunction::Integer,
        args: vec![Expression::VarRef {
            name: "resolution".into(),
            subscripts: vec![],
            span: test_span(),
        }],
        span: test_span(),
    };

    let error = eval_expr(&expr, &ctx).expect_err("no declaration means no ordinal");
    assert_eq!(
        error.runtime_dependent_reason(),
        Some(RuntimeDependentReason::UnimplementedForm),
        "{error}"
    );
}

#[test]
fn resolved_enum_catalog_rejects_duplicate_declaration_issuance() {
    let declaration = rumoca_core::DefId::new(401);
    assert!(matches!(
        ResolvedEnumCatalog::try_from_declarations(vec![
            ResolvedEnumDeclaration {
                declaration,
                type_name: "Pkg.Mode".to_string(),
                literals: vec!["On".to_string()],
            },
            ResolvedEnumDeclaration {
                declaration,
                type_name: "Alias.Mode".to_string(),
                literals: vec!["Off".to_string(), "On".to_string()],
            },
        ]),
        Err(EvalError::Internal { .. })
    ));
}

#[test]
fn a_preserved_partial_application_is_not_invoked_as_a_malformed_call() {
    let real = rumoca_core::TypeId::new(41);
    let effective = rumoca_core::EffectiveType::new(real, real, vec![]).unwrap();
    let mut function = Function::new(
        "Pkg.integrand",
        rumoca_core::DefId::new(11_002),
        test_span(),
    );
    function.inputs = ["u", "A", "w"]
        .into_iter()
        .map(|name| rumoca_core::FunctionParam::new(name, "Real", effective.clone(), test_span()))
        .collect();
    function.outputs.push(rumoca_core::FunctionParam::new(
        "y",
        "Real",
        effective,
        test_span(),
    ));
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: "y".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(901),
            }],
        )
        .unwrap(),
        value: Expression::VarRef {
            name: rumoca_core::Reference::new("u"),
            subscripts: Vec::new(),
            span: test_span(),
        },
        span: test_span(),
    }];
    let mut ctx = EvalContext::structural_preidentity();
    ctx.insert_direct_function_fixture(function);
    let named = |name: &str, value: f64| Expression::FunctionCall {
        name: rumoca_core::Reference::generated(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![make_real(value)],
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };
    let partial = Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.integrand"),
        args: vec![named("A", 2.0), named("w", 3.0)],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::PartialApplication,
        span: test_span(),
    };
    let invocation = Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.integrand"),
        args: vec![named("u", 4.0), named("A", 2.0), named("w", 3.0)],
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: test_span(),
    };

    assert_eq!(
        eval_optional(&partial, &ctx).expect("unsupported function values defer explicitly"),
        None
    );
    assert_eq!(
        eval_expr(&invocation, &ctx).expect("ordinary named call remains executable"),
        Value::Real(4.0)
    );
}

#[test]
fn test_try_eval_helpers() {
    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("n", Value::Integer(5));

    let expr = Expression::VarRef {
        name: "n".into(),
        subscripts: vec![],
        span: test_span(),
    };

    assert_eq!(try_eval_integer(&expr, &ctx).unwrap(), Some(5));
    assert_eq!(try_eval_real(&expr, &ctx).unwrap(), Some(5.0));
    assert!(matches!(
        try_eval_bool(&expr, &ctx),
        Err(EvalError::TypeMismatch { .. })
    ));
}

#[test]
fn typed_try_helpers_defer_only_runtime_dependent_values() {
    let ctx = EvalContext::structural_preidentity();
    let unknown = Expression::VarRef {
        name: "runtime_value".into(),
        subscripts: vec![],
        span: test_span(),
    };
    assert_eq!(try_eval_integer(&unknown, &ctx).unwrap(), None);

    let empty = Expression::Empty { span: test_span() };
    assert!(matches!(
        try_eval_integer(&empty, &ctx),
        Err(EvalError::InvalidSemanticIr { span, .. }) if span == test_span()
    ));
}

#[test]
fn optional_evaluation_suppresses_only_runtime_dependent_failures() {
    let ctx = EvalContext::structural_preidentity();
    let unknown = Expression::VarRef {
        name: "runtime_value".into(),
        subscripts: vec![],
        span: test_span(),
    };
    assert_eq!(eval_optional(&unknown, &ctx).unwrap(), None);

    let invalid = Expression::Empty { span: test_span() };
    let error = eval_optional(&invalid, &ctx).expect_err("invalid IR must not become absence");
    assert!(matches!(error, EvalError::InvalidSemanticIr { .. }));
    assert_eq!(error.span(), Some(test_span()));
}

#[test]
fn exact_scalar_projection_precedes_aggregate_fallback() {
    let span = test_span();
    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("f", Value::Real(10.0));
    ctx.add_parameter("f[2]", Value::Real(20.0));
    let projection = Expression::Index {
        base: Box::new(Expression::VarRef {
            name: "f".into(),
            subscripts: vec![],
            span,
        }),
        subscripts: vec![Subscript::Index { value: 2, span }],
        span,
    };

    assert_eq!(eval_expr(&projection, &ctx).unwrap(), Value::Real(20.0));

    let scalar_index = Expression::Index {
        base: Box::new(make_real(1.0)),
        subscripts: vec![Subscript::Index { value: 1, span }],
        span,
    };
    assert!(matches!(
        eval_expr(&scalar_index, &ctx),
        Err(EvalError::TypeMismatch { .. })
    ));

    let out_of_bounds = Expression::Index {
        base: Box::new(make_vector(&[1, 2])),
        subscripts: vec![Subscript::Index { value: 3, span }],
        span,
    };
    assert!(matches!(
        eval_expr(&out_of_bounds, &ctx),
        Err(EvalError::IndexOutOfBounds { .. })
    ));
}

#[test]
fn legal_noninteger_index_domains_defer_without_domain_metadata() {
    let span = test_span();
    let base = || Box::new(make_vector(&[10, 20]));
    let index = |expr| Expression::Index {
        base: base(),
        subscripts: vec![Subscript::Expr {
            expr: Box::new(expr),
            span,
        }],
        span,
    };

    assert_eq!(
        eval_optional(
            &index(make_bool(true)),
            &EvalContext::structural_preidentity()
        )
        .unwrap(),
        None,
        "Boolean indexing is legal Modelica but needs the declared index domain"
    );

    let mut enum_context = EvalContext::structural_preidentity();
    enum_context.add_parameter(
        "mode",
        Value::Enum("Pkg.Mode".to_string(), "On".to_string()),
    );
    let enum_index = index(Expression::VarRef {
        name: "mode".into(),
        subscripts: Vec::new(),
        span,
    });
    assert_eq!(eval_optional(&enum_index, &enum_context).unwrap(), None);

    assert!(matches!(
        eval_optional(
            &index(make_real(1.0)),
            &EvalContext::structural_preidentity()
        ),
        Err(EvalError::TypeMismatch { .. })
    ));
}

#[test]
fn legal_noninteger_range_selectors_defer_without_domain_metadata() {
    let span = test_span();
    let index = |range| Expression::Index {
        base: Box::new(make_vector(&[10, 20])),
        subscripts: vec![Subscript::Expr {
            expr: Box::new(range),
            span,
        }],
        span,
    };
    let boolean_range = Expression::Range {
        start: Box::new(make_bool(false)),
        step: None,
        end: Box::new(make_bool(true)),
        span,
    };
    assert_eq!(
        eval_optional(
            &index(boolean_range),
            &EvalContext::structural_preidentity()
        )
        .unwrap(),
        None
    );

    let mut enum_context = EvalContext::structural_preidentity();
    enum_context.add_parameter(
        "first_mode",
        Value::Enum("Pkg.Mode".to_string(), "Off".to_string()),
    );
    enum_context.add_parameter(
        "last_mode",
        Value::Enum("Pkg.Mode".to_string(), "On".to_string()),
    );
    let enum_bound = |name: &str| Expression::VarRef {
        name: name.into(),
        subscripts: Vec::new(),
        span,
    };
    let enum_range = Expression::Range {
        start: Box::new(enum_bound("first_mode")),
        step: None,
        end: Box::new(enum_bound("last_mode")),
        span,
    };
    assert_eq!(
        eval_optional(&index(enum_range), &enum_context).unwrap(),
        None
    );

    let integer_range = Expression::Range {
        start: Box::new(make_int(1)),
        step: None,
        end: Box::new(make_int(2)),
        span,
    };
    assert_eq!(
        eval_expr(
            &index(integer_range),
            &EvalContext::structural_preidentity()
        )
        .unwrap(),
        Value::Array(vec![Value::Integer(10), Value::Integer(20)])
    );
}

#[test]
fn range_and_colon_subscripts_preserve_matrix_shape() {
    let span = test_span();
    let expression = Expression::Index {
        base: Box::new(make_matrix(&[&[1, 2], &[3, 4], &[5, 6]])),
        subscripts: vec![
            Subscript::Expr {
                expr: Box::new(Expression::Range {
                    start: Box::new(make_int(2)),
                    step: None,
                    end: Box::new(make_int(3)),
                    span,
                }),
                span,
            },
            Subscript::Colon { span },
        ],
        span,
    };

    assert_eq!(
        eval_expr(&expression, &EvalContext::structural_preidentity()).unwrap(),
        Value::Array(vec![
            Value::Array(vec![Value::Integer(3), Value::Integer(4)]),
            Value::Array(vec![Value::Integer(5), Value::Integer(6)]),
        ])
    );
}

#[test]
fn direct_multidimensional_selection_obeys_the_retained_node_budget() {
    let span = test_span();
    let selector = Subscript::Expr {
        expr: Box::new(make_vector(&[1; 400])),
        span,
    };
    let expression = Expression::Index {
        base: Box::new(make_matrix(&[&[7]])),
        subscripts: vec![selector.clone(), selector],
        span,
    };

    let error = eval_expr(&expression, &EvalContext::structural_preidentity())
        .expect_err("bounded selectors must not create an unbounded Cartesian result");
    assert!(
        matches!(error, EvalError::UnsupportedExpression { .. }),
        "unexpected selection error: {error}"
    );
}

#[test]
fn shape_metadata_size_matches_value_backed_array_semantics() {
    let span = test_span();
    let mut context = EvalContext::structural_preidentity();
    context.add_array_dimensions("vector", vec![3]);
    context.add_array_dimensions("matrix", vec![2, 4]);
    context.add_array_dimensions("empty_matrix", vec![0, 3]);

    let shape_call = |function, name: &str| Expression::BuiltinCall {
        function,
        args: vec![Expression::VarRef {
            name: name.into(),
            subscripts: Vec::new(),
            span,
        }],
        span,
    };

    assert_eq!(
        eval_expr(&shape_call(BuiltinFunction::Size, "vector"), &context).unwrap(),
        Value::Array(vec![Value::Integer(3)])
    );
    assert_eq!(
        eval_expr(&shape_call(BuiltinFunction::Size, "matrix"), &context).unwrap(),
        Value::Array(vec![Value::Integer(2), Value::Integer(4)])
    );
    assert_eq!(
        eval_expr(&shape_call(BuiltinFunction::Ndims, "vector"), &context).unwrap(),
        Value::Integer(1)
    );
    assert_eq!(
        eval_expr(&shape_call(BuiltinFunction::Ndims, "matrix"), &context).unwrap(),
        Value::Integer(2)
    );
    assert_eq!(
        eval_expr(&shape_call(BuiltinFunction::Size, "empty_matrix"), &context).unwrap(),
        Value::Array(vec![Value::Integer(0), Value::Integer(3)])
    );
    assert_eq!(
        eval_expr(
            &shape_call(BuiltinFunction::Ndims, "empty_matrix"),
            &context
        )
        .unwrap(),
        Value::Integer(2)
    );
}

#[test]
fn test_eval_lookup_trait_resolves_scoped_values() {
    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_parameter("sys.n", Value::Integer(5));
    ctx.add_parameter("sys.inner.pi", Value::Real(3.0));
    ctx.add_parameter("sys.flag", Value::Bool(true));

    assert_eq!(ctx.lookup_integer("n", "sys.inner"), Some(5));
    assert_eq!(ctx.lookup_real("pi", "sys.inner"), Some(3.0));
    assert_eq!(ctx.lookup_boolean("flag", "sys.inner"), Some(true));
}

#[test]
fn instance_keyed_values_are_reachable_by_identity_not_only_by_rendering() {
    let first = rumoca_core::InstanceId::new(11);
    let second = rumoca_core::InstanceId::new(12);
    let first_key = ResolvedOccurrenceKey {
        instance_id: first,
        root_def_id: rumoca_core::DefId::new(21),
    };
    let second_key = ResolvedOccurrenceKey {
        instance_id: second,
        root_def_id: rumoca_core::DefId::new(22),
    };
    let Ok(inventory) = ResolvedIdentityInventory::try_from_bindings(
        vec![
            ResolvedValueBinding {
                identity: first_key,
                value: Value::Integer(5),
            },
            ResolvedValueBinding {
                identity: second_key,
                value: Value::Integer(6),
            },
        ],
        Vec::new(),
    ) else {
        panic!("fixture identity inventory must be unique");
    };
    let mut ctx = EvalContext::resolved(1, 0, inventory, ResolvedEnumCatalog::empty());
    ctx.add_parameter("a.n", Value::Integer(99));

    let occurrence = |rendered: &str, root_def_id, instance_id| {
        let Ok(component) = rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: rendered.to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: root_def_id,
            }],
        ) else {
            panic!("fixture occurrence must carry a declaration identity");
        };
        Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(component)
                .with_instance_id(instance_id),
            subscripts: Vec::new(),
            span: test_span(),
        }
    };

    assert_eq!(ctx.occurrence_value(first_key), Some(&Value::Integer(5)));
    assert_eq!(ctx.occurrence_value(second_key), Some(&Value::Integer(6)));
    assert!(matches!(
        eval_expr(&occurrence("a.n", first_key.root_def_id, first), &ctx),
        Ok(Value::Integer(5))
    ));
    assert!(matches!(
        eval_expr(&occurrence("a.n", second_key.root_def_id, second), &ctx),
        Ok(Value::Integer(6))
    ));
    assert!(matches!(
        eval_expr(&occurrence("a.n", first_key.root_def_id, second), &ctx),
        Err(EvalError::UnknownVariable { .. })
    ));
    assert!(matches!(
        eval_expr(
            &Expression::VarRef {
                name: rumoca_core::Reference::new("a.n"),
                subscripts: Vec::new(),
                span: test_span(),
            },
            &ctx,
        ),
        Err(EvalError::InvalidSemanticIr { .. })
    ));
}

#[test]
fn resolved_identity_inventory_rejects_duplicate_occurrence_keys() {
    let identity = ResolvedOccurrenceKey {
        instance_id: rumoca_core::InstanceId::new(41),
        root_def_id: rumoca_core::DefId::new(42),
    };
    assert!(matches!(
        ResolvedIdentityInventory::try_from_bindings(
            vec![
                ResolvedValueBinding {
                    identity,
                    value: Value::Integer(1),
                },
                ResolvedValueBinding {
                    identity,
                    value: Value::Integer(2),
                },
            ],
            Vec::new(),
        ),
        Err(EvalError::Internal { .. })
    ));
}

#[test]
fn resolved_declaration_binding_rejects_duplicates_atomically() {
    let declaration = rumoca_core::DefId::new(91);
    let mut context = EvalContext::resolved_empty();
    context
        .try_bind_resolved_declaration_value(declaration, Value::Integer(1))
        .expect("one exact declaration binding is admitted");
    assert!(matches!(
        context.try_bind_resolved_declaration_value(declaration, Value::Integer(2)),
        Err(EvalError::Internal { .. })
    ));

    let component = rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: "formal".to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id: declaration,
        }],
    )
    .expect("fixture declaration reference is exact");
    let reference = Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component),
        subscripts: Vec::new(),
        span: test_span(),
    };
    let value = eval_expr(&reference, &context)
        .expect("duplicate refusal must retain the first exact binding");
    assert_eq!(value, Value::Integer(1));
}

#[test]
fn only_undetermined_failures_carry_a_runtime_dependent_reason() {
    let span = test_span();
    let undetermined = [
        (
            EvalError::unknown_variable("n", span),
            RuntimeDependentReason::UnknownValue,
        ),
        (
            EvalError::unknown_function("f", span),
            RuntimeDependentReason::UnknownValue,
        ),
        (
            EvalError::not_constant("reads a state", span),
            RuntimeDependentReason::NotConstant,
        ),
        (
            EvalError::UnsupportedExpression {
                kind: "ArrayComprehension".to_string(),
                span,
            },
            RuntimeDependentReason::UnimplementedForm,
        ),
    ];
    for (error, reason) in undetermined {
        assert_eq!(
            error.runtime_dependent_reason(),
            Some(reason),
            "{error} must leave the value for the runtime"
        );
    }

    let proven_wrong = [
        EvalError::type_mismatch("Integer", "Boolean", span),
        EvalError::DivisionByZero { span },
        EvalError::CircularDependency {
            path: "a -> b -> a".to_string(),
            span,
        },
        EvalError::function_error("missing required argument x", span),
        EvalError::InvalidSemanticIr {
            reason: "empty expression".to_string(),
            span,
        },
        EvalError::IndexOutOfBounds {
            index: 4,
            size: 2,
            span,
        },
        EvalError::WrongArgCount {
            expected: 2,
            actual: 3,
            span,
        },
        EvalError::FieldNotFound {
            field: "re".to_string(),
            span,
        },
        EvalError::range_error("step cannot be zero", span),
        EvalError::missing_source_context("binding without provenance"),
        EvalError::Internal {
            message: "unreachable".to_string(),
        },
    ];
    for error in proven_wrong {
        assert_eq!(
            error.runtime_dependent_reason(),
            None,
            "{error} proves the expression wrong and must surface"
        );
    }
}

/// A declared `fixed = false` parameter is reported as deferred, not unknown.
///
/// MLS §4.4 lets a declaration defer its value to initialization, so the name
/// resolves and only the number is missing. Reporting it through
/// `UnknownVariable` describes a defect that is not there; a caller reading the
/// message goes looking for a scope or flat-name bug instead of the construct.
///
/// The registration is also *scoped* like every other value lookup, so a
/// component-qualified deferred parameter answers for its own occurrence — the
/// MSL shape is `meanVoltage.t0`, never a bare `t0`.
#[test]
fn a_deferred_parameter_is_reported_as_deferred_rather_than_unknown() {
    let span = test_span();
    let mut ctx = EvalContext::structural_preidentity();
    ctx.add_deferred_parameter("meanVoltage.t0", DeferredParameterSource::StartInstant);
    ctx.add_deferred_parameter("later.t0", DeferredParameterSource::InitializationSystem);

    let error = eval_expr_with_span(
        &Expression::VarRef {
            name: rumoca_core::Reference::new("meanVoltage.t0"),
            subscripts: Vec::new(),
            span,
        },
        &ctx,
        span,
    )
    .expect_err("a deferred parameter has no translation-time value");
    assert!(
        matches!(
            error,
            EvalError::InitializationDeferred {
                settled_by: DeferredParameterSource::StartInstant,
                ..
            }
        ),
        "expected a start-instant deferral, got {error}"
    );
    // MLS §4.4 names initialization as a legitimate origin, so the fold must
    // treat this as "no value yet" rather than as a wrong model.
    assert_eq!(
        error.runtime_dependent_reason(),
        Some(RuntimeDependentReason::UnknownValue)
    );
    assert_eq!(
        ctx.deferred_parameter("later.t0"),
        Some(DeferredParameterSource::InitializationSystem)
    );
    // A name that was never declared stays unknown: the new variant must not
    // swallow the resolution failure it was introduced to be distinguished from.
    assert!(matches!(
        eval_expr_with_span(
            &Expression::VarRef {
                name: rumoca_core::Reference::new("absent"),
                subscripts: Vec::new(),
                span,
            },
            &ctx,
            span,
        )
        .expect_err("an undeclared name is still unknown"),
        EvalError::UnknownVariable { .. }
    ));
}
