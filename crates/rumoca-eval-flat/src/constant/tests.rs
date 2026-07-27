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

#[test]
fn test_eval_literal() {
    let ctx = EvalContext::new();

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
    let ctx = EvalContext::new();
    let expr = Expression::Literal {
        value: Literal::Integer(42),
        span: rumoca_core::Span::DUMMY,
    };

    let err = eval_expr(&expr, &ctx).expect_err("unspanned constants should fail fast");
    assert!(matches!(err, EvalError::MissingSourceContext { .. }));
}

#[test]
fn test_eval_binary() {
    let ctx = EvalContext::new();

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
    let ctx = EvalContext::new();
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
    let ctx = EvalContext::new();
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
    let ctx = EvalContext::new();

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
    let ctx = EvalContext::new();

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
    let ctx = EvalContext::new();

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
    let ctx = EvalContext::new();

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
    let ctx = EvalContext::new();

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
        collect_int_range(i64::MAX, i64::MAX, 1),
        vec![Value::Integer(i64::MAX)]
    );
    assert_eq!(
        collect_int_range(i64::MIN, i64::MIN, -1),
        vec![Value::Integer(i64::MIN)]
    );
}

#[test]
fn test_real_range_rejects_non_advancing_step() {
    let start = 1.0e20_f64;
    let end = f64::from_bits(start.to_bits() + 1);

    assert!(matches!(
        collect_real_range(start, end, 1.0, test_span()),
        Err(EvalError::RangeError { .. })
    ));
}

#[test]
fn test_eval_if() {
    let ctx = EvalContext::new();

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
    let ctx = EvalContext::new();
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
    let ctx = EvalContext::new();
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
fn test_eval_if_requires_exactly_equal_real_outcomes() {
    let ctx = EvalContext::new();
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
    let ctx = EvalContext::new();
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
fn test_eval_parameter() {
    let mut ctx = EvalContext::new();
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
    let ctx = EvalContext::new();

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
fn test_eval_builtin_integer_overflow_returns_error() {
    let ctx = EvalContext::new();
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

#[test]
fn test_try_eval_helpers() {
    let mut ctx = EvalContext::new();
    ctx.add_parameter("n", Value::Integer(5));

    let expr = Expression::VarRef {
        name: "n".into(),
        subscripts: vec![],
        span: test_span(),
    };

    assert_eq!(try_eval_integer(&expr, &ctx), Some(5));
    assert_eq!(try_eval_real(&expr, &ctx), Some(5.0));
    assert_eq!(try_eval_bool(&expr, &ctx), None);
}

#[test]
fn test_eval_lookup_trait_resolves_scoped_values() {
    let mut ctx = EvalContext::new();
    ctx.add_parameter("sys.n", Value::Integer(5));
    ctx.add_parameter("sys.inner.pi", Value::Real(3.0));
    ctx.add_parameter("sys.flag", Value::Bool(true));
    ctx.enum_literals.insert(
        "sys.mode".to_string(),
        ("Modes".to_string(), "Fast".to_string()),
    );

    assert_eq!(ctx.lookup_integer("n", "sys.inner"), Some(5));
    assert_eq!(ctx.lookup_real("pi", "sys.inner"), Some(3.0));
    assert_eq!(ctx.lookup_boolean("flag", "sys.inner"), Some(true));
    assert_eq!(
        ctx.lookup_enum("mode", "sys.inner").as_deref(),
        Some("Modes.Fast")
    );
}
