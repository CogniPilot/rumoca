//! The array and value-transparent builtins of MLS 3.7 §10.3 and §3.7.4 fold at
//! both call sites: a binding expression (`Expression::BuiltinCall`, dispatched
//! by node kind) and a function body (dispatched by the builtin's name).

use super::*;

fn real(value: f64) -> Expression {
    make_real(value)
}

fn real_vector(values: &[f64]) -> Expression {
    Expression::Array {
        elements: values.iter().copied().map(real).collect(),
        kind: rumoca_core::ArrayConstructor::Array,
        span: test_span(),
    }
}

fn int_values(rows: &[&[i64]]) -> Value {
    Value::Array(
        rows.iter()
            .map(|row| Value::Array(row.iter().copied().map(Value::Integer).collect()))
            .collect(),
    )
}

fn real_values(rows: &[&[f64]]) -> Value {
    Value::Array(
        rows.iter()
            .map(|row| Value::Array(row.iter().copied().map(Value::Real).collect()))
            .collect(),
    )
}

/// One call per builtin with its MLS 3.7 result.
fn cases() -> Vec<(BuiltinFunction, Vec<Expression>, Value)> {
    vec![
        // §10.3.6 transpose(A): the first two dimensions exchanged.
        (
            BuiltinFunction::Transpose,
            vec![make_matrix(&[&[1, 2, 3], &[4, 5, 6]])],
            int_values(&[&[1, 4], &[2, 5], &[3, 6]]),
        ),
        // §10.3.4 identity(n): the Integer identity matrix.
        (
            BuiltinFunction::Identity,
            vec![make_int(2)],
            int_values(&[&[1, 0], &[0, 1]]),
        ),
        // §10.3.4 diagonal(v): v on the diagonal, zero elsewhere.
        (
            BuiltinFunction::Diagonal,
            vec![real_vector(&[1.5, 2.5])],
            real_values(&[&[1.5, 0.0], &[0.0, 2.5]]),
        ),
        // §10.3.6 outerProduct(x, y) = matrix(x) * transpose(matrix(y)).
        (
            BuiltinFunction::OuterProduct,
            vec![make_vector(&[1, 2]), make_vector(&[3, 4, 5])],
            int_values(&[&[3, 4, 5], &[6, 8, 10]]),
        ),
        // §10.3.6 cross(x, y) = {x[2]*y[3]-x[3]*y[2], x[3]*y[1]-x[1]*y[3],
        // x[1]*y[2]-x[2]*y[1]}.
        (
            BuiltinFunction::Cross,
            vec![real_vector(&[1.0, 2.0, 3.0]), real_vector(&[4.0, 5.0, 6.0])],
            Value::Array(vec![Value::Real(-3.0), Value::Real(6.0), Value::Real(-3.0)]),
        ),
        // §10.3.6 skew(x) = {{0, -x[3], x[2]}, {x[3], 0, -x[1]}, {-x[2], x[1], 0}}.
        (
            BuiltinFunction::Skew,
            vec![real_vector(&[1.0, 2.0, 3.0])],
            real_values(&[&[0.0, -3.0, 2.0], &[3.0, 0.0, -1.0], &[-2.0, 1.0, 0.0]]),
        ),
        // §10.3.6 symmetric(A): the upper triangle mirrored below the diagonal.
        (
            BuiltinFunction::Symmetric,
            vec![make_matrix(&[&[1, 2], &[3, 4]])],
            int_values(&[&[1, 2], &[2, 4]]),
        ),
        // §3.7.4.2 smooth(p, expr) and §3.7.4.1 noEvent(expr) return the value
        // of expr.
        (
            BuiltinFunction::Smooth,
            vec![make_int(1), real(2.5)],
            Value::Real(2.5),
        ),
        (BuiltinFunction::NoEvent, vec![real(2.5)], Value::Real(2.5)),
    ]
}

/// The binding-expression site: `final parameter Real A[3, 2] = transpose(B)`.
#[test]
fn array_and_transparent_builtins_fold_in_a_binding_expression() {
    let ctx = EvalContext::new();
    for (function, args, expected) in cases() {
        let expr = Expression::BuiltinCall {
            function,
            args,
            span: test_span(),
        };
        let value = eval_expr(&expr, &ctx)
            .unwrap_or_else(|error| panic!("{} folds in a binding: {error}", function.name()));
        assert_eq!(value, expected, "{}", function.name());
    }
}

/// The function-body site: `algorithm y := transpose(A);` inside a pure
/// function called from a binding.
#[test]
fn array_and_transparent_builtins_fold_in_a_function_body() {
    for (function, args, expected) in cases() {
        let mut body = Function::new("test.body", test_span());
        let real = rumoca_core::TypeId::new(1);
        let effective_type =
            rumoca_core::EffectiveType::new(real, real, Vec::new()).expect("scalar Real type");
        body.add_output(rumoca_core::FunctionParam::new(
            "y",
            "Real",
            effective_type,
            test_span(),
        ));
        body.pure = true;
        body.body = vec![rumoca_core::Statement::Assignment {
            comp: rumoca_core::ComponentReference::construct(
                false,
                test_span(),
                vec![rumoca_core::ComponentRefPart {
                    ident: "y".to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: rumoca_core::DefId::new(1),
                }],
            )
            .expect("fixture target is exact"),
            value: Expression::BuiltinCall {
                function,
                args,
                span: test_span(),
            },
            span: test_span(),
        }];
        let value = eval_function(
            &body,
            Vec::new(),
            &EvalContext::new(),
            &EvalLimits::default(),
            0,
            test_span(),
        )
        .unwrap_or_else(|error| panic!("{} folds in a function body: {error}", function.name()));
        assert_eq!(value, expected, "{}", function.name());
    }
}

/// A builtin applied to an argument outside its MLS domain is reported, never
/// folded to a guessed value.
#[test]
fn array_builtins_refuse_arguments_outside_their_domain() {
    let refused = [
        (BuiltinFunction::Transpose, vec![make_vector(&[1, 2])]),
        (BuiltinFunction::Identity, vec![make_int(-1)]),
        (BuiltinFunction::Diagonal, vec![make_matrix(&[&[1]])]),
        (
            BuiltinFunction::OuterProduct,
            vec![make_vector(&[1]), make_matrix(&[&[1]])],
        ),
        (
            BuiltinFunction::Cross,
            vec![make_vector(&[1, 2]), make_vector(&[1, 2, 3])],
        ),
        (BuiltinFunction::Skew, vec![make_vector(&[1, 2, 3, 4])]),
        (BuiltinFunction::Symmetric, vec![make_matrix(&[&[1, 2]])]),
        (BuiltinFunction::Smooth, vec![real(1.0)]),
        (BuiltinFunction::NoEvent, Vec::new()),
    ];
    let ctx = EvalContext::new();
    for (function, args) in refused {
        let expr = Expression::BuiltinCall {
            function,
            args,
            span: test_span(),
        };
        assert!(
            eval_expr(&expr, &ctx).is_err(),
            "{} outside its domain is refused",
            function.name()
        );
    }
}
