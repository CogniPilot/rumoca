//! The array and value-transparent builtins of MLS 3.7 §10.3 and §3.7.4 fold at
//! both call sites: a binding expression (`Expression::BuiltinCall`, dispatched
//! by node kind) and a function body (dispatched by the builtin's name).

use super::*;

fn real(value: f64) -> Expression {
    make_real(value)
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        kind: rumoca_core::ArrayConstructor::Array,
        span: test_span(),
    }
}

fn real_vector(values: &[f64]) -> Expression {
    array(values.iter().copied().map(real).collect())
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

fn call(function: BuiltinFunction, args: Vec<Expression>) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span: test_span(),
    }
}

/// One call per builtin with its MLS 3.7 result, and the declared type and
/// shape of a function output that receives it.
struct Case {
    function: BuiltinFunction,
    args: Vec<Expression>,
    expected: Value,
    output_type: &'static str,
    output_dims: Vec<i64>,
}

fn cases() -> Vec<Case> {
    vec![
        // §10.3.4 transpose(A): the first two dimensions exchanged, element
        // type kept.
        Case {
            function: BuiltinFunction::Transpose,
            args: vec![make_matrix(&[&[1, 2, 3], &[4, 5, 6]])],
            expected: int_values(&[&[1, 4], &[2, 5], &[3, 6]]),
            output_type: "Integer",
            output_dims: vec![3, 2],
        },
        // §10.3.3 identity(n): the Integer identity matrix.
        Case {
            function: BuiltinFunction::Identity,
            args: vec![make_int(2)],
            expected: int_values(&[&[1, 0], &[0, 1]]),
            output_type: "Integer",
            output_dims: vec![2, 2],
        },
        // §10.3.3 diagonal(v): v on the diagonal, zero of its type elsewhere.
        Case {
            function: BuiltinFunction::Diagonal,
            args: vec![real_vector(&[1.5, 2.5])],
            expected: real_values(&[&[1.5, 0.0], &[0.0, 2.5]]),
            output_type: "Real",
            output_dims: vec![2, 2],
        },
        Case {
            function: BuiltinFunction::Diagonal,
            args: vec![make_vector(&[1, 2])],
            expected: int_values(&[&[1, 0], &[0, 2]]),
            output_type: "Integer",
            output_dims: vec![2, 2],
        },
        // §10.3.5 outerProduct(x, y): Real x[n], y[m] give Real [n, m].
        Case {
            function: BuiltinFunction::OuterProduct,
            args: vec![make_vector(&[1, 2]), make_vector(&[3, 4, 5])],
            expected: real_values(&[&[3.0, 4.0, 5.0], &[6.0, 8.0, 10.0]]),
            output_type: "Real",
            output_dims: vec![2, 3],
        },
        // §10.3.5 cross(x, y) = {x[2]*y[3]-x[3]*y[2], x[3]*y[1]-x[1]*y[3],
        // x[1]*y[2]-x[2]*y[1]}, Real.
        Case {
            function: BuiltinFunction::Cross,
            args: vec![make_vector(&[1, 2, 3]), real_vector(&[4.0, 5.0, 6.0])],
            expected: Value::Array(vec![Value::Real(-3.0), Value::Real(6.0), Value::Real(-3.0)]),
            output_type: "Real",
            output_dims: vec![3],
        },
        // §10.3.5 skew(x) = {{0, -x[3], x[2]}, {x[3], 0, -x[1]}, {-x[2], x[1], 0}},
        // Real.
        Case {
            function: BuiltinFunction::Skew,
            args: vec![make_vector(&[1, 2, 3])],
            expected: real_values(&[&[0.0, -3.0, 2.0], &[3.0, 0.0, -1.0], &[-2.0, 1.0, 0.0]]),
            output_type: "Real",
            output_dims: vec![3, 3],
        },
        // §10.3.5 symmetric(A): the upper triangle mirrored below the diagonal,
        // Real.
        Case {
            function: BuiltinFunction::Symmetric,
            args: vec![make_matrix(&[&[1, 2], &[3, 4]])],
            expected: real_values(&[&[1.0, 2.0], &[2.0, 4.0]]),
            output_type: "Real",
            output_dims: vec![2, 2],
        },
        // §3.7.4 smooth(p, expr) and noEvent(expr) return the value of expr.
        Case {
            function: BuiltinFunction::Smooth,
            args: vec![make_int(1), real(2.5)],
            expected: Value::Real(2.5),
            output_type: "Real",
            output_dims: Vec::new(),
        },
        Case {
            function: BuiltinFunction::NoEvent,
            args: vec![real(2.5)],
            expected: Value::Real(2.5),
            output_type: "Real",
            output_dims: Vec::new(),
        },
    ]
}

/// The binding-expression site: `final parameter Real A[3, 2] = transpose(B)`.
#[test]
fn array_and_transparent_builtins_fold_in_a_binding_expression() {
    let ctx = EvalContext::new();
    for case in cases() {
        let name = case.function.name();
        let value = eval_expr(&call(case.function, case.args), &ctx)
            .unwrap_or_else(|error| panic!("{name} folds in a binding: {error}"));
        assert_eq!(value, case.expected, "{name}");
    }
}

fn output_param(type_name: &str, dims: Vec<i64>) -> rumoca_core::FunctionParam {
    let type_id = rumoca_core::TypeId::new(if type_name == "Integer" { 2 } else { 1 });
    let effective_type =
        rumoca_core::EffectiveType::new(type_id, type_id, dims).expect("output type");
    rumoca_core::FunctionParam::new("y", type_name, effective_type, test_span())
}

/// `function f output <type> y[<dims>]; algorithm y := <call>; end f;`
fn body_function(call_expr: Expression, output: rumoca_core::FunctionParam) -> Function {
    let mut body = Function::new("test.body", test_span());
    body.add_output(output);
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
        value: call_expr,
        span: test_span(),
    }];
    body
}

fn fold_body(function: &Function, limits: &EvalLimits) -> Result<Value, EvalError> {
    eval_function(
        function,
        Vec::new(),
        &EvalContext::new(),
        limits,
        0,
        test_span(),
    )
}

/// The function-body site: `algorithm y := transpose(A);` inside a pure
/// function whose output has the result's declared type and shape.
#[test]
fn array_and_transparent_builtins_fold_in_a_function_body() {
    for case in cases() {
        let name = case.function.name();
        let body = body_function(
            call(case.function, case.args),
            output_param(case.output_type, case.output_dims),
        );
        let value = fold_body(&body, &EvalLimits::default())
            .unwrap_or_else(|error| panic!("{name} folds in a function body: {error}"));
        assert_eq!(value, case.expected, "{name}");
    }
}

/// MLS 3.7 §10.3.5 gives outerProduct, cross, skew and symmetric Real
/// arguments, so an Integer argument is converted before the arithmetic: a
/// product beyond the Integer range is the finite Real product, not an
/// overflow.
#[test]
fn real_matrix_builtins_compute_in_real_arithmetic() {
    let ctx = EvalContext::new();
    let big = 3_037_000_500_i64;
    let product = eval_expr(
        &call(
            BuiltinFunction::OuterProduct,
            vec![array(vec![make_int(big)]), array(vec![make_int(big)])],
        ),
        &ctx,
    )
    .expect("the Real product is finite");
    assert_eq!(product, real_values(&[&[(big as f64) * (big as f64)]]));

    let skewed = eval_expr(
        &call(
            BuiltinFunction::Skew,
            vec![array(vec![make_int(i64::MIN), make_int(0), make_int(0)])],
        ),
        &ctx,
    )
    .expect("negating a converted Integer is a finite Real");
    assert_eq!(
        skewed,
        real_values(&[
            &[0.0, -0.0, 0.0],
            &[0.0, 0.0, -(i64::MIN as f64)],
            &[-0.0, i64::MIN as f64, 0.0],
        ])
    );
}

/// transpose permutes only the first two dimensions of a higher-rank array.
#[test]
fn transpose_keeps_the_trailing_dimensions_of_a_rank_three_array() {
    let cell = |a: i64, b: i64| array(vec![make_int(a), make_int(b)]);
    let a = array(vec![
        array(vec![cell(1, 2), cell(3, 4), cell(5, 6)]),
        array(vec![cell(7, 8), cell(9, 10), cell(11, 12)]),
    ]);
    let value = eval_expr(
        &call(BuiltinFunction::Transpose, vec![a]),
        &EvalContext::new(),
    )
    .expect("rank-3 transpose folds");
    let pair = |a: i64, b: i64| Value::Array(vec![Value::Integer(a), Value::Integer(b)]);
    assert_eq!(
        value,
        Value::Array(vec![
            Value::Array(vec![pair(1, 2), pair(7, 8)]),
            Value::Array(vec![pair(3, 4), pair(9, 10)]),
            Value::Array(vec![pair(5, 6), pair(11, 12)]),
        ])
    );
}

/// A builtin applied to an argument outside its MLS domain is a typed error
/// that proves the expression wrong (no runtime-dependent reason).
#[test]
fn array_builtins_refuse_arguments_outside_their_domain() {
    let refused = [
        (BuiltinFunction::Transpose, vec![make_vector(&[1, 2])]),
        (BuiltinFunction::Identity, vec![make_int(-1)]),
        (BuiltinFunction::Identity, vec![real(2.0)]),
        (BuiltinFunction::Diagonal, vec![make_matrix(&[&[1]])]),
        (
            BuiltinFunction::Diagonal,
            vec![array(vec![make_bool(true), make_bool(false)])],
        ),
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
        (
            BuiltinFunction::Symmetric,
            vec![array(vec![
                array(vec![make_vector(&[1, 2]), make_vector(&[3, 4])]),
                array(vec![make_vector(&[5, 6]), make_vector(&[7, 8])]),
            ])],
        ),
        (
            BuiltinFunction::Symmetric,
            vec![array(vec![
                array(vec![make_bool(true), make_bool(false)]),
                array(vec![make_bool(false), make_bool(true)]),
            ])],
        ),
        (BuiltinFunction::Smooth, vec![real(1.0)]),
        (BuiltinFunction::NoEvent, Vec::new()),
    ];
    let ctx = EvalContext::new();
    for (function, args) in refused {
        let error = eval_expr(&call(function, args), &ctx)
            .expect_err("an argument outside the domain is refused");
        assert_eq!(
            error.runtime_dependent_reason(),
            None,
            "{} outside its domain proves the expression wrong: {error}",
            function.name()
        );
    }
}

/// A result whose leading extent is zero has no rank or trailing extent this
/// evaluator's array values can carry (`identity(0)` would read as a vector),
/// so it is refused as an unimplemented form rather than folded to a value
/// of the wrong shape or reported as an invalid model.
#[test]
fn zero_extent_matrix_results_are_refused_as_unimplemented() {
    let empty = || array(Vec::new());
    let unrepresentable = [
        (BuiltinFunction::Identity, vec![make_int(0)]),
        (BuiltinFunction::Diagonal, vec![empty()]),
        (
            BuiltinFunction::Transpose,
            vec![array(vec![empty(), empty()])],
        ),
        (BuiltinFunction::Transpose, vec![empty()]),
        (BuiltinFunction::Symmetric, vec![empty()]),
        (
            BuiltinFunction::OuterProduct,
            vec![empty(), make_vector(&[1, 2])],
        ),
    ];
    let ctx = EvalContext::new();
    for (function, args) in unrepresentable {
        let error =
            eval_expr(&call(function, args), &ctx).expect_err("a shape-losing result is refused");
        assert_eq!(
            error.runtime_dependent_reason(),
            Some(RuntimeDependentReason::UnimplementedForm),
            "{}: {error}",
            function.name()
        );
    }

    // A zero trailing extent is carried by the non-empty leading dimension.
    let value = eval_expr(
        &call(
            BuiltinFunction::OuterProduct,
            vec![make_vector(&[1, 2]), empty()],
        ),
        &ctx,
    )
    .expect("a 2 x 0 result keeps its rows");
    assert_eq!(value, Value::Array(vec![Value::Array(Vec::new()); 2]));
}

/// identity, diagonal and outerProduct charge their result size against the
/// evaluator's element budget before building it: the default budget at a
/// binding, the caller's limits inside a function body.
#[test]
fn matrix_results_are_charged_against_the_element_budget() {
    let ctx = EvalContext::new();
    eval_expr(&call(BuiltinFunction::Identity, vec![make_int(316)]), &ctx)
        .expect("316 x 316 cells fit the default budget");
    let over_budget = [
        call(BuiltinFunction::Identity, vec![make_int(317)]),
        call(BuiltinFunction::Identity, vec![make_int(i64::MAX)]),
        call(
            BuiltinFunction::Diagonal,
            vec![array((0..317).map(make_int).collect())],
        ),
        call(
            BuiltinFunction::OuterProduct,
            vec![
                array((0..401).map(make_int).collect()),
                array((0..250).map(make_int).collect()),
            ],
        ),
    ];
    for expr in over_budget {
        let error = eval_expr(&expr, &ctx).expect_err("an over-budget result is refused");
        assert_eq!(
            error.runtime_dependent_reason(),
            Some(RuntimeDependentReason::UnimplementedForm),
            "{error}"
        );
    }

    let limits = EvalLimits {
        max_iterations: 8,
        ..EvalLimits::default()
    };
    // `y := size(identity(n), 1)`: the output is one Integer, so only the
    // intermediate matrix is charged.
    let extent_of_identity = |n: i64| {
        body_function(
            call(
                BuiltinFunction::Size,
                vec![
                    call(BuiltinFunction::Identity, vec![make_int(n)]),
                    make_int(1),
                ],
            ),
            output_param("Integer", Vec::new()),
        )
    };
    assert_eq!(
        fold_body(&extent_of_identity(2), &limits).expect("4 cells fit a budget of 8"),
        Value::Integer(2)
    );
    fold_body(&extent_of_identity(3), &limits).expect_err("9 cells exceed a budget of 8");
}
