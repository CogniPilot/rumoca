//! A value stored in a declaration takes the declared type (MLS 3.7
//! §10.6.13): an Integer actual of a Real input, an Integer assigned to a Real
//! output or bound to a Real local, computes as a Real afterwards.

use super::*;

/// A context that knows the fixtures' predefined type identities
/// (`real_param` is `TypeId(1)`, `integer_param` is `TypeId(2)`).
pub(super) fn typed_context() -> EvalContext {
    let mut ctx = EvalContext::new();
    ctx.set_predefined_types(rumoca_ir_flat::PredefinedTypeIds {
        real: rumoca_core::TypeId::new(1),
        integer: rumoca_core::TypeId::new(2),
        boolean: rumoca_core::TypeId::new(3),
        string: rumoca_core::TypeId::new(4),
        clock: rumoca_core::TypeId::new(5),
    });
    ctx
}

fn typed_fold(func: &Function, args: Vec<Value>) -> Result<Value, EvalError> {
    eval_function(
        func,
        args,
        &typed_context(),
        &EvalLimits::default(),
        0,
        Span::DUMMY,
    )
}

fn negate(expr: Expression) -> Expression {
    Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Box::new(expr),
        span: Span::DUMMY,
    }
}

fn real_vector_of(name: &str, extent: i64) -> rumoca_core::FunctionParam {
    let real = rumoca_core::TypeId::new(1);
    let effective_type =
        rumoca_core::EffectiveType::new(real, real, vec![extent]).expect("vector type");
    rumoca_core::FunctionParam::new(name, "Real", effective_type, test_span())
}

/// The `S[1, 2] := -v[3]` of a skew-symmetric wedge: `v` is declared Real, so
/// an Integer zero passed for it is the Real zero, and its negation is `-0.0`.
#[test]
fn an_integer_actual_of_a_real_input_negates_as_a_real() {
    let mut func = Function::new("test.wedgeEntry", Span::DUMMY);
    func.add_input(real_vector_of("v", 3));
    func.add_input(integer_param("i"));
    func.add_output(real_param("y"));
    func.pure = true;
    func.body = vec![assign(component_reference("y"), negate(indexed("v", "i")))];
    let value = typed_fold(
        &func,
        vec![
            Value::Array(vec![
                Value::Real(0.015),
                Value::Integer(0),
                Value::Integer(0),
            ]),
            Value::Integer(3),
        ],
    )
    .expect("the wedge entry folds");
    let Value::Real(y) = value else {
        panic!("a Real output holds a Real: {value:?}");
    };
    assert_eq!(y.to_bits(), (-0.0_f64).to_bits());
}

/// An Integer actual of a Real input multiplies as a Real: the product is
/// finite, not an Integer overflow.
#[test]
fn an_integer_actual_of_a_real_input_multiplies_as_a_real() {
    let mut func = Function::new("test.square", Span::DUMMY);
    func.add_input(real_param("x"));
    func.add_output(real_param("y"));
    func.pure = true;
    func.body = vec![assign(
        component_reference("y"),
        binary(rumoca_core::OpBinary::Mul, var_ref("x"), var_ref("x")),
    )];
    let big = 3_037_000_500_i64;
    assert_eq!(
        typed_fold(&func, vec![Value::Integer(big)]).expect("the Real square is finite"),
        Value::Real((big as f64) * (big as f64))
    );
}

/// An Integer assigned to a Real output, or bound to a Real local, is stored
/// as a Real.
#[test]
fn integers_stored_in_real_outputs_and_locals_are_reals() {
    let mut func = Function::new("test.store", Span::DUMMY);
    func.add_output(real_param("y"));
    func.add_output(real_param("z"));
    let mut zero = real_param("zero");
    zero.default = Some(integer_literal(0));
    func.add_local(zero);
    func.pure = true;
    func.body = vec![
        assign(component_reference("y"), integer_literal(2)),
        assign(component_reference("z"), negate(var_ref("zero"))),
    ];
    let value = typed_fold(&func, Vec::new()).expect("the stores fold");
    let Value::Array(outputs) = value else {
        panic!("two outputs: {value:?}");
    };
    assert_eq!(outputs[0], Value::Real(2.0));
    let Value::Real(z) = outputs[1] else {
        panic!("a Real output holds a Real: {:?}", outputs[1]);
    };
    assert_eq!(z.to_bits(), (-0.0_f64).to_bits());
}

/// Nothing converts a Real to an Integer, or a Boolean to a number: such an
/// actual is refused as a type error.
#[test]
fn a_value_that_does_not_convert_to_the_declared_type_is_refused() {
    let mut func = Function::new("test.identity", Span::DUMMY);
    func.add_input(integer_param("n"));
    func.add_output(integer_param("m"));
    func.pure = true;
    func.body = vec![assign(component_reference("m"), var_ref("n"))];
    let error = typed_fold(&func, vec![Value::Real(2.5)]).expect_err("Real to Integer is refused");
    assert_eq!(error.runtime_dependent_reason(), None, "{error}");

    let mut func = Function::new("test.real", Span::DUMMY);
    func.add_input(real_param("x"));
    func.add_output(real_param("y"));
    func.pure = true;
    func.body = vec![assign(component_reference("y"), var_ref("x"))];
    let error = typed_fold(&func, vec![Value::Bool(true)]).expect_err("Boolean to Real is refused");
    assert_eq!(error.runtime_dependent_reason(), None, "{error}");
}
