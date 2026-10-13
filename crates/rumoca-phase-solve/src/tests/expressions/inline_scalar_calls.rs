//! A call site whose compact ABI is scalar lowers to scalar row operations;
//! a site with a tensor argument or result keeps its compact owner.

use super::*;

/// `z - f(z)` with `f(u) = u * u + 1`: a Y-dependent call whose
/// owner has a directional relation. A tensor `f` returns `u` unchanged.
fn call_model(tensor: bool) -> dae::Dae {
    let source = TestSource::new("function f input Real u; output Real y; end f; Real z; z=f(z);");
    let at = source.at(0, 60);
    dae::Dae::construct(source.map, |model| {
        let (real, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
            ))
        })?;
        let value_type = if tensor { vector } else { real };
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), [value_type], [value_type], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let input = model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
            let output = model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
            let value = model.expressions(|e| body_value(e, input, at, tensor))?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            model.functions(|f| {
                f.assign(&mut body, output, value, at)?;
                f.define(body, at)
            })
        })?;
        let variable = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                value_type,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|e| {
            let z = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            let call = e.at(at).call(function, 0, [z])?;
            e.at(at).binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap()
}

fn retained_call_count(model: &dae::Dae) -> usize {
    let package = lower_solve_package(model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = package.problem.continuous.residual.nodes.as_slice()
    else {
        panic!("one residual block");
    };
    rows.programs()
        .iter()
        .flatten()
        .filter(|operation| {
            matches!(
                operation,
                LinearOp::PureCall { .. } | LinearOp::PureCallDirectional { .. }
            )
        })
        .count()
}

#[test]
fn y_dependent_scalar_call_site_expands_inline_without_a_retained_owner() {
    let model = call_model(false);
    assert!(
        lower_solve_package(&model).unwrap().pure_calls.owners()[0]
            .directional()
            .is_some(),
        "the scalar owner has a directional relation, which must not retain it"
    );
    assert_eq!(retained_call_count(&model), 0);
}

#[test]
fn y_dependent_tensor_call_site_keeps_its_compact_owner() {
    assert_eq!(retained_call_count(&call_model(true)), 1);
}

fn body_value<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    input: dae::FunctionParameterId<'dae>,
    at: dae::DaeProvenance,
    tensor: bool,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let input = expressions.at(at).function_parameter(input)?;
    if tensor {
        return Ok(input);
    }
    let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
    let square = expressions
        .at(at)
        .binary(dae::BinaryOperator::Multiply, input, input)?;
    expressions
        .at(at)
        .binary(dae::BinaryOperator::Add, square, one)
}

/// The body shapes `ordered_call_model` builds around `z = f(z)`.
#[derive(Clone, Copy)]
enum OrderedBody {
    /// `y := u; if u > 0 then y := u * u + 1; end if;`: the missing `else`
    /// leaves the output at its incoming value.
    KeepsIncoming,
    /// `y := if u == 9007199254740993 then 0 else u;`: the Integer literal is
    /// beyond binary64.
    InexactInteger,
}

fn ordered_call_model(body_shape: OrderedBody) -> dae::Dae {
    let source = TestSource::new("function f input Real u; output Real y; end f; Real z; z=f(z);");
    let at = source.at(0, 60);
    dae::Dae::construct(source.map, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let input = model.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
            let output = model.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
            let mut body = model.functions(|f| f.begin(reservation, at))?;
            let values = model.expressions(|e| shaped_values(e, input, body_shape, at))?;
            body = model.functions(|f| shaped_body(f, body_shape, body, output, values, at))?;
            model.functions(|f| f.define(body, at))
        })?;
        let variable = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|e| {
            let z = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            let call = e.at(at).call(function, 0, [z])?;
            e.at(at).binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap()
}

#[test]
fn pure_ordered_conditional_call_site_expands_inline_without_a_retained_owner() {
    assert_eq!(
        retained_call_count(&ordered_call_model(OrderedBody::KeepsIncoming)),
        0
    );
}

#[test]
fn body_with_an_integer_beyond_binary64_keeps_its_typed_owner() {
    assert_eq!(
        retained_call_count(&ordered_call_model(OrderedBody::InexactInteger)),
        1
    );
}

fn keeps_incoming_values<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    input: dae::FunctionParameterId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(dae::ExprId<'dae>, dae::ExprId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError> {
    let u = e.at(at).function_parameter(input)?;
    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
    let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
    let square = e.at(at).binary(dae::BinaryOperator::Multiply, u, u)?;
    let guard = e.at(at).binary(dae::BinaryOperator::Greater, u, zero)?;
    let value = e.at(at).binary(dae::BinaryOperator::Add, square, one)?;
    Ok((u, guard, value))
}

fn inexact_integer_value<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    input: dae::FunctionParameterId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let u = e.at(at).function_parameter(input)?;
    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
    let huge = e
        .at(at)
        .literal(dae::DaeLiteral::Integer(9_007_199_254_740_993))?;
    let equal = e.at(at).binary(dae::BinaryOperator::Equal, u, huge)?;
    e.at(at).conditional([(equal, zero)], u)
}

/// `y := incoming; if guard then y := value; end if;` with an empty `else`.
fn keeps_incoming_body<'dae>(
    f: &mut dae::Functions<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    output: dae::FunctionValueId<'dae>,
    [incoming, guard, value]: [dae::ExprId<'dae>; 3],
    at: dae::DaeProvenance,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    f.assign(&mut body, output, incoming, at)?;
    let (mut branch, finish) = f.begin_branch(body, at)?;
    f.assign(&mut branch, output, value, at)?;
    let (parent, taken) = f.finish_branch(finish, branch)?;
    let (keep, finish) = f.begin_branch(parent, at)?;
    let (mut parent, kept) = f.finish_branch(finish, keep)?;
    f.finish_conditional(&mut parent, vec![(guard, taken)], kept, at)?;
    Ok(parent)
}

/// The expressions one body shape assigns: incoming value, guard, taken value.
fn shaped_values<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    input: dae::FunctionParameterId<'dae>,
    shape: OrderedBody,
    at: dae::DaeProvenance,
) -> Result<[dae::ExprId<'dae>; 3], dae::DaeConstructionError> {
    match shape {
        OrderedBody::KeepsIncoming => {
            let (incoming, guard, value) = keeps_incoming_values(e, input, at)?;
            Ok([incoming, guard, value])
        }
        OrderedBody::InexactInteger => {
            let value = inexact_integer_value(e, input, at)?;
            Ok([value; 3])
        }
    }
}

fn shaped_body<'dae>(
    f: &mut dae::Functions<'_, 'dae>,
    shape: OrderedBody,
    mut body: dae::FunctionBody<'dae>,
    output: dae::FunctionValueId<'dae>,
    values: [dae::ExprId<'dae>; 3],
    at: dae::DaeProvenance,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    match shape {
        OrderedBody::KeepsIncoming => keeps_incoming_body(f, body, output, values, at),
        OrderedBody::InexactInteger => {
            f.assign(&mut body, output, values[0], at)?;
            Ok(body)
        }
    }
}
