use rumoca_core::{SourceMap, Span, VarName};
use rumoca_ir_dae as dae;

use super::FunctionCallContext;

fn source() -> (SourceMap, dae::DaeProvenance) {
    let mut sources = SourceMap::new();
    let id = sources.add("selector.mo", "if selector then u else -u");
    let at = dae::DaeProvenance::source(Span::from_offsets(id, 0, 25)).unwrap();
    (sources, at)
}

fn integer_guard<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    at: dae::DaeProvenance,
    operation: dae::BinaryOperator,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let guard = expressions.at(at).binary(operation, lhs, rhs)?;
    if operation == dae::BinaryOperator::Add {
        expressions
            .at(at)
            .binary(dae::BinaryOperator::Equal, guard, rhs)
    } else {
        Ok(guard)
    }
}

#[test]
fn integer_guards_are_exact_and_overflow_stays_unknown() {
    use dae::BinaryOperator as Op;
    let (sources, at) = source();
    let mut cases = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        model.expressions(|expressions| {
            let yes = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let no = expressions.at(at).literal(dae::DaeLiteral::Real(-1.0))?;
            for (lhs, rhs, operation, expected) in [
                (
                    9_007_199_254_740_992,
                    9_007_199_254_740_993,
                    Op::Equal,
                    Some(false),
                ),
                (
                    9_007_199_254_740_992,
                    9_007_199_254_740_993,
                    Op::Less,
                    Some(true),
                ),
                (i64::MAX, 1, Op::Add, None),
            ] {
                let lhs = expressions.at(at).literal(dae::DaeLiteral::Integer(lhs))?;
                let rhs = expressions.at(at).literal(dae::DaeLiteral::Integer(rhs))?;
                let guard = integer_guard(expressions, at, operation, lhs, rhs)?;
                let conditional = expressions.at(at).conditional([(guard, yes)], no)?;
                cases.push((
                    conditional.index(),
                    expected.map(|yes_branch| [no.index(), yes.index()][usize::from(yes_branch)]),
                ));
            }
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        for (expression, expected) in cases {
            let expression = view.expression_id(expression as usize).unwrap();
            assert_eq!(
                FunctionCallContext::default()
                    .selected_branch(view, expression)
                    .map(|id| id.index()),
                expected
            );
        }
    });
}

#[test]
fn parameter_defaults_and_runtime_guards_cannot_select_a_branch() {
    let (sources, at) = source();
    let mut cases = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        let boolean = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at))?;
        let binding = model.expressions(|expressions| {
            expressions.at(at).literal(dae::DaeLiteral::Boolean(true))
        })?;
        let attributes = dae::VariableAttributes {
            binding: Some(binding),
            ..Default::default()
        };
        let constant = model.variables(|variables| {
            variables.constant(
                VarName::new("constantGuard"),
                boolean,
                at,
                attributes.clone(),
            )
        })?;
        let parameter = model.variables(|variables| {
            variables.parameter(VarName::new("parameterGuard"), boolean, at, attributes)
        })?;
        model.expressions(|expressions| {
            let yes = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let no = expressions.at(at).literal(dae::DaeLiteral::Real(-1.0))?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let runtime = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, time, yes)?;
            let constant = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(constant))?;
            let parameter = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            for (guard, expected) in [
                (constant, Some(yes.index())),
                (parameter, None),
                (runtime, None),
            ] {
                let conditional = expressions.at(at).conditional([(guard, yes)], no)?;
                cases.push((conditional.index(), expected));
            }
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        for (expression, expected) in cases {
            let expression = view.expression_id(expression as usize).unwrap();
            assert_eq!(
                FunctionCallContext::default()
                    .selected_branch(view, expression)
                    .map(|id| id.index()),
                expected
            );
        }
    });
}

fn select_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    integer: dae::ValueTypeId<'dae>,
    assert_guard: bool,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature =
        dae::FunctionSignature::new(VarName::new("select"), [integer, vector], [vector], at);
    model
        .function(signature, |model, reservation| {
            let selector = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("selector"), 0, at)
            })?;
            let value = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let (result, guard, message) = model.expressions(|expressions| {
                let selector = expressions.at(at).function_parameter(selector)?;
                let value = expressions.at(at).function_parameter(value)?;
                let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
                let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
                let guard = expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Equal, selector, one)?;
                let other = expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Equal, selector, two)?;
                let negative = expressions
                    .at(at)
                    .unary(dae::UnaryOperator::Negate, value)?;
                let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                let fallback =
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, zero, value)?;
                let result = expressions
                    .at(at)
                    .conditional([(guard, value), (other, negative)], fallback)?;
                let message = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::String("guard must hold".into()))?;
                Ok((result, guard, message))
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            if assert_guard {
                model.functions(|functions| functions.assertion(&mut body, guard, message, at))?;
            }
            model.functions(|functions| functions.assign(&mut body, output, result, at))?;
            model.functions(|functions| functions.define(body, at))
        })
        .map(|(function, ())| function)
}

fn wrapper_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    select: dae::FunctionId<'dae>,
    vector: dae::ValueTypeId<'dae>,
    sequence: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature =
        dae::FunctionSignature::new(VarName::new("wrapper"), [sequence, vector], [vector], at);
    model
        .function(signature, |model, reservation| {
            let selector = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("sequence"), 0, at)
            })?;
            let value = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let result = model.expressions(|expressions| {
                let selector = expressions.at(at).function_parameter(selector)?;
                let value = expressions.at(at).function_parameter(value)?;
                let index = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
                let selector = expressions.at(at).index(
                    selector,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: at,
                    }],
                )?;
                expressions.at(at).call(select, 0, [selector, value])
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, result, at))?;
            model.functions(|functions| functions.define(body, at))
        })
        .map(|(function, ())| function)
}

fn nested_selector_model(extent: u32) -> (dae::Dae, Vec<(u32, i64)>) {
    let (sources, at) = source();
    let mut calls = Vec::new();
    let model = dae::Dae::construct(sources, |model| {
        let vector = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)
        })?;
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let sequence = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Integer, [3]), at)
        })?;
        let select = select_function(model, vector, integer, false, at)?;
        let wrapper = wrapper_function(model, select, vector, sequence, at)?;
        let value = model.variables(|variables| {
            variables.algebraic(
                VarName::new("runtimeTensor"),
                vector,
                at,
                Default::default(),
            )
        })?;
        model.expressions(|expressions| {
            let value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(value))?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            for axis in [1, 2, 3] {
                let axis_value = expressions.at(at).literal(dae::DaeLiteral::Integer(axis))?;
                let sequence = expressions.at(at).array([one, axis_value, one])?;
                let call = expressions.at(at).call(wrapper, 0, [sequence, value])?;
                calls.push((call.index(), axis));
            }
            Ok(())
        })
    })
    .unwrap();
    (model, calls)
}

#[test]
fn nested_selectors_keep_call_sites_distinct_without_expanding_tensors() {
    let mut counts = Vec::new();
    for extent in [3, 4096] {
        let (model, calls) = nested_selector_model(extent);
        model.inspect(|view| {
            counts.push(view.expression_count());
            for (call, axis) in calls {
                let root = FunctionCallContext::default();
                let (outer, context) = root
                    .call_result(view, view.expression_id(call as usize).unwrap())
                    .unwrap();
                let (conditional, context) = context.call_result(view, outer).unwrap();
                let selected = context.selected_branch(view, conditional).unwrap();
                let operation = view.expression(selected).unwrap().operation();
                assert!(match axis {
                    1 => matches!(
                        operation,
                        dae::ExpressionOperation::Coordinate(
                            dae::CoordinateView::FunctionParameter(_)
                        )
                    ),
                    2 => matches!(
                        operation,
                        dae::ExpressionOperation::Unary {
                            operator: dae::UnaryOperator::Negate,
                            ..
                        }
                    ),
                    3 => matches!(
                        operation,
                        dae::ExpressionOperation::Binary {
                            operator: dae::BinaryOperator::Multiply,
                            ..
                        }
                    ),
                    _ => unreachable!(),
                });
            }
        });
    }
    assert_eq!(counts[0], counts[1]);
}

#[test]
fn functions_with_assertions_cannot_be_substituted_away() {
    let (sources, at) = source();
    let mut call_id = None;
    let model = dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let function = select_function(model, real, integer, true, at)?;
        model.expressions(|expressions| {
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let value = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
            call_id = Some(expressions.at(at).call(function, 0, [one, value])?.index());
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let call = view.expression_id(call_id.unwrap() as usize).unwrap();
        assert!(
            FunctionCallContext::default()
                .call_result(view, call)
                .is_none()
        );
    });
}
