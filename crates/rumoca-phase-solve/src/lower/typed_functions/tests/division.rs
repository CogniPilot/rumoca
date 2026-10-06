//! Division keeps the source quotient, not a rounded reciprocal and product.

use super::*;
use rumoca_eval_solve::{TypedValue, eval_pure_call};

#[test]
fn plain_and_elementwise_tensor_division_preserve_direct_binary64_results() {
    assert_eq!((347.0_f64 / 3.0).to_bits(), 4_637_839_735_013_419_691);
    assert_eq!(
        (347.0_f64 * (1.0 / 3.0)).to_bits(),
        4_637_839_735_013_419_690
    );
    assert_eq!((394.0_f64 / 3.0).to_bits(), 0x4060_6aaa_aaaa_aaab);
    assert_eq!((394.0_f64 * (1.0 / 3.0)).to_bits(), 0x4060_6aaa_aaaa_aaaa);
    for shape in [vec![], vec![4], vec![2, 2], vec![2, 1, 2]] {
        for (operator, scalar_on_lhs) in [
            (dae::BinaryOperator::Divide, false),
            (dae::BinaryOperator::ElementwiseDivide, false),
            (dae::BinaryOperator::ElementwiseDivide, true),
        ] {
            check_division(operator, &shape, scalar_on_lhs);
        }
    }
}

fn check_division(operator: dae::BinaryOperator, shape: &[u32], scalar_on_lhs: bool) {
    let model = division_fixture(operator, shape, scalar_on_lhs);
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one source-issued division owner");
    };
    if !shape.is_empty() {
        assert_eq!(
            owner
                .body()
                .operations()
                .iter()
                .filter(|op| matches!(
                    op.operation(), solve::SolveOperation::BroadcastBinary {
                        operator: solve::SolveBinaryOperator::Divide,
                        scalar_on_lhs: orientation, ..
                    } if *orientation == scalar_on_lhs
                ))
                .count(),
            1
        );
        assert!(!owner.body().operations().iter().any(|op| matches!(
            op.operation(),
            solve::SolveOperation::Scale { .. }
                | solve::SolveOperation::Binary {
                    operator: solve::SolveBinaryOperator::Divide,
                    ..
                }
        )));
    }
    let width = owner.inputs()[0].scalar_count() as usize;
    for (numerators, divisor) in division_cases() {
        let inputs = [
            real_value(&owner.inputs()[0], &numerators[..width]),
            real_value(&owner.inputs()[1], &[divisor]),
        ];
        let results = eval_pure_call(&table, owner.id(), &inputs).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value_type(), owner.outputs()[0].value_type());
        for (&value, &numerator) in results[0].elements().iter().zip(&numerators[..width]) {
            let solve::SolveValueKind::Real64(actual) = value else {
                panic!("binary64 result");
            };
            let expected = if scalar_on_lhs {
                divisor / numerator
            } else {
                numerator / divisor
            };
            if expected.is_nan() {
                assert!(f64::from_bits(actual).is_nan());
            } else {
                assert_eq!(actual, expected.to_bits(), "{numerator:?}, {divisor:?}");
            }
        }
    }
}

fn real_value(value_type: &solve::SolveValueType, values: &[f64]) -> TypedValue {
    TypedValue::construct(
        value_type.clone(),
        values
            .iter()
            .map(|value| solve::SolveValueKind::Real64(value.to_bits()))
            .collect(),
    )
    .unwrap()
}

fn division_cases() -> Vec<([f64; 4], f64)> {
    let tiny = f64::from_bits(1);
    let nan = f64::from_bits(0x7ff8_0000_0000_0042);
    vec![
        ([347.0, 394.0, 439.0, 484.0], 3.0),
        ([-347.0, -394.0, -439.0, -484.0], 3.0),
        ([tiny, -tiny, 0.0, -0.0], tiny),
        ([1e-300, -1e-300, 1e-309, -1e-309], 1e-309),
        ([f64::MAX, -f64::MAX, tiny, -0.0], f64::MAX),
        ([f64::MAX, -f64::MAX, tiny, -tiny], f64::MIN_POSITIVE),
        ([1.0, -1.0, 0.0, -0.0], 0.0),
        ([1.0, -1.0, 0.0, -0.0], -0.0),
        (
            [f64::MAX, tiny, f64::INFINITY, f64::NEG_INFINITY],
            f64::INFINITY,
        ),
        (
            [f64::MAX, tiny, f64::INFINITY, f64::NEG_INFINITY],
            f64::NEG_INFINITY,
        ),
        ([1.0, 0.0, -0.0, f64::INFINITY], nan),
        ([nan, f64::NEG_INFINITY, f64::INFINITY, -0.0], 1.0),
    ]
}

fn division_fixture(operator: dae::BinaryOperator, shape: &[u32], scalar_on_lhs: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "division.mo",
        &division_source(operator, shape, scalar_on_lhs),
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (aggregate, scalar) = model.types(|types| {
            let aggregate = if shape.is_empty() {
                dae::ValueType::scalar(dae::ScalarType::Real)
            } else {
                dae::ValueType::array(dae::ScalarType::Real, shape.to_vec())
            };
            Ok((
                types.derived(aggregate, at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(
                VarName::new("quotient"),
                [aggregate, scalar],
                [aggregate],
                at,
            ),
            |model, reservation| {
                let (a, s, y) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("a"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("s"), 1, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let value = model.expressions(|expressions| {
                    let a = expressions.at(at).function_parameter(a)?;
                    let s = expressions.at(at).function_parameter(s)?;
                    let (lhs, rhs) = ordered_operands(scalar_on_lhs, s, a);
                    expressions.at(at).binary(operator, lhs, rhs)
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, y, value, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let (a, s) = model.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("a"),
                    aggregate,
                    dae::InputVariability::Continuous,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("s"),
                    scalar,
                    dae::InputVariability::Continuous,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(a))?;
            let s = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(s))?;
            expressions.at(at).call(function, 0, [a, s])?;
            Ok(())
        })
    })
    .unwrap()
}

fn ordered_operands<T>(scalar_on_lhs: bool, scalar: T, aggregate: T) -> (T, T) {
    if scalar_on_lhs {
        (scalar, aggregate)
    } else {
        (aggregate, scalar)
    }
}

fn division_source(operator: dae::BinaryOperator, shape: &[u32], scalar_on_lhs: bool) -> String {
    let suffix = if shape.is_empty() {
        String::new()
    } else {
        format!(
            "[{}]",
            shape
                .iter()
                .map(u32::to_string)
                .collect::<Vec<_>>()
                .join(",")
        )
    };
    let operation = match operator {
        dae::BinaryOperator::Divide => "/",
        dae::BinaryOperator::ElementwiseDivide => "./",
        _ => unreachable!("division fixture"),
    };
    let (lhs, rhs) = if scalar_on_lhs {
        ("s", "a")
    } else {
        ("a", "s")
    };
    format!(
        "function quotient input Real a{suffix}; input Real s; output Real y{suffix}; algorithm y := {lhs} {operation} {rhs}; end quotient;"
    )
}
