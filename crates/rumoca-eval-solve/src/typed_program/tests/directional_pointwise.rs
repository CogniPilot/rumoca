//! Tensor-destination elementwise operations keep a checked directional
//! relation: the scalar rule of each operator, applied per element.

use super::*;

const X: [f64; 4] = [0.4, 2.0, 0.25, 3.0];
const Y: [f64; 4] = [1.5, -0.5, 2.0, 0.75];
const DX: [f64; 4] = [0.3, -0.2, 0.7, 0.1];
const DY: [f64; 4] = [-0.6, 0.5, 0.2, 0.9];

fn tensor_type() -> SolveValueType {
    let arithmetic = profile(SolveRealFormat::Binary64);
    SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![4]).unwrap()
}

fn typed(values: &[f64]) -> TypedValue {
    TypedValue::construct(
        tensor_type(),
        values
            .iter()
            .map(|value| real_kind(SolveRealFormat::Binary64, *value))
            .collect(),
    )
    .unwrap()
}

fn numbers(value: &TypedValue) -> Vec<f64> {
    value
        .elements()
        .iter()
        .map(|value| match value {
            SolveValueKind::Real64(bits) => f64::from_bits(*bits),
            _ => panic!("expected Binary64 fixture"),
        })
        .collect()
}

fn unary_table(operator: SolveUnaryOperator) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(620),
            vec![tensor_type()],
            vec![SolvePureCallOutput::result(tensor_type())],
            span(1700),
            |builder, inputs, outputs| {
                let operand = builder.load(inputs[0], span(1701))?;
                let result = builder.unary(operator, operand, span(1702))?;
                builder.store(outputs[0], result, span(1703))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn binary_table(operator: SolveBinaryOperator) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(621),
            vec![tensor_type(), tensor_type()],
            vec![SolvePureCallOutput::result(tensor_type())],
            span(1710),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span(1711))?;
                let rhs = builder.load(inputs[1], span(1712))?;
                let result = builder.binary(operator, lhs, rhs, span(1713))?;
                builder.store(outputs[0], result, span(1714))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn broadcast_table(operator: SolveBinaryOperator, scalar_on_lhs: bool) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(622),
            vec![tensor_type(), scalar],
            vec![SolvePureCallOutput::result(tensor_type())],
            span(1720),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1721))?;
                let scalar = builder.load(inputs[1], span(1722))?;
                let result = builder.broadcast_binary(
                    operator,
                    aggregate,
                    scalar,
                    scalar_on_lhs,
                    span(1723),
                )?;
                builder.store(outputs[0], result, span(1724))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn run(table: &SolvePureCallTable, arguments: &[TypedValue]) -> (Vec<f64>, Vec<f64>) {
    let owner = &table.owners()[0];
    assert!(owner.directional().is_some(), "directional owner exists");
    let outputs = eval_pure_call_directional(table, owner.id(), arguments).unwrap();
    (numbers(&outputs[0]), numbers(&outputs[1]))
}

fn unary_case(
    operator: SolveUnaryOperator,
    x: &[f64],
    primal: fn(f64) -> f64,
    partial: fn(f64) -> f64,
) {
    let (value, tangent) = run(&unary_table(operator), &[typed(x), typed(&DX)]);
    for i in 0..4 {
        assert!((value[i] - primal(x[i])).abs() < 1e-14, "{operator:?} {i}");
        assert!(
            (tangent[i] - partial(x[i]) * DX[i]).abs() < 1e-12,
            "{operator:?} {i}"
        );
    }
}

#[test]
fn tensor_unary_roots_logs_and_arcs_have_elementwise_tangents() {
    let x = [0.4, 0.25, 0.9, 0.1];
    unary_case(SolveUnaryOperator::Sqrt, &x, f64::sqrt, |x| 0.5 / x.sqrt());
    unary_case(SolveUnaryOperator::Log, &x, f64::ln, |x| 1.0 / x);
    unary_case(SolveUnaryOperator::Log10, &x, f64::log10, |x| {
        1.0 / (x * std::f64::consts::LN_10)
    });
    unary_case(SolveUnaryOperator::Asin, &x, f64::asin, |x| {
        1.0 / (1.0 - x * x).sqrt()
    });
    unary_case(SolveUnaryOperator::Acos, &x, f64::acos, |x| {
        -1.0 / (1.0 - x * x).sqrt()
    });
}

#[test]
fn tensor_unary_tangent_is_zero_where_the_partial_does_not_exist() {
    let x = [0.0, 1.0, -1.0, 4.0];
    let args = [typed(&x), typed(&DX)];
    let (_, sqrt) = run(&unary_table(SolveUnaryOperator::Sqrt), &args);
    assert_eq!(sqrt[0], 0.0);
    assert!((sqrt[3] - 0.25 * DX[3]).abs() < 1e-14);
    let (_, log) = run(&unary_table(SolveUnaryOperator::Log), &args);
    assert_eq!(log[0], 0.0);
    let (_, asin) = run(&unary_table(SolveUnaryOperator::Asin), &args);
    assert_eq!((asin[1], asin[2]), (0.0, 0.0));
}

fn binary_expected(operator: SolveBinaryOperator, x: f64, y: f64, dx: f64, dy: f64) -> (f64, f64) {
    match operator {
        SolveBinaryOperator::Divide => (x / y, dx / y - x * dy / (y * y)),
        SolveBinaryOperator::Power => (
            x.powf(y),
            y * x.powf(y - 1.0) * dx
                + if x > 0.0 {
                    x.powf(y) * x.ln() * dy
                } else {
                    0.0
                },
        ),
        SolveBinaryOperator::Min => (x.min(y), if x <= y { dx } else { dy }),
        SolveBinaryOperator::Max => (x.max(y), if x >= y { dx } else { dy }),
        SolveBinaryOperator::Atan2 => (x.atan2(y), (y * dx - x * dy) / (x * x + y * y)),
        _ => unreachable!("fixture covers the pointwise operators"),
    }
}

const POINTWISE: [SolveBinaryOperator; 5] = [
    SolveBinaryOperator::Divide,
    SolveBinaryOperator::Power,
    SolveBinaryOperator::Min,
    SolveBinaryOperator::Max,
    SolveBinaryOperator::Atan2,
];

#[test]
fn tensor_binary_operations_have_elementwise_tangents() {
    for operator in POINTWISE {
        let (value, tangent) = run(
            &binary_table(operator),
            &[typed(&X), typed(&DX), typed(&Y), typed(&DY)],
        );
        for i in 0..4 {
            let (v, t) = binary_expected(operator, X[i], Y[i], DX[i], DY[i]);
            assert!((value[i] - v).abs() < 1e-13, "{operator:?} value {i}");
            assert!((tangent[i] - t).abs() < 1e-12, "{operator:?} tangent {i}");
        }
    }
}

#[test]
fn tensor_quotient_and_power_tangents_follow_their_domain_conditions() {
    let x = [1.0, 0.0, 0.0, 2.0];
    let y = [0.0, 0.0, 2.0, 3.0];
    let args = [typed(&x), typed(&DX), typed(&y), typed(&DY)];
    let (_, quotient) = run(&binary_table(SolveBinaryOperator::Divide), &args);
    // Both partials are non-finite at a zero denominator and contribute zero.
    assert_eq!(&quotient[..2], &[0.0, 0.0]);
    let (_, power) = run(&binary_table(SolveBinaryOperator::Power), &args);
    // The exponent partial exists only for a positive base.
    assert_eq!(power[1], 0.0);
    assert_eq!(power[2], 2.0 * 0.0 * DX[2]);
    let expected = 3.0 * 4.0 * DX[3] + 8.0 * 2.0f64.ln() * DY[3];
    assert!((power[3] - expected).abs() < 1e-12);
}

fn check_broadcast(operator: SolveBinaryOperator, scalar_on_lhs: bool) {
    let (scalar, dscalar) = (0.8, 0.45);
    let table = broadcast_table(operator, scalar_on_lhs);
    let scalar_type = table.owners()[0].inputs()[1].clone();
    let scalar_value = |value: f64| {
        TypedValue::construct(
            scalar_type.clone(),
            vec![real_kind(SolveRealFormat::Binary64, value)],
        )
        .unwrap()
    };
    let (value, tangent) = run(
        &table,
        &[
            typed(&X),
            typed(&DX),
            scalar_value(scalar),
            scalar_value(dscalar),
        ],
    );
    for i in 0..4 {
        let (v, t) = if scalar_on_lhs {
            binary_expected(operator, scalar, X[i], dscalar, DX[i])
        } else {
            binary_expected(operator, X[i], scalar, DX[i], dscalar)
        };
        assert!((value[i] - v).abs() < 1e-13, "{operator:?} value {i}");
        assert!((tangent[i] - t).abs() < 1e-12, "{operator:?} tangent {i}");
    }
}

#[test]
fn broadcast_extrema_arctangent_and_scalar_numerator_quotient_have_tangents() {
    for operator in [
        SolveBinaryOperator::Min,
        SolveBinaryOperator::Max,
        SolveBinaryOperator::Atan2,
    ] {
        check_broadcast(operator, false);
        check_broadcast(operator, true);
    }
    check_broadcast(SolveBinaryOperator::Divide, true);
}
