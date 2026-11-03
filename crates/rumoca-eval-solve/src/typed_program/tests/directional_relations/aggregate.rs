use super::*;

const X: [f64; 8] = [
    -0.,
    0.,
    0.4,
    2.,
    -2.,
    f64::INFINITY,
    f64::NEG_INFINITY,
    f64::NAN,
];
const Y: [f64; 8] = [2., 0., 2.3, 1.7, 3., 2., -0., -0.];
const DX: [f64; 8] = [-0., 0., 0.3, -0.6, 0.2, 0., 1., -1.];
const DY: [f64; 8] = [0., 1., -0.6, 0.3, 0., 1., 0., -0.];

fn ty(format: SolveRealFormat) -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::real(profile(format)), vec![2, 4]).unwrap()
}

fn values(format: SolveRealFormat, values: &[f64]) -> TypedValue {
    TypedValue::construct(
        ty(format),
        values.iter().map(|v| real_kind(format, *v)).collect(),
    )
    .unwrap()
}

fn unary(format: SolveRealFormat, op: SolveUnaryOperator) -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(format), |table| {
        table.add_owner(
            identity(720),
            vec![ty(format); 2],
            vec![SolvePureCallOutput::result(ty(format)); 2],
            span(4300),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(4300))?;
                let dx = b.load(inputs[1], span(4300))?;
                let source = b.aggregate_unary_directional_source(op, x, span(4300))?;
                let tangent = b
                    .aggregate_unary_directional_tangent(source, Some(dx))?
                    .unwrap();
                b.store(outputs[0], source.primal(), span(4300))?;
                b.store(outputs[1], tangent, span(4300))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn binary(format: SolveRealFormat, op: SolveBinaryOperator) -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(format), |table| {
        table.add_owner(
            identity(721),
            vec![ty(format); 4],
            vec![SolvePureCallOutput::result(ty(format)); 2],
            span(4400),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(4400))?;
                let dx = b.load(inputs[1], span(4400))?;
                let y = b.load(inputs[2], span(4400))?;
                let dy = b.load(inputs[3], span(4400))?;
                let source = b.aggregate_binary_directional_source(op, [x, y], span(4400))?;
                let tangent = b
                    .aggregate_binary_directional_tangent(source, [Some(dx), Some(dy)])?
                    .unwrap();
                b.store(outputs[0], source.primal(), span(4400))?;
                b.store(outputs[1], tangent, span(4400))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn compact_unary_source_channels_match_independent_scalar_owners_bit_for_bit() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for op in [
            SolveUnaryOperator::Negate,
            SolveUnaryOperator::Abs,
            SolveUnaryOperator::Sin,
            SolveUnaryOperator::Cos,
            SolveUnaryOperator::Exp,
            SolveUnaryOperator::Log,
            SolveUnaryOperator::Sqrt,
            SolveUnaryOperator::Floor,
            SolveUnaryOperator::Sign,
        ] {
            let reference = unary_reference(format, op);
            let aggregate = unary(format, op);
            let output = eval_pure_call(
                &aggregate,
                aggregate.owners()[0].id(),
                &[values(format, &X), values(format, &DX)],
            )
            .unwrap();
            for index in 0..X.len() {
                let x = scalar(format, X[index]);
                let dx = scalar(format, DX[index]);
                let primal = eval_pure_call(
                    &reference,
                    reference.owners()[0].id(),
                    std::slice::from_ref(&x),
                )
                .unwrap();
                let directional =
                    eval_pure_call_directional(&reference, reference.owners()[0].id(), &[x, dx])
                        .unwrap();
                assert_eq!(
                    output[0].elements()[index],
                    primal[0].elements()[0],
                    "{op:?}/{format:?}/{index}: primal"
                );
                assert_eq!(
                    output[1].elements()[index],
                    directional[1].elements()[0],
                    "{op:?}/{format:?}/{index}: tangent"
                );
            }
        }
    }
}

#[test]
fn compact_binary_source_channels_match_independent_scalar_owners_bit_for_bit() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for op in [
            SolveBinaryOperator::Add,
            SolveBinaryOperator::Subtract,
            SolveBinaryOperator::Multiply,
            SolveBinaryOperator::Divide,
            SolveBinaryOperator::Power,
            SolveBinaryOperator::Atan2,
            SolveBinaryOperator::Min,
            SolveBinaryOperator::Max,
        ] {
            let reference = binary_reference(format, op);
            let aggregate = binary(format, op);
            let inputs = [
                values(format, &X),
                values(format, &DX),
                values(format, &Y),
                values(format, &DY),
            ];
            let output = eval_pure_call(&aggregate, aggregate.owners()[0].id(), &inputs).unwrap();
            for index in 0..X.len() {
                let point = [
                    scalar(format, X[index]),
                    scalar(format, DX[index]),
                    scalar(format, Y[index]),
                    scalar(format, DY[index]),
                ];
                let primal = eval_pure_call(
                    &reference,
                    reference.owners()[0].id(),
                    &[point[0].clone(), point[2].clone()],
                )
                .unwrap();
                let directional =
                    eval_pure_call_directional(&reference, reference.owners()[0].id(), &point)
                        .unwrap();
                assert_eq!(
                    output[0].elements()[index],
                    primal[0].elements()[0],
                    "{op:?}/{format:?}/{index}: primal"
                );
                assert_eq!(
                    output[1].elements()[index],
                    directional[1].elements()[0],
                    "{op:?}/{format:?}/{index}: tangent"
                );
            }
        }
    }
}
