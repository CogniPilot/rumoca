//! Issued primal handles reuse the original directional arithmetic relation.
use super::*;

fn scalar(format: SolveRealFormat, value: f64) -> TypedValue {
    TypedValue::construct(
        SolveValueType::scalar(SolveScalarType::real(profile(format))),
        vec![real_kind(format, value)],
    )
    .unwrap()
}

fn unary_reference(format: SolveRealFormat, operator: SolveUnaryOperator) -> SolvePureCallTable {
    let arithmetic = profile(format);
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(710),
            vec![ty.clone()],
            vec![SolvePureCallOutput::result(ty)],
            span(4100),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(4100))?;
                let value = b.unary(operator, x, span(4100))?;
                b.store(outputs[0], value, span(4100))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn unary_retained(
    format: SolveRealFormat,
    operator: SolveUnaryOperator,
    present: bool,
) -> SolvePureCallTable {
    let arithmetic = profile(format);
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(711),
            vec![ty.clone(); 2],
            vec![SolvePureCallOutput::result(ty); 2],
            span(4100),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(4100))?;
                let dx = b.load(inputs[1], span(4100))?;
                let source = b.scalar_unary_directional_source(operator, x, span(4100))?;
                let tangent = b
                    .scalar_unary_directional_tangent(source, present.then_some(dx))?
                    .unwrap();
                b.store(outputs[0], source.primal(), span(4100))?;
                b.store(outputs[1], tangent, span(4100))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn binary_reference(format: SolveRealFormat, operator: SolveBinaryOperator) -> SolvePureCallTable {
    let arithmetic = profile(format);
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(712),
            vec![ty.clone(); 2],
            vec![SolvePureCallOutput::result(ty)],
            span(4200),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(4200))?;
                let y = b.load(inputs[1], span(4200))?;
                let value = b.binary(operator, x, y, span(4200))?;
                b.store(outputs[0], value, span(4200))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn binary_retained(format: SolveRealFormat, operator: SolveBinaryOperator) -> SolvePureCallTable {
    let arithmetic = profile(format);
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(713),
            vec![ty.clone(); 4],
            vec![SolvePureCallOutput::result(ty); 2],
            span(4200),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(4200))?;
                let dx = b.load(inputs[1], span(4200))?;
                let y = b.load(inputs[2], span(4200))?;
                let dy = b.load(inputs[3], span(4200))?;
                let source = b.scalar_binary_directional_source(operator, [x, y], span(4200))?;
                let tangent = b
                    .scalar_binary_directional_tangent(source, [Some(dx), Some(dy)])?
                    .unwrap();
                b.store(outputs[0], source.primal(), span(4200))?;
                b.store(outputs[1], tangent, span(4200))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn assert_original_channels(
    primal: Result<Vec<TypedValue>, TypedProgramEvalError>,
    directional: Result<Vec<TypedValue>, TypedProgramEvalError>,
    retained: Result<Vec<TypedValue>, TypedProgramEvalError>,
) {
    match (primal, directional, retained) {
        (Ok(primal), Ok(directional), Ok(retained)) => {
            // The directional owner's guarded primal is deliberately not a
            // replacement for the original value (notably for zero/zero).
            assert_eq!(retained[0], primal[0]);
            assert_eq!(retained[1], directional[1]);
        }
        (Err(original), _, Err(retained)) | (Ok(_), Err(original), Err(retained)) => {
            assert_eq!(retained, original)
        }
        other => panic!("original channels and retained relation disagree: {other:?}"),
    }
}

#[test]
fn issued_unary_relations_match_original_value_and_canonical_tangent_bits() {
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
            let retained = unary_retained(format, op, true);
            for x in [
                0.4,
                2.,
                0.,
                -0.,
                -2.,
                f64::INFINITY,
                f64::NEG_INFINITY,
                f64::NAN,
            ] {
                let x = scalar(format, x);
                let dx = scalar(format, -0.3);
                assert_original_channels(
                    eval_pure_call(
                        &reference,
                        reference.owners()[0].id(),
                        std::slice::from_ref(&x),
                    ),
                    eval_pure_call_directional(
                        &reference,
                        reference.owners()[0].id(),
                        &[x.clone(), dx.clone()],
                    ),
                    eval_pure_call(&retained, retained.owners()[0].id(), &[x, dx]),
                );
            }
        }
    }
}

#[test]
fn issued_binary_relations_match_original_value_and_canonical_tangent_bits() {
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
            let retained = binary_retained(format, op);
            for [x, y, dx, dy] in [
                [1.7, 2.3, 0.3, -0.6],
                [-0., 2., -0., 0.],
                [0., 0., 1., 0.],
                [f64::INFINITY, 2., 1., 0.],
                [f64::NAN, -0., 0., 1.],
                [2., f64::NEG_INFINITY, -0., 1.],
            ] {
                let arguments = [
                    scalar(format, x),
                    scalar(format, dx),
                    scalar(format, y),
                    scalar(format, dy),
                ];
                assert_original_channels(
                    eval_pure_call(
                        &reference,
                        reference.owners()[0].id(),
                        &[arguments[0].clone(), arguments[2].clone()],
                    ),
                    eval_pure_call_directional(&reference, reference.owners()[0].id(), &arguments),
                    eval_pure_call(&retained, retained.owners()[0].id(), &arguments),
                );
            }
        }
    }
}

#[test]
fn present_negative_zero_is_not_replaced_by_an_absent_direction() {
    let format = SolveRealFormat::Binary64;
    let present = unary_retained(format, SolveUnaryOperator::Negate, true);
    let absent = unary_retained(format, SolveUnaryOperator::Negate, false);
    let inputs = [scalar(format, 2.), scalar(format, -0.)];
    let positive_zero = eval_pure_call(&present, present.owners()[0].id(), &inputs).unwrap();
    let negative_zero = eval_pure_call(&absent, absent.owners()[0].id(), &inputs).unwrap();
    assert_eq!(positive_zero[1].elements(), &[real_kind(format, 0.)]);
    assert_eq!(negative_zero[1].elements(), &[real_kind(format, -0.)]);
}
