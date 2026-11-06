//! Constructor folds preserve bits on their proof domain and keep IEEE work
//! outside it. Raw NaN payloads are supplied without numeric conversion.
use super::*;
use rumoca_ir_solve::{SolveFiniteCheck, SolveOperation, SolveSlotAccess, SolveStorageClass};

fn owner(
    format: SolveRealFormat,
    operation: SolveBinaryOperator,
    checked: bool,
    keep_arithmetic: bool,
) -> SolvePureCallTable {
    let p = profile(format);
    let ty = SolveValueType::scalar(SolveScalarType::real(p));
    SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(7100),
            vec![ty.clone()],
            vec![SolvePureCallOutput::result(ty.clone())],
            span(7100),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(7101))?;
                if checked {
                    b.check_finite(input, SolveFiniteCheck::Input { coordinate: 7 }, span(7102))?;
                }
                let operand = if keep_arithmetic {
                    let local = b.declare_slot(
                        ty,
                        SolveStorageClass::MethodLocal,
                        SolveSlotAccess::ReadWrite,
                        span(7103),
                    )?;
                    b.store(local, input, span(7104))?;
                    b.load(local, span(7105))?
                } else {
                    input
                };
                let constant = b.constant(
                    SolveValue::real(
                        p,
                        if operation == SolveBinaryOperator::Subtract {
                            0.0
                        } else {
                            1.0
                        },
                    ),
                    span(7106),
                )?;
                let result = b.binary(operation, operand, constant, span(7107))?;
                b.store(outputs[0], result, span(7108))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn cases(format: SolveRealFormat) -> Vec<SolveValueKind> {
    match format {
        SolveRealFormat::Binary32 => [
            0,
            0x8000_0000,
            1,
            0x8000_0001,
            0x3f80_0000,
            0xc000_0000,
            0x7f80_0000,
            0xff80_0000,
            0x7fc0_0123,
            0xffc0_0456,
            0x7f80_0123,
            0xff80_0456,
        ]
        .map(SolveValueKind::Real32)
        .to_vec(),
        SolveRealFormat::Binary64 => [
            0,
            0x8000_0000_0000_0000,
            1,
            0x8000_0000_0000_0001,
            0x3ff0_0000_0000_0000,
            0xc000_0000_0000_0000,
            0x7ff0_0000_0000_0000,
            0xfff0_0000_0000_0000,
            0x7ff8_0000_0000_0123,
            0xfff8_0000_0000_0456,
            0x7ff0_0000_0000_0123,
            0xfff0_0000_0000_0456,
        ]
        .map(SolveValueKind::Real64)
        .to_vec(),
    }
}

#[test]
fn checked_aliases_keep_finite_bits_and_the_first_nonfinite_error() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for operation in [
            SolveBinaryOperator::Multiply,
            SolveBinaryOperator::Divide,
            SolveBinaryOperator::Subtract,
        ] {
            let folded = owner(format, operation, true, false);
            let kept = owner(format, operation, true, true);
            let a = &folded.owners()[0];
            let b = &kept.owners()[0];
            assert!(
                !a.body()
                    .operations()
                    .iter()
                    .any(|op| matches!(op.operation(), SolveOperation::Binary { .. }))
            );
            assert!(
                b.body()
                    .operations()
                    .iter()
                    .any(|op| matches!(op.operation(), SolveOperation::Binary { .. }))
            );
            for bits in cases(format) {
                compare_checked_case(&folded, &kept, bits);
            }
        }
    }
}

#[test]
fn raw_ieee_inputs_keep_arithmetic_and_double_negation_keeps_every_bit() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for operation in [
            SolveBinaryOperator::Multiply,
            SolveBinaryOperator::Divide,
            SolveBinaryOperator::Subtract,
        ] {
            let table = owner(format, operation, false, false);
            assert!(
                table.owners()[0]
                    .body()
                    .operations()
                    .iter()
                    .any(|op| matches!(op.operation(), SolveOperation::Binary { .. })),
                "unknown sNaN inputs must not become aliases"
            );
        }
        let p = profile(format);
        let ty = SolveValueType::scalar(SolveScalarType::real(p));
        let table = SolvePureCallTable::construct(p, |table| {
            table.add_owner(
                identity(7101),
                vec![ty.clone()],
                vec![SolvePureCallOutput::result(ty)],
                span(7110),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(7111))?;
                    let negative = b.unary(SolveUnaryOperator::Negate, x, span(7112))?;
                    let positive = b.unary(SolveUnaryOperator::Negate, negative, span(7113))?;
                    assert_eq!(positive, x);
                    b.store(outputs[0], positive, span(7114))
                },
            )?;
            Ok(())
        })
        .unwrap();
        let owner = &table.owners()[0];
        for bits in cases(format) {
            let input = TypedValue::construct(owner.inputs()[0].clone(), vec![bits]).unwrap();
            let actual = stopping_call(&table, owner.id(), &[input]).unwrap();
            assert_eq!(actual[0].elements(), &[bits]);
        }
    }
}

#[test]
fn literal_sign_inversion_preserves_raw_nan_payloads_in_both_formats() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let p = profile(format);
        for kind in cases(format) {
            // The existing typed wire value carries raw Binary32 bits without
            // a float-width conversion that could itself quiet an sNaN.
            let mut wire = serde_json::to_value(SolveValue::real(p, 0.0)).unwrap();
            let (bits, expected) = match kind {
                SolveValueKind::Real32(bits) => {
                    (u64::from(bits), SolveValueKind::Real32(bits ^ (1 << 31)))
                }
                SolveValueKind::Real64(bits) => (bits, SolveValueKind::Real64(bits ^ (1 << 63))),
                _ => unreachable!(),
            };
            wire["kind"]["bits"] = serde_json::json!(bits);
            let literal: SolveValue = serde_json::from_value(wire).unwrap();
            let table = literal_negation_owner(p, literal);
            let owner = &table.owners()[0];
            assert!(
                !owner
                    .body()
                    .operations()
                    .iter()
                    .any(|op| matches!(op.operation(), SolveOperation::Unary { .. }))
            );
            let actual = stopping_call(&table, owner.id(), &[]).unwrap();
            assert_eq!(actual[0].elements(), &[expected]);
        }
    }
}

fn compare_checked_case(
    folded: &SolvePureCallTable,
    kept: &SolvePureCallTable,
    bits: SolveValueKind,
) {
    let a = &folded.owners()[0];
    let b = &kept.owners()[0];
    let input = TypedValue::construct(a.inputs()[0].clone(), vec![bits]).unwrap();
    let actual = stopping_call(folded, a.id(), std::slice::from_ref(&input));
    let expected = stopping_call(kept, b.id(), std::slice::from_ref(&input));
    assert_eq!(actual, expected, "{bits:?}");
    if let Ok(values) = actual {
        assert_eq!(values[0].elements(), &[bits]);
    }
}

fn literal_negation_owner(p: SolveArithmeticProfile, literal: SolveValue) -> SolvePureCallTable {
    let ty = literal.value_type().clone();
    SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(7102),
            vec![],
            vec![SolvePureCallOutput::result(ty.clone())],
            span(7120),
            |b, _, outputs| {
                let value = b.constant(literal, span(7121))?;
                let negative = b.unary(SolveUnaryOperator::Negate, value, span(7122))?;
                b.store(outputs[0], negative, span(7123))
            },
        )?;
        Ok(())
    })
    .unwrap()
}
