//! Proof domains and retained IEEE operations of constructor-time arithmetic.
use super::*;

fn real_profile(format: SolveRealFormat) -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(format, crate::SolveIntegerDomain::FULL)
}

#[test]
fn finite_identity_folds_require_an_earlier_check_and_keep_its_identity() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let arithmetic = real_profile(format);
        let program = TypedProgram::construct(arithmetic, |b| {
            let input = b.declare_slot(
                SolveValueType::scalar(SolveScalarType::real(arithmetic)),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(0),
            )?;
            let x = b.load(input, span(1))?;
            let one = b.constant(SolveValue::real(arithmetic, 1.0), span(2))?;
            let zero = b.constant(SolveValue::real(arithmetic, 0.0), span(3))?;
            for (op, left, right) in [
                (SolveBinaryOperator::Multiply, x, one),
                (SolveBinaryOperator::Multiply, one, x),
                (SolveBinaryOperator::Divide, x, one),
                (SolveBinaryOperator::Subtract, x, zero),
            ] {
                assert_ne!(
                    b.binary(op, left, right, span(4))?,
                    x,
                    "unknown raw IEEE values must retain arithmetic NaN quieting"
                );
            }
            b.check_finite(x, SolveFiniteCheck::Input { coordinate: 8 }, span(5))?;
            for (op, left, right) in [
                (SolveBinaryOperator::Multiply, x, one),
                (SolveBinaryOperator::Multiply, one, x),
                (SolveBinaryOperator::Divide, x, one),
                (SolveBinaryOperator::Subtract, x, zero),
            ] {
                assert_eq!(b.binary(op, left, right, span(6))?, x);
            }
            // Signed-zero addition/subtraction and numeric zero multiplication
            // are not SC identities, even after finite admission.
            let negative_zero = b.constant(SolveValue::real(arithmetic, -0.0), span(7))?;
            for (op, right) in [
                (SolveBinaryOperator::Add, zero),
                (SolveBinaryOperator::Subtract, negative_zero),
                (SolveBinaryOperator::Multiply, zero),
            ] {
                assert_ne!(b.binary(op, x, right, span(8))?, x);
            }
            Ok(())
        })
        .unwrap();
        let checks = program
            .operations()
            .iter()
            .filter_map(|op| match op.operation() {
                SolveOperation::CheckFinite { check, .. } => Some((check, op.provenance())),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            checks,
            vec![(&SolveFiniteCheck::Input { coordinate: 8 }, span(5))]
        );
        let encoded = serde_json::to_value(&program).unwrap();
        let replay: TypedProgram = serde_json::from_value(encoded).unwrap();
        assert_eq!(program, replay);
    }
}

#[test]
fn literal_folds_round_in_the_issued_real_format_and_retain_domain_results() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let arithmetic = real_profile(format);
        TypedProgram::construct(arithmetic, |b| {
            let two = b.constant(SolveValue::real(arithmetic, 2.0), span(10))?;
            let three = b.constant(SolveValue::real(arithmetic, 3.0), span(11))?;
            let six = b.constant(SolveValue::real(arithmetic, 6.0), span(12))?;
            assert_eq!(
                b.binary(SolveBinaryOperator::Multiply, two, three, span(13))?,
                six
            );
            let zero = b.constant(SolveValue::real(arithmetic, 0.0), span(14))?;
            let negative = b.constant(SolveValue::real(arithmetic, -0.0), span(15))?;
            assert_eq!(
                b.unary(SolveUnaryOperator::Negate, zero, span(16))?,
                negative
            );
            assert_eq!(
                b.binary(SolveBinaryOperator::Add, negative, zero, span(17))?,
                zero
            );
            for op in [SolveBinaryOperator::Divide, SolveBinaryOperator::Power] {
                let result = b.binary(op, zero, zero, span(18))?;
                assert!(matches!(b.operations.last().unwrap().operation(),
                    SolveOperation::Binary { destination, .. } if *destination == result.id));
            }
            let big = b.constant(SolveValue::real(arithmetic, 16_777_216.0), span(19))?;
            let one = b.constant(SolveValue::real(arithmetic, 1.0), span(20))?;
            let expected = b.constant(
                SolveValue::real(
                    arithmetic,
                    if format == SolveRealFormat::Binary32 {
                        16_777_216.0
                    } else {
                        16_777_217.0
                    },
                ),
                span(21),
            )?;
            assert_eq!(
                b.binary(SolveBinaryOperator::Add, big, one, span(22))?,
                expected
            );
            Ok(())
        })
        .unwrap();
    }
}

#[test]
fn double_negation_is_bitwise_but_integer_negation_keeps_its_fault() {
    let arithmetic = real_profile(SolveRealFormat::Binary64);
    TypedProgram::construct(arithmetic, |b| {
        for scalar in [
            SolveScalarType::real(arithmetic),
            SolveScalarType::integer(arithmetic),
        ] {
            let input = b.declare_slot(
                SolveValueType::scalar(scalar),
                SolveStorageClass::Input,
                SolveSlotAccess::ReadOnly,
                span(30),
            )?;
            let x = b.load(input, span(31))?;
            let negative = b.unary(SolveUnaryOperator::Negate, x, span(32))?;
            let positive = b.unary(SolveUnaryOperator::Negate, negative, span(33))?;
            if matches!(scalar, SolveScalarType::Real { .. }) {
                assert_eq!(positive, x);
            } else {
                assert_ne!(positive, x, "minimum Integer negation must still fault");
            }
        }
        Ok(())
    })
    .unwrap();
}

#[test]
fn aggregate_identity_is_compact_and_requires_whole_value_finite_evidence() {
    let arithmetic = real_profile(SolveRealFormat::Binary64);
    let vector =
        SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![1_000_000]).unwrap();
    let program = TypedProgram::construct(arithmetic, |b| {
        let input = b.declare_slot(
            vector,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(40),
        )?;
        let x = b.load(input, span(41))?;
        let one = b.constant(SolveValue::real(arithmetic, 1.0), span(42))?;
        assert_ne!(b.scale(x, one, span(43))?, x);
        b.check_finite(x, SolveFiniteCheck::Derivative, span(44))?;
        assert_eq!(b.scale(x, one, span(45))?, x);
        assert_eq!(
            b.broadcast_binary(SolveBinaryOperator::Divide, x, one, false, span(46))?,
            x
        );
        assert_ne!(
            b.broadcast_binary(SolveBinaryOperator::Divide, x, one, true, span(47))?,
            x
        );
        Ok(())
    })
    .unwrap();
    assert_eq!(
        program.operations().len(),
        5,
        "metadata must not grow with tensor extent"
    );
}

#[test]
fn wire_replay_does_not_apply_new_literal_or_identity_folds() {
    let arithmetic = real_profile(SolveRealFormat::Binary64);
    let program = TypedProgram::construct(arithmetic, |b| {
        b.numbered_values = false;
        let one = b.constant(SolveValue::real(arithmetic, 1.0), span(50))?;
        b.binary(SolveBinaryOperator::Multiply, one, one, span(51))?;
        Ok(())
    })
    .unwrap();
    assert_eq!(program.operations().len(), 2);
    let replay: TypedProgram =
        serde_json::from_value(serde_json::to_value(&program).unwrap()).unwrap();
    assert_eq!(program, replay);
}

#[test]
fn an_outer_finite_check_does_not_license_folds_in_a_fresh_region() {
    let arithmetic = real_profile(SolveRealFormat::Binary64);
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    TypedProgram::construct(arithmetic, |b| {
        let slot = b.declare_slot(
            scalar.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(60),
        )?;
        let x = b.load(slot, span(61))?;
        b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, span(62))?;
        let condition = b.constant(SolveValue::boolean(true), span(63))?;
        b.conditional(
            condition,
            &[x],
            vec![scalar],
            span(64),
            |region, inputs, outputs| {
                let x = region.load(inputs[0], span(65))?;
                let one = region.constant(SolveValue::real(arithmetic, 1.0), span(66))?;
                let product = region.binary(SolveBinaryOperator::Multiply, x, one, span(67))?;
                assert_ne!(product, x, "fresh captures carry no implicit finite fact");
                region.store(outputs[0], product, span(68))
            },
            |region, inputs, outputs| {
                let x = region.load(inputs[0], span(69))?;
                region.store(outputs[0], x, span(70))
            },
        )?;
        Ok(())
    })
    .unwrap();
}

#[test]
fn a_child_finite_check_cannot_license_outer_or_sibling_arithmetic() {
    let arithmetic = real_profile(SolveRealFormat::Binary64);
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    TypedProgram::construct(arithmetic, |b| {
        let slot = b.declare_slot(
            scalar.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(71),
        )?;
        let x = b.load(slot, span(72))?;
        let condition = b.constant(SolveValue::boolean(true), span(73))?;
        b.conditional(
            condition,
            &[x],
            vec![scalar],
            span(74),
            |region, inputs, outputs| {
                let x = region.load(inputs[0], span(75))?;
                region.check_finite(x, SolveFiniteCheck::Derivative, span(76))?;
                let one = region.constant(SolveValue::real(arithmetic, 1.0), span(77))?;
                assert_eq!(
                    region.binary(SolveBinaryOperator::Multiply, x, one, span(78))?,
                    x
                );
                region.store(outputs[0], x, span(79))
            },
            |region, inputs, outputs| {
                let x = region.load(inputs[0], span(80))?;
                let one = region.constant(SolveValue::real(arithmetic, 1.0), span(81))?;
                let product = region.binary(SolveBinaryOperator::Multiply, x, one, span(82))?;
                assert_ne!(product, x, "the sibling check does not dominate this arm");
                region.store(outputs[0], product, span(83))
            },
        )?;
        let one = b.constant(SolveValue::real(arithmetic, 1.0), span(84))?;
        assert_ne!(
            b.binary(SolveBinaryOperator::Multiply, x, one, span(85))?,
            x
        );
        Ok(())
    })
    .unwrap();
}

#[test]
fn a_checked_projection_does_not_admit_the_whole_aggregate_as_finite() {
    let arithmetic = real_profile(SolveRealFormat::Binary64);
    let tensor = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2]).unwrap();
    TypedProgram::construct(arithmetic, |b| {
        let slot = b.declare_slot(
            tensor,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(86),
        )?;
        let aggregate = b.load(slot, span(87))?;
        let point = b.project_element(aggregate, vec![0], span(88))?;
        b.check_finite(point, SolveFiniteCheck::Derivative, span(89))?;
        let one = b.constant(SolveValue::real(arithmetic, 1.0), span(90))?;
        assert_eq!(
            b.binary(SolveBinaryOperator::Multiply, point, one, span(91))?,
            point
        );
        assert_ne!(b.scale(aggregate, one, span(92))?, aggregate);
        Ok(())
    })
    .unwrap();
}
