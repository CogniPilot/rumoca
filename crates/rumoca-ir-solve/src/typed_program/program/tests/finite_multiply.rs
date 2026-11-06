//! Finite scalar Multiply key sharing; no algebraic reassociation.
use super::*;

fn input<'program>(
    b: &mut TypedProgramBuilder<'program>,
    value_type: SolveValueType,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let slot = b.declare_slot(
        value_type,
        SolveStorageClass::Input,
        SolveSlotAccess::ReadOnly,
        span(0),
    )?;
    b.load(slot, span(1))
}

#[test]
fn finite_scalar_multiply_shares_commuted_key_but_keeps_first_operation() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let profile = SolveArithmeticProfile::construct(format, crate::SolveIntegerDomain::FULL);
        let program = TypedProgram::construct(profile, |b| {
            let ty = SolveValueType::scalar(SolveScalarType::real(profile));
            let x = input(b, ty.clone())?;
            let y = input(b, ty)?;
            b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, span(2))?;
            b.check_finite(y, SolveFiniteCheck::Input { coordinate: 1 }, span(3))?;
            let first = b.binary(SolveBinaryOperator::Multiply, y, x, span(4))?;
            let before = b.operations.len();
            assert_eq!(
                b.binary(SolveBinaryOperator::Multiply, x, y, span(5))?,
                first
            );
            assert_eq!(b.operations.len(), before);
            assert!(matches!(b.operations.last().unwrap().operation(),
                SolveOperation::Binary { lhs, rhs, .. } if *lhs == y.id && *rhs == x.id));
            assert_eq!(b.operations.last().unwrap().provenance(), span(4));
            b.check_finite(first, SolveFiniteCheck::Derivative, span(6))
        })
        .unwrap();
        let checks = program
            .operations()
            .iter()
            .filter_map(|op| match op.operation() {
                SolveOperation::CheckFinite { check, .. } => Some((*check, op.provenance())),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            checks,
            vec![
                (SolveFiniteCheck::Input { coordinate: 0 }, span(2)),
                (SolveFiniteCheck::Input { coordinate: 1 }, span(3)),
                (SolveFiniteCheck::Derivative, span(6)),
            ]
        );
    }
}

#[test]
fn unknown_or_one_sided_finite_operands_keep_ordered_keys() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let ty = SolveValueType::scalar(SolveScalarType::real(profile));
        let x = input(b, ty.clone())?;
        let y = input(b, ty)?;
        let xy = b.binary(SolveBinaryOperator::Multiply, x, y, span(2))?;
        let yx = b.binary(SolveBinaryOperator::Multiply, y, x, span(3))?;
        assert_ne!(xy, yx, "unknown NaN operand order is retained");
        b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, span(4))?;
        assert_eq!(b.binary(SolveBinaryOperator::Multiply, x, y, span(5))?, xy);
        assert_eq!(b.binary(SolveBinaryOperator::Multiply, y, x, span(6))?, yx);
        assert_ne!(
            b.binary(SolveBinaryOperator::Subtract, x, y, span(7))?,
            b.binary(SolveBinaryOperator::Subtract, y, x, span(8))?
        );
        Ok(())
    })
    .unwrap();
}

#[test]
fn whole_finite_tensors_stay_distinct_and_wire_preserves_scalar_order() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let tensor = SolveValueType::tensor(SolveScalarType::real(profile), vec![2]).unwrap();
        let x = input(b, tensor.clone())?;
        let y = input(b, tensor)?;
        b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, span(2))?;
        b.check_finite(y, SolveFiniteCheck::Input { coordinate: 2 }, span(3))?;
        assert_ne!(
            b.binary(SolveBinaryOperator::Multiply, x, y, span(4))?,
            b.binary(SolveBinaryOperator::Multiply, y, x, span(5))?
        );
        b.numbered_values = false;
        let scalar = SolveValueType::scalar(SolveScalarType::real(profile));
        let x = input(b, scalar.clone())?;
        let y = input(b, scalar)?;
        b.check_finite(x, SolveFiniteCheck::Input { coordinate: 4 }, span(6))?;
        b.check_finite(y, SolveFiniteCheck::Input { coordinate: 5 }, span(7))?;
        assert_ne!(
            b.binary(SolveBinaryOperator::Multiply, x, y, span(8))?,
            b.binary(SolveBinaryOperator::Multiply, y, x, span(9))?
        );
        Ok(())
    })
    .unwrap();
    let replay: TypedProgram =
        serde_json::from_value(serde_json::to_value(&program).unwrap()).unwrap();
    assert_eq!(replay, program);
}

#[test]
fn finite_seed_one_tangent_uses_the_existing_common_alias_rule() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let scalar = SolveValueType::scalar(SolveScalarType::real(profile));
        let x = input(b, scalar.clone())?;
        let p = input(b, scalar)?;
        b.check_finite(p, SolveFiniteCheck::Input { coordinate: 1 }, span(2))?;
        // The checked model-J constructor seeds both constants before directions.
        let zero = b.constant(SolveValue::real(profile, 0.0), span(3))?;
        let one = b.constant(SolveValue::real(profile, 1.0), span(3))?;
        let product =
            b.scalar_binary_directional_source(SolveBinaryOperator::Multiply, [x, p], span(4))?;
        let before = b.operations.len();
        assert_eq!(
            b.scalar_binary_structural_tangent(product, [Some(one), None])?,
            Some(p)
        );
        assert_eq!(b.operations.len(), before);
        assert_eq!(b.constant(SolveValue::real(profile, 0.0), span(5))?, zero);
        Ok(())
    })
    .unwrap();
}

fn unguarded_arm<'region>(
    b: &mut TypedProgramBuilder<'region>,
    inputs: &[ProgramSlot<'region>],
    outputs: &[ProgramSlot<'region>],
) -> Result<(), SolveProgramConstructionError> {
    let x = b.load(inputs[0], span(10))?;
    let y = b.load(inputs[1], span(11))?;
    let xy = b.binary(SolveBinaryOperator::Multiply, x, y, span(12))?;
    let yx = b.binary(SolveBinaryOperator::Multiply, y, x, span(13))?;
    assert_ne!(
        xy, yx,
        "outer and sibling finite facts cannot enter a region"
    );
    b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, span(14))?;
    b.check_finite(y, SolveFiniteCheck::Input { coordinate: 1 }, span(15))?;
    b.store(outputs[0], xy, span(16))
}

#[test]
fn parent_and_sibling_regions_cannot_license_commutation() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let scalar = SolveValueType::scalar(SolveScalarType::real(profile));
        let x = input(b, scalar.clone())?;
        let y = input(b, scalar.clone())?;
        b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, span(2))?;
        b.check_finite(y, SolveFiniteCheck::Input { coordinate: 1 }, span(3))?;
        let condition = b.constant(SolveValue::boolean(true), span(4))?;
        b.conditional(
            condition,
            &[x, y],
            vec![scalar],
            span(5),
            unguarded_arm,
            unguarded_arm,
        )?;
        Ok(())
    })
    .unwrap();
}

#[test]
fn integer_products_and_finite_real_sums_keep_ordered_keys() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let integer = SolveValueType::scalar(SolveScalarType::integer(profile));
        let i = input(b, integer.clone())?;
        let j = input(b, integer)?;
        assert_ne!(
            b.binary(SolveBinaryOperator::Multiply, i, j, span(2))?,
            b.binary(SolveBinaryOperator::Multiply, j, i, span(3))?
        );
        let scalar = SolveValueType::scalar(SolveScalarType::real(profile));
        let x = input(b, scalar.clone())?;
        let y = input(b, scalar)?;
        b.check_finite(x, SolveFiniteCheck::Input { coordinate: 0 }, span(4))?;
        b.check_finite(y, SolveFiniteCheck::Input { coordinate: 1 }, span(5))?;
        assert_ne!(
            b.binary(SolveBinaryOperator::Add, x, y, span(6))?,
            b.binary(SolveBinaryOperator::Add, y, x, span(7))?
        );
        Ok(())
    })
    .unwrap();
}
