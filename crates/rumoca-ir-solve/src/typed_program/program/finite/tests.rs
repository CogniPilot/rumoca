use super::*;
use crate::{FaultClass, FaultKind, SolveLiveness, SolveOutputDemand};
use rumoca_core::{SourceId, StructuredIndexBinder, StructuredIndexDomain};

fn at() -> Span {
    Span::from_offsets(SourceId::from_source_name("FiniteGuard.mo"), 1, 2)
}

#[test]
fn compact_finite_guard_survives_unused_output_demand_and_wire_replay() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let scalar = b.constant(SolveValue::real(profile, 1.0), at())?;
        let tensor = b.fill(scalar, vec![1_000_000], at())?;
        b.check_finite(
            tensor,
            SolveFiniteCheck::Primal {
                program: 2,
                operation: 3,
            },
            at(),
        )
    })
    .unwrap();
    assert_eq!(program.operations().len(), 3);
    assert_eq!(
        program.region_fault_class(),
        FaultClass::MayFault(FaultKind::RealDomain)
    );
    let live = SolveLiveness::of(&program, &SolveOutputDemand::all());
    assert!(!live.is_dead(2));
    let wire = serde_json::to_value(&program).unwrap();
    let replay: TypedProgram = serde_json::from_value(wire).unwrap();
    assert_eq!(replay, program);
    assert_eq!(
        replay.fault_class(2),
        FaultClass::MayFault(FaultKind::RealDomain)
    );
}

#[test]
fn finite_guard_refuses_boolean_without_emitting_a_check() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let value = b.constant(SolveValue::boolean(true), at())?;
        let before = b.operations.len();
        assert_eq!(
            b.check_finite(value, SolveFiniteCheck::Derivative, at()),
            Err(SolveProgramConstructionError::TypeMismatch { provenance: at() })
        );
        assert_eq!(b.operations.len(), before);
        Ok(())
    })
    .unwrap();
}

fn ordinal_domain(extents: &[i64]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: extents
            .iter()
            .enumerate()
            .map(|(id, upper)| StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id,
                display_name: format!("k{id}"),
                lower: 1,
                upper: *upper,
                step: 1,
            })
            .collect(),
    }
}

fn integer<'program>(
    b: &mut TypedProgramBuilder<'program>,
    profile: SolveArithmeticProfile,
    value: i64,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    b.constant(SolveValue::integer(profile, value).unwrap(), at())
}

/// A 2x3 map whose body checks `value` with the row-major point ordinal
/// `(k0 - 1) * 3 + (k1 - 1) + shift`.
fn mapped_check_program(
    profile: SolveArithmeticProfile,
    point_count: u32,
    shift: i64,
) -> Result<TypedProgram, SolveProgramConstructionError> {
    TypedProgram::construct(profile, |b| {
        let value = b.constant(SolveValue::real(profile, 1.0), at())?;
        b.map(
            ordinal_domain(&[2, 3]),
            &[value],
            SolveValueType::scalar(SolveScalarType::real(profile)),
            at(),
            |b, captures, binders, output| {
                let value = b.load(captures[0], at())?;
                let row = b.load(binders[0], at())?;
                let column = b.load(binders[1], at())?;
                let one = integer(b, profile, 1)?;
                let stride = integer(b, profile, 3)?;
                let shift = integer(b, profile, shift)?;
                let row = b.binary(SolveBinaryOperator::Subtract, row, one, at())?;
                let column = b.binary(SolveBinaryOperator::Subtract, column, one, at())?;
                let row = b.binary(SolveBinaryOperator::Multiply, row, stride, at())?;
                let point = b.binary(SolveBinaryOperator::Add, row, column, at())?;
                let point = b.binary(SolveBinaryOperator::Add, point, shift, at())?;
                b.check_finite_mapped(value, 40, point_count, 7, point, at())?;
                b.store(output, value, at())
            },
        )?;
        Ok(())
    })
}

#[test]
fn mapped_check_reports_base_plus_point_and_survives_wire_replay() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let program = mapped_check_program(profile, 6, 0).unwrap();
    let wire = serde_json::to_value(&program).unwrap();
    let replay: TypedProgram = serde_json::from_value(wire).unwrap();
    assert_eq!(replay, program);
    let check = SolveFiniteCheck::MappedPrimal {
        program_base: 40,
        point_count: 6,
        operation: 7,
    };
    assert_eq!(
        check.reported(Some(5)),
        Some(SolveFiniteCheck::Primal {
            program: 45,
            operation: 7,
        })
    );
    assert_eq!(check.reported(Some(6)), None);
    assert_eq!(check.reported(None), None);
    assert_eq!(
        SolveFiniteCheck::Derivative.reported(Some(0)),
        None,
        "an unmapped identity never takes a point"
    );
}

#[test]
fn mapped_check_refuses_points_the_map_does_not_bound() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    // Point range 1..=6 overruns a six-point identity, and 0..=5 overruns five.
    for (point_count, shift) in [(6, 1), (5, 0), (6, -1)] {
        assert_eq!(
            mapped_check_program(profile, point_count, shift),
            Err(SolveProgramConstructionError::InvalidMap { provenance: at() }),
            "count {point_count}, shift {shift}"
        );
    }
}

#[test]
fn mapped_check_is_refused_outside_a_map_body() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let value = b.constant(SolveValue::real(profile, 1.0), at())?;
        let point = b.constant(SolveValue::integer(profile, 0).unwrap(), at())?;
        let before = b.operations.len();
        assert_eq!(
            b.check_finite_mapped(value, 0, 1, 0, point, at()),
            Err(SolveProgramConstructionError::TypeMismatch { provenance: at() })
        );
        let mapped = SolveFiniteCheck::MappedPrimal {
            program_base: 0,
            point_count: 1,
            operation: 0,
        };
        assert_eq!(
            b.check_finite(value, mapped, at()),
            Err(SolveProgramConstructionError::TypeMismatch { provenance: at() })
        );
        assert_eq!(b.operations.len(), before);
        Ok(())
    })
    .unwrap();
}

#[test]
fn mapped_check_below_a_nested_map_refuses_the_enclosing_map() {
    // The inner map proves its own point, but the enclosing map's body then
    // holds a mapped check in a nested region, whose identity the enclosing
    // map's points do not determine: the enclosing map refuses.
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let real = SolveValueType::scalar(SolveScalarType::real(profile));
    let result = TypedProgram::construct(profile, |b| {
        let value = b.constant(SolveValue::real(profile, 1.0), at())?;
        b.map(
            ordinal_domain(&[2]),
            &[value],
            SolveValueType::tensor(SolveScalarType::real(profile), vec![3]).unwrap(),
            at(),
            |b, captures, _, output| {
                let value = b.load(captures[0], at())?;
                let inner = b.map(
                    ordinal_domain(&[3]),
                    &[value],
                    real.clone(),
                    at(),
                    |b, captures, binders, output| {
                        let value = b.load(captures[0], at())?;
                        let k = b.load(binders[0], at())?;
                        let one = integer(b, profile, 1)?;
                        let point = b.binary(SolveBinaryOperator::Subtract, k, one, at())?;
                        b.check_finite_mapped(value, 0, 3, 0, point, at())?;
                        b.store(output, value, at())
                    },
                )?;
                b.store(output, inner, at())
            },
        )?;
        Ok(())
    });
    assert_eq!(
        result,
        Err(SolveProgramConstructionError::InvalidMap { provenance: at() })
    );
}

fn issued_checks(program: &TypedProgram) -> Vec<SolveFiniteCheck> {
    program
        .operations()
        .iter()
        .filter_map(|op| match op.operation() {
            SolveOperation::CheckFinite { check, .. } => Some(*check),
            _ => None,
        })
        .collect()
}

fn loaded_real<'program>(
    b: &mut TypedProgramBuilder<'program>,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let input = b.declare_slot(
        SolveValueType::scalar(SolveScalarType::real(b.arithmetic)),
        SolveStorageClass::Input,
        SolveSlotAccess::ReadOnly,
        at(),
    )?;
    b.load(input, at())
}

#[test]
fn primal_dedup_preserves_input_publication_and_arithmetic_facts() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let value = loaded_real(b)?;
        b.check_finite(value, SolveFiniteCheck::Input { coordinate: 3 }, at())?;
        b.check_finite(
            value,
            SolveFiniteCheck::Primal {
                program: 2,
                operation: 7,
            },
            at(),
        )?;
        let one = b.constant(SolveValue::real(profile, 1.0), at())?;
        assert_eq!(
            b.binary(SolveBinaryOperator::Multiply, value, one, at())?,
            value
        );
        b.check_finite(value, SolveFiniteCheck::Derivative, at())
    })
    .unwrap();
    assert_eq!(
        issued_checks(&program),
        [
            SolveFiniteCheck::Input { coordinate: 3 },
            SolveFiniteCheck::Derivative,
        ]
    );
}

#[test]
fn first_unknown_primal_check_keeps_its_exact_identity() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let first = SolveFiniteCheck::Primal {
        program: 4,
        operation: 9,
    };
    let program = TypedProgram::construct(profile, |b| {
        let value = loaded_real(b)?;
        b.check_finite(value, first, at())?;
        b.check_finite(
            value,
            SolveFiniteCheck::Primal {
                program: 5,
                operation: 0,
            },
            at(),
        )
    })
    .unwrap();
    assert_eq!(issued_checks(&program), [first]);
}

#[test]
fn finite_literal_primal_checks_are_omitted_but_nonfinite_checks_remain() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let check = SolveFiniteCheck::Primal {
        program: 0,
        operation: 0,
    };
    let program = TypedProgram::construct(profile, |b| {
        for value in [0.0, -0.0, 1.0] {
            let value = b.constant(SolveValue::real(profile, value), at())?;
            b.check_finite(value, check, at())?;
        }
        for value in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
            let value = b.constant(SolveValue::real(profile, value), at())?;
            b.check_finite(value, check, at())?;
        }
        Ok(())
    })
    .unwrap();
    assert_eq!(issued_checks(&program), [check; 3]);
}

#[test]
fn primal_dedup_is_disabled_during_recorded_wire_replay() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let check = SolveFiniteCheck::Primal {
        program: 0,
        operation: 0,
    };
    let program = TypedProgram::construct(profile, |b| {
        b.numbered_values = false;
        let value = loaded_real(b)?;
        b.check_finite(value, check, at())?;
        b.check_finite(value, check, at())
    })
    .unwrap();
    let replay: TypedProgram =
        serde_json::from_value(serde_json::to_value(&program).unwrap()).unwrap();
    assert_eq!(replay, program);
    assert_eq!(issued_checks(&replay), [check; 2]);
}

#[test]
fn known_finite_primal_check_still_requires_provenance() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    TypedProgram::construct(profile, |b| {
        let value = b.constant(SolveValue::real(profile, 1.0), at())?;
        let before = b.operations.len();
        let check = SolveFiniteCheck::Primal {
            program: 0,
            operation: 0,
        };
        assert_eq!(
            b.check_finite(value, check, Span::DUMMY),
            Err(SolveProgramConstructionError::MissingProvenance)
        );
        assert_eq!(b.operations.len(), before);
        Ok(())
    })
    .unwrap();
}

fn primal_checked_arm<'region>(
    b: &mut TypedProgramBuilder<'region>,
    inputs: &[ProgramSlot<'region>],
    outputs: &[ProgramSlot<'region>],
) -> Result<(), SolveProgramConstructionError> {
    let value = b.load(inputs[0], at())?;
    b.check_finite(
        value,
        SolveFiniteCheck::Primal {
            program: 1,
            operation: 0,
        },
        at(),
    )?;
    b.store(outputs[0], value, at())
}

#[test]
fn outer_and_sibling_regions_do_not_supply_primal_finite_facts() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let program = TypedProgram::construct(profile, |b| {
        let value = loaded_real(b)?;
        b.check_finite(value, SolveFiniteCheck::Input { coordinate: 0 }, at())?;
        let condition = b.constant(SolveValue::boolean(true), at())?;
        let result = SolveValueType::scalar(SolveScalarType::real(profile));
        b.conditional(
            condition,
            &[value],
            vec![result],
            at(),
            primal_checked_arm,
            primal_checked_arm,
        )?;
        Ok(())
    })
    .unwrap();
    for op in program.operations() {
        if let SolveOperation::Conditional {
            if_true, if_false, ..
        } = op.operation()
        {
            assert_eq!(issued_checks(if_true.body()).len(), 1);
            assert_eq!(issued_checks(if_false.body()).len(), 1);
        }
    }
}

#[test]
fn later_mapped_check_retains_its_own_point_bound_validation() {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let result = TypedProgram::construct(profile, |b| {
        let value = loaded_real(b)?;
        b.map(
            ordinal_domain(&[2]),
            &[value],
            SolveValueType::scalar(SolveScalarType::real(profile)),
            at(),
            |b, captures, binders, output| {
                let value = b.load(captures[0], at())?;
                let k = b.load(binders[0], at())?;
                let one = integer(b, profile, 1)?;
                let point = b.binary(SolveBinaryOperator::Subtract, k, one, at())?;
                b.check_finite_mapped(value, 40, 2, 7, point, at())?;
                // Same operand and point, but 0..=1 does not fit point_count=1.
                b.check_finite_mapped(value, 40, 1, 8, point, at())?;
                b.store(output, value, at())
            },
        )?;
        Ok(())
    });
    assert_eq!(
        result,
        Err(SolveProgramConstructionError::InvalidMap { provenance: at() })
    );
}
