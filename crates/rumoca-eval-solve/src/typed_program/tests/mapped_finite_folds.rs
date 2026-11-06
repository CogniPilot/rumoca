//! A mapped finite check admits identity folds only inside its own map body.
//! The check keeps its scalar identity and order, so the earliest failing
//! logical row is reported exactly as the scalarized programs would.
use super::*;
use rumoca_ir_solve::SolveFiniteCheck;

const PROGRAM_BASE: u32 = 40;
const POINTS: u32 = 6;
const OPERATION: u32 = 7;

fn grid() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: [2, 3]
            .into_iter()
            .enumerate()
            .map(|(id, upper)| StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id,
                display_name: format!("k{id}"),
                lower: 1,
                upper,
                step: 1,
            })
            .collect(),
    }
}

/// `1 / (point - x)` at the row-major ordinal `point = (k0 - 1) * 3 + (k1 - 1)`.
fn reciprocal_distance<'region>(
    b: &mut TypedProgramBuilder<'region>,
    arithmetic: SolveArithmeticProfile,
    captures: &[ProgramSlot<'region>],
    binders: &[ProgramSlot<'region>],
) -> Result<(ProgramRegister<'region>, ProgramRegister<'region>), SolveProgramConstructionError> {
    let x = b.load(captures[0], span(7202))?;
    let row = b.load(binders[0], span(7203))?;
    let column = b.load(binders[1], span(7204))?;
    let one = b.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(7201))?;
    let stride = b.constant(SolveValue::integer(arithmetic, 3).unwrap(), span(7201))?;
    let row = b.binary(SolveBinaryOperator::Subtract, row, one, span(7205))?;
    let column = b.binary(SolveBinaryOperator::Subtract, column, one, span(7206))?;
    let row = b.binary(SolveBinaryOperator::Multiply, row, stride, span(7207))?;
    let point = b.binary(SolveBinaryOperator::Add, row, column, span(7208))?;
    let real_point = b.convert(SolveConversionOperator::IntegerToReal, point, span(7209))?;
    let distance = b.binary(SolveBinaryOperator::Subtract, real_point, x, span(7210))?;
    let unit = b.constant(SolveValue::real(arithmetic, 1.0), span(7211))?;
    let value = b.binary(SolveBinaryOperator::Divide, unit, distance, span(7212))?;
    Ok((value, point))
}

fn identities<'region>(
    b: &mut TypedProgramBuilder<'region>,
    arithmetic: SolveArithmeticProfile,
    value: ProgramRegister<'region>,
) -> Result<[ProgramRegister<'region>; 3], SolveProgramConstructionError> {
    let one = b.constant(SolveValue::real(arithmetic, 1.0), span(7220))?;
    let zero = b.constant(SolveValue::real(arithmetic, 0.0), span(7221))?;
    Ok([
        b.binary(SolveBinaryOperator::Multiply, value, one, span(7222))?,
        b.binary(SolveBinaryOperator::Divide, value, one, span(7223))?,
        b.binary(SolveBinaryOperator::Subtract, value, zero, span(7224))?,
    ])
}

fn assert_all_fold<'region>(
    folded: [ProgramRegister<'region>; 3],
    value: ProgramRegister<'region>,
) {
    for register in folded {
        assert_eq!(
            register, value,
            "the same-body mapped check admits the fold"
        );
    }
}

fn assert_none_fold<'region>(kept: [ProgramRegister<'region>; 3], value: ProgramRegister<'region>) {
    for register in kept {
        assert_ne!(
            register, value,
            "no finite fact crosses into a sibling body"
        );
    }
}

/// Outputs: the checked map, an unchecked sibling map over the same capture,
/// and an outer `x * 1`.
fn composed_owner(arithmetic: SolveArithmeticProfile) -> SolvePureCallTable {
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let grid_type = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 3]).unwrap();
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(7200),
            vec![scalar.clone()],
            vec![
                SolvePureCallOutput::result(grid_type.clone()),
                SolvePureCallOutput::result(grid_type.clone()),
                SolvePureCallOutput::result(scalar.clone()),
            ],
            span(7230),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(7231))?;
                let checked = b.map(
                    grid(),
                    &[x],
                    scalar.clone(),
                    span(7232),
                    |b, captures, binders, output| {
                        let (value, point) = reciprocal_distance(b, arithmetic, captures, binders)?;
                        b.check_finite_mapped(
                            value,
                            PROGRAM_BASE,
                            POINTS,
                            OPERATION,
                            point,
                            span(7233),
                        )?;
                        assert_all_fold(identities(b, arithmetic, value)?, value);
                        b.store(output, value, span(7234))
                    },
                )?;
                let sibling = b.map(
                    grid(),
                    &[x],
                    scalar.clone(),
                    span(7235),
                    |b, captures, binders, output| {
                        let (value, _) = reciprocal_distance(b, arithmetic, captures, binders)?;
                        let kept = identities(b, arithmetic, value)?;
                        assert_none_fold(kept, value);
                        b.store(output, kept[0], span(7236))
                    },
                )?;
                let one = b.constant(SolveValue::real(arithmetic, 1.0), span(7237))?;
                let outer = b.binary(SolveBinaryOperator::Multiply, x, one, span(7238))?;
                assert_ne!(outer, x, "no finite fact escapes a map body");
                b.store(outputs[0], checked, span(7239))?;
                b.store(outputs[1], sibling, span(7240))?;
                b.store(outputs[2], outer, span(7241))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn first_fault(table: &SolvePureCallTable, x: SolveValueKind) -> Option<(SolveFiniteCheck, usize)> {
    let owner = &table.owners()[0];
    let input = TypedValue::construct(owner.inputs()[0].clone(), vec![x]).unwrap();
    match stopping_call(table, owner.id(), &[input]) {
        Ok(_) => None,
        Err(TypedProgramEvalError::NonFinite { check, element, .. }) => Some((check, element)),
        Err(other) => panic!("expected only a finite-check fault, got {other:?}"),
    }
}

fn assert_finite_run(table: &SolvePureCallTable, x: f64) {
    let real = |value| real_kind(SolveRealFormat::Binary64, value);
    let owner = &table.owners()[0];
    let input = TypedValue::construct(owner.inputs()[0].clone(), vec![real(x)]).unwrap();
    let outputs = stopping_call(table, owner.id(), &[input]).unwrap();
    let expected = (0..POINTS)
        .map(|point| real(1.0 / (f64::from(point) - x)))
        .collect::<Vec<_>>();
    assert_eq!(outputs[0].elements(), expected.as_slice());
    assert_eq!(outputs[1].elements(), expected.as_slice());
    assert_eq!(outputs[2].elements(), &[real(x)]);
}

#[test]
fn mapped_check_folds_its_own_body_and_reports_the_earliest_logical_row() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let table = composed_owner(arithmetic);
    let primal = |point| SolveFiniteCheck::Primal {
        program: PROGRAM_BASE + point,
        operation: OPERATION,
    };

    assert_finite_run(&table, 2.5);
    // Only point 4 divides by zero: the fault names that row, not row 0.
    assert_eq!(
        first_fault(&table, real_kind(SolveRealFormat::Binary64, 4.0)),
        Some((primal(4), 0))
    );
    // A quiet NaN with a payload fails every row; the first logical row wins.
    assert_eq!(
        first_fault(&table, SolveValueKind::Real64(0x7ff8_0000_0000_0123)),
        Some((primal(0), 0))
    );
    // A later finite evaluation of the same table is unaffected.
    assert_finite_run(&table, 2.5);
    assert_finite_run(&table, -1.0);
}
