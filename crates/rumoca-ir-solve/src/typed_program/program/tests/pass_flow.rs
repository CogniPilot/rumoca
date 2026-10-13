use super::*;
use crate::{IndependentPasses, LaneExactness, SolveIndexForm, index_forms};

fn real(arithmetic: SolveArithmeticProfile) -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(arithmetic))
}

fn domain(binders: &[(&str, i64, i64)]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: binders
            .iter()
            .enumerate()
            .map(|(id, (name, lower, upper))| StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id,
                display_name: (*name).to_string(),
                lower: *lower,
                upper: *upper,
                step: 1,
            })
            .collect(),
    }
}

fn fold_index(program: &TypedProgram) -> usize {
    program
        .operations()
        .iter()
        .position(|operation| matches!(operation.operation(), SolveOperation::Fold { .. }))
        .unwrap()
}

fn passes_of_fold(program: &TypedProgram) -> Option<IndependentPasses> {
    program.independent_passes(fold_index(program))
}

/// A fold over `binders` carrying a real array of `dimensions`, whose pass
/// body is `body(region, array, source, binders)` returning the array stored
/// back. `source` is a captured real array of the same extent.
fn array_fold(
    binders: &[(&str, i64, i64)],
    dimensions: Vec<u32>,
    body: impl for<'r> FnOnce(
        &mut TypedProgramBuilder<'r>,
        ProgramRegister<'r>,
        ProgramRegister<'r>,
        &[ProgramRegister<'r>],
    ) -> Result<ProgramRegister<'r>, SolveProgramConstructionError>,
) -> TypedProgram {
    let arithmetic = profile();
    let array =
        SolveValueType::tensor(SolveScalarType::real(arithmetic), dimensions.clone()).unwrap();
    TypedProgram::construct(arithmetic, |builder| {
        let source_slot = builder.declare_slot(
            array.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let source = builder.load(source_slot, span(1))?;
        let zero = builder.constant(SolveValue::real(arithmetic, 0.0), span(2))?;
        let initial = builder.fill(zero, dimensions.clone(), span(3))?;
        builder.fold(
            domain(binders),
            &[initial],
            &[source],
            span(4),
            |region, carried, captures, binder_slots, outputs| {
                let array = region.load(carried[0], span(5))?;
                let source = region.load(captures[0], span(6))?;
                let binders = binder_slots
                    .iter()
                    .map(|slot| region.load(*slot, span(7)))
                    .collect::<Result<Vec<_>, _>>()?;
                let next = body(region, array, source, &binders)?;
                region.store(outputs[0], next, span(8))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_dense_image_pass_over_disjoint_output_slices_is_independent_and_lane_exact() {
    // gray[row, column] := (source[row, column] + source[row, column]) / 3
    let arithmetic = profile();
    let program = array_fold(
        &[("row", 1, 4), ("column", 1, 8)],
        vec![4, 8],
        |region, gray, source, at| {
            let pixel = region.project_element_dynamic(source, at, span(11))?;
            let sum = region.binary(SolveBinaryOperator::Add, pixel, pixel, span(12))?;
            let three = region.constant(SolveValue::real(arithmetic, 3.0), span(13))?;
            let mean = region.binary(SolveBinaryOperator::Divide, sum, three, span(14))?;
            region.update_element(gray, mean, at, span(15))
        },
    );
    let passes = passes_of_fold(&program).expect("disjoint binder-indexed writes are independent");
    assert_eq!(passes.exactness(), LaneExactness::Exact);
    assert!(passes.admits_lanes());
}

#[test]
fn a_loop_carrying_a_running_sum_is_not_independent() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let zero = builder.constant(SolveValue::real(arithmetic, 0.0), span(0))?;
        builder.fold(
            domain(&[("i", 1, 4)]),
            &[zero],
            &[],
            span(1),
            |region, carried, _, binders, outputs| {
                let sum = region.load(carried[0], span(2))?;
                let index = region.load(binders[0], span(3))?;
                let step =
                    region.convert(SolveConversionOperator::IntegerToReal, index, span(4))?;
                let next = region.binary(SolveBinaryOperator::Add, sum, step, span(5))?;
                region.store(outputs[0], next, span(6))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(passes_of_fold(&program), None);
}

#[test]
fn a_loop_reading_the_element_before_the_one_it_writes_is_not_independent() {
    // y[i] := y[i - 1] + source[i]
    let arithmetic = profile();
    let program = array_fold(&[("i", 2, 8)], vec![8], |region, y, source, at| {
        let one = region.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(10))?;
        let before = region.binary(SolveBinaryOperator::Subtract, at[0], one, span(11))?;
        let previous = region.project_element_dynamic(y, &[before], span(12))?;
        let own = region.project_element_dynamic(source, at, span(13))?;
        let sum = region.binary(SolveBinaryOperator::Add, previous, own, span(14))?;
        region.update_element(y, sum, at, span(15))
    });
    assert_eq!(passes_of_fold(&program), None);
}

#[test]
fn a_loop_reading_the_element_it_writes_is_independent() {
    // y[i] := y[i] + source[i]
    let program = array_fold(&[("i", 1, 8)], vec![8], |region, y, source, at| {
        let old = region.project_element_dynamic(y, at, span(11))?;
        let own = region.project_element_dynamic(source, at, span(12))?;
        let sum = region.binary(SolveBinaryOperator::Add, old, own, span(13))?;
        region.update_element(y, sum, at, span(14))
    });
    assert!(passes_of_fold(&program).is_some());
}

#[test]
fn a_write_at_an_offset_of_the_binder_is_disjoint_but_a_shared_element_is_not() {
    let arithmetic = profile();
    // y[i + 1] := source[i] writes distinct elements.
    let shifted = array_fold(&[("i", 1, 7)], vec![8], |region, y, source, at| {
        let one = region.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(10))?;
        let next = region.binary(SolveBinaryOperator::Add, at[0], one, span(11))?;
        let own = region.project_element_dynamic(source, at, span(12))?;
        region.update_element(y, own, &[next], span(13))
    });
    assert!(passes_of_fold(&shifted).is_some());
    // y[1] := source[i] writes the same element on every pass.
    let shared = array_fold(&[("i", 1, 7)], vec![8], |region, y, source, at| {
        let first = region.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(10))?;
        let own = region.project_element_dynamic(source, at, span(12))?;
        region.update_element(y, own, &[first], span(13))
    });
    assert_eq!(passes_of_fold(&shared), None);
    // y[row] := source[row, column] over two binders writes a row once per column.
    let repeated = array_fold(
        &[("row", 1, 4), ("column", 1, 8)],
        vec![4, 8],
        |region, y, source, at| {
            let pixel = region.project_element_dynamic(source, at, span(12))?;
            let zero = region.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(13))?;
            region.update_element(y, pixel, &[at[0], zero], span(14))
        },
    );
    assert_eq!(passes_of_fold(&repeated), None);
}

#[test]
fn an_unproven_subscript_keeps_pass_order_and_a_proven_one_is_exact() {
    let arithmetic = profile();
    // The binder runs to 9 over an extent of 8: the read may fault.
    let past_the_end = array_fold(&[("i", 1, 9)], vec![8], |region, y, source, at| {
        let own = region.project_element_dynamic(source, &[at[0]], span(12))?;
        region.update_element(y, own, &[at[0]], span(13))
    });
    let passes = passes_of_fold(&past_the_end).expect("the writes are still disjoint");
    assert_eq!(passes.exactness(), LaneExactness::PassOrdered);
    let inside = array_fold(&[("i", 1, 8)], vec![8], |region, y, source, at| {
        let _ = arithmetic;
        let own = region.project_element_dynamic(source, &[at[0]], span(12))?;
        region.update_element(y, own, &[at[0]], span(13))
    });
    assert_eq!(
        passes_of_fold(&inside).unwrap().exactness(),
        LaneExactness::Exact
    );
}

#[test]
fn an_operation_without_an_exact_lane_form_keeps_pass_order() {
    // y[i] := sin(source[i]) is independent; the host's sine is not a lane form.
    let program = array_fold(&[("i", 1, 8)], vec![8], |region, y, source, at| {
        let own = region.project_element_dynamic(source, at, span(12))?;
        let sine = region.unary(SolveUnaryOperator::Sin, own, span(13))?;
        region.update_element(y, sine, at, span(14))
    });
    let passes = passes_of_fold(&program).unwrap();
    assert_eq!(passes.exactness(), LaneExactness::PassOrdered);
    assert!(!passes.admits_lanes());
}

#[test]
fn a_conditional_update_of_the_pass_element_stays_independent() {
    // if source[i] > 0 then y[i] := source[i] else y unchanged.
    let arithmetic = profile();
    let program = array_fold(&[("i", 1, 8)], vec![8], |region, y, source, at| {
        let own = region.project_element_dynamic(source, at, span(12))?;
        let zero = region.constant(SolveValue::real(arithmetic, 0.0), span(13))?;
        let positive = region.compare(SolveCompareOperator::Greater, own, zero, span(14))?;
        let array = region.value_type_of(y, span(15))?.clone();
        let updated = region.conditional(
            positive,
            &[y, own, at[0]],
            vec![array],
            span(16),
            |branch, inputs, outputs| {
                let y = branch.load(inputs[0], span(17))?;
                let own = branch.load(inputs[1], span(18))?;
                let index = branch.load(inputs[2], span(19))?;
                let next = branch.update_element(y, own, &[index], span(20))?;
                branch.store(outputs[0], next, span(21))
            },
            |branch, inputs, outputs| {
                let y = branch.load(inputs[0], span(22))?;
                branch.store(outputs[0], y, span(23))
            },
        )?;
        Ok(updated[0])
    });
    assert!(passes_of_fold(&program).is_some());
}

#[test]
fn every_map_has_independent_passes() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        builder.map(
            domain(&[("i", 1, 4)]),
            &[],
            real(arithmetic),
            span(0),
            |region, _, binders, output| {
                let index = region.load(binders[0], span(1))?;
                let value =
                    region.convert(SolveConversionOperator::IntegerToReal, index, span(2))?;
                region.store(output, value, span(3))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        program.independent_passes(0),
        Some(IndependentPasses::new(LaneExactness::Exact))
    );
    assert_eq!(program.independent_passes(1), None);
}

#[test]
fn index_forms_follow_affine_integer_arithmetic_of_the_binders() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        builder.map(
            domain(&[("i", 1, 4)]),
            &[],
            SolveValueType::scalar(SolveScalarType::integer(arithmetic)),
            span(0),
            |region, _, binders, output| {
                let index = region.load(binders[0], span(1))?;
                let three =
                    region.constant(SolveValue::integer(arithmetic, 3).unwrap(), span(2))?;
                let scaled = region.binary(SolveBinaryOperator::Multiply, index, three, span(3))?;
                let one = region.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(4))?;
                let shifted = region.binary(SolveBinaryOperator::Subtract, scaled, one, span(5))?;
                let squared =
                    region.binary(SolveBinaryOperator::Multiply, shifted, shifted, span(6))?;
                region.store(output, squared, span(7))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let SolveOperation::Map { body, .. } = program.operations()[0].operation() else {
        panic!("a map");
    };
    let forms = index_forms(body.body(), &[SolveIndexForm::binder(0)]);
    assert_eq!(
        forms[..5],
        [
            SolveIndexForm::binder(0),
            SolveIndexForm::Invariant(Some(3)),
            SolveIndexForm::Binder {
                ordinal: 0,
                scale: 3,
                offset: 0
            },
            SolveIndexForm::Invariant(Some(1)),
            SolveIndexForm::Binder {
                ordinal: 0,
                scale: 3,
                offset: -1
            },
        ]
    );
    // The square of a varying form is varying.
    assert_eq!(forms[5], SolveIndexForm::Varying);
}
