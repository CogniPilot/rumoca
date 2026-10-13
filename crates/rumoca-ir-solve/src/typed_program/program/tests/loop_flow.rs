use super::*;

fn counted_map(count: i64, body_operations: usize) -> TypedProgram {
    let arithmetic = profile();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: count,
            step: 1,
        }],
    };
    TypedProgram::construct(arithmetic, |builder| {
        builder.map(
            domain.clone(),
            &[],
            integer,
            span(0),
            |region, _, binders, output| {
                let mut value = region.load(binders[0], span(1))?;
                for step in 0..body_operations {
                    let one = region.constant(
                        SolveValue::integer(arithmetic, step as i64 + 1).unwrap(),
                        span(2),
                    )?;
                    value = region.binary(SolveBinaryOperator::Add, value, one, span(3))?;
                }
                region.store(output, value, span(4))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_loop_with_a_constant_trip_count_within_the_bound_carries_an_unroll_property() {
    let program = counted_map(MAX_UNROLLED_PASSES as i64, 2);
    assert_eq!(program.unrolled_passes(0, false), Some(MAX_UNROLLED_PASSES));
    assert_eq!(program.unrolled_passes(0, true), Some(MAX_UNROLLED_PASSES));
}

#[test]
fn a_loop_past_the_pass_or_operation_bound_keeps_its_counted_loop() {
    assert_eq!(
        counted_map(MAX_UNROLLED_PASSES as i64 + 1, 2).unrolled_passes(0, false),
        None
    );
    // Each pass emits 2 * 17 + 2 operations, so 32 passes exceed the budget.
    let body = MAX_UNROLLED_OPERATIONS / MAX_UNROLLED_PASSES;
    assert_eq!(counted_map(32, body).unrolled_passes(0, false), None);
}

#[test]
fn a_program_without_a_loop_at_an_operation_has_no_unroll_property() {
    let program = counted_map(4, 1);
    assert_eq!(program.unrolled_passes(0, false), Some(4));
    assert_eq!(program.unrolled_passes(99, false), None);
    assert!(!program.observes());
}

/// What the early-exit test of `guarded_fold` reads.
#[derive(Clone, Copy)]
enum ExitTest {
    /// The first captured Boolean.
    Capture,
    /// The conjunction of both captured Booleans.
    Conjunction,
    /// The conjunction of a captured Boolean and a comparison of the carried
    /// value with zero.
    Carried,
    /// The conjunction of a captured Boolean and a comparison of the binder
    /// of the pass about to run.
    Binder,
}

/// `s := s + i` over `1:4`, tested before each pass by `test`.
fn guarded_fold(test: ExitTest) -> TypedProgram {
    let arithmetic = profile();
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 4,
            step: 1,
        }],
    };
    TypedProgram::construct(arithmetic, |builder| {
        let initial = builder.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(0))?;
        let enabled = builder.constant(SolveValue::boolean(true), span(0))?;
        let allowed = builder.constant(SolveValue::boolean(true), span(0))?;
        builder.fold_while(
            domain.clone(),
            &[initial],
            &[enabled, allowed],
            span(1),
            |region, carried, captures, binders, outputs| {
                let flag = region.load(captures[0], span(2))?;
                let zero = region.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(3))?;
                let probed = if matches!(test, ExitTest::Carried) {
                    carried[0]
                } else {
                    binders[0]
                };
                let tested = match test {
                    ExitTest::Capture => flag,
                    ExitTest::Conjunction => {
                        let other = region.load(captures[1], span(3))?;
                        region.binary(SolveBinaryOperator::And, flag, other, span(3))?
                    }
                    ExitTest::Carried | ExitTest::Binder => {
                        let value = region.load(probed, span(3))?;
                        let positive = region.compare(
                            SolveCompareOperator::GreaterEqual,
                            value,
                            zero,
                            span(3),
                        )?;
                        region.binary(SolveBinaryOperator::And, flag, positive, span(3))?
                    }
                };
                region.store(outputs[0], tested, span(4))
            },
            |region, carried, _, binders, outputs| {
                let old = region.load(carried[0], span(5))?;
                let index = region.load(binders[0], span(5))?;
                let next = region.binary(SolveBinaryOperator::Add, old, index, span(5))?;
                region.store(outputs[0], next, span(5))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

/// The fold of `guarded_fold(test)`, its operation index, and whether its
/// exit test is invariant.
fn guard_of(test: ExitTest) -> (TypedProgram, usize, bool) {
    let program = guarded_fold(test);
    let fold = program
        .operations()
        .iter()
        .position(|operation| matches!(operation.operation(), SolveOperation::Fold { .. }))
        .unwrap();
    let invariant = invariant_guard(program.operations()[fold].operation()).is_some();
    (program, fold, invariant)
}

#[test]
fn an_exit_test_over_one_capture_is_invariant_and_leaves_the_loop_unrolled() {
    let (program, fold, invariant) = guard_of(ExitTest::Capture);
    assert!(invariant);
    assert_eq!(program.unrolled_passes(fold, false), Some(4));
}

#[test]
fn an_exit_test_computed_from_several_captures_is_invariant_and_leaves_the_loop_unrolled() {
    let (program, fold, invariant) = guard_of(ExitTest::Conjunction);
    assert!(invariant);
    assert_eq!(program.unrolled_passes(fold, false), Some(4));
}

#[test]
fn an_exit_test_that_reads_the_carried_value_keeps_the_counted_loop() {
    let (program, fold, invariant) = guard_of(ExitTest::Carried);
    assert!(!invariant);
    assert_eq!(program.unrolled_passes(fold, false), None);
}

#[test]
fn an_exit_test_that_reads_the_binder_keeps_the_counted_loop() {
    let (program, fold, invariant) = guard_of(ExitTest::Binder);
    assert!(!invariant);
    assert_eq!(program.unrolled_passes(fold, false), None);
}
