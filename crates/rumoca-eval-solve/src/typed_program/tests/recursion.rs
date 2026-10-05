use super::*;
use rumoca_ir_solve::{SolveRecursionProfile, SolveRecursiveMember};

fn arithmetic() -> SolveArithmeticProfile {
    profile(SolveRealFormat::Binary64)
}

fn integer_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::integer(arithmetic()))
}

fn real_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(arithmetic()))
}

/// One ordinary entry owner calling the self-recursive member
/// `f(n) = if n > 0 then f(n - 1) + 1 else 0`.
fn count_down_table() -> SolvePureCallTable {
    SolvePureCallTable::construct(arithmetic(), |table| {
        let ids = table.add_recursive_group(
            vec![SolveRecursiveMember::new(
                identity(1),
                vec![integer_type()],
                vec![SolvePureCallOutput::result(real_type())],
                span(1),
            )],
            |_, ids, builder, inputs, outputs| {
                let n = builder.load(inputs[0], span(2))?;
                let zero =
                    builder.constant(SolveValue::integer(arithmetic(), 0).unwrap(), span(3))?;
                let positive = builder.compare(SolveCompareOperator::Greater, n, zero, span(4))?;
                let result = builder.conditional(
                    positive,
                    &[n],
                    vec![real_type()],
                    span(5),
                    |region, inputs, outputs| {
                        let n = region.load(inputs[0], span(6))?;
                        let one = region
                            .constant(SolveValue::integer(arithmetic(), 1).unwrap(), span(7))?;
                        let previous =
                            region.binary(SolveBinaryOperator::Subtract, n, one, span(8))?;
                        let rest = region.call(ids[0], &[previous], span(9))?;
                        let step =
                            region.constant(SolveValue::real(arithmetic(), 1.0), span(10))?;
                        let value =
                            region.binary(SolveBinaryOperator::Add, rest[0], step, span(11))?;
                        region.store(outputs[0], value, span(12))
                    },
                    |region, _, outputs| {
                        let zero =
                            region.constant(SolveValue::real(arithmetic(), 0.0), span(13))?;
                        region.store(outputs[0], zero, span(14))
                    },
                )?;
                builder.store(outputs[0], result[0], span(15))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![integer_type()],
            vec![SolvePureCallOutput::result(real_type())],
            span(20),
            |builder, inputs, outputs| {
                let n = builder.load(inputs[0], span(21))?;
                let result = builder.call(ids[0], &[n], span(22))?;
                builder.store(outputs[0], result[0], span(23))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn count(n: i64) -> TypedValue {
    TypedValue::construct(
        integer_type(),
        vec![SolveValue::integer(arithmetic(), n).unwrap().kind()],
    )
    .unwrap()
}

fn real_value(value: f64) -> TypedValue {
    TypedValue::construct(
        real_type(),
        vec![real_kind(SolveRealFormat::Binary64, value)],
    )
    .unwrap()
}

#[test]
fn recursive_group_executes_each_member_invocation_in_its_own_frame() {
    let table = count_down_table();
    let limit = i64::from(SolveRecursionProfile::HOSTED.depth_limit());
    for owner in table.owners() {
        // Entering from outside the group or at the member itself, the
        // deepest admitted chain holds exactly `limit` member invocations.
        let deepest = limit - 1;
        let result = eval_pure_call(&table, owner.id(), &[count(deepest)]).unwrap();
        assert_eq!(result, vec![real_value(deepest as f64)]);
    }
}

#[test]
fn recursive_call_beyond_the_depth_limit_fails_at_that_call() {
    let table = count_down_table();
    let limit = SolveRecursionProfile::HOSTED.depth_limit();
    for owner in table.owners() {
        let error = eval_pure_call(&table, owner.id(), &[count(i64::from(limit))]).unwrap_err();
        assert_eq!(
            error,
            TypedProgramEvalError::RecursionDepthExceeded {
                limit,
                provenance: span(9),
            }
        );
        assert!(error.to_string().contains("depth limit of 64"), "{error}");
    }
}

/// Both the deepest admitted chain and the refusal one call beyond it run
/// within 1 MiB, the smallest default thread stack of a hosted platform (the
/// Windows main thread): each member invocation takes one interpreter frame,
/// so the depth limit bounds the native stack.
#[test]
fn recursion_depth_limit_holds_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(1024 * 1024)
        .spawn(|| {
            recursive_group_executes_each_member_invocation_in_its_own_frame();
            recursive_call_beyond_the_depth_limit_fails_at_that_call();
        })
        .expect("spawn the small-stack evaluation")
        .join()
        .expect("the recursive evaluation stays within a small stack");
}
