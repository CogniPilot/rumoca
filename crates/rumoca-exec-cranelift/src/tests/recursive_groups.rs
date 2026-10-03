use super::*;
use rumoca_ir_solve::{
    SolveArithmeticProfile, SolveBinaryOperator, SolveCompareOperator, SolveIntegerDomain,
    SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable, SolveRealFormat,
    SolveRecursionProfile, SolveRecursiveMember, SolveScalarType, SolveValue, SolveValueType,
};

fn arithmetic() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

fn real_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(arithmetic()))
}

fn identity(value: u64) -> SolvePureCallIdentity {
    SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
}

/// An ordinary entry owner calling the self-recursive member
/// `f(x) = if x > 0.5 then f(x - 1) + 1 else 0`.
fn count_down_table() -> SolvePureCallTable {
    let span = fixture_span();
    let real = real_type();
    SolvePureCallTable::construct(arithmetic(), |table| {
        let ids = table.add_recursive_group(
            vec![SolveRecursiveMember::new(
                identity(1),
                vec![real.clone()],
                vec![SolvePureCallOutput::result(real.clone())],
                span,
            )],
            |_, ids, builder, inputs, outputs| {
                let x = builder.load(inputs[0], span)?;
                let half = builder.constant(SolveValue::real(arithmetic(), 0.5), span)?;
                let positive = builder.compare(SolveCompareOperator::Greater, x, half, span)?;
                let result = builder.conditional(
                    positive,
                    &[x],
                    vec![real.clone()],
                    span,
                    |region, inputs, outputs| {
                        let x = region.load(inputs[0], span)?;
                        let one = region.constant(SolveValue::real(arithmetic(), 1.0), span)?;
                        let previous =
                            region.binary(SolveBinaryOperator::Subtract, x, one, span)?;
                        let rest = region.call(ids[0], &[previous], span)?;
                        let value = region.binary(SolveBinaryOperator::Add, rest[0], one, span)?;
                        region.store(outputs[0], value, span)
                    },
                    |region, _, outputs| {
                        let zero = region.constant(SolveValue::real(arithmetic(), 0.0), span)?;
                        region.store(outputs[0], zero, span)
                    },
                )?;
                builder.store(outputs[0], result[0], span)
            },
        )?;
        table.add_owner(
            identity(2),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span,
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span)?;
                let result = builder.call(ids[0], &[x], span)?;
                builder.store(outputs[0], result[0], span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn call(
    compiled: &CompiledPureCallTable,
    table: &SolvePureCallTable,
    owner: usize,
    x: f64,
) -> Result<f64, CompileError> {
    let site = table.owners()[owner].call_site();
    let mut output = [0.0];
    compiled.call_scalar_payload(
        rumoca_eval_solve::PureCallInvocation::Primal(&site),
        &[x],
        &mut output,
        &mut Vec::new(),
        &mut Vec::new(),
    )?;
    Ok(output[0])
}

#[test]
fn compiled_recursive_group_counts_member_invocations_against_the_profile_limit() {
    let table = count_down_table();
    let compiled = compile_pure_call_table(&table).unwrap();
    let limit = f64::from(SolveRecursionProfile::HOSTED.depth_limit());
    for owner in 0..table.owners().len() {
        assert_eq!(call(&compiled, &table, owner, 3.0).unwrap(), 3.0);
        // `limit` member invocations are admitted; one more is refused.
        assert_eq!(
            call(&compiled, &table, owner, limit - 1.0).unwrap(),
            limit - 1.0
        );
        let error = call(&compiled, &table, owner, limit).unwrap_err();
        assert!(error.to_string().contains("depth limit"), "{error}");
        // A refused chain leaves no state behind for the next invocation.
        assert_eq!(call(&compiled, &table, owner, 2.0).unwrap(), 2.0);
    }
}
