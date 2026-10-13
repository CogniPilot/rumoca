use super::*;
use rumoca_eval_solve::PureCallInvocation;
use rumoca_ir_solve::{
    SolveArithmeticProfile, SolveBinaryOperator, SolveIntegerDomain, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallTable, SolveRealFormat, SolveScalarType, SolveValue,
    SolveValueType,
};

fn arithmetic() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

fn real_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(arithmetic()))
}

fn integer_type() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::integer(arithmetic()))
}

fn identity(value: u64) -> SolvePureCallIdentity {
    SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
}

/// Owner 0 doubles a Real, owner 1 folds an Integer over a domain whose point
/// count exceeds the native i64 ordinal (a form the adapter has no lowering for), owner 2 calls owner 1, and owner 3 calls
/// owner 0.
fn mixed_table() -> SolvePureCallTable {
    let span = fixture_span();
    SolvePureCallTable::construct(arithmetic(), |table| {
        let double = table.add_owner(
            identity(1),
            vec![real_type()],
            vec![SolvePureCallOutput::result(real_type())],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let two = b.constant(SolveValue::real(arithmetic(), 2.0), span)?;
                let value = b.binary(SolveBinaryOperator::Multiply, x, two, span)?;
                b.store(outputs[0], value, span)
            },
        )?;
        let power = table.add_owner(
            identity(2),
            vec![integer_type()],
            vec![SolvePureCallOutput::result(integer_type())],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let domain = rumoca_core::StructuredIndexDomain {
                    binders: (0..2)
                        .map(|id| rumoca_core::StructuredIndexBinder {
                            provenance: rumoca_core::IndexBinderProvenance::Generated,
                            id,
                            display_name: format!("axis{id}"),
                            lower: 1,
                            upper: 3_100_000_000,
                            step: 1,
                        })
                        .collect(),
                };
                let value = b.fold(
                    domain,
                    &[x],
                    &[],
                    span,
                    |builder, carried, _captures, _binders, outputs| {
                        let carried = builder.load(carried[0], span)?;
                        builder.store(outputs[0], carried, span)
                    },
                )?;
                b.store(outputs[0], value[0], span)
            },
        )?;
        table.add_owner(
            identity(3),
            vec![integer_type()],
            vec![SolvePureCallOutput::result(integer_type())],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let result = b.call(power, &[x], span)?;
                b.store(outputs[0], result[0], span)
            },
        )?;
        table.add_owner(
            identity(4),
            vec![real_type()],
            vec![SolvePureCallOutput::result(real_type())],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let result = b.call(double, &[x], span)?;
                b.store(outputs[0], result[0], span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn admission_is_decided_per_owner_when_the_table_is_built() {
    let table = mixed_table();
    let compiled = compile_pure_call_table(&table).unwrap();
    let support = |owner: usize| compiled.owner_support(table.owners()[owner].id()).unwrap();
    assert!(support(0).primal().is_native());
    assert!(matches!(
        support(1).primal(),
        NativeFormSupport::Declined(NativeFormDecline::Lowering(_))
    ));
    assert_eq!(
        support(2).primal(),
        &NativeFormSupport::Declined(NativeFormDecline::Callee(table.owners()[1].id()))
    );
    assert!(support(3).primal().is_native());
    assert_eq!(support(0).directional(), Some(&NativeFormSupport::Native));
    assert_eq!(
        support(1).directional(),
        Some(&NativeFormSupport::Declined(
            NativeFormDecline::PrimalDeclined
        ))
    );
    for (owner, native) in [(0, true), (1, false), (2, false), (3, true)] {
        let site = table.owners()[owner].call_site();
        assert_eq!(compiled.executes(PureCallInvocation::Primal(&site)), native);
    }
}

#[test]
fn owners_beside_a_declined_one_still_run_natively() {
    let table = mixed_table();
    let compiled = compile_pure_call_table(&table).unwrap();
    let site = table.owners()[3].call_site();
    let mut output = [0.0];
    compiled
        .call_scalar_payload(
            PureCallInvocation::Primal(&site),
            &[4.5],
            &mut output,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    assert_eq!(output, [9.0]);
    let site = table.owners()[1].call_site();
    assert!(
        compiled
            .call_scalar_payload(
                PureCallInvocation::Primal(&site),
                &[3.0],
                &mut [0.0],
                &mut Vec::new(),
                &mut Vec::new(),
            )
            .is_err()
    );
}
