use super::*;
use crate::SolveBinaryOperator;

fn real() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile()))
}

/// A caller that calls one callee at the top level, in a map body, and in a
/// conditional arm, so the count follows every nested region.
#[test]
fn call_sites_count_every_nested_region_per_caller_and_callee() {
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let table = SolvePureCallTable::construct(profile(), |table| {
        let callee = table.add_owner(
            identity(1),
            vec![real()],
            vec![SolvePureCallOutput::result(real())],
            span(0),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1))?;
                builder.store(outputs[0], x, span(2))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![real(), boolean.clone()],
            vec![SolvePureCallOutput::result(real())],
            span(10),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(11))?;
                let flag = builder.load(inputs[1], span(12))?;
                let top = builder.call(callee, &[x], span(13))?;
                let domain = rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                };
                let mapped = builder.map(
                    domain,
                    &[x],
                    real(),
                    span(14),
                    |region, captures, _binders, output| {
                        let x = region.load(captures[0], span(15))?;
                        let called = region.call(callee, &[x], span(16))?;
                        region.store(output, called[0], span(17))
                    },
                )?;
                let _ = mapped;
                let chosen = builder.conditional(
                    flag,
                    &[x],
                    vec![real()],
                    span(18),
                    |arm, inputs, outputs| {
                        let x = arm.load(inputs[0], span(19))?;
                        let called = arm.call(callee, &[x], span(20))?;
                        arm.store(outputs[0], called[0], span(21))
                    },
                    |arm, inputs, outputs| {
                        let x = arm.load(inputs[0], span(22))?;
                        arm.store(outputs[0], x, span(23))
                    },
                )?;
                let sum = builder.binary(SolveBinaryOperator::Add, top[0], chosen[0], span(24))?;
                builder.store(outputs[0], sum, span(25))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let counts = table.call_site_counts();
    assert_eq!(counts.len(), 1, "only the caller owner issues calls");
    assert_eq!(counts[0].caller, table.owners()[1].id());
    assert_eq!(counts[0].callee, table.owners()[0].id());
    assert_eq!(counts[0].callee_provenance, span(0));
    assert_eq!(
        counts[0].sites, 3,
        "top level, map body and conditional arm"
    );
}
