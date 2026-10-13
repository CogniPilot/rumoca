use super::*;
use crate::{SolveAssertionLevel, SolvePureCallIdentity, SolvePureCallTable};
use rumoca_core::SourceId;
use std::num::NonZeroU64;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("call-capability.mo"), 0, 1)
}

#[test]
fn call_capability_binds_operation_owner_and_every_destination() {
    let p = SolveArithmeticProfile::construct(
        crate::SolveRealFormat::Binary64,
        crate::SolveIntegerDomain::FULL,
    );
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let mut table = SolvePureCallTable::builder(p);
    let mut children = Vec::new();
    for identity in [1, 2] {
        children.push(
            table
                .add_owner(
                    SolvePureCallIdentity::issued(NonZeroU64::new(identity).unwrap()),
                    vec![boolean.clone()],
                    vec![SolvePureCallOutput::assertion_predicate_at_level(
                        SolveAssertionLevel::Error,
                    )],
                    span(),
                    |builder, inputs, outputs| {
                        let condition = builder.load(inputs[0], span())?;
                        let assertion = builder.assertion_output(0, span())?;
                        builder
                            .check_assertion(assertion, condition, &[], span(), |_, _, _| Ok(()))?;
                        builder.store(outputs[0], condition, span())
                    },
                )
                .unwrap(),
        );
    }
    table
        .add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(3).unwrap()),
            vec![boolean],
            vec![SolvePureCallOutput::assertion_predicate()],
            span(),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span())?;
                let call = builder.emit_call(children[0], &[condition], span())?;
                let another = builder.emit_call(children[0], &[condition], span())?;
                let parent = builder.assertion_output(0, span())?;
                let mut forged = ProgramCall {
                    operation: another.operation,
                    owner: call.owner,
                    registers: call.registers.clone(),
                    marker: PhantomData,
                };
                assert!(
                    builder
                        .forward_assertion(&forged, 0, parent, span())
                        .is_err()
                );
                forged.operation = call.operation;
                forged.owner = children[1];
                assert!(
                    builder
                        .forward_assertion(&forged, 0, parent, span())
                        .is_err()
                );
                forged.owner = call.owner;
                forged.registers = another.registers.clone();
                assert!(
                    builder
                        .forward_assertion(&forged, 0, parent, span())
                        .is_err()
                );
                builder.forward_assertion(&call, 0, parent, span())?;
                builder.store(outputs[0], call.registers()[0], span())
            },
        )
        .unwrap();
}
