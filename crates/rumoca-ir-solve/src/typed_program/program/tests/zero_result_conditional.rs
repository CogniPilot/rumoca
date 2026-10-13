use super::*;

#[test]
fn zero_result_flow_does_not_admit_an_empty_pure_call_interface() {
    let mut table = crate::SolvePureCallTable::builder(profile());
    let identity = crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(919).unwrap());
    let error = table
        .add_owner(identity, Vec::new(), Vec::new(), span(1), |_, _, _| {
            panic!("empty call interface must refuse before building its body")
        })
        .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::EmptyCallOutput {
            provenance: span(1)
        }
    );
}

#[test]
fn zero_result_conditional_retains_both_region_bodies_and_checked_replay() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let condition = builder.constant(SolveValue::boolean(true), span(0))?;
        let results = builder.conditional(
            condition,
            &[],
            Vec::new(),
            span(1),
            |region, _, outputs| {
                assert!(outputs.is_empty());
                region.constant(SolveValue::boolean(true), span(2))?;
                Ok(())
            },
            |region, _, outputs| {
                assert!(outputs.is_empty());
                region.constant(SolveValue::boolean(false), span(3))?;
                Ok(())
            },
        )?;
        assert!(results.is_empty());
        Ok(())
    })
    .unwrap();
    let bytes = serde_json::to_vec(&program).unwrap();
    let replayed: TypedProgram = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(replayed, program);
    let SolveOperation::Conditional {
        destinations,
        if_true,
        if_false,
        ..
    } = program.operations()[1].operation()
    else {
        panic!("source flow conditional was erased");
    };
    assert!(destinations.is_empty());
    assert!(if_true.outputs().is_empty());
    assert!(if_false.outputs().is_empty());
    assert_eq!(if_true.body().operations()[0].provenance(), span(2));
    assert_eq!(if_false.body().operations()[0].provenance(), span(3));
}
