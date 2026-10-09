use super::*;

#[derive(Clone, Copy, Debug)]
enum Region {
    Conditional,
    Map,
    FoldTransition,
    FoldContinuation,
}

type BuildResult = Result<(), solve::SolveProgramConstructionError>;

fn store_minimum<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    output: solve::ProgramSlot<'p>,
) -> BuildResult {
    let x = b.constant(solve::SolveValue::real(profile(), 1.0), span())?;
    let y = b.binary(solve::SolveBinaryOperator::Min, x, x, span())?;
    b.store(output, y, span())
}

fn passthrough<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    inputs: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> BuildResult {
    let x = b.load(inputs[0], span())?;
    b.store(outputs[0], x, span())
}

fn fold_minimum<'p>(
    b: &mut solve::TypedProgramBuilder<'p>,
    _carried: &[solve::ProgramSlot<'p>],
    _captures: &[solve::ProgramSlot<'p>],
    _binders: &[solve::ProgramSlot<'p>],
    outputs: &[solve::ProgramSlot<'p>],
) -> BuildResult {
    store_minimum(b, outputs[0])
}

fn domain() -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

fn region_table(region: Region) -> solve::SolvePureCallTable {
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()));
    let output = if matches!(region, Region::Map) {
        solve::SolveValueType::tensor(real.element_type(), vec![2]).unwrap()
    } else {
        real.clone()
    };
    solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(3).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::result(output)],
            span(),
            |b, _, outputs| {
                let x = b.constant(solve::SolveValue::real(profile(), 1.0), span())?;
                let y = match region {
                    Region::Conditional => {
                        let condition = b.constant(solve::SolveValue::boolean(true), span())?;
                        b.conditional(
                            condition,
                            &[x],
                            vec![real],
                            span(),
                            |b, _, out| store_minimum(b, out[0]),
                            passthrough,
                        )?[0]
                    }
                    Region::Map => b.map(domain(), &[], real, span(), |b, _, _, output| {
                        store_minimum(b, output)
                    })?,
                    Region::FoldTransition => b.fold(domain(), &[x], &[], span(), fold_minimum)?[0],
                    Region::FoldContinuation => b.fold_while(
                        domain(),
                        &[x],
                        &[],
                        span(),
                        |b, _, _, outputs| {
                            let x = b.constant(solve::SolveValue::real(profile(), 1.0), span())?;
                            let y = b.binary(solve::SolveBinaryOperator::Min, x, x, span())?;
                            let condition =
                                b.compare(solve::SolveCompareOperator::Equal, x, y, span())?;
                            b.store(outputs[0], condition, span())
                        },
                        |b, carried, _, _, outputs| {
                            let x = b.load(carried[0], span())?;
                            b.store(outputs[0], x, span())
                        },
                    )?[0],
                };
                b.store(outputs[0], y, span())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn typed_extrema_detection_enters_each_structured_region_kind() {
    for region in [
        Region::Conditional,
        Region::Map,
        Region::FoldTransition,
        Region::FoldContinuation,
    ] {
        assert!(detected(&region_table(region)), "{region:?}");
    }
}

#[test]
fn typed_extrema_detection_enters_failed_only_assertion_message_regions() {
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()));
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(4).unwrap()),
            vec![],
            vec![
                solve::SolvePureCallOutput::assertion_predicate(),
                solve::SolvePureCallOutput::assertion_message_value(real, 1),
            ],
            span(),
            |b, _, outputs| {
                let condition = b.constant(solve::SolveValue::boolean(true), span())?;
                let assertion = b.assertion_output(0, span())?;
                let messages =
                    b.check_assertion(assertion, condition, &[], span(), |b, _, outputs| {
                        store_minimum(b, outputs[0])
                    })?;
                b.store(outputs[0], condition, span())?;
                b.store(outputs[1], messages[0], span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert!(detected(&table));
}

#[test]
fn typed_table_walk_visits_canonical_primal_and_directional_owners_once_without_following_calls() {
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()));
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        let child = table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(5).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::result(real.clone())],
            span(),
            |b, _, out| store_minimum(b, out[0]),
        )?;
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(6).unwrap()),
            vec![],
            vec![solve::SolvePureCallOutput::result(real)],
            span(),
            |b, _, out| {
                let result = b.call(child, &[], span())?;
                b.store(out[0], result[0], span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    #[derive(Default)]
    struct Programs(std::collections::BTreeSet<usize>);
    impl solve::SolveVisitor for Programs {
        type Error = std::convert::Infallible;
        fn visit_typed_program(
            &mut self,
            program: &solve::TypedProgram,
        ) -> Result<(), Self::Error> {
            assert!(
                self.0.insert(std::ptr::from_ref(program) as usize),
                "duplicate owner"
            );
            solve::walk_typed_program(self, program)
        }
    }
    use solve::SolveVisitor;
    let mut visitor = Programs::default();
    let model = solve::SolveModel {
        pure_calls: table,
        ..Default::default()
    };
    visitor.visit_solve_model(&model).unwrap();
    assert!(
        visitor.0.is_empty(),
        "ordinary model walks do not opt into typed owners"
    );
    visitor.visit_pure_call_table(&model.pure_calls).unwrap();
    let mut expected = std::collections::BTreeSet::new();
    for owner in model.pure_calls.owners() {
        expected.insert(std::ptr::from_ref(owner.body()) as usize);
        expected.insert(std::ptr::from_ref(owner.directional().unwrap().body()) as usize);
    }
    assert_eq!(visitor.0, expected);
    assert_eq!(expected.len(), 4);
}
