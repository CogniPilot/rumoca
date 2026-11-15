use super::*;
use crate::{SolveFiniteCheck, SolveUnaryOperator};

fn guarded_table(guard: bool, logarithm: bool) -> SolvePureCallTable {
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1100),
            vec![scalar.clone(), scalar.clone()],
            vec![SolvePureCallOutput::result(scalar.clone())],
            span(1100),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1101))?;
                let q = builder.load(inputs[1], span(1102))?;
                let checked = if logarithm {
                    builder.unary(SolveUnaryOperator::Log, q, span(1103))?
                } else {
                    q
                };
                if guard {
                    builder.check_finite(
                        checked,
                        SolveFiniteCheck::Primal {
                            program: 7,
                            operation: 3,
                        },
                        span(1104),
                    )?;
                }
                builder.store(outputs[0], x, span(1105))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_guard_only_input_is_an_execution_read_but_not_a_value_dependency() {
    for logarithm in [false, true] {
        let table = guarded_table(true, logarithm);
        let site = table.owners()[0].call_site();
        assert_eq!(site.execution_reads(), &[1]);
        assert_eq!(
            site.output_dependencies()[0]
                .iter()
                .flat_map(SolveCallDependency::inputs)
                .collect::<Vec<_>>(),
            [0],
            "the result x keeps its original value/Jacobian dependency"
        );
    }
}

#[test]
fn an_unused_value_without_a_guard_has_no_extra_execution_read() {
    assert!(
        guarded_table(false, true).owners()[0]
            .call_site()
            .execution_reads()
            .is_empty()
    );
}

#[test]
fn wire_replay_rederives_guard_reads_instead_of_trusting_a_wire_field() {
    let table = guarded_table(true, false);
    let mut wire = serde_json::to_value(&table).unwrap();
    assert!(!wire.to_string().contains("execution_reads"));
    wire["owners"][0]["execution_reads"] = serde_json::json!([]);
    let replay: SolvePureCallTable = serde_json::from_value(wire).unwrap();
    assert_eq!(replay.owners()[0].call_site().execution_reads(), &[1]);
    assert_eq!(table, replay);
}

#[test]
fn site_and_owner_equality_compare_the_execution_reads() {
    // Same identity, interface and value dependencies; only the guard differs.
    let guarded = guarded_table(true, false);
    let plain = guarded_table(false, false);
    let guarded_site = guarded.owners()[0].call_site();
    let plain_site = plain.owners()[0].call_site();
    assert_eq!(
        guarded_site.output_dependencies(),
        plain_site.output_dependencies()
    );
    assert_ne!(guarded_site, plain_site);
    assert!(guarded.matches_site(&guarded_site));
    assert!(!plain.matches_site(&guarded_site));
    assert!(!guarded.matches_site(&plain_site));
    let guarded_directional = guarded_site.directional().unwrap();
    let plain_directional = plain_site.directional().unwrap();
    assert_ne!(guarded_directional, plain_directional);
}

#[test]
fn the_directional_interface_retains_the_primal_guard_input() {
    let table = guarded_table(true, false);
    let site = table.owners()[0].call_site();
    let directional = site
        .directional()
        .expect("a finite check has a tangent owner");
    assert!(directional.execution_reads().contains(&2));
}

fn real() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile()))
}

fn domain() -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

/// A finite check of a constant NaN: an obligation that reads no input.
fn check_nan(b: &mut TypedProgramBuilder<'_>) -> Result<(), SolveProgramConstructionError> {
    let nan = b.constant(SolveValue::real(profile(), f64::NAN), span(1277))?;
    check(b, nan)
}

fn check<'p>(
    b: &mut TypedProgramBuilder<'p>,
    q: super::super::super::program::ProgramRegister<'p>,
) -> Result<(), SolveProgramConstructionError> {
    b.check_finite(q, SolveFiniteCheck::Derivative, span(1200))
}

#[test]
fn guard_reads_substitute_through_two_calls_without_widening_values() {
    let mut table = SolvePureCallTable::builder(profile());
    let child = table
        .add_owner(
            identity(1200),
            vec![real(), real()],
            vec![SolvePureCallOutput::result(real())],
            span(1200),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(1201))?;
                let q = b.load(inputs[1], span(1202))?;
                check(b, q)?;
                b.store(outputs[0], x, span(1203))
            },
        )
        .unwrap();
    let mut previous = child;
    for ordinal in 0..2 {
        previous = table
            .add_owner(
                identity(1201 + ordinal),
                vec![real(), real()],
                vec![SolvePureCallOutput::result(real())],
                span(1210),
                |b, inputs, outputs| {
                    let first = b.load(inputs[0], span(1211))?;
                    let second = b.load(inputs[1], span(1212))?;
                    let result = b.call(previous, &[second, first], span(1213))?;
                    b.store(outputs[0], result[0], span(1214))
                },
            )
            .unwrap();
    }
    let table = table.finish();
    for (owner, guard, value) in [(0, 1, 0), (1, 0, 1), (2, 1, 0)] {
        let site = table.owners()[owner].call_site();
        assert_eq!(site.execution_reads(), &[guard]);
        assert_eq!(
            site.output_dependencies()[0]
                .iter()
                .flat_map(SolveCallDependency::inputs)
                .collect::<Vec<_>>(),
            [value]
        );
    }
}

#[test]
fn a_constant_guard_still_makes_its_branch_predicate_an_execution_read() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1220),
            vec![SolveValueType::scalar(SolveScalarType::Boolean), real()],
            vec![SolvePureCallOutput::result(real())],
            span(1220),
            |b, inputs, outputs| {
                let condition = b.load(inputs[0], span(1221))?;
                b.conditional(
                    condition,
                    &[],
                    vec![],
                    span(1222),
                    |b, _, _| {
                        let nan = b.constant(SolveValue::real(profile(), f64::NAN), span(1223))?;
                        check(b, nan)
                    },
                    |_, _, _| Ok(()),
                )?;
                let x = b.load(inputs[1], span(1224))?;
                b.store(outputs[0], x, span(1225))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert_eq!(site.execution_reads(), &[0]);
    assert!(site.has_execution_obligations());
    assert_eq!(
        site.output_dependencies()[0][0]
            .inputs()
            .collect::<Vec<_>>(),
        [1]
    );
}

fn region_guard<'p>(
    kind: u64,
    b: &mut TypedProgramBuilder<'p>,
    q: super::super::super::program::ProgramRegister<'p>,
    condition: super::super::super::program::ProgramRegister<'p>,
) -> Result<(), SolveProgramConstructionError> {
    let zero = b.constant(SolveValue::real(profile(), 0.0), span(1233))?;
    match kind {
        0 => {
            b.conditional(
                condition,
                &[q],
                vec![],
                span(1234),
                |b, captures, _| {
                    let q = b.load(captures[0], span(1235))?;
                    check(b, q)
                },
                |_, _, _| Ok(()),
            )?;
        }
        1 => {
            b.conditional_assigned(
                condition,
                (&[q], &[]),
                (vec![], vec![]),
                span(1234),
                |b, captures, _, _| {
                    let q = b.load(captures[0], span(1235))?;
                    check(b, q)
                },
                |_, _, _, _| Ok(()),
            )?;
        }
        2 => {
            b.map(
                domain(),
                &[q],
                real(),
                span(1234),
                |b, captures, _, output| {
                    let q = b.load(captures[0], span(1235))?;
                    check(b, q)?;
                    b.store(output, q, span(1236))
                },
            )?;
        }
        3 => {
            b.fold(
                domain(),
                &[zero],
                &[q],
                span(1234),
                |b, carried, captures, _, outputs| {
                    let q = b.load(captures[0], span(1235))?;
                    check(b, q)?;
                    let value = b.load(carried[0], span(1236))?;
                    b.store(outputs[0], value, span(1237))
                },
            )?;
        }
        _ => {
            b.fold_while(
                domain(),
                &[zero],
                &[q],
                span(1234),
                |b, _, captures, _, outputs| {
                    let q = b.load(captures[0], span(1235))?;
                    check(b, q)?;
                    let yes = b.constant(SolveValue::boolean(true), span(1236))?;
                    b.store(outputs[0], yes, span(1237))
                },
                |b, carried, _, _, outputs| {
                    let value = b.load(carried[0], span(1236))?;
                    b.store(outputs[0], value, span(1237))
                },
            )?;
        }
    }
    Ok(())
}

#[test]
fn every_region_retains_guard_only_captures_at_region_granularity() {
    for kind in 0..5 {
        let table = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1230 + kind),
                vec![
                    real(),
                    real(),
                    SolveValueType::scalar(SolveScalarType::Boolean),
                ],
                vec![SolvePureCallOutput::result(real())],
                span(1230),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(1231))?;
                    let q = b.load(inputs[1], span(1232))?;
                    let condition = b.load(inputs[2], span(1233))?;
                    region_guard(kind, b, q, condition)?;
                    b.store(outputs[0], x, span(1238))
                },
            )?;
            Ok(())
        })
        .unwrap();
        let site = table.owners()[0].call_site();
        assert_eq!(
            site.execution_reads(),
            if kind < 2 { &[1, 2][..] } else { &[1][..] },
            "region {kind}"
        );
        assert_eq!(
            site.output_dependencies()[0][0]
                .inputs()
                .collect::<Vec<_>>(),
            [0]
        );
        let replay: SolvePureCallTable =
            serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
        assert_eq!(replay, table);
    }
}

#[test]
fn recursive_group_guard_reads_reach_members_without_a_local_guard() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        let members = (0..2)
            .map(|ordinal| {
                SolveRecursiveMember::new(
                    identity(1250 + ordinal),
                    vec![real(), real()],
                    vec![SolvePureCallOutput::result(real())],
                    span(1250),
                )
            })
            .collect();
        table.add_recursive_group(members, |ordinal, ids, b, inputs, outputs| {
            let x = b.load(inputs[0], span(1251))?;
            let q = b.load(inputs[1], span(1252))?;
            if ordinal == 0 {
                check(b, q)?;
            }
            b.call(ids[1 - ordinal], &[x, q], span(1253))?;
            b.store(outputs[0], x, span(1254))
        })?;
        Ok(())
    })
    .unwrap();
    for owner in table.owners() {
        let site = owner.call_site();
        assert_eq!(site.execution_reads(), &[1]);
        assert!(site.has_execution_obligations());
        assert_eq!(
            site.output_dependencies()[0][0]
                .inputs()
                .collect::<Vec<_>>(),
            [0]
        );
    }
    let replay: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
    assert_eq!(replay, table);
}

#[test]
fn assertion_predicate_reads_are_already_owned_value_dependencies() {
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1260),
            vec![boolean],
            vec![SolvePureCallOutput::assertion_predicate()],
            span(1260),
            |b, inputs, outputs| {
                let condition = b.load(inputs[0], span(1261))?;
                let assertion = b.assertion_output(0, span(1262))?;
                b.check_assertion(assertion, condition, &[], span(1263), |_, _, _| Ok(()))?;
                b.store(outputs[0], condition, span(1264))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert_eq!(
        site.output_dependencies()[0][0]
            .inputs()
            .collect::<Vec<_>>(),
        [0]
    );
    assert_eq!(site.execution_reads(), &[0]);
}

#[test]
fn assigned_region_state_captures_retain_their_guard_execution_reads() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1270),
            vec![real()],
            vec![SolvePureCallOutput::result(real())],
            span(1270),
            |b, inputs, outputs| {
                let q = b.load(inputs[0], span(1271))?;
                let state = b.present_assigned_state(q, span(1272))?;
                let yes = b.constant(SolveValue::boolean(true), span(1273))?;
                b.conditional_assigned(
                    yes,
                    (&[], &[state]),
                    (vec![], vec![]),
                    span(1274),
                    |b, _, states, _| {
                        let present = b.assigned_state_is_present(states[0], span(1275))?;
                        b.conditional(
                            present,
                            &[],
                            vec![],
                            span(1276),
                            |b, _, _| check_nan(b),
                            |_, _, _| Ok(()),
                        )?;
                        Ok(())
                    },
                    |_, _, _, _| Ok(()),
                )?;
                let value = b.constant(SolveValue::real(profile(), 1.0), span(1278))?;
                b.store(outputs[0], value, span(1279))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert_eq!(site.execution_reads(), &[0]);
    assert!(site.output_dependencies()[0].is_empty());
}

type Slots<'a, 'p> = &'a [super::super::super::program::ProgramSlot<'p>];

/// One pass that checks the carried value and carries it on.
fn guarded_pass<'p>(
    b: &mut TypedProgramBuilder<'p>,
    carried: Slots<'_, 'p>,
    _: Slots<'_, 'p>,
    _: Slots<'_, 'p>,
    outputs: Slots<'_, 'p>,
) -> Result<(), SolveProgramConstructionError> {
    let value = b.load(carried[0], span(1295))?;
    check(b, value)?;
    b.store(outputs[0], value, span(1296))
}

/// One pass of an inner fold: it checks the value it captured and keeps its
/// own carried value.
fn capturing_pass<'p>(
    b: &mut TypedProgramBuilder<'p>,
    carried: Slots<'_, 'p>,
    captures: Slots<'_, 'p>,
    _: Slots<'_, 'p>,
    outputs: Slots<'_, 'p>,
) -> Result<(), SolveProgramConstructionError> {
    let captured = b.load(captures[0], span(1297))?;
    check(b, captured)?;
    let kept = b.load(carried[0], span(1298))?;
    b.store(outputs[0], kept, span(1299))
}

/// One pass of an outer fold whose guard sits in an inner fold that captures
/// the outer carried value.
fn nesting_pass<'p>(
    b: &mut TypedProgramBuilder<'p>,
    carried: Slots<'_, 'p>,
    _: Slots<'_, 'p>,
    _: Slots<'_, 'p>,
    outputs: Slots<'_, 'p>,
) -> Result<(), SolveProgramConstructionError> {
    let value = b.load(carried[0], span(1295))?;
    let one = b.constant(SolveValue::real(profile(), 1.0), span(1293))?;
    let inner = b.fold(domain(), &[one], &[value], span(1296), capturing_pass)?;
    let _ = inner;
    b.store(outputs[0], value, span(1300))
}

/// A fold whose passes are followed at their binder values: the guard reads
/// the carried value, which starts from input 1; input 0 is the result.
fn followed_fold_table(nested: bool) -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1290 + u64::from(nested)),
            vec![real(), real()],
            vec![SolvePureCallOutput::result(real())],
            span(1290),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(1291))?;
                let q = b.load(inputs[1], span(1292))?;
                let carried = if nested {
                    b.fold(domain(), &[q], &[], span(1294), nesting_pass)?
                } else {
                    b.fold(domain(), &[q], &[], span(1294), guarded_pass)?
                };
                let _ = carried;
                b.store(outputs[0], x, span(1301))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_followed_fold_keeps_the_guard_read_of_its_carried_start() {
    use crate::typed_program::call::dependency::followed_work;
    for nested in [false, true] {
        followed_work::reset();
        let table = followed_fold_table(nested);
        assert!(
            followed_work::most().passes > 0,
            "the fold's passes are followed at their binder values (nested={nested})"
        );
        let site = table.owners()[0].call_site();
        assert_eq!(site.execution_reads(), &[1], "nested={nested}");
        assert!(site.has_execution_obligations());
        assert_eq!(
            site.output_dependencies()[0]
                .iter()
                .flat_map(SolveCallDependency::inputs)
                .collect::<Vec<_>>(),
            [0],
            "nested={nested}"
        );
        let replay: SolvePureCallTable =
            serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
        assert_eq!(replay, table);
    }
}

fn vector() -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::real(profile()), vec![3]).unwrap()
}

/// A finite check of `r[lane]` in a branch taken while `r[lane] < x`.
fn lane_branch<'p>(
    b: &mut TypedProgramBuilder<'p>,
    r: super::super::super::program::ProgramRegister<'p>,
    x: super::super::super::program::ProgramRegister<'p>,
    lane: u32,
) -> Result<(), SolveProgramConstructionError> {
    let z = b.project_element(r, vec![lane], span(1315))?;
    let near = b.compare(crate::SolveCompareOperator::Less, z, x, span(1316))?;
    b.conditional(
        near,
        &[z],
        vec![],
        span(1317),
        |b, captures, _| {
            let z = b.load(captures[0], span(1318))?;
            check(b, z)
        },
        |_, _, _| Ok(()),
    )?;
    Ok(())
}

/// Checks only `r[lane]`: `kind` 0 directly, 1 in a branch on it, 2 in that
/// branch inside a fold followed pass by pass over captures `(r, x)`, 3 in
/// that branch in the early-exit predicate of a fold summarised over all
/// passes, and 4 by an Integer conversion of it that can fault but whose
/// result nothing reads.
fn lane_guard<'p>(
    kind: u64,
    b: &mut TypedProgramBuilder<'p>,
    r: super::super::super::program::ProgramRegister<'p>,
    x: super::super::super::program::ProgramRegister<'p>,
    lane: u32,
) -> Result<(), SolveProgramConstructionError> {
    let zero = b.constant(SolveValue::real(profile(), 0.0), span(1313))?;
    match kind {
        0 => {
            let z = b.project_element(r, vec![lane], span(1314))?;
            check(b, z)
        }
        1 => lane_branch(b, r, x, lane),
        4 => {
            let z = b.project_element(r, vec![lane], span(1314))?;
            b.convert(
                crate::SolveConversionOperator::RealToIntegerTowardZero,
                z,
                span(1327),
            )?;
            Ok(())
        }
        2 => {
            b.fold(
                domain(),
                &[zero],
                &[r, x],
                span(1320),
                |b, carried, captures, _, outputs| {
                    let r = b.load(captures[0], span(1321))?;
                    let x = b.load(captures[1], span(1322))?;
                    lane_branch(b, r, x, lane)?;
                    let kept = b.load(carried[0], span(1323))?;
                    b.store(outputs[0], kept, span(1324))
                },
            )?;
            Ok(())
        }
        _ => {
            b.fold_while(
                domain(),
                &[zero],
                &[r, x],
                span(1320),
                |b, _, captures, _, outputs| {
                    let r = b.load(captures[0], span(1321))?;
                    let x = b.load(captures[1], span(1322))?;
                    lane_branch(b, r, x, lane)?;
                    let yes = b.constant(SolveValue::boolean(true), span(1325))?;
                    b.store(outputs[0], yes, span(1326))
                },
                |b, carried, _, _, outputs| {
                    let kept = b.load(carried[0], span(1323))?;
                    b.store(outputs[0], kept, span(1324))
                },
            )?;
            Ok(())
        }
    }
}

/// An owner of `(r[3], x)` that returns `x` after [`lane_guard`].
fn lane_guard_table(kind: u64, lane: u32) -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1310 + kind),
            vec![vector(), real()],
            vec![SolvePureCallOutput::result(real())],
            span(1310),
            |b, inputs, outputs| {
                let r = b.load(inputs[0], span(1311))?;
                let x = b.load(inputs[1], span(1312))?;
                lane_guard(kind, b, r, x, lane)?;
                b.store(outputs[0], x, span(1319))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_guard_on_one_lane_of_a_vector_input_reads_that_lane_alone() {
    use crate::typed_program::call::dependency::followed_work;
    for kind in 0..5 {
        for lane in [0, 2] {
            followed_work::reset();
            let table = lane_guard_table(kind, lane);
            if kind == 2 {
                assert!(followed_work::most().passes > 0, "the fold is followed");
            }
            let site = table.owners()[0].call_site();
            assert!(site.has_execution_obligations(), "kind {kind}");
            let elements = site.execution_read_elements();
            assert_eq!(
                elements[0],
                (0, Some(vec![lane as usize].into_boxed_slice())),
                "kind {kind} lane {lane}: the other lanes are never read"
            );
            // A branch is decided by `r[lane] < x`, so it also reads `x`.
            let expected: &[usize] = if matches!(kind, 0 | 4) { &[0] } else { &[0, 1] };
            assert_eq!(site.execution_reads(), expected, "kind {kind}");
            assert_eq!(
                site.output_dependencies()[0]
                    .iter()
                    .flat_map(SolveCallDependency::inputs)
                    .collect::<Vec<_>>(),
                [1],
                "the result keeps its value dependency on x alone"
            );
            let replay: SolvePureCallTable =
                serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
            assert_eq!(replay, table);
        }
    }
}

#[test]
fn a_lane_guard_substitutes_through_a_call_that_passes_the_whole_vector() {
    let mut table = SolvePureCallTable::builder(profile());
    let child = table
        .add_owner(
            identity(1330),
            vec![vector()],
            vec![SolvePureCallOutput::result(real())],
            span(1330),
            |b, inputs, outputs| {
                let r = b.load(inputs[0], span(1331))?;
                let z = b.project_element(r, vec![2], span(1332))?;
                check(b, z)?;
                let one = b.constant(SolveValue::real(profile(), 1.0), span(1333))?;
                b.store(outputs[0], one, span(1334))
            },
        )
        .unwrap();
    table
        .add_owner(
            identity(1331),
            vec![vector()],
            vec![SolvePureCallOutput::result(real())],
            span(1335),
            |b, inputs, outputs| {
                let r = b.load(inputs[0], span(1336))?;
                let result = b.call(child, &[r], span(1337))?;
                b.store(outputs[0], result[0], span(1338))
            },
        )
        .unwrap();
    let table = table.finish();
    for owner in table.owners() {
        assert_eq!(
            owner.call_site().execution_read_elements(),
            [(0, Some(vec![2].into_boxed_slice()))]
        );
    }
}

#[test]
fn a_guard_that_reads_a_carried_value_of_a_summarised_fold_reads_every_start_and_capture() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1340),
            vec![vector(), real()],
            vec![SolvePureCallOutput::result(real())],
            span(1340),
            |b, inputs, outputs| {
                let r = b.load(inputs[0], span(1341))?;
                let x = b.load(inputs[1], span(1342))?;
                b.fold_while(
                    domain(),
                    &[x],
                    &[r],
                    span(1343),
                    |b, carried, _, _, outputs| {
                        let value = b.load(carried[0], span(1344))?;
                        check(b, value)?;
                        let yes = b.constant(SolveValue::boolean(true), span(1345))?;
                        b.store(outputs[0], yes, span(1346))
                    },
                    |b, carried, captures, _, outputs| {
                        let r = b.load(captures[0], span(1347))?;
                        let z = b.project_element(r, vec![0], span(1348))?;
                        let kept = b.load(carried[0], span(1349))?;
                        let next = b.binary(crate::SolveBinaryOperator::Add, kept, z, span(1350))?;
                        b.store(outputs[0], next, span(1351))
                    },
                )?;
                b.store(outputs[0], x, span(1352))
            },
        )?;
        Ok(())
    })
    .unwrap();
    // The carried value starts at x and each pass adds r[1], so the guard
    // covers x and every lane of r, not only the lane the update reads.
    let site = table.owners()[0].call_site();
    let elements = site.execution_read_elements();
    assert_eq!(
        elements,
        [
            (0, Some(vec![0, 1, 2].into_boxed_slice())),
            (1, Some(vec![0].into_boxed_slice()))
        ]
    );
}

#[test]
fn an_early_exit_predicate_is_read_by_the_obligations_it_lets_run() {
    // The transition checks capture c; the predicate `0 < n` on capture n
    // decides whether a pass runs, so n is read although no check reads it.
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1360),
            vec![real(), real(), real()],
            vec![SolvePureCallOutput::result(real())],
            span(1360),
            |b, inputs, outputs| {
                let c = b.load(inputs[0], span(1361))?;
                let n = b.load(inputs[1], span(1362))?;
                let x = b.load(inputs[2], span(1363))?;
                let zero = b.constant(SolveValue::real(profile(), 0.0), span(1364))?;
                b.fold_while(
                    domain(),
                    &[zero],
                    &[c, n],
                    span(1365),
                    |b, _, captures, _, outputs| {
                        let n = b.load(captures[1], span(1366))?;
                        let zero = b.constant(SolveValue::real(profile(), 0.0), span(1367))?;
                        let go =
                            b.compare(crate::SolveCompareOperator::Less, zero, n, span(1368))?;
                        b.store(outputs[0], go, span(1369))
                    },
                    |b, carried, captures, _, outputs| {
                        let c = b.load(captures[0], span(1370))?;
                        check(b, c)?;
                        let kept = b.load(carried[0], span(1371))?;
                        b.store(outputs[0], kept, span(1372))
                    },
                )?;
                b.store(outputs[0], x, span(1373))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert!(site.has_execution_obligations());
    assert_eq!(site.execution_reads(), &[0, 1]);
}

/// `out[i] = a[i] + t*(b[i] - a[i])` over `i` in 1..3, reading `a` and `b`
/// through bounds-checked binder subscripts, as an interpolation does.
fn interpolation_table() -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1380),
            vec![real(), vector(), vector()],
            vec![SolvePureCallOutput::result(vector())],
            span(1380),
            |b, inputs, outputs| {
                let t = b.load(inputs[0], span(1381))?;
                let a = b.load(inputs[1], span(1382))?;
                let c = b.load(inputs[2], span(1383))?;
                let domain = rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".into(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                };
                let out = b.map(domain, &[t, a, c], real(), span(1384), interpolate)?;
                b.store(outputs[0], out, span(1390))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn interpolate<'p>(
    b: &mut TypedProgramBuilder<'p>,
    captures: Slots<'_, 'p>,
    binders: Slots<'_, 'p>,
    output: super::super::super::program::ProgramSlot<'p>,
) -> Result<(), SolveProgramConstructionError> {
    let t = b.load(captures[0], span(1385))?;
    let a = b.load(captures[1], span(1385))?;
    let c = b.load(captures[2], span(1385))?;
    let i = b.load(binders[0], span(1386))?;
    let ai = b.project_element_dynamic(a, &[i], span(1387))?;
    let ci = b.project_element_dynamic(c, &[i], span(1387))?;
    let d = b.binary(crate::SolveBinaryOperator::Subtract, ci, ai, span(1388))?;
    let s = b.binary(crate::SolveBinaryOperator::Multiply, t, d, span(1388))?;
    let r = b.binary(crate::SolveBinaryOperator::Add, ai, s, span(1388))?;
    b.store(output, r, span(1389))
}

#[test]
fn a_bounds_checked_map_subscript_reads_no_element_of_its_aggregate() {
    let table = interpolation_table();
    let site = table.owners()[0].call_site();
    // Element i of the result reads element i of a and b; the bounds checks
    // read only the binder, which reads no input.
    assert!(site.execution_reads().is_empty(), "{:?}", site.execution_read_elements());
}

#[test]
fn a_data_derived_subscript_still_reads_its_index() {
    let integer = SolveValueType::scalar(SolveScalarType::integer(profile()));
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1395),
            vec![vector(), integer],
            vec![SolvePureCallOutput::result(real())],
            span(1395),
            |b, inputs, outputs| {
                let r = b.load(inputs[0], span(1396))?;
                let k = b.load(inputs[1], span(1397))?;
                let rk = b.project_element_dynamic(r, &[k], span(1398))?;
                b.store(outputs[0], rk, span(1399))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert!(site.has_execution_obligations());
    assert_eq!(site.execution_reads(), &[1], "the index k, never the values of r");
}
