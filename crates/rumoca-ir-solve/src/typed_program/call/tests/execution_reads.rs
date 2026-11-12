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
