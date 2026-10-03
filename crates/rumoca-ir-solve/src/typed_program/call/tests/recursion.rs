use super::*;
use crate::{SolveBinaryOperator, SolveCompareOperator};

fn integer() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::integer(profile()))
}

fn real() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile()))
}

fn member(value: u64, inputs: Vec<SolveValueType>) -> SolveRecursiveMember {
    SolveRecursiveMember::new(
        identity(value),
        inputs,
        vec![SolvePureCallOutput::result(real())],
        span(value as usize),
    )
}

/// `f(n) = if n > 0 then f(n - 1) + 1 else 0`, the callee named by `next`.
fn count_down<'program>(
    next: SolvePureCallOwnerId,
    builder: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
) -> Result<(), SolveProgramConstructionError> {
    let n = builder.load(inputs[0], span(20))?;
    let zero = builder.constant(SolveValue::integer(profile(), 0).unwrap(), span(21))?;
    let positive = builder.compare(SolveCompareOperator::Greater, n, zero, span(22))?;
    let result = builder.conditional(
        positive,
        &[n],
        vec![real()],
        span(23),
        |region, inputs, outputs| {
            let n = region.load(inputs[0], span(24))?;
            let one = region.constant(SolveValue::integer(profile(), 1).unwrap(), span(25))?;
            let previous = region.binary(SolveBinaryOperator::Subtract, n, one, span(26))?;
            let rest = region.call(next, &[previous], span(27))?;
            let step = region.constant(SolveValue::real(profile(), 1.0), span(28))?;
            let value = region.binary(SolveBinaryOperator::Add, rest[0], step, span(29))?;
            region.store(outputs[0], value, span(30))
        },
        |region, _, outputs| {
            let zero = region.constant(SolveValue::real(profile(), 0.0), span(31))?;
            region.store(outputs[0], zero, span(32))
        },
    )?;
    builder.store(outputs[0], result[0], span(33))
}

/// Two mutually recursive count-down members after one ordinary owner.
fn mutual_table() -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |table| {
        add_passthrough_owner(table, identity(9), &vector_type(), span(9))?;
        table.add_recursive_group(
            vec![member(1, vec![integer()]), member(2, vec![integer()])],
            |ordinal, ids, builder, inputs, outputs| {
                count_down(ids[1 - ordinal], builder, inputs, outputs)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn mutual_recursion_forms_one_checked_group_that_replays() {
    let table = mutual_table();
    let [group] = table.recursive_groups() else {
        panic!("one recursive group");
    };
    assert_eq!(table.owners().len(), 3);
    assert!(table.recursive_group(table.owners()[0].id()).is_none());
    for owner in &table.owners()[1..] {
        assert!(group.contains(owner.id()));
        assert!(owner.directional().is_none());
        // Least fixed point: each result depends on the whole Integer input.
        let dependencies = owner.call_site().output_dependencies().to_vec();
        assert_eq!(dependencies.len(), 1);
        assert_eq!(dependencies[0].len(), 1);
        assert_eq!(dependencies[0][0].input_index(), 0);
    }
    assert_eq!(
        group.depth_limit(),
        SolveRecursionProfile::HOSTED.depth_limit()
    );
    assert!(group.frame_cells() > 0);
    let json = serde_json::to_string(&table).unwrap();
    let replayed: SolvePureCallTable = serde_json::from_str(&json).unwrap();
    assert_eq!(replayed, table);
}

#[test]
fn wire_with_a_forged_group_bound_is_refused() {
    let table = mutual_table();
    let mut json: serde_json::Value = serde_json::to_value(&table).unwrap();
    json["groups"][0]["depth_limit"] = serde_json::json!(1_000_000);
    assert!(serde_json::from_value::<SolvePureCallTable>(json).is_err());
}

#[test]
fn wire_with_a_group_outside_its_owners_is_refused() {
    let table = mutual_table();
    for (start, end) in [(7, 9), (2, 3)] {
        let mut json: serde_json::Value = serde_json::to_value(&table).unwrap();
        json["groups"][0]["start"] = serde_json::json!(start);
        json["groups"][0]["end"] = serde_json::json!(end);
        assert!(serde_json::from_value::<SolvePureCallTable>(json).is_err());
    }
}

#[test]
fn construction_errors_name_the_recursive_group_rule() {
    for (error, text) in [
        (
            SolveProgramConstructionError::InvalidRecursiveGroup {
                provenance: span(1),
            },
            "one recursive call cycle",
        ),
        (
            SolveProgramConstructionError::RecursionFrameBound {
                provenance: span(1),
            },
            "profile stack budget",
        ),
        (
            SolveProgramConstructionError::RecursiveAssertion {
                provenance: span(1),
            },
            "call-scoped assertions",
        ),
        (
            SolveProgramConstructionError::RecursiveCall {
                provenance: span(1),
            },
            "recursive owner group",
        ),
    ] {
        assert_eq!(error.source_span(), Some(span(1)));
        assert!(error.to_string().contains(text), "{error}");
    }
}

#[test]
fn members_without_a_call_cycle_are_not_a_recursive_group() {
    let error = SolvePureCallTable::construct(profile(), |table| {
        table.add_recursive_group(
            vec![member(1, vec![real()])],
            |_, _, builder, inputs, outputs| {
                let value = builder.load(inputs[0], span(40))?;
                builder.store(outputs[0], value, span(41))
            },
        )?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        SolveProgramConstructionError::InvalidRecursiveGroup { .. }
    ));
}

#[test]
fn group_frame_beyond_the_profile_stack_budget_is_refused() {
    let wide = SolveValueType::tensor(SolveScalarType::real(profile()), vec![4096]).unwrap();
    let error = SolvePureCallTable::construct(profile(), |table| {
        table.add_recursive_group(
            vec![SolveRecursiveMember::new(
                identity(1),
                vec![wide.clone()],
                vec![SolvePureCallOutput::result(wide.clone())],
                span(50),
            )],
            |_, ids, builder, inputs, outputs| {
                let value = builder.load(inputs[0], span(51))?;
                let result = builder.call(ids[0], &[value], span(52))?;
                builder.store(outputs[0], result[0], span(53))
            },
        )?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        SolveProgramConstructionError::RecursionFrameBound { .. }
    ));
}

#[test]
fn group_member_with_an_assertion_output_is_refused() {
    let error = SolvePureCallTable::construct(profile(), |table| {
        table.add_recursive_group(
            vec![SolveRecursiveMember::new(
                identity(1),
                vec![real()],
                vec![
                    SolvePureCallOutput::result(real()),
                    SolvePureCallOutput::assertion_predicate(),
                ],
                span(60),
            )],
            |_, _, _, _, _| Ok(()),
        )?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        SolveProgramConstructionError::RecursiveAssertion { .. }
    ));
}

#[test]
fn group_identity_reuse_is_refused() {
    let error = SolvePureCallTable::construct(profile(), |table| {
        table.add_recursive_group(
            vec![member(1, vec![integer()]), member(1, vec![integer()])],
            |ordinal, ids, builder, inputs, outputs| {
                count_down(ids[1 - ordinal], builder, inputs, outputs)
            },
        )?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        SolveProgramConstructionError::DuplicateCallIdentity { .. }
    ));
}
