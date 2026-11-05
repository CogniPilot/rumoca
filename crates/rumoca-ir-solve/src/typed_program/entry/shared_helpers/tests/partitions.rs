use super::*;

fn mixed(calls: Arc<SolvePureCallTable>) -> SharedHelperProgramEntry {
    let owner = calls.owners()[1].id();
    let arithmetic = calls.arithmetic();
    SharedHelperProgramEntry::construct(calls, span(50), |builder, catalog| {
        let zero = builder.constant(SolveValue::real(arithmetic, 0.), span(51))?;
        let original = catalog.original(owner, span(52))?;
        let directional = catalog.directional(owner, span(53))?;
        builder.emit_entry_call(original, &[zero, zero], span(54))?;
        builder.emit_entry_call(original, &[zero, zero], span(55))?;
        builder.emit_entry_call(directional, &[zero, zero, zero, zero], span(56))?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn repeated_invocations_share_helper_partitions_not_call_occurrences() {
    let entry = mixed(helpers());
    let plan = entry.partition_calls().unwrap();
    assert!(std::ptr::eq(plan.entry(), &entry));
    // Entry, outer primal, outer directional, inner primal, inner directional.
    assert_eq!(plan.partitions().len(), 5);
    let calls = plan.partitions()[0].calls();
    assert_eq!(calls.len(), 3);
    for call in calls {
        let occurrence = call.occurrence();
        assert!(std::ptr::eq(
            plan.entry_call_at(occurrence.program(), occurrence.operation_index())
                .unwrap(),
            call
        ));
    }
    assert!(calls[0].occurrence().id() != calls[1].occurrence().id());
    assert!(std::ptr::eq(
        plan.resolve(&calls[0]).unwrap(),
        plan.resolve(&calls[1]).unwrap()
    ));
    assert!(!std::ptr::eq(
        plan.resolve(&calls[0]).unwrap(),
        plan.resolve(&calls[2]).unwrap()
    ));
    for partition in plan.partitions() {
        assert!(std::ptr::eq(
            partition.storage_plan(),
            partition.program().storage_plan()
        ));
        for call in partition.calls() {
            let target = plan.resolve(call).unwrap();
            assert!(
                target
                    .helper()
                    .unwrap()
                    .shares_binding(call.occurrence().callee())
            );
            assert!(std::ptr::eq(
                target.program(),
                call.occurrence().callee().body()
            ));
        }
    }
    assert!(plan.partitions()[3].calls().is_empty());
    assert!(plan.partitions()[4].calls().is_empty());
}

#[test]
fn equal_independent_entries_cannot_exchange_partition_calls() {
    let calls = helpers();
    let entry = mixed(calls.clone());
    let foreign = mixed(calls);
    let own = entry.partition_calls().unwrap();
    let other = foreign.partition_calls().unwrap();
    let foreign_call = &other.partitions()[0].calls()[0];
    let occurrence = foreign_call.occurrence();
    assert!(
        own.entry_call_at(occurrence.program(), occurrence.operation_index())
            .is_none()
    );
    assert!(matches!(
        own.resolve(foreign_call),
        Err(SolveProgramConstructionError::UnknownCallOwner { provenance })
            if provenance == span(54)
    ));
    let cloned = entry.clone();
    let shared = cloned.partition_calls().unwrap();
    assert!(shared.resolve(&own.partitions()[0].calls()[0]).is_ok());
}

#[test]
fn unreachable_helpers_are_not_compilation_partitions() {
    let calls = helpers();
    let owner = calls.owners()[0].id();
    let arithmetic = calls.arithmetic();
    let entry = SharedHelperProgramEntry::construct(calls, span(60), |builder, catalog| {
        let zero = builder.constant(SolveValue::real(arithmetic, 0.), span(61))?;
        let target = catalog.original(owner, span(62))?;
        builder.emit_entry_call(target, &[zero, zero], span(63))?;
        Ok(())
    })
    .unwrap();
    let plan = entry.partition_calls().unwrap();
    assert_eq!(plan.partitions().len(), 2);
    assert_eq!(plan.partitions()[0].calls().len(), 1);
    assert!(plan.partitions()[1].calls().is_empty());
    assert!(plan.partitions()[0].helper().is_none());
}

#[test]
fn lazy_region_calls_keep_their_scopes_and_share_only_the_helper_body() {
    let calls = helpers();
    let owner = calls.owners()[0].id();
    let arithmetic = calls.arithmetic();
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let entry = SharedHelperProgramEntry::construct(calls, span(70), |builder, catalog| {
        let target = catalog.original(owner, span(71))?;
        let zero = builder.constant(SolveValue::real(arithmetic, 0.), span(72))?;
        let condition = builder.constant(SolveValue::boolean(true), span(73))?;
        let value = builder.conditional(
            condition,
            &[zero],
            vec![ty.clone()],
            span(74),
            |b, captures, output| arm(b, captures, output, target),
            |b, captures, output| arm(b, captures, output, target),
        )?;
        let output = builder.declare_slot(
            ty,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(75),
        )?;
        builder.store(output, value[0], span(76))
    })
    .unwrap();
    let plan = entry.partition_calls().unwrap();
    assert_eq!(plan.partitions().len(), 2);
    let calls = plan.partitions()[0].calls();
    assert_eq!(calls.len(), 2);
    for call in calls {
        let occurrence = call.occurrence();
        assert!(std::ptr::eq(
            plan.entry_call_at(occurrence.program(), occurrence.operation_index())
                .unwrap(),
            call
        ));
    }
    assert!(!std::ptr::eq(
        calls[0].occurrence().program(),
        calls[1].occurrence().program()
    ));
    assert!(!std::ptr::eq(calls[0].occurrence().program(), entry.body()));
    assert!(std::ptr::eq(
        plan.resolve(&calls[0]).unwrap(),
        plan.resolve(&calls[1]).unwrap()
    ));
}

#[test]
fn recursive_helper_is_one_partition_with_a_back_edge() {
    let arithmetic = SolvePureCallTable::default().arithmetic();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let calls = Arc::new(
        SolvePureCallTable::construct(arithmetic, |table| {
            table.add_recursive_group(
                vec![crate::SolveRecursiveMember::new(
                    SolvePureCallIdentity::issued(NonZeroU64::new(7).unwrap()),
                    vec![integer.clone()],
                    vec![SolvePureCallOutput::result(integer.clone())],
                    span(80),
                )],
                |_, ids, builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span(81))?;
                    let result = builder.call(ids[0], &[input], span(82))?;
                    builder.store(outputs[0], result[0], span(83))
                },
            )?;
            Ok(())
        })
        .unwrap(),
    );
    let owner = calls.owners()[0].id();
    let entry = SharedHelperProgramEntry::construct(calls, span(84), |builder, catalog| {
        let zero = builder.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(85))?;
        builder.emit_entry_call(catalog.original(owner, span(86))?, &[zero], span(87))?;
        Ok(())
    })
    .unwrap();
    // Planning is finite; this fixture's recursive function is never executed.
    let plan = entry.partition_calls().unwrap();
    assert_eq!(plan.partitions().len(), 2);
    let recursive = &plan.partitions()[1];
    assert_eq!(recursive.calls().len(), 1);
    assert!(std::ptr::eq(
        plan.resolve(&recursive.calls()[0]).unwrap(),
        recursive
    ));
}

#[test]
fn empty_entry_has_one_empty_partition() {
    let entry = SharedHelperProgramEntry::construct(
        Arc::new(SolvePureCallTable::default()),
        span(90),
        |_, _| Ok(()),
    )
    .unwrap();
    let plan = entry.partition_calls().unwrap();
    assert_eq!(plan.partitions().len(), 1);
    assert!(plan.partitions()[0].calls().is_empty());
    assert!(std::ptr::eq(plan.partitions()[0].program(), entry.body()));
}
