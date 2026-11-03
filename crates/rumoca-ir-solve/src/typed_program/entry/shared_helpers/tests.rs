use super::*;
use crate::{
    SolveBinaryOperator, SolveOperation, SolvePureCallIdentity, SolvePureCallOutput,
    SolveScalarType, SolveSlotAccess, SolveStorageClass, SolveValue, SolveValueType,
};
use std::num::NonZeroU64;

fn span(n: usize) -> Span {
    crate::source_span_from_offsets(51, n, n + 1)
}

fn helpers() -> Arc<SolvePureCallTable> {
    let arithmetic = SolvePureCallTable::default().arithmetic();
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    Arc::new(
        SolvePureCallTable::construct(arithmetic, |table| {
            let inner = table.add_owner(
                SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
                vec![ty.clone(); 2],
                vec![SolvePureCallOutput::result(ty.clone())],
                span(0),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(1))?;
                    let y = b.load(inputs[1], span(2))?;
                    let result = b.binary(SolveBinaryOperator::Divide, x, y, span(3))?;
                    b.store(outputs[0], result, span(4))
                },
            )?;
            table.add_owner(
                SolvePureCallIdentity::issued(NonZeroU64::new(2).unwrap()),
                vec![ty.clone(); 2],
                vec![SolvePureCallOutput::result(ty)],
                span(5),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(6))?;
                    let y = b.load(inputs[1], span(7))?;
                    let result = b.call(inner, &[x, y], span(8))?;
                    b.store(outputs[0], result[0], span(9))
                },
            )?;
            Ok(())
        })
        .unwrap(),
    )
}

#[test]
fn mixed_entry_issues_exact_occurrences_and_preserves_child_body_namespace() {
    let calls = helpers();
    let id = calls.owners()[1].id();
    let arithmetic = calls.arithmetic();
    let entry = SharedHelperProgramEntry::construct(calls.clone(), span(10), |b, catalog| {
        let original = catalog.original(id, span(11))?;
        let directional = catalog.directional(id, span(12))?;
        let zero = b.constant(SolveValue::real(arithmetic, 0.), span(13))?;
        let raw = b.emit_entry_call(original, &[zero, zero], span(14))?;
        let dual = b.emit_entry_call(directional, &[zero, zero, zero, zero], span(15))?;
        let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
        for value in [raw.registers()[0], dual.registers()[1]] {
            let output = b.declare_slot(
                ty.clone(),
                SolveStorageClass::Output,
                SolveSlotAccess::ReadWrite,
                span(16),
            )?;
            b.store(output, value, span(17))?;
        }
        Ok(())
    })
    .unwrap();
    let occurrences = entry
        .call_occurrences()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    assert_eq!(occurrences.len(), 2);
    let repeated = entry
        .call_occurrences()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    assert!(occurrences[0].id() == repeated[0].id());
    assert!(occurrences[0].id() != occurrences[1].id());
    for occurrence in &occurrences {
        assert!(std::ptr::eq(
            &occurrence.program().operations()[occurrence.operation_index()],
            occurrence.operation()
        ));
    }
    use crate::SolveCallValueRole::{Primal, Tangent};
    assert_eq!(
        occurrences[0].callee().input_roles().collect::<Vec<_>>(),
        vec![Primal { original: 0 }, Primal { original: 1 }]
    );
    assert_eq!(
        occurrences[1].callee().input_roles().collect::<Vec<_>>(),
        vec![
            Primal { original: 0 },
            Tangent { original: 0 },
            Primal { original: 1 },
            Tangent { original: 1 }
        ]
    );
    assert_eq!(
        occurrences[1].callee().output_roles().collect::<Vec<_>>(),
        vec![Primal { original: 0 }, Tangent { original: 0 }]
    );

    assert_eq!(entry.output_slots().len(), 2);
    assert!(entry.shares_entry_owner(&entry.clone()));
    assert!(entry.input_slots().is_empty());
    for (index, occurrence) in occurrences.iter().enumerate() {
        assert!(std::ptr::eq(occurrence.program(), entry.body()));
        let SolveOperation::Call {
            arguments,
            destinations,
            assertion_forwarding,
            ..
        } = occurrence.operation().operation()
        else {
            panic!("issued call occurrence")
        };
        assert!(std::ptr::eq(occurrence.arguments(), arguments.as_ref()));
        assert!(std::ptr::eq(
            occurrence.destinations(),
            destinations.as_ref()
        ));
        assert_eq!(
            occurrence.assertion_forwarding(),
            assertion_forwarding.as_ref()
        );
        let expected = if index == 0 {
            calls.bind_original_body(id, span(14)).unwrap()
        } else {
            calls.bind_directional_body(id, span(15)).unwrap()
        };
        assert!(occurrence.callee().shares_binding(expected));
        let child = occurrence
            .callee()
            .call_occurrences()
            .next()
            .unwrap()
            .unwrap();
        assert_eq!(child.arguments().len(), if index == 0 { 2 } else { 4 });
        assert_eq!(child.destinations().len(), if index == 0 { 1 } else { 2 });
    }
}

#[test]
fn internal_foreign_catalog_target_is_refused_during_construction() {
    let calls = helpers();
    let id = calls.owners()[0].id();
    let arithmetic = calls.arithmetic();
    SharedHelperProgramEntry::construct(calls, span(22), |b, own| {
        check_internal_foreign(b, own, id, arithmetic)
    })
    .unwrap();
}

fn check_internal_foreign<'program>(
    b: &mut TypedProgramBuilder<'program>,
    own: &EntryCallCatalog<'program>,
    id: SolvePureCallOwnerId,
    arithmetic: crate::SolveArithmeticProfile,
) -> Result<(), SolveProgramConstructionError> {
    let target = EntryCallTarget {
        namespace: &own.bindings[..0],
        selector: SolvePureCallOwnerId::LIFECYCLE_METHOD,
        marker: std::marker::PhantomData,
    };
    let zero = b.constant(SolveValue::real(arithmetic, 0.), span(23))?;
    assert!(
        matches!(b.emit_entry_call(target, &[zero, zero], span(24)), Err(SolveProgramConstructionError::UnknownCallOwner { provenance }) if provenance == span(24))
    );
    let _ = b.emit_entry_call(own.original(id, span(25))?, &[zero, zero], span(26))?;
    Ok(())
}

#[test]
fn call_occurrences_walk_lazy_regions_in_source_order_with_local_register_owners() {
    let calls = helpers();
    let id = calls.owners()[0].id();
    let arithmetic = calls.arithmetic();
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let entry = SharedHelperProgramEntry::construct(calls, span(30), |b, catalog| {
        let original = catalog.original(id, span(31))?;
        let zero = b.constant(SolveValue::real(arithmetic, 0.), span(32))?;
        let condition = b.constant(SolveValue::boolean(true), span(33))?;
        // Distinct region occurrences stay distinct even with identical bodies.
        let output = b.conditional(
            condition,
            &[zero],
            vec![ty.clone()],
            span(37),
            |b, c, o| arm(b, c, o, original),
            |b, c, o| arm(b, c, o, original),
        )?;
        let slot = b.declare_slot(
            ty,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(38),
        )?;
        b.store(slot, output[0], span(39))
    })
    .unwrap();
    let occurrences = entry
        .call_occurrences()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    assert_eq!(occurrences.len(), 2);
    assert!(!std::ptr::eq(occurrences[0].program(), entry.body()));
    assert!(!std::ptr::eq(
        occurrences[0].program(),
        occurrences[1].program()
    ));
    let arms = entry
        .body()
        .operations()
        .iter()
        .find_map(|op| match op.operation() {
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => Some((if_true.body(), if_false.body())),
            _ => None,
        })
        .unwrap();
    assert!(std::ptr::eq(occurrences[0].program(), arms.0));
    assert!(std::ptr::eq(occurrences[1].program(), arms.1));
}

fn arm<'program>(
    b: &mut TypedProgramBuilder<'program>,
    captures: &[crate::ProgramSlot<'program>],
    outputs: &[crate::ProgramSlot<'program>],
    target: EntryCallTarget<'_>,
) -> Result<(), SolveProgramConstructionError> {
    let value = b.load(captures[0], span(34))?;
    let target = b.rebind_entry_call_target(target, span(35))?;
    let result = b.emit_entry_call(target, &[value, value], span(35))?;
    b.store(outputs[0], result.registers()[0], span(36))
}

#[test]
fn nested_independent_entry_refuses_a_foreign_rebind() {
    let calls = helpers();
    let inner_calls = calls.clone();
    let id = calls.owners()[0].id();
    SharedHelperProgramEntry::construct(calls, span(40), |_outer, catalog| {
        let target = catalog.original(id, span(41))?;
        SharedHelperProgramEntry::construct(inner_calls, span(42), |inner, _| {
            assert!(matches!(inner.rebind_entry_call_target(target, span(43)), Err(SolveProgramConstructionError::UnknownCallOwner { provenance }) if provenance == span(43)));
            Ok(())
        })?;
        Ok(())
    }).unwrap();
}
