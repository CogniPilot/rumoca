use super::*;
use crate::{SolveScalarType, SolveSlotAccess, SolveStorageClass, SolveValue, SolveValueType};
use rumoca_core::{SourceId, Span};

fn span(n: usize) -> Span {
    Span::from_offsets(SourceId::from_source_name("complete_entry.mo"), n, n + 1)
}
fn calls() -> Arc<SolvePureCallTable> {
    Arc::new(SolvePureCallTable::default())
}
fn real(calls: &SolvePureCallTable) -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(calls.arithmetic()))
}
fn copy(calls: Arc<SolvePureCallTable>) -> TypedProgramEntry {
    let ty = real(&calls);
    TypedProgramEntry::construct(calls, |b| {
        let input = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let scratch = b.declare_slot(
            ty.clone(),
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let output = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let value = b.load(input, span(3))?;
        b.store(scratch, value, span(4))?;
        let value = b.load(scratch, span(5))?;
        b.store(output, value, span(6))
    })
    .unwrap()
}

#[test]
fn calls_resolve_only_in_the_retained_construction_namespace() {
    let arithmetic = calls().arithmetic();
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |t| {
        t.add_owner(
            crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![ty.clone()],
            vec![crate::SolvePureCallOutput::result(ty.clone())],
            span(0),
            |b, inputs, outputs| {
                let v = b.load(inputs[0], span(1))?;
                b.store(outputs[0], v, span(2))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = table.owners()[0].id();
    let table = Arc::new(table);
    let entry = TypedProgramEntry::construct(table.clone(), |b| {
        let out = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(3),
        )?;
        let v = b.constant(SolveValue::real(arithmetic, -0.0), span(4))?;
        let values = b.call(owner, &[v], span(5))?;
        b.store(out, values[0], span(6))
    })
    .unwrap();
    assert!(entry.calls().shares_table_owner(&table));
    assert!(
        matches!(entry.body().operations()[1].operation(), crate::SolveOperation::Call { owner: actual, .. } if *actual == owner)
    );
    let error = TypedProgramEntry::construct(calls(), |b| {
        let v = b.constant(SolveValue::real(arithmetic, -0.0), span(4))?;
        b.call(owner, &[v], span(5))?;
        Ok(())
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::UnknownCallOwner {
            provenance: span(5)
        }
    );
}

#[test]
fn large_tensor_entry_interface_does_not_enumerate_elements() {
    let table = calls();
    let ty =
        SolveValueType::tensor(SolveScalarType::real(table.arithmetic()), vec![2_000_000]).unwrap();
    let entry = TypedProgramEntry::construct(table, |b| {
        let input = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let v = b.load(input, span(2))?;
        b.store(output, v, span(3))
    })
    .unwrap();
    assert_eq!(entry.body().register_types().len(), 1);
    assert_eq!(entry.body().operations().len(), 2);
    assert_eq!(entry.input_slots().len(), 1);
    assert_eq!(entry.output_slots().len(), 1);
}

#[test]
fn interface_tracks_arguments_and_results_but_not_local_scratch() {
    let table = calls();
    let entry = copy(table.clone());
    assert!(entry.calls().shares_table_owner(&table));
    assert_eq!(entry.input_slots(), &[entry.body().slots()[0].id()]);
    assert_eq!(entry.output_slots(), &[entry.body().slots()[2].id()]);
    assert_eq!(entry.body().operations().len(), 4);
}

#[test]
fn cloned_entries_share_original_owners_but_equal_constructions_do_not() {
    let table = calls();
    let entry = copy(table.clone());
    assert!(entry.shares_entry_owner(&entry.clone()));
    let second = copy(table);
    assert_eq!(entry.body(), second.body());
    assert!(!entry.shares_entry_owner(&second));
    assert!(!entry.calls().shares_table_owner(&calls()));
}

#[test]
fn every_externally_initialized_role_is_a_read_only_argument() {
    for role in [
        SolveStorageClass::Input,
        SolveStorageClass::Constant,
        SolveStorageClass::TunableParameter,
        SolveStorageClass::CalculatedParameter,
        SolveStorageClass::PersistentState,
        SolveStorageClass::PreviousState,
        SolveStorageClass::SignalStatus,
    ] {
        let table = calls();
        let ty = real(&table);
        let entry = TypedProgramEntry::construct(table, |b| {
            b.declare_slot(ty, role, SolveSlotAccess::ReadOnly, span(0))?;
            Ok(())
        })
        .unwrap();
        assert_eq!(entry.input_slots().len(), 1);
        assert!(entry.output_slots().is_empty());
    }
}

#[test]
fn writable_external_arguments_refuse_at_declaration() {
    let table = calls();
    let ty = real(&table);
    let error = TypedProgramEntry::construct(table, |b| {
        b.declare_slot(
            ty,
            SolveStorageClass::Input,
            SolveSlotAccess::ReadWrite,
            span(7),
        )?;
        Ok(())
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::InvalidEntryInterface {
            provenance: span(7)
        }
    );
}

#[test]
fn missing_output_is_refused_at_its_declaration() {
    let table = calls();
    let ty = real(&table);
    let error = TypedProgramEntry::construct(table, |b| {
        b.declare_slot(
            ty,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(8),
        )?;
        Ok(())
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::UninitializedSlot {
            provenance: span(8)
        }
    );
}

#[test]
fn duplicate_output_store_refuses_without_publishing_an_entry() {
    let table = calls();
    let ty = real(&table);
    let arithmetic = table.arithmetic();
    let error = TypedProgramEntry::construct(table, |b| {
        let slot = b.declare_slot(
            ty,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(0),
        )?;
        let zero = b.constant(SolveValue::real(arithmetic, -0.0), span(1))?;
        b.store(slot, zero, span(2))?;
        b.store(slot, zero, span(3))
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::InvalidEntryInterface {
            provenance: span(3)
        }
    );
}

#[test]
fn read_only_output_refuses_and_empty_entry_is_complete() {
    let table = calls();
    let ty = real(&table);
    let error = TypedProgramEntry::construct(table, |b| {
        b.declare_slot(
            ty,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadOnly,
            span(4),
        )?;
        Ok(())
    })
    .unwrap_err();
    assert_eq!(
        error,
        SolveProgramConstructionError::InvalidEntryInterface {
            provenance: span(4)
        }
    );
    let empty = TypedProgramEntry::construct(calls(), |_| Ok(())).unwrap();
    assert!(empty.input_slots().is_empty() && empty.output_slots().is_empty());
    assert!(empty.body().operations().is_empty());
}

#[test]
fn output_obligations_survive_out_of_order_writes_and_later_declarations() {
    for missing in [None, Some(0), Some(1), Some(2), Some(3)] {
        let table = calls();
        let ty = real(&table);
        let arithmetic = table.arithmetic();
        let result = TypedProgramEntry::construct(table, |b| {
            let value = b.constant(SolveValue::real(arithmetic, -0.0), span(10))?;
            let mut outputs = Vec::new();
            for index in 0..3 {
                outputs.push(b.declare_slot(
                    ty.clone(),
                    SolveStorageClass::Output,
                    SolveSlotAccess::ReadWrite,
                    span(index),
                )?);
                b.declare_slot(
                    ty.clone(),
                    SolveStorageClass::MethodLocal,
                    SolveSlotAccess::ReadWrite,
                    span(20 + index),
                )?;
            }
            for index in [2, 0, 1]
                .into_iter()
                .filter(|index| missing != Some(*index))
            {
                b.store(outputs[index], value, span(30 + index))?;
            }
            let last = b.declare_slot(
                ty,
                SolveStorageClass::Output,
                SolveSlotAccess::ReadWrite,
                span(3),
            )?;
            if missing != Some(3) {
                b.store(last, value, span(40))?;
            }
            Ok(())
        });
        if let Some(index) = missing {
            assert_eq!(
                result.unwrap_err(),
                SolveProgramConstructionError::UninitializedSlot {
                    provenance: span(index)
                }
            );
        } else {
            assert_eq!(result.unwrap().output_slots().len(), 4);
        }
    }
}

#[test]
fn lifecycle_assumptions_do_not_initialize_entry_scratch_or_results() {
    for role in [SolveStorageClass::MethodLocal, SolveStorageClass::Output] {
        let table = calls();
        let ty = real(&table);
        let error = TypedProgramEntry::construct(table, |b| {
            let slot = b.declare_slot(ty, role, SolveSlotAccess::ReadWrite, span(8))?;
            b.assume_slot_initialized(slot, span(9))
        })
        .unwrap_err();
        assert_eq!(
            error,
            SolveProgramConstructionError::UninitializedSlot {
                provenance: span(9)
            }
        );
    }
}
