use super::*;
use crate::{SolveIntegerDomain, SolveRealFormat, SolveScalarType};
use rumoca_core::SourceId;

type LocalSources = Result<Vec<SolveAlgorithmCodeRef>, SolveProgramConstructionError>;

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("algorithm_block.alg"),
        start,
        start + 1,
    )
}

fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

fn real() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile()))
}

fn source(kind: SolveAlgorithmCodeEntityKind, name: &str, start: usize) -> SolveAlgorithmCodeRef {
    SolveAlgorithmCodeRef::construct(kind, name, span(start))
}

fn slot(
    storage: SolveStorageClass,
    access: SolveSlotAccess,
    kind: SolveAlgorithmCodeEntityKind,
    name: &str,
    start: usize,
) -> SolveBlockSlotDeclaration {
    SolveBlockSlotDeclaration::construct(real(), storage, access, source(kind, name, start))
}

fn block_slots() -> Vec<SolveBlockSlotDeclaration> {
    vec![
        slot(
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            SolveAlgorithmCodeEntityKind::Interface,
            "u",
            1,
        ),
        slot(
            SolveStorageClass::PersistentState,
            SolveSlotAccess::ReadWrite,
            SolveAlgorithmCodeEntityKind::Protected,
            "x",
            2,
        ),
    ]
}

fn no_locals(_: &mut TypedProgramBuilder<'_>, _: &[ProgramSlot<'_>]) -> LocalSources {
    Ok(Vec::new())
}

fn one_local(builder: &mut TypedProgramBuilder<'_>, _: &[ProgramSlot<'_>]) -> LocalSources {
    builder.declare_slot(
        real(),
        SolveStorageClass::MethodLocal,
        SolveSlotAccess::ReadWrite,
        span(10),
    )?;
    Ok(vec![source(
        SolveAlgorithmCodeEntityKind::MethodLocal,
        "DoStep.tmp",
        10,
    )])
}

#[test]
fn three_methods_share_the_block_slot_prefix() {
    let block =
        SolveAlgorithmBlock::construct(profile(), block_slots(), no_locals, no_locals, one_local)
            .expect("checked block");
    assert_eq!(block.slots().len(), 2);
    let kinds = block.methods().map(SolveBlockMethod::kind);
    assert_eq!(
        kinds,
        [
            SolveBlockMethodKind::Startup,
            SolveBlockMethodKind::Recalibrate,
            SolveBlockMethodKind::DoStep,
        ]
    );
    for method in block.methods() {
        let declared = method.program().slots();
        for (index, expected) in block.slots().iter().enumerate() {
            assert_eq!(declared[index].id().index(), index);
            assert_eq!(declared[index].storage(), expected.storage());
            assert_eq!(declared[index].provenance(), expected.source().span());
        }
    }
    assert!(block.startup().local_sources().is_empty());
    assert_eq!(block.do_step().local_sources().len(), 1);
    assert_eq!(block.do_step().program().slots().len(), 3);
}

#[test]
fn block_table_rejects_method_local_and_previous_state() {
    for storage in [
        SolveStorageClass::MethodLocal,
        SolveStorageClass::PreviousState,
    ] {
        let mut slots = block_slots();
        slots.push(slot(
            storage,
            SolveSlotAccess::ReadWrite,
            SolveAlgorithmCodeEntityKind::Protected,
            "bad",
            3,
        ));
        assert_eq!(
            SolveAlgorithmBlock::construct(profile(), slots, no_locals, no_locals, no_locals),
            Err(
                SolveAlgorithmBlockConstructionError::UnsupportedBlockStorage {
                    provenance: span(3)
                }
            )
        );
    }
}

#[test]
fn method_cannot_declare_block_storage() {
    let persistent_in_method =
        |builder: &mut TypedProgramBuilder<'_>, _: &[ProgramSlot<'_>]| -> LocalSources {
            builder.declare_slot(
                real(),
                SolveStorageClass::PersistentState,
                SolveSlotAccess::ReadWrite,
                span(20),
            )?;
            Ok(Vec::new())
        };
    assert_eq!(
        SolveAlgorithmBlock::construct(
            profile(),
            block_slots(),
            no_locals,
            persistent_in_method,
            no_locals
        ),
        Err(
            SolveAlgorithmBlockConstructionError::MethodOwnedBlockStorage {
                method: SolveBlockMethodKind::Recalibrate,
                provenance: span(20),
            }
        )
    );
}

#[test]
fn local_sources_pair_one_to_one_with_locals() {
    let unsourced_local =
        |builder: &mut TypedProgramBuilder<'_>, _: &[ProgramSlot<'_>]| -> LocalSources {
            builder.declare_slot(
                real(),
                SolveStorageClass::MethodLocal,
                SolveSlotAccess::ReadWrite,
                span(30),
            )?;
            Ok(Vec::new())
        };
    assert_eq!(
        SolveAlgorithmBlock::construct(
            profile(),
            block_slots(),
            unsourced_local,
            no_locals,
            no_locals
        ),
        Err(SolveAlgorithmBlockConstructionError::LocalSourceCount {
            method: SolveBlockMethodKind::Startup,
        })
    );
}

#[test]
fn source_kinds_match_storage() {
    let local_named_as_block =
        |builder: &mut TypedProgramBuilder<'_>, _: &[ProgramSlot<'_>]| -> LocalSources {
            builder.declare_slot(
                real(),
                SolveStorageClass::MethodLocal,
                SolveSlotAccess::ReadWrite,
                span(40),
            )?;
            Ok(vec![source(
                SolveAlgorithmCodeEntityKind::Protected,
                "x2",
                40,
            )])
        };
    assert_eq!(
        SolveAlgorithmBlock::construct(
            profile(),
            block_slots(),
            no_locals,
            no_locals,
            local_named_as_block
        ),
        Err(SolveAlgorithmBlockConstructionError::SourceKindMismatch {
            provenance: span(40)
        })
    );

    let mut slots = block_slots();
    slots.push(slot(
        SolveStorageClass::Output,
        SolveSlotAccess::ReadWrite,
        SolveAlgorithmCodeEntityKind::MethodLocal,
        "y",
        41,
    ));
    assert_eq!(
        SolveAlgorithmBlock::construct(profile(), slots, no_locals, no_locals, no_locals),
        Err(SolveAlgorithmBlockConstructionError::SourceKindMismatch {
            provenance: span(41)
        })
    );
}

#[test]
fn sources_are_injective_across_methods() {
    assert_eq!(
        SolveAlgorithmBlock::construct(profile(), block_slots(), one_local, no_locals, one_local),
        Err(SolveAlgorithmBlockConstructionError::DuplicateSource {
            provenance: span(10)
        })
    );
}

#[test]
fn missing_provenance_is_reported_against_its_method() {
    let mut slots = block_slots();
    slots.push(SolveBlockSlotDeclaration::construct(
        real(),
        SolveStorageClass::Output,
        SolveSlotAccess::ReadWrite,
        SolveAlgorithmCodeRef::construct(SolveAlgorithmCodeEntityKind::Interface, "y", Span::DUMMY),
    ));
    assert_eq!(
        SolveAlgorithmBlock::construct(profile(), slots, no_locals, no_locals, no_locals),
        Err(SolveAlgorithmBlockConstructionError::Program {
            method: SolveBlockMethodKind::Startup,
            error: SolveProgramConstructionError::MissingProvenance,
        })
    );
}

#[test]
fn method_local_source_requires_the_declared_provenance() {
    for source_span in [Span::DUMMY, span(11)] {
        let mismatched =
            |builder: &mut TypedProgramBuilder<'_>, _: &[ProgramSlot<'_>]| -> LocalSources {
                builder.declare_slot(
                    real(),
                    SolveStorageClass::MethodLocal,
                    SolveSlotAccess::ReadWrite,
                    span(10),
                )?;
                Ok(vec![SolveAlgorithmCodeRef::construct(
                    SolveAlgorithmCodeEntityKind::MethodLocal,
                    "DoStep.tmp",
                    source_span,
                )])
            };
        assert_eq!(
            SolveAlgorithmBlock::construct(
                profile(),
                block_slots(),
                no_locals,
                no_locals,
                mismatched
            ),
            Err(
                SolveAlgorithmBlockConstructionError::LocalSourceProvenance {
                    method: SolveBlockMethodKind::DoStep,
                    provenance: span(10),
                }
            )
        );
    }
}

#[test]
fn block_storage_cannot_refine_a_function_or_statement() {
    for kind in [
        SolveAlgorithmCodeEntityKind::Function,
        SolveAlgorithmCodeEntityKind::Statement,
    ] {
        let slots = vec![slot(
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            kind,
            "u",
            50,
        )];
        assert_eq!(
            SolveAlgorithmBlock::construct(profile(), slots, no_locals, no_locals, no_locals),
            Err(SolveAlgorithmBlockConstructionError::SourceKindMismatch {
                provenance: span(50)
            })
        );
    }
}

#[test]
fn changing_provenance_cannot_duplicate_one_qualified_entity() {
    let slots = vec![
        slot(
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            SolveAlgorithmCodeEntityKind::Interface,
            "u",
            1,
        ),
        slot(
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            SolveAlgorithmCodeEntityKind::Interface,
            "u",
            2,
        ),
    ];
    assert_eq!(
        SolveAlgorithmBlock::construct(profile(), slots, no_locals, no_locals, no_locals),
        Err(SolveAlgorithmBlockConstructionError::DuplicateSource {
            provenance: span(2)
        })
    );
}

#[test]
fn distinct_declarations_and_locals_are_accepted() {
    let slots = vec![
        slot(
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            SolveAlgorithmCodeEntityKind::Interface,
            "u",
            1,
        ),
        slot(
            SolveStorageClass::PersistentState,
            SolveSlotAccess::ReadWrite,
            SolveAlgorithmCodeEntityKind::CompartmentMember,
            "filter.state",
            2,
        ),
        slot(
            SolveStorageClass::TunableParameter,
            SolveSlotAccess::ReadOnly,
            SolveAlgorithmCodeEntityKind::Protected,
            "gain",
            3,
        ),
    ];
    let startup_local =
        |builder: &mut TypedProgramBuilder<'_>, _: &[ProgramSlot<'_>]| -> LocalSources {
            builder.declare_slot(
                real(),
                SolveStorageClass::MethodLocal,
                SolveSlotAccess::ReadWrite,
                span(60),
            )?;
            Ok(vec![source(
                SolveAlgorithmCodeEntityKind::MethodLocal,
                "Startup.tmp",
                60,
            )])
        };
    let block =
        SolveAlgorithmBlock::construct(profile(), slots, startup_local, no_locals, one_local)
            .expect("distinct declarations and locals");
    assert_eq!(block.slots().len(), 3);
    assert_eq!(block.startup().local_sources().len(), 1);
    assert_eq!(block.do_step().local_sources().len(), 1);
    assert_eq!(
        block.do_step().local_sources()[0].identity(),
        (SolveAlgorithmCodeEntityKind::MethodLocal, "DoStep.tmp")
    );
}

#[test]
fn signal_status_slots_carry_derived_provenance() {
    let status = |kind: SolveAlgorithmCodeEntityKind| {
        SolveBlockSlotDeclaration::construct(
            SolveValueType::scalar(SolveScalarType::Boolean),
            SolveStorageClass::SignalStatus,
            SolveSlotAccess::ReadWrite,
            source(kind, "ErrorSignalStatus.NAN", 50),
        )
    };
    let construct =
        |slots| SolveAlgorithmBlock::construct(profile(), slots, no_locals, no_locals, no_locals);
    let mut slots = block_slots();
    slots.push(status(SolveAlgorithmCodeEntityKind::ErrorSignalStatus));
    assert!(construct(slots).is_ok());

    // A status bit is not a checked interface declaration.
    let mut slots = block_slots();
    slots.push(status(SolveAlgorithmCodeEntityKind::Interface));
    assert_eq!(
        construct(slots),
        Err(SolveAlgorithmBlockConstructionError::SourceKindMismatch {
            provenance: span(50)
        })
    );

    // Nor is declared state derived status.
    let mut slots = block_slots();
    slots.push(slot(
        SolveStorageClass::PersistentState,
        SolveSlotAccess::ReadWrite,
        SolveAlgorithmCodeEntityKind::ErrorSignalStatus,
        "z",
        51,
    ));
    assert_eq!(
        construct(slots),
        Err(SolveAlgorithmBlockConstructionError::SourceKindMismatch {
            provenance: span(51)
        })
    );
}

#[test]
fn declared_escapes_name_signal_status_slots() {
    let mut slots = block_slots();
    slots.push(SolveBlockSlotDeclaration::construct(
        SolveValueType::scalar(SolveScalarType::Boolean),
        SolveStorageClass::SignalStatus,
        SolveSlotAccess::ReadWrite,
        source(
            SolveAlgorithmCodeEntityKind::ErrorSignalStatus,
            "ErrorSignalStatus.NAN",
            50,
        ),
    ));
    let block =
        SolveAlgorithmBlock::construct(profile(), slots, no_locals, no_locals, no_locals).unwrap();
    let block = block
        .with_method_escapes(SolveBlockMethodKind::DoStep, [2, 2])
        .unwrap();
    assert_eq!(block.do_step().escapes(), [2]);
    assert!(block.startup().escapes().is_empty());
    assert_eq!(
        block.with_method_escapes(SolveBlockMethodKind::Startup, [1]),
        Err(
            SolveAlgorithmBlockConstructionError::EscapeNotSignalStatus {
                method: SolveBlockMethodKind::Startup,
                slot: 1
            }
        )
    );
}

/// A block with two signal status slots and one function whose body declares
/// its two status bits first, built with the status bindings `bindings`
/// derives from its function table.
fn block_with_one_function(
    bindings: impl FnOnce(&SolvePureCallTable) -> Vec<SolveStatusBinding>,
) -> Result<SolveAlgorithmBlock, SolveAlgorithmBlockConstructionError> {
    use crate::{SolvePureCallIdentity, SolvePureCallOutput, SolveValue};
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let status = |name: &str, start| {
        SolveBlockSlotDeclaration::construct(
            boolean.clone(),
            SolveStorageClass::SignalStatus,
            SolveSlotAccess::ReadWrite,
            source(SolveAlgorithmCodeEntityKind::ErrorSignalStatus, name, start),
        )
    };
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::MIN),
            vec![real()],
            vec![
                SolvePureCallOutput::result(boolean.clone()),
                SolvePureCallOutput::result(boolean.clone()),
            ],
            span(60),
            |builder, _, outputs| {
                let clear = builder.constant(SolveValue::boolean(false), span(61))?;
                for &output in outputs {
                    let local = builder.declare_slot(
                        boolean.clone(),
                        SolveStorageClass::MethodLocal,
                        SolveSlotAccess::ReadWrite,
                        span(62),
                    )?;
                    builder.store(local, clear, span(63))?;
                    let value = builder.load(local, span(64))?;
                    builder.store(output, value, span(65))?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let bindings = bindings(&table);
    let mut slots = block_slots();
    slots.push(status("ErrorSignalStatus.OVERFLOW", 50));
    slots.push(status("ErrorSignalStatus.NAN", 51));
    SolveAlgorithmBlock::construct_with_functions(
        profile(),
        slots,
        SolveBlockFunctions {
            table,
            sources: vec![source(SolveAlgorithmCodeEntityKind::Function, "f", 60)],
            status_bindings: bindings,
        },
        no_locals,
        no_locals,
        no_locals,
    )
}

/// The status slot ids of the only owner: inputs (1), outputs (2), then the
/// two status locals.
fn owner_slots(table: &SolvePureCallTable) -> (crate::SolvePureCallOwnerId, Vec<SolveSlotId>) {
    let owner = &table.owners()[0];
    (
        owner.id(),
        owner
            .body()
            .slots()
            .iter()
            .map(crate::SolveSlot::id)
            .collect(),
    )
}

/// Regression: a binding must cover every function owner
/// once and name exactly its canonical status channels in signal order.
#[test]
fn status_bindings_must_name_each_owners_canonical_channels() {
    let bind = |pick: fn(&[SolveSlotId]) -> Vec<SolveSlotId>| {
        move |table: &SolvePureCallTable| {
            let (owner, slots) = owner_slots(table);
            vec![SolveStatusBinding::new(owner, pick(&slots))]
        }
    };
    assert!(block_with_one_function(bind(|s| vec![s[3], s[4]])).is_ok());
    let refused = |result: Result<SolveAlgorithmBlock, SolveAlgorithmBlockConstructionError>| {
        matches!(
            result,
            Err(SolveAlgorithmBlockConstructionError::StatusBinding { .. })
        )
    };
    // Omitted owner.
    assert!(refused(block_with_one_function(|_| Vec::new())));
    // An owner outside the table.
    let unknown = block_with_one_function(|table| {
        let (_, slots) = owner_slots(table);
        vec![SolveStatusBinding::new(
            crate::SolvePureCallOwnerId::LIFECYCLE_METHOD,
            vec![slots[3], slots[4]],
        )]
    });
    assert!(matches!(
        unknown,
        Err(SolveAlgorithmBlockConstructionError::StatusBindingUnknownOwner { .. })
    ));
    assert!(
        unknown
            .unwrap_err()
            .to_string()
            .contains("function table lacks")
    );
    // Repeated slot, permuted channels, and another local (an output slot).
    assert!(refused(block_with_one_function(bind(|s| vec![s[3], s[3]]))));
    assert!(refused(block_with_one_function(bind(|s| vec![s[4], s[3]]))));
    assert!(refused(block_with_one_function(bind(|s| vec![s[1], s[4]]))));
    // Too few channels.
    assert!(refused(block_with_one_function(bind(|s| vec![s[3]]))));
    // The same owner bound twice.
    assert!(refused(block_with_one_function(|table| {
        let (owner, slots) = owner_slots(table);
        vec![
            SolveStatusBinding::new(owner, vec![slots[3], slots[4]]),
            SolveStatusBinding::new(owner, vec![slots[3], slots[4]]),
        ]
    })));
}
