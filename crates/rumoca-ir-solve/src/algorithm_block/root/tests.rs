use rumoca_ir_galec::package::GeneratedSubjectKind;

use super::*;

#[test]
fn algorithm_action_and_storage_relations_cannot_regress_to_parallel_truncation() {
    let source = include_str!("../root.rs");
    assert!(
        !source.contains(".zip("),
        "Solve Algorithm construction must retain action execution and storage binding facts atomically"
    );
    assert!(
        !source.contains("Span::DUMMY"),
        "Solve Algorithm construction must refuse source-free executable subjects"
    );
}

fn provenance() -> SemanticProvenance {
    SemanticProvenance::Generated(GeneratedSubjectKind::Declaration)
}

fn issue(
    allocator: &mut SolveLogicalStorageAllocator,
    storage: SolveAlgorithmBlockStorageClass,
    scalar_count: u64,
) -> SolveLogicalStorageRun {
    let run = allocator
        .prepare(storage, scalar_count, provenance())
        .expect("the fixture logical run is bounded");
    allocator.commit(run);
    run
}

#[test]
fn interleaved_storage_classes_keep_independent_class_local_bases() {
    let mut allocator = SolveLogicalStorageAllocator::new();

    let first_input = issue(&mut allocator, SolveAlgorithmBlockStorageClass::Input, 2);
    let constant = issue(&mut allocator, SolveAlgorithmBlockStorageClass::Constant, 5);
    let second_input = issue(&mut allocator, SolveAlgorithmBlockStorageClass::Input, 3);
    let totals = allocator.finish();

    assert_eq!(first_input.scalar_base(), 0);
    assert_eq!(constant.scalar_base(), 0);
    assert_eq!(second_input.scalar_base(), 2);
    assert_eq!(second_input.scalar_end(), 5);
    assert_eq!(
        totals.scalar_count(SolveAlgorithmBlockStorageClass::Input),
        5
    );
    assert_eq!(
        totals.scalar_count(SolveAlgorithmBlockStorageClass::Constant),
        5
    );
}

#[test]
fn failed_logical_run_preparation_does_not_advance_storage() {
    let mut allocator = SolveLogicalStorageAllocator::new();
    allocator.scalar_counts[SolveAlgorithmBlockStorageClass::Input.index()] = u64::MAX;

    let error = allocator
        .prepare(SolveAlgorithmBlockStorageClass::Input, 1, provenance())
        .expect_err("overflow must fail before storage mutation");

    assert!(matches!(
        error,
        SolveAlgorithmBlockConstructionError::LogicalStorageOverflow {
            storage: SolveStorageClass::Input,
            ..
        }
    ));
    assert_eq!(
        allocator
            .finish()
            .scalar_count(SolveAlgorithmBlockStorageClass::Input),
        u64::MAX
    );
}

#[test]
fn declaration_dimension_owners_use_the_branded_index_not_a_linear_scan() {
    let source = include_str!("../root.rs");

    assert!(source.contains(
        "declaration_indices: IndexMap<DeclarationId<'id>, PendingDeclarationLocation<'id>>"
    ));
    assert!(source.contains("self.declaration_indices.get(&owner).copied()"));
    assert!(!source.contains("self.declarations.iter().position("));
}

#[test]
fn method_locals_stay_owned_by_their_branded_region_and_never_enter_block_storage() {
    let source = include_str!("../root.rs");
    let scoped = include_str!("scoped_declaration.rs");

    assert!(source.contains("owner: LifecycleMethodId<'id>"));
    assert!(source.contains("pending_method_locals"));
    assert!(source.contains("expected_locals"));
    assert!(scoped.contains("SolveAlgorithmScopedLifetime::MethodInvocation"));
    assert!(!scoped.contains("Name"));
    assert!(!scoped.contains("Vec<SolveValue>"));
    assert!(!scoped.contains("scalar_values"));
}
