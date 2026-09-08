use super::*;
use crate::instance::equality_constraint::EqualityConstraintPrototype;
use crate::instance::test_semantic_catalog_projection;
use crate::instance::{ClassInstanceData, InstanceData, QualifiedName};

const RECORD: DefId = DefId(1);
const SLOT: DefId = DefId(3);
const FUNCTION: DefId = DefId(4);
const INPUT_A: DefId = DefId(5);
const INPUT_B: DefId = DefId(6);
const OUTPUT: DefId = DefId(7);
const REAL: DefId = DefId(8);

fn pending_exposure(
    record_instance: InstanceId,
    declaration_def_id: DefId,
) -> PendingEqualityConstraintOccurrenceExposure {
    PendingEqualityConstraintOccurrenceExposure {
        specialization_key: EqualityConstraintSpecializationKey {
            record_instance,
            occurrence_component_def_id: declaration_def_id,
            slot_def_id: SLOT,
            selected_function_def_id: FUNCTION,
        },
        exposure: EqualityConstraintExposure {
            prototype: EqualityConstraintPrototype {
                record_type_def_id: RECORD,
                slot_def_id: SLOT,
                selected_function_def_id: FUNCTION,
                occurrence_component_def_id: Some(declaration_def_id),
                input_def_ids: [INPUT_A, INPUT_B],
                input_type_def_ids: [RECORD, RECORD],
                output_def_id: OUTPUT,
                output_type_def_id: REAL,
                slot_declaration_span: Span::DUMMY,
                selected_function_declaration_span: Span::DUMMY,
                output_declaration_span: Span::DUMMY,
            },
            cardinality: EqualityConstraintCardinality::Vacuous,
        },
    }
}

fn add_record(overlay: &mut InstanceOverlay, name: &str, declaration_def_id: DefId) -> InstanceId {
    let record = overlay.alloc_id();
    let qualified_name = QualifiedName::from_ident(name);
    overlay
        .add_component(InstanceData {
            instance_id: record,
            declaration_def_id: Some(declaration_def_id),
            qualified_name: qualified_name.clone(),
            type_def_id: Some(RECORD),
            ..Default::default()
        })
        .expect("record fixture insertion");
    let class = overlay.alloc_id();
    overlay
        .add_class(ClassInstanceData {
            instance_id: class,
            owner_component_id: Some(record),
            class_def_id: Some(RECORD),
            qualified_name,
            ..Default::default()
        })
        .expect("class fixture insertion");
    overlay
        .register_overconstrained_record(pending_exposure(record, declaration_def_id))
        .expect("pending exposure fixture registration");
    record
}

fn prepare_effective_roots(overlay: &mut InstanceOverlay, records: &[InstanceId]) {
    let nominal = TypeId::new(10);
    overlay.type_ids_by_def_id.insert(RECORD, nominal);
    overlay.type_roots.insert(nominal, nominal);
    for record in records {
        overlay.components[record].type_id = nominal;
    }
}

#[test]
fn occurrence_token_rejects_compact_array_state() {
    let mut overlay = InstanceOverlay::new();
    let record = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: record,
            declaration_def_id: Some(INPUT_A),
            qualified_name: QualifiedName::from_ident("records"),
            dims: vec![2],
            type_def_id: Some(RECORD),
            ..Default::default()
        })
        .expect("array fixture insertion");
    assert_eq!(
        overlay.equality_constraint_record_occurrence(record),
        Err(EqualityConstraintOccurrenceError::ArrayRecordOccurrence(
            record
        )),
    );
    overlay.components[&record].dims.clear();
    overlay.components[&record].dims_expr = vec![crate::Subscript::Empty];
    assert_eq!(
        overlay.equality_constraint_record_occurrence(record),
        Err(EqualityConstraintOccurrenceError::ArrayRecordOccurrence(
            record
        )),
        "unevaluated compact dimensions are also non-scalar evidence",
    );
}

#[test]
fn effective_upgrade_is_atomic_for_invalid_shape() {
    let mut overlay = InstanceOverlay::new();
    let first = add_record(&mut overlay, "first", INPUT_A);
    let second = add_record(&mut overlay, "second", INPUT_B);
    overlay
        .finalize_overconstrained_record_owners()
        .expect("owner ancestry fixture finalizes");

    prepare_effective_roots(&mut overlay, &[first, second]);
    overlay.components[&second].dims = vec![-1];
    let before = overlay.overconstrained_construction_counts();
    assert!(
        matches!(
            overlay.finalize_effective_type_publication(test_semantic_catalog_projection()),
            Err(EffectiveTypePublicationError::InvalidComponentDescriptor {
                occurrence,
                ..
            }) if occurrence == second
        ),
        "same nominal DefId with a different array TypeId must fail",
    );
    assert_eq!(overlay.overconstrained_construction_counts(), before);
    assert_eq!(
        overlay.finalized_overconstrained().err(),
        Some(EqualityConstraintOccurrenceError::EffectiveTypeCatalogNotFinalized),
        "a partial upgrade cannot become readable",
    );

    overlay.components[&second].dims.clear();
    overlay
        .finalize_effective_type_publication(test_semantic_catalog_projection())
        .expect("the repaired complete transaction finalizes");
    let finalized = overlay
        .finalized_overconstrained()
        .expect("both private finalization states are complete");
    assert_eq!(
        finalized
            .exposure(second)
            .expect("second exposure")
            .effective_record_identity()
            .effective_type_id(),
        overlay.components[&second].type_id,
    );
}

#[test]
fn root_constructor_owns_deterministic_effective_identity_issuance() {
    let mut overlay = InstanceOverlay::new();
    let first = add_record(&mut overlay, "first", INPUT_A);
    let second = add_record(&mut overlay, "second", INPUT_B);
    overlay
        .finalize_overconstrained_record_owners()
        .expect("owner ancestry fixture finalizes");
    prepare_effective_roots(&mut overlay, &[first, second]);
    overlay
        .finalize_effective_type_publication(test_semantic_catalog_projection())
        .expect("root-owned publication succeeds");
    assert_eq!(overlay.components[&first].type_id, TypeId::new(11));
    assert_eq!(overlay.components[&second].type_id, TypeId::new(11));
    assert_eq!(overlay.effective_types.len(), 1);
    assert_eq!(
        overlay.type_roots.get(&TypeId::new(11)),
        Some(&TypeId::new(10)),
    );
}

#[test]
fn publication_api_exposes_no_caller_owned_identity_base_or_order() {
    let source = include_str!("../occurrence.rs");
    for forbidden in [
        "pub struct EffectiveTypePublicationCandidate",
        "nominal_type_count",
        "occurrences: impl IntoIterator",
        "finalize_effective_type_publication(\n        &mut self,\n        candidate",
    ] {
        assert!(
            !source.contains(forbidden),
            "effective identity authority escaped through `{forbidden}`",
        );
    }
    assert!(
        source.contains("candidate.effective_type_ids.get(&descriptor).copied()")
            && source.contains("!= Some(record_effective_type_id)"),
        "input exposure must replay the exact rank-zero effective TypeId",
    );
}
