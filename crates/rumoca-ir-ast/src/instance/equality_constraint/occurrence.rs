use super::{
    EqualityConstraintCardinality, EqualityConstraintDeclarationIndex, EqualityConstraintExposure,
};
use crate::ClassTree;
use crate::instance::{
    FastIndexMap, InstanceData, InstanceOverlay, QualifiedName, SemanticCatalogProjection,
};
use indexmap::IndexSet;
use rumoca_core::{DefId, EffectiveType, InstanceId, Span, TypeId};
use std::collections::HashMap;

mod errors;
pub use errors::{EffectiveTypePublicationError, EqualityConstraintOccurrenceError};

#[cfg(test)]
mod tests;

/// Construction-issued identity of one exact specialized callable exposure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EqualityConstraintSpecializationKey {
    record_instance: InstanceId,
    occurrence_component_def_id: DefId,
    slot_def_id: DefId,
    selected_function_def_id: DefId,
}

impl EqualityConstraintSpecializationKey {
    pub fn record_instance(self) -> InstanceId {
        self.record_instance
    }

    pub fn occurrence_component_def_id(self) -> DefId {
        self.occurrence_component_def_id
    }

    pub fn slot_def_id(self) -> DefId {
        self.slot_def_id
    }

    pub fn selected_function_def_id(self) -> DefId {
        self.selected_function_def_id
    }
}

/// Opaque proof that an exact allocated occurrence is a non-array record.
///
/// Only [`InstanceOverlay::equality_constraint_record_occurrence`] issues this
/// token. Completion consumes it rather than accepting a caller-owned identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::instance) struct EqualityConstraintRecordOccurrence {
    instance_id: InstanceId,
    declaration_def_id: DefId,
    record_type_def_id: DefId,
}

impl EqualityConstraintRecordOccurrence {
    pub(super) fn instance_id(self) -> InstanceId {
        self.instance_id
    }

    pub(super) fn declaration_def_id(self) -> DefId {
        self.declaration_def_id
    }

    pub(super) fn record_type_def_id(self) -> DefId {
        self.record_type_def_id
    }
}

/// Declaration-complete exposure awaiting post-typecheck effective identity.
///
/// The exact prototype retained here includes both input declaration and
/// declared-type identities. This pending proof is never available through the
/// finalized consumer guard.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::instance) struct PendingEqualityConstraintOccurrenceExposure {
    specialization_key: EqualityConstraintSpecializationKey,
    exposure: EqualityConstraintExposure,
}

impl PendingEqualityConstraintOccurrenceExposure {
    fn specialization_key(self) -> EqualityConstraintSpecializationKey {
        self.specialization_key
    }

    fn cardinality(self) -> EqualityConstraintCardinality {
        self.exposure.cardinality()
    }
}

/// Exact effective identity of the non-array record occurrence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EqualityConstraintEffectiveRecordIdentity {
    record_type_def_id: DefId,
    effective_type_id: TypeId,
}

impl EqualityConstraintEffectiveRecordIdentity {
    pub fn record_type_def_id(self) -> DefId {
        self.record_type_def_id
    }

    pub fn effective_type_id(self) -> TypeId {
        self.effective_type_id
    }
}

/// Checked equalityConstraint proof bound to one exact effective occurrence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EqualityConstraintOccurrenceExposure {
    specialization_key: EqualityConstraintSpecializationKey,
    effective_record_identity: EqualityConstraintEffectiveRecordIdentity,
    exposure: EqualityConstraintExposure,
}

/// One finalized overconstrained record occurrence and its exact structured
/// instance path. This projection can only be borrowed from the finalized
/// catalog that issued it.
#[derive(Clone, Copy)]
pub struct FinalizedOverconstrainedRecord<'catalog> {
    instance_id: InstanceId,
    qualified_name: &'catalog QualifiedName,
    exposure: EqualityConstraintOccurrenceExposure,
}

/// Total overconstrained classification for one component occurrence owned by
/// the finalized catalog. Absence from the catalog is kept distinct from an
/// ordinary (non-overconstrained) occurrence.
#[derive(Clone, Copy)]
pub enum FinalizedOverconstrainedComponent<'catalog> {
    Foreign,
    Ordinary,
    Record(FinalizedOverconstrainedRecord<'catalog>),
}

impl<'catalog> FinalizedOverconstrainedRecord<'catalog> {
    pub fn instance_id(self) -> InstanceId {
        self.instance_id
    }

    pub fn qualified_name(self) -> &'catalog QualifiedName {
        self.qualified_name
    }

    pub fn exposure(self) -> EqualityConstraintOccurrenceExposure {
        self.exposure
    }
}

#[derive(Debug, Clone)]
pub(in crate::instance) struct FinalizedOverconstrainedOccurrence {
    qualified_name: QualifiedName,
    exposure: EqualityConstraintOccurrenceExposure,
}

/// Detached candidate for one atomic effective-type publication.
///
/// Construction walks the overlay's owning component order, derives the first
/// free TypeId from its issued root catalog, and owns descriptor interning.
/// Callers cannot supply an identity base, reorder occurrences, or forge fields.
///
/// This candidate allocates only one occurrence-to-TypeId entry per component
/// and one descriptor/root entry per unique effective type. It never clones
/// classes, equations, expressions, or component payloads.
struct EffectiveTypePublicationCandidate {
    component_type_ids: FastIndexMap<InstanceId, TypeId>,
    effective_type_ids: HashMap<EffectiveType, TypeId>,
    effective_types: FastIndexMap<TypeId, EffectiveType>,
    effective_type_roots: FastIndexMap<TypeId, TypeId>,
    enumeration_types: IndexSet<TypeId>,
}

impl EffectiveTypePublicationCandidate {
    fn construct(overlay: &InstanceOverlay) -> Result<Self, EffectiveTypePublicationError> {
        let mut component_type_ids = FastIndexMap::default();
        let mut effective_types = FastIndexMap::default();
        let mut effective_type_roots = FastIndexMap::default();
        let mut enumeration_types = IndexSet::new();
        let mut interned = HashMap::<EffectiveType, TypeId>::new();
        let first_effective_index = next_effective_type_index(overlay)?;
        for (occurrence, component) in &overlay.components {
            let canonical = overlay
                .type_roots
                .get(&component.type_id)
                .copied()
                .filter(|canonical| overlay.type_roots.get(canonical) == Some(canonical))
                .ok_or(EffectiveTypePublicationError::InvalidComponentDescriptor {
                    occurrence: *occurrence,
                    reason: "its nominal type has no complete acyclic canonical root".into(),
                })?;
            let descriptor =
                EffectiveType::new(component.type_id, canonical, component.dims.clone()).map_err(
                    |error| EffectiveTypePublicationError::InvalidComponentDescriptor {
                        occurrence: *occurrence,
                        reason: error.to_string(),
                    },
                )?;
            let effective_type_id = if let Some(type_id) = interned.get(&descriptor).copied() {
                type_id
            } else {
                let index = first_effective_index
                    .checked_add(effective_types.len())
                    .and_then(|index| u32::try_from(index).ok())
                    .filter(|index| *index != TypeId::UNKNOWN.index())
                    .ok_or(
                        EffectiveTypePublicationError::EffectiveIdentityArenaExhausted(*occurrence),
                    )?;
                let type_id = TypeId::new(index);
                interned.insert(descriptor.clone(), type_id);
                effective_type_roots.insert(type_id, descriptor.canonical_type());
                effective_types.insert(type_id, descriptor);
                type_id
            };
            if overlay.enumeration_type_roots.contains(&canonical) {
                enumeration_types.insert(effective_type_id);
            }
            component_type_ids.insert(*occurrence, effective_type_id);
        }
        Ok(Self {
            component_type_ids,
            effective_type_ids: interned,
            effective_types,
            effective_type_roots,
            enumeration_types,
        })
    }
}

fn next_effective_type_index(
    overlay: &InstanceOverlay,
) -> Result<usize, EffectiveTypePublicationError> {
    let Some(maximum) = overlay
        .type_roots
        .keys()
        .map(|type_id| type_id.index())
        .max()
    else {
        return Ok(0);
    };
    maximum
        .checked_add(1)
        .and_then(|index| usize::try_from(index).ok())
        .filter(|index| *index < TypeId::UNKNOWN.index() as usize)
        .ok_or(EffectiveTypePublicationError::InvalidCatalogIdentity {
            type_id: TypeId::new(maximum),
            reason: "the issued type-root catalog exhausts the TypeId arena",
        })
}

struct CheckedEffectiveTypePublication {
    candidate: EffectiveTypePublicationCandidate,
    overconstrained_records: FastIndexMap<InstanceId, FinalizedOverconstrainedOccurrence>,
    semantic_catalogs: SemanticCatalogProjection,
}

impl EqualityConstraintOccurrenceExposure {
    pub fn specialization_key(self) -> EqualityConstraintSpecializationKey {
        self.specialization_key
    }

    pub fn effective_record_identity(self) -> EqualityConstraintEffectiveRecordIdentity {
        self.effective_record_identity
    }

    pub fn record_type_def_id(self) -> DefId {
        self.effective_record_identity.record_type_def_id()
    }

    pub fn slot_def_id(self) -> DefId {
        self.exposure.slot_def_id()
    }

    pub fn selected_function_def_id(self) -> DefId {
        self.exposure.selected_function_def_id()
    }

    pub fn input_def_ids(self) -> [DefId; 2] {
        self.exposure.prototype.input_def_ids()
    }

    pub fn input_type_def_ids(self) -> [DefId; 2] {
        self.exposure.prototype.input_type_def_ids()
    }

    pub fn output_def_id(self) -> DefId {
        self.exposure.output_def_id()
    }

    pub fn output_type_def_id(self) -> DefId {
        self.exposure.output_type_def_id()
    }

    pub fn slot_declaration_span(self) -> Span {
        self.exposure.slot_declaration_span()
    }

    pub fn selected_function_declaration_span(self) -> Span {
        self.exposure.selected_function_declaration_span()
    }

    pub fn output_declaration_span(self) -> Span {
        self.exposure.output_declaration_span()
    }

    pub fn cardinality(self) -> EqualityConstraintCardinality {
        self.exposure.cardinality()
    }
}

pub(super) fn bind_equality_constraint_occurrence(
    occurrence: EqualityConstraintRecordOccurrence,
    exposure: EqualityConstraintExposure,
) -> PendingEqualityConstraintOccurrenceExposure {
    PendingEqualityConstraintOccurrenceExposure {
        specialization_key: EqualityConstraintSpecializationKey {
            record_instance: occurrence.instance_id,
            occurrence_component_def_id: occurrence.declaration_def_id,
            slot_def_id: exposure.slot_def_id(),
            selected_function_def_id: exposure.selected_function_def_id(),
        },
        exposure,
    }
}

impl InstanceOverlay {
    /// Issue an occurrence token only for an exact allocated scalar record.
    fn equality_constraint_record_occurrence(
        &self,
        record_instance: InstanceId,
    ) -> Result<EqualityConstraintRecordOccurrence, EqualityConstraintOccurrenceError> {
        if record_instance.is_unset() {
            return Err(EqualityConstraintOccurrenceError::UnsetRecordIdentity);
        }
        if record_instance.index() > self.next_id {
            return Err(
                EqualityConstraintOccurrenceError::UnallocatedRecordIdentity(record_instance),
            );
        }
        let record = self.components.get(&record_instance).ok_or(
            EqualityConstraintOccurrenceError::MissingRecordOccurrence(record_instance),
        )?;
        let declaration_def_id = record.declaration_def_id.ok_or(
            EqualityConstraintOccurrenceError::MissingRecordDeclarationIdentity(record_instance),
        )?;
        let record_type_def_id = record
            .type_def_id
            .ok_or(EqualityConstraintOccurrenceError::MissingRecordTypeIdentity(record_instance))?;
        validate_record_occurrence_shape(record_instance, record)?;
        Ok(EqualityConstraintRecordOccurrence {
            instance_id: record_instance,
            declaration_def_id,
            record_type_def_id,
        })
    }

    /// Atomically prove and register one exact occurrence exposure.
    ///
    /// No occurrence token or pending proof crosses this root operation. The
    /// selected slot is replayed against the occurrence-owned override map
    /// before the tree-owned prototype is completed and committed.
    pub fn construct_and_register_equality_constraint_occurrence(
        &mut self,
        declarations: &EqualityConstraintDeclarationIndex,
        tree: &ClassTree,
        record_instance: InstanceId,
    ) -> Result<Option<EqualityConstraintCardinality>, EqualityConstraintOccurrenceError> {
        if self.overconstrained_record_owners_finalized
            || self.overconstrained_effective_types_finalized
        {
            return Err(EqualityConstraintOccurrenceError::ExposureCatalogAlreadyFinalized);
        }
        let occurrence = self.equality_constraint_record_occurrence(record_instance)?;
        let record = self.components.get(&record_instance).ok_or(
            EqualityConstraintOccurrenceError::MissingRecordOccurrence(record_instance),
        )?;
        let Some(selection) = declarations
            .prove_equality_constraint_occurrence_selection(
                tree,
                occurrence,
                &record.class_overrides,
            )
            .map_err(EqualityConstraintOccurrenceError::InvalidExposure)?
        else {
            return Ok(None);
        };
        let prototype = declarations
            .check_equality_constraint_prototype(tree, selection)
            .map_err(EqualityConstraintOccurrenceError::InvalidExposure)?;
        let pending = declarations
            .complete_equality_constraint_occurrence(tree, prototype, occurrence)
            .map_err(EqualityConstraintOccurrenceError::InvalidExposure)?;
        let cardinality = pending.cardinality();
        self.register_overconstrained_record(pending)?;
        Ok(Some(cardinality))
    }

    /// Non-semantic transaction inventory for rollback tests.
    pub fn overconstrained_construction_counts(&self) -> (usize, usize) {
        (
            self.pending_overconstrained_records.len() + self.overconstrained_records.len(),
            self.overconstrained_record_owners.len(),
        )
    }

    /// Register one pending proof. Its occurrence is derived from the proof.
    fn register_overconstrained_record(
        &mut self,
        exposure: PendingEqualityConstraintOccurrenceExposure,
    ) -> Result<(), EqualityConstraintOccurrenceError> {
        if self.overconstrained_record_owners_finalized
            || self.overconstrained_effective_types_finalized
        {
            return Err(EqualityConstraintOccurrenceError::ExposureCatalogAlreadyFinalized);
        }
        let specialization = exposure.specialization_key();
        let occurrence =
            self.equality_constraint_record_occurrence(specialization.record_instance())?;
        if occurrence.record_type_def_id() != exposure.exposure.record_type_def_id() {
            return Err(EqualityConstraintOccurrenceError::RecordTypeMismatch {
                occurrence: occurrence.instance_id(),
                actual: Some(occurrence.record_type_def_id()),
                expected: exposure.exposure.record_type_def_id(),
            });
        }
        if occurrence.declaration_def_id() != specialization.occurrence_component_def_id() {
            return Err(
                EqualityConstraintOccurrenceError::RecordDeclarationMismatch {
                    occurrence: occurrence.instance_id(),
                    actual: Some(occurrence.declaration_def_id()),
                    expected: specialization.occurrence_component_def_id(),
                },
            );
        }
        let record_instance = occurrence.instance_id();
        if self
            .pending_overconstrained_records
            .contains_key(&record_instance)
            || self.overconstrained_records.contains_key(&record_instance)
        {
            return Err(EqualityConstraintOccurrenceError::DuplicateRecordExposure(
                record_instance,
            ));
        }
        self.pending_overconstrained_records
            .insert(record_instance, exposure);
        Ok(())
    }

    /// Atomically derive every primitive's innermost overconstrained owner.
    pub fn finalize_overconstrained_record_owners(
        &mut self,
    ) -> Result<(), EqualityConstraintOccurrenceError> {
        if self.overconstrained_record_owners_finalized {
            return Err(EqualityConstraintOccurrenceError::OwnerCatalogAlreadyFinalized);
        }
        let classes_by_owner = self.classes_by_owner_component();
        self.validate_overconstrained_record_classes(&classes_by_owner)?;
        let mut owners = FastIndexMap::default();
        if self.pending_overconstrained_records.is_empty() {
            self.overconstrained_record_owners = owners;
            self.overconstrained_record_owners_finalized = true;
            return Ok(());
        }
        for component in self
            .components
            .values()
            .filter(|component| component.is_primitive)
        {
            if let Some(record) = self.nearest_overconstrained_record(component.instance_id)? {
                owners.insert(component.instance_id, record);
            }
        }
        self.overconstrained_record_owners = owners;
        self.overconstrained_record_owners_finalized = true;
        Ok(())
    }

    /// Atomically publish exact component effective identities and every
    /// equalityConstraint occurrence derived from the same detached candidate.
    pub fn finalize_effective_type_publication(
        &mut self,
        semantic_catalogs: SemanticCatalogProjection,
    ) -> Result<(), EffectiveTypePublicationError> {
        self.require_effective_publication_open()?;
        let candidate = EffectiveTypePublicationCandidate::construct(self)?;
        let checked = self.check_effective_type_publication(candidate, semantic_catalogs)?;
        self.publish_effective_type_publication(checked);
        Ok(())
    }

    fn require_effective_publication_open(&self) -> Result<(), EffectiveTypePublicationError> {
        if !self.overconstrained_record_owners_finalized {
            return Err(EqualityConstraintOccurrenceError::OwnerCatalogNotFinalized.into());
        }
        if self.overconstrained_effective_types_finalized {
            return Err(
                EqualityConstraintOccurrenceError::EffectiveTypeCatalogAlreadyFinalized.into(),
            );
        }
        if self.semantic_catalogs.is_some() {
            return Err(EffectiveTypePublicationError::InvalidCatalogIdentity {
                type_id: TypeId::UNKNOWN,
                reason: "the overlay already contains checked semantic catalogs",
            });
        }
        if let Some(type_id) = self.effective_types.keys().next().copied() {
            return Err(EffectiveTypePublicationError::InvalidCatalogIdentity {
                type_id,
                reason: "the overlay already contains an unpublished effective catalog",
            });
        }
        if let Some(type_id) = self.enumeration_types.iter().next().copied() {
            return Err(EffectiveTypePublicationError::InvalidCatalogIdentity {
                type_id,
                reason: "the overlay already contains unpublished enumeration identities",
            });
        }
        Ok(())
    }

    fn check_effective_type_publication(
        &self,
        candidate: EffectiveTypePublicationCandidate,
        semantic_catalogs: SemanticCatalogProjection,
    ) -> Result<CheckedEffectiveTypePublication, EffectiveTypePublicationError> {
        let mut finalized = FastIndexMap::default();
        for (record_instance, pending) in &self.pending_overconstrained_records {
            let identity =
                self.check_effective_record_identity(*record_instance, pending, &candidate)?;
            let record = self.components.get(record_instance).ok_or(
                EqualityConstraintOccurrenceError::MissingRecordOccurrence(*record_instance),
            )?;
            finalized.insert(
                *record_instance,
                FinalizedOverconstrainedOccurrence {
                    qualified_name: record.qualified_name.clone(),
                    exposure: EqualityConstraintOccurrenceExposure {
                        specialization_key: pending.specialization_key,
                        effective_record_identity: identity,
                        exposure: pending.exposure,
                    },
                },
            );
        }
        Ok(CheckedEffectiveTypePublication {
            candidate,
            overconstrained_records: finalized,
            semantic_catalogs,
        })
    }

    fn publish_effective_type_publication(&mut self, checked: CheckedEffectiveTypePublication) {
        let CheckedEffectiveTypePublication {
            candidate,
            overconstrained_records,
            semantic_catalogs,
        } = checked;
        for (component, effective_type_id) in self
            .components
            .values_mut()
            .zip(candidate.component_type_ids.values().copied())
        {
            component.type_id = effective_type_id;
        }
        self.type_roots.extend(candidate.effective_type_roots);
        self.effective_types = candidate.effective_types;
        self.enumeration_types = candidate.enumeration_types;
        self.overconstrained_records = overconstrained_records;
        self.semantic_catalogs = Some(semantic_catalogs);
        self.pending_overconstrained_records.clear();
        self.overconstrained_effective_types_finalized = true;
    }

    fn check_effective_record_identity(
        &self,
        record_instance: InstanceId,
        pending: &PendingEqualityConstraintOccurrenceExposure,
        candidate: &EffectiveTypePublicationCandidate,
    ) -> Result<EqualityConstraintEffectiveRecordIdentity, EqualityConstraintOccurrenceError> {
        let record = self.components.get(&record_instance).ok_or(
            EqualityConstraintOccurrenceError::MissingRecordOccurrence(record_instance),
        )?;
        validate_record_occurrence_shape(record_instance, record)?;
        self.check_pending_occurrence_identity(record_instance, record, pending)?;
        let effective_type_id = candidate
            .component_type_ids
            .get(&record_instance)
            .copied()
            .ok_or(
                EqualityConstraintOccurrenceError::MissingEffectiveRecordIdentity {
                    occurrence: record_instance,
                    type_id: TypeId::UNKNOWN,
                },
            )?;
        let expected_nominal = self
            .type_ids_by_def_id
            .get(&pending.exposure.record_type_def_id())
            .copied()
            .ok_or(
                EqualityConstraintOccurrenceError::EffectiveRecordCertificateNotImplemented(
                    record_instance,
                ),
            )?;
        if record.type_id != expected_nominal {
            return Err(
                EqualityConstraintOccurrenceError::EffectiveRecordNominalMismatch {
                    occurrence: record_instance,
                    actual_nominal: record.type_id,
                    expected_nominal,
                },
            );
        }
        self.check_input_effective_identities(
            record_instance,
            pending,
            effective_type_id,
            candidate,
        )?;
        Ok(EqualityConstraintEffectiveRecordIdentity {
            record_type_def_id: pending.exposure.record_type_def_id(),
            effective_type_id,
        })
    }

    fn check_pending_occurrence_identity(
        &self,
        record_instance: InstanceId,
        record: &InstanceData,
        pending: &PendingEqualityConstraintOccurrenceExposure,
    ) -> Result<(), EqualityConstraintOccurrenceError> {
        let specialization = pending.specialization_key;
        if specialization.record_instance() != record_instance {
            return Err(
                EqualityConstraintOccurrenceError::RecordOccurrenceMismatch {
                    catalog_key: record_instance,
                    proof_occurrence: specialization.record_instance(),
                },
            );
        }
        if record.declaration_def_id != Some(specialization.occurrence_component_def_id()) {
            return Err(
                EqualityConstraintOccurrenceError::RecordDeclarationMismatch {
                    occurrence: record_instance,
                    actual: record.declaration_def_id,
                    expected: specialization.occurrence_component_def_id(),
                },
            );
        }
        if record.type_def_id != Some(pending.exposure.record_type_def_id()) {
            return Err(EqualityConstraintOccurrenceError::RecordTypeMismatch {
                occurrence: record_instance,
                actual: record.type_def_id,
                expected: pending.exposure.record_type_def_id(),
            });
        }
        Ok(())
    }

    fn check_input_effective_identities(
        &self,
        occurrence: InstanceId,
        pending: &PendingEqualityConstraintOccurrenceExposure,
        record_effective_type_id: TypeId,
        candidate: &EffectiveTypePublicationCandidate,
    ) -> Result<(), EqualityConstraintOccurrenceError> {
        let input_def_ids = pending.exposure.prototype.input_def_ids();
        let input_type_def_ids = pending.exposure.prototype.input_type_def_ids();
        for (input_index, (input_def_id, input_type_def_id)) in input_def_ids
            .into_iter()
            .zip(input_type_def_ids)
            .enumerate()
        {
            if input_type_def_id != pending.exposure.record_type_def_id() {
                return Err(
                    EqualityConstraintOccurrenceError::InputEffectiveTypeMismatch {
                        occurrence,
                        input_index,
                        input_def_id,
                    },
                );
            }
            let input_nominal = self
                .type_ids_by_def_id
                .get(&input_type_def_id)
                .copied()
                .ok_or(
                EqualityConstraintOccurrenceError::InputEffectiveTypeCertificateNotImplemented {
                    occurrence,
                    input_index,
                    input_def_id,
                },
            )?;
            let input_canonical = self
                .type_roots
                .get(&input_nominal)
                .copied()
                .filter(|canonical| self.type_roots.get(canonical) == Some(canonical))
                .ok_or(
                    EqualityConstraintOccurrenceError::InputEffectiveTypeCertificateNotImplemented {
                        occurrence,
                        input_index,
                        input_def_id,
                    },
                )?;
            let descriptor =
                EffectiveType::new(input_nominal, input_canonical, []).map_err(|_| {
                    EqualityConstraintOccurrenceError::InputEffectiveTypeCertificateNotImplemented {
                        occurrence,
                        input_index,
                        input_def_id,
                    }
                })?;
            if candidate.effective_type_ids.get(&descriptor).copied()
                != Some(record_effective_type_id)
            {
                return Err(
                    EqualityConstraintOccurrenceError::InputEffectiveTypeMismatch {
                        occurrence,
                        input_index,
                        input_def_id,
                    },
                );
            }
        }
        Ok(())
    }

    fn classes_by_owner_component(&self) -> std::collections::HashMap<InstanceId, Vec<InstanceId>> {
        let mut classes_by_owner = std::collections::HashMap::new();
        for class in self.classes.values() {
            if let Some(owner) = class.owner_component_id {
                classes_by_owner
                    .entry(owner)
                    .or_insert_with(Vec::new)
                    .push(class.instance_id);
            }
        }
        classes_by_owner
    }

    fn validate_overconstrained_record_classes(
        &self,
        classes_by_owner: &std::collections::HashMap<InstanceId, Vec<InstanceId>>,
    ) -> Result<(), EqualityConstraintOccurrenceError> {
        for (record_instance, occurrence) in &self.pending_overconstrained_records {
            let classes = classes_by_owner
                .get(record_instance)
                .map(Vec::as_slice)
                .unwrap_or_default();
            let [class_instance] = classes else {
                return Err(Self::record_class_count_error(*record_instance, classes));
            };
            self.validate_overconstrained_record_class(
                *record_instance,
                *class_instance,
                occurrence,
            )?;
        }
        Ok(())
    }

    fn record_class_count_error(
        record_instance: InstanceId,
        classes: &[InstanceId],
    ) -> EqualityConstraintOccurrenceError {
        if classes.is_empty() {
            EqualityConstraintOccurrenceError::MissingRecordClass(record_instance)
        } else {
            EqualityConstraintOccurrenceError::MultipleRecordClasses {
                record: record_instance,
                count: classes.len(),
            }
        }
    }

    fn validate_overconstrained_record_class(
        &self,
        record_instance: InstanceId,
        class_instance: InstanceId,
        occurrence: &PendingEqualityConstraintOccurrenceExposure,
    ) -> Result<(), EqualityConstraintOccurrenceError> {
        let record = self.components.get(&record_instance).ok_or(
            EqualityConstraintOccurrenceError::MissingRecordOccurrence(record_instance),
        )?;
        let class = self.classes.get(&class_instance).ok_or(
            EqualityConstraintOccurrenceError::MissingOwnerClass {
                class: class_instance,
                descendant: record_instance,
            },
        )?;
        if class.qualified_name != record.qualified_name
            || class.class_def_id != Some(occurrence.exposure.record_type_def_id())
        {
            return Err(
                EqualityConstraintOccurrenceError::RecordClassEvidenceMismatch {
                    record: record_instance,
                    class: class_instance,
                },
            );
        }
        Ok(())
    }

    fn nearest_overconstrained_record(
        &self,
        descendant: InstanceId,
    ) -> Result<Option<InstanceId>, EqualityConstraintOccurrenceError> {
        let mut current = descendant;
        let mut visited_components = std::collections::HashSet::new();
        let mut visited_classes = std::collections::HashSet::new();
        loop {
            if !visited_components.insert(current) {
                return Err(EqualityConstraintOccurrenceError::CyclicInstanceAncestry(
                    current,
                ));
            }
            let component = self
                .components
                .get(&current)
                .ok_or(EqualityConstraintOccurrenceError::MissingDescendantOccurrence(current))?;
            let Some(owner_class_id) = component.owner_class_id else {
                return Ok(None);
            };
            if !visited_classes.insert(owner_class_id) {
                return Err(EqualityConstraintOccurrenceError::CyclicInstanceAncestry(
                    owner_class_id,
                ));
            }
            let owner_class = self.classes.get(&owner_class_id).ok_or(
                EqualityConstraintOccurrenceError::MissingOwnerClass {
                    class: owner_class_id,
                    descendant,
                },
            )?;
            let Some(owner_component) = owner_class.owner_component_id else {
                return Ok(None);
            };
            if self
                .pending_overconstrained_records
                .contains_key(&owner_component)
            {
                return Ok(Some(owner_component));
            }
            current = owner_component;
        }
    }

    /// Acquire read access only after both one-shot proofs are complete.
    pub fn finalized_overconstrained(
        &self,
    ) -> Result<FinalizedOverconstrainedCatalog<'_>, EqualityConstraintOccurrenceError> {
        if !self.overconstrained_record_owners_finalized {
            return Err(EqualityConstraintOccurrenceError::OwnerCatalogNotFinalized);
        }
        if !self.overconstrained_effective_types_finalized {
            return Err(EqualityConstraintOccurrenceError::EffectiveTypeCatalogNotFinalized);
        }
        let semantic_catalogs = self
            .semantic_catalogs
            .as_ref()
            .ok_or(EqualityConstraintOccurrenceError::SemanticCatalogNotFinalized)?;
        Ok(FinalizedOverconstrainedCatalog {
            overlay: self,
            semantic_catalogs,
        })
    }
}

fn validate_record_occurrence_shape(
    occurrence: InstanceId,
    record: &InstanceData,
) -> Result<(), EqualityConstraintOccurrenceError> {
    if record.is_primitive {
        return Err(EqualityConstraintOccurrenceError::PrimitiveRecordOccurrence(occurrence));
    }
    if !record.dims.is_empty() || !record.dims_expr.is_empty() {
        return Err(EqualityConstraintOccurrenceError::ArrayRecordOccurrence(
            occurrence,
        ));
    }
    Ok(())
}

/// Read-only proof that overconstrained ownership and effective types finalized.
pub struct FinalizedOverconstrainedCatalog<'overlay> {
    overlay: &'overlay InstanceOverlay,
    semantic_catalogs: &'overlay SemanticCatalogProjection,
}

impl FinalizedOverconstrainedCatalog<'_> {
    /// Exact overlay whose one-shot finalization issued this catalog.
    pub fn overlay(&self) -> &InstanceOverlay {
        self.overlay
    }

    pub fn semantic_catalogs(&self) -> &SemanticCatalogProjection {
        self.semantic_catalogs
    }

    pub fn exposure(
        &self,
        record_instance: InstanceId,
    ) -> Option<&EqualityConstraintOccurrenceExposure> {
        self.overlay
            .overconstrained_records
            .get(&record_instance)
            .map(|record| &record.exposure)
    }

    pub fn record_owner(&self, descendant: InstanceId) -> Option<InstanceId> {
        self.overlay
            .overconstrained_record_owners
            .get(&descendant)
            .copied()
    }

    /// Exact finalized record occurrence owning `descendant`, if any.
    pub fn record_owner_occurrence(
        &self,
        descendant: InstanceId,
    ) -> Option<FinalizedOverconstrainedRecord<'_>> {
        let record = self.record_owner(descendant)?;
        self.record(record)
    }

    /// Classify one exact component occurrence without an ambiguous absence
    /// sentinel. Foreign identities can never be treated as ordinary.
    pub fn classify_component(
        &self,
        component: InstanceId,
    ) -> FinalizedOverconstrainedComponent<'_> {
        if !self.overlay.components.contains_key(&component) {
            return FinalizedOverconstrainedComponent::Foreign;
        }
        self.record_owner_occurrence(component).map_or(
            FinalizedOverconstrainedComponent::Ordinary,
            FinalizedOverconstrainedComponent::Record,
        )
    }

    /// Exact finalized record occurrence for one catalog identity.
    pub fn record(
        &self,
        record_instance: InstanceId,
    ) -> Option<FinalizedOverconstrainedRecord<'_>> {
        let record = self.overlay.overconstrained_records.get(&record_instance)?;
        Some(FinalizedOverconstrainedRecord {
            instance_id: record_instance,
            qualified_name: &record.qualified_name,
            exposure: record.exposure,
        })
    }

    pub fn exposures(
        &self,
    ) -> impl Iterator<Item = (InstanceId, &EqualityConstraintOccurrenceExposure)> {
        self.overlay
            .overconstrained_records
            .iter()
            .map(|(record, occurrence)| (*record, &occurrence.exposure))
    }

    pub fn records(&self) -> impl Iterator<Item = FinalizedOverconstrainedRecord<'_>> {
        self.overlay
            .overconstrained_records
            .iter()
            .map(|(instance_id, record)| FinalizedOverconstrainedRecord {
                instance_id: *instance_id,
                qualified_name: &record.qualified_name,
                exposure: record.exposure,
            })
    }
}
