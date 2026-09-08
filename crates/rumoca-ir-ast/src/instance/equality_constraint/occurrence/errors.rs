use super::super::EqualityConstraintExposureError;
use rumoca_core::{DefId, InstanceId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EqualityConstraintOccurrenceError {
    InvalidExposure(EqualityConstraintExposureError),
    UnsetRecordIdentity,
    UnallocatedRecordIdentity(InstanceId),
    MissingRecordOccurrence(InstanceId),
    MissingRecordDeclarationIdentity(InstanceId),
    MissingRecordTypeIdentity(InstanceId),
    PrimitiveRecordOccurrence(InstanceId),
    ArrayRecordOccurrence(InstanceId),
    MissingDescendantOccurrence(InstanceId),
    MissingOwnerClass {
        class: InstanceId,
        descendant: InstanceId,
    },
    MissingRecordClass(InstanceId),
    MultipleRecordClasses {
        record: InstanceId,
        count: usize,
    },
    RecordClassEvidenceMismatch {
        record: InstanceId,
        class: InstanceId,
    },
    CyclicInstanceAncestry(InstanceId),
    DuplicateRecordExposure(InstanceId),
    OwnerCatalogNotFinalized,
    OwnerCatalogAlreadyFinalized,
    EffectiveTypeCatalogNotFinalized,
    SemanticCatalogNotFinalized,
    EffectiveTypeCatalogAlreadyFinalized,
    ExposureCatalogAlreadyFinalized,
    RecordOccurrenceMismatch {
        catalog_key: InstanceId,
        proof_occurrence: InstanceId,
    },
    RecordTypeMismatch {
        occurrence: InstanceId,
        actual: Option<DefId>,
        expected: DefId,
    },
    RecordDeclarationMismatch {
        occurrence: InstanceId,
        actual: Option<DefId>,
        expected: DefId,
    },
    MissingEffectiveRecordIdentity {
        occurrence: InstanceId,
        type_id: TypeId,
    },
    EffectiveRecordCertificateNotImplemented(InstanceId),
    EffectiveRecordNominalMismatch {
        occurrence: InstanceId,
        actual_nominal: TypeId,
        expected_nominal: TypeId,
    },
    InputEffectiveTypeCertificateNotImplemented {
        occurrence: InstanceId,
        input_index: usize,
        input_def_id: DefId,
    },
    InputEffectiveTypeMismatch {
        occurrence: InstanceId,
        input_index: usize,
        input_def_id: DefId,
    },
}

impl std::fmt::Display for EqualityConstraintOccurrenceError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(result) = self.fmt_catalog_error(formatter) {
            return result;
        }
        match self {
            Self::InvalidExposure(reason) => write!(
                formatter,
                "invalid equalityConstraint declaration exposure: {reason}"
            ),
            Self::UnsetRecordIdentity => {
                formatter.write_str("an overconstrained occurrence cannot use InstanceId::UNSET")
            }
            Self::UnallocatedRecordIdentity(instance) => write!(
                formatter,
                "overconstrained record occurrence {instance:?} was not allocated by this overlay"
            ),
            Self::MissingRecordOccurrence(instance) => write!(
                formatter,
                "overconstrained record occurrence {instance:?} is absent"
            ),
            Self::MissingRecordDeclarationIdentity(instance) => write!(
                formatter,
                "overconstrained record occurrence {instance:?} has no exact component declaration identity"
            ),
            Self::MissingRecordTypeIdentity(instance) => write!(
                formatter,
                "overconstrained record occurrence {instance:?} has no nominal record identity"
            ),
            Self::PrimitiveRecordOccurrence(instance) => write!(
                formatter,
                "overconstrained record occurrence {instance:?} is primitive"
            ),
            Self::ArrayRecordOccurrence(instance) => write!(
                formatter,
                "overconstrained record occurrence {instance:?} is an array; only scalarized non-array occurrences may expose equalityConstraint"
            ),
            Self::MissingDescendantOccurrence(instance) => write!(
                formatter,
                "overconstrained descendant occurrence {instance:?} is absent"
            ),
            Self::MissingOwnerClass { class, descendant } => write!(
                formatter,
                "descendant occurrence {descendant:?} references missing owner class {class:?}"
            ),
            Self::MissingRecordClass(record) => write!(
                formatter,
                "overconstrained record occurrence {record:?} has no exact class occurrence"
            ),
            Self::MultipleRecordClasses { record, count } => write!(
                formatter,
                "overconstrained record occurrence {record:?} has {count} class occurrences"
            ),
            Self::RecordClassEvidenceMismatch { record, class } => write!(
                formatter,
                "class occurrence {class:?} does not match the exact name/type evidence of overconstrained record {record:?}"
            ),
            Self::CyclicInstanceAncestry(instance) => write!(
                formatter,
                "instance ancestry is cyclic at occurrence {instance:?}"
            ),
            Self::DuplicateRecordExposure(instance) => write!(
                formatter,
                "overconstrained record occurrence {instance:?} has duplicate exposure"
            ),
            catalog_error => formatter.write_str(
                catalog_error
                    .catalog_message()
                    .expect("catalog errors are formatted by the catalog formatter"),
            ),
        }
    }
}

impl EqualityConstraintOccurrenceError {
    fn fmt_catalog_error(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> Option<std::fmt::Result> {
        match self {
            Self::OwnerCatalogNotFinalized => {
                Some(formatter.write_str("overconstrained descendant ownership is not finalized"))
            }
            Self::OwnerCatalogAlreadyFinalized
            | Self::EffectiveTypeCatalogNotFinalized
            | Self::SemanticCatalogNotFinalized
            | Self::EffectiveTypeCatalogAlreadyFinalized
            | Self::ExposureCatalogAlreadyFinalized => None,
            Self::RecordOccurrenceMismatch {
                catalog_key,
                proof_occurrence,
            } => Some(write!(
                formatter,
                "overconstrained catalog key {catalog_key:?} does not match proof occurrence {proof_occurrence:?}"
            )),
            Self::RecordTypeMismatch {
                occurrence,
                actual,
                expected,
            } => Some(write!(
                formatter,
                "overconstrained record occurrence {occurrence:?} has type {actual:?}, expected {expected:?}"
            )),
            Self::RecordDeclarationMismatch {
                occurrence,
                actual,
                expected,
            } => Some(write!(
                formatter,
                "overconstrained record occurrence {occurrence:?} comes from declaration {actual:?}, expected {expected:?}"
            )),
            Self::MissingEffectiveRecordIdentity {
                occurrence,
                type_id,
            } => Some(write!(
                formatter,
                "overconstrained record occurrence {occurrence:?} references missing effective type {type_id:?}"
            )),
            Self::EffectiveRecordCertificateNotImplemented(occurrence) => Some(write!(
                formatter,
                "effective equalityConstraint record identity certificate is unavailable for occurrence {occurrence:?}"
            )),
            Self::EffectiveRecordNominalMismatch {
                occurrence,
                actual_nominal,
                expected_nominal,
            } => Some(write!(
                formatter,
                "effective equalityConstraint record occurrence {occurrence:?} has nominal type {actual_nominal:?}, expected {expected_nominal:?}"
            )),
            Self::InputEffectiveTypeCertificateNotImplemented {
                occurrence,
                input_index,
                input_def_id,
            } => Some(write!(
                formatter,
                "effective type certificate for equalityConstraint input {} ({input_def_id:?}) is unavailable at record occurrence {occurrence:?}",
                input_index + 1,
            )),
            Self::InputEffectiveTypeMismatch {
                occurrence,
                input_index,
                input_def_id,
            } => Some(write!(
                formatter,
                "equalityConstraint input {} ({input_def_id:?}) does not have the exact effective type of record occurrence {occurrence:?}",
                input_index + 1,
            )),
            _ => None,
        }
    }

    fn catalog_message(&self) -> Option<&'static str> {
        match self {
            Self::OwnerCatalogAlreadyFinalized => {
                Some("overconstrained descendant ownership is already finalized")
            }
            Self::EffectiveTypeCatalogNotFinalized => {
                Some("overconstrained effective record identities are not finalized")
            }
            Self::SemanticCatalogNotFinalized => {
                Some("checked semantic catalogs are not finalized")
            }
            Self::EffectiveTypeCatalogAlreadyFinalized => {
                Some("overconstrained effective record identities are already finalized")
            }
            Self::ExposureCatalogAlreadyFinalized => {
                Some("overconstrained exposure construction is already finalized")
            }
            _ => None,
        }
    }
}

impl std::error::Error for EqualityConstraintOccurrenceError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectiveTypePublicationError {
    EffectiveIdentityArenaExhausted(InstanceId),
    InvalidComponentDescriptor {
        occurrence: InstanceId,
        reason: String,
    },
    InvalidCatalogIdentity {
        type_id: TypeId,
        reason: &'static str,
    },
    EqualityConstraint(EqualityConstraintOccurrenceError),
}

impl EffectiveTypePublicationError {
    pub fn occurrence(&self) -> Option<InstanceId> {
        match self {
            Self::EffectiveIdentityArenaExhausted(occurrence)
            | Self::InvalidComponentDescriptor { occurrence, .. } => Some(*occurrence),
            Self::InvalidCatalogIdentity { .. } => None,
            Self::EqualityConstraint(error) => error.occurrence(),
        }
    }
}

impl std::fmt::Display for EffectiveTypePublicationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EffectiveIdentityArenaExhausted(occurrence) => write!(
                formatter,
                "effective identity arena is exhausted at occurrence {occurrence:?}"
            ),
            Self::InvalidComponentDescriptor { occurrence, reason } => write!(
                formatter,
                "effective publication for occurrence {occurrence:?} is invalid: {reason}"
            ),
            Self::InvalidCatalogIdentity { type_id, reason } => write!(
                formatter,
                "effective publication identity {type_id:?} is invalid: {reason}"
            ),
            Self::EqualityConstraint(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for EffectiveTypePublicationError {}

impl From<EqualityConstraintOccurrenceError> for EffectiveTypePublicationError {
    fn from(error: EqualityConstraintOccurrenceError) -> Self {
        Self::EqualityConstraint(error)
    }
}

impl EqualityConstraintOccurrenceError {
    pub fn occurrence(&self) -> Option<InstanceId> {
        match self {
            Self::InvalidExposure(_)
            | Self::UnsetRecordIdentity
            | Self::OwnerCatalogNotFinalized
            | Self::OwnerCatalogAlreadyFinalized
            | Self::EffectiveTypeCatalogNotFinalized
            | Self::SemanticCatalogNotFinalized
            | Self::EffectiveTypeCatalogAlreadyFinalized
            | Self::ExposureCatalogAlreadyFinalized => None,
            Self::UnallocatedRecordIdentity(instance)
            | Self::MissingRecordOccurrence(instance)
            | Self::MissingRecordDeclarationIdentity(instance)
            | Self::MissingRecordTypeIdentity(instance)
            | Self::PrimitiveRecordOccurrence(instance)
            | Self::ArrayRecordOccurrence(instance)
            | Self::MissingDescendantOccurrence(instance)
            | Self::MissingRecordClass(instance)
            | Self::CyclicInstanceAncestry(instance)
            | Self::DuplicateRecordExposure(instance)
            | Self::EffectiveRecordCertificateNotImplemented(instance) => Some(*instance),
            Self::MissingOwnerClass { descendant, .. } => Some(*descendant),
            Self::MultipleRecordClasses { record, .. }
            | Self::RecordClassEvidenceMismatch { record, .. }
            | Self::RecordTypeMismatch {
                occurrence: record, ..
            }
            | Self::RecordDeclarationMismatch {
                occurrence: record, ..
            }
            | Self::MissingEffectiveRecordIdentity {
                occurrence: record, ..
            }
            | Self::EffectiveRecordNominalMismatch {
                occurrence: record, ..
            }
            | Self::InputEffectiveTypeCertificateNotImplemented {
                occurrence: record, ..
            }
            | Self::InputEffectiveTypeMismatch {
                occurrence: record, ..
            } => Some(*record),
            Self::RecordOccurrenceMismatch { catalog_key, .. } => Some(*catalog_key),
        }
    }
}
