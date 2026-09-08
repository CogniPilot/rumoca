mod occurrence;
mod selection;

pub use occurrence::{
    EffectiveTypePublicationError, EqualityConstraintEffectiveRecordIdentity,
    EqualityConstraintOccurrenceError, EqualityConstraintOccurrenceExposure,
    EqualityConstraintSpecializationKey, FinalizedOverconstrainedCatalog,
    FinalizedOverconstrainedComponent, FinalizedOverconstrainedRecord,
};
pub(in crate::instance) use occurrence::{
    EqualityConstraintRecordOccurrence, FinalizedOverconstrainedOccurrence,
    PendingEqualityConstraintOccurrenceExposure,
};

use self::selection::{
    collect_equality_constraint_identities, effective_equality_constraint_slot,
    inherited_equality_constraint_selection, occurrence_equality_constraint_selection,
};
use super::FastIndexMap;
use crate::{Causality, ClassDef, ClassOverrideMap, ClassTree, ClassType, Expression, Subscript};
use rumoca_core::{DefId, Span};

/// Checked cardinality of an MLS §9.4.1 `equalityConstraint` result.
///
/// Zero is a real semantic result: a removed virtual-connection edge then
/// contributes no residual equation. Keeping it disjoint from a non-empty
/// result prevents callers from interpreting `None`, zero, and malformed
/// prototype metadata as the same state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EqualityConstraintCardinality {
    Vacuous,
    NonEmpty(std::num::NonZeroUsize),
}

impl EqualityConstraintCardinality {
    pub fn scalar_count(self) -> usize {
        match self {
            Self::Vacuous => 0,
            Self::NonEmpty(count) => count.get(),
        }
    }
}

/// Duplicate-aware declaration catalog built once by each owning phase.
///
/// It stores identities only; declaration bodies remain tree-owned. Repeated
/// equalityConstraint checks therefore replay O(1) identity counts and O(path
/// depth) tree lookup instead of rebuilding a whole-tree index per record.
#[derive(Debug, Clone)]
pub struct EqualityConstraintDeclarationIndex {
    occurrences: FastIndexMap<DefId, usize>,
    class_paths: FastIndexMap<DefId, Vec<String>>,
    component_paths: FastIndexMap<DefId, (Vec<String>, String)>,
    predefined_real_def_id: Option<DefId>,
}

/// Opaque proof that one record exposes one exact effective reserved slot and
/// that its selected callable came from the resolved redeclare chain.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EqualityConstraintSelectionProof {
    record_type_def_id: DefId,
    slot_def_id: DefId,
    selected_function_def_id: DefId,
    occurrence_component_def_id: Option<DefId>,
    slot_declaration_span: Span,
    selected_function_declaration_span: Span,
}

impl EqualityConstraintSelectionProof {
    pub fn record_type_def_id(self) -> DefId {
        self.record_type_def_id
    }

    pub fn slot_def_id(self) -> DefId {
        self.slot_def_id
    }

    pub fn selected_function_def_id(self) -> DefId {
        self.selected_function_def_id
    }

    pub fn occurrence_component_def_id(self) -> Option<DefId> {
        self.occurrence_component_def_id
    }
}

/// Resolved declaration/interface proof for one exact `equalityConstraint`
/// selection. Resolve may issue this before an occurrence-specific constant
/// extent is available; Instantiate completes it into an exposure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EqualityConstraintPrototype {
    record_type_def_id: DefId,
    slot_def_id: DefId,
    selected_function_def_id: DefId,
    occurrence_component_def_id: Option<DefId>,
    input_def_ids: [DefId; 2],
    input_type_def_ids: [DefId; 2],
    output_def_id: DefId,
    output_type_def_id: DefId,
    slot_declaration_span: Span,
    selected_function_declaration_span: Span,
    output_declaration_span: Span,
}

impl EqualityConstraintPrototype {
    pub fn record_type_def_id(self) -> DefId {
        self.record_type_def_id
    }

    pub fn slot_def_id(self) -> DefId {
        self.slot_def_id
    }

    pub fn selected_function_def_id(self) -> DefId {
        self.selected_function_def_id
    }

    pub fn occurrence_component_def_id(self) -> Option<DefId> {
        self.occurrence_component_def_id
    }

    pub fn input_def_ids(self) -> [DefId; 2] {
        self.input_def_ids
    }

    pub fn input_type_def_ids(self) -> [DefId; 2] {
        self.input_type_def_ids
    }

    pub fn output_def_id(self) -> DefId {
        self.output_def_id
    }

    pub fn output_type_def_id(self) -> DefId {
        self.output_type_def_id
    }

    pub fn slot_declaration_span(self) -> Span {
        self.slot_declaration_span
    }

    pub fn selected_function_declaration_span(self) -> Span {
        self.selected_function_declaration_span
    }

    pub fn output_declaration_span(self) -> Span {
        self.output_declaration_span
    }
}

/// Exact, checked `equalityConstraint` declaration selected for one effective
/// record occurrence.
///
/// The fields are deliberately private. Construction starts from a checked
/// prototype and evaluates its exact sole output dimension; no API accepts a
/// caller's independent `DefId`/cardinality claim.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct EqualityConstraintExposure {
    prototype: EqualityConstraintPrototype,
    cardinality: EqualityConstraintCardinality,
}

impl EqualityConstraintExposure {
    pub(super) fn record_type_def_id(self) -> DefId {
        self.prototype.record_type_def_id()
    }

    pub(super) fn slot_def_id(self) -> DefId {
        self.prototype.slot_def_id()
    }

    pub(super) fn selected_function_def_id(self) -> DefId {
        self.prototype.selected_function_def_id()
    }

    pub(super) fn output_def_id(self) -> DefId {
        self.prototype.output_def_id()
    }

    pub(super) fn output_type_def_id(self) -> DefId {
        self.prototype.output_type_def_id()
    }

    pub(super) fn slot_declaration_span(self) -> Span {
        self.prototype.slot_declaration_span()
    }

    pub(super) fn selected_function_declaration_span(self) -> Span {
        self.prototype.selected_function_declaration_span()
    }

    pub(super) fn output_declaration_span(self) -> Span {
        self.prototype.output_declaration_span()
    }

    pub(super) fn cardinality(self) -> EqualityConstraintCardinality {
        self.cardinality
    }
}

/// Why a resolved/effective declaration cannot expose MLS §9.4.1
/// `equalityConstraint` semantics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EqualityConstraintExposureError {
    MissingRecordIdentity,
    MissingSlotIdentity,
    MissingSelectedFunctionIdentity,
    MissingOccurrenceComponentIdentity,
    MissingInputIdentity { index: usize },
    MissingOutputIdentity,
    ReservedIdentity,
    UnknownRecordIdentity(DefId),
    UnknownSlotIdentity(DefId),
    UnknownSelectedFunctionIdentity(DefId),
    UnknownOccurrenceComponentIdentity(DefId),
    UnknownInterfaceIdentity(DefId),
    MissingBaseRecordIdentity,
    CyclicRecordInheritance(DefId),
    AmbiguousInheritedSlot,
    MissingRedeclareAliasIdentity,
    MissingRedeclareTargetIdentity,
    AmbiguousSelectedCallable,
    OccurrenceSpecializationNotReplayable,
    RecordOccurrenceTokenMismatch,
    CallableModifierCertificateNotImplemented,
    NonUniqueDeclarationIdentity { def_id: DefId, occurrences: usize },
    DuplicateInterfaceIdentity(DefId),
    RecordIsNotRecord,
    SlotHasWrongName,
    SlotIsNotFunction,
    SelectedDeclarationIsNotFunction,
    MissingCallableBaseIdentity,
    CyclicCallableInheritance(DefId),
    AmbiguousInheritedCallableInterface,
    InheritedCallableInterfaceCertificateNotImplemented,
    WrongInputCount(usize),
    InputIsArray { index: usize },
    InputHasWrongRecordIdentity { index: usize },
    WrongOutputCount(usize),
    OutputIsNotPredefinedReal,
    OutputIsNotRankOne,
    OutputIdentityChanged,
    OutputExtentIsNotConstant,
    SymbolicExtentCertificateNotImplemented,
    OutputExtentIsNegative(String),
    OutputExtentExceedsUsize(String),
    OutputExtentContradictsLiteral,
    MissingDeclarationProvenance,
}

impl std::fmt::Display for EqualityConstraintExposureError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingInputIdentity { index } => write!(
                formatter,
                "equalityConstraint input {} has no resolved declaration identity",
                index + 1
            ),
            Self::UnknownRecordIdentity(def_id) => write!(
                formatter,
                "effective record identity {def_id:?} is absent from the resolved class tree"
            ),
            Self::UnknownSlotIdentity(def_id) => write!(
                formatter,
                "equalityConstraint slot identity {def_id:?} is absent from the resolved class tree"
            ),
            Self::UnknownSelectedFunctionIdentity(def_id) => write!(
                formatter,
                "selected equalityConstraint callable identity {def_id:?} is absent from the resolved class tree"
            ),
            Self::UnknownOccurrenceComponentIdentity(def_id) => write!(
                formatter,
                "equalityConstraint occurrence component identity {def_id:?} is absent from the resolved class tree"
            ),
            Self::UnknownInterfaceIdentity(def_id) => write!(
                formatter,
                "equalityConstraint interface identity {def_id:?} is absent from the resolved class tree"
            ),
            Self::MissingBaseRecordIdentity => formatter.write_str(
                "an equalityConstraint record base has no resolved declaration identity",
            ),
            Self::CyclicRecordInheritance(def_id) => write!(
                formatter,
                "equalityConstraint record inheritance is cyclic at {def_id:?}"
            ),
            Self::NonUniqueDeclarationIdentity {
                def_id,
                occurrences,
            } => write!(
                formatter,
                "declaration identity {def_id:?} occurs {occurrences} times in the resolved class tree"
            ),
            Self::DuplicateInterfaceIdentity(def_id) => write!(
                formatter,
                "equalityConstraint reuses declaration identity {def_id:?} for multiple interface elements"
            ),
            Self::CyclicCallableInheritance(def_id) => write!(
                formatter,
                "equalityConstraint callable inheritance is cyclic at {def_id:?}"
            ),
            Self::WrongInputCount(count) => write!(
                formatter,
                "equalityConstraint has {count} inputs instead of exactly two"
            ),
            Self::InputIsArray { index } => write!(
                formatter,
                "equalityConstraint input {} is an array; both inputs must be non-array values of the exact effective record type",
                index + 1
            ),
            Self::InputHasWrongRecordIdentity { index } => write!(
                formatter,
                "equalityConstraint input {} does not have the exact effective record type",
                index + 1
            ),
            Self::WrongOutputCount(count) => write!(
                formatter,
                "equalityConstraint has {count} outputs instead of exactly one"
            ),
            Self::OutputExtentIsNegative(value) => write!(
                formatter,
                "the equalityConstraint output extent {value} is negative"
            ),
            Self::OutputExtentExceedsUsize(value) => write!(
                formatter,
                "the equalityConstraint output extent {value} exceeds the compiler index range"
            ),
            static_error => formatter.write_str(static_error.message()),
        }
    }
}

impl EqualityConstraintExposureError {
    fn message(&self) -> &'static str {
        match self {
            Self::MissingRecordIdentity => {
                "the effective record has no resolved declaration identity"
            }
            Self::MissingSlotIdentity => {
                "the equalityConstraint slot has no resolved declaration identity"
            }
            Self::MissingSelectedFunctionIdentity => {
                "the selected equalityConstraint callable has no resolved declaration identity"
            }
            Self::MissingOccurrenceComponentIdentity => {
                "the equalityConstraint exposure has no exact source component identity"
            }
            Self::MissingOutputIdentity => {
                "the selected equalityConstraint output has no resolved declaration identity"
            }
            Self::MissingBaseRecordIdentity => {
                "an equalityConstraint record base has no resolved declaration identity"
            }
            Self::ReservedIdentity => {
                "the effective record or equalityConstraint interface uses reserved DefId(0)"
            }
            Self::AmbiguousInheritedSlot => {
                "the effective record inherits multiple distinct equalityConstraint slots"
            }
            Self::MissingRedeclareAliasIdentity => {
                "an equalityConstraint redeclare alias has no exact resolved slot identity"
            }
            Self::MissingRedeclareTargetIdentity => {
                "an equalityConstraint redeclare target has no exact resolved callable identity"
            }
            Self::AmbiguousSelectedCallable => {
                "multiple equalityConstraint redeclares select a non-unique effective callable"
            }
            Self::OccurrenceSpecializationNotReplayable => {
                "the effective equalityConstraint occurrence specialization differs from its indexed source declaration and has no replay certificate"
            }
            Self::RecordOccurrenceTokenMismatch => {
                "the checked record occurrence token does not match the equalityConstraint selection proof"
            }
            Self::CallableModifierCertificateNotImplemented => {
                "equalityConstraint callable modifier specialization certificate not implemented"
            }
            Self::RecordIsNotRecord => "the effective equalityConstraint owner is not a record",
            Self::SlotHasWrongName => "the selected slot is not named equalityConstraint",
            Self::SlotIsNotFunction => "the equalityConstraint slot is not a function",
            Self::SelectedDeclarationIsNotFunction => {
                "the selected equalityConstraint callable is not a function"
            }
            Self::MissingCallableBaseIdentity => {
                "an inherited equalityConstraint callable base has no resolved declaration identity"
            }
            Self::AmbiguousInheritedCallableInterface => {
                "the selected equalityConstraint callable has multiple inherited interfaces"
            }
            Self::InheritedCallableInterfaceCertificateNotImplemented => {
                "modified or locally extended equalityConstraint callable interface certificate not implemented"
            }
            Self::OutputIsNotPredefinedReal => {
                "the equalityConstraint output is not the predefined Real type"
            }
            Self::OutputIsNotRankOne => {
                "the equalityConstraint output is not one rank-1 Real[n] value"
            }
            Self::OutputIdentityChanged => {
                "the equalityConstraint output declaration changed after prototype checking"
            }
            Self::OutputExtentIsNotConstant => {
                "the equalityConstraint output extent is not a constant Integer expression evaluable during translation"
            }
            Self::SymbolicExtentCertificateNotImplemented => {
                "symbolic equalityConstraint extent certificate not implemented"
            }
            Self::OutputExtentContradictsLiteral => {
                "the cached equalityConstraint output shape contradicts its exact literal extent"
            }
            Self::MissingDeclarationProvenance => {
                "the equalityConstraint declaration has no source provenance"
            }
            Self::MissingInputIdentity { .. }
            | Self::UnknownRecordIdentity(_)
            | Self::UnknownSlotIdentity(_)
            | Self::UnknownSelectedFunctionIdentity(_)
            | Self::UnknownOccurrenceComponentIdentity(_)
            | Self::UnknownInterfaceIdentity(_)
            | Self::CyclicRecordInheritance(_)
            | Self::NonUniqueDeclarationIdentity { .. }
            | Self::DuplicateInterfaceIdentity(_)
            | Self::CyclicCallableInheritance(_)
            | Self::WrongInputCount(_)
            | Self::InputIsArray { .. }
            | Self::InputHasWrongRecordIdentity { .. }
            | Self::WrongOutputCount(_)
            | Self::OutputExtentIsNegative(_)
            | Self::OutputExtentExceedsUsize(_) => {
                unreachable!("data-bearing exposure errors are formatted directly")
            }
        }
    }
}

impl std::error::Error for EqualityConstraintExposureError {}

impl EqualityConstraintDeclarationIndex {
    pub fn new(tree: &ClassTree) -> Self {
        let mut occurrences = FastIndexMap::default();
        let mut class_paths = FastIndexMap::default();
        let mut component_paths = FastIndexMap::default();
        for (name, class) in &tree.definitions.classes {
            collect_equality_constraint_identities(
                class,
                &mut vec![name.clone()],
                &mut occurrences,
                &mut class_paths,
                &mut component_paths,
            );
        }
        let predefined_real_def_id = tree
            .scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"));
        Self {
            occurrences,
            class_paths,
            component_paths,
            predefined_real_def_id,
        }
    }

    /// Derive the reserved slot and selected callable from tree-owned
    /// inheritance/redeclare syntax. Absence is distinct from malformed
    /// presence and produces `Ok(None)` only when no slot is exposed.
    pub fn prove_equality_constraint_selection(
        &self,
        tree: &ClassTree,
        record_type_def_id: DefId,
    ) -> Result<Option<EqualityConstraintSelectionProof>, EqualityConstraintExposureError> {
        self.prove_equality_constraint_selection_from(tree, record_type_def_id, None, None)
    }

    /// Derive the exact callable for one allocated occurrence from the closed
    /// override catalog issued by instantiation. Source modifier syntax is not
    /// replayed here: Resolve owns its resolved RHS, while Instantiate owns the
    /// occurrence-specific slot selection.
    pub(super) fn prove_equality_constraint_occurrence_selection(
        &self,
        tree: &ClassTree,
        occurrence: EqualityConstraintRecordOccurrence,
        class_overrides: &ClassOverrideMap,
    ) -> Result<Option<EqualityConstraintSelectionProof>, EqualityConstraintExposureError> {
        self.require_unique_component(tree, occurrence.declaration_def_id())?;
        self.prove_equality_constraint_selection_from(
            tree,
            occurrence.record_type_def_id(),
            Some(occurrence.declaration_def_id()),
            Some(class_overrides),
        )
    }

    fn prove_equality_constraint_selection_from(
        &self,
        tree: &ClassTree,
        record_type_def_id: DefId,
        occurrence_component_def_id: Option<DefId>,
        class_overrides: Option<&ClassOverrideMap>,
    ) -> Result<Option<EqualityConstraintSelectionProof>, EqualityConstraintExposureError> {
        let record = self.require_unique_class(
            tree,
            record_type_def_id,
            EqualityConstraintExposureError::UnknownRecordIdentity(record_type_def_id),
        )?;
        let Some(slot_def_id) = effective_equality_constraint_slot(self, tree, record)? else {
            return Ok(None);
        };
        self.require_unique_class(
            tree,
            slot_def_id,
            EqualityConstraintExposureError::UnknownSlotIdentity(slot_def_id),
        )?;
        let occurrence_selection = class_overrides
            .map(|overrides| occurrence_equality_constraint_selection(overrides, slot_def_id))
            .transpose()?
            .flatten();
        let inherited_selection =
            inherited_equality_constraint_selection(self, tree, record, slot_def_id)?;
        let selected_function_def_id = occurrence_selection
            .or(inherited_selection)
            .unwrap_or(slot_def_id);
        let selected = self.require_unique_class(
            tree,
            selected_function_def_id,
            EqualityConstraintExposureError::UnknownSelectedFunctionIdentity(
                selected_function_def_id,
            ),
        )?;
        let slot = self.require_unique_class(
            tree,
            slot_def_id,
            EqualityConstraintExposureError::UnknownSlotIdentity(slot_def_id),
        )?;
        Ok(Some(EqualityConstraintSelectionProof {
            record_type_def_id,
            slot_def_id,
            selected_function_def_id,
            occurrence_component_def_id,
            slot_declaration_span: equality_constraint_token_span(&slot.name)?,
            selected_function_declaration_span: equality_constraint_token_span(&selected.name)?,
        }))
    }

    /// Replay the exact tree-owned callable associated with an issued proof.
    pub fn selected_equality_constraint_function<'tree>(
        &self,
        tree: &'tree ClassTree,
        selection: EqualityConstraintSelectionProof,
    ) -> Result<&'tree ClassDef, EqualityConstraintExposureError> {
        let selected_function_def_id = selection.selected_function_def_id();
        self.require_unique_class(
            tree,
            selected_function_def_id,
            EqualityConstraintExposureError::UnknownSelectedFunctionIdentity(
                selected_function_def_id,
            ),
        )
    }

    /// Replay the exact tree-owned reserved slot associated with an issued proof.
    pub fn equality_constraint_slot<'tree>(
        &self,
        tree: &'tree ClassTree,
        selection: EqualityConstraintSelectionProof,
    ) -> Result<&'tree ClassDef, EqualityConstraintExposureError> {
        let slot_def_id = selection.slot_def_id();
        self.require_unique_class(
            tree,
            slot_def_id,
            EqualityConstraintExposureError::UnknownSlotIdentity(slot_def_id),
        )
    }

    /// Check the identity, non-array record inputs, predefined Real output,
    /// rank, and provenance of one exact effective record/function selection.
    pub fn check_equality_constraint_prototype(
        &self,
        tree: &ClassTree,
        selection: EqualityConstraintSelectionProof,
    ) -> Result<EqualityConstraintPrototype, EqualityConstraintExposureError> {
        let record_type_def_id = selection.record_type_def_id;
        let slot_def_id = selection.slot_def_id;
        let selected_function_def_id = selection.selected_function_def_id;
        let record = self.require_unique_class(
            tree,
            record_type_def_id,
            EqualityConstraintExposureError::UnknownRecordIdentity(record_type_def_id),
        )?;
        let slot = self.require_unique_class(
            tree,
            slot_def_id,
            EqualityConstraintExposureError::UnknownSlotIdentity(slot_def_id),
        )?;
        let selected_function = self.require_unique_class(
            tree,
            selected_function_def_id,
            EqualityConstraintExposureError::UnknownSelectedFunctionIdentity(
                selected_function_def_id,
            ),
        )?;
        if record.class_type != ClassType::Record {
            return Err(EqualityConstraintExposureError::RecordIsNotRecord);
        }
        if slot.name.text.as_ref() != "equalityConstraint" {
            return Err(EqualityConstraintExposureError::SlotHasWrongName);
        }
        if slot.class_type != ClassType::Function {
            return Err(EqualityConstraintExposureError::SlotIsNotFunction);
        }
        if selected_function.class_type != ClassType::Function {
            return Err(EqualityConstraintExposureError::SelectedDeclarationIsNotFunction);
        }
        let interface = self.equality_constraint_callable_interface(tree, selected_function)?;
        let mut interface_identities = std::collections::HashSet::new();
        let inputs = check_equality_constraint_inputs(
            self,
            interface,
            record_type_def_id,
            &mut interface_identities,
        )?;
        let output = check_equality_constraint_output(self, interface, &mut interface_identities)?;

        Ok(EqualityConstraintPrototype {
            record_type_def_id,
            slot_def_id,
            selected_function_def_id,
            occurrence_component_def_id: selection.occurrence_component_def_id,
            input_def_ids: inputs.def_ids,
            input_type_def_ids: inputs.type_def_ids,
            output_def_id: output.def_id,
            output_type_def_id: output.type_def_id,
            slot_declaration_span: selection.slot_declaration_span,
            selected_function_declaration_span: selection.selected_function_declaration_span,
            output_declaration_span: output.declaration_span,
        })
    }

    /// Check a prototype's cardinality by replaying the indexed tree-owned
    /// selected declaration and its exact literal dimension.
    ///
    /// Symbolic constant evaluation deliberately has no callback here: a
    /// caller-provided evaluator would be an unchecked cardinality claim. A
    /// later cutover may accept an independently replayable certificate.
    pub fn check_equality_constraint_cardinality(
        &self,
        tree: &ClassTree,
        prototype: EqualityConstraintPrototype,
    ) -> Result<EqualityConstraintCardinality, EqualityConstraintExposureError> {
        let selected_function_def_id = prototype.selected_function_def_id();
        let selected_function = self.require_unique_class(
            tree,
            selected_function_def_id,
            EqualityConstraintExposureError::UnknownSelectedFunctionIdentity(
                selected_function_def_id,
            ),
        )?;
        let interface = self.equality_constraint_callable_interface(tree, selected_function)?;
        let outputs: Vec<_> = interface
            .components
            .values()
            .filter(|component| matches!(component.causality, Causality::Output(_)))
            .collect();
        let [output] = outputs.as_slice() else {
            return Err(EqualityConstraintExposureError::WrongOutputCount(
                outputs.len(),
            ));
        };
        if output.def_id != Some(prototype.output_def_id())
            || output.type_def_id != Some(prototype.output_type_def_id())
        {
            return Err(EqualityConstraintExposureError::OutputIdentityChanged);
        }
        let [Subscript::Expression(extent_expression)] = output.shape_expr.as_slice() else {
            return Err(EqualityConstraintExposureError::OutputIsNotRankOne);
        };
        let extent = equality_constraint_literal_extent(extent_expression)?;
        if output.shape.as_slice() != [extent] {
            return Err(EqualityConstraintExposureError::OutputExtentContradictsLiteral);
        }
        Ok(std::num::NonZeroUsize::new(extent).map_or(
            EqualityConstraintCardinality::Vacuous,
            EqualityConstraintCardinality::NonEmpty,
        ))
    }

    fn equality_constraint_callable_interface<'tree>(
        &self,
        tree: &'tree ClassTree,
        selected_function: &'tree ClassDef,
    ) -> Result<&'tree ClassDef, EqualityConstraintExposureError> {
        let mut current = selected_function;
        let mut visited = std::collections::HashSet::new();
        loop {
            let current_def_id = current
                .def_id
                .ok_or(EqualityConstraintExposureError::MissingSelectedFunctionIdentity)?;
            if !visited.insert(current_def_id) {
                return Err(EqualityConstraintExposureError::CyclicCallableInheritance(
                    current_def_id,
                ));
            }
            if current.extends.is_empty() {
                return Ok(current);
            }
            if !current.components.is_empty() {
                return Err(
                    EqualityConstraintExposureError::InheritedCallableInterfaceCertificateNotImplemented,
                );
            }
            let [base] = current.extends.as_slice() else {
                return Err(EqualityConstraintExposureError::AmbiguousInheritedCallableInterface);
            };
            if !base.modifications.is_empty() || !base.break_names.is_empty() || base.is_protected {
                return Err(
                    EqualityConstraintExposureError::InheritedCallableInterfaceCertificateNotImplemented,
                );
            }
            let base_def_id = base
                .base_def_id
                .ok_or(EqualityConstraintExposureError::MissingCallableBaseIdentity)?;
            current = self.require_unique_class(
                tree,
                base_def_id,
                EqualityConstraintExposureError::UnknownSelectedFunctionIdentity(base_def_id),
            )?;
            if current.class_type != ClassType::Function {
                return Err(EqualityConstraintExposureError::SelectedDeclarationIsNotFunction);
            }
        }
    }

    /// Bind a checked tree-owned declaration proof to one exact instance.
    pub(super) fn complete_equality_constraint_occurrence(
        &self,
        tree: &ClassTree,
        prototype: EqualityConstraintPrototype,
        occurrence: EqualityConstraintRecordOccurrence,
    ) -> Result<PendingEqualityConstraintOccurrenceExposure, EqualityConstraintExposureError> {
        let occurrence_component_def_id = prototype
            .occurrence_component_def_id()
            .ok_or(EqualityConstraintExposureError::MissingOccurrenceComponentIdentity)?;
        if occurrence.record_type_def_id() != prototype.record_type_def_id()
            || occurrence.declaration_def_id() != occurrence_component_def_id
        {
            return Err(EqualityConstraintExposureError::RecordOccurrenceTokenMismatch);
        }
        let cardinality = self.check_equality_constraint_cardinality(tree, prototype)?;
        Ok(occurrence::bind_equality_constraint_occurrence(
            occurrence,
            EqualityConstraintExposure {
                prototype,
                cardinality,
            },
        ))
    }

    fn require_unique_class<'tree>(
        &self,
        tree: &'tree ClassTree,
        def_id: DefId,
        unknown: EqualityConstraintExposureError,
    ) -> Result<&'tree ClassDef, EqualityConstraintExposureError> {
        let occurrences = self.occurrences.get(&def_id).copied().unwrap_or(0);
        if occurrences == 0 {
            return Err(unknown);
        }
        if occurrences != 1 {
            return Err(
                EqualityConstraintExposureError::NonUniqueDeclarationIdentity {
                    def_id,
                    occurrences,
                },
            );
        }
        let path = self
            .class_paths
            .get(&def_id)
            .ok_or_else(|| unknown.clone())?;
        class_at_indexed_path(tree, path).ok_or(unknown)
    }

    fn require_unique_component<'tree>(
        &self,
        tree: &'tree ClassTree,
        def_id: DefId,
    ) -> Result<&'tree crate::Component, EqualityConstraintExposureError> {
        let occurrences = self.occurrences.get(&def_id).copied().unwrap_or(0);
        if occurrences == 0 {
            return Err(
                EqualityConstraintExposureError::UnknownOccurrenceComponentIdentity(def_id),
            );
        }
        if occurrences != 1 {
            return Err(
                EqualityConstraintExposureError::NonUniqueDeclarationIdentity {
                    def_id,
                    occurrences,
                },
            );
        }
        let (class_path, component_name) = self
            .component_paths
            .get(&def_id)
            .ok_or(EqualityConstraintExposureError::UnknownOccurrenceComponentIdentity(def_id))?;
        class_at_indexed_path(tree, class_path)
            .and_then(|class| class.components.get(component_name))
            .ok_or(EqualityConstraintExposureError::UnknownOccurrenceComponentIdentity(def_id))
    }
}

fn class_at_indexed_path<'tree>(
    tree: &'tree ClassTree,
    path: &[String],
) -> Option<&'tree ClassDef> {
    let (root, nested) = path.split_first()?;
    let mut class = tree.definitions.classes.get(root)?;
    for name in nested {
        class = class.classes.get(name)?;
    }
    Some(class)
}

struct EqualityConstraintOutputProof {
    def_id: DefId,
    type_def_id: DefId,
    declaration_span: Span,
}

struct EqualityConstraintInputProof {
    def_ids: [DefId; 2],
    type_def_ids: [DefId; 2],
}

fn check_equality_constraint_inputs(
    index: &EqualityConstraintDeclarationIndex,
    selected_function: &ClassDef,
    record_type_def_id: DefId,
    interface_identities: &mut std::collections::HashSet<DefId>,
) -> Result<EqualityConstraintInputProof, EqualityConstraintExposureError> {
    let inputs: Vec<_> = selected_function
        .components
        .values()
        .filter(|component| matches!(component.causality, Causality::Input(_)))
        .collect();
    if inputs.len() != 2 {
        return Err(EqualityConstraintExposureError::WrongInputCount(
            inputs.len(),
        ));
    }
    let mut def_ids = [DefId(0); 2];
    let mut type_def_ids = [DefId(0); 2];
    for (input_index, input) in inputs.into_iter().enumerate() {
        let input_def_id = input
            .def_id
            .ok_or(EqualityConstraintExposureError::MissingInputIdentity { index: input_index })?;
        check_equality_constraint_interface_identity(index, interface_identities, input_def_id)?;
        if !input.shape.is_empty() || !input.shape_expr.is_empty() {
            return Err(EqualityConstraintExposureError::InputIsArray { index: input_index });
        }
        if input.type_def_id != Some(record_type_def_id) {
            return Err(
                EqualityConstraintExposureError::InputHasWrongRecordIdentity { index: input_index },
            );
        }
        def_ids[input_index] = input_def_id;
        type_def_ids[input_index] = record_type_def_id;
    }
    Ok(EqualityConstraintInputProof {
        def_ids,
        type_def_ids,
    })
}

fn check_equality_constraint_output(
    index: &EqualityConstraintDeclarationIndex,
    selected_function: &ClassDef,
    interface_identities: &mut std::collections::HashSet<DefId>,
) -> Result<EqualityConstraintOutputProof, EqualityConstraintExposureError> {
    let outputs: Vec<_> = selected_function
        .components
        .values()
        .filter(|component| matches!(component.causality, Causality::Output(_)))
        .collect();
    let [output] = outputs.as_slice() else {
        return Err(EqualityConstraintExposureError::WrongOutputCount(
            outputs.len(),
        ));
    };
    let output_def_id = output
        .def_id
        .ok_or(EqualityConstraintExposureError::MissingOutputIdentity)?;
    check_equality_constraint_interface_identity(index, interface_identities, output_def_id)?;
    let predefined_real = index
        .predefined_real_def_id
        .ok_or(EqualityConstraintExposureError::OutputIsNotPredefinedReal)?;
    if output.type_def_id != Some(predefined_real) {
        return Err(EqualityConstraintExposureError::OutputIsNotPredefinedReal);
    }
    if !matches!(output.shape_expr.as_slice(), [Subscript::Expression(_)]) {
        return Err(EqualityConstraintExposureError::OutputIsNotRankOne);
    }
    Ok(EqualityConstraintOutputProof {
        def_id: output_def_id,
        type_def_id: predefined_real,
        declaration_span: equality_constraint_token_span(&output.name_token)?,
    })
}

fn equality_constraint_literal_extent(
    expression: &Expression,
) -> Result<usize, EqualityConstraintExposureError> {
    match expression {
        Expression::Terminal {
            terminal_type: crate::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse::<usize>().map_err(|_| {
            EqualityConstraintExposureError::OutputExtentExceedsUsize(token.text.to_string())
        }),
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => {
            let Expression::Terminal {
                terminal_type: crate::TerminalType::UnsignedInteger,
                token,
                ..
            } = rhs.as_ref()
            else {
                return Err(
                    EqualityConstraintExposureError::SymbolicExtentCertificateNotImplemented,
                );
            };
            Err(EqualityConstraintExposureError::OutputExtentIsNegative(
                format!("-{}", token.text),
            ))
        }
        _ => Err(EqualityConstraintExposureError::SymbolicExtentCertificateNotImplemented),
    }
}

fn check_equality_constraint_interface_identity(
    index: &EqualityConstraintDeclarationIndex,
    identities: &mut std::collections::HashSet<DefId>,
    def_id: DefId,
) -> Result<(), EqualityConstraintExposureError> {
    if def_id.index() == 0 {
        return Err(EqualityConstraintExposureError::ReservedIdentity);
    }
    let occurrences = index.occurrences.get(&def_id).copied().unwrap_or(0);
    if occurrences == 0 {
        return Err(EqualityConstraintExposureError::UnknownInterfaceIdentity(
            def_id,
        ));
    }
    if occurrences != 1 {
        return Err(
            EqualityConstraintExposureError::NonUniqueDeclarationIdentity {
                def_id,
                occurrences,
            },
        );
    }
    if !identities.insert(def_id) {
        return Err(EqualityConstraintExposureError::DuplicateInterfaceIdentity(
            def_id,
        ));
    }
    Ok(())
}

fn equality_constraint_token_span(
    token: &rumoca_core::Token,
) -> Result<Span, EqualityConstraintExposureError> {
    let source = token.location.source;
    let start = token.location.start as usize;
    let end = (token.location.end as usize).max(start.saturating_add(1));
    Span::from_offsets(source, start, end)
        .require_provenance("checking equalityConstraint declaration")
        .map_err(|_| EqualityConstraintExposureError::MissingDeclarationProvenance)
        .map(rumoca_core::ProvenanceSpan::span)
}
