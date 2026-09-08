use super::{EqualityConstraintDeclarationIndex, EqualityConstraintExposureError, FastIndexMap};
use crate::{ClassDef, ClassOverrideMap, ClassTree, ComponentReference, Expression};
use rumoca_core::DefId;

pub(super) fn collect_equality_constraint_identities(
    class: &ClassDef,
    path: &mut Vec<String>,
    occurrences: &mut FastIndexMap<DefId, usize>,
    class_paths: &mut FastIndexMap<DefId, Vec<String>>,
    component_paths: &mut FastIndexMap<DefId, (Vec<String>, String)>,
) {
    if let Some(def_id) = class.def_id {
        *occurrences.entry(def_id).or_default() += 1;
        class_paths.entry(def_id).or_insert_with(|| path.clone());
    }
    for (name, component) in &class.components {
        if let Some(def_id) = component.def_id {
            *occurrences.entry(def_id).or_default() += 1;
            component_paths
                .entry(def_id)
                .or_insert_with(|| (path.clone(), name.clone()));
        }
    }
    for (name, nested) in &class.classes {
        path.push(name.clone());
        collect_equality_constraint_identities(
            nested,
            path,
            occurrences,
            class_paths,
            component_paths,
        );
        path.pop();
    }
}

pub(super) fn effective_equality_constraint_slot(
    index: &EqualityConstraintDeclarationIndex,
    tree: &ClassTree,
    record: &ClassDef,
) -> Result<Option<DefId>, EqualityConstraintExposureError> {
    let mut active = Vec::new();
    let candidates = inherited_slot_candidates(index, tree, record, &mut active)?;
    unique_candidate(
        candidates,
        EqualityConstraintExposureError::AmbiguousInheritedSlot,
    )
}

fn inherited_slot_candidates(
    index: &EqualityConstraintDeclarationIndex,
    tree: &ClassTree,
    record: &ClassDef,
    active: &mut Vec<DefId>,
) -> Result<Vec<DefId>, EqualityConstraintExposureError> {
    let record_def_id = record
        .def_id
        .ok_or(EqualityConstraintExposureError::MissingRecordIdentity)?;
    if active.contains(&record_def_id) {
        return Err(EqualityConstraintExposureError::CyclicRecordInheritance(
            record_def_id,
        ));
    }
    if let Some(slot) = record.classes.get("equalityConstraint") {
        return Ok(vec![
            slot.def_id
                .ok_or(EqualityConstraintExposureError::MissingSlotIdentity)?,
        ]);
    }

    active.push(record_def_id);
    let mut candidates = Vec::new();
    for extend in &record.extends {
        let base_def_id = extend
            .base_def_id
            .ok_or(EqualityConstraintExposureError::MissingBaseRecordIdentity)?;
        let base = index.require_unique_class(
            tree,
            base_def_id,
            EqualityConstraintExposureError::UnknownRecordIdentity(base_def_id),
        )?;
        append_unique(
            &mut candidates,
            inherited_slot_candidates(index, tree, base, active)?,
        );
    }
    active.pop();
    Ok(candidates)
}

pub(super) fn occurrence_equality_constraint_selection(
    class_overrides: &ClassOverrideMap,
    slot_def_id: DefId,
) -> Result<Option<DefId>, EqualityConstraintExposureError> {
    let Some(selection) = class_overrides.get(&slot_def_id) else {
        return Ok(None);
    };
    if selection.alias_def_id != slot_def_id {
        return Err(EqualityConstraintExposureError::OccurrenceSpecializationNotReplayable);
    }
    if !selection.modifier_args.is_empty() {
        return Err(EqualityConstraintExposureError::CallableModifierCertificateNotImplemented);
    }
    Ok(Some(selection.target_def_id))
}

pub(super) fn inherited_equality_constraint_selection(
    index: &EqualityConstraintDeclarationIndex,
    tree: &ClassTree,
    record: &ClassDef,
    slot_def_id: DefId,
) -> Result<Option<DefId>, EqualityConstraintExposureError> {
    inherited_selection(index, tree, record, slot_def_id, &mut Vec::new())
}

fn inherited_selection(
    index: &EqualityConstraintDeclarationIndex,
    tree: &ClassTree,
    record: &ClassDef,
    slot_def_id: DefId,
    active: &mut Vec<DefId>,
) -> Result<Option<DefId>, EqualityConstraintExposureError> {
    let record_def_id = record
        .def_id
        .ok_or(EqualityConstraintExposureError::MissingRecordIdentity)?;
    if active.contains(&record_def_id) {
        return Err(EqualityConstraintExposureError::CyclicRecordInheritance(
            record_def_id,
        ));
    }
    let mut local = Vec::new();
    for extend in &record.extends {
        for modification in &extend.modifications {
            if !modification.redeclare {
                continue;
            }
            if let Some(selected) = exact_redeclare_selection(&modification.expr, slot_def_id)? {
                local.push(selected);
            }
        }
    }
    if !local.is_empty() {
        return unique_candidate(
            local,
            EqualityConstraintExposureError::AmbiguousSelectedCallable,
        );
    }

    active.push(record_def_id);
    let mut inherited = Vec::new();
    for extend in &record.extends {
        let base_def_id = extend
            .base_def_id
            .ok_or(EqualityConstraintExposureError::MissingBaseRecordIdentity)?;
        let base = index.require_unique_class(
            tree,
            base_def_id,
            EqualityConstraintExposureError::UnknownRecordIdentity(base_def_id),
        )?;
        if let Some(selected) = inherited_selection(index, tree, base, slot_def_id, active)? {
            push_unique(&mut inherited, selected);
        }
    }
    active.pop();
    unique_candidate(
        inherited,
        EqualityConstraintExposureError::AmbiguousSelectedCallable,
    )
}

fn exact_redeclare_selection(
    expression: &Expression,
    slot_def_id: DefId,
) -> Result<Option<DefId>, EqualityConstraintExposureError> {
    let Some(alias) = modification_alias(expression) else {
        return Ok(None);
    };
    let alias_def_id = alias.target_def_id();
    if alias_def_id != Some(slot_def_id) {
        if alias_def_id.is_none()
            && alias
                .parts
                .first()
                .is_some_and(|part| part.ident.text.as_ref() == "equalityConstraint")
        {
            return Err(EqualityConstraintExposureError::MissingRedeclareAliasIdentity);
        }
        return Ok(None);
    }
    let selected = replacement_reference(expression)
        .and_then(ComponentReference::target_def_id)
        .ok_or(EqualityConstraintExposureError::MissingRedeclareTargetIdentity)?;
    if redeclare_has_modifier_actuals(expression) {
        return Err(EqualityConstraintExposureError::CallableModifierCertificateNotImplemented);
    }
    Ok(Some(selected))
}

fn redeclare_has_modifier_actuals(expression: &Expression) -> bool {
    match expression {
        Expression::Modification {
            value: Some(value), ..
        } => redeclare_has_modifier_actuals(value),
        Expression::ClassModification { modifications, .. } => !modifications.is_empty(),
        Expression::FunctionCall { args, .. } => !args.is_empty(),
        _ => false,
    }
}

fn modification_alias(expression: &Expression) -> Option<&ComponentReference> {
    match expression {
        Expression::Modification { target, .. } | Expression::ClassModification { target, .. } => {
            Some(target)
        }
        _ => None,
    }
}

fn replacement_reference(expression: &Expression) -> Option<&ComponentReference> {
    match expression {
        Expression::Modification {
            value: Some(value), ..
        } => replacement_reference(value),
        Expression::ClassModification { target, .. } => Some(target),
        Expression::FunctionCall { comp, .. } => Some(comp),
        Expression::ComponentReference(reference) => Some(reference),
        _ => None,
    }
}

fn unique_candidate(
    candidates: Vec<DefId>,
    ambiguous: EqualityConstraintExposureError,
) -> Result<Option<DefId>, EqualityConstraintExposureError> {
    match candidates.as_slice() {
        [] => Ok(None),
        [candidate] => Ok(Some(*candidate)),
        _ => Err(ambiguous),
    }
}

fn push_unique(candidates: &mut Vec<DefId>, candidate: DefId) {
    if !candidates.contains(&candidate) {
        candidates.push(candidate);
    }
}

fn append_unique(candidates: &mut Vec<DefId>, additions: Vec<DefId>) {
    for candidate in additions {
        push_unique(candidates, candidate);
    }
}
