//! Redeclaration and virtual-class selection for the instantiate phase
//! (MLS §7.3).
//!
//! Instantiation owns the concrete selection for every replaceable class or
//! package alias. The submodules split that work by responsibility:
//!
//! - [`override_map`]: the effective alias-to-target selections of one scope.
//! - [`override_collection`]: gathering those selections from a class context.
//! - [`class_hierarchy`]: extends-chain lookups the collection relies on.
//! - [`redeclare_values`]: proving the class identity of a redeclare value.
//! - [`redeclare_modifiers`]: structural reads over redeclare modifiers.
//! - [`component_type_selection`]: applying selections to component types.
//! - [`component_class_overrides`] and [`component_redeclare_validation`]:
//!   extracting and validating component-level class redeclarations.
//! - [`deferred_references`] and [`selected_class_members`]: re-proving
//!   references Resolve deferred across a replaceable edge.

mod class_hierarchy;
mod component_class_overrides;
mod component_redeclare_validation;
mod component_type_selection;
mod deferred_references;
mod override_collection;
mod override_map;
mod redeclare_modifiers;
mod redeclare_values;
mod selected_class_members;

#[cfg(test)]
mod tests;

pub(super) use class_hierarchy::{
    contains_component_by_def_id_in_hierarchy, find_component_by_def_id_in_hierarchy,
    find_nested_class_by_def_id_in_hierarchy,
};
pub(super) use component_class_overrides::extract_component_class_overrides;
pub(super) use component_redeclare_validation::validate_component_class_redeclare_target;
pub(super) use deferred_references::{
    SelectedComponentTypeCatalog, SelectedComponentTypes, component_with_issued_type_selection,
    issue_selected_component_types, resolve_dynamic_equation_targets_at_occurrence,
    resolve_dynamic_expression_targets_at_occurrence,
    resolve_dynamic_statement_targets_at_occurrence,
    resolve_dynamic_subscript_targets_at_occurrence,
};
pub(super) use override_collection::build_type_override_map;
pub(super) use override_map::TypeOverrideMap;
pub(super) use redeclare_modifiers::{
    class_redeclare_alias_ref, class_redeclare_modifier_args, direct_source_redeclare,
};
pub(super) use redeclare_values::{resolve_cref_def_id, resolve_redeclare_value_def_id};
pub(super) use selected_class_members::resolve_class_override_modifier_targets;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ExactSourceForwardingWitness {
    lhs_slot_def_id: rumoca_core::DefId,
    rhs_alias_def_id: rumoca_core::DefId,
    effective_target_def_id: rumoca_core::DefId,
}

impl ExactSourceForwardingWitness {
    pub(super) fn lhs_slot_def_id(self) -> rumoca_core::DefId {
        self.lhs_slot_def_id
    }

    #[cfg(test)]
    pub(super) fn rhs_alias_def_id(self) -> rumoca_core::DefId {
        self.rhs_alias_def_id
    }

    pub(super) fn effective_target_def_id(self) -> rumoca_core::DefId {
        self.effective_target_def_id
    }
}

/// Exact evidence for one source-level self-forwarding redeclare.
///
/// There is deliberately no modification-environment input. Resolve has
/// already issued the RHS declaration in the modifier's lexical scope, and
/// `type_overrides` is the sole authority that maps that alias to its active
/// class. Looking up the RHS spelling in mutable modifier state would create a
/// second authority and can misclassify legal `Medium = Medium` as a cycle.
pub(super) struct SourceForwardingEvidence<'a> {
    pub(super) tree: &'a rumoca_ir_ast::ClassTree,
    pub(super) type_overrides: &'a TypeOverrideMap,
    pub(super) is_redeclare: bool,
    pub(super) source: &'a rumoca_ir_ast::Expression,
    pub(super) resolved: &'a rumoca_ir_ast::Expression,
    pub(super) target_name: &'a str,
    pub(super) alias_def_id: rumoca_core::DefId,
}

#[derive(Debug)]
pub(super) enum SourceForwardingEvidenceError {
    MissingResolvedLhsIdentity,
    MismatchedResolvedLhsIdentity {
        expected: rumoca_core::DefId,
        found: rumoca_core::DefId,
    },
    MissingResolvedRhsIdentity,
    MissingActiveRhsAlias(rumoca_core::DefId),
    ContradictorySourceAndResolvedShape,
    Resolution(Box<crate::InstantiateError>),
}

impl std::fmt::Display for SourceForwardingEvidenceError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingResolvedLhsIdentity => {
                formatter.write_str("self-forwarding redeclare LHS has no exact resolved DefId")
            }
            Self::MismatchedResolvedLhsIdentity { expected, found } => write!(
                formatter,
                "self-forwarding redeclare LHS identifies {found:?}, but the active alias slot identifies {expected:?}"
            ),
            Self::MissingResolvedRhsIdentity => {
                formatter.write_str("self-forwarding redeclare RHS has no exact resolved DefId")
            }
            Self::MissingActiveRhsAlias(def_id) => write!(
                formatter,
                "self-forwarding redeclare RHS {def_id:?} has no exact active alias mapping"
            ),
            Self::ContradictorySourceAndResolvedShape => formatter.write_str(
                "source and resolved redeclare evidence disagree on self-forwarding syntax",
            ),
            Self::Resolution(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

pub(super) fn checked_source_forwarding_witness(
    evidence: SourceForwardingEvidence<'_>,
) -> Result<Option<ExactSourceForwardingWitness>, SourceForwardingEvidenceError> {
    let SourceForwardingEvidence {
        tree,
        type_overrides,
        is_redeclare,
        source,
        resolved,
        target_name,
        alias_def_id,
    } = evidence;
    if !is_redeclare
        || redeclare_modifiers::component_source_modifier_target_name(source).as_deref()
            != Some(target_name)
    {
        return Ok(None);
    }
    let source_is_self =
        redeclare_modifiers::is_forwarding_component_redeclare(source, target_name);
    let resolved_is_self =
        redeclare_modifiers::is_forwarding_component_redeclare(resolved, target_name);
    match (source_is_self, resolved_is_self) {
        (true, true) => {}
        (false, true) => {
            return Err(SourceForwardingEvidenceError::ContradictorySourceAndResolvedShape);
        }
        (_, false) => return Ok(None),
    }
    let source_alias = redeclare_modifiers::class_redeclare_alias_ref(source)
        .ok_or(SourceForwardingEvidenceError::MissingResolvedLhsIdentity)?;
    let source_alias_def_id = redeclare_values::resolve_cref_def_id(source_alias)
        .ok_or(SourceForwardingEvidenceError::MissingResolvedLhsIdentity)?;
    if source_alias_def_id != alias_def_id {
        return Err(
            SourceForwardingEvidenceError::MismatchedResolvedLhsIdentity {
                expected: alias_def_id,
                found: source_alias_def_id,
            },
        );
    }
    let rhs_alias_def_id = redeclare_values::resolve_redeclare_value_def_id(tree, resolved, None)
        .map_err(SourceForwardingEvidenceError::Resolution)?
        .ok_or(SourceForwardingEvidenceError::MissingResolvedRhsIdentity)?;
    let effective_target_def_id = type_overrides
        .checked_target_for_alias_def_id(tree, rhs_alias_def_id, resolved.span())
        .map_err(SourceForwardingEvidenceError::Resolution)?
        .ok_or(SourceForwardingEvidenceError::MissingActiveRhsAlias(
            rhs_alias_def_id,
        ))?;
    Ok(Some(ExactSourceForwardingWitness {
        lhs_slot_def_id: source_alias_def_id,
        rhs_alias_def_id,
        effective_target_def_id,
    }))
}
