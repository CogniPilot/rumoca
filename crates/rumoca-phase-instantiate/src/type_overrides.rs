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

pub(super) use class_hierarchy::find_nested_class_in_hierarchy;
pub(super) use component_class_overrides::extract_component_class_overrides;
pub(super) use component_redeclare_validation::validate_component_class_redeclare_target;
pub(super) use component_type_selection::apply_type_override;
pub(super) use deferred_references::{
    SelectedComponentTypes, resolve_dynamic_equation_targets, resolve_dynamic_expression_targets,
    resolve_dynamic_statement_targets,
};
pub(super) use override_collection::build_type_override_map;
pub(super) use override_map::TypeOverrideMap;
pub(super) use redeclare_modifiers::class_redeclare_modifier_args;
pub(super) use redeclare_values::resolve_redeclare_value_def_id;
pub(super) use selected_class_members::resolve_class_override_modifier_targets;
