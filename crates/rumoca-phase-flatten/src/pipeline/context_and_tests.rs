use super::context_import_shadowing::{
    EffectiveExpressionContext, imports_without_shadowed_aliases,
    qualify_expression_with_effective_imports,
};
use super::enum_dimensions::{enum_type_dimension, infer_enum_range_dimensions};
use super::function_overrides_and_dims::*;
use super::*;

mod alias_lookup;
mod array_dimensions;
mod class_instance;
mod component_dimensions;
mod component_instance;
mod enum_params;
mod numeric_params;
mod param_binding;
mod parameter_lookup;
mod qualification;

pub(crate) use alias_lookup::*;
pub(crate) use class_instance::*;
pub(crate) use component_instance::*;
pub(crate) use qualification::*;

pub(super) fn resolved_path_has_import_alias(resolved_path: &str, alias: &str) -> bool {
    rumoca_core::top_level_last_segment(resolved_path) == alias
}
