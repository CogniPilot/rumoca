//! Replaceable class/function override rewriting: collecting the alias
//! targets a scope selects, and retargeting calls and member references onto
//! the selected implementations.

use super::*;
use crate::path_utils::{enclosing_scope, leaf_segment};
use crate::source_spans::required_location_span;
use rumoca_core::{ComponentPath, ExpressionRewriter, StatementRewriter};
use rumoca_ir_ast::ExpressionTransformer;
use rustc_hash::FxHashSet;

mod constructor_aliases;
mod exact_identity_queries;
mod expression_rewrite;
mod flat_rewrite;
mod function_selection;
mod member_calls;
mod member_references;
mod named_args;
mod override_map;
mod override_scope;
mod override_target;
mod package_chain;
mod redeclare_aliases;
mod replaceable_modifiers;
mod rewrite_context;
mod scoped_member_name;

pub(crate) use constructor_aliases::collect_component_constructor_aliases;
use constructor_aliases::{
    collect_component_constructor_aliases_for_class, resolve_class_ref_name,
    resolve_package_alias_chain,
};
use exact_identity_queries::{
    collect_function_exposures_for_implementation, exact_function_member_name,
    exact_package_chain_contains_def_id, exact_package_function_exposure,
    exact_prefix_owner_def_id, function_alias_requires_exact_selection,
    resolve_function_extends_target_def_id,
};
use expression_rewrite::{
    FunctionOverrideExpressionRewriter, expression_contains_function_call, function_local_def_ids,
    rewritten_function_reference,
};
pub(crate) use flat_rewrite::*;
use function_selection::{
    CallOccurrenceIdentity, FunctionSelection, ResolvedFunctionRewrite,
    resolve_exact_function_rewrite,
};
pub(crate) use member_calls::*;
use member_references::{
    canonical_instance_reference_name, reference_source_package_def_id_from_index,
    resolve_override_member_name,
};
use named_args::{named_function_arg, named_function_arg_names};
pub(crate) use override_map::build_component_override_map;
#[cfg(test)]
use override_map::component_class_override_is_active;
#[cfg(test)]
pub(crate) use override_map::component_overrides;
use override_scope::override_context_cache_key;
pub(crate) use override_scope::{
    override_aliases_for_component_path, override_context_for_component_path,
    override_context_for_scope, override_package_names,
    override_package_names_with_preferred_aliases,
};
pub(crate) use override_target::{ComponentOverrideMap, OverrideTarget};
use override_target::{
    FunctionModifierArg, OverrideContext, OverrideFunctionMap, ResolvedClassRef,
    function_modifier_arg_from_ast, is_receiver_alias_type, resolved_class_ref_for_def_id,
};
pub(crate) use package_chain::{
    collect_package_chain, package_chain_contains, resolve_function_in_package_chain,
    resolve_function_in_package_chain_exposed,
};
use package_chain::{package_chain_contains_def_id, resolve_member_in_package_chain_exposed};
use redeclare_aliases::collect_extends_redeclare_aliases_for_class;
pub(crate) use redeclare_aliases::extends_class_redeclare_target;
use replaceable_modifiers::{append_replaceable_function_modifier_args, single_component_ref_name};
pub(crate) use rewrite_context::FunctionOverrideRewriteContext;
use scoped_member_name::scoped_override_component_member_name;

#[cfg(test)]
mod tests;

pub(crate) fn rewrite_function_overrides_in_expression_with_ctx(
    expr: &mut Expression,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Result<(), FlattenError> {
    if ctx.override_packages.is_empty()
        && ctx.override_functions.is_empty()
        && !expression_contains_function_call(expr)
    {
        return Ok(());
    }
    let mut rewriter = FunctionOverrideExpressionRewriter::new(ctx);
    *expr = rewriter.rewrite_expression(expr);
    rewriter.finish()
}
