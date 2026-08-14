//! rumoca_core::Function collection and flattening for user-defined functions.
//!
//! This module is responsible for:
//! - Collecting function calls used in the model
//! - Looking up function definitions from the ast::ClassTree
//! - Converting function definitions to rumoca_core::Function
//!
//! Per MLS §12, functions in Modelica are callable units with:
//! - Input parameters (values passed in)
//! - Output parameters (values returned)
//! - An algorithm section (the function body)
//!
mod call_args;
mod call_canonicalization;
mod call_collection;
mod callable_scope_identity;
mod constructor_signature;
mod deferred_members;
mod function_context;
mod function_metadata;
mod function_output_validation;
mod function_param_alias;
mod function_requests;
#[cfg(test)]
mod tests;

use indexmap::IndexSet;
#[cfg(test)]
use rumoca_core::Span;
use rumoca_core::{ExpressionRewriter, ExpressionVisitor, StatementRewriter};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashSet;
use std::collections::{HashMap, HashSet};

pub(crate) use call_args::materialize_flat_function_call_args;
pub(crate) use call_canonicalization::{
    canonicalize_collected_function_calls, canonicalize_function_calls_in_expression_with_scope,
};
use call_collection::collect_function_call_requests;
#[cfg(test)]
use call_collection::collect_function_calls;
pub(crate) use call_collection::collect_function_dep_requests;
use constructor_signature::{
    convert_constructor_signature, inherit_operator_constructor_defaults,
    normalize_function_local_references,
};
use function_context::{
    collect_function_context, collect_lexical_constant_aliases, extend_imports_if_absent,
    function_initial_import_map, resolve_import_pairs,
};
pub(crate) use function_metadata::FunctionTypeCatalog;
use function_metadata::*;
pub(crate) use function_metadata::{
    lower_record_function_params, specialize_static_function_params,
};
use function_output_validation::validate_function_outputs_assigned;
use function_param_alias::function_param_type_alias_dims;
use function_requests::{FunctionIdentitySet, same_function_request};
pub(crate) use function_requests::{FunctionRequest, FunctionRequests};

use crate::algorithms;
use crate::ast_lower;
use crate::errors::FlattenError;
use crate::function_lowering::rewrite_record_field_access_in_body;
use crate::path_utils;
use crate::pipeline::{collect_package_chain, rewrite_function_extends_aliases_in_function};
use crate::qualify;
use crate::source_spans::required_location_span;

fn is_callable_class_candidate(class_type: &rumoca_core::ClassType) -> bool {
    !matches!(
        class_type,
        rumoca_core::ClassType::Package
            | rumoca_core::ClassType::Connector
            | rumoca_core::ClassType::Operator
    )
}

pub(crate) fn record_type_fields(
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    qualified_name: &str,
    tree: &ast::ClassTree,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Vec<flat::RecordField>, FlattenError> {
    let constructor = convert_constructor_signature(
        class_index,
        class_def,
        qualified_name,
        &tree.source_map,
        ast_lower::PredefinedIntrinsicIds::from_tree(tree),
        type_catalog,
    )?;
    constructor
        .inputs
        .into_iter()
        .map(|field| {
            let def_id = field.def_id.ok_or_else(|| {
                FlattenError::missing_resolved_class_metadata(
                    format!("{qualified_name}.{}", field.name),
                    "record field identity",
                    field.span,
                )
            })?;
            Ok(flat::RecordField {
                name: field.name.clone(),
                def_id,
                dims: field.dimensions().to_vec(),
            })
        })
        .collect()
}

fn class_by_name_or_def_id<'a>(
    class_index: &ast::ClassDefIndex<'a>,
    name: &str,
    def_id: Option<rumoca_core::DefId>,
) -> Option<&'a ast::ClassDef> {
    def_id
        .and_then(|def_id| class_index.get(def_id))
        .or_else(|| class_index.get_by_qualified_name(name))
}

/// Collect and flatten all function definitions used by the model.
///
/// This finds all function calls in the model, looks up their definitions
/// in the ast::ClassTree, and converts them to rumoca_core::Function objects.
pub(crate) fn collect_functions(
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: Option<&str>,
) -> Result<(), FlattenError> {
    let type_catalog = FunctionTypeCatalog::new(overlay);
    let mut member_cache = qualify::MemberDefIdCache::default();
    let initial_calls = collect_function_call_requests(flat);
    let mut pending: Vec<(FunctionRequest, Option<String>)> = initial_calls
        .iter()
        .cloned()
        .map(|request| (request, caller_scope.map(str::to_string)))
        .collect();
    pending.extend(
        flat.functions
            .keys()
            .map(|name| (FunctionRequest::from_name(name.as_str().to_string()), None)),
    );
    let mut requested: Vec<(FunctionRequest, Option<String>)> = Vec::new();
    let mut expanded = FunctionIdentitySet::default();
    let mut inserted: Vec<String> = flat
        .functions
        .keys()
        .map(|n| n.as_str().to_string())
        .collect();
    for request in &initial_calls {
        insert_unique_name(&mut inserted, &request.name);
    }

    while let Some((request, caller_scope)) = pending.pop() {
        if request_seen_in_scope(&requested, &request, caller_scope.as_deref()) {
            continue;
        }
        requested.push((request.clone(), caller_scope.clone()));

        if let Some(qualified_name) = existing_executable_flat_function(flat, &request.name)
            .map(|existing| existing.name.as_str().to_string())
        {
            let existing = flat
                .functions
                .get_mut(&rumoca_core::VarName::new(&qualified_name))
                .expect("the selected existing function remains in its table");
            refine_existing_function_nonreplaceability(existing, class_index, &request);
            if !expanded.insert_function(existing) {
                continue;
            }
            queue_unseen_function_dependencies(&mut pending, &requested, &qualified_name, existing);
            insert_unique_name(&mut inserted, &qualified_name);
            continue;
        }

        let resolved = if let Some(resolved) = lookup_function_request_with_scope(
            tree,
            class_index,
            &request,
            caller_scope.as_deref(),
            &mut member_cache,
            type_catalog,
        )? {
            if is_executable_flat_function(&resolved.1) {
                Some(resolved)
            } else {
                lookup_function_in_known_packages(
                    tree,
                    class_index,
                    &request.name,
                    &inserted,
                    &mut member_cache,
                    type_catalog,
                )?
            }
        } else {
            flat.functions
                .get(&rumoca_core::VarName::new(request.name.clone()))
                .cloned()
                .map(|f| (f.name.as_str().to_string(), f))
        };
        let resolved = match resolved {
            Some(resolved) => Some(resolved),
            None => lookup_function_in_known_packages(
                tree,
                class_index,
                &request.name,
                &inserted,
                &mut member_cache,
                type_catalog,
            )?,
        };
        let Some((qualified_name, flat_func)) = resolved else {
            // If not found or not a function type, it might be:
            // - An external function (MLS §12.9)
            // - A library function we don't have the source for
            // - A record constructor or operator function (MLS §14)
            // Code generators handle these cases or error appropriately
            continue;
        };

        retain_constructor_record_type(flat, &flat_func)?;

        if !expanded.insert_function(&flat_func) {
            continue;
        }

        for dep in collect_function_dep_requests(&flat_func) {
            if !request_seen_in_scope(&requested, &dep, Some(&qualified_name)) {
                pending.push((dep, Some(qualified_name.clone())));
            }
        }
        insert_unique_name(&mut inserted, &qualified_name);
        if existing_executable_flat_function(flat, flat_func.name.as_str()).is_some() {
            continue;
        }
        flat.add_function(flat_func);
    }

    // Precollection can seed constructors before the reachability worklist.
    // Reconcile the complete retained function set at the phase boundary so
    // every constructor, independent of discovery route, contributes its
    // compact aggregate layout.
    retain_discovered_constructor_types(flat)
}

fn retain_discovered_constructor_types(flat: &mut flat::Model) -> Result<(), FlattenError> {
    let constructors = flat
        .functions
        .values()
        .filter(|function| function.is_constructor)
        .cloned()
        .collect::<Vec<_>>();
    for constructor in &constructors {
        retain_constructor_record_type(flat, constructor)?;
    }
    Ok(())
}

/// Retain the compact record layout whenever function reachability discovers
/// its constructor, even if the model has no record-valued component instance.
///
/// Calls can return and immediately project a record (for example
/// `bodyTwist(state).bodyLinearVelocity`).  Such a value never passes through
/// component-instance flattening, so its constructor signature is the Flat
/// producer that owns the downstream field-layout proof.
fn retain_constructor_record_type(
    flat: &mut flat::Model,
    function: &rumoca_core::Function,
) -> Result<(), FlattenError> {
    if !function.is_constructor {
        return Ok(());
    }
    let record = function.def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            function.name.as_str(),
            "record constructor identity",
            function.span,
        )
    })?;
    let fields = function
        .inputs
        .iter()
        .map(|field| {
            let def_id = field.def_id.ok_or_else(|| {
                FlattenError::missing_resolved_class_metadata(
                    format!("{}.{}", function.name, field.name),
                    "record field identity",
                    field.span,
                )
            })?;
            Ok(flat::RecordField {
                name: field.name.clone(),
                def_id,
                dims: field.dimensions().to_vec(),
            })
        })
        .collect::<Result<Vec<_>, FlattenError>>()?;

    if let Some(existing) = flat.record_types.get(&record) {
        if existing.fields != fields {
            return Err(FlattenError::missing_resolved_class_metadata(
                function.name.as_str(),
                "one exact record layout for the constructor declaration",
                function.span,
            ));
        }
        return Ok(());
    }
    flat.record_types.insert(
        record,
        flat::RecordType {
            name: function.name.as_str().to_string(),
            fields,
        },
    );
    Ok(())
}

fn refine_existing_function_nonreplaceability(
    existing: &mut rumoca_core::Function,
    class_index: &ast::ClassDefIndex<'_>,
    request: &FunctionRequest,
) {
    // Name-only requests are synthetic worklist entries used to expand an
    // already-seeded body. They carry no exposure-path evidence and therefore
    // cannot mint or revoke the constructor's certificate. A real structured
    // occurrence remains proof-bearing in both directions, including a
    // replaceable exposure path.
    if request.component_ref.is_some() {
        existing.transitively_non_replaceable &=
            request_proves_transitive_non_replaceability(class_index, request);
    }
}

fn existing_executable_flat_function<'model>(
    flat: &'model flat::Model,
    func_name: &str,
) -> Option<&'model rumoca_core::Function> {
    let function = flat.functions.get(&rumoca_core::VarName::new(func_name))?;
    is_executable_flat_function(function).then_some(function)
}

fn function_by_request<'model>(
    flat: &'model flat::Model,
    request: &FunctionRequest,
) -> Option<&'model rumoca_core::Function> {
    if let Some(function) = flat
        .functions
        .get(&rumoca_core::VarName::new(&request.name))
    {
        return Some(function);
    }
    if let Some(def_id) = request.target_def_id
        && let Some(function) = flat
            .functions
            .values()
            .find(|function| function.def_id == Some(def_id))
    {
        return Some(function);
    }
    None
}

fn request_seen_in_scope(
    requested: &[(FunctionRequest, Option<String>)],
    request: &FunctionRequest,
    caller_scope: Option<&str>,
) -> bool {
    requested.iter().any(|(existing, existing_scope)| {
        same_function_request(existing, request) && existing_scope.as_deref() == caller_scope
    })
}

fn queue_unseen_function_dependencies(
    pending: &mut Vec<(FunctionRequest, Option<String>)>,
    requested: &[(FunctionRequest, Option<String>)],
    qualified_name: &str,
    function: &rumoca_core::Function,
) {
    for dep in collect_function_dep_requests(function) {
        if !request_seen_in_scope(requested, &dep, Some(qualified_name)) {
            pending.push((dep, Some(qualified_name.to_string())));
        }
    }
}

fn insert_unique_name(names: &mut Vec<String>, name: &str) {
    if !names.iter().any(|existing| existing == name) {
        names.push(name.to_string());
    }
}

pub(crate) fn prune_unreachable_functions(flat: &mut flat::Model) {
    let mut reachable = FunctionIdentitySet::default();
    let mut pending = collect_function_call_requests(flat);
    for request in &pending {
        reachable.insert_request(request);
    }

    while let Some(request) = pending.pop() {
        let Some(function) = function_by_request(flat, &request) else {
            continue;
        };
        for dependency in collect_function_dep_requests(function) {
            if reachable.insert_request(&dependency) {
                pending.push(dependency);
            }
        }
    }

    flat.functions.retain(|name, function| {
        reachable.contains_function(function) || reachable.contains_name(name.as_str())
    });
}

pub(crate) fn validate_flat_function_bindings(flat: &flat::Model) -> Result<(), FlattenError> {
    for function in flat.functions.values() {
        if is_executable_flat_function(function) {
            continue;
        }
        return Err(FlattenError::function_without_body(
            function.name.as_str(),
            function.span,
        ));
    }
    Ok(())
}

pub(crate) fn is_executable_flat_function(function: &rumoca_core::Function) -> bool {
    function.is_constructor
        || function.external.is_some()
        || !function.body.is_empty()
        || function
            .outputs
            .iter()
            .any(|output| output.default.is_some())
}

fn lookup_function_with_scope<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    func_name: &str,
    caller_scope: Option<&str>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let Some(resolved) =
        resolve_function_class_with_scope(tree, class_index, func_name, caller_scope)
    else {
        return Ok(None);
    };
    let flat_func = convert_callable(
        tree,
        class_index,
        resolved.class_def,
        &resolved.exposed_name,
        &tree.source_map,
        member_cache,
        type_catalog,
    )?;
    let Some(flat_func) = flat_func else {
        return Ok(None);
    };
    Ok(Some((resolved.exposed_name, flat_func)))
}

fn lookup_function_request_with_scope<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    request: &FunctionRequest,
    caller_scope: Option<&str>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let mut resolved = lookup_function_request_with_scope_uncertified(
        tree,
        class_index,
        request,
        caller_scope,
        member_cache,
        type_catalog,
    )?;
    if let Some((_, function)) = &mut resolved {
        function.transitively_non_replaceable =
            request_proves_transitive_non_replaceability(class_index, request);
    }
    Ok(resolved)
}

fn lookup_function_request_with_scope_uncertified<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    request: &FunctionRequest,
    caller_scope: Option<&str>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    if let Some(resolved) = lookup_exposed_function_request_by_name(
        tree,
        class_index,
        request,
        caller_scope,
        member_cache,
        type_catalog,
    )? {
        return Ok(Some(resolved));
    }
    if let Some(def_id) = request.target_def_id
        && let Some(class_def) = class_index.get(def_id)
        && is_callable_class_candidate(&class_def.class_type)
        && !class_def.partial
    {
        let exposed_name =
            request_exposed_qualified_name(class_index, request).unwrap_or_else(|| {
                class_index
                    .qualified_name(def_id)
                    .unwrap_or(request.name.as_str())
                    .to_string()
            });
        if let Some(flat_func) = convert_callable(
            tree,
            class_index,
            class_def,
            &exposed_name,
            &tree.source_map,
            member_cache,
            type_catalog,
        )? && is_executable_flat_function(&flat_func)
        {
            return Ok(Some((exposed_name, flat_func)));
        }
    }

    lookup_function_with_scope(
        tree,
        class_index,
        &request.name,
        caller_scope,
        member_cache,
        type_catalog,
    )
}

fn request_exposed_qualified_name(
    class_index: &ast::ClassDefIndex<'_>,
    request: &FunctionRequest,
) -> Option<String> {
    let reference = request.component_ref.as_ref()?;
    let scope = reference.component_scope();
    let owner = scope.prefix_parts().last()?;
    let owner_name = class_index.qualified_name(owner.def_id)?;
    Some(format!("{owner_name}.{}", scope.leaf_ident()?))
}

fn request_proves_transitive_non_replaceability(
    class_index: &ast::ClassDefIndex<'_>,
    request: &FunctionRequest,
) -> bool {
    request.component_ref.as_ref().is_some_and(|reference| {
        !reference.parts().is_empty()
            && reference
                .parts()
                .iter()
                .all(|part| !part.ident.contains('.'))
            && class_index.proves_transitively_non_replaceable_path(
                reference.parts().iter().map(|part| part.def_id),
            )
    })
}

fn lookup_exposed_function_request_by_name<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    request: &FunctionRequest,
    caller_scope: Option<&str>,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let Some(def_id) = request.target_def_id else {
        return Ok(None);
    };
    if class_index
        .qualified_name(def_id)
        .is_some_and(|canonical_name| canonical_name == request.name)
    {
        return Ok(None);
    }
    let Some(resolved) =
        resolve_function_class_with_scope(tree, class_index, &request.name, caller_scope)
    else {
        return Ok(None);
    };
    if resolved.class_def.def_id != Some(def_id)
        || !is_callable_class_candidate(&resolved.class_def.class_type)
        || resolved.class_def.partial
    {
        return Ok(None);
    }
    let Some(flat_func) = convert_callable(
        tree,
        class_index,
        resolved.class_def,
        &resolved.exposed_name,
        &tree.source_map,
        member_cache,
        type_catalog,
    )?
    else {
        return Ok(None);
    };
    Ok(is_executable_flat_function(&flat_func).then_some((resolved.exposed_name, flat_func)))
}

pub(crate) fn lookup_function_request(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    request: &FunctionRequest,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let mut member_cache = qualify::MemberDefIdCache::default();
    lookup_function_request_with_scope(
        tree,
        class_index,
        request,
        None,
        &mut member_cache,
        type_catalog,
    )
}

pub(crate) struct FunctionClassResolution<'a> {
    pub(crate) exposed_name: String,
    pub(crate) class_def: &'a ast::ClassDef,
}

/// Resolve alias-style function names (e.g. `Medium.dynamicViscosity`) by
/// reusing package prefixes already present in the model's known function set.
fn lookup_function_in_known_packages<'tree>(
    tree: &'tree ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    func_name: &str,
    known_functions: &[String],
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<(String, rumoca_core::Function)>, FlattenError> {
    let Some((_first, remainder)) = path_utils::root_split(func_name) else {
        return Ok(None);
    };
    if remainder.is_empty() {
        return Ok(None);
    }

    let mut matched: Option<String> = None;
    for known in known_functions {
        let Some(pkg_prefix) = path_utils::enclosing_scope(known) else {
            continue;
        };
        if resolve_function_in_package_chain_class(tree, class_index, pkg_prefix, remainder)
            .is_none()
        {
            continue;
        }
        let candidate = format!("{pkg_prefix}.{remainder}");
        if matched
            .as_ref()
            .is_some_and(|existing| existing != &candidate)
        {
            return Ok(None);
        }
        matched = Some(candidate);
    }

    let Some(qualified_name) = matched else {
        return Ok(None);
    };
    let Some((class_def, _source_name)) =
        path_utils::scope_split(&qualified_name).and_then(|(package, leaf)| {
            resolve_function_in_package_chain_class(tree, class_index, package, leaf)
        })
    else {
        return Ok(None);
    };
    let flat_func = convert_callable(
        tree,
        class_index,
        class_def,
        &qualified_name,
        &tree.source_map,
        member_cache,
        type_catalog,
    )?;
    let Some(flat_func) = flat_func else {
        return Ok(None);
    };
    if !is_executable_flat_function(&flat_func) {
        return Ok(None);
    }
    Ok(Some((qualified_name, flat_func)))
}

pub(crate) fn resolve_function_class_with_scope<'a>(
    tree: &'a ast::ClassTree,
    class_index: &ast::ClassDefIndex<'a>,
    func_name: &str,
    caller_scope: Option<&str>,
) -> Option<FunctionClassResolution<'a>> {
    if let Some(class_def) = class_index.get_by_qualified_name(func_name)
        && is_callable_class_candidate(&class_def.class_type)
    {
        if class_def.partial
            && let Some(caller_scope) = caller_scope
        {
            let short_name = path_utils::leaf_segment(func_name);
            if let Some(scoped_match) =
                resolve_function_in_caller_packages(tree, class_index, caller_scope, short_name)
                && scoped_match != func_name
            {
                return resolve_function_class_with_scope(
                    tree,
                    class_index,
                    &scoped_match,
                    Some(caller_scope),
                );
            }
        }
        return Some(FunctionClassResolution {
            exposed_name: func_name.to_string(),
            class_def,
        });
    }

    if let Some((package_name, function_leaf)) = path_utils::scope_split(func_name)
        && let Some((class_def, _source_name)) =
            resolve_function_in_package_chain_class(tree, class_index, package_name, function_leaf)
    {
        return Some(FunctionClassResolution {
            exposed_name: func_name.to_string(),
            class_def,
        });
    }

    if let Some(caller_scope) = caller_scope
        && let Some(scoped_match) =
            resolve_function_path_in_caller_packages(tree, class_index, caller_scope, func_name)
    {
        return resolve_function_class_with_scope(
            tree,
            class_index,
            &scoped_match,
            Some(caller_scope),
        );
    }

    let short_name = path_utils::leaf_segment(func_name);
    if short_name != func_name
        && let Some(caller_scope) = caller_scope
        && let Some(scoped_match) =
            resolve_function_in_caller_packages(tree, class_index, caller_scope, short_name)
    {
        return resolve_function_class_with_scope(
            tree,
            class_index,
            &scoped_match,
            Some(caller_scope),
        );
    }

    None
}

fn resolve_function_path_in_caller_packages(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: &str,
    func_path: &str,
) -> Option<String> {
    let mut visited = HashSet::new();
    resolve_function_path_in_caller_packages_inner(
        tree,
        class_index,
        caller_scope,
        func_path,
        &mut visited,
    )
}

fn resolve_function_path_in_caller_packages_inner(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: &str,
    func_path: &str,
    visited: &mut HashSet<String>,
) -> Option<String> {
    if !visited.insert(caller_scope.to_string()) {
        return None;
    }

    if let Some(class_def) = class_index.get_by_qualified_name(caller_scope) {
        for ext in &class_def.extends {
            let base_name = ext.base_name.to_string();
            if let Some(base_scope) =
                crate::pipeline::resolve_extends_base_qname(class_index, &base_name, caller_scope)
                && let Some(resolved) = resolve_function_path_in_caller_packages_inner(
                    tree,
                    class_index,
                    &base_scope,
                    func_path,
                    visited,
                )
            {
                return Some(resolved);
            }
        }
    }

    if !path_utils::is_nested_name(func_path) {
        return resolve_function_in_caller_packages(tree, class_index, caller_scope, func_path);
    }

    for prefix in path_utils::enclosing_scopes(caller_scope) {
        let candidate = format!("{prefix}.{func_path}");
        if let Some((package_name, function_leaf)) = path_utils::scope_split(&candidate)
            && resolve_function_in_package_chain_class(
                tree,
                class_index,
                package_name,
                function_leaf,
            )
            .is_some()
        {
            return Some(candidate);
        }
    }
    None
}

fn resolve_function_in_package_chain_class<'a>(
    tree: &'a ast::ClassTree,
    class_index: &ast::ClassDefIndex<'a>,
    package_name: &str,
    function_leaf: &str,
) -> Option<(&'a ast::ClassDef, String)> {
    fn resolve_inner<'a>(
        tree: &'a ast::ClassTree,
        class_index: &ast::ClassDefIndex<'a>,
        package_name: &str,
        function_leaf: &str,
        visited: &mut HashSet<String>,
    ) -> Option<(&'a ast::ClassDef, String)> {
        if !visited.insert(package_name.to_string()) {
            return None;
        }

        let direct = format!("{package_name}.{function_leaf}");
        if let Some(class_def) = class_index.get_by_qualified_name(&direct)
            && is_callable_class_candidate(&class_def.class_type)
        {
            return Some((class_def, direct));
        }

        let package_class =
            if let Some(package_class) = class_index.get_by_qualified_name(package_name) {
                package_class
            } else {
                let (owner_scope, exposed_package) = path_utils::scope_split(package_name)?;
                let owner = class_index.get_by_qualified_name(owner_scope)?;
                let target = crate::pipeline::extends_class_redeclare_target(
                    tree,
                    class_index,
                    owner,
                    owner_scope,
                    exposed_package,
                )?;
                let target_name = target.def_id.and_then(|def_id| tree.def_map.get(&def_id))?;
                return resolve_inner(tree, class_index, target_name, function_leaf, visited);
            };
        // MLS §7.3: an extends-clause class redeclare replaces the inherited
        // member, so it wins over the (possibly partial) lexical base member.
        if let Some(target) = crate::pipeline::extends_class_redeclare_target(
            tree,
            class_index,
            package_class,
            package_name,
            function_leaf,
        ) && is_callable_class_candidate(&target.class_type)
        {
            return Some((target, direct));
        }
        for ext in &package_class.extends {
            let base_name = ext.base_name.to_string();
            let resolved_base = ext
                .base_def_id
                .and_then(|def_id| tree.def_map.get(&def_id).cloned())
                .or_else(|| {
                    crate::resolve_class_in_scope_indexed(class_index, &base_name, package_name).1
                })
                .or_else(|| {
                    class_index
                        .get_by_qualified_name(&base_name)
                        .map(|_| base_name.clone())
                });
            if let Some(base) = resolved_base
                && let Some(resolved) =
                    resolve_inner(tree, class_index, &base, function_leaf, visited)
            {
                return Some(resolved);
            }
        }

        None
    }

    let mut visited = HashSet::new();
    resolve_inner(tree, class_index, package_name, function_leaf, &mut visited)
}

fn resolve_function_in_caller_packages(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    caller_scope: &str,
    short_name: &str,
) -> Option<String> {
    let mut prefix = path_utils::enclosing_scope(caller_scope)?;
    loop {
        let candidate = format!("{prefix}.{short_name}");
        if let Some((package_name, function_leaf)) = path_utils::scope_split(&candidate)
            && resolve_function_in_package_chain_class(
                tree,
                class_index,
                package_name,
                function_leaf,
            )
            .is_some()
        {
            return Some(candidate);
        }
        let Some(parent) = path_utils::enclosing_scope(prefix) else {
            break;
        };
        prefix = parent;
    }
    None
}

fn convert_callable<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<rumoca_core::Function>, FlattenError> {
    match &class_def.class_type {
        rumoca_core::ClassType::Function => convert_function(
            tree,
            class_index,
            class_def,
            qualified_name,
            source_map,
            member_cache,
            type_catalog,
        )
        .map(Some),
        rumoca_core::ClassType::Record => {
            let mut constructor = convert_constructor_signature(
                class_index,
                class_def,
                qualified_name,
                source_map,
                ast_lower::PredefinedIntrinsicIds::from_tree(tree),
                type_catalog,
            )?;
            contextualize_record_param_type_names(
                tree,
                class_index,
                qualified_name,
                &mut constructor,
            )?;
            inherit_operator_constructor_defaults(
                tree,
                class_index,
                class_def,
                &mut constructor,
                source_map,
                member_cache,
                type_catalog,
            )?;
            Ok(Some(constructor))
        }
        _ => convert_external_object_callable(
            tree,
            class_index,
            class_def,
            qualified_name,
            source_map,
            member_cache,
            type_catalog,
        ),
    }
}

fn convert_external_object_callable<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    exposed_name: &str,
    source_map: &rumoca_core::SourceMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<Option<rumoca_core::Function>, FlattenError> {
    let owner_span = required_location_span(
        source_map,
        &class_def.location,
        "callable class declaration",
    )?;
    let owner_def_id = class_def.def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            exposed_name,
            "callable class identity",
            owner_span,
        )
    })?;
    let lifecycle = match class_index.external_object_lifecycle(owner_def_id) {
        Ok(lifecycle) => lifecycle,
        Err(error) => {
            let (context, span) =
                external_object_lifecycle_failure_context(source_map, owner_span, error)?;
            return Err(FlattenError::missing_resolved_class_metadata(
                exposed_name,
                context,
                span,
            ));
        }
    };
    lifecycle
        .map(|lifecycle| {
            convert_external_object_constructor(
                tree,
                class_index,
                lifecycle.constructor(),
                exposed_name,
                source_map,
                member_cache,
                type_catalog,
            )
        })
        .transpose()
}

fn convert_external_object_constructor<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    constructor: &'tree ast::ClassDef,
    exposed_name: &str,
    source_map: &rumoca_core::SourceMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<rumoca_core::Function, FlattenError> {
    convert_function(
        tree,
        class_index,
        constructor,
        exposed_name,
        source_map,
        member_cache,
        type_catalog,
    )
}

fn external_object_lifecycle_failure_context(
    source_map: &rumoca_core::SourceMap,
    fallback_span: rumoca_core::Span,
    error: ast::ExternalObjectLifecycleError<'_>,
) -> Result<(&'static str, rumoca_core::Span), FlattenError> {
    let context = error.required_fact();
    let span = error
        .declaration_location()
        .map_or(Ok(fallback_span), |location| {
            required_location_span(source_map, location, context)
        })?;
    Ok((context, span))
}

/// Convert a ast::ClassDef (function) to a rumoca_core::Function.
fn convert_function<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    qualified_name: &str,
    source_map: &rumoca_core::SourceMap,
    member_cache: &mut qualify::MemberDefIdCache<'tree>,
    type_catalog: FunctionTypeCatalog<'_>,
) -> Result<rumoca_core::Function, FlattenError> {
    let span = required_location_span(source_map, &class_def.location, "function definition")?;
    let mut func = rumoca_core::Function::new(qualified_name, span);
    func.def_id = class_def.def_id;
    let mut context = collect_function_context(tree, class_index, class_def, member_cache);
    // MLS §7.3: a function body is converted from the class tree rather than
    // instantiated, so the member tails Resolve deferred across replaceable
    // class edges are proved here before lowering demands exact identity.
    deferred_members::prove_deferred_members_in_algorithms(
        tree,
        class_index,
        qualified_name,
        &context.components,
        &mut context.algorithms,
    );
    let effective_components = context.components;
    let mut import_map =
        function_initial_import_map(tree, class_index, class_def, qualified_name, member_cache);
    extend_imports_if_absent(&mut import_map, context.imports);
    resolve_import_pairs(&class_def.imports, class_index, &mut import_map);
    if let Some(class_def_id) = class_def.def_id {
        collect_lexical_constant_aliases(tree, class_index, class_def_id, &mut import_map, true);
    }
    let prefix = ast::QualifiedName::new();
    let function_locals: HashSet<String> = effective_components.keys().cloned().collect();

    // Process components to find inputs, outputs, and locals
    for (comp_name, component) in &effective_components {
        let param = convert_component_to_param(
            class_index,
            comp_name,
            component,
            source_map,
            FunctionExpressionContext {
                predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(tree),
                type_catalog,
            },
            &import_map,
            &function_locals,
        )?;

        match &component.causality {
            rumoca_core::Causality::Input(_) => func.add_input(param),
            rumoca_core::Causality::Output(_) => func.add_output(param),
            rumoca_core::Causality::Empty => func.add_local(param),
        }
    }

    for alg in &context.algorithms {
        let flat_alg = algorithms::flatten_algorithm_section(
            alg,
            algorithms::AlgorithmSectionContext {
                prefix: &prefix,
                imports: &import_map,
                initial_locals: &function_locals,
                source_map: Some(source_map),
                instance_name: None,
                predefined_string_declaration: tree
                    .scope_tree
                    .predefined_member(&rumoca_core::ComponentPath::from_flat_path("String")),
                predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(tree),
            },
            algorithms::AlgorithmSectionMetadata::new(span, qualified_name.to_string()),
        )?;
        func.body.extend(flat_alg.statements);
    }

    normalize_function_local_references(&mut func);

    // MLS §4.9: Rewrite FieldAccess on record-typed function parameters
    // to direct VarRef names (e.g., `c.re` → `c_re`). This allows backends
    // to render them as simple variable names. The function signature is NOT
    // changed here — that happens optionally in the codegen/DAE phase for
    // backends that need it.
    rewrite_record_field_access_in_body(&mut func);

    // MLS 3.7 §12.3 purity, carried as the two facts the declaration states:
    // the written prefix (`pure` unless `impure` was written) and whether a
    // prefix was written at all. Flat keeps both because they answer different
    // questions: an external function that wrote no prefix "shall be treated as
    // impure" for transformations while the deprecation of its bare form is a
    // report, not a call restriction. `Function::body_is_pure` owns the first
    // question for every consumer.
    func.pure = class_def.pure;
    func.purity_declared = class_def.purity_declared;

    // Convert external function declaration (MLS §12.9)
    if let Some(ref ext) = class_def.external {
        func.external = Some(convert_external_function(
            ext,
            ast_lower::PredefinedIntrinsicIds::from_tree(tree),
        )?);
    }

    // Extract derivative annotations (MLS §12.7.1)
    func.derivatives = extract_derivative_annotations(&class_def.annotation);

    rewrite_function_extends_aliases_in_function(&mut func, tree, class_index)?;
    contextualize_record_param_type_names(tree, class_index, qualified_name, &mut func)?;
    crate::function_lowering::coalesce_proven_record_output_assignments(&mut func);
    crate::function_lowering::inline_proven_loop_scratch_assignments(&mut func);
    if !class_def.partial && is_executable_flat_function(&func) {
        validate_function_outputs_assigned(&func)?;
    }

    Ok(func)
}

/// Rewrite record-typed parameter type names to the exposed qualified names
/// resolved in the callable's own scope (redeclare-aware, like body calls).
///
/// Downstream record decomposition and output projection perform exact
/// constructor lookups keyed by these names, so a record param must carry the
/// concrete resolved type name rather than source-relative text.
pub(crate) fn contextualize_record_param_type_names(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    exposed_name: &str,
    func: &mut rumoca_core::Function,
) -> Result<(), FlattenError> {
    for param in func
        .inputs
        .iter_mut()
        .chain(func.outputs.iter_mut())
        .chain(func.locals.iter_mut())
    {
        if param.type_class != Some(rumoca_core::ClassType::Record) {
            continue;
        }
        let resolution = resolve_record_class_in_exposed_package(
            tree,
            class_index,
            &param.type_name,
            exposed_name,
        )
        .or_else(|| {
            resolve_function_class_with_scope(
                tree,
                class_index,
                &param.type_name,
                Some(exposed_name),
            )
        })
        .or_else(|| {
            let type_def_id = param.type_def_id?;
            let class_def = class_index.get(type_def_id)?;
            Some(FunctionClassResolution {
                exposed_name: class_index.qualified_name(type_def_id)?.to_string(),
                class_def,
            })
        })
        .ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                &param.type_name,
                format!("record function parameter type contextualization in `{exposed_name}`"),
                param.span,
            )
        })?;
        if effective_function_param_class_type(class_index, resolution.class_def)
            != rumoca_core::ClassType::Record
        {
            return Err(FlattenError::missing_resolved_class_metadata(
                &param.type_name,
                "record function parameter resolved to a non-record class",
                param.span,
            ));
        }
        param.type_name = resolution.exposed_name;
        param.type_def_id = resolution.class_def.def_id;
    }
    Ok(())
}

fn resolve_record_class_in_exposed_package<'a>(
    tree: &'a ast::ClassTree,
    class_index: &ast::ClassDefIndex<'a>,
    type_name: &str,
    exposed_callable: &str,
) -> Option<FunctionClassResolution<'a>> {
    let canonical = resolve_function_class_with_scope(tree, class_index, type_name, None)?;
    let canonical_owner = canonical
        .class_def
        .def_id
        .and_then(|def_id| class_index.parent_def_id(def_id))?;
    let exposed_package = path_utils::enclosing_scope(exposed_callable)?;
    let mut package_chain = Vec::new();
    crate::pipeline::collect_package_chain(
        tree,
        class_index,
        exposed_package,
        &mut package_chain,
        &mut FxHashSet::default(),
    );
    if !package_chain.contains(&canonical_owner) {
        return None;
    }
    let leaf = path_utils::leaf_segment(type_name);
    let exposed = resolve_function_in_caller_packages(tree, class_index, exposed_callable, leaf)?;
    resolve_function_class_with_scope(tree, class_index, &exposed, Some(exposed_callable))
}
