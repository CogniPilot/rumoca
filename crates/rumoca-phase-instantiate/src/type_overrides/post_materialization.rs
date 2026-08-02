//! Final exact-identity proof for occurrence-dependent component references.
//!
//! A modifier written in an enclosing class can become a binding on a nested
//! component before all of the writer's sibling occurrences exist. Once the
//! root overlay is complete, this pass uses structured occurrence scopes to
//! re-prove those deferred member tails without rendered-name recovery.

use super::deferred_references::{DynamicExpressionTargetBatch, SelectedComponentTypes};
use super::override_map::TypeOverrideMap;
use crate::{InstantiateError, InstantiateResult};
use indexmap::IndexMap;
use rumoca_core::{ComponentPath, DefId, InstanceId};
use rumoca_ir_ast as ast;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Default)]
struct ScopeProof {
    overrides: TypeOverrideMap,
    selected_component_types: SelectedComponentTypes,
}

#[derive(Default)]
struct ProofIndex {
    proofs: FxHashMap<InstanceId, ScopeProof>,
    scope_ids: FxHashMap<ComponentPath, InstanceId>,
    selected_roots: FxHashSet<DefId>,
}

#[derive(Clone, Copy)]
enum ComponentExpressionSurface {
    Binding,
    BindingSource,
    Start,
    Min,
    Max,
    Nominal,
    Dimensions,
}

/// Re-prove every stored component expression after the full root occurrence
/// graph has materialized.
///
/// The proof index and the mutation are each one linear overlay traversal.
/// Expressions are grouped by exact source occurrence so one resolver handles
/// every surface owned by the same proof context.
pub(crate) fn resolve_post_materialization_component_targets(
    tree: &ast::ClassTree,
    overlay: &mut ast::InstanceOverlay,
) -> InstantiateResult<()> {
    let index = build_scope_proofs(tree, overlay)?;
    for component in overlay.components.values_mut() {
        resolve_component_surfaces(component, &index, tree)?;
    }
    Ok(())
}

fn build_scope_proofs(
    tree: &ast::ClassTree,
    overlay: &ast::InstanceOverlay,
) -> InstantiateResult<ProofIndex> {
    let mut index = ProofIndex::default();
    for class in overlay.classes.values() {
        let mut proof = ScopeProof::default();
        for class_override in class.class_overrides.values() {
            proof.overrides.insert_class_override(class_override);
        }
        index.proofs.insert(class.instance_id, proof);
        index
            .scope_ids
            .insert(class.qualified_name.to_component_path(), class.instance_id);
    }

    for component in overlay.components.values() {
        let (Some(owner_class_id), Some(reference), Some(selected_type)) = (
            component.owner_class_id,
            component.component_ref.as_ref(),
            component.type_def_id,
        ) else {
            continue;
        };
        let declaration = reference.target_def_id();
        let proof = index.proofs.get_mut(&owner_class_id).ok_or_else(|| {
            Box::new(InstantiateError::missing_source_context(format!(
                "component occurrence `{}` has no owning class occurrence",
                component.qualified_name.to_flat_string()
            )))
        })?;
        if let Some(previous) = proof
            .selected_component_types
            .insert(declaration, selected_type)
            && previous != selected_type
        {
            let previous_name = tree
                .def_map
                .get(&previous)
                .map_or_else(|| format!("DefId({})", previous.index()), Clone::clone);
            let selected_name = tree
                .def_map
                .get(&selected_type)
                .map_or_else(|| format!("DefId({})", selected_type.index()), Clone::clone);
            return Err(Box::new(InstantiateError::redeclare_error(
                component.qualified_name.to_flat_string(),
                format!(
                    "one component declaration selected conflicting occurrence types `{previous_name}` and `{selected_name}`"
                ),
                reference.span(),
            )));
        }
        index.selected_roots.insert(declaration);
    }
    Ok(index)
}

fn resolve_component_surfaces(
    component: &mut ast::InstanceData,
    index: &ProofIndex,
    tree: &ast::ClassTree,
) -> InstantiateResult<()> {
    let owner = component.owner_class_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_source_context(format!(
            "component occurrence `{}` has no owning class identity",
            component.qualified_name.to_flat_string()
        )))
    })?;
    let mut grouped = IndexMap::<InstanceId, Vec<ComponentExpressionSurface>>::new();

    if component.binding.is_some() {
        let scope = binding_scope(
            component,
            component.binding.as_ref(),
            owner,
            &index.scope_ids,
            &index.selected_roots,
            "binding",
        )?;
        push_surface(&mut grouped, scope, ComponentExpressionSurface::Binding);
    }
    if component.binding_source.is_some() {
        let scope = binding_scope(
            component,
            component.binding_source.as_ref(),
            owner,
            &index.scope_ids,
            &index.selected_roots,
            "binding source",
        )?;
        push_surface(
            &mut grouped,
            scope,
            ComponentExpressionSurface::BindingSource,
        );
    }
    for (name, expression, surface) in [
        (
            "start",
            component.start.as_ref(),
            ComponentExpressionSurface::Start,
        ),
        (
            "min",
            component.min.as_ref(),
            ComponentExpressionSurface::Min,
        ),
        (
            "max",
            component.max.as_ref(),
            ComponentExpressionSurface::Max,
        ),
        (
            "nominal",
            component.nominal.as_ref(),
            ComponentExpressionSurface::Nominal,
        ),
    ] {
        let Some(expression) = expression else {
            continue;
        };
        let scope = attribute_scope(
            component,
            name,
            expression,
            owner,
            &index.scope_ids,
            &index.selected_roots,
        )?;
        push_surface(&mut grouped, scope, surface);
    }
    if !component.dims_expr.is_empty() {
        push_surface(
            &mut grouped,
            Some(owner),
            ComponentExpressionSurface::Dimensions,
        );
    }

    for (scope_id, surfaces) in grouped {
        let proof = index.proofs.get(&scope_id).ok_or_else(|| {
            Box::new(InstantiateError::missing_source_context(format!(
                "component occurrence `{}` references an absent source occurrence",
                component.qualified_name.to_flat_string()
            )))
        })?;
        if proof.overrides.is_empty() && proof.selected_component_types.is_empty() {
            continue;
        }
        let mut batch = DynamicExpressionTargetBatch::new(
            tree,
            &proof.overrides,
            &proof.selected_component_types,
        );
        for surface in surfaces {
            transform_surface(&mut batch, component, surface);
        }
        batch.finish(())?;
    }
    Ok(())
}

fn binding_scope(
    component: &ast::InstanceData,
    expression: Option<&ast::Expression>,
    owner: InstanceId,
    scope_ids: &FxHashMap<ComponentPath, InstanceId>,
    selected_roots: &FxHashSet<DefId>,
    surface: &str,
) -> InstantiateResult<Option<InstanceId>> {
    if !component.binding_from_modification {
        return Ok(Some(owner));
    }
    source_scope(
        component,
        component.binding_source_scope.as_ref(),
        expression,
        scope_ids,
        selected_roots,
        surface,
    )
}

fn attribute_scope(
    component: &ast::InstanceData,
    name: &str,
    expression: &ast::Expression,
    owner: InstanceId,
    scope_ids: &FxHashMap<ComponentPath, InstanceId>,
    selected_roots: &FxHashSet<DefId>,
) -> InstantiateResult<Option<InstanceId>> {
    let Some(written_scope) = component.attribute_source_scopes.get(name) else {
        return Ok(Some(owner));
    };
    source_scope(
        component,
        Some(written_scope),
        Some(expression),
        scope_ids,
        selected_roots,
        name,
    )
}

fn source_scope(
    component: &ast::InstanceData,
    source: Option<&ast::QualifiedName>,
    expression: Option<&ast::Expression>,
    scope_ids: &FxHashMap<ComponentPath, InstanceId>,
    selected_roots: &FxHashSet<DefId>,
    surface: &str,
) -> InstantiateResult<Option<InstanceId>> {
    let scope_id = source.and_then(|scope| scope_ids.get(&scope.to_component_path()).copied());
    if scope_id.is_some()
        || !expression.is_some_and(|value| deferred_selected_root(value, selected_roots))
    {
        return Ok(scope_id);
    }
    Err(Box::new(InstantiateError::missing_source_context(format!(
        "component occurrence `{}` has a deferred selected-member {surface} without an exact source occurrence",
        component.qualified_name.to_flat_string()
    ))))
}

fn deferred_selected_root(expression: &ast::Expression, selected_roots: &FxHashSet<DefId>) -> bool {
    ast::collect_component_refs(expression)
        .into_iter()
        .any(|reference| {
            reference.target_def_id().is_none()
                && reference
                    .root_def_id()
                    .is_some_and(|root| selected_roots.contains(&root))
        })
}

fn push_surface(
    grouped: &mut IndexMap<InstanceId, Vec<ComponentExpressionSurface>>,
    scope: Option<InstanceId>,
    surface: ComponentExpressionSurface,
) {
    if let Some(scope) = scope {
        grouped.entry(scope).or_default().push(surface);
    }
}

fn transform_surface(
    batch: &mut DynamicExpressionTargetBatch<'_>,
    component: &mut ast::InstanceData,
    surface: ComponentExpressionSurface,
) {
    match surface {
        ComponentExpressionSurface::Binding => {
            batch.transform_optional_expression(&mut component.binding);
        }
        ComponentExpressionSurface::BindingSource => {
            batch.transform_optional_expression(&mut component.binding_source);
        }
        ComponentExpressionSurface::Start => {
            batch.transform_optional_expression(&mut component.start);
        }
        ComponentExpressionSurface::Min => {
            batch.transform_optional_expression(&mut component.min);
        }
        ComponentExpressionSurface::Max => {
            batch.transform_optional_expression(&mut component.max);
        }
        ComponentExpressionSurface::Nominal => {
            batch.transform_optional_expression(&mut component.nominal);
        }
        ComponentExpressionSurface::Dimensions => {
            batch.transform_subscripts(&mut component.dims_expr);
        }
    }
}
