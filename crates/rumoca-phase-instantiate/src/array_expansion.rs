use super::inheritance::resolve_effective_components_for_eval;
use super::source_scope::component_declaration_source_scope;
use super::type_overrides::TypeOverrideMap;
use super::{ComponentInstantiationScope, instantiate_component};
use super::{
    InstantiateContext, InstantiateResult, find_class_in_tree, get_effective_components,
    location_to_span,
};
use rumoca_eval_ast::eval_instantiate::{InstantiateEvalCtx, try_eval_integer_expr};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rumoca_ir_ast::ExpressionTransformer;
use std::sync::Arc;

mod family_replication;
mod homogeneity;
mod selection_projection;
use selection_projection::project_array_selection_for_element;

#[cfg(test)]
mod tests;

/// Expand an array component into individual indexed instances.
///
/// MLS §10.1: Array component expansion creates indexed instances.
/// Also registers the parent array path with its dimensions for use in
/// array equation expansion (MLS §10.5).
pub(super) struct ArrayExpansionScope<'a> {
    pub(super) tree: &'a ast::ClassTree,
    pub(super) effective_components: &'a IndexMap<String, ast::Component>,
    pub(super) type_overrides: &'a TypeOverrideMap,
    pub(super) owner_class_id: rumoca_core::InstanceId,
    pub(super) imports: super::ComponentImports<'a>,
}

/// Per-element declaration state shared by every domain point of one array.
///
/// The scalar element declaration is intentionally carried across iterations
/// (matching the historical loop) so modifier distribution accumulates exactly
/// as before.
struct ArrayElementPlan<'a> {
    name: &'a str,
    comp: &'a ast::Component,
    resolved_mods: Vec<(String, ast::Expression)>,
    resolved_mod_names: std::collections::HashSet<String>,
    original_binding: Option<ast::Expression>,
    binding_qn: ast::QualifiedName,
    mod_env_binding: Option<ast::ModificationValue>,
    binding_source_scope: Option<ast::QualifiedName>,
    scalar_comp: ast::Component,
    source_scoped_modifiers: Vec<SourceScopedModifierProjection>,
}

struct SourceScopedModifierProjection {
    key: ast::QualifiedName,
    original: ast::ModificationValue,
    value: ast::Expression,
    source: Option<ast::Expression>,
    source_components: IndexMap<String, ast::Component>,
}

/// A homogeneous array cleared for template-and-replicate instantiation.
struct CompactionAttempt {
    domain: rumoca_core::StructuredIndexDomain,
    watermarks: family_replication::ReplicationWatermarks,
    span: rumoca_core::Span,
}

pub(super) fn expand_array_component(
    scope: &ArrayExpansionScope<'_>,
    name: &str,
    comp: &ast::Component,
    dims: &[i64],
    ctx: &mut InstantiateContext,
    overlay: &mut rumoca_ir_ast::InstanceOverlay,
) -> InstantiateResult<()> {
    // Register the array parent dimensions for use in array equation expansion.
    // When plug_p.pin[3] is expanded, we register "prefix.plug_p.pin" -> [3]
    // so that equations like v = plug_p.pin.v can be properly expanded.
    ctx.push_path(name);
    let parent_path = ctx.current_path();
    ctx.pop_path();
    overlay
        .array_parent_dims
        .insert(parent_path.to_string(), dims.to_vec());
    let root = parent_path.to_component_path();

    let mut plan = build_element_plan(scope, name, comp, dims, ctx)?;
    let mut indices = super::array_index_tuples(dims);
    let Some(first) = indices.next() else {
        return Ok(());
    };

    // SPEC_0032 §1: decide compaction before the template mutates the overlay so
    // the watermarks describe exactly the template's own contribution.
    let attempt = compaction_attempt(scope, &plan, dims, ctx, overlay)?;
    instantiate_array_element(scope, &mut plan, &first, ctx, overlay)?;
    if let Some(attempt) = attempt
        && family_replication::template_is_replicable(ctx, overlay, &attempt.watermarks)
    {
        let request = family_replication::ReplicationRequest {
            domain: &attempt.domain,
            root: &root,
            template_tuple: &first,
            span: attempt.span,
        };
        family_replication::replicate_template(ctx, overlay, &attempt.watermarks, &request)?;
        return Ok(());
    }

    for idx in indices {
        instantiate_array_element(scope, &mut plan, &idx, ctx, overlay)?;
    }

    Ok(())
}

/// Collect the array-level binding and modifier state shared by all elements.
fn build_element_plan<'a>(
    scope: &ArrayExpansionScope<'_>,
    name: &'a str,
    comp: &'a ast::Component,
    dims: &[i64],
    ctx: &mut InstantiateContext,
) -> InstantiateResult<ArrayElementPlan<'a>> {
    // Extract the original binding for indexing: the active component modifier
    // wins over the declaration binding (MLS §7.2).
    //
    // `comp.start` is not consulted. MLS §4.9 makes `start` an initial guess,
    // and the parser seeds it with the declared type's default, so using it as
    // the array's binding would distribute a value the model never wrote
    // (SPEC_0008). We only READ the binding here - `comp.start` is left intact
    // because typecheck still needs it for dimension inference.
    let binding_qn = ast::QualifiedName::from_ident(name);
    let mod_env_binding = ctx.mod_env().get(&binding_qn).cloned();

    let original_binding = mod_env_binding
        .as_ref()
        .map(|mv| mv.value.clone())
        .or_else(|| comp.binding.clone());
    let binding_source_scope = mod_env_binding
        .as_ref()
        .and_then(|mv| mv.source_scope.clone())
        .or_else(|| component_declaration_source_scope(ctx, comp));

    // MLS §7.2.5: Pre-resolve non-`each` modifications that reference array values.
    // Resolve once so each element can be indexed from the resolved array.
    // Also clear parent-scope entries for these modifications so distributed scalar
    // values are not shadowed during populate_modification_environment.
    let resolved_mods = pre_resolve_array_modifications(
        comp,
        ctx.mod_env(),
        scope.effective_components,
        scope.tree,
    );
    let resolved_mod_names: std::collections::HashSet<String> = resolved_mods
        .iter()
        .map(|(mod_name, _)| mod_name.clone())
        .collect();
    for (mod_name, _) in &resolved_mods {
        let qn = ast::QualifiedName::from_ident(mod_name);
        ctx.mod_env_mut().active.shift_remove(&qn);
    }

    // Create indexed components.
    let mut scalar_comp = comp.clone();
    scalar_comp.shape = vec![];
    scalar_comp.shape_expr = vec![];
    let source_scoped_modifiers = source_scoped_modifier_projections(scope, name, dims.len(), ctx)?;

    Ok(ArrayElementPlan {
        name,
        comp,
        resolved_mods,
        resolved_mod_names,
        original_binding,
        binding_qn,
        mod_env_binding,
        binding_source_scope,
        scalar_comp,
        source_scoped_modifiers,
    })
}

/// Rewrite the scalar element declaration for one domain point (MLS §7.2.5).
fn prepare_element_declaration(
    scope: &ArrayExpansionScope<'_>,
    plan: &mut ArrayElementPlan<'_>,
    idx: &[i64],
) -> InstantiateResult<()> {
    plan.scalar_comp.start =
        indexed_array_component_start(scope.tree, scope.effective_components, plan.comp, idx)?;

    // MLS §10.1: When an array component has a binding (e.g., `v1[m] = plug1.pin.v`),
    // each expanded element `v1[k]` gets the indexed binding `plug1.pin[k].v`.
    // This is essential for propagating bindings through array-of-record types
    // (e.g., Complex output arrays in QS transformer models).
    if let Some(binding) = &plan.original_binding {
        plan.scalar_comp.binding = Some(index_binding_for_element(
            scope.tree,
            scope.effective_components,
            binding,
            idx,
        )?);
    }

    distribute_mods_for_element(&mut plan.scalar_comp, &plan.resolved_mods, idx);
    distribute_component_ref_mods_for_element(
        &mut plan.scalar_comp,
        plan.comp,
        &plan.resolved_mod_names,
        scope.tree,
        scope.effective_components,
        idx,
    )
}

/// Instantiate one domain point through the ordinary scalar code path.
fn instantiate_array_element(
    scope: &ArrayExpansionScope<'_>,
    plan: &mut ArrayElementPlan<'_>,
    idx: &[i64],
    ctx: &mut InstantiateContext,
    overlay: &mut rumoca_ir_ast::InstanceOverlay,
) -> InstantiateResult<()> {
    prepare_element_declaration(scope, plan, idx)?;
    let projected_modifiers =
        projected_source_scoped_modifiers(scope, &plan.source_scoped_modifiers, idx)?;

    // Ensure scalar element instantiation sees the indexed binding (MLS §10.1).
    // Without this scoped override, a parent unindexed modifier entry for this
    // component name would overwrite the per-element indexed binding.
    let previous_binding = ctx.mod_env().active.get(&plan.binding_qn).cloned();
    let binding_modification = plan
        .scalar_comp
        .binding
        .as_ref()
        .filter(|_| plan.mod_env_binding.is_some())
        .map(|binding| {
            array_element_binding_modification(
                scope,
                binding,
                idx,
                plan.mod_env_binding.as_ref(),
                plan.binding_source_scope.clone(),
            )
        })
        .transpose()?;
    apply_projected_modifiers(ctx, &projected_modifiers);
    if let Some(modification) = binding_modification {
        ctx.mod_env_mut()
            .active
            .insert(plan.binding_qn.clone(), modification);
    }

    ctx.push_path_part(plan.name, idx.to_vec());
    let inst_result = instantiate_component(
        scope.tree,
        &plan.scalar_comp,
        ctx,
        overlay,
        ComponentInstantiationScope {
            owner_class_id: Some(scope.owner_class_id),
            effective_components: scope.effective_components,
            type_overrides: scope.type_overrides,
            imports: scope.imports,
        },
    );
    ctx.pop_path();

    // Restore parent binding scope for subsequent elements/siblings.
    match previous_binding {
        Some(prev) => {
            ctx.mod_env_mut()
                .active
                .insert(plan.binding_qn.clone(), prev);
        }
        None => {
            ctx.mod_env_mut().active.shift_remove(&plan.binding_qn);
        }
    }
    restore_projected_modifiers(ctx, &plan.source_scoped_modifiers);

    inst_result
}

fn projected_source_scoped_modifiers(
    scope: &ArrayExpansionScope<'_>,
    projections: &[SourceScopedModifierProjection],
    indices: &[i64],
) -> InstantiateResult<Vec<(ast::QualifiedName, ast::ModificationValue)>> {
    let mut projected = Vec::with_capacity(projections.len());
    for projection in projections {
        let projected_value = index_array_expression_for_element(
            scope.tree,
            &projection.source_components,
            &projection.value,
            indices,
        )?
        .expect("the projection probe proves every planned modifier value");
        let projected_source = projection
            .source
            .as_ref()
            .map(|source| {
                index_array_expression_for_element(
                    scope.tree,
                    &projection.source_components,
                    source,
                    indices,
                )
            })
            .transpose()?
            .flatten()
            .or_else(|| projection.original.source.clone());
        projected.push((
            projection.key.clone(),
            ast::ModificationValue::with_source_scope_and_prefixes(
                projected_value,
                projected_source,
                projection.original.source_scope.clone(),
                projection.original.each,
                projection.original.final_,
            ),
        ));
    }
    Ok(projected)
}

fn apply_projected_modifiers(
    ctx: &mut InstantiateContext,
    projected: &[(ast::QualifiedName, ast::ModificationValue)],
) {
    for (key, value) in projected {
        ctx.mod_env_mut().active.insert(key.clone(), value.clone());
    }
}

fn restore_projected_modifiers(
    ctx: &mut InstantiateContext,
    projections: &[SourceScopedModifierProjection],
) {
    for projection in projections {
        ctx.mod_env_mut()
            .active
            .insert(projection.key.clone(), projection.original.clone());
    }
}

fn source_scoped_modifier_projections(
    scope: &ArrayExpansionScope<'_>,
    array_name: &str,
    rank: usize,
    ctx: &InstantiateContext,
) -> InstantiateResult<Vec<SourceScopedModifierProjection>> {
    let probe = vec![1; rank];
    let candidates = ctx.mod_env().active.iter().filter(|(key, value)| {
        !value.each
            && key.parts.len() > 1
            && key
                .parts
                .first()
                .is_some_and(|(name, subscripts)| name == array_name && subscripts.is_empty())
    });
    let mut projections = Vec::new();
    for (key, value) in candidates {
        let Some(source_scope) = value.source_scope.as_ref() else {
            continue;
        };
        let Some(source_components) = source_scope_components(scope.tree, ctx, source_scope)?
        else {
            continue;
        };
        let Some(projected_value) = projection_source_expression(
            scope.tree,
            ctx.mod_env(),
            &source_components,
            &value.value,
            &probe,
        )?
        else {
            continue;
        };
        let projected_source = value
            .source
            .as_ref()
            .map(|source| {
                projection_source_expression(
                    scope.tree,
                    ctx.mod_env(),
                    &source_components,
                    source,
                    &probe,
                )
            })
            .transpose()?
            .flatten();
        projections.push(SourceScopedModifierProjection {
            key: key.clone(),
            original: value.clone(),
            value: projected_value,
            source: projected_source,
            source_components,
        });
    }
    Ok(projections)
}

fn projection_source_expression(
    tree: &ast::ClassTree,
    mod_env: &ast::ModificationEnvironment,
    source_components: &IndexMap<String, ast::Component>,
    expression: &ast::Expression,
    probe: &[i64],
) -> InstantiateResult<Option<ast::Expression>> {
    if index_array_expression_for_element(tree, source_components, expression, probe)?.is_some() {
        return Ok(Some(expression.clone()));
    }
    let resolved = resolve_mod_to_array(expression, mod_env, source_components, tree);
    Ok(
        index_array_expression_for_element(tree, source_components, &resolved, probe)?
            .map(|_| resolved),
    )
}

fn source_scope_components(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    source_scope: &ast::QualifiedName,
) -> InstantiateResult<Option<IndexMap<String, ast::Component>>> {
    let rendered = source_scope.to_flat_string();
    let Some(frame) = ctx
        .active_instantiations
        .iter()
        .rev()
        .find(|frame| frame.instance_path == rendered)
    else {
        return Ok(None);
    };
    let super::InstantiationFrameKey::Def(def_id) = frame.key;
    let Some(class) = tree.get_class_by_def_id(def_id) else {
        return Ok(None);
    };
    let effective = get_effective_components(tree, class)?;
    Ok(Some(if effective.is_empty() {
        class.components.clone()
    } else {
        effective
    }))
}

/// Row-major compact domain over an array component's own dimensions.
fn component_family_domain(dims: &[i64]) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: dims
            .iter()
            .enumerate()
            .map(|(position, upper)| rumoca_core::StructuredIndexBinder {
                id: position,
                display_name: format!("__comp_i{}", position + 1),
                lower: 1,
                upper: *upper,
                step: 1,
            })
            .collect(),
    }
}

/// Decide whether this array may be instantiated from a single template.
fn compaction_attempt(
    scope: &ArrayExpansionScope<'_>,
    plan: &ArrayElementPlan<'_>,
    dims: &[i64],
    ctx: &InstantiateContext,
    overlay: &rumoca_ir_ast::InstanceOverlay,
) -> InstantiateResult<Option<CompactionAttempt>> {
    if !ctx.options.compact_component_families {
        return Ok(None);
    }
    // SPEC_0032 §1 requires scalar instantiation for non-`each`, per-element
    // modifiers. The exact projection plans are the proof that this family is
    // index-dependent; Instance IR still retains every authoritative element.
    if !plan.source_scoped_modifiers.is_empty() {
        log_scalar_expansion_fallback(plan.name, "source-scoped array modifier");
        return Ok(None);
    }
    let domain = component_family_domain(dims);
    let (Ok(count), Ok(probe_tuples)) = (domain.scalar_count(), domain_probe_tuples(&domain))
    else {
        return Ok(None);
    };
    if count == 0 {
        return Ok(None);
    }
    // A missing declaration span would leave replicated members without
    // provenance, so fall back rather than fabricate one.
    let Ok(span) = location_to_span(
        &plan.comp.location,
        &scope.tree.source_map,
        "array component declaration",
    ) else {
        return Ok(None);
    };
    let verdict = homogeneity::component_array_homogeneity(&homogeneity::HomogeneityInput {
        comp: plan.comp,
        resolved_mods: &plan.resolved_mods,
        original_binding: plan.original_binding.as_ref(),
        tree: scope.tree,
        parent_components: scope.effective_components,
        probe_tuples: &probe_tuples,
    })?;
    if let homogeneity::HomogeneityVerdict::Scalar(reason) = verdict {
        log_scalar_expansion_fallback(plan.name, reason);
        return Ok(None);
    }
    Ok(Some(CompactionAttempt {
        domain,
        watermarks: family_replication::watermarks(ctx, overlay),
        span,
    }))
}

/// Probe the first and last domain points so index-dependent element rewrites
/// are observed at both ends of the domain.
fn domain_probe_tuples(
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<Vec<Vec<i64>>, rumoca_core::StructuredIndexDomainError> {
    let count = domain.scalar_count()?;
    let mut tuples = Vec::new();
    for ordinal in [0, count.saturating_sub(1)] {
        let Some(tuple) = domain.index_tuple_at(ordinal)? else {
            continue;
        };
        if !tuples.contains(&tuple) {
            tuples.push(tuple);
        }
    }
    Ok(tuples)
}

fn log_scalar_expansion_fallback(name: &str, reason: &'static str) {
    #[cfg(feature = "tracing")]
    tracing::debug!(
        target: "rumoca_phase_instantiate::array_expansion",
        component = %name,
        reason = %reason,
        "component array kept scalar expansion"
    );

    #[cfg(not(feature = "tracing"))]
    {
        let _ = (name, reason);
    }
}

fn indexed_array_component_start(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    comp: &ast::Component,
    idx: &[i64],
) -> InstantiateResult<ast::Expression> {
    if comp.start_has_each || matches!(comp.start, ast::Expression::Empty { .. }) {
        return Ok(comp.start.clone());
    }
    Ok(
        index_array_expression_for_element(tree, parent_components, &comp.start, idx)?
            .unwrap_or_else(|| comp.start.clone()),
    )
}

fn array_element_binding_modification(
    scope: &ArrayExpansionScope<'_>,
    binding_expr: &ast::Expression,
    idx: &[i64],
    parent_mod: Option<&ast::ModificationValue>,
    binding_source_scope: Option<ast::QualifiedName>,
) -> InstantiateResult<ast::ModificationValue> {
    let Some(parent_mod) = parent_mod else {
        return Ok(ast::ModificationValue::with_source_scope(
            binding_expr.clone(),
            Some(binding_expr.clone()),
            binding_source_scope,
        ));
    };
    let source = parent_mod
        .source
        .as_ref()
        .map(|source_expr| {
            index_binding_for_element(scope.tree, scope.effective_components, source_expr, idx)
        })
        .transpose()?
        .or_else(|| Some(binding_expr.clone()));
    let source_scope = parent_mod.source_scope.clone().or(binding_source_scope);
    Ok(ast::ModificationValue::with_source_scope(
        binding_expr.clone(),
        source,
        source_scope,
    ))
}

/// Pre-resolve non-`each` modifications that have array values.
///
/// MLS §7.2.5: Returns a list of (mod_name, resolved_array_expr) pairs for
/// modifications that can be distributed across array elements.
pub(super) fn pre_resolve_array_modifications(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Vec<(String, ast::Expression)> {
    let mut resolved = Vec::new();
    for (name, expr) in &comp.modifications {
        if comp.each_modifications.contains(name) {
            continue; // `each` modifications are not distributed
        }
        // Keep component-reference modifiers (for example `R=R`) in symbolic
        // form so per-element indexing can preserve owner scope (`R[1]`).
        // Eager resolution here can over-collapse to outer-source expressions
        // like `RRef.d`, losing the declaring component context.
        if matches!(expr, ast::Expression::ComponentReference(_)) {
            continue;
        }
        let val = resolve_mod_to_array(expr, mod_env, effective_components, tree);
        if matches!(val, ast::Expression::Array { .. }) {
            resolved.push((name.clone(), val));
        }
    }
    resolved
}

/// Apply pre-resolved array modifications to a scalar array element.
///
/// MLS §7.2.5: For each resolved array modification, indexes the array
/// to get the element-specific value.
pub(super) fn distribute_mods_for_element(
    scalar_comp: &mut ast::Component,
    resolved_mods: &[(String, ast::Expression)],
    indices: &[i64],
) {
    for (name, resolved) in resolved_mods {
        if let Some(indexed) = index_array_modification(resolved, indices) {
            scalar_comp.modifications.insert(name.clone(), indexed);
        }
    }
}

/// Distribute non-`each` component-reference modifications by indexing proven array parts.
///
/// This covers cases where a modifier is array-valued but not materialized as an
/// `ast::Expression::Array` during pre-resolution (e.g., `cellData=stackData.cellData`).
fn distribute_component_ref_mods_for_element(
    scalar_comp: &mut ast::Component,
    original_comp: &ast::Component,
    resolved_mod_names: &std::collections::HashSet<String>,
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    indices: &[i64],
) -> InstantiateResult<()> {
    for (name, expr) in &original_comp.modifications {
        if original_comp.each_modifications.contains(name)
            || resolved_mod_names.contains(name.as_str())
        {
            continue;
        }

        if let Some(indexed) =
            index_nested_modification_for_element(tree, parent_components, expr, indices)?
        {
            scalar_comp.modifications.insert(name.clone(), indexed);
            continue;
        }

        let indexed = index_binding_for_element(tree, parent_components, expr, indices)?;
        if !matches!(indexed, ast::Expression::ArrayIndex { .. }) {
            scalar_comp.modifications.insert(name.clone(), indexed);
        }
    }
    Ok(())
}

fn index_nested_modification_for_element(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    expr: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<Option<ast::Expression>> {
    let mut indexed = expr.clone();
    match &mut indexed {
        ast::Expression::ClassModification {
            modifications,
            each_flags,
            ..
        } => {
            for (position, nested) in modifications.iter_mut().enumerate() {
                if !each_flags.get(position).copied().unwrap_or(false)
                    && let Some(value) = index_nested_modification_for_element(
                        tree,
                        parent_components,
                        nested,
                        indices,
                    )?
                {
                    *nested = value;
                }
            }
        }
        ast::Expression::Modification { value, .. } => {
            if let Some(indexed_value) =
                index_array_expression_for_element(tree, parent_components, value, indices)?
            {
                *value = Arc::new(indexed_value);
            }
        }
        _ => return Ok(None),
    }
    Ok(Some(indexed))
}

/// Resolve a modification expression to its value, handling component references.
///
/// For component references like `R` that refer to array parameters in the parent
/// scope, this resolves them to their actual array values so they can be indexed.
pub(super) fn resolve_mod_to_array(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> ast::Expression {
    resolve_mod_to_array_depth(expr, mod_env, effective_components, tree, 0)
}

/// Resolve a modification expression to an array value with depth limit.
fn resolve_mod_to_array_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> ast::Expression {
    const MAX_DEPTH: usize = 5;
    if depth >= MAX_DEPTH {
        return expr.clone();
    }
    if let ast::Expression::ComponentReference(cref) = expr
        && cref.parts.len() == 1
    {
        let name = cref.parts[0].ident.text.as_ref();
        if let Some(resolved) = resolve_single_ref(name, mod_env, effective_components, tree, depth)
        {
            return resolved;
        }
    }
    // MLS §11.1.2.1: Evaluate array comprehensions like {j for j in 1:m}
    if let Some(array) = try_eval_array_comprehension(expr, mod_env, effective_components, tree) {
        return array;
    }
    // Evaluate structural array-valued expressions used in non-`each` modifiers
    // (e.g., k=fill(1, n), phase=-symmetricOrientation(m)). This allows
    // per-element modifier distribution during array component expansion
    // (MLS §7.2.5).
    if let Some(array) = try_eval_structural_array_expr(expr, mod_env, effective_components, tree) {
        return array;
    }
    expr.clone()
}

/// Resolve a single-part component reference to an array value.
fn resolve_single_ref(
    name: &str,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> Option<ast::Expression> {
    // Check mod_env first
    let qn = ast::QualifiedName::from_ident(name);
    if let Some(mv) = mod_env.active.get(&qn) {
        let resolved =
            resolve_mod_to_array_depth(&mv.value, mod_env, effective_components, tree, depth + 1);
        if matches!(resolved, ast::Expression::Array { .. }) {
            return Some(resolved);
        }
    }
    // Check effective_components for array-valued bindings (including fill/zeros/ones).
    let comp = effective_components.get(name)?;
    if let Some(ref binding) = comp.binding {
        let resolved =
            resolve_mod_to_array_depth(binding, mod_env, effective_components, tree, depth + 1);
        if matches!(resolved, ast::Expression::Array { .. }) {
            return Some(resolved);
        }
    }
    let resolved_start =
        resolve_mod_to_array_depth(&comp.start, mod_env, effective_components, tree, depth + 1);
    if matches!(resolved_start, ast::Expression::Array { .. }) {
        return Some(resolved_start);
    }
    None
}

/// Evaluate simple structural array expressions to concrete 1-D arrays.
///
/// Supports:
/// - `fill(v, n)` -> `{v, v, ..., v}` (n elements)
/// - `zeros(n)` -> `{0, ..., 0}` (n elements)
/// - `ones(n)` -> `{1, ..., 1}` (n elements)
/// - unary signs over structural arrays
/// - MSL polyphase `symmetricOrientation(m)` structural phase vectors
///
/// This is intentionally limited to 1-D constructors because modifier
/// distribution currently indexes one dimension at a time.
fn try_eval_structural_array_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    match expr {
        ast::Expression::Unary { op, rhs, span } => {
            let array = try_eval_structural_array_expr(rhs, mod_env, effective_components, tree)?;
            return apply_unary_to_structural_array(op, &array, *span);
        }
        ast::Expression::Parenthesized { inner, .. } => {
            return try_eval_structural_array_expr(inner, mod_env, effective_components, tree);
        }
        _ => {}
    }

    let ast::Expression::FunctionCall { comp, args, .. } = expr else {
        return None;
    };

    let call_span = expr.span();
    let func = comp
        .parts
        .last()
        .map(|part| part.ident.text.as_ref())
        .unwrap_or("");
    let make_int_lit = |value: i64| ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: value.to_string().into(),
            ..rumoca_core::Token::default()
        },
        span: call_span,
    };

    let make_repeated =
        |value: ast::Expression, count_expr: &ast::Expression| -> Option<ast::Expression> {
            let eval_ctx = InstantiateEvalCtx {
                tree,
                mod_env,
                effective_components,
                resolve_class_components: resolve_effective_components_for_eval,
            };
            let n = try_eval_integer_expr(&eval_ctx, count_expr)?;
            let len = n.max(0) as usize;
            Some(ast::Expression::Array {
                elements: std::iter::repeat_n(value, len).collect(),
                is_matrix: false,
                span: call_span,
            })
        };

    // MLS §7.2.5: Resolve simple forwarded fill() element expressions in the
    // parent scope before distributing across array elements. This keeps forwarded
    // scalar parameters (e.g. fill(useHeatPort, m)) concrete after per-element
    // modifier propagation without paying for full expression evaluation here.
    let resolve_fill_value = |value: &ast::Expression| -> ast::Expression {
        let ast::Expression::ComponentReference(cref) = value else {
            return value.clone();
        };
        if cref.parts.len() != 1 {
            return value.clone();
        }

        let name = cref.parts[0].ident.text.as_ref();
        let qn = ast::QualifiedName::from_ident(name);
        if let Some(mod_value) = mod_env.get(&qn) {
            return mod_value.value.clone();
        }

        let Some(comp) = effective_components.get(name) else {
            return value.clone();
        };
        if let Some(binding) = &comp.binding {
            return binding.clone();
        }
        if !matches!(comp.start, ast::Expression::Empty { .. }) {
            return comp.start.clone();
        }
        value.clone()
    };

    match func {
        "fill" if comp.parts.len() == 1 && args.len() == 2 => {
            make_repeated(resolve_fill_value(&args[0]), &args[1])
        }
        "zeros" if comp.parts.len() == 1 && args.len() == 1 => {
            make_repeated(make_int_lit(0), &args[0])
        }
        "ones" if comp.parts.len() == 1 && args.len() == 1 => {
            make_repeated(make_int_lit(1), &args[0])
        }
        "symmetricOrientation"
            if is_polyphase_symmetric_orientation_call(comp) && args.len() == 1 =>
        {
            let eval_ctx = InstantiateEvalCtx {
                tree,
                mod_env,
                effective_components,
                resolve_class_components: resolve_effective_components_for_eval,
            };
            let m = try_eval_integer_expr(&eval_ctx, &args[0])?;
            let values = symmetric_orientation_values(m)?;
            Some(ast::Expression::Array {
                elements: values
                    .into_iter()
                    .map(|value| make_real_lit(value, call_span))
                    .collect(),
                is_matrix: false,
                span: call_span,
            })
        }
        _ => None,
    }
}

fn is_polyphase_symmetric_orientation_call(comp: &ast::ComponentReference) -> bool {
    let path = comp
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    path == "Modelica.Electrical.Polyphase.Functions.symmetricOrientation"
        || path == "Electrical.Polyphase.Functions.symmetricOrientation"
        || path == "Polyphase.Functions.symmetricOrientation"
}

fn symmetric_orientation_values(m: i64) -> Option<Vec<f64>> {
    if !(1..=1024).contains(&m) {
        return None;
    }
    if m % 2 == 0 {
        if m == 2 {
            return Some(vec![0.0, std::f64::consts::FRAC_PI_2]);
        }
        let half = m / 2;
        let base = symmetric_orientation_values(half)?;
        let offset = std::f64::consts::PI / m as f64;
        let mut values = Vec::with_capacity(m as usize);
        values.extend(base.iter().copied());
        values.extend(base.into_iter().map(|value| value - offset));
        return Some(values);
    }
    Some(
        (1..=m)
            .map(|k| (k - 1) as f64 * 2.0 * std::f64::consts::PI / m as f64)
            .collect(),
    )
}

fn apply_unary_to_structural_array(
    op: &rumoca_core::OpUnary,
    array: &ast::Expression,
    span: rumoca_core::Span,
) -> Option<ast::Expression> {
    let ast::Expression::Array {
        elements,
        is_matrix,
        ..
    } = array
    else {
        return None;
    };
    let mapped = elements
        .iter()
        .map(|elem| apply_unary_to_structural_array_element(op, elem, span))
        .collect::<Option<Vec<_>>>()?;
    Some(ast::Expression::Array {
        elements: mapped,
        is_matrix: *is_matrix,
        span,
    })
}

fn apply_unary_to_structural_array_element(
    op: &rumoca_core::OpUnary,
    elem: &ast::Expression,
    span: rumoca_core::Span,
) -> Option<ast::Expression> {
    match op {
        rumoca_core::OpUnary::Plus
        | rumoca_core::OpUnary::DotPlus
        | rumoca_core::OpUnary::Empty => Some(elem.clone()),
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
            Some(negate_structural_numeric_expr(elem, span))
        }
        rumoca_core::OpUnary::Not => None,
    }
}

fn negate_structural_numeric_expr(
    expr: &ast::Expression,
    span: rumoca_core::Span,
) -> ast::Expression {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
            ..
        } => token
            .text
            .parse::<f64>()
            .ok()
            .map(|value| make_real_lit(-value, span))
            .unwrap_or_else(|| ast::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Arc::new(expr.clone()),
                span,
            }),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token
            .text
            .parse::<i64>()
            .ok()
            .map(|value| make_real_lit(-(value as f64), span))
            .unwrap_or_else(|| ast::Expression::Unary {
                op: rumoca_core::OpUnary::Minus,
                rhs: Arc::new(expr.clone()),
                span,
            }),
        ast::Expression::Parenthesized { inner, .. } => negate_structural_numeric_expr(inner, span),
        _ => ast::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Arc::new(expr.clone()),
            span,
        },
    }
}

fn make_real_lit(value: f64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedReal,
        token: rumoca_core::Token {
            text: format!("{value:.17}").into(),
            ..rumoca_core::Token::default()
        },
        span,
    }
}

/// Evaluate an array comprehension `{expr for j in start:end}` to a concrete array.
///
/// MLS §11.1.2.1: For simple comprehensions like `{j for j in 1:m}`,
/// evaluates the range and substitutes the loop variable for each value.
fn try_eval_array_comprehension(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    let ast::Expression::ArrayComprehension {
        expr: body,
        indices,
        filter,
        span,
    } = expr
    else {
        return None;
    };
    // Only handle single-index comprehensions without filters
    if indices.len() != 1 || filter.is_some() {
        return None;
    }
    let for_idx = &indices[0];
    let loop_var = for_idx.ident.text.as_ref();

    // Evaluate the range bounds
    let (start, end) = eval_range_bounds(&for_idx.range, mod_env, effective_components, tree)?;

    // Generate elements by substituting the loop variable
    let mut elements = Vec::with_capacity((end - start + 1).max(0) as usize);
    for val in start..=end {
        let elem = substitute_var(body, loop_var, val);
        elements.push(elem);
    }
    Some(ast::Expression::Array {
        elements,
        is_matrix: false,
        span: *span,
    })
}

/// Evaluate range bounds from a Range expression, returning (start, end).
fn eval_range_bounds(
    range: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<(i64, i64)> {
    match range {
        ast::Expression::Range { start, end, .. } => {
            let eval_ctx = InstantiateEvalCtx {
                tree,
                mod_env,
                effective_components,
                resolve_class_components: resolve_effective_components_for_eval,
            };
            let s = try_eval_integer_expr(&eval_ctx, start)?;
            let e = try_eval_integer_expr(&eval_ctx, end)?;
            Some((s, e))
        }
        _ => None,
    }
}

/// Substitute a component reference to `var_name` with an integer literal.
fn substitute_var(expr: &ast::Expression, var_name: &str, value: i64) -> ast::Expression {
    LoopIndexSubstituter { var_name, value }.transform_expression(expr.clone())
}

fn replace_component_reference_with_integer(
    expr: &ast::Expression,
    var_name: &str,
    value: i64,
) -> Option<ast::Expression> {
    let ast::Expression::ComponentReference(cref) = expr else {
        return None;
    };
    if cref.parts.len() != 1 || cref.parts[0].ident.text.as_ref() != var_name {
        return None;
    }

    Some(ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: Arc::from(value.to_string().as_str()),
            ..rumoca_core::Token::default()
        },
        span: cref.span,
    })
}

struct LoopIndexSubstituter<'a> {
    var_name: &'a str,
    value: i64,
}

impl ast::ExpressionTransformer for LoopIndexSubstituter<'_> {
    fn transform_expression(&mut self, expr: ast::Expression) -> ast::Expression {
        match expr {
            ast::Expression::ArrayComprehension {
                expr: inner_expr,
                indices,
                filter,
                span,
            } => self.transform_array_comprehension(inner_expr, indices, filter, span),
            _ => replace_component_reference_with_integer(&expr, self.var_name, self.value)
                .unwrap_or_else(|| self.walk_expression(expr)),
        }
    }
}

impl LoopIndexSubstituter<'_> {
    fn transform_array_comprehension(
        &mut self,
        inner_expr: Arc<ast::Expression>,
        indices: Vec<rumoca_ir_ast::ForIndex>,
        filter: Option<Arc<ast::Expression>>,
        span: rumoca_core::Span,
    ) -> ast::Expression {
        if indices
            .iter()
            .any(|for_index| for_index.ident.text.as_ref() == self.var_name)
        {
            return ast::Expression::ArrayComprehension {
                expr: inner_expr,
                indices,
                filter,
                span,
            };
        }

        ast::Expression::ArrayComprehension {
            expr: Arc::new(self.transform_expression(inner_expr.as_ref().clone())),
            indices: indices
                .into_iter()
                .map(|for_index| self.transform_for_index(for_index))
                .collect(),
            filter: filter.map(|filter_expr| {
                Arc::new(self.transform_expression(filter_expr.as_ref().clone()))
            }),
            span,
        }
    }
}

/// Index an array modification value for a specific element.
///
/// If the expression is an `Array { elements }`, returns the element at the
/// given 1-based index. For non-array values, returns None (no change needed).
fn index_array_modification(expr: &ast::Expression, indices: &[i64]) -> Option<ast::Expression> {
    match expr {
        ast::Expression::Array { elements, .. } => {
            let (&first_idx, remaining) = indices.split_first()?;
            let idx = first_idx.checked_sub(1)? as usize; // Convert 1-based to 0-based
            let selected = elements.get(idx)?;
            if remaining.is_empty() {
                Some(selected.clone())
            } else {
                index_array_modification(selected, remaining)
            }
        }
        ast::Expression::Parenthesized { inner, .. } => index_array_modification(inner, indices),
        _ => None,
    }
}

/// Index a binding expression for an array element.
///
/// When an array component has a binding (e.g., `v1[m] = plug1.pin.v`), each expanded
/// element needs an indexed binding (e.g., `v1[1] = plug1.pin[1].v`).
///
/// Uses type information to walk the ast::ComponentReference parts, looking up each part's
/// type in the class tree to find which part introduces array dimensions. The subscript
/// is placed on that part. For example, `plug1.pin.v` where `pin` is declared as
/// `PositivePin pin[m]` becomes `plug1.pin[k].v`.
///
/// MLS §10.5.1: Field access distributes over arrays, so `(a.b.c)[k] = a.b[k].c`
/// when `b` introduces the array dimension.
pub(super) fn index_binding_for_element(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    binding: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<ast::Expression> {
    let make_subscripts = || -> Vec<ast::Subscript> {
        indices
            .iter()
            .map(|&i| {
                ast::Subscript::Expression(ast::Expression::Terminal {
                    terminal_type: ast::TerminalType::UnsignedInteger,
                    token: rumoca_core::Token {
                        text: i.to_string().into(),
                        ..rumoca_core::Token::default()
                    },
                    span: binding.span(),
                })
            })
            .collect()
    };

    // For ast::ComponentReference bindings, walk the cref parts using type info
    // to find which part introduces array dimensions.
    if let ast::Expression::ComponentReference(cref) = binding
        && !cref.parts.is_empty()
    {
        // MLS §10.5.1: if we cannot prove which part introduces array
        // dimensions, preserve the binding shape as `(cref)[k]` instead of
        // guessing a field position.
        let Some(pos) = find_array_part(tree, parent_components, &cref.parts)? else {
            return Ok(ast::Expression::ArrayIndex {
                base: Arc::new(binding.clone()),
                subscripts: make_subscripts(),
                span: binding.span(),
            });
        };
        let mut new_ref = cref.clone();
        let subscripts = project_array_selection_for_element(
            tree,
            parent_components,
            new_ref.parts[pos].subs.as_deref(),
            indices,
            binding.span(),
        )?;
        new_ref.parts[pos] = ast::ComponentRefPart {
            ident: new_ref.parts[pos].ident.clone(),
            subs: Some(subscripts),
            def_id: new_ref.parts[pos].def_id,
        };
        return Ok(ast::Expression::ComponentReference(new_ref));
    }

    if let Some(indexed) = index_non_component_reference_binding(binding, indices) {
        return Ok(indexed);
    }

    // Fallback for non-ast::ComponentReference bindings: wrap with ArrayIndex
    Ok(ast::Expression::ArrayIndex {
        base: Arc::new(binding.clone()),
        subscripts: make_subscripts(),
        span: binding.span(),
    })
}

fn index_non_component_reference_binding(
    binding: &ast::Expression,
    indices: &[i64],
) -> Option<ast::Expression> {
    match binding {
        ast::Expression::Array { .. } => index_array_modification(binding, indices),
        ast::Expression::ArrayComprehension { .. } => {
            index_array_comprehension_for_element(binding, indices)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            index_non_component_reference_binding(inner, indices)
        }
        _ => None,
    }
}

fn index_array_expression_for_element(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    expr: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<Option<ast::Expression>> {
    match expr {
        ast::Expression::Array { .. } | ast::Expression::ArrayComprehension { .. } => {
            Ok(index_non_component_reference_binding(expr, indices))
        }
        ast::Expression::ComponentReference(cref) => {
            index_component_reference_array_part(tree, parent_components, cref, expr, indices)
        }
        ast::Expression::Binary { op, lhs, rhs, span } => {
            let indexed_lhs =
                index_array_expression_for_element(tree, parent_components, lhs, indices)?;
            let indexed_rhs =
                index_array_expression_for_element(tree, parent_components, rhs, indices)?;
            if indexed_lhs.is_none() && indexed_rhs.is_none() {
                return Ok(None);
            }
            Ok(Some(ast::Expression::Binary {
                op: op.clone(),
                lhs: Arc::new(indexed_lhs.unwrap_or_else(|| lhs.as_ref().clone())),
                rhs: Arc::new(indexed_rhs.unwrap_or_else(|| rhs.as_ref().clone())),
                span: *span,
            }))
        }
        ast::Expression::Unary { op, rhs, span } => {
            Ok(
                index_array_expression_for_element(tree, parent_components, rhs, indices)?.map(
                    |indexed_rhs| ast::Expression::Unary {
                        op: op.clone(),
                        rhs: Arc::new(indexed_rhs),
                        span: *span,
                    },
                ),
            )
        }
        ast::Expression::Parenthesized { inner, span } => {
            Ok(
                index_array_expression_for_element(tree, parent_components, inner, indices)?.map(
                    |indexed_inner| ast::Expression::Parenthesized {
                        inner: Arc::new(indexed_inner),
                        span: *span,
                    },
                ),
            )
        }
        _ => Ok(None),
    }
}

fn index_component_reference_array_part(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    cref: &ast::ComponentReference,
    expr: &ast::Expression,
    indices: &[i64],
) -> InstantiateResult<Option<ast::Expression>> {
    if cref.parts.is_empty() {
        return Ok(None);
    }
    let Some(pos) = find_array_part(tree, parent_components, &cref.parts)? else {
        return Ok(None);
    };
    let mut indexed = cref.clone();
    let subscripts = project_array_selection_for_element(
        tree,
        parent_components,
        indexed.parts[pos].subs.as_deref(),
        indices,
        expr.span(),
    )?;
    indexed.parts[pos] = ast::ComponentRefPart {
        ident: indexed.parts[pos].ident.clone(),
        subs: Some(subscripts),
        def_id: indexed.parts[pos].def_id,
    };
    Ok(Some(ast::Expression::ComponentReference(indexed)))
}

fn index_array_comprehension_for_element(
    expr: &ast::Expression,
    indices: &[i64],
) -> Option<ast::Expression> {
    let ast::Expression::ArrayComprehension {
        expr: body,
        indices: for_indices,
        filter,
        ..
    } = expr
    else {
        return None;
    };

    if filter.is_some() || indices.len() < for_indices.len() {
        return None;
    }

    let mut projected = body.as_ref().clone();
    for (for_index, value) in for_indices.iter().zip(indices.iter().copied()) {
        projected = substitute_var(&projected, for_index.ident.text.as_ref(), value);
    }

    if indices.len() == for_indices.len() {
        return Some(projected);
    }

    let remaining = &indices[for_indices.len()..];
    index_non_component_reference_binding(&projected, remaining).or(Some(projected))
}

/// Walk ast::ComponentReference parts using type information to find which part
/// introduces array dimensions. Returns the index of the array part, if found.
fn find_array_part(
    tree: &ast::ClassTree,
    parent_components: &IndexMap<String, ast::Component>,
    parts: &[rumoca_ir_ast::ComponentRefPart],
) -> InstantiateResult<Option<usize>> {
    // Use Cow-like pattern: borrow parent_components for the first lookup,
    // only allocate owned components when we need to traverse deeper.
    let mut owned: Option<IndexMap<String, ast::Component>>;
    let mut current: &IndexMap<String, ast::Component> = parent_components;

    for (i, part) in parts.iter().enumerate() {
        let Some(comp) = current.get(part.ident.text.as_ref()) else {
            return Ok(None);
        };
        if !comp.shape.is_empty() || !comp.shape_expr.is_empty() {
            return Ok(Some(i));
        }
        if i + 1 < parts.len() {
            let Some(class) = lookup_class_def(tree, comp) else {
                return Ok(None);
            };
            let effective = get_effective_components(tree, class)?;
            owned = Some(if effective.is_empty() {
                class.components.clone()
            } else {
                effective
            });
            current = owned
                .as_ref()
                .expect("owned effective components must be present after insertion");
        }
    }
    Ok(None)
}

/// Look up the ast::ClassDef for a ast::Component's type using the class tree.
fn lookup_class_def<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::Component,
) -> Option<&'a rumoca_ir_ast::ClassDef> {
    comp.type_def_id
        .or(comp.type_name.def_id)
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, &comp.type_name.to_string()))
}
