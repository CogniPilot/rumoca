//! Template capture and replication for compact component arrays.
//!
//! SPEC_0032 §1: instantiation resolves the first domain point of a homogeneous
//! component array through the ordinary scalar code path and then derives every
//! remaining point from that template by reindexing its instance paths, instead
//! of re-running type lookup, inheritance merge, modifier environment work and
//! attribute evaluation once per element.
//!
//! This is purely an instantiation optimization: the per-element overlay
//! entries it produces — content *and* `InstanceId` numbering (SPEC_0001) —
//! must be indistinguishable from element-by-element expansion.
//!
//! Replication is only sound when the template produced no path-dependent
//! global side effects. `template_is_replicable` is that guard: it refuses
//! whenever inner/outer registration, diagnostics, any context stack, or the
//! template's own id accounting moved while the template was instantiated.

use rumoca_core::{ComponentPath, Span, StructuredIndexDomain};
use rumoca_eval_ast::component_family::{
    FamilyReindex, family_member_class, family_member_component,
};
use rumoca_ir_ast as ast;

use crate::{InstantiateContext, InstantiateError, InstantiateResult};

/// Insertion-order watermarks taken immediately before the template element is
/// instantiated.
pub(super) struct ReplicationWatermarks {
    components: usize,
    classes: usize,
    allocated_instances: u32,
    disabled_components: usize,
    each_modifier_bindings: usize,
    array_parent_dims: usize,
    component_families: usize,
    outer_prefix_to_inner: usize,
    inner_outer_to_parent_inner: usize,
    synthesized_inners: usize,
    inner_outer_events: usize,
    diagnostics: usize,
    context_depths: ContextDepths,
}

/// Context stack depths, used purely as a balance assertion.
#[derive(PartialEq, Eq)]
struct ContextDepths {
    context_path: usize,
    inner_scopes: usize,
    missing_inners: usize,
    scope_frames: usize,
    active_instantiations: usize,
    active_type_overrides: usize,
}

impl ContextDepths {
    fn capture(ctx: &InstantiateContext) -> Self {
        Self {
            context_path: ctx.context_path.len(),
            inner_scopes: ctx.inner_scopes.len(),
            missing_inners: ctx.missing_inners.len(),
            scope_frames: ctx.scope_frames.len(),
            active_instantiations: ctx.active_instantiations.len(),
            active_type_overrides: ctx.active_type_overrides.len(),
        }
    }
}

/// Capture the overlay and context state that the template element will extend.
pub(super) fn watermarks(
    ctx: &InstantiateContext,
    overlay: &ast::InstanceOverlay,
) -> ReplicationWatermarks {
    ReplicationWatermarks {
        components: overlay.components.len(),
        classes: overlay.classes.len(),
        allocated_instances: overlay.allocated_instance_count(),
        disabled_components: overlay.disabled_components.len(),
        each_modifier_bindings: overlay.each_modifier_bindings.len(),
        array_parent_dims: overlay.array_parent_dims.len(),
        component_families: overlay.component_families.len(),
        outer_prefix_to_inner: overlay.outer_prefix_to_inner.len(),
        inner_outer_to_parent_inner: overlay.inner_outer_to_parent_inner.len(),
        synthesized_inners: overlay.synthesized_inners.len(),
        inner_outer_events: ctx.inner_outer_events,
        diagnostics: ctx.diags.len(),
        context_depths: ContextDepths::capture(ctx),
    }
}

/// True when the template subtree produced no path-dependent global side
/// effects, so the remaining domain points are pure reindexed copies.
pub(super) fn template_is_replicable(
    ctx: &InstantiateContext,
    overlay: &ast::InstanceOverlay,
    before: &ReplicationWatermarks,
) -> bool {
    overlay.components.len() > before.components
        && ctx.inner_outer_events == before.inner_outer_events
        && ctx.diags.len() == before.diagnostics
        && overlay.outer_prefix_to_inner.len() == before.outer_prefix_to_inner
        && overlay.inner_outer_to_parent_inner.len() == before.inner_outer_to_parent_inner
        && overlay.synthesized_inners.len() == before.synthesized_inners
        && ContextDepths::capture(ctx) == before.context_depths
        && template_ids_are_accounted_for(overlay, before)
}

/// True when every id the template allocated is still reachable through the
/// overlay.
///
/// SPEC_0001: each replicated member must land on the same numeric ids the
/// scalar path would have produced, which only holds when the template consumed
/// exactly one id per snapshotted instance. A discarded id would shift every
/// later member, so refuse replication instead.
fn template_ids_are_accounted_for(
    overlay: &ast::InstanceOverlay,
    before: &ReplicationWatermarks,
) -> bool {
    let allocated = overlay
        .allocated_instance_count()
        .saturating_sub(before.allocated_instances) as usize;
    let snapshotted =
        (overlay.components.len() - before.components) + (overlay.classes.len() - before.classes);
    allocated == snapshotted
}

/// Identity of the array component whose remaining elements are being derived.
pub(super) struct ReplicationRequest<'a> {
    pub(super) domain: &'a StructuredIndexDomain,
    /// Path of the array component itself, without family subscripts.
    pub(super) root: &'a ComponentPath,
    /// Subscripts of the already-instantiated template domain point.
    pub(super) template_tuple: &'a [i64],
    /// Declaration span of the array component.
    pub(super) span: Span,
}

impl ReplicationRequest<'_> {
    /// Zero-based position of the family-subscripted segment in member paths.
    fn subscript_depth(&self) -> usize {
        self.root.len().saturating_sub(1)
    }

    fn segment(&self) -> &str {
        self.root
            .parts()
            .last()
            .map_or("", std::string::String::as_str)
    }
}

/// The template subtree, snapshotted before any overlay mutation so replication
/// never borrows the overlay while inserting into it.
struct TemplateSnapshot {
    components: Vec<ast::InstanceData>,
    classes: Vec<ast::ClassInstanceData>,
    /// Order in which the scalar path allocated the template's instance ids.
    plan: AllocationPlan,
    disabled_components: Vec<ComponentPath>,
    each_modifier_bindings: Vec<ComponentPath>,
    array_parent_dims: Vec<(String, Vec<i64>)>,
    /// Compact owners recorded by nested arrays inside the template element.
    nested_families: Vec<ast::InstanceComponentFamily>,
    known_int_params: Vec<(String, i64)>,
    known_bool_params: Vec<(String, bool)>,
    known_real_params: Vec<(String, f64)>,
}

fn snapshot_template(
    ctx: &InstantiateContext,
    overlay: &ast::InstanceOverlay,
    before: &ReplicationWatermarks,
    template_prefix: &str,
) -> TemplateSnapshot {
    let components: Vec<ast::InstanceData> = overlay
        .components
        .values()
        .skip(before.components)
        .cloned()
        .collect();
    let classes: Vec<ast::ClassInstanceData> = overlay
        .classes
        .values()
        .skip(before.classes)
        .cloned()
        .collect();
    let plan = AllocationPlan::from_template(&components, &classes);
    TemplateSnapshot {
        components,
        classes,
        plan,
        disabled_components: overlay
            .disabled_components
            .iter()
            .skip(before.disabled_components)
            .cloned()
            .collect(),
        each_modifier_bindings: overlay
            .each_modifier_bindings
            .iter()
            .skip(before.each_modifier_bindings)
            .cloned()
            .collect(),
        array_parent_dims: overlay
            .array_parent_dims
            .iter()
            .skip(before.array_parent_dims)
            .map(|(path, dims)| (path.clone(), dims.clone()))
            .collect(),
        nested_families: overlay
            .component_families
            .iter()
            .skip(before.component_families)
            .cloned()
            .collect(),
        known_int_params: ctx
            .known_int_params
            .iter()
            .filter(|(path, _)| path_is_under(path, template_prefix))
            .map(|(path, value)| (path.clone(), *value))
            .collect(),
        known_bool_params: ctx
            .known_bool_params
            .iter()
            .filter(|(path, _)| path_is_under(path, template_prefix))
            .map(|(path, value)| (path.clone(), *value))
            .collect(),
        known_real_params: ctx
            .known_real_params
            .iter()
            .filter(|(path, _)| path_is_under(path, template_prefix))
            .map(|(path, value)| (path.clone(), *value))
            .collect(),
    }
}

/// True when `path` is `prefix` itself or a descendant segment of it.
fn path_is_under(path: &str, prefix: &str) -> bool {
    path == prefix
        || (path.len() > prefix.len()
            && path.starts_with(prefix)
            && path.as_bytes()[prefix.len()] == b'.')
}

/// Derive every remaining domain point from the already-instantiated template.
pub(super) fn replicate_template(
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    before: &ReplicationWatermarks,
    request: &ReplicationRequest<'_>,
) -> InstantiateResult<ast::InstanceComponentFamily> {
    let depth = request.subscript_depth();
    let segment = request.segment().to_string();
    // Rendered instance segments enclosing the array component. Reindexing is
    // refused for any path that does not carry this exact prefix, so a sibling
    // instance reusing the segment name is never rewritten.
    let ancestors: Vec<String> = request.root.parts()[..depth].to_vec();
    let template_prefix = rendered_prefix(request.root, &segment, request.template_tuple);
    let template = snapshot_template(ctx, overlay, before, &template_prefix);
    let template_components: Vec<ast::InstanceId> = template
        .components
        .iter()
        .map(|data| data.instance_id)
        .collect();
    let template_classes: Vec<ast::InstanceId> = template
        .classes
        .iter()
        .map(|data| data.instance_id)
        .collect();

    let count = request
        .domain
        .scalar_count()
        .map_err(|error| domain_error(&template_prefix, &error.to_string(), request.span))?;
    for ordinal in 1..count {
        let tuple = request
            .domain
            .index_tuple_at(ordinal)
            .map_err(|error| domain_error(&template_prefix, &error.to_string(), request.span))?
            .ok_or_else(|| {
                domain_error(
                    &template_prefix,
                    &format!("missing domain ordinal {ordinal}"),
                    request.span,
                )
            })?;
        let reindex = FamilyReindex {
            ancestors: &ancestors,
            segment: &segment,
            depth,
            template_tuple: request.template_tuple,
            tuple: &tuple,
        };
        replicate_member(ctx, overlay, &template, &reindex, request.span)?;
    }

    Ok(ast::InstanceComponentFamily {
        domain: request.domain.clone(),
        root: request.root.clone(),
        subscript_depth: depth,
        template_components,
        template_classes,
        span: request.span,
    })
}

fn replicate_member(
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    template: &TemplateSnapshot,
    reindex: &FamilyReindex<'_>,
    span: Span,
) -> InstantiateResult<()> {
    // SPEC_0001: allocate first, in the scalar path's interleaving, then insert
    // in overlay order. Allocating while inserting would put every component of
    // the member ahead of every one of its classes, which is not what a
    // per-element scalar expansion produces.
    let ids: Vec<ast::InstanceId> = std::iter::repeat_with(|| overlay.alloc_id())
        .take(template.plan.len())
        .collect();
    let missing = |kind: &str| domain_error(&reindex.member_segment(), kind, span);
    for (position, data) in template.components.iter().enumerate() {
        let instance_id = template
            .plan
            .component_id(position, &ids)
            .ok_or_else(|| missing("member component id was not allocated"))?;
        let member = family_member_component(data, instance_id, reindex);
        ctx.register_known_integer_instance(&member);
        overlay.add_component(member);
    }
    for (position, data) in template.classes.iter().enumerate() {
        let instance_id = template
            .plan
            .class_id(position, &ids)
            .ok_or_else(|| missing("member class id was not allocated"))?;
        overlay.add_class(family_member_class(data, instance_id, reindex));
    }
    for path in &template.disabled_components {
        overlay
            .disabled_components
            .insert(reindex.component_path(path));
    }
    for path in &template.each_modifier_bindings {
        overlay
            .each_modifier_bindings
            .insert(reindex.component_path(path));
    }
    replicate_path_keyed_metadata(
        PathKeyedMetadataTarget { ctx, overlay },
        template,
        reindex,
        &ids,
        span,
    )
}

/// One instance of the template, addressed by its position in the snapshot.
#[derive(Clone, Copy)]
enum TemplateSlot {
    Component(usize),
    Class(usize),
}

/// The order in which the scalar path allocated the template's instance ids.
///
/// `instantiate_component` allocates a component's id before descending into
/// its class, and `instantiate_class` allocates the class id before its own
/// components, so a template's ids are already in allocation order. Merging the
/// two snapshot lists back together by id therefore recovers exactly the
/// component-then-its-class interleaving that per-element expansion produces,
/// which flat variable identity (`first_instance_index + instance_id`) depends
/// on (SPEC_0001).
#[derive(Default)]
struct AllocationPlan {
    /// Allocation rank of each template component, by snapshot position.
    component_ranks: Vec<usize>,
    /// Allocation rank of each template class, by snapshot position.
    class_ranks: Vec<usize>,
    /// Allocation rank of each template component, by template id.
    component_rank_by_id: rustc_hash::FxHashMap<ast::InstanceId, usize>,
    /// Allocation rank of each template class, by template id.
    class_rank_by_id: rustc_hash::FxHashMap<ast::InstanceId, usize>,
}

impl AllocationPlan {
    fn from_template(components: &[ast::InstanceData], classes: &[ast::ClassInstanceData]) -> Self {
        let mut slots: Vec<(ast::InstanceId, TemplateSlot)> = components
            .iter()
            .enumerate()
            .map(|(position, data)| (data.instance_id, TemplateSlot::Component(position)))
            .chain(
                classes
                    .iter()
                    .enumerate()
                    .map(|(position, data)| (data.instance_id, TemplateSlot::Class(position))),
            )
            .collect();
        slots.sort_unstable_by_key(|(id, _)| id.index());

        let mut plan = Self {
            component_ranks: vec![0; components.len()],
            class_ranks: vec![0; classes.len()],
            ..Self::default()
        };
        for (rank, (id, slot)) in slots.into_iter().enumerate() {
            match slot {
                TemplateSlot::Component(position) => {
                    plan.component_ranks[position] = rank;
                    plan.component_rank_by_id.insert(id, rank);
                }
                TemplateSlot::Class(position) => {
                    plan.class_ranks[position] = rank;
                    plan.class_rank_by_id.insert(id, rank);
                }
            }
        }
        plan
    }

    /// Number of ids one derived domain point consumes.
    fn len(&self) -> usize {
        self.component_ranks.len() + self.class_ranks.len()
    }

    fn component_id(&self, position: usize, ids: &[ast::InstanceId]) -> Option<ast::InstanceId> {
        ids.get(*self.component_ranks.get(position)?).copied()
    }

    fn class_id(&self, position: usize, ids: &[ast::InstanceId]) -> Option<ast::InstanceId> {
        ids.get(*self.class_ranks.get(position)?).copied()
    }

    fn map_components(
        &self,
        template_ids: &[ast::InstanceId],
        ids: &[ast::InstanceId],
    ) -> Vec<ast::InstanceId> {
        map_ranks(&self.component_rank_by_id, template_ids, ids)
    }

    fn map_classes(
        &self,
        template_ids: &[ast::InstanceId],
        ids: &[ast::InstanceId],
    ) -> Vec<ast::InstanceId> {
        map_ranks(&self.class_rank_by_id, template_ids, ids)
    }
}

fn map_ranks(
    ranks: &rustc_hash::FxHashMap<ast::InstanceId, usize>,
    template_ids: &[ast::InstanceId],
    ids: &[ast::InstanceId],
) -> Vec<ast::InstanceId> {
    template_ids
        .iter()
        .filter_map(|id| ids.get(*ranks.get(id)?).copied())
        .collect()
}

/// The mutable state that path-keyed instantiation metadata is written into.
struct PathKeyedMetadataTarget<'a> {
    ctx: &'a mut InstantiateContext,
    overlay: &'a mut ast::InstanceOverlay,
}

fn replicate_path_keyed_metadata(
    target: PathKeyedMetadataTarget<'_>,
    template: &TemplateSnapshot,
    reindex: &FamilyReindex<'_>,
    ids: &[ast::InstanceId],
    span: Span,
) -> InstantiateResult<()> {
    let PathKeyedMetadataTarget { ctx, overlay } = target;
    for (path, dims) in &template.array_parent_dims {
        let member_path = reindex
            .flat_path(path)
            .ok_or_else(|| domain_error(path, "array parent path is not a family member", span))?;
        overlay.array_parent_dims.insert(member_path, dims.clone());
    }
    for (path, value) in &template.known_int_params {
        let member_path = reindex.flat_path_or_same(path);
        ctx.known_int_params.insert(member_path, *value);
    }
    for (path, value) in &template.known_bool_params {
        let member_path = reindex.flat_path_or_same(path);
        ctx.known_bool_params.insert(member_path, *value);
    }
    for (path, value) in &template.known_real_params {
        let member_path = reindex.flat_path_or_same(path);
        ctx.known_real_params.insert(member_path, *value);
    }
    // A nested array inside the template element owns its own compact family;
    // each derived element needs the same owner rooted at its own path.
    for family in &template.nested_families {
        overlay.add_component_family(ast::InstanceComponentFamily {
            root: reindex.component_path(&family.root),
            template_components: template
                .plan
                .map_components(&family.template_components, ids),
            template_classes: template.plan.map_classes(&family.template_classes, ids),
            ..family.clone()
        });
    }
    Ok(())
}

/// Render `root` with its final segment carrying `tuple` (`plug_p.pin[1]`).
fn rendered_prefix(root: &ComponentPath, segment: &str, tuple: &[i64]) -> String {
    let mut parts: Vec<String> = root.parts().to_vec();
    if let Some(last) = parts.last_mut() {
        *last = render_segment(segment, tuple);
    }
    parts.join(".")
}

fn render_segment(name: &str, subscripts: &[i64]) -> String {
    use std::fmt::Write as _;

    let mut rendered = name.to_string();
    if subscripts.is_empty() {
        return rendered;
    }
    rendered.push('[');
    for (position, value) in subscripts.iter().enumerate() {
        if position > 0 {
            rendered.push(',');
        }
        let _ = write!(rendered, "{value}");
    }
    rendered.push(']');
    rendered
}

fn domain_error(path: &str, reason: &str, span: Span) -> Box<InstantiateError> {
    Box::new(InstantiateError::structural_param_error(
        format!("component family {path}"),
        reason.to_string(),
        span,
    ))
}
