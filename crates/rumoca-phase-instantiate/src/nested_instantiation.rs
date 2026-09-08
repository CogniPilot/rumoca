use super::*;

pub(super) struct NestedInstantiationInput<'a> {
    pub(super) instance_id: rumoca_core::InstanceId,
    pub(super) nested_class: &'a ast::ClassDef,
    pub(super) comp: &'a ast::Component,
    pub(super) effective_variability: &'a rumoca_core::Variability,
    pub(super) causality: &'a rumoca_core::Causality,
    pub(super) flow: bool,
    pub(super) stream: bool,
    pub(super) binding_for_record_expansion: Option<&'a ast::Expression>,
    pub(super) binding_source_for_record_expansion: Option<&'a ast::Expression>,
    pub(super) binding_scope_for_record_expansion: Option<&'a ast::QualifiedName>,
    pub(super) binding_is_each: bool,
    pub(super) effective_components: &'a IndexMap<String, ast::Component>,
    pub(super) type_overrides: &'a TypeOverrideMap,
    pub(super) component_type_selections: crate::nested_scope::NestedComponentTypeSelections,
    /// Import aliases of the class that wrote these modifications (MLS §13.2).
    pub(super) modifier_imports: crate::dims::ImportRewrite<'a>,
}

pub(super) fn instantiate_nested_class(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    input: NestedInstantiationInput<'_>,
) -> InstantiateResult<()> {
    let mod_env_snapshot = prepare_nested_modifications(tree, ctx, &input)?;
    let record_span = location_to_span(
        &input.comp.location,
        &tree.source_map,
        "effective equalityConstraint record occurrence",
    )?;
    if input.nested_class.class_type == rumoca_core::ClassType::Record {
        construct_equality_constraint_exposure(
            tree,
            input.nested_class,
            overlay,
            input.instance_id,
            ctx,
            record_span,
        )?;
    }
    let result = instantiate_nested_body(tree, ctx, overlay, input);
    ctx.mod_env_mut().active = mod_env_snapshot;
    result
}

fn prepare_nested_modifications(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    input: &NestedInstantiationInput<'_>,
) -> InstantiateResult<IndexMap<ast::QualifiedName, ast::ModificationValue>> {
    // MLS §7.2: nested component modifications are scoped to this occurrence.
    let mod_env_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys = collect_shifted_parent_mod_keys(input.comp, &mod_env_snapshot);
    let targeted_keys = collect_targeted_mod_keys(input.comp, &mod_env_snapshot);
    shift_modifications_down(ctx, &input.comp.name);
    populate_modification_environment(
        ctx,
        tree,
        PopulateModEnvInput {
            comp: input.comp,
            effective_components: input.effective_components,
            type_overrides: input.type_overrides,
            target_class: Some(input.nested_class),
            parent_snapshot: &mod_env_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
            modifier_imports: input.modifier_imports,
        },
    )?;
    let record_projected_keys = if let Some(binding) = input.binding_for_record_expansion {
        propagate_record_binding_to_fields(
            tree,
            ctx,
            RecordBindingProjection {
                value: binding,
                source: input.binding_source_for_record_expansion,
                source_scope: input.binding_scope_for_record_expansion.cloned(),
                each: input.binding_is_each,
            },
            input.nested_class,
            &targeted_keys,
        )?
    } else {
        IndexMap::default()
    };
    let referenced_mod_roots = collect_referenced_mod_roots(input.comp);
    ctx.mod_env_mut().active.retain(|key, _| {
        !mod_env_snapshot.contains_key(key)
            || record_projected_keys.contains_key(key)
            || targeted_keys.contains_key(key)
            || key_matches_referenced_root(key, &referenced_mod_roots)
    });
    Ok(mod_env_snapshot)
}

fn instantiate_nested_body(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    input: NestedInstantiationInput<'_>,
) -> InstantiateResult<()> {
    let (evaluate_annotation, explicitly_disables_structural_evaluation) = {
        let eval_ctx = InstantiateEvalCtx {
            tree,
            mod_env: ctx.mod_env(),
            effective_components: input.effective_components,
            resolve_class_components: resolve_effective_components_for_eval,
        };
        (
            component_has_evaluate_annotation(input.comp, &eval_ctx),
            component_explicitly_disables_structural_evaluation(
                &input.comp.name,
                input.comp,
                &eval_ctx,
            ),
        )
    };
    ctx.push_scope_frame(ScopeFrameInput {
        variability: input.effective_variability,
        evaluate: evaluate_annotation || ctx.inherited_evaluate(),
        structural_evaluation_blocked: ctx.structural_evaluation_blocked()
            || (matches!(
                input.effective_variability,
                rumoca_core::Variability::Parameter(_)
            ) && explicitly_disables_structural_evaluation),
        causality: input.causality,
        flow: input.flow,
        stream: input.stream,
        expandable: input.nested_class.expandable,
        protected: input.comp.is_protected,
    });

    let active_package_alias = active_package_constant_alias(input.comp, input.type_overrides);
    if let Some(alias) = active_package_alias.as_ref() {
        ctx.active_package_constant_aliases.push(alias.clone());
    }
    ctx.active_type_overrides.push(input.type_overrides.clone());
    let result = instantiate_class(
        tree,
        input.modifier_imports.class_index,
        input.nested_class,
        ClassOccurrenceConstruction::Nested {
            owner_component_id: input.instance_id,
            component_type_selections: input.component_type_selections,
        },
        ctx,
        overlay,
    );
    ctx.active_type_overrides.pop();
    if active_package_alias.is_some() {
        ctx.active_package_constant_aliases.pop();
    }
    ctx.pop_scope_frame();

    result?;
    Ok(())
}
