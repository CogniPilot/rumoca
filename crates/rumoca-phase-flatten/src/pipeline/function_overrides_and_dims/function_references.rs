use super::*;

pub(super) fn function_local_def_ids(
    function: &rumoca_core::Function,
) -> FxHashSet<rumoca_core::DefId> {
    function
        .inputs
        .iter()
        .chain(function.outputs.iter())
        .chain(function.locals.iter())
        .filter_map(|param| param.def_id)
        .collect()
}

pub(super) fn reference_targets_function_local_def(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> bool {
    reference
        .target_def_id()
        .is_some_and(|def_id| ctx.local_def_ids.contains(&def_id))
}

pub(super) fn rewritten_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> rumoca_core::Reference {
    rewritten_function_reference(original, resolved_name, ctx.tree, ctx.class_index)
}

pub(super) fn rewritten_function_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> rumoca_core::Reference {
    let Some(mut component_ref) = original.component_ref().cloned() else {
        return rumoca_core::Reference::new(resolved_name);
    };
    component_ref.def_id = tree.name_map.get(&resolved_name).copied().or_else(|| {
        class_index
            .get_by_qualified_name(&resolved_name)
            .and_then(|class_def| class_def.def_id)
    });
    component_ref.parts = ComponentPath::from_flat_path(&resolved_name)
        .parts()
        .iter()
        .map(|part| rumoca_core::ComponentRefPart {
            ident: part.clone(),
            span: component_ref.span,
            subs: Vec::new(),
        })
        .collect();
    rumoca_core::Reference::with_component_reference(resolved_name, component_ref)
}
