use super::*;

/// Resolve a function call's component reference to its fully qualified name.
/// Uses the exact callable identity from the resolve-phase contract.
pub(crate) fn resolve_function_name(
    comp: &rumoca_ir_ast::ComponentReference,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> String {
    let textual_name = QualifiedName::from_component_reference(comp).to_flat_string();

    if let Some(def_id) = comp.target_def_id
        && let Some(base_name) = tree.def_map.get(&def_id)
        && let Some(class_def) = class_index.get_by_qualified_name(base_name)
        && class_def.class_type == rumoca_core::ClassType::Function
    {
        return base_name.clone();
    }

    #[cfg(feature = "tracing")]
    if comp.target_def_id.is_some() {
        tracing::warn!(
            "Function call has target_def_id {:?} but not found in def_map: {}",
            comp.target_def_id,
            comp
        );
    } else {
        tracing::debug!("Function call without def_id: {}", textual_name);
    }
    textual_name
}
