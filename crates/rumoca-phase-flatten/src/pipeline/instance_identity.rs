use rumoca_ir_ast::{ClassTree, InstanceData, InstanceId};
use rumoca_ir_flat::Model;

pub(crate) fn assign_instance_identity_to_flat_variable(
    flat: &mut Model,
    flat_var: &mut rumoca_ir_flat::Variable,
    tree: &ClassTree,
    instance_data: &InstanceData,
) {
    let instance_def_id = instance_def_id(tree, instance_data.instance_id);
    let source_def_id = flat_var
        .component_ref
        .as_ref()
        .and_then(|component_ref| component_ref.def_id);
    if let Some(component_ref) = flat_var.component_ref.as_mut() {
        component_ref.def_id = Some(instance_def_id);
    }
    if let Some(source_def_id) = source_def_id {
        let mut ancestry = flat
            .symbol_ancestry
            .get(&source_def_id)
            .cloned()
            .unwrap_or_default();
        if !ancestry.contains(&source_def_id) {
            ancestry.push(source_def_id);
        }
        flat.symbol_ancestry.insert(instance_def_id, ancestry);
    }
}

fn instance_def_id(tree: &ClassTree, instance_id: InstanceId) -> rumoca_core::DefId {
    let source_max = tree
        .def_map
        .keys()
        .map(|def_id| def_id.index())
        .max()
        .unwrap_or(0);
    rumoca_core::DefId::new(
        source_max
            .saturating_add(1)
            .saturating_add(instance_id.index()),
    )
}
