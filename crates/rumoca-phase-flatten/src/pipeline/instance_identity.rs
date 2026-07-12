use rumoca_ir_ast::{ClassDefIndex, ClassTree, InstanceData, InstanceId};
use rumoca_ir_flat::Model;

#[derive(Debug, Clone, Copy)]
pub(crate) struct InstanceIdentitySpace {
    first_instance_index: u32,
}

impl InstanceIdentitySpace {
    pub(crate) fn from_tree(tree: &ClassTree) -> Self {
        let first_instance_index = tree
            .def_map
            .keys()
            .map(|def_id| def_id.index())
            .max()
            .unwrap_or(0)
            .saturating_add(1);
        Self {
            first_instance_index,
        }
    }

    fn def_id(self, instance_id: InstanceId) -> rumoca_core::DefId {
        rumoca_core::DefId::new(
            self.first_instance_index
                .saturating_add(instance_id.index()),
        )
    }
}

pub(crate) fn assign_instance_identity_to_flat_variable(
    flat: &mut Model,
    flat_var: &mut rumoca_ir_flat::Variable,
    identity_space: InstanceIdentitySpace,
    class_index: &ClassDefIndex<'_>,
    instance_data: &InstanceData,
) {
    let instance_def_id = identity_space.def_id(instance_data.instance_id);
    let source_def_id = flat_var
        .component_ref
        .as_ref()
        .and_then(|component_ref| component_ref.def_id);
    if let Some(component_ref) = flat_var.component_ref.as_mut() {
        component_ref.def_id = Some(instance_def_id);
    }
    if let Some(source_def_id) = source_def_id {
        let ancestry = instance_ancestry(flat, source_def_id, || {
            class_index.def_ancestry(source_def_id)
        });
        flat.symbol_ancestry.insert(instance_def_id, ancestry);
    }
}

fn instance_ancestry<F>(
    flat: &mut Model,
    source_def_id: rumoca_core::DefId,
    load: F,
) -> rumoca_core::SymbolAncestry
where
    F: FnOnce() -> Vec<rumoca_core::DefId>,
{
    if let Some(ancestry) = flat.symbol_ancestry.get(&source_def_id) {
        return ancestry.clone();
    }
    let ancestry: rumoca_core::SymbolAncestry = load().into();
    flat.symbol_ancestry.insert(source_def_id, ancestry.clone());
    ancestry
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::sync::Arc;

    use super::*;

    #[test]
    fn identity_space_keeps_instance_ids_disjoint_from_source_ids() {
        let mut tree = ClassTree::new();
        tree.def_map
            .insert(rumoca_core::DefId::new(3), "P.x".to_string());
        tree.def_map
            .insert(rumoca_core::DefId::new(17), "P.y".to_string());

        let identity_space = InstanceIdentitySpace::from_tree(&tree);

        assert_eq!(identity_space.def_id(InstanceId::new(0)).index(), 18);
        assert_eq!(identity_space.def_id(InstanceId::new(9)).index(), 27);
    }

    #[test]
    fn sibling_instances_share_immutable_source_ancestry() {
        let source = rumoca_core::DefId::new(7);
        let parent = rumoca_core::DefId::new(2);
        let mut flat = Model::new();
        let loads = Cell::new(0);

        let first = instance_ancestry(&mut flat, source, || {
            loads.set(loads.get() + 1);
            vec![parent, source]
        });
        let second = instance_ancestry(&mut flat, source, || {
            loads.set(loads.get() + 1);
            vec![parent, source]
        });

        assert_eq!(first.as_ref(), [parent, source]);
        assert!(Arc::ptr_eq(&first, &second));
        assert_eq!(loads.get(), 1, "source ancestry should be loaded once");
    }
}
