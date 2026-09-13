//! Value-independent rank proofs for resolved AST component references.

use rumoca_core::{ClassType, ComponentPath, DefId};
use rumoca_ir_ast::{ClassDef, ClassTree, ComponentReference, InstanceOverlay};
use std::collections::HashSet;

/// Scalar declarations whose complete type and reference paths prove rank zero.
/// Array and replaceable declarations remain unknown until instance facts exist.
#[derive(Default)]
pub struct DeclaredDimensions {
    namespaces: HashSet<DefId>,
    scalar_components: HashSet<DefId>,
}

impl DeclaredDimensions {
    /// Add only rank facts shared by every occurrence of a declaration in this
    /// overlay. This is a universal proof, not selection of a runtime instance:
    /// one array occurrence or unapplied redeclaration vetoes the scalar fact.
    pub fn from_instanced(tree: &ClassTree, overlay: &InstanceOverlay) -> Self {
        let mut result = Self::from_resolved_tree(tree);
        let mut scalar = HashSet::new();
        let mut unknown_or_array: HashSet<_> = overlay
            .array_component_declarations
            .iter()
            .copied()
            .collect();
        for data in overlay.components.values() {
            let Some(part) = data
                .component_ref
                .as_ref()
                .and_then(|reference| reference.parts().last())
            else {
                continue;
            };
            if !data.has_unapplied_redeclare
                && data.dims.is_empty()
                && data.dims_expr.is_empty()
                && part.subs.is_empty()
            {
                scalar.insert(part.def_id);
            } else {
                unknown_or_array.insert(part.def_id);
            }
        }
        result.scalar_components.extend(scalar);
        result
            .scalar_components
            .retain(|id| !unknown_or_array.contains(id));
        result
    }

    /// Build from Resolve identities and declaration syntax (MLS §10.1).
    pub fn from_resolved_tree(tree: &ClassTree) -> Self {
        let classes: Vec<_> = tree
            .name_map
            .values()
            .filter_map(|id| tree.get_class_by_def_id(*id))
            .collect();
        let scalar_types = declared_scalar_types(tree, &classes);
        let mut result = Self::default();
        for class in classes {
            if let Some(id) = class.def_id
                && !class.is_replaceable
            {
                result.namespaces.insert(id);
            }
            result.collect_scalar_components(class, &scalar_types);
        }
        result
    }

    fn collect_scalar_components(&mut self, class: &ClassDef, scalar_types: &HashSet<DefId>) {
        for component in class.components.values() {
            if let Some(id) = component.def_id
                && !component.is_replaceable
                && !component.redeclared_by_modification
                && component.shape.is_empty()
                && component.shape_expr.is_empty()
                && component
                    .type_def_id
                    .is_some_and(|ty| scalar_types.contains(&ty))
            {
                self.scalar_components.insert(id);
            }
        }
    }

    /// Every component prefix contributes rank; a scalar field of an array
    /// component does not establish a scalar reference (MLS §10.6.9).
    pub fn proves_scalar_reference(&self, reference: &ComponentReference) -> bool {
        let Some((target, parents)) = reference.parts.split_last() else {
            return false;
        };
        target.subs.as_ref().is_none_or(Vec::is_empty)
            && target
                .def_id
                .is_some_and(|id| self.scalar_components.contains(&id))
            && parents.iter().all(|part| {
                part.subs.as_ref().is_none_or(Vec::is_empty)
                    && part.def_id.is_some_and(|id| {
                        self.namespaces.contains(&id) || self.scalar_components.contains(&id)
                    })
            })
    }
}

fn declared_scalar_types(tree: &ClassTree, classes: &[&ClassDef]) -> HashSet<DefId> {
    let mut scalar_types: HashSet<_> = ["Real", "Integer", "Boolean", "String", "Clock"]
        .iter()
        .filter_map(|name| {
            tree.scope_tree
                .predefined_member(&ComponentPath::from_flat_path(name))
        })
        .collect();
    // The type table normalizes aliases to their element types. Retain the
    // declaration's array subscripts so an array alias cannot prove a scalar.
    loop {
        let previous = scalar_types.len();
        for class in classes {
            if let Some(id) = class.def_id
                && class_has_scalar_rank(class, &scalar_types)
            {
                scalar_types.insert(id);
            }
        }
        if scalar_types.len() == previous {
            break;
        }
    }
    scalar_types
}

fn class_has_scalar_rank(class: &ClassDef, scalar_types: &HashSet<DefId>) -> bool {
    if class.is_replaceable || !class.array_subscripts.is_empty() {
        return false;
    }
    let value_class = match class.class_type {
        ClassType::Type => !class.enum_literals.is_empty() || !class.extends.is_empty(),
        ClassType::Model
        | ClassType::Class
        | ClassType::Block
        | ClassType::Connector
        | ClassType::Record => true,
        ClassType::Package | ClassType::Function | ClassType::Operator => false,
    };
    value_class
        && class.extends.iter().all(|base| {
            base.base_def_id
                .is_some_and(|id| scalar_types.contains(&id))
        })
}
