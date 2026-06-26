//! Phase 1: Registration - assign DefIds and build scope tree.
//!
//! This phase walks all classes and registers DefIds, creating scopes
//! for the scope tree. All names are registered before resolution begins.

use crate::Resolver;
use rumoca_core::{ComponentPath, ScopeId};
use rumoca_ir_ast as ast;

impl Resolver {
    /// Register all class and component names in a ast::StoredDefinition.
    pub(crate) fn register_stored_definition(
        &mut self,
        def: &mut ast::StoredDefinition,
        scope: ScopeId,
        prefix: &str,
    ) {
        // Register all top-level class names
        for (name, class) in def.classes.iter_mut() {
            let enclosing = prefix.trim_end_matches('.');
            let def_id = self.alloc_def_id((!enclosing.is_empty()).then_some(enclosing), name);
            class.def_id = Some(def_id);
            self.class_types.insert(def_id, class.class_type.clone());
            self.scope_tree
                .add_member(scope, ComponentPath::from_flat_path(name), def_id);
            if class.is_replaceable {
                self.partial_type_root_ids.insert(def_id);
            }
        }

        // Recursively register nested classes
        for (name, class) in def.classes.iter_mut() {
            let qualified_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            self.register_class(class, scope, &qualified_name);
        }
    }

    /// Register all names in a ast::ClassDef (Phase 1).
    pub(crate) fn register_class(
        &mut self,
        class: &mut ast::ClassDef,
        enclosing: ScopeId,
        qualified_name: &str,
    ) {
        // Create a scope for this class
        let scope_kind = if class.encapsulated {
            ast::ScopeKind::Encapsulated
        } else {
            ast::ScopeKind::Class
        };
        let class_scope = self.scope_tree.create_scope(enclosing, scope_kind);
        class.scope_id = Some(class_scope);
        if class.encapsulated {
            self.encapsulated_class_names
                .insert(qualified_name.to_string());
        }
        if let Some(class_def_id) = class.def_id {
            self.scope_to_class_def.insert(class_scope, class_def_id);
            self.class_def_scopes.insert(class_def_id, class_scope);
        }

        // Register component names
        for (name, comp) in class.components.iter_mut() {
            let def_id = self.alloc_def_id(Some(qualified_name), name);
            comp.def_id = Some(def_id);
            self.component_variabilities
                .insert(def_id, comp.variability.clone());
            self.scope_tree
                .add_member(class_scope, ComponentPath::from_flat_path(name), def_id);
            if comp.is_replaceable {
                self.partial_type_root_ids.insert(def_id);
            }
        }

        // Register nested class names
        for (name, nested) in class.classes.iter_mut() {
            let def_id = self.alloc_def_id(Some(qualified_name), name);
            nested.def_id = Some(def_id);
            self.class_types.insert(def_id, nested.class_type.clone());
            self.scope_tree
                .add_member(class_scope, ComponentPath::from_flat_path(name), def_id);
            if nested.is_replaceable {
                self.partial_type_root_ids.insert(def_id);
            }
        }

        // Recursively register nested classes
        for (name, nested) in class.classes.iter_mut() {
            let nested_qualified = format!("{}.{}", qualified_name, name);
            self.register_class(nested, class_scope, &nested_qualified);
        }
    }
}
