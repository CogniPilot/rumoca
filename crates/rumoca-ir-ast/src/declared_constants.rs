//! Constant declarations keyed by declaration identity (MLS 3.7 section 5.3).
//!
//! A name resolved by Resolve carries the [`DefId`] of the declaration it
//! reaches, and a constant's binding is evaluated in the lexical scope of that
//! declaration, whichever class reads it. This index is the one place that maps
//! a resolved reference to the binding of the constant it names, so a package
//! constant or a chain of constant aliases evaluates from any reading scope.
//!
//! Only a declaration whose value no extends clause can change is indexed
//! (MLS 3.7 section 7.2): a `final` or protected declaration, or a constant no
//! extends modification names. A declaration that a package may modify takes
//! the value of its exposing package, which Flatten settles per exposure.

use crate::{ClassDef, ClassTree, ComponentReference, Expression};
use rumoca_core::{DefId, Variability};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::Arc;

/// Bindings of the fixed constant declarations of a class tree, by declaration id.
#[derive(Debug, Default, Clone)]
pub struct DeclaredConstants {
    bindings: Arc<FxHashMap<DefId, Expression>>,
}

impl DeclaredConstants {
    /// Index every fixed constant declaration with a binding in `tree`.
    pub fn from_tree(tree: &ClassTree) -> Self {
        let mut modified = FxHashSet::default();
        for class in tree.definitions.classes.values() {
            collect_modified_names(class, &mut modified);
        }
        let mut bindings = FxHashMap::default();
        for class in tree.definitions.classes.values() {
            collect_class(class, &modified, &mut bindings);
        }
        Self {
            bindings: Arc::new(bindings),
        }
    }

    /// The binding of the constant `reference` resolves to, if it names one.
    pub fn binding_of(&self, reference: &ComponentReference) -> Option<&Expression> {
        self.bindings.get(&reference.target_def_id()?)
    }

    /// The declaration id of the constant `reference` resolves to, if it names one.
    pub fn constant_id(&self, reference: &ComponentReference) -> Option<DefId> {
        let def_id = reference.target_def_id()?;
        self.bindings.contains_key(&def_id).then_some(def_id)
    }
}

fn collect_modified_names(class: &ClassDef, modified: &mut FxHashSet<String>) {
    for extend in &class.extends {
        for modification in &extend.modifications {
            if let Expression::Modification { target, .. } = &modification.expr
                && let Some(part) = target.parts.last()
            {
                modified.insert(part.ident.text.to_string());
            }
        }
    }
    for nested in class.classes.values() {
        collect_modified_names(nested, modified);
    }
}

fn collect_class(
    class: &ClassDef,
    modified: &FxHashSet<String>,
    bindings: &mut FxHashMap<DefId, Expression>,
) {
    for (name, component) in &class.components {
        let fixed = match component.variability {
            Variability::Constant(_) => {
                component.is_final || component.is_protected || !modified.contains(name)
            }
            Variability::Parameter(_) => component.is_final || component.is_protected,
            _ => false,
        };
        if fixed
            && let (Some(def_id), Some(binding)) = (component.def_id, component.binding.as_ref())
        {
            bindings.insert(def_id, binding.clone());
        }
    }
    for nested in class.classes.values() {
        collect_class(nested, modified, bindings);
    }
}
