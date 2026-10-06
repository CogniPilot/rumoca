//! Constant declarations keyed by declaration identity (MLS 3.7 section 5.3).
//!
//! A name resolved by Resolve carries the [`DefId`] of the declaration it
//! reaches, and a constant's binding is evaluated in the lexical scope of that
//! declaration, whichever class reads it. This index is the one place that maps
//! a resolved reference to the binding of the constant it names, so a package
//! constant or a chain of constant aliases evaluates from any reading scope.

use rumoca_core::{DefId, Variability};
use rumoca_ir_ast::{ClassDef, ClassTree, ComponentReference, Expression};
use rustc_hash::FxHashMap;

/// Bindings of every constant declaration in a class tree, by declaration id.
#[derive(Debug, Default, Clone)]
pub struct DeclaredConstants {
    bindings: FxHashMap<DefId, Expression>,
}

impl DeclaredConstants {
    /// Index every constant with a declaration binding in `tree`.
    pub fn from_tree(tree: &ClassTree) -> Self {
        let mut constants = Self::default();
        for class in tree.definitions.classes.values() {
            constants.collect_class(class);
        }
        constants
    }

    fn collect_class(&mut self, class: &ClassDef) {
        for component in class.components.values() {
            if matches!(component.variability, Variability::Constant(_))
                && let (Some(def_id), Some(binding)) =
                    (component.def_id, component.binding.as_ref())
            {
                self.bindings.insert(def_id, binding.clone());
            }
        }
        for nested in class.classes.values() {
            self.collect_class(nested);
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

/// Cycle guard for evaluating constant alias chains: a constant that is being
/// evaluated is never entered again.
#[derive(Debug, Default)]
pub struct ConstantWalk {
    active: std::cell::RefCell<Vec<DefId>>,
}

impl ConstantWalk {
    /// Run `eval` on the binding of the constant `reference` names, unless that
    /// constant is already being evaluated (a cyclic declaration has no value).
    pub fn enter<T>(
        &self,
        constants: &DeclaredConstants,
        reference: &ComponentReference,
        eval: impl FnOnce(&Expression) -> Option<T>,
    ) -> Option<T> {
        let def_id = constants.constant_id(reference)?;
        if self.active.borrow().contains(&def_id) {
            return None;
        }
        self.active.borrow_mut().push(def_id);
        let value = constants.binding_of(reference).and_then(eval);
        self.active.borrow_mut().pop();
        value
    }
}
