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
//! extends modification reaches. A modification reaches the constant its name
//! resolves to in the extended class hierarchy; a modification whose extended
//! class or target is not resolved to one declaration reaches every constant
//! of its name. A declaration that a package may modify takes the value of
//! its exposing package, which Flatten settles per exposure.

use crate::{ClassDef, ClassTree, ComponentRefPart, ComponentReference, Expression};
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
        let mut modified = Modified::default();
        for class in tree.definitions.classes.values() {
            collect_modified(tree, class, &mut modified);
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

/// The constant declarations extends modifications reach: by identity where
/// the modification resolves to one, and by name where it does not.
#[derive(Default)]
struct Modified {
    declarations: FxHashSet<DefId>,
    names: FxHashSet<String>,
}

impl Modified {
    fn reaches(&self, name: &str, def_id: Option<DefId>) -> bool {
        self.names.contains(name) || def_id.is_some_and(|id| self.declarations.contains(&id))
    }

    /// Record what a modification of `target` on an extends of `base` reaches.
    fn record(&mut self, tree: &ClassTree, base: Option<&ClassDef>, target: &[ComponentRefPart]) {
        match (base, target) {
            (Some(base), [part]) => {
                self.declarations.extend(constant_in_hierarchy(
                    tree,
                    base,
                    part.ident.text.as_ref(),
                ));
            }
            (_, [.., part]) => {
                self.names.insert(part.ident.text.to_string());
            }
            (_, []) => {}
        }
    }
}

fn collect_modified(tree: &ClassTree, class: &ClassDef, modified: &mut Modified) {
    for extend in &class.extends {
        let base = extend
            .base_def_id
            .and_then(|base_def_id| tree.get_class_by_def_id(base_def_id));
        for modification in &extend.modifications {
            let Expression::Modification { target, .. } = &modification.expr else {
                continue;
            };
            modified.record(tree, base, &target.parts);
        }
    }
    for nested in class.classes.values() {
        collect_modified(tree, nested, modified);
    }
}

fn collect_class(
    class: &ClassDef,
    modified: &Modified,
    bindings: &mut FxHashMap<DefId, Expression>,
) {
    for (name, component) in &class.components {
        let fixed = match component.variability {
            Variability::Constant(_) => {
                component.is_final
                    || component.is_protected
                    || !modified.reaches(name, component.def_id)
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

impl DeclaredConstants {
    /// The constants as one exposing package gives them (MLS 3.7 section 7.2).
    ///
    /// A constant that an extends clause modifies takes, for a declaration
    /// read through `exposing`, the value its outermost modification gives it;
    /// a constant that no modification on that package's extends chain names
    /// keeps its own binding. A reading scope that is not in the package's
    /// lexical or extends hierarchy never calls this and sees only the fixed
    /// declarations.
    pub fn exposed_by(&self, tree: &ClassTree, exposing: &ClassDef) -> Self {
        let mut bindings = (*self.bindings).clone();
        let mut pinned = FxHashSet::default();
        let mut visited = FxHashSet::default();
        apply_exposure(tree, exposing, &mut bindings, &mut pinned, &mut visited);
        Self {
            bindings: Arc::new(bindings),
        }
    }
}

fn apply_exposure(
    tree: &ClassTree,
    class: &ClassDef,
    bindings: &mut FxHashMap<DefId, Expression>,
    pinned: &mut FxHashSet<DefId>,
    visited: &mut FxHashSet<DefId>,
) {
    if let Some(def_id) = class.def_id
        && !visited.insert(def_id)
    {
        return;
    }
    for extend in &class.extends {
        let Some(base) = extend
            .base_def_id
            .and_then(|base_def_id| tree.get_class_by_def_id(base_def_id))
        else {
            continue;
        };
        for modification in &extend.modifications {
            let Expression::Modification { target, value, .. } = &modification.expr else {
                continue;
            };
            if let [part] = target.parts.as_slice()
                && let Some(def_id) = constant_in_hierarchy(tree, base, part.ident.text.as_ref())
                && pinned.insert(def_id)
            {
                bindings.insert(def_id, (**value).clone());
            }
        }
        apply_exposure(tree, base, bindings, pinned, visited);
    }
    for component in class.components.values() {
        if matches!(component.variability, Variability::Constant(_))
            && let (Some(def_id), Some(binding)) = (component.def_id, component.binding.as_ref())
            && !pinned.contains(&def_id)
        {
            bindings.entry(def_id).or_insert_with(|| binding.clone());
        }
    }
}

fn constant_in_hierarchy(tree: &ClassTree, class: &ClassDef, name: &str) -> Option<DefId> {
    if let Some(component) = class.components.get(name)
        && matches!(component.variability, Variability::Constant(_))
    {
        return component.def_id;
    }
    class.extends.iter().find_map(|extend| {
        let base = tree.get_class_by_def_id(extend.base_def_id?)?;
        constant_in_hierarchy(tree, base, name)
    })
}
