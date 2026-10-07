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
//! resolves to in the extended class hierarchy, and nothing when it names a
//! declaration that is not a constant; a modification whose extended class is
//! not resolved, whose target is a path, or whose target is found nowhere in a
//! hierarchy with an unresolved extends clause reaches every constant of its
//! name, and an exposure then reads no constant of that name at its declared
//! value. A declaration that a package may modify takes the value of
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
        let Some(part) = target.last() else {
            return;
        };
        let name = part.ident.text.as_ref();
        let member = match (base, target) {
            (Some(base), [_]) => member_in_hierarchy(tree, base, name),
            _ => HierarchyMember::Unresolved,
        };
        match member {
            HierarchyMember::Constant(def_id) => self.declarations.extend(def_id),
            HierarchyMember::Other => {}
            HierarchyMember::Unresolved => {
                self.names.insert(name.to_string());
            }
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
        // A first pass finds the targets the hierarchy does not resolve, so
        // the second keeps every constant of such a name out wherever on the
        // chain it is declared.
        let mut probe = Exposure::new(&self.bindings, FxHashSet::default());
        probe.apply(tree, exposing);
        let mut exposure = Exposure::new(&self.bindings, probe.unresolved);
        exposure.apply(tree, exposing);
        Self {
            bindings: Arc::new(exposure.bindings),
        }
    }
}

/// The constant bindings one exposing package's extends chain settles.
struct Exposure {
    bindings: FxHashMap<DefId, Expression>,
    /// Declarations an outer modification already gave a value.
    pinned: FxHashSet<DefId>,
    /// Names of modification targets the hierarchy does not resolve: every
    /// constant of such a name may be modified, so none is read at its
    /// declared value.
    unresolved: FxHashSet<String>,
    visited: FxHashSet<DefId>,
}

impl Exposure {
    fn new(bindings: &FxHashMap<DefId, Expression>, unresolved: FxHashSet<String>) -> Self {
        Self {
            bindings: bindings.clone(),
            pinned: FxHashSet::default(),
            unresolved,
            visited: FxHashSet::default(),
        }
    }

    fn apply(&mut self, tree: &ClassTree, class: &ClassDef) {
        if let Some(def_id) = class.def_id
            && !self.visited.insert(def_id)
        {
            return;
        }
        for extend in &class.extends {
            let base = extend
                .base_def_id
                .and_then(|base_def_id| tree.get_class_by_def_id(base_def_id));
            for modification in &extend.modifications {
                let Expression::Modification { target, value, .. } = &modification.expr else {
                    continue;
                };
                let [part] = target.parts.as_slice() else {
                    continue;
                };
                let name = part.ident.text.as_ref();
                let member = base.map_or(HierarchyMember::Unresolved, |base| {
                    member_in_hierarchy(tree, base, name)
                });
                match member {
                    HierarchyMember::Constant(Some(def_id)) => {
                        if self.pinned.insert(def_id) {
                            self.bindings.insert(def_id, (**value).clone());
                        }
                    }
                    HierarchyMember::Constant(None) | HierarchyMember::Other => {}
                    HierarchyMember::Unresolved => {
                        self.unresolved.insert(name.to_string());
                    }
                }
            }
            if let Some(base) = base {
                self.apply(tree, base);
            }
        }
        for (name, component) in &class.components {
            if matches!(component.variability, Variability::Constant(_))
                && !self.unresolved.contains(name)
                && let (Some(def_id), Some(binding)) =
                    (component.def_id, component.binding.as_ref())
                && !self.pinned.contains(&def_id)
            {
                self.bindings
                    .entry(def_id)
                    .or_insert_with(|| binding.clone());
            }
        }
    }
}

/// What a modification target `name` denotes in the hierarchy of `class`.
enum HierarchyMember {
    /// A constant declaration (its id when it has one).
    Constant(Option<DefId>),
    /// A declaration that is not a constant.
    Other,
    /// No declaration found while some extends clause of the hierarchy is
    /// not resolved, so the target may be any constant of its name.
    Unresolved,
}

fn member_in_hierarchy(tree: &ClassTree, class: &ClassDef, name: &str) -> HierarchyMember {
    if let Some(component) = class.components.get(name) {
        return if matches!(component.variability, Variability::Constant(_)) {
            HierarchyMember::Constant(component.def_id)
        } else {
            HierarchyMember::Other
        };
    }
    let mut unresolved = false;
    for extend in &class.extends {
        let Some(base) = extend
            .base_def_id
            .and_then(|base_def_id| tree.get_class_by_def_id(base_def_id))
        else {
            unresolved = true;
            continue;
        };
        match member_in_hierarchy(tree, base, name) {
            HierarchyMember::Unresolved => unresolved = true,
            found => return found,
        }
    }
    if unresolved {
        HierarchyMember::Unresolved
    } else {
        HierarchyMember::Other
    }
}
