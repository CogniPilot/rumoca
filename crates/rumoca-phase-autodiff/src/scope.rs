//! Name lookup for the functions a synthesis run may descend into.
//!
//! A scope is the lexical chain from the class that owns a call site outward
//! to the file's top level, innermost first. Lookup takes the first class that
//! declares the name, which is Modelica's own rule and is what makes a
//! generated function, minted as a sibling of the function it differentiates,
//! see exactly what its source saw.

use rumoca_ir_ast as ast;

/// One level of a lexical chain: the classes declared there, and the path that
/// reaches that level from the file's top level.
struct Level<'a> {
    path: Vec<String>,
    classes: &'a ast::AstIndexMap<String, ast::ClassDef>,
}

/// A borrowed lexical chain of classes, innermost first.
pub(crate) struct FunctionScope<'a> {
    definition: &'a ast::StoredDefinition,
    levels: Vec<Level<'a>>,
}

impl<'a> FunctionScope<'a> {
    /// The chain reaching `path` inside `definition`, innermost first.
    ///
    /// `path` names the classes from the file's top level down to the class
    /// that owns the construct being differentiated.
    pub(crate) fn reaching(definition: &'a ast::StoredDefinition, path: &[String]) -> Self {
        let mut levels = vec![Level {
            path: Vec::new(),
            classes: &definition.classes,
        }];
        let mut classes = &definition.classes;
        let mut walked = Vec::new();
        for name in path {
            let Some(class) = classes.get(name) else {
                break;
            };
            classes = &class.classes;
            walked.push(name.clone());
            levels.push(Level {
                path: walked.clone(),
                classes,
            });
        }
        levels.reverse();
        Self { definition, levels }
    }

    /// The class a simple name denotes, innermost declaration first.
    pub(crate) fn find(&self, name: &str) -> Option<&'a ast::ClassDef> {
        self.levels.iter().find_map(|level| level.classes.get(name))
    }

    /// Find a simple class name and the lexical path that owns it.
    pub(crate) fn find_with_owner(&self, name: &str) -> Option<(&'a ast::ClassDef, Vec<String>)> {
        self.levels.iter().find_map(|level| {
            level
                .classes
                .get(name)
                .map(|class| (class, level.path.clone()))
        })
    }

    /// Resolve a name path to its class and the path of the class that owns it.
    pub(crate) fn locate(&self, parts: &[String]) -> Option<(&'a ast::ClassDef, Vec<String>)> {
        let (head, rest) = parts.split_first()?;
        for level in &self.levels {
            let Some(mut class) = level.classes.get(head) else {
                continue;
            };
            let mut owner = level.path.clone();
            for part in rest {
                owner.push(class.name.text.to_string());
                class = class.classes.get(part)?;
            }
            return Some((class, owner));
        }
        None
    }

    /// Prove that a simple primitive spelling cannot be shadowed before the
    /// predefined scope is reached.
    ///
    /// This is deliberately an absence certificate, not a second name
    /// resolver. Any import, inheritance, or unavailable enclosing package
    /// that could affect lookup makes the proof fail closed.
    pub(crate) fn predefined_type_shadow_reason(
        &self,
        class: &ast::ClassDef,
        owner: &[String],
        spelling: &str,
    ) -> Option<String> {
        if let Some(reason) = class_shadow_reason(class, spelling) {
            return Some(reason);
        }
        if class.encapsulated {
            return None;
        }

        let mut classes = &self.definition.classes;
        let mut enclosing = Vec::new();
        for name in owner {
            let Some(owner_class) = classes.get(name) else {
                return Some(format!(
                    "the owning lexical scope `{}` is unavailable",
                    owner.join(".")
                ));
            };
            enclosing.push(owner_class);
            classes = &owner_class.classes;
        }
        for owner_class in enclosing.into_iter().rev() {
            if let Some(reason) = class_shadow_reason(owner_class, spelling) {
                return Some(reason);
            }
            if owner_class.encapsulated {
                return None;
            }
        }

        if self.definition.classes.contains_key(spelling) {
            return Some(format!(
                "the top-level source scope declares a class or type named `{spelling}`"
            ));
        }
        if self
            .definition
            .within
            .as_ref()
            .is_some_and(|within| !within.name.is_empty())
        {
            return Some(
                "the source has a `within` package whose enclosing declarations are unavailable"
                    .to_string(),
            );
        }
        None
    }
}

fn class_shadow_reason(class: &ast::ClassDef, spelling: &str) -> Option<String> {
    if class.classes.contains_key(spelling) {
        return Some(format!(
            "`{}` declares a local class or type named `{spelling}`",
            class.name.text
        ));
    }
    if class.components.contains_key(spelling) {
        return Some(format!(
            "`{}` declares a local component named `{spelling}`",
            class.name.text
        ));
    }
    if class
        .imports
        .iter()
        .any(|import| import_may_bind(import, spelling))
    {
        return Some(format!(
            "`{}` has an import that can bind `{spelling}`",
            class.name.text
        ));
    }
    if !class.extends.is_empty() {
        return Some(format!(
            "`{}` has an extends clause, so inherited `{spelling}` lookup is not proven absent",
            class.name.text
        ));
    }
    None
}

fn import_may_bind(import: &ast::Import, spelling: &str) -> bool {
    match import {
        ast::Import::Qualified { path, .. } => path
            .name
            .last()
            .is_some_and(|part| part.text.as_ref() == spelling),
        ast::Import::Renamed { alias, .. } => alias.text.as_ref() == spelling,
        ast::Import::Selective { names, .. } => names
            .iter()
            .any(|selected| selected.text.as_ref() == spelling),
        ast::Import::Unqualified { .. } => true,
    }
}
