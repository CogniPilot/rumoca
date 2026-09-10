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
        Self { levels }
    }

    /// The class a simple name denotes, innermost declaration first.
    pub(crate) fn find(&self, name: &str) -> Option<&'a ast::ClassDef> {
        self.levels.iter().find_map(|level| level.classes.get(name))
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
}
