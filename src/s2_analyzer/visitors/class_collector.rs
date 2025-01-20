use rumoca_parser::{ast, Visitor};
use std::collections::HashMap;

//=============================================================================
/// Collects classes into a dictionary with names as keys
#[derive(Default)]
pub struct ClassCollector<'a> {
    /// A struct to traverse a tree and find all calsses and put references
    /// in a dictionary. The references have the same lifetime as the
    /// struct.
    pub classes: HashMap<String, &'a ast::ClassDefinition>,
}

impl<'a> Visitor<'a> for ClassCollector<'a> {
    /// Visits the parse tree, storing classes in a dictionary by name
    fn enter_class_definition(&mut self, node: &'a ast::ClassDefinition) {
        #[allow(clippy::single_match)]
        match &node.specifier {
            ast::ClassSpecifier::Long { name, .. } => {
                self.classes.insert(name.clone(), node);
            }
            _ => {}
        }
    }
}
