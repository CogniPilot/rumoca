use rumoca_parser::{ast, Visitor};
use std::collections::HashMap;

//=============================================================================
/// Collects variables for each class
#[derive(Default)]
pub struct VarCollector<'a> {
    /// A struct to hold a dictionary of variables to component definitions
    pub vars: HashMap<String, &'a ast::ComponentDeclaration>,
}

impl<'a> Visitor<'a> for VarCollector<'a> {
    fn enter_component_declaration(&mut self, node: &'a ast::ComponentDeclaration) {
        self.vars.insert(node.declaration.name.clone(), node);
    }
}
