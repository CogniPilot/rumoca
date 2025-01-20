use rumoca_parser::{ast, VisitorMut};

//=============================================================================
/// Mutably expand all component refernces to strings in a tree
#[derive(Default, Debug)]
pub struct CompNameExpander {
    pub scope: Vec<ast::RefPart>,
}

impl VisitorMut for CompNameExpander {
    /// pushes class scope
    fn enter_class_definition_mut(&mut self, node: &mut ast::ClassDefinition) {
        if let ast::ClassSpecifier::Long { name, .. } = &node.specifier {
            self.scope.push(ast::RefPart {
                name: name.clone(),
                ..Default::default()
            })
        }
    }

    /// pops class scope
    fn exit_class_definition_mut(&mut self, node: &mut ast::ClassDefinition) {
        if let ast::ClassSpecifier::Long { .. } = &node.specifier {
            self.scope.pop();
        }
    }

    /// expands component ref to string
    fn enter_component_reference_mut(&mut self, node: &mut ast::ComponentReference) {
        let mut new_parts = self.scope.clone();
        new_parts.append(&mut node.parts);
        node.parts = new_parts;
    }
}
