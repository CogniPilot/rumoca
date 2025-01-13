use rumoca_parser::{ast, Visitor};
use std::collections::HashMap;

//=============================================================================
/// Find all components, and expands component refs to strings
#[derive(Default, Debug)]
pub struct ComponentFinder<'a> {
    pub component_ref_to_str: HashMap<&'a ast::ComponentReference, String>,
    pub component_to_str: HashMap<&'a ast::ComponentDeclaration, String>,
    pub str_to_component: HashMap<String, &'a ast::ComponentDeclaration>,
    pub scope: Vec<String>,
}

impl<'a> Visitor<'a> for ComponentFinder<'a> {
    /// pushes class scope
    fn enter_class_definition(&mut self, node: &'a ast::ClassDefinition) {
        if let ast::ClassSpecifier::Long { name, .. } = &node.specifier {
            self.scope.push(name.clone());
        }
    }

    /// pops class scope
    fn exit_class_definition(&mut self, node: &'a ast::ClassDefinition) {
        if let ast::ClassSpecifier::Long { .. } = &node.specifier {
            self.scope.pop();
        }
    }

    /// creates lookup for component from name ane name from component
    fn enter_component_declaration(&mut self, node: &'a ast::ComponentDeclaration) {
        let s = format!("{}.{}", self.scope.join("."), node.declaration.name);
        self.component_to_str.insert(node, s.clone());
        self.str_to_component.insert(s.clone(), node);
    }

    /// expands component ref to string
    fn enter_component_reference(&mut self, node: &'a ast::ComponentReference) {
        let mut s: String = "".to_string();
        for (index, part) in node.parts.iter().enumerate() {
            if index != 0 || node.local {
                s += ".";
            }
            s += &part.name;
        }
        self.component_ref_to_str
            .insert(node, format!("{}.{}", self.scope.join("."), s));
    }
}
