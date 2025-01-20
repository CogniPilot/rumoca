use ordermap::OrderSet;
use rumoca_parser::{ast, VisitorMut};
use std::collections::HashMap;

//=============================================================================
/// Expands extends clause
#[derive(Default, Debug)]
pub struct ClassExploder<'a> {
    pub classes: HashMap<String, &'a ast::ClassDefinition>,
    pub classes_to_expand: OrderSet<String>,
}

impl VisitorMut for ClassExploder<'_> {
    fn enter_component_clause_mut(&mut self, node: &mut ast::ComponentClause) {
        let mut name = node.type_specifier.name.join(".");
        if node.type_specifier.local {
            name = format!(".{}", name);
        }
        let _class = match name.as_str() {
            "Real" => {
                // no need to expand core type
                return;
            }
            _ => self
                .classes
                .get(&name)
                .unwrap_or_else(|| panic!("no key: {}, keys: {:?}", name, self.classes.keys())),
        };
        println!("expanding: {}", name);
    }
    fn exit_composition_part_mut(&mut self, _node: &mut ast::CompositionPart) {}
}
