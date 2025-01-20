use rumoca_parser::{ast, VisitorMut};

/// Renames all classes "test" as a tree mutation test
#[derive(Default, Debug, Clone)]
pub struct ClassNamer {}

impl VisitorMut for ClassNamer {
    fn enter_class_definition_mut(&mut self, node: &mut ast::ClassDefinition) {
        #[allow(clippy::single_match)]
        match &mut node.specifier {
            ast::ClassSpecifier::Long { name, .. } => {
                *name = "test".into();
            }
            _ => {}
        }
    }
}
