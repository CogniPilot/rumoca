use crate::ir;
use crate::ir::visitor::Visitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct SubCompNamer {
    pub comp: String,
}

impl Visitor for SubCompNamer {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        if node.parts[0].ident.text == self.comp {
            node.parts.remove(0);
            node.parts[0].ident.text = format!("{}_{}", self.comp, node.parts[0].ident.text);
        }
    }
}
