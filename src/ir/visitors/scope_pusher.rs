use crate::ir;
use crate::ir::visitor::Visitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ScopePusher {
    pub comp: String,
}

impl Visitor for ScopePusher {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        if node.parts[0].ident.text != "der" {
            node.parts.insert(
                0,
                ir::ast::ComponentRefPart {
                    ident: ir::ast::Token {
                        text: self.comp.clone(),
                        ..Default::default()
                    },
                    subs: None,
                },
            );
        }
    }
}
