use std::collections::HashSet;

use crate::ir;
use crate::ir::visitor::Visitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct StateFinder {
    pub states: HashSet<String>,
}

impl Visitor for StateFinder {
    fn exit_expression(&mut self, node: &mut ir::ast::Expression) {
        match &node {
            ir::ast::Expression::FunctionCall { comp, args } => {
                if comp.parts[0].ident.text == "der" {
                    let arg = args.get(0).unwrap();
                    match &arg {
                        ir::ast::Expression::ComponentReference(comp) => {
                            self.states.insert(comp.parts[0].ident.text.clone());
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}
