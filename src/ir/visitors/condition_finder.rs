//! Finds conditions, and replaces them with variables
use indexmap::IndexMap;

use crate::ir;
use crate::ir::ast::{Expression, Token};
use crate::ir::visitor::Visitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ConditionFinder {
    pub conditions: IndexMap<String, Expression>,
}

impl Visitor for ConditionFinder {
    fn exit_equation(&mut self, node: &mut ir::ast::Equation) {
        match node {
            ir::ast::Equation::When(blocks) => {
                for block in blocks {
                    let i = self.conditions.len();
                    let name = format!("__c{}", i);
                    self.conditions.insert(name.clone(), block.cond.clone());
                    block.cond = Expression::ComponentReference(ir::ast::ComponentReference {
                        local: false,
                        parts: vec![ir::ast::ComponentRefPart {
                            ident: Token {
                                text: name.clone(),
                                ..Default::default()
                            },
                            subs: None,
                        }],
                    });
                }
            }
            _ => {}
        }
    }
}
