//! Finds conditions, and replaces them with variables
use indexmap::IndexMap;

use crate::ir;
use crate::ir::ast::{
    Component, ComponentRefPart, ComponentReference, Equation, EquationBlock, Expression, Name,
    Token,
};
use crate::ir::transform::constants::{TYPE_BOOL, condition_name};
use crate::ir::visitor::MutVisitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ConditionFinder {
    pub conditions: IndexMap<String, Component>,
    pub expressions: IndexMap<String, Expression>,
}

impl ConditionFinder {
    fn process_condition_block(&mut self, block: &mut EquationBlock) {
        let i = self.conditions.len();
        let name = condition_name(i);
        let comp = Component {
            name: name.clone(),
            type_name: Name {
                name: vec![Token {
                    text: TYPE_BOOL.to_string(),
                    ..Default::default()
                }],
            },
            start: Expression::Terminal {
                terminal_type: ir::ast::TerminalType::Bool,
                token: Token {
                    text: "false".to_string(),
                    ..Default::default()
                },
            },
            ..Default::default()
        };
        self.conditions.insert(name.clone(), comp.clone());
        self.expressions.insert(name.clone(), block.cond.clone());
        block.cond = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: comp.name.clone(),
                    ..Default::default()
                },
                subs: None,
            }],
        });
    }
}

impl MutVisitor for ConditionFinder {
    fn exit_equation(&mut self, node: &mut Equation) {
        match node {
            Equation::When(blocks) => {
                for block in blocks.iter_mut() {
                    self.process_condition_block(block);
                }
            }
            ir::ast::Equation::If {
                cond_blocks,
                else_block: _,
            } => {
                for block in cond_blocks.iter_mut() {
                    self.process_condition_block(block);
                }
            }
            _ => {}
        }
    }
}
