//! Finds conditions, and replaces them with variables
use std::collections::HashSet;

use crate::ir;
use crate::ir::visitor::Visitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct ConditionFinder {
    pub states: HashSet<String>,
}

impl Visitor for ConditionFinder {
    fn exit_equation(&mut self, node: &mut ir::ast::Equation) {
        match &node {
            ir::ast::Equation::When (..) => {
                println!("Found a when clause");
            }
            _ => {}
        }
    }
}
