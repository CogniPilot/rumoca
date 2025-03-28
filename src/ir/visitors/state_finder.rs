//! A visitor implementation for finding and transforming state variables in an
//! abstract syntax tree (AST). The `StateFinder` struct is designed to traverse
//! the AST and identify state variables that are referenced within derivative
//! function calls (`der`). It collects these state variable names into a
//! `HashSet` and modifies the AST to replace the original state variable
//! references with their derivative counterparts.
//!
//! # Fields
//! - `states`: A `HashSet` containing the names of the state variables found
//!   during the traversal.
//!
//! # Visitor Implementation
//! - The `exit_expression` method is invoked when exiting an expression node
//!   during the AST traversal. It performs the following actions:
//!   - Checks if the expression is a function call with the identifier `der`.
//!   - If the first argument of the `der` function is a component reference,
//!     the state variable name is extracted and added to the `states` set.
//!   - Modifies the AST by replacing the original state variable reference with
//!     a new component reference prefixed with `der_`.
//!
//! This visitor is useful for analyzing and transforming ASTs in scenarios
//! where state variables and their derivatives need to be explicitly tracked
//! and processed.

use indexmap::IndexSet;

use crate::ir;
use crate::ir::visitor::Visitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct StateFinder {
    pub states: IndexSet<String>,
}

impl Visitor for StateFinder {
    fn exit_expression(&mut self, node: &mut ir::ast::Expression) {
        match &node {
            ir::ast::Expression::FunctionCall { comp, args } => {
                if comp.to_string() == "der" {
                    let arg = args.get(0).unwrap();
                    match &arg {
                        ir::ast::Expression::ComponentReference(comp) => {
                            self.states.insert(comp.parts[0].ident.text.clone());
                            let mut der_comp = comp.clone();
                            der_comp.parts[0].ident.text =
                                format!("der_{}", comp.parts[0].ident.text);
                            *node = ir::ast::Expression::ComponentReference(der_comp);
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}
