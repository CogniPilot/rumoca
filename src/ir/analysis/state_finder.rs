//! A visitor implementation for finding state variables in an abstract syntax
//! tree (AST). The `StateFinder` struct is designed to traverse the AST and
//! identify state variables that are referenced within derivative function calls
//! (`der`). It collects these state variable names into a `HashSet` for later
//! processing.
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
//!   - **Note:** The AST is NOT modified - `der()` calls remain as function calls
//!     to maintain Base Modelica compliance.
//!
//! This visitor is useful for identifying which variables are states (appear in
//! der() calls) without transforming the AST representation.

use indexmap::IndexSet;

use crate::ir;
use crate::ir::transform::constants::BUILTIN_DER;
use crate::ir::visitor::MutVisitor;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct StateFinder {
    pub states: IndexSet<String>,
}

impl MutVisitor for StateFinder {
    fn exit_expression(&mut self, node: &mut ir::ast::Expression) {
        if let ir::ast::Expression::FunctionCall { comp, args } = &node
            && comp.to_string() == BUILTIN_DER
        {
            // SAFETY: Modelica der() function always has exactly 1 argument
            let arg = args.first().unwrap();
            if let ir::ast::Expression::ComponentReference(comp) = &arg {
                // Collect the state variable name
                self.states.insert(comp.parts[0].ident.text.clone());
                // DO NOT transform the AST - keep der() as a function call
                // for Base Modelica compliance
            }
        }
    }
}
