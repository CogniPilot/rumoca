//! Equation validation checks.
//!
//! MLS §8: Equations

mod basic;
mod reinit;
mod when;

pub use basic::{
    check_equation, check_equation_loop_variable_assignments, check_equation_with_classes,
    check_equations, check_equations_with_classes, check_statement, check_statement_with_classes,
    check_statements, check_statements_with_classes,
};
pub use reinit::{
    check_duplicate_reinit, check_edge_argument_type, check_reinit_context,
    check_reinit_variable_type, check_when_condition_type,
};
pub use when::check_when_restrictions;
