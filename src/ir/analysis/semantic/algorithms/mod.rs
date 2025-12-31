//! Algorithm and statement validation checks.
//!
//! MLS §11: Statements and Algorithm Sections

mod assertions;
mod statements;

pub use assertions::check_assert_arguments;
pub use statements::{
    check_algorithm_assignments_with_types, check_algorithm_condition_types,
    check_loop_variable_assignments,
};
