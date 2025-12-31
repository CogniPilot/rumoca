//! Class-level restriction validation.
//!
//! MLS §12 (Functions), §14 (Operator Records), §16 (Clocks), §17 (State Machines)

mod clock;
mod functions;
mod operator_record;
mod state_machine;

pub use clock::check_clock_restrictions;
pub use functions::{
    check_function_component_classes, check_function_forbidden_operators,
    check_function_no_inner_outer, check_function_public_components,
    check_function_single_algorithm,
};
pub use operator_record::{
    check_operator_record_cannot_be_extended, check_operator_record_no_extends,
    check_operator_record_no_partial,
};
pub use state_machine::{
    check_single_initial_state, check_state_is_block_or_model, check_transition_priority_unique,
};
