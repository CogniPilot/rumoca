//! Component-level validation checks.
//!
//! MLS §4: Declarations, §7: Modifications

mod attributes;
mod bindings;
mod variability;

pub use attributes::check_builtin_attribute_modifiers;
pub use bindings::check_component_bindings;
pub use variability::{check_constant_bindings, check_variability_dependencies};
