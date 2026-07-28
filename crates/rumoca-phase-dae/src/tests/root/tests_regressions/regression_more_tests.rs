//! Topic-grouped DAE lowering regression fixtures.
//!
//! These cases accumulated as one oversized module and are now split into
//! sibling submodules by subject area; the modules below hold the tests
//! verbatim.

use super::*;

mod array_residual_size_tests;
mod connector_interface_tests;
mod function_lhs_size_tests;
mod lhs_var_size_tests;
mod record_range_size_tests;
mod subscript_form_tests;
