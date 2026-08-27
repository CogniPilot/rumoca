//! Architecture-hardening span and size validation tests.
//!
//! SPEC_0021 file-size: this suite was split from a single >2000-line file into
//! a shared scanning-helper module plus test modules grouped by logical unit.
//! Every child uses the normal `size_and_validation/` module hierarchy.

mod helpers;

mod span_debt;

mod clippy_exceptions;

mod architecture_boundaries;

mod source_named_spans;
