//! Architecture-hardening span and size validation tests.
//!
//! SPEC_0021 file-size: this suite was split from a single >2000-line file into
//! a shared scanning-helper module plus test modules grouped by logical unit.
//! Paths are explicit because this parent is itself loaded via `#[path]`.

#[path = "size_and_validation/helpers.rs"]
mod helpers;

#[path = "size_and_validation/span_debt.rs"]
mod span_debt;

#[path = "size_and_validation/fixture_provenance_a.rs"]
mod fixture_provenance_a;

#[path = "size_and_validation/fixture_provenance_b.rs"]
mod fixture_provenance_b;

#[path = "size_and_validation/architecture_boundaries.rs"]
mod architecture_boundaries;

#[path = "size_and_validation/source_named_spans.rs"]
mod source_named_spans;
