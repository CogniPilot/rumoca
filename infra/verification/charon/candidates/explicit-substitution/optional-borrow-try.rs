//@ known-failure
//@ no-default-options
//@ charon-args=--preset=aeneas --monomorphize-mut
//@ charon-args=--start-from=test_crate::increment_borrowed
//@ charon-args=--include=core::option::*::branch --include=core::option::*::from_residual
//! Mutable-borrow specialization of `?` on `Option<&mut u32>`. Explicit-only
//! substitution must preserve the implicit `Self` witness inside the
//! associated-type constraints of the specialized `Try` impl instead of
//! panicking. Translation then stops at the next trait-clause agreement
//! refusal, which the expected output records.
pub fn increment_borrowed(value: Option<&mut u32>) -> Option<&mut u32> {
    let value = value?;
    *value += 1;
    Some(value)
}
