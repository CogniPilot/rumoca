//! Load-bearing callable-plan boundary gates from SPEC_0043 row 328.
//!
//! These are syntax and manifest coverage plus compiler-level trait
//! assertions. They are not a proof of Rust name resolution or of callable
//! semantics. Together they close the reviewed routes by which a callable
//! crate could reacquire DAE construction, replay, or mutation authority, and
//! the routes by which crate-local code could mint a `CallablePlan` outside
//! its construction authority.
//!
//! - [`authority`] scans production Rust for DAE authority escape.
//! - [`sole_mint`] scans the plan crate for a second plan/storage mint and for
//!   invariant fields that leave the mint module.
//! - [`manifest`] pins the Cargo surface: no build script, no proc-macro, and
//!   an exact production/dev dependency allowlist.
//! - [`negative_traits`] asks the compiler, not a parser, whether the plan root
//!   can be cloned, defaulted, serialized, or deserialized.

mod authority;
mod manifest;
mod negative_traits;
mod sole_mint;

/// Every crate whose production sources carry the callable boundary.
pub(super) const CALLABLE_CRATES: [&str; 2] = ["rumoca-plan-callable", "rumoca-phase-callable"];

/// The crate that owns the plan root, its storage, and their sole mint.
pub(super) const PLAN_CRATE: &str = "rumoca-plan-callable";
