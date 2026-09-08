//! Exact live inventory for the phase-codegen presentation boundary.
//!
//! The partitions are dispositions, not exemptions:
//! - registry rows carry their own diagnostic-only, target-neutral-data,
//!   exact-lexical-codec, or non-neutral-debt disposition in the identity;
//! - `rust_codegen` inventories orchestration transport plus non-neutral Rust
//!   debt; generic renderer/context transport and DAE-view JSON projection are
//!   target-neutral, pure diagnostic construction is diagnostic-only, but
//!   target-named schema assembly, semantic transformation, mutation, owned-IR,
//!   and target-language generated-text rows remain debt;
//! - `rust_views` inventories the permitted typed/read-only projection surface;
//!   its semantic aliases/wrappers/consumers are target-neutral only while the
//!   fingerprinted body stays read-only, and every mutation, transform,
//!   evaluator, owned-IR, or generated-text row remains non-neutral debt;
//! - template findings are conservative heuristics for logic, expansion,
//!   fallback, and namespace mutation and therefore remain reviewed debt.
//!
//! Deleting debt requires deleting its entry in the same reviewed change.
//! None of these rows weakens the absolute legacy-renderer, registry-identity,
//! unused-registration, or strict-environment checks.

mod registry;
mod rust_codegen;
mod rust_views;
mod templates;

pub(super) const ALLOWED_SOURCE_DEBT: &[&str] = &[
    registry::ALLOWED_SOURCE_DEBT,
    rust_codegen::ALLOWED_SOURCE_DEBT,
    rust_views::ALLOWED_SOURCE_DEBT,
    templates::ALLOWED_SOURCE_DEBT,
];
