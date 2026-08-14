//! Bounded verification harnesses for the Solve runtime and the FMI 3 ME
//! component kernel.
//!
//! Each property is written once, as a plain function whose doc comment states
//! it in one sentence and cites the registry row or FMI clause it comes from.
//! Its driver is selected by the shape of the input domain:
//!
//! - finite tables and named invalid-input classes are enumerated completely
//!   by ordinary tests;
//! - `mod proof`, compiled only under `cfg(kani)`, is reserved for production
//!   kernels whose symbolic numeric or typed-state domains make enumeration
//!   impractical;
//! - a property-test driver samples each retained symbolic domain during the
//!   normal developer workflow, without being reported as proof evidence.
//!
//! Kani 0.67.0 is pinned in the dedicated `nix develop .#kani` shell. The
//! canonical `cargo xtask verify kani` gate reads
//! `verification/kani-proofs.json` and proves every required harness listed
//! there. The fallback remains useful conventional test evidence, but a green
//! property-test run is never reported as a proof. Both drivers call the same
//! property function, so the proved and sampled statements cannot drift.
//!
//! Every submodule below is test- or Kani-only, so no production build carries
//! one and the SPEC_0038 FMI-boundary hardening test never sees them.
//! `kani` is declared to `check-cfg` in the workspace lints table (root
//! `Cargo.toml`), so the harnesses need no lint suppression here and a genuine
//! cfg typo anywhere in this crate still warns.

#[cfg(test)]
mod condition_memory;
#[cfg(any(test, kani))]
mod event_iteration;
#[cfg(test)]
mod kernel_lifecycle;
#[cfg(test)]
mod me_lifecycle;
#[cfg(test)]
mod model_fixture;
