//! Bounded verification harnesses for the Solve runtime and the FMI 3 ME
//! component kernel.
//!
//! Each property is written once, as a plain function whose doc comment states
//! it in one sentence and cites the registry row or FMI clause it comes from.
//! Two drivers then run that same function:
//!
//! - `mod proof`, compiled only under `cfg(kani)`, feeds it `kani::any()`
//!   values constrained by `kani::assume` and proves the property over the
//!   whole bounded input space;
//! - `mod fallback`, compiled only under `cfg(all(test, not(kani)))`, feeds it
//!   proptest strategies over the same bounds and samples that space.
//!
//! Kani 0.67.0 is pinned in the dedicated `nix develop .#kani` shell. The
//! canonical `cargo xtask verify kani` gate reads
//! `verification/kani-proofs.json` and proves every required harness listed
//! there. The fallback remains useful conventional test evidence, but a green
//! fallback run is never reported as a proof. Because both drivers call the
//! same function, a property cannot drift between what is proved and sampled.
//!
//! Every submodule below is `cfg(any(test, kani))`, so no production build
//! carries one and the SPEC_0038 FMI-boundary hardening test never sees them.
//! `kani` is declared to `check-cfg` in the workspace lints table (root
//! `Cargo.toml`), so the harnesses need no lint suppression here and a genuine
//! cfg typo anywhere in this crate still warns.

#[cfg(any(test, kani))]
mod condition_memory;
#[cfg(any(test, kani))]
mod kernel_lifecycle;
#[cfg(any(test, kani))]
mod me_lifecycle;
#[cfg(any(test, kani))]
mod model_fixture;
