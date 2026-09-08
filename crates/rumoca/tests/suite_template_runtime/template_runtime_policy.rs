//! Shared policy for template-runtime checks that need external programs.
//!
//! A direct, focused Cargo test may skip a check whose external toolchain is
//! absent. `cargo xtask verify template-runtimes` always creates the marker
//! below, so an official verification gate can never report success after an
//! external check was skipped.

use std::path::Path;

pub(crate) fn external_tools_are_required() -> bool {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/template-runtimes/strict")
        .is_file()
}

pub(crate) fn prerequisites_are_available(check: &str, prerequisites: &[(&str, bool)]) -> bool {
    let missing: Vec<&str> = prerequisites
        .iter()
        .filter_map(|(name, available)| (!available).then_some(*name))
        .collect();
    if missing.is_empty() {
        return true;
    }

    let missing = missing.join(", ");
    assert!(
        !external_tools_are_required(),
        "{check} requires unavailable external prerequisites: {missing}",
    );
    eprintln!("skipping optional local {check}; unavailable prerequisites: {missing}");
    false
}
