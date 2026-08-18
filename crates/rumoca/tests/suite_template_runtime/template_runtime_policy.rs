//! Shared policy for template-runtime checks that need external programs.
//!
//! Ordinary local verification runs every self-contained check and may skip a
//! check whose external toolchain is absent. The dedicated CI lanes pass
//! `--require-external-tools`, which creates the marker below and turns the
//! same absence into a hard failure.

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
