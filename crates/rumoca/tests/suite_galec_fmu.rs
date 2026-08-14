//! Umbrella binary for the eFMI/eFMU container suites, which inspect the
//! packaged zip through the optional `zip` dependency and therefore require
//! the `fmu-packaging` feature. See `suite_core.rs` for the grouping rules.
//!
//! Both members define identically named tests (they pin the same container
//! invariants for the Algorithm Code and Production Code tracks); the module
//! prefix keeps the two sets distinct inside the shared binary.

// Shared helpers of the GALEC family, declared once for the whole binary so
// the members below share one copy (`clippy::duplicate_mod` rejects loading a
// file as a module twice, and a shared copy is what we want anyway). Every
// item is used by at least one member, keeping the zero-`allow` dead-code
// discipline intact.
#[path = "galec_cli_support/cc.rs"]
mod cc_support;
#[path = "galec_cli_support/cli.rs"]
mod cli_support;
#[path = "galec_cli_support/container_xml.rs"]
mod container_xml_support;
#[path = "galec_cli_support/metadata.rs"]
mod metadata_support;

#[path = "cli_target_galec.rs"]
mod cli_target_galec;
#[path = "cli_target_galec_production.rs"]
mod cli_target_galec_production;
