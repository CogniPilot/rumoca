//! Umbrella binary for GALEC-family integration suites. The compiler and
//! differential-execution checks run without optional features; the eFMU
//! container members are enabled only by `fmu-packaging`.
//!
//! Both container members define identically named tests (they pin the same container
//! invariants for the Algorithm Code and Production Code tracks); the module
//! prefix keeps the two sets distinct inside the shared binary.

mod cc_support;
mod cli_support;
mod cli_target_embedded_c_galec;
mod galec_call_boundary;
mod galec_enumeration_status;
mod galec_equivalence;
mod galec_store_order;

#[cfg(feature = "fmu-packaging")]
mod cli_target_galec;
#[cfg(feature = "fmu-packaging")]
mod cli_target_galec_production;
#[cfg(feature = "fmu-packaging")]
mod container_xml_support;
#[cfg(feature = "fmu-packaging")]
mod metadata_support;
