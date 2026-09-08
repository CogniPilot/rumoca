//! Umbrella binary for GALEC-family integration suites. The compiler checks
//! run without optional features; the eFMU container members are enabled only
//! by `fmu-packaging`.
//!
//! GALEC never emits C: the C-track suites that drove the retired
//! `embedded-c-galec`/`galec-production` renderings were removed with those
//! products, and the C differential-equivalence obligation moves to the
//! Solve-rendered embedded target's differential gate.

mod algorithm_code_render_parity;
#[cfg(feature = "fmu-packaging")]
mod cli_support;

#[cfg(feature = "fmu-packaging")]
mod cli_target_efmu;
#[cfg(feature = "fmu-packaging")]
mod cli_target_galec;
#[cfg(feature = "fmu-packaging")]
mod container_xml_support;
#[cfg(feature = "fmu-packaging")]
mod metadata_support;
