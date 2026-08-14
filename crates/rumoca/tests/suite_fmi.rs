//! FMI 2/3 standards-conformance suite.
//!
//! This target is isolated from general template runtimes so the gate can run
//! with only compiler and FMU-packaging features.

#[path = "cli_target_fmi.rs"]
mod cli_target_fmi;
#[path = "fmi_ls_dae_contract.rs"]
mod fmi_ls_dae_contract;
#[path = "template_runtime_policy.rs"]
mod template_runtime_policy;
