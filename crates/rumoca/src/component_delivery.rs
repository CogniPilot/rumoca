//! Client orchestration over the retained prepared build request.

use std::path::Path;

use anyhow::{Context, Result, bail};

use crate::packaging::PreparedTargetPackage;

pub(crate) fn deliver(prepared: PreparedTargetPackage, output: &Path) -> Result<()> {
    let slots = prepared.build_slots();
    if slots.len() != 1 {
        bail!("component delivery requires exactly one declared compiled slot");
    }
    let scratch = tempfile::Builder::new()
        .prefix("rumoca-component-delivery-")
        .tempdir()
        .context("create owned component delivery scratch")?;
    let target = std::env::var_os("CARGO_TARGET_DIR")
        .map(|root| std::path::PathBuf::from(root).join("rumoca-component-build"))
        .unwrap_or_else(|| scratch.path().join("target"));
    let component = rumoca_exec_wasm::build_wasm_component(
        &prepared.build_request(&slots[0])?,
        scratch.path(),
        &target,
    )
    .map_err(anyhow::Error::msg)?;
    crate::target_manifest::publish_wasm_component(prepared, component, output)
}
