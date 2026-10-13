//! Client orchestration over the retained prepared build request.

use std::path::Path;

use anyhow::{Context, Result, bail};

use crate::packaging::PreparedTargetPackage;

/// The one compiled slot a delivered package declares.
fn single_slot(prepared: &PreparedTargetPackage) -> Result<String> {
    let mut slots = prepared.build_slots();
    match (slots.pop(), slots.is_empty()) {
        (Some(slot), true) => Ok(slot),
        _ => bail!("component delivery requires exactly one declared compiled slot"),
    }
}

/// Build the package's compiled component and publish it.
///
/// Coverage exemption: the build runs `cargo` for wasm32-wasip2 and
/// `wasm-tools`, which no workspace test can run (the Template Runtime wasm CI
/// job exercises it); the slot rule is
/// [`single_slot`] and the publication is `publish_wasm_component`, which the
/// tests cover.
#[cfg_attr(coverage_nightly, coverage(off))]
pub(crate) fn deliver(prepared: PreparedTargetPackage, output: &Path) -> Result<()> {
    let slot = single_slot(&prepared)?;
    let scratch = tempfile::Builder::new()
        .prefix("rumoca-component-delivery-")
        .tempdir()
        .context("create owned component delivery scratch")?;
    let target = std::env::var_os("CARGO_TARGET_DIR")
        .map(|root| std::path::PathBuf::from(root).join("rumoca-component-build"))
        .unwrap_or_else(|| scratch.path().join("target"));
    let component = rumoca_exec_wasm::build_wasm_component(
        &prepared.build_request(&slot)?,
        scratch.path(),
        &target,
    )
    .map_err(anyhow::Error::msg)?;
    crate::target_manifest::publish_wasm_component(prepared, component, output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_package_with_one_compiled_slot_delivers_through_that_slot() {
        let result = crate::Compiler::new()
            .model("Decay")
            .compile_str(
                "model Decay input Real gain = 2; output Real y; equation y = gain * time; end Decay;",
                "Decay.mo",
            )
            .unwrap();
        let prepared = crate::prepare_packaged_target(&result, "Decay", "fmi-ls-wasm").unwrap();
        assert_eq!(single_slot(&prepared).unwrap(), "component");
        assert!(prepared.build_request("component").is_ok());
    }
}
