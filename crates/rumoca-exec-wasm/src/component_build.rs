//! Sealed compiled-component results; tool construction is feature-scoped.

use rumoca_core::artifact_build::ArtifactBuildBinding;
#[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
use rumoca_core::artifact_build::PreparedBuildRequest;
#[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
mod cache_lease;
#[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
mod cargo_artifact;
#[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
mod interface;

/// Validated binary bytes bound to the exact immutable source inventory/slot.
/// No deserialize or raw-byte constructor acquires this adapter result.
pub struct CompiledWasmComponent<S> {
    binding: ArtifactBuildBinding<S>,
    bytes: Vec<u8>,
}

impl<S> CompiledWasmComponent<S> {
    pub fn binding(&self) -> &ArtifactBuildBinding<S> {
        &self.binding
    }
    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

#[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
mod tools {
    use super::*;
    use std::path::Path;
    use std::process::Command;

    /// Materialize the exact retained inventory in an owned scratch staging
    /// project, build it and validate the resulting component. Mutable paths
    /// supplied by a caller never substitute source bytes for this request.
    pub fn build_wasm_component<S>(
        request: &PreparedBuildRequest<'_, S>,
        staging_parent: &Path,
        target_directory: &Path,
    ) -> Result<CompiledWasmComponent<S>, String> {
        let parameters = request.parameters();
        if parameters.get("kind").map(String::as_str) != Some("cargo-wasm-component") {
            return Err("slot has no Cargo WebAssembly component build".into());
        }
        let manifest = Path::new(parameters.get("manifest").ok_or("missing build manifest")?);
        rumoca_core::artifact_build::validate_artifact_relative_path(manifest)?;
        if !request.files().contains_key(manifest) {
            return Err("missing retained build manifest".into());
        }
        let library = parameters
            .get("library")
            .ok_or("missing component library identifier")?;
        if library.is_empty()
            || !library
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
        {
            return Err("component library name must be one identifier".into());
        }
        let expected_interface = interface::ExpectedInterface::from_request(request)?;
        let staging = tempfile::Builder::new()
            .prefix("component-build-")
            .tempdir_in(staging_parent)
            .map_err(|error| format!("create owned component staging: {error}"))?;
        materialize(request, staging.path())?;
        let _cache_lease = cache_lease::acquire(target_directory)?;
        let artifacts = run(
            Command::new("cargo")
                .args([
                    "build",
                    "--message-format=json-render-diagnostics",
                    "--release",
                    "--target",
                    "wasm32-wasip2",
                    "--manifest-path",
                ])
                .arg(staging.path().join(manifest))
                .env("CARGO_TARGET_DIR", target_directory)
                .env("RUSTFLAGS", "-Dwarnings"),
            "build component",
        )?;
        let component = target_directory
            .join("wasm32-wasip2/release")
            .join(format!("{library}.wasm"));
        cargo_artifact::require_exact_artifact(
            &artifacts,
            &staging.path().join(manifest),
            library,
            &component,
        )?;
        run(
            Command::new("wasm-tools").arg("validate").arg(&component),
            "validate component",
        )?;
        let bytes =
            std::fs::read(&component).map_err(|error| format!("read component: {error}"))?;
        expected_interface.check(&bytes)?;
        Ok(CompiledWasmComponent {
            binding: request.binding(),
            bytes,
        })
    }

    fn materialize<S>(request: &PreparedBuildRequest<'_, S>, root: &Path) -> Result<(), String> {
        for (relative, bytes) in request.files() {
            let path = root.join(relative);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|error| format!("create build directory: {error}"))?;
            }
            std::fs::write(path, bytes)
                .map_err(|error| format!("write retained build input: {error}"))?;
        }
        Ok(())
    }

    fn run(command: &mut Command, context: &str) -> Result<String, String> {
        let output = command
            .output()
            .map_err(|error| format!("{context}: {error}"))?;
        if !output.status.success() {
            return Err(format!(
                "{context} failed: {}\n{}\n{}",
                output.status,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            ));
        }
        String::from_utf8(output.stdout).map_err(|error| format!("{context} output: {error}"))
    }
}

#[cfg(all(not(target_arch = "wasm32"), feature = "component-build"))]
pub use tools::build_wasm_component;

#[cfg(all(test, not(target_arch = "wasm32"), feature = "component-build"))]
mod tests {
    use super::*;
    use rumoca_core::artifact_build::PreparedBuildInventory;
    use std::collections::BTreeMap;

    #[test]
    fn component_build_refuses_missing_retained_manifest_before_materialization() {
        let inventory = PreparedBuildInventory::construct(
            vec![("source.rs".into(), b"retained".to_vec())],
            (),
            BTreeMap::from([(
                "component".into(),
                BTreeMap::from([
                    ("kind".into(), "cargo-wasm-component".into()),
                    ("manifest".into(), "Cargo.toml".into()),
                    ("library".into(), "component".into()),
                ]),
            )]),
        )
        .unwrap();
        let work = tempfile::tempdir().unwrap();
        let result = build_wasm_component(
            &inventory.request("component").unwrap(),
            work.path(),
            &work.path().join("target"),
        );
        assert!(matches!(result, Err(message) if message == "missing retained build manifest"));
        assert_eq!(std::fs::read_dir(work.path()).unwrap().count(), 0);
    }

    #[test]
    fn component_build_refuses_wrong_retained_world_before_materialization() {
        let inventory = PreparedBuildInventory::construct(
            vec![
                ("Cargo.toml".into(), b"retained manifest".to_vec()),
                (
                    "wit/contract.wit".into(),
                    b"package example:product; world actual {}".to_vec(),
                ),
            ],
            (),
            BTreeMap::from([(
                "component".into(),
                BTreeMap::from([
                    ("kind".into(), "cargo-wasm-component".into()),
                    ("manifest".into(), "Cargo.toml".into()),
                    ("library".into(), "component".into()),
                    ("wit-directory".into(), "wit".into()),
                    ("world".into(), "absent".into()),
                ]),
            )]),
        )
        .unwrap();
        let work = tempfile::tempdir().unwrap();
        let result = build_wasm_component(
            &inventory.request("component").unwrap(),
            work.path(),
            &work.path().join("target"),
        );
        assert!(matches!(result, Err(message) if message.starts_with("select target WIT world:")));
        assert_eq!(std::fs::read_dir(work.path()).unwrap().count(), 0);
    }
}
