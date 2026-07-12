//! MSL/OMC reference, parity, and profiling tooling for the rumoca test
//! infrastructure. These modules link the compiler stack and were moved out of
//! the `xtask` crate so that `xtask` stays dependency-light (parses args and
//! shells out; see the `rumoca-msl-tools` / `rumoca-msl-profile` bins).

pub mod msl_flamegraph;
pub mod msl_tools;
pub mod proc;
pub mod web_assets;

use std::path::PathBuf;

/// Resolve the workspace root.
///
/// Relocatable: walk up from the CWD to the nearest `[workspace]` Cargo.toml so
/// a prebuilt (Nix/crane) binary — whose baked `CARGO_MANIFEST_DIR` is gone at
/// runtime — still resolves the root. Falls back to the compile-time manifest
/// dir for a normal `cargo run` when the CWD is outside a rumoca workspace.
pub fn repo_root() -> PathBuf {
    fn workspace_root_from_cwd() -> Option<PathBuf> {
        let cwd = std::env::current_dir().ok()?;
        cwd.ancestors()
            .find(|dir| {
                let manifest = dir.join("Cargo.toml");
                manifest.is_file()
                    && std::fs::read_to_string(&manifest)
                        .map(|s| s.contains("[workspace]"))
                        .unwrap_or(false)
            })
            .map(std::path::Path::to_path_buf)
    }
    let root = workspace_root_from_cwd().unwrap_or_else(|| {
        rumoca_compile::compile::core::workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR"))
    });
    root.canonicalize().unwrap_or(root)
}
