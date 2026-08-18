//! MSL/OMC reference, parity, and profiling tooling for the rumoca test
//! infrastructure. The parity gate lives here with the harness and its result
//! schema instead of behind a repository task runner.

pub mod editor_gate;
pub mod msl_flamegraph;
pub mod msl_hotspots;
pub mod msl_tools;
pub mod parity_gate;
pub mod proc;
pub mod resource_budget;
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
                        .is_ok_and(|contents| contents.contains("[workspace]"))
            })
            .map(std::path::Path::to_path_buf)
    }
    let root = workspace_root_from_cwd().unwrap_or_else(|| {
        rumoca_compile::compile::core::workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR"))
    });
    root.canonicalize().unwrap_or(root)
}
