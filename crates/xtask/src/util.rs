//! Small self-contained path helpers, inlined so `xtask` needs no `rumoca-*`
//! dependency.
//!
//! These duplicate `rumoca_core::{workspace_root_from_manifest_dir,
//! split_path_with_indices}`. They are tiny and stable; sharing them via a
//! dependency on rumoca-core would pull a workspace crate into `xtask` and defeat
//! the light-xtask invariant (enforced by the CI arch check). Duplicating a
//! handful of lines is the sanctioned trade (SPEC_0029 §7).

use std::path::PathBuf;

/// Resolve the workspace root from a crate manifest directory.
///
/// For crates under `<workspace>/crates/*`, this returns `<workspace>`.
pub(crate) fn workspace_root_from_manifest_dir(manifest_dir: &str) -> PathBuf {
    PathBuf::from(manifest_dir).join("../..")
}

/// Split a dotted path while preserving dots inside bracket expressions.
///
/// For `bus[data.medium].pin.v`, returns `["bus[data.medium]", "pin", "v"]`.
pub(crate) fn split_path_with_indices(path: &str) -> Vec<&str> {
    let mut parts = Vec::with_capacity(4);
    let mut start = 0;
    let mut bracket_depth = 0usize;
    for (idx, byte) in path.bytes().enumerate() {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => {
                if start < idx {
                    parts.push(&path[start..idx]);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start < path.len() {
        parts.push(&path[start..]);
    }
    parts
}
