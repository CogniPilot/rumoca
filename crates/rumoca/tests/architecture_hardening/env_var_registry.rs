//! Zero-`RUMOCA_*`-environment-variable enforcement, checked by
//! [`test_rumoca_env_vars_are_registered`]. Policy: SPEC_0018 (Tool
//! Configuration), "Configuration Channels (No `RUMOCA_*` Environment
//! Variables)".
//!
//! The policy is **literal zero**: no `"RUMOCA_*"` string literal may appear in
//! any `crates/**/*.rs` file (product code AND test/dev-harness code). Every
//! former knob has been migrated to one of the discoverable, self-documenting
//! channels instead:
//!
//! * a **CLI flag** (the `--help` UI) for anything a human or agent might tune;
//! * a **baked-in constant** for things that never needed to vary;
//! * a **`--trace` target** (the `tracing` feature) for debug output;
//! * a **fixed-path config/marker file** when a child process (a libtest
//!   harness, a node script behind nested npm, a VS Code extension host) cannot
//!   take argv — e.g. `cargo xtask verify msl-parity` writes
//!   `target/msl/parity-config.json` for the MSL harness.
//!
//! Why enforce zero rather than maintain an allowlist: AI agents (and humans)
//! sprinkle ad-hoc `std::env::var("RUMOCA_FOO_DEBUG")` knobs while debugging and
//! forget to remove them. An allowlist normalizes "just register one more"; a
//! hard zero makes every new `RUMOCA_*` literal a build failure, forcing the
//! author to pick a real, documented channel up front. If a genuinely
//! unavoidable host/CI need ever arises, that is a deliberate policy change to
//! this file and its rationale — not a quiet addition elsewhere.

use super::*;
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Registered `RUMOCA_*` environment variables. Intentionally tiny: the policy
/// is literal zero by default (see module docs). Adding an entry here is a
/// deliberate, reviewable policy exception — not the default escape hatch.
///
/// `RUMOCA_OMC_DOCKER_IMAGE` is a host/CI compatibility override for the
/// Docker-backed `omc` wrapper used by MSL parity verification. The parity
/// harness cannot pass argv into the shell wrapper, and the committed MSL
/// baseline is tied to a specific OpenModelica image version.
const REGISTERED_ENV_VARS: &[&str] = &["RUMOCA_OMC_DOCKER_IMAGE"];

/// Extract every `RUMOCA_<NAME>` token on one source line that is used as an
/// environment variable, across both Rust and JS/TS source. A token qualifies
/// when it is either:
///   * a fully-quoted string literal — `"RUMOCA_X"` / `'RUMOCA_X'` — closed by
///     the matching quote immediately after the name (covers Rust
///     `std::env::var("…")` / `.env("…", …)` and JS `process.env["…"]`); or
///   * a property access `…env.RUMOCA_X` (covers JS `process.env.RUMOCA_X`).
///
/// Bare uppercase identifiers (Rust `const RUMOCA_FOO`, C macros in embedded
/// strings) and mid-string mentions (`"… RUMOCA_FOO_* …"`) do not qualify, so
/// they are never false-positives.
fn extract_rumoca_env_literals(line: &str) -> Vec<String> {
    const MARKER: &str = "RUMOCA_";
    let bytes = line.as_bytes();
    let mut out = Vec::new();
    let mut search_from = 0;
    while let Some(rel) = line[search_from..].find(MARKER) {
        let name_start = search_from + rel;
        let name_end = name_start
            + line[name_start..]
                .find(|c: char| !is_env_name_char(c))
                .unwrap_or(line.len() - name_start);
        let before = bytes.get(name_start.wrapping_sub(1)).copied();
        let after = bytes.get(name_end).copied();
        let quoted = (before == Some(b'"') && after == Some(b'"'))
            || (before == Some(b'\'') && after == Some(b'\''));
        let env_access = line[..name_start].ends_with("env.");
        if quoted || env_access {
            out.push(line[name_start..name_end].to_string());
        }
        search_from = name_end.max(name_start + 1);
    }
    out
}

/// Characters allowed in a `RUMOCA_*` env-var name (after the `RUMOCA_` prefix).
fn is_env_name_char(c: char) -> bool {
    c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_'
}

/// Source file extensions scanned for `RUMOCA_*` environment-variable usage.
const SCANNED_EXTENSIONS: &[&str] = &["rs", "ts", "mjs", "cjs", "js"];

/// Directory names that are build output, dependencies, or vendored third-party
/// code — never first-party source, so they are not scanned.
const SKIPPED_DIRS: &[&str] = &[
    "target",
    "node_modules",
    ".git",
    ".vscode-test",
    "dist",
    "out",
    "build",
    "pkg",
    "vendor",
    "coverage",
];

/// Recursively collect first-party source files (any [`SCANNED_EXTENSIONS`])
/// under `dir`, skipping [`SKIPPED_DIRS`].
fn collect_source_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let skip = path
                .file_name()
                .and_then(|n| n.to_str())
                .is_some_and(|name| SKIPPED_DIRS.contains(&name));
            if !skip {
                collect_source_files(&path, out);
            }
            continue;
        }
        if path
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| SCANNED_EXTENSIONS.contains(&ext))
        {
            out.push(path);
        }
    }
}

/// Yield `(name, line_index)` for every `RUMOCA_*` literal in `content` that is
/// not registered, skipping line/block comments.
fn unregistered_env_literals(content: &str) -> Vec<(String, usize)> {
    content
        .lines()
        .enumerate()
        .filter(|(_, line)| {
            let trimmed = line.trim_start();
            !(trimmed.starts_with("//") || trimmed.starts_with('*'))
        })
        .flat_map(|(line_idx, line)| {
            extract_rumoca_env_literals(line)
                .into_iter()
                .filter(|name| !REGISTERED_ENV_VARS.contains(&name.as_str()))
                .map(move |name| (name, line_idx))
        })
        .collect()
}

#[test]
fn test_rumoca_env_vars_are_registered() {
    let root = workspace_root();
    let mut source_files = Vec::new();
    collect_source_files(&root, &mut source_files);

    let mut offenders: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for path in source_files {
        // The architecture-hardening tree necessarily names env vars as test
        // fixtures / banlists; skip it so it does not count as usage.
        if path.to_string_lossy().contains("architecture_hardening") {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for (name, line_idx) in unregistered_env_literals(&content) {
            offenders
                .entry(name)
                .or_default()
                .push(format!("{}:{}", path.display(), line_idx + 1));
        }
    }

    assert!(
        offenders.is_empty(),
        "found RUMOCA_* environment variable(s) in source (Rust and editor/JS \
code are both scanned). The policy is \
literal zero — do not add a new environment variable. Use a documented CLI flag \
(the discoverable `--help` UI), a baked-in constant, or a `--trace` target (for \
debug output) instead; when a child process cannot take argv, write a fixed-path \
config/marker file (see how `cargo xtask verify msl-parity` produces \
target/msl/parity-config.json). A genuinely unavoidable host/CI exception is a \
deliberate policy change to REGISTERED_ENV_VARS in \
crates/rumoca/tests/architecture_hardening/env_var_registry.rs, with rationale.\n\
Offending RUMOCA_* literals:\n{offenders:#?}"
    );
}
