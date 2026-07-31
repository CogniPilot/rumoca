//! Build-resource ceilings for the `rumoca` test binaries.
//!
//! # Acceptance contract
//!
//! Every `[[test]]` target in `crates/rumoca` statically links the whole
//! compiler, so linking — not codegen — dominates the memory and disk cost of
//! `cargo test -p rumoca`, and parallel links have already been able to trip the
//! OOM killer on this repo. `autotests = false` plus the `suite_*.rs` umbrellas
//! exist to keep that cost bounded; these two gates keep it bounded.
//!
//! What is accepted:
//!
//! * **Exactly** the [`EXPECTED_RUMOCA_TEST_TARGETS`] set of `[[test]]` targets
//!   — the eight that exist today. Any number of *test files* may be added:
//!   they cost nothing extra as long as a `suite_*.rs` umbrella `#[path]`s them
//!   in. Only adding a whole new linked binary is rejected, and that rejection
//!   is editable: append the name here with a sentence saying why the new
//!   target could not live in an existing umbrella.
//! * **Any** dev-profile test binary up to [`TEST_BINARY_SIZE_CEILING_MIB`] MiB.
//!   The largest today is `suite_core` at ~140 MiB, so the ceiling is a little
//!   over 2x current head-room: normal growth, new dependencies, and new test
//!   files are all accepted, and only a step change in what a test binary links
//!   is rejected.
//! * **Absence of measurement.** A checkout that has not built its test
//!   binaries, or a non-`debug` profile directory, skips rather than fails: a
//!   size gate that fails on a clean tree would be a gate on having run cargo,
//!   not on the artifacts.
//!
//! Both rejections are loud (they name the offending target and the measured
//! number) and neither can hang: both are directory reads.

use super::*;
use std::path::PathBuf;

/// Every `[[test]]` target declared by `crates/rumoca/Cargo.toml`.
///
/// Seven `suite_*` umbrellas plus the architecture gate, which is its own
/// target because people rerun it by name and its failure has to be readable on
/// its own. Adding an entry here is a deliberate, reviewable act: it means one
/// more whole-compiler link in every `cargo test -p rumoca`.
const EXPECTED_RUMOCA_TEST_TARGETS: &[&str] = &[
    "architecture_hardening_test",
    "suite_core",
    "suite_examples_smoke",
    "suite_galec_fmu",
    "suite_gates",
    "suite_heavy_solve",
    "suite_msl_sim",
    "suite_template_runtime",
];

/// Per-binary dev-profile size ceiling, in MiB.
///
/// Measured head-room, not an aspiration: the largest dev-profile test binary
/// in this workspace is ~140 MiB (`suite_core`, which links the compiler plus
/// every default-feature test file). The ceiling is set at a bit over 2x that,
/// so it fires on a step change — a new umbrella that pulls a second copy of a
/// heavy dependency graph, or a test target that accidentally links the CLI —
/// and not on ordinary growth.
const TEST_BINARY_SIZE_CEILING_MIB: u64 = 300;

const BYTES_PER_MIB: u64 = 1024 * 1024;

/// The `name = "..."` of every `[[test]]` block in a Cargo manifest.
fn test_target_names(manifest: &str) -> Vec<String> {
    let mut names = Vec::new();
    let mut in_test_section = false;
    for line in manifest.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            in_test_section = trimmed == "[[test]]";
            continue;
        }
        if !in_test_section {
            continue;
        }
        let Some(value) = trimmed.strip_prefix("name") else {
            continue;
        };
        let Some(value) = value.trim_start().strip_prefix('=') else {
            continue;
        };
        let value = value.trim().trim_matches('"');
        if !value.is_empty() {
            names.push(value.to_string());
        }
    }
    names.sort();
    names
}

#[test]
fn test_rumoca_test_target_count_is_pinned() {
    let manifest = fs::read_to_string(workspace_root().join("crates/rumoca/Cargo.toml"))
        .expect("read rumoca Cargo.toml");
    let declared = test_target_names(&manifest);

    assert!(
        manifest.contains("autotests = false"),
        "crates/rumoca must keep `autotests = false`; auto-discovering `tests/*.rs` \
         would create one whole-compiler link per test file"
    );
    assert_eq!(
        declared, EXPECTED_RUMOCA_TEST_TARGETS,
        "the set of `[[test]]` targets in crates/rumoca changed. Each one is a separate \
         whole-compiler link in every `cargo test -p rumoca`, which is why the count is \
         pinned. A new test FILE needs no change here — `#[path]` it into the matching \
         `suite_*.rs` umbrella (see `tests/suite_core.rs` for the rules). A new TARGET is \
         allowed, but update EXPECTED_RUMOCA_TEST_TARGETS in the same commit with a \
         sentence saying why it could not live in an existing umbrella."
    );
    assert_eq!(
        declared.len(),
        8,
        "expected 7 `suite_*` umbrellas plus `architecture_hardening_test`"
    );
}

/// The dev-profile `deps` directory this test binary is running out of, or
/// `None` when the profile is not `debug` (the ceiling is a dev-profile budget;
/// release binaries have a different, smaller shape).
fn dev_profile_deps_dir() -> Option<PathBuf> {
    let current_exe = std::env::current_exe().ok()?;
    let deps_dir = current_exe.parent()?;
    if deps_dir.file_name()? != "deps" {
        return None;
    }
    (deps_dir.parent()?.file_name()? == "debug").then(|| deps_dir.to_path_buf())
}

/// Every workspace test-target binary stem cargo could produce.
///
/// Explicit `[[test]] name` entries, plus — for crates that have not turned
/// auto-discovery off — the stem of every `tests/*.rs` file. Restricted to test
/// targets on purpose: `[[bin]]` and lib unit-test binaries have their own size
/// story and are not what the `suite_*` umbrella policy is about.
fn workspace_test_target_stems() -> BTreeSet<String> {
    let mut stems = BTreeSet::new();
    let crates_dir = workspace_root().join("crates");
    let entries = fs::read_dir(&crates_dir).expect("read crates dir");
    for entry in entries.flatten() {
        let crate_dir = entry.path();
        let manifest_path = crate_dir.join("Cargo.toml");
        let Ok(manifest) = fs::read_to_string(&manifest_path) else {
            continue;
        };
        for name in test_target_names(&manifest) {
            stems.insert(name.replace('-', "_"));
        }
        if manifest
            .lines()
            .any(|line| line.trim() == "autotests = false")
        {
            continue;
        }
        let Ok(test_files) = fs::read_dir(crate_dir.join("tests")) else {
            continue;
        };
        for test_file in test_files.flatten() {
            let path = test_file.path();
            if path.extension().is_some_and(|ext| ext == "rs")
                && let Some(stem) = path.file_stem().and_then(|stem| stem.to_str())
            {
                stems.insert(stem.replace('-', "_"));
            }
        }
    }
    stems
}

/// Split `suite_core-72a6ba267c6847bb` into its target stem.
///
/// Cargo appends a 16-hex-digit metadata hash; anything else in `deps` (rlibs,
/// `.d` files, build scripts) is not a test binary.
fn test_binary_stem(file_name: &str) -> Option<&str> {
    let (stem, hash) = file_name.rsplit_once('-')?;
    (hash.len() == 16 && hash.bytes().all(|byte| byte.is_ascii_hexdigit())).then_some(stem)
}

#[test]
fn test_dev_profile_test_binaries_stay_under_the_link_size_ceiling() {
    let Some(deps_dir) = dev_profile_deps_dir() else {
        eprintln!(
            "skipping dev-profile test-binary size ceiling: not running from a debug \
             `target/debug/deps` directory"
        );
        return;
    };
    let stems = workspace_test_target_stems();
    let ceiling_bytes = TEST_BINARY_SIZE_CEILING_MIB * BYTES_PER_MIB;

    let mut measured = 0usize;
    let mut largest: Option<(u64, String)> = None;
    let mut offenders = Vec::new();
    let entries = fs::read_dir(&deps_dir).expect("read dev-profile deps dir");
    for entry in entries.flatten() {
        let path = entry.path();
        // Test binaries have no extension; `.d`/`.rlib`/`.so` siblings do.
        if path.extension().is_some() {
            continue;
        }
        let file_name = entry.file_name();
        let Some(file_name) = file_name.to_str() else {
            continue;
        };
        let Some(stem) = test_binary_stem(file_name) else {
            continue;
        };
        if !stems.contains(stem) {
            continue;
        }
        let Ok(metadata) = entry.metadata() else {
            continue;
        };
        if !metadata.is_file() {
            continue;
        }
        let bytes = metadata.len();
        measured += 1;
        if largest.as_ref().is_none_or(|(largest, _)| bytes > *largest) {
            largest = Some((bytes, file_name.to_string()));
        }
        if bytes > ceiling_bytes {
            offenders.push(format!(
                "{file_name}: {} MiB",
                bytes.div_ceil(BYTES_PER_MIB)
            ));
        }
    }

    if measured == 0 {
        eprintln!(
            "skipping dev-profile test-binary size ceiling: no test binaries built in {}",
            deps_dir.display()
        );
        return;
    }

    if let Some((bytes, name)) = largest {
        eprintln!(
            "largest dev-profile test binary: {name} at {} MiB (ceiling {TEST_BINARY_SIZE_CEILING_MIB} MiB, {measured} measured)",
            bytes / BYTES_PER_MIB
        );
    }

    assert!(
        offenders.is_empty(),
        "dev-profile test binaries exceed the {TEST_BINARY_SIZE_CEILING_MIB} MiB per-binary \
         ceiling: {offenders:#?}. Every one of these is linked (often in parallel) by \
         `cargo test`, so a step change here is a step change in peak link memory. Move the \
         heavy members into an existing `suite_*` umbrella, or raise the ceiling here with a \
         measurement showing why the new size is necessary."
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_target_names_reads_only_test_blocks() {
        let manifest = "\
[package]
name = \"demo\"

[[bin]]
name = \"not-a-test\"

[[test]]
name = \"suite_b\"
path = \"tests/suite_b.rs\"

[[test]]
name = \"suite_a\"
path = \"tests/suite_a.rs\"
required-features = [\"x\"]

[dependencies]
name = \"still-not-a-test\"
";
        assert_eq!(test_target_names(manifest), vec!["suite_a", "suite_b"]);
    }

    #[test]
    fn only_hash_suffixed_names_count_as_test_binaries() {
        assert_eq!(
            test_binary_stem("suite_core-72a6ba267c6847bb"),
            Some("suite_core")
        );
        assert_eq!(test_binary_stem("librumoca"), None);
        assert_eq!(test_binary_stem("suite_core-not16hexdigits"), None);
        assert_eq!(test_binary_stem("suite-core-72a6ba267c6847bZ"), None);
    }
}
