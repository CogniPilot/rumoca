//! #483 acceptance: the emitted C names the Solve program it was printed from.
//!
//! Both C targets record `SolveAlgorithmBlock::digest` of the refined block in
//! each model unit's banner, and `galec-production` also in its Production
//! Code manifest. These tests read that digest out of what the real CLI wrote
//! and recompute it independently: compile the same file through the library,
//! project the checked GALEC package, refine it under the target's own declared
//! profile, and digest the block.
//!
//! Routing is proved by content, not by name. A second build keeps the target,
//! the model name and the file path, and changes one literal without moving any
//! span; its artifacts must carry a different digest, and each build's digest
//! must equal the recomputation from its own source only. The original source
//! built from another directory must carry the original digest, because the
//! generated C does not depend on the checkout location.

use std::fs;
use std::path::Path;

use rumoca_compile::codegen::targets::TargetBundle;
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};
use tempfile::tempdir;

use super::cli_support::{run_compile_target, write_fixture};

const MODEL: &str = "SolveDigestRouting";

const SOURCE: &str = "\
model SolveDigestRouting
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end SolveDigestRouting;
";

/// [`SOURCE`] with one DoStep literal changed and every byte offset kept.
const PERTURBED: &str = "\
model SolveDigestRouting
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 3.0);
  end when;
end SolveDigestRouting;
";

/// The banner line both C model units print, followed by the hex digest.
const C_MARKER: &str = "/* Solve program digest (BLAKE3): ";

/// The one digest `path` records in its banner: exactly one marker line, and
/// 64 lower-case hex digits after it.
fn c_banner_digest(path: &Path) -> String {
    let text =
        fs::read_to_string(path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    let digests = text
        .lines()
        .filter_map(|line| line.strip_prefix(C_MARKER))
        .map(str::trim)
        .collect::<Vec<_>>();
    assert_eq!(
        digests.len(),
        1,
        "{} must print one Solve program digest line:\n{text}",
        path.display()
    );
    let digest = digests[0];
    assert!(
        digest.len() == 64
            && digest
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)),
        "{}: not a BLAKE3 hex digest: {digest:?}",
        path.display()
    );
    digest.to_owned()
}

/// Recompute the digest from `file` without the CLI: library compile, GALEC
/// projection, refinement under `target`'s declared profile.
fn recomputed_digest(file: &Path, target: &str) -> String {
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_path(file)
        .unwrap_or_else(|error| panic!("library compile of {}: {error}", file.display()));
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, MODEL),
        &GalecOptions::default(),
    )
    .unwrap_or_else(|errors| panic!("GALEC projection: {errors:?}"));
    let profile = TargetBundle::builtin(target)
        .unwrap_or_else(|| panic!("`{target}` must be a builtin target"))
        .parse_manifest()
        .unwrap_or_else(|error| panic!("parse `{target}` target.toml: {error}"))
        .algorithm_block_profile()
        .unwrap_or_else(|error| panic!("`{target}` profile: {error}"));
    rumoca_phase_solve::lower_algorithm_block(&package, profile)
        .unwrap_or_else(|error| panic!("Solve refinement: {error}"))
        .digest()
        .unwrap_or_else(|error| panic!("Solve block digest: {error}"))
        .to_string()
}

/// Build `source` at `file` with `target` into `out_dir` through the CLI.
fn compile(file: &Path, source: &str, target: &str, out_dir: &Path) {
    fs::write(file, source).expect("write fixture");
    let output = run_compile_target(file, target, out_dir);
    assert!(
        output.status.success(),
        "`compile --target {target}` failed (status {:?}).\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

/// Build [`SOURCE`] and then [`PERTURBED`] at one path with `target`, read
/// every recorded digest of each build with `recorded`, and require each build
/// to agree with the recomputation from its own source and to differ from the
/// other build.
fn assert_routed_by_content(target: &str, recorded: impl Fn(&Path) -> Vec<String>) {
    let dir = tempdir().expect("tempdir");
    let file = write_fixture(dir.path(), MODEL, SOURCE);
    let mut builds = Vec::new();
    for (label, source) in [("original", SOURCE), ("perturbed", PERTURBED)] {
        let out_dir = dir.path().join(label);
        compile(&file, source, target, &out_dir);
        let digests = recorded(&out_dir);
        assert!(!digests.is_empty(), "{label}: no recorded digest");
        let expected = recomputed_digest(&file, target);
        for digest in &digests {
            assert_eq!(
                digest, &expected,
                "{target} {label}: the artifact names a Solve program other than the \
                 refinement of its own checked package"
            );
        }
        builds.push(expected);
    }
    assert_ne!(
        builds[0], builds[1],
        "{target}: one target, model name and path with a changed literal must name a \
         different Solve program"
    );
    // The C is byte-identical from any checkout, so its digest must be too:
    // the original source built from another directory names the same block.
    let elsewhere = dir.path().join("elsewhere");
    fs::create_dir(&elsewhere).expect("create second source directory");
    let moved = write_fixture(&elsewhere, MODEL, SOURCE);
    let out_dir = dir.path().join("moved");
    compile(&moved, SOURCE, target, &out_dir);
    for digest in recorded(&out_dir) {
        assert_eq!(
            digest, builds[0],
            "{target}: the source path must not change the Solve program digest"
        );
    }
}

#[test]
fn embedded_c_galec_names_the_solve_block_its_c_was_printed_from() {
    assert_routed_by_content("embedded-c-galec", |out_dir| {
        [".h", ".c"]
            .map(|extension| c_banner_digest(&out_dir.join(format!("{MODEL}{extension}"))))
            .to_vec()
    });
}

#[cfg(feature = "fmu-packaging")]
#[test]
fn galec_production_names_the_solve_block_in_its_c_and_production_code_manifest() {
    use super::container_xml_support::sole_attribute_value;

    assert_routed_by_content("galec-production", |out_dir| {
        let production = out_dir.join(MODEL).join("ProductionCode");
        let manifest = production.join("manifest.xml");
        assert_eq!(sole_attribute_value(&manifest, "digestAlgorithm"), "BLAKE3");
        let mut digests = [".h", ".c"]
            .map(|extension| c_banner_digest(&production.join(format!("{MODEL}{extension}"))))
            .to_vec();
        digests.push(sole_attribute_value(&manifest, "digest"));
        digests
    });
}
