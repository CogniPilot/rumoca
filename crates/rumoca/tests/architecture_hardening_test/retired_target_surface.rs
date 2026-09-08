//! Deleted target-name tombstones and the GALEC-never-emits-C tripwire.
//!
//! `c-ode`, `embedded-c-galec`, and `galec-production` are deleted spellings:
//! the C export surface is the combined FMI 3 ME+CS product
//! (Model Exchange for host-owned integration, Co-Simulation for the built-in
//! solver) plus the future `efmu` `SolveAlgorithmProduct`, whose output files
//! borrow the appropriate retained Algorithm Code or Solve view. GALEC never
//! emits C: every Production Code C artifact renders from the Solve block.
//!
//! Every scan in this module is a CREEP-BACK TRIPWIRE, not a production
//! compatibility registry. Deleted spellings carry no runtime record or
//! tailored behavior; they are ordinary unknown targets.

use std::fs;
use std::path::PathBuf;

use super::architecture_hardening_support::{collect_rs_files, workspace_root};

fn codegen_templates_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../crates/rumoca-phase-codegen/src/templates")
}

const FORBIDDEN_TARGET_NAMES: &[&str] = &["c-ode", "embedded-c-galec", "galec-production"];

/// Deleted names resolve to nothing and their template directories are gone.
#[test]
fn deleted_target_names_have_no_product_surface() {
    for deleted in FORBIDDEN_TARGET_NAMES {
        assert!(
            rumoca_phase_codegen::templates::builtin_target(deleted).is_none(),
            "deleted target `{deleted}` must not resolve as a built-in"
        );
        assert!(
            !codegen_templates_root().join(deleted).exists(),
            "deleted target directory `{deleted}` reappeared"
        );
    }
}

/// Every built-in manifest passes the current per-file kind/context parser.
/// That construction is the sole proof that GALEC files cannot be C/H; an
/// Algorithm Code target and a mixed eFMI target cannot be classified by one
/// retired target-wide `ir` string.
#[test]
fn current_target_parser_validates_all_builtin_file_relations() {
    let descriptors = rumoca_compile::codegen::targets::builtin_target_descriptors()
        .expect("every built-in target satisfies the current per-file construction contract");
    assert!(descriptors.iter().any(|target| target.id == "galec"));
    assert!(descriptors.iter().any(|target| target.id == "efmu"));
}

/// The retired target-wide classifier was vacuous after the current manifest
/// cutover and must never return as a second, weaker acceptance path.
#[test]
fn retired_target_wide_ir_classifier_cannot_return() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let build = fs::read_to_string(root.join("crates/rumoca-phase-codegen/build.rs"))
        .expect("read phase-codegen build script");
    assert!(!build.contains("manifest_is_algorithm_code"));
    assert!(!build.contains("reject_algorithm_code_c_templates"));

    for target in rumoca_phase_codegen::templates::builtin_targets() {
        assert!(
            !target.manifest.lines().any(|line| {
                line.split_once('=')
                    .is_some_and(|(key, _)| key.trim() == "ir")
            }),
            "built-in target `{}` restored the forbidden target-wide `ir` field",
            target.name
        );
    }
}

/// TRIPWIRE: the deleted AC-to-C rendering surfaces stay deleted. The
/// authoritative guarantee is the closed product construction; this pins the
/// specific files whose creep-back history motivated deletion.
#[test]
fn deleted_ac_to_c_surfaces_stay_deleted() {
    let phase_codegen_src =
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../crates/rumoca-phase-codegen/src");
    for relative in [
        "templates/c-ode",
        "templates/embedded-c-galec",
        "templates/galec-production",
    ] {
        assert!(
            !phase_codegen_src.join(relative).exists(),
            "`{relative}` reappeared: the C export surface is the fmi3 target (FMI 3.0 ME+CS) \
             and the future `efmu` SolveAlgorithmProduct; GALEC never emits C"
        );
    }
}

/// UI target registries and generated package bundles must use the current
/// target vocabulary.  Generated directories are optional in a source
/// checkout, but when a package build materializes them they are checked too;
/// this prevents a stale bundle from silently restoring a retired target in a
/// shipped editor surface.
#[test]
fn web_target_surfaces_contain_no_retired_names() {
    let root = workspace_root();
    let mut files = Vec::new();
    for relative in [
        "packages/rumoca-web/viz",
        "packages/rumoca-web/runtime",
        "packages/rumoca-web/vendor",
        "packages/playground/vendor",
        "packages/vscode/media/vendor",
        "packages/vscode/out",
        "packages/rumoca/dist",
    ] {
        collect_web_text_files(&root.join(relative), &mut files);
    }

    let mut offenders = Vec::new();
    for path in files {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("failed to read {}: {error}", path.display()));
        for retired in FORBIDDEN_TARGET_NAMES {
            if source.contains(retired) {
                offenders.push(format!("{}: `{retired}`", path.display()));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "web source or generated package surface restored retired target names:\n{}",
        offenders.join("\n")
    );
}

fn collect_web_text_files(root: &std::path::Path, files: &mut Vec<PathBuf>) {
    if !root.exists() {
        return;
    }
    let entries = fs::read_dir(root)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", root.display()));
    for entry in entries {
        let entry =
            entry.unwrap_or_else(|error| panic!("failed to enumerate {}: {error}", root.display()));
        let path = entry.path();
        if path.is_dir() {
            collect_web_text_files(&path, files);
            continue;
        }
        let is_text_surface = path
            .extension()
            .and_then(|extension| extension.to_str())
            .is_some_and(|extension| {
                matches!(
                    extension,
                    "cjs" | "css" | "html" | "js" | "json" | "mjs" | "ts"
                )
            });
        if is_text_surface {
            files.push(path);
        }
    }
}

/// The eFMU target owns its typed final-emission policy. Benchmark rows cannot
/// recreate the deleted free-form compiler flags or authenticate C comments as
/// a second policy channel.
#[test]
fn retired_benchmark_policy_channels_cannot_return() {
    let mut files = Vec::new();
    collect_rs_files(&workspace_root().join("crates/xtask/src"), &mut files);
    let forbidden = [
        ["--", "inline", "-", "policy"].concat(),
        ["--", "scalarize", "-", "policy"].concat(),
        ["inline", " policy"].concat(),
        ["scalarize", " policy"].concat(),
    ];
    let mut offenders = Vec::new();
    for path in files {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("failed to read {}: {error}", path.display()));
        for retired in &forbidden {
            if source.contains(retired) {
                offenders.push(format!("{}: `{retired}`", path.display()));
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "retired benchmark policy channels reappeared:\n{}",
        offenders.join("\n")
    );
}
