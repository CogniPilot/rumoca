//! Retired/suspended target tombstones and the GALEC-never-emits-C tripwire.
//!
//! `c-ode` and `embedded-c-galec` are permanently retired: the C export
//! surface is the combined FMI 3 ME+CS product (Model Exchange for host-owned
//! integration, Co-Simulation for the built-in solver) plus the future
//! Solve-rendered embedded target. `galec-production` is suspended pending
//! its Solve-rendered Production Code leaf. GALEC never emits C: every
//! Production/Embedded C artifact renders from the refined Solve product.
//!
//! Every scan in this module is a CREEP-BACK TRIPWIRE, not the construction
//! proof. The authoritative gate is the mandatory closed per-file
//! kind/context schema and compatibility table (SPEC_0034 GAL-043); until it
//! lands, these tests make re-adding the retired products loud instead of
//! silent. The registry-level refusal (`build.rs` retired/suspended lists,
//! `templates::builtin_target`) is the enforcement; these tests prove that
//! enforcement stays wired.

use std::path::{Path, PathBuf};

fn codegen_templates_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../crates/rumoca-phase-codegen/src/templates")
}

/// The retired names resolve to nothing, carry registry messages, and their
/// template directories are gone. A reappearing directory fails `build.rs`
/// first; this test is the runtime half, so the failure is visible even in a
/// tree that bypassed the build script (for example a stale generated
/// registry).
#[test]
fn retired_target_names_are_tombstoned() {
    for retired in rumoca_phase_codegen::templates::retired_targets() {
        assert!(
            rumoca_phase_codegen::templates::builtin_target(retired.name).is_none(),
            "retired target `{}` must not resolve as a built-in",
            retired.name
        );
        assert!(
            !retired.message.is_empty(),
            "retired target `{}` must carry a retirement message",
            retired.name
        );
        assert!(
            !codegen_templates_root().join(retired.name).exists(),
            "retired target directory `{}` reappeared; its identity never returns: {}",
            retired.name,
            retired.message
        );
    }
    let retired_names: Vec<&str> = rumoca_phase_codegen::templates::retired_targets()
        .iter()
        .map(|target| target.name)
        .collect();
    assert!(
        retired_names.contains(&"c-ode") && retired_names.contains(&"embedded-c-galec"),
        "the permanent retirement list lost an entry; removing a name is a reviewed decision, \
         never a side effect: {retired_names:?}"
    );
}

/// The suspended names resolve to nothing and carry messages naming the
/// checked roots that bring them back. Distinct from retirement: removing a
/// suspended entry is the re-registration act, reviewed against those roots.
#[test]
fn suspended_target_names_fail_closed_until_their_roots_land() {
    for suspended in rumoca_phase_codegen::templates::suspended_targets() {
        assert!(
            rumoca_phase_codegen::templates::builtin_target(suspended.name).is_none(),
            "suspended target `{}` must not resolve as a built-in while suspended",
            suspended.name
        );
        assert!(
            suspended.message.contains("Solve"),
            "suspended target `{}` must name the Solve-rendered root that brings it back: {}",
            suspended.name,
            suspended.message
        );
        assert!(
            !codegen_templates_root().join(suspended.name).exists(),
            "suspended target directory `{}` reappeared before its checked roots landed: {}",
            suspended.name,
            suspended.message
        );
    }
    assert!(
        rumoca_phase_codegen::templates::suspended_target("galec-production").is_some(),
        "galec-production is suspended (container identity valid, C leaf pending the \
         SolveAlgorithmBlock rendering); if it was re-registered, its two checked per-file \
         roots must exist and this assertion moves to the compatibility schema"
    );
}

/// TRIPWIRE: no Algorithm Code target may bundle a C/H template or declare a
/// C/H product file. The same scan runs in `build.rs` before the registry is
/// generated; this runtime twin proves the shipped registry agrees, and the
/// negative fixture below proves the scan actually detects a violation.
#[test]
fn algorithm_code_targets_bundle_no_c_templates() {
    for target in rumoca_phase_codegen::templates::builtin_targets() {
        if !manifest_is_algorithm_code(target.manifest) {
            continue;
        }
        for template in target.templates {
            assert!(
                !template_path_is_c(template.path),
                "Algorithm Code target `{}` bundles C/H template `{}`: GALEC never emits C; \
                 render C from the refined Solve product",
                target.name,
                template.path
            );
        }
        for violation in manifest_c_product_lines(target.manifest) {
            panic!(
                "Algorithm Code target `{}` declares C/H product `{violation}`: GALEC never \
                 emits C; render C from the refined Solve product",
                target.name
            );
        }
    }
}

/// Negative fixture: the scan predicates detect each planted violation shape,
/// so a passing sweep means "no violation exists", not "the scan matched
/// nothing".
#[test]
fn galec_c_tripwire_detects_planted_violations() {
    assert!(template_path_is_c("model.c.jinja"));
    assert!(template_path_is_c("kernels.h.jinja"));
    assert!(template_path_is_c("model.c"));
    assert!(!template_path_is_c("model.alg.jinja"));
    assert!(!template_path_is_c("manifest.xml.jinja"));

    let planted =
        "version = 1\nir = \"algorithm-code\"\n[[files]]\npath = \"{{ model_name }}.c\"\n";
    assert!(manifest_is_algorithm_code(planted));
    assert_eq!(
        manifest_c_product_lines(planted),
        vec!["path = \"{{ model_name }}.c\"".to_string()]
    );

    let clean = "version = 1\nir = \"algorithm-code\"\n[[files]]\npath = \"Model.alg\"\n";
    assert!(manifest_c_product_lines(clean).is_empty());

    let solve = "version = 1\nir = \"solve\"\n[[files]]\npath = \"{{ model_name }}.c\"\n";
    assert!(!manifest_is_algorithm_code(solve));
}

/// TRIPWIRE: the deleted AC-to-C rendering surfaces stay deleted. The
/// authoritative guarantee is registry-level (no C template can be declared
/// under `ir = "algorithm-code"`); this pins the specific files whose
/// creep-back history motivated the retirement.
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
             and the Solve-rendered embedded target; GALEC never emits C"
        );
    }
}

fn manifest_is_algorithm_code(manifest: &str) -> bool {
    manifest.lines().any(|line| {
        let trimmed = line.trim_start();
        trimmed.starts_with("ir") && trimmed.contains("\"algorithm-code\"")
    })
}

fn template_path_is_c(path: &str) -> bool {
    let stem = path.strip_suffix(".jinja").unwrap_or(path);
    Path::new(stem)
        .extension()
        .is_some_and(|extension| extension == "c" || extension == "h")
}

fn manifest_c_product_lines(manifest: &str) -> Vec<String> {
    manifest
        .lines()
        .map(str::trim_start)
        .filter(|line| line.starts_with("path") && (line.contains(".c\"") || line.contains(".h\"")))
        .map(str::to_owned)
        .collect()
}
