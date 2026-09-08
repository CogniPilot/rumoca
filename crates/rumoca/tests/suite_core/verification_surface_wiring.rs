use std::path::PathBuf;

fn repository_file(relative: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(relative);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", path.display()))
}

#[test]
fn required_msl_sim_regressions_are_selected_by_workspace_verification() {
    let manifest = repository_file("crates/rumoca/Cargo.toml");
    for required in [
        "msl-sim-tests = []",
        "name = \"suite_msl_sim\"",
        "required-features = [\"msl-sim-tests\"]",
    ] {
        assert!(
            manifest.contains(required),
            "MSL simulation regression manifest wiring is missing `{required}`"
        );
    }

    // The gated target is an umbrella binary, so also pin that it still pulls
    // in the regression file itself — otherwise the feature could stay wired
    // while the tests it selects silently vanish.
    let suite = repository_file("crates/rumoca/tests/suite_msl_sim/main.rs");
    assert!(
        suite.contains("mod msl_sim_regression;"),
        "`suite_msl_sim` must declare `msl_sim_regression`"
    );

    let workspace_runner = repository_file("crates/xtask/src/test_cmd.rs");
    assert!(
        workspace_runner.contains(r#"["--features", "rumoca/msl-sim-tests"]"#),
        "`cargo xtask verify workspace` must select the MSL simulation regressions"
    );

    let workflow = repository_file(".github/workflows/ci.yml");
    let ensure_msl = workflow
        .find("- name: Ensure MSL")
        .expect("CI must stage the pinned MSL tree");
    let workspace_tests = workflow
        .find("cargo xtask verify workspace")
        .expect("CI must run workspace verification");
    assert!(
        ensure_msl < workspace_tests,
        "CI must stage MSL before selecting the required MSL simulation regressions"
    );
}

#[test]
fn nightly_selects_each_reproducible_external_msl_cross_check() {
    let workflow = repository_file(".github/workflows/nightly.yml");
    let target = "casadi_msl_test";
    assert!(
        workflow.contains(&format!("--features msl-external-tests --test {target}")),
        "nightly external-MSL diagnostics do not select `{target}`"
    );

    assert!(
        !workflow.contains("--test fmu_target_discovery"),
        "target-list discovery writes proposed fixtures and must remain a manual maintenance command"
    );
}

#[test]
fn ci_and_nightly_require_the_embedded_head_to_head_ratchet() {
    for (workflow_path, next_job) in [
        (".github/workflows/ci.yml", "kani"),
        (".github/workflows/nightly.yml", "parser-fuzz"),
    ] {
        let workflow = repository_file(workflow_path);
        let start = workflow
            .find("\n  embedded-head-to-head:\n")
            .unwrap_or_else(|| panic!("{workflow_path} must own the embedded head-to-head job"));
        let end = workflow[start..]
            .find(&format!("\n  {next_job}:\n"))
            .map(|offset| start + offset)
            .unwrap_or_else(|| {
                panic!("embedded head-to-head must remain a distinct job in {workflow_path}")
            });
        let job = &workflow[start..end];
        for required in [
            "nix develop .#embedded-benchmark",
            "cargo xtask verify embedded-head-to-head",
            "target/verification/embedded-head-to-head-summary.json",
            "target/verification/embedded-head-to-head/*/*.trace",
            "if: always()",
            "CARGO_BUILD_JOBS: 4",
            "RUST_TEST_THREADS: 4",
            "RAYON_NUM_THREADS: 4",
        ] {
            assert!(
                job.contains(required),
                "required embedded ratchet wiring is missing `{required}` from {workflow_path}"
            );
        }
        assert!(
            !job.contains("continue-on-error"),
            "the embedded instruction ratchet must fail its {workflow_path} job"
        );
    }

    let flake = repository_file("flake.nix");
    assert!(
        flake.contains("devShells.embedded-benchmark = embeddedBenchmarkShell;"),
        "the embedded benchmark shell must remain exposed"
    );
    let shell_start = flake
        .find("embeddedBenchmarkShell = templateRuntimeShell [")
        .expect("flake must define the embedded benchmark shell");
    let shell_end = flake[shell_start..]
        .find("];\n        juliaShell")
        .map(|offset| shell_start + offset)
        .expect("embedded benchmark shell must remain a distinct package list");
    let embedded_shell = &flake[shell_start..shell_end];
    for required in ["ciPython", "pkgs.gcc-arm-embedded", "pkgs.qemu"] {
        assert!(
            embedded_shell.contains(required),
            "embedded benchmark shell is missing `{required}`"
        );
    }
}

#[test]
fn opt_in_backend_stress_survey_is_not_mislabeled_as_a_required_gate() {
    let contributing = repository_file("CONTRIBUTING.md");
    for required in [
        "`backend-stress-tests` is an opt-in 30-model diagnostic survey",
        "`msl-external-tests` contains opt-in MSL corpus cross-checks",
    ] {
        assert!(
            contributing.contains(required),
            "verification-surface documentation is missing `{required}`"
        );
    }
}
