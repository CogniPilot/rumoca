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
        "name = \"msl_sim_regression\"",
        "required-features = [\"msl-sim-tests\"]",
    ] {
        assert!(
            manifest.contains(required),
            "MSL simulation regression manifest wiring is missing `{required}`"
        );
    }

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
    for target in [
        "fmi2_msl_test",
        "fmi3_msl_test",
        "embedded_c_msl_test",
        "casadi_msl_test",
    ] {
        assert!(
            workflow.contains(&format!("--features msl-external-tests --test {target}")),
            "nightly external-MSL diagnostics do not select `{target}`"
        );
    }

    assert!(
        !workflow.contains("--test fmu_target_discovery"),
        "target-list discovery writes proposed fixtures and must remain a manual maintenance command"
    );
}

#[test]
fn opt_in_backend_stress_survey_is_not_mislabeled_as_a_required_gate() {
    let contributing = repository_file("CONTRIBUTING.md");
    for required in [
        "`backend-stress-tests` is an opt-in 30-model diagnostic survey",
        "`msl-external-tests` contains opt-in MSL corpus cross-checks",
        "`fmu_target_discovery` is a manual target-list maintenance",
    ] {
        assert!(
            contributing.contains(required),
            "verification-surface documentation is missing `{required}`"
        );
    }
}
