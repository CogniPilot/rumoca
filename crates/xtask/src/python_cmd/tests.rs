use super::{PythonTestArgs, run};

#[test]
fn invalid_wheel_is_rejected_before_interpreter_invocation() {
    let directory = tempfile::tempdir().expect("temporary wheel context");
    for wheel in [
        directory.path().join("missing.whl"),
        directory.path().to_path_buf(),
    ] {
        let error = run(PythonTestArgs {
            python: directory.path().join("interpreter-must-not-run"),
            wheel: Some(wheel),
            require_guide: true,
        })
        .expect_err("invalid wheel must fail before invoking Python or installing files");
        assert!(
            error
                .to_string()
                .contains("--wheel must name an existing wheel"),
            "{error}"
        );
    }
}
