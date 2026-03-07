use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

fn write_model(path: &Path, source: &str) {
    fs::write(path, source).expect("write model file");
}

#[test]
fn fmt_check_fails_when_file_needs_formatting() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("needs_fmt.mo");
    write_model(&file, "model NeedsFmt\n  Real x(start=1);\nend NeedsFmt;\n");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--check")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --check");

    assert!(
        !output.status.success(),
        "fmt --check should fail when file needs formatting"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Would reformat"));
    assert!(stderr.contains("need formatting"));
}

#[test]
fn fmt_check_succeeds_for_already_formatted_file() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("formatted.mo");
    write_model(
        &file,
        "model Formatted\n  Real x(start = 1);\nend Formatted;\n",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--check")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --check");

    assert!(
        output.status.success(),
        "fmt --check should succeed for already formatted file"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("files checked"));
    assert!(stderr.contains("0 need formatting"));
}

#[test]
fn lint_fails_on_syntax_error() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("bad.mo");
    write_model(&file, "model Bad\n  Real x\nend Bad;\n");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("lint")
        .arg(&file)
        .output()
        .expect("run rumoca lint");

    assert!(!output.status.success(), "lint should fail on syntax error");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stdout.contains("syntax-error"));
    assert!(stdout.contains("[error]"));
    assert!(stderr.contains("errors="));
}

#[test]
fn lint_warnings_as_errors_fails_on_warning() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("warn.mo");
    write_model(&file, "model badModel\n  Real x;\nend badModel;\n");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("lint")
        .arg("--warnings-as-errors")
        .arg(&file)
        .output()
        .expect("run rumoca lint --warnings-as-errors");

    assert!(
        !output.status.success(),
        "lint --warnings-as-errors should fail when warnings are emitted"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("[warning]"));
}

#[test]
fn lint_succeeds_for_clean_model() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("clean.mo");
    write_model(
        &file,
        "model CleanModel\n  Real x;\nequation\n  der(x) = x;\nend CleanModel;\n",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("lint")
        .arg(&file)
        .output()
        .expect("run rumoca lint");

    assert!(
        output.status.success(),
        "lint should succeed for clean model"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("errors=0"));
}
