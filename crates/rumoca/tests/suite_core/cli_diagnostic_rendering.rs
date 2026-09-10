//! What a compile failure shows on the terminal.
//!
//! A phase diagnostic is a structure: a message, one label per source location
//! it can name, and the notes carrying its `help(...)` text. The LSP and the
//! Python/API consumers read that structure through `PhaseError::to_diagnostic`,
//! so a renderer that keeps only the primary span reports strictly less to the
//! person actually running the compiler than to the person editing the file.
//! These tests pin the two ends of that contract: a two-label error renders both
//! locations and its help, and a one-label error gains nothing it did not state.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

fn write_model(path: &Path, source: &str) {
    fs::write(path, source).expect("write model file");
}

fn compile_stderr(file: &Path, model: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--model")
        .arg(model)
        .output()
        .expect("run rumoca compile");
    assert!(
        !output.status.success(),
        "the model must be rejected: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    String::from_utf8_lossy(&output.stderr).into_owned()
}

/// MLS §10.5 / §9.1: subscripting a `connect` argument selects along a declared
/// dimension, so `connect(a[1], b)` against a dimension-less `C a` is a
/// violation stated by two locations — the endpoint and the declaration — plus
/// the rule that makes it one. All three must reach the terminal.
///
/// The declaration and the `connect` sit far enough apart that the renderer
/// gives each its own snippet, which is what makes both one-based
/// `file:line:column` headers assertable: an off-by-one in either is invisible
/// to a test that only greps for the error code.
#[test]
fn compile_reports_every_label_and_the_help_of_a_two_label_error() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("EndpointSubscript.mo");
    write_model(
        &file,
        "connector C\n  Real e;\n  flow Real f;\nend C;\n\nmodel EndpointSubscript\n  C a;\n  C b;\n  Real filler1;\n  Real filler2;\n  Real filler3;\n  Real filler4;\nequation\n  filler1 = 1.0;\n  filler2 = 2.0;\n  filler3 = 3.0;\n  filler4 = 4.0;\n  connect(a[1], b);\nend EndpointSubscript;\n",
    );

    let stderr = compile_stderr(&file, "EndpointSubscript");

    assert!(stderr.contains("EF026"), "{stderr}");

    // `C a;` is line 7, column 3; `connect(a[1], b);` subscripts `a` at line 18,
    // column 11. Both are one-based, as every `file:line:column` this compiler
    // prints is.
    let declaration_location = format!("{}:7:3", file.display());
    let endpoint_location = format!("{}:18:11", file.display());
    assert!(
        stderr.contains(&declaration_location),
        "the declaration-site label must name {declaration_location}: {stderr}"
    );
    assert!(
        stderr.contains(&endpoint_location),
        "the endpoint label must name {endpoint_location}: {stderr}"
    );

    assert!(
        stderr.contains("declared here without dimensions"),
        "the declaration-site label text must reach the terminal: {stderr}"
    );
    assert!(
        stderr.contains("this connect endpoint is subscripted"),
        "the endpoint label text must reach the terminal: {stderr}"
    );
    assert!(
        stderr.contains("MLS §10.5"),
        "the diagnostic's help must reach the terminal: {stderr}"
    );
}

/// The counterpart: an error that states one location and no help renders one
/// location and no help. Carrying the extra structure must not invent a second
/// snippet or an empty `help:` line for the diagnostics that never had either.
#[test]
fn compile_reports_a_one_label_error_with_a_single_location_and_no_help() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("UndefinedRef.mo");
    write_model(
        &file,
        "model UndefinedRef\n  Real x;\nequation\n  x = missingThing;\nend UndefinedRef;\n",
    );

    let stderr = compile_stderr(&file, "UndefinedRef");

    assert!(stderr.contains("ER002"), "{stderr}");
    assert!(
        stderr.contains("unresolved component reference"),
        "the sole label text must reach the terminal: {stderr}"
    );

    let location = format!("{}:4:7", file.display());
    assert!(
        stderr.contains(&location),
        "the sole label must name {location}: {stderr}"
    );
    assert_eq!(
        stderr.matches(&format!("{}:", file.display())).count(),
        1,
        "a one-label error must render exactly one source location: {stderr}"
    );
    assert!(
        !stderr.contains("help:"),
        "a diagnostic with no help must not grow one: {stderr}"
    );
}
