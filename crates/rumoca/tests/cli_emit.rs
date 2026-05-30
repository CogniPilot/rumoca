//! End-to-end coverage for `rumoca compile --emit <stage>` (and `--inspect`).
//!
//! These invoke the real binary so the codegen templates and IR serialization
//! are exercised — the prior parse-only CLI tests missed broken `ast-mo`
//! (template field/whitespace bugs) and `ast-json` (non-string map key).

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

/// A small model that uses a `parameter`, a `when` equation, and `der()` — the
/// features whose rendering/serialization previously broke the AST emitters.
const FIXTURE: &str = "\
model EmitFixture
  parameter Real g = 9.81;
  Real x(start = 1);
  Real v(start = 0);
equation
  der(x) = v;
  der(v) = -g;
  when x < 0 then
    reinit(v, -v);
  end when;
end EmitFixture;
";

fn fixture_file() -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("EmitFixture.mo");
    fs::write(&file, FIXTURE).expect("write fixture");
    (dir, file)
}

fn compile_emit(file: &Path, emit: &str) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--emit")
        .arg(emit)
        .output()
        .unwrap_or_else(|err| panic!("run rumoca compile --emit {emit}: {err}"))
}

fn assert_emit_ok(file: &Path, emit: &str) -> String {
    let output = compile_emit(file, emit);
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "`compile --emit {emit}` failed (status {:?}).\nstdout:\n{stdout}\nstderr:\n{stderr}",
        output.status.code()
    );
    assert!(
        !stdout.trim().is_empty(),
        "`compile --emit {emit}` produced empty output"
    );
    stdout
}

#[test]
fn emit_modelica_stages_render() {
    let (_dir, file) = fixture_file();
    // Each Modelica-form stage renders non-empty source. (The `when` equation
    // only survives literally in the pre-lowering AST; flat/dae lower it into
    // discrete event handling, so it is not asserted here.)
    for emit in ["ast-mo", "flat-mo", "dae-mo"] {
        let out = assert_emit_ok(&file, emit);
        assert!(
            out.contains("der(x)"),
            "`--emit {emit}` should render the model equations, got:\n{out}"
        );
    }
}

#[test]
fn emit_json_stages_are_valid_json() {
    let (_dir, file) = fixture_file();
    for emit in ["ast-json", "flat-json", "dae-json", "solve-json"] {
        let out = assert_emit_ok(&file, emit);
        serde_json::from_str::<serde_json::Value>(&out)
            .unwrap_or_else(|err| panic!("`--emit {emit}` did not produce valid JSON: {err}"));
    }
}

#[test]
fn emit_ast_mo_is_well_formed_modelica() {
    let (_dir, file) = fixture_file();
    let out = assert_emit_ok(&file, "ast-mo");
    // Regression guards for the AST pretty-printer whitespace/field bugs.
    assert!(
        out.contains("parameter Real g"),
        "variability prefix must keep its trailing space (`parameter Real`), got:\n{out}"
    );
    assert!(
        !out.contains("parameterReal"),
        "variability prefix collapsed into the type name:\n{out}"
    );
    assert!(
        out.starts_with("model EmitFixture"),
        "output should start at the model declaration (no leading blank line):\n{out:?}"
    );
    assert!(
        out.contains("reinit(v, -v)") || out.contains("reinit(v,-v)"),
        "the `when` body must be rendered, got:\n{out}"
    );
}

#[test]
fn inspect_structure_on_compile() {
    let (_dir, file) = fixture_file();
    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(&file)
        .arg("--inspect")
        .arg("structure")
        .output()
        .expect("run rumoca compile --inspect structure");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "`compile --inspect structure` failed.\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
}
