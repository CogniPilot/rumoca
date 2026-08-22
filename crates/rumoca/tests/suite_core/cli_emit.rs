//! End-to-end coverage for `rumoca compile --emit <stage>` (and `--inspect`).
//!
//! These invoke the real binary so the codegen templates and IR serialization
//! are exercised, including the AST JSON map-key representation.

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

const FLAT_ONLY_FIXTURE: &str = "\
model FlatOnlyFixture
  Real x;
algorithm
  x := 1;
end FlatOnlyFixture;
";

const STRUCTURED_EQUATION_FIXTURE: &str = "\
model StructuredEquationFixture
  parameter Integer N = 3;
  Real x[N];
equation
  for i in 1:N loop
    x[i] = i;
  end for;
end StructuredEquationFixture;
";

const NON_MATERIALIZED_STRUCTURED_FIXTURE: &str = "\
model NonMaterializedStructuredFixture
  parameter Integer N = 6;
  parameter Real k = 1;
  Real x[N](each start = 1);
equation
  for i in 1:N loop
    der(x[i]) = -k * x[i] + 0.5;
  end for;
end NonMaterializedStructuredFixture;
";

const OCCURRENCE_SCOPED_VARIABILITY_FIXTURE: &str = "\
model ParameterFamily
  parameter Integer N = 6;
  parameter Real p = 2;
  Real h[N];
equation
  for i in 1:N loop
    h[i] = p * i;
  end for;
end ParameterFamily;

model StateFamily
  parameter Integer N = 6;
  Real h[N];
  Real x[N](each start = 1);
equation
  for i in 1:N loop
    h[i] = x[i];
  end for;
  for i in 1:N loop
    der(x[i]) = -h[i];
  end for;
end StateFamily;

model OccurrenceScopedVariabilityFixture
  ParameterFamily parameterFamily;
  StateFamily stateFamily;
end OccurrenceScopedVariabilityFixture;
";

fn fixture_file() -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("EmitFixture.mo");
    fs::write(&file, FIXTURE).expect("write fixture");
    (dir, file)
}

fn named_fixture_file(name: &str, source: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join(format!("{name}.mo"));
    fs::write(&file, source).expect("write fixture");
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

fn compile_emit_to(file: &Path, emit: &str, output: &Path) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(file)
        .arg("--model")
        .arg("EmitFixture")
        .arg("--emit")
        .arg(emit)
        .arg("--output")
        .arg(output)
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
    for emit in ["flat-mo", "dae-mo"] {
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
fn failed_emit_invalidates_previous_output() {
    let (dir, file) = fixture_file();
    let artifact = dir.path().join("artifact.json");
    let first = compile_emit_to(&file, "dae-json", &artifact);
    assert!(first.status.success(), "initial emit must succeed");
    assert!(artifact.is_file(), "initial emit must create its artifact");

    fs::write(&file, FIXTURE.replace("-g", "-missingSymbol")).expect("break fixture");
    let second = compile_emit_to(&file, "dae-json", &artifact);
    assert!(
        !second.status.success(),
        "broken source must fail compilation"
    );
    assert!(
        !artifact.exists(),
        "failed compilation must not leave the previous IR artifact"
    );
}

#[test]
fn emit_flat_json_stops_before_todae() {
    let (_dir, file) = named_fixture_file("FlatOnlyFixture", FLAT_ONLY_FIXTURE);
    let out = assert_emit_ok(&file, "flat-json");
    let json = serde_json::from_str::<serde_json::Value>(&out)
        .expect("flat-json should produce valid JSON even when later phases reject the model");
    assert!(
        json.get("algorithms").is_some(),
        "flat artifact should contain the model algorithm section, got:\n{out}"
    );
}

#[test]
fn emit_flat_json_exposes_structured_equation_families() {
    let (_dir, file) = named_fixture_file("StructuredEquationFixture", STRUCTURED_EQUATION_FIXTURE);
    let out = assert_emit_ok(&file, "flat-json");
    let json = serde_json::from_str::<serde_json::Value>(&out)
        .expect("flat-json should produce valid JSON");
    let families = json
        .get("structured_equations")
        .and_then(serde_json::Value::as_array)
        .expect("flat artifact should expose structured_equations");

    assert_eq!(families.len(), 1);
    assert!(
        json.get("for_equations").is_none(),
        "flat artifact should not expose obsolete for_equations key, got:\n{out}"
    );
    assert_eq!(
        families[0]["domain"]["binders"][0]["display_name"],
        serde_json::json!("i")
    );
    assert_eq!(families[0]["equations_per_point"], serde_json::json!(1));
    assert!(
        families[0].get("iterations").is_none(),
        "structured family should not serialize one entry per scalar iteration"
    );
}

#[test]
fn flat_modelica_fails_closed_for_non_materialized_structured_families() {
    let (_dir, file) = named_fixture_file(
        "NonMaterializedStructuredFixture",
        NON_MATERIALIZED_STRUCTURED_FIXTURE,
    );
    let output = compile_emit(&file, "flat-mo");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        !output.status.success(),
        "non-materialized Flat families must not render placeholder equations:\n{stdout}"
    );
    assert!(
        stderr.contains("scalar equation view is unavailable")
            && stderr.contains("structured family"),
        "the rejection must identify the unsupported scalar view:\n{stderr}"
    );
    assert!(
        stderr.contains("rumoca::codegen::EC007"),
        "the rejection must carry its stable diagnostic code so a caller can match on \
         the refusal rather than on prose:\n{stderr}"
    );
    assert!(
        !stdout.contains("= 0.0"),
        "failed Flat export must not publish cheapened equation bodies:\n{stdout}"
    );
}

/// The Flat JSON dump is the self-describing sibling of the export above: it
/// serializes the cheapened rows AND the `interiors_materialized` flag that
/// says they are cheapened, so it makes no claim a checked view could falsify
/// and is deliberately not refused.
#[test]
fn flat_json_publishes_the_materialization_flag_with_the_cheapened_rows() {
    let (_dir, file) = named_fixture_file(
        "NonMaterializedStructuredFixture",
        NON_MATERIALIZED_STRUCTURED_FIXTURE,
    );
    let out = assert_emit_ok(&file, "flat-json");
    let json =
        serde_json::from_str::<serde_json::Value>(&out).expect("flat-json must be valid JSON");
    let families = json
        .get("structured_equations")
        .and_then(serde_json::Value::as_array)
        .expect("flat artifact exposes structured_equations");
    assert!(
        families
            .iter()
            .any(|family| family["interiors_materialized"] == serde_json::json!(false)),
        "the dump must state that its interior rows are not materialized:\n{out}"
    );
}

#[test]
fn same_named_families_do_not_share_variability_proofs_across_occurrences() {
    let (_dir, file) = named_fixture_file(
        "OccurrenceScopedVariabilityFixture",
        OCCURRENCE_SCOPED_VARIABILITY_FIXTURE,
    );

    let out = assert_emit_ok(&file, "dae-json");
    serde_json::from_str::<serde_json::Value>(&out)
        .expect("occurrence-scoped family model should produce valid DAE JSON");
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
