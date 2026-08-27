//! The expanded battery must elaborate in OpenModelica.
//!
//! The finite-difference gate proves the synthesized derivative is right; this
//! row proves the artifact is portable. It expands the same battery through
//! the entry `compile --emit-standard-modelica` uses, then asks `omc` to
//! instantiate each model. A construct only this compiler accepts would fail
//! here, which is the point: the expansion has to be ordinary Modelica.

use std::fs;
use std::process::Command;

use super::jacobian_finite_difference::battery_sources;

const SURFACE_FIXTURE: &str = "\
function scale
  input Real u[2];
  input Real k;
  output Real y[2];
algorithm
  y[1] := k*u[1]*u[2];
  y[2] := sin(u[1]) + u[2];
end scale;

model SurfaceFixture
  parameter Real u[2] = {0.3, -1.2};
  parameter Real k = 2.5;
  Real J[2, 2] = jacobian(scale(u, k), u);
end SurfaceFixture;
";

#[test]
fn the_cli_writes_the_expansion() {
    let directory = tempfile::tempdir().expect("temporary CLI directory");
    let file = directory.path().join("SurfaceFixture.mo");
    fs::write(&file, SURFACE_FIXTURE).expect("write fixture");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(&file)
        .arg("--model")
        .arg("SurfaceFixture")
        .arg("--emit-standard-modelica")
        .output()
        .expect("run rumoca compile --emit-standard-modelica");
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "the CLI must write the expansion: {stderr}"
    );
    assert!(
        stdout.contains("function scale_jacobian_u")
            && stdout.contains("function scale_ad_tangent"),
        "the CLI output must carry the generated functions:\n{stdout}"
    );
    assert!(
        stdout.contains("Real J[2, 2] = scale_jacobian_u(u, k);"),
        "the CLI output must rewrite the call:\n{stdout}"
    );
}

fn omc_available() -> bool {
    Command::new("omc")
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
}

#[test]
fn the_expansion_carries_no_extension_construct() {
    for (name, source) in battery_sources() {
        let expanded = rumoca_compile::parsing::expand_source_to_standard_modelica(
            &source,
            &format!("{name}.mo"),
        )
        .unwrap_or_else(|error| panic!("{name} must expand: {error:#}"));
        let model = expanded
            .split_once(&format!("model {name}"))
            .map(|(_, tail)| tail)
            .unwrap_or_else(|| panic!("{name} must keep its model:\n{expanded}"));
        assert!(
            !model.contains("jacobian("),
            "{name} kept a surface call in the model body:\n{expanded}"
        );
        // The wrapper is named for the formal it differentiates, which is the
        // differentiated function's own input name, not the caller's argument.
        assert!(
            expanded.contains("_jacobian_u"),
            "{name} did not mint a wrapper:\n{expanded}"
        );
        assert!(
            expanded.contains("_ad_tangent"),
            "{name} did not mint a tangent function:\n{expanded}"
        );
    }
}

#[test]
fn the_expanded_battery_instantiates_under_omc() {
    if !omc_available() {
        assert!(
            !std::path::Path::new("target/msl/omc-differential-required").is_file(),
            "the OpenModelica elaboration row is required in this lane, but no working `omc` is \
             on PATH"
        );
        eprintln!("skipping OpenModelica elaboration row: omc not available");
        return;
    }

    let directory = tempfile::tempdir().expect("temporary elaboration directory");
    for (name, source) in battery_sources() {
        let expanded = rumoca_compile::parsing::expand_source_to_standard_modelica(
            &source,
            &format!("{name}.mo"),
        )
        .unwrap_or_else(|error| panic!("{name} must expand: {error:#}"));
        let model_file = format!("{name}.mo");
        fs::write(directory.path().join(&model_file), &expanded).expect("write expanded model");
        fs::write(
            directory.path().join("check.mos"),
            format!("loadFile(\"{model_file}\");\ninstantiateModel({name});\ngetErrorString();\n"),
        )
        .expect("write elaboration script");

        let output = Command::new("omc")
            .arg("check.mos")
            .current_dir(directory.path())
            .output()
            .expect("run omc");
        let transcript = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            output.status.success(),
            "omc failed on the expanded {name}:\n{transcript}\n{expanded}"
        );
        assert!(
            transcript.contains(&format!("class {name}")),
            "omc did not instantiate the expanded {name}:\n{transcript}\n{expanded}"
        );
        assert!(
            !transcript.contains("Error:"),
            "omc reported errors for the expanded {name}:\n{transcript}\n{expanded}"
        );
    }
}

/// A number the elaboration script printed under a named marker.
fn marked_value(transcript: &str, marker: &str) -> Option<f64> {
    transcript
        .lines()
        .find_map(|line| line.strip_prefix(marker))
        .and_then(|text| text.trim().parse::<f64>().ok())
}

/// Running the expansion, not merely elaborating it, is what proves the
/// portable artifact carries the derivative the finite-difference gate checked.
///
/// Elaboration answers a typing question. A tangent that used a scalar product
/// where the shape called for an elementwise one is well typed whenever the
/// surrounding expression absorbs the collapsed rank, so it elaborates cleanly
/// and answers a different question than the one asked. Each battery probe
/// already computes its own central-difference Jacobian and their largest
/// entrywise gap, so simulating it under `omc` compares the two derivatives
/// inside the other tool.
#[test]
fn the_expanded_battery_agrees_with_finite_differences_under_omc() {
    if !omc_available() {
        assert!(
            !std::path::Path::new("target/msl/omc-differential-required").is_file(),
            "the OpenModelica differential row is required in this lane, but no working `omc` \
             is on PATH"
        );
        eprintln!("skipping OpenModelica differential row: omc not available");
        return;
    }

    let directory = tempfile::tempdir().expect("temporary differential directory");
    let mut checked = 0usize;
    for (name, source) in battery_sources() {
        let expanded = rumoca_compile::parsing::expand_source_to_standard_modelica(
            &source,
            &format!("{name}.mo"),
        )
        .unwrap_or_else(|error| panic!("{name} must expand: {error:#}"));
        let model_file = format!("{name}.mo");
        fs::write(directory.path().join(&model_file), &expanded).expect("write expanded model");
        fs::write(
            directory.path().join("run.mos"),
            format!(
                "loadFile(\"{model_file}\");\n\
                 simulate({name}, startTime = 0, stopTime = 0.001, numberOfIntervals = 1);\n\
                 print(\"OMC_ERRORS:\" + getErrorString() + \"\\n\");\n\
                 print(\"OMC_GAP:\" + String(val(gap, 0.001), significantDigits = 12) + \"\\n\");\n\
                 print(\"OMC_SIZE:\" + String(val(size_of_jacobian, 0.001), \
                 significantDigits = 12) + \"\\n\");\n"
            ),
        )
        .expect("write differential script");

        let output = Command::new("omc")
            .arg("run.mos")
            .current_dir(directory.path())
            .output()
            .expect("run omc");
        let transcript = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            transcript.contains("The simulation finished successfully"),
            "omc did not simulate the expanded {name}:\n{transcript}\n{expanded}"
        );
        assert_eq!(
            transcript
                .lines()
                .find_map(|line| line.strip_prefix("OMC_ERRORS:")),
            Some(""),
            "omc reported errors simulating the expanded {name}:\n{transcript}\n{expanded}"
        );
        let gap = marked_value(&transcript, "OMC_GAP:")
            .unwrap_or_else(|| panic!("{name} must report a gap:\n{transcript}"));
        let size = marked_value(&transcript, "OMC_SIZE:")
            .unwrap_or_else(|| panic!("{name} must report a magnitude:\n{transcript}"));

        // The probes are written at a step of 1e-5, so a central difference is
        // accurate to about 1e-10 relative plus the step's own truncation; the
        // tolerance below is the same one the in-process gate uses.
        let tolerance = 2.0e-5 * size.max(1.0);
        assert!(
            gap <= tolerance,
            "the expanded {name} disagrees with central differences under omc by {gap:e} \
             (tolerance {tolerance:e}, |J|max {size:e})\n{expanded}"
        );
        assert!(
            size > 1.0e-6,
            "the expanded {name} has an all-zero Jacobian under omc, so the comparison proves \
             nothing\n{expanded}"
        );
        checked += 1;
    }
    assert!(checked > 0, "the differential row must check the battery");
}

/// A String literal argument carries quotes into the wrapper's description
/// string; those quotes must be escaped so the emitted standard Modelica still
/// parses. Before the escape was added, `--emit-standard-modelica` reported
/// success on output that neither OpenModelica nor rumoca's own re-parse would
/// accept, which is the silent third state this feature must not have.
const STRING_ARGUMENT_FIXTURE: &str = "\
model StringArgExpansion
  function f
    input Real z;
    input String label;
    output Real y;
  algorithm
    y := z * z;
  end f;
  Real x = 0.5;
  Real J[1, 1] = jacobian(f(x, \"hi\"), x);
end StringArgExpansion;
";

#[test]
fn a_string_literal_argument_is_escaped_in_the_expansion() {
    let expanded = rumoca_compile::parsing::expand_source_to_standard_modelica(
        STRING_ARGUMENT_FIXTURE,
        "StringArgExpansion.mo",
    )
    .expect("a String literal argument must expand to valid standard Modelica");
    // The provenance quotes are escaped, never left bare to close the
    // description string early.
    assert!(
        expanded.contains("\\\"hi\\\""),
        "the String argument's quotes must be escaped in the wrapper description:\n{expanded}"
    );
    // The emitted text re-parses: expanding the expansion is a no-op that
    // succeeds, which it cannot do if the description string was truncated.
    rumoca_compile::parsing::expand_source_to_standard_modelica(&expanded, "Reparsed.mo")
        .expect("the emitted standard Modelica must itself re-parse");
}

#[test]
fn the_cli_emit_of_a_string_argument_reparses() {
    let directory = tempfile::tempdir().expect("temporary CLI directory");
    let file = directory.path().join("StringArgExpansion.mo");
    fs::write(&file, STRING_ARGUMENT_FIXTURE).expect("write fixture");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("compile")
        .arg(&file)
        .arg("--model")
        .arg("StringArgExpansion")
        .arg("--emit-standard-modelica")
        .output()
        .expect("run rumoca compile --emit-standard-modelica");
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    assert!(
        output.status.success(),
        "the CLI must not report success on unparseable output: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    // What the CLI wrote must re-parse, so emit and sim cannot disagree.
    rumoca_compile::parsing::expand_source_to_standard_modelica(&stdout, "CliReparsed.mo")
        .expect("the CLI-emitted standard Modelica must re-parse");
}
