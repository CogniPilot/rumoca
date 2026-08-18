//! Runtime acceptance checks for the Modelica interchange targets.

use std::fs;
use std::process::Command;

use rumoca::{Compiler, render_target_files};
use rumoca_compile::codegen::targets::RenderedTargetFile;

const SMOKE_MODEL: &str = "Smoke";
const SMOKE_SOURCE: &str = r#"
model Smoke
  Real x(start = 1);
  parameter Real k = 2;
equation
  der(x) = -k * x;
end Smoke;
"#;

fn rendered_targets() -> Vec<(&'static str, RenderedTargetFile)> {
    let compiled = Compiler::new()
        .model(SMOKE_MODEL)
        .compile_str(SMOKE_SOURCE, "Smoke.mo")
        .expect("compile Modelica interchange fixture");
    ["base-modelica", "flat-modelica", "dae-modelica"]
        .into_iter()
        .map(|target| {
            let files = render_target_files(&compiled, SMOKE_MODEL, target, None)
                .unwrap_or_else(|error| panic!("{target} must render: {error:#}"));
            assert_eq!(files.len(), 1, "{target} must emit one source document");
            (target, files.into_iter().next().expect("one rendered file"))
        })
        .collect()
}

#[test]
fn targets_round_trip_through_the_compiler() {
    for (target, file) in rendered_targets() {
        Compiler::new()
            .model(SMOKE_MODEL)
            .compile_str(&file.content, &file.path)
            .unwrap_or_else(|error| {
                panic!(
                    "{target} output must recompile through Rumoca: {error:#}\n{}",
                    file.content
                )
            });
    }
}

#[test]
fn targets_are_accepted_by_required_omc() {
    let omc_available = Command::new("omc").arg("--version").output().is_ok();
    if !super::template_runtime_policy::prerequisites_are_available(
        "OMC interchange check",
        &[("OMC", omc_available)],
    ) {
        return;
    }

    for (target, file) in rendered_targets() {
        let directory = tempfile::tempdir().expect("temporary OMC interchange directory");
        fs::write(directory.path().join(&file.path), &file.content)
            .expect("write Modelica interchange source");
        fs::write(
            directory.path().join("check.mos"),
            format!(
                "loadFile(\"{}\");\ncheckModel({SMOKE_MODEL});\ngetErrorString();\n",
                file.path
            ),
        )
        .expect("write OMC interchange script");
        let output = Command::new("omc")
            .arg("check.mos")
            .current_dir(directory.path())
            .output()
            .expect("start OMC interchange check");
        let transcript = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );
        assert!(
            output.status.success()
                && transcript.contains(&format!("Check of {SMOKE_MODEL} completed successfully")),
            "OMC rejected {target} output:\n{transcript}\n{}",
            file.content,
        );
    }
}
