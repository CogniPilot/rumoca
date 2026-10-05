//! The generated C of a state-event FMI component compiles warning-free under
//! `-Wall -Wextra -Werror` at `-O2` and `-O3` with source fortification, the
//! optimization levels at which GCC's flow-sensitive warnings such as
//! `-Wmaybe-uninitialized` see through the inlined Co-Simulation locator
//! (SPEC_0044 ME-EVENT-004). Importers build packaged sources with warnings as
//! errors, so a warning there is a build failure. The module runs on Linux,
//! where `cc` is the GCC these warnings come from.

use std::path::{Path, PathBuf};
use std::process::Command;

const SOURCE: &str = r#"
model RelationSwitch
  Real x(start = 1.0, fixed = true);
  Boolean low;
equation
  der(x) = if x > 0.5 then -1.0 else -0.2;
  low = x < 0.3;
end RelationSwitch;

model BouncingBall
  parameter Real e = 0.8;
  parameter Real g = 9.81;
  Real h(start = 1.0, fixed = true);
  Real v(start = 0.0, fixed = true);
equation
  der(h) = v;
  der(v) = -g;
  when h < 0 then
    reinit(v, -e * pre(v));
  end when;
end BouncingBall;
"#;

/// The FMI headers of `target`: the FMI 3 headers the fmi-ls-wasm target
/// carries, and the FMI 2 headers vendored as a test fixture.
fn headers(target: &str) -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    match target {
        "fmi2" => manifest.join("tests/fixtures/fmi2-headers"),
        _ => manifest.join("../rumoca-phase-codegen/src/templates/fmi-ls-wasm/fmi3-headers"),
    }
}

/// Write the rendered sources of `model` for `target` under `work`; returns
/// the sources directory and its translation units.
fn render(work: &Path, model: &str, target: &str) -> (PathBuf, Vec<PathBuf>) {
    let compiled = rumoca::Compiler::new()
        .model(model)
        .compile_str(SOURCE, "StateEvents.mo")
        .unwrap_or_else(|error| panic!("compile {model}: {error:#}"));
    let files = rumoca::render_target_files(&compiled, model, target, None)
        .unwrap_or_else(|error| panic!("render {model} {target}: {error:#}"));
    let root = work.join(format!("{model}-{target}"));
    let sources = root.join("sources");
    std::fs::create_dir_all(&sources).expect("create the sources directory");
    let mut units = Vec::new();
    for file in files
        .iter()
        .filter(|file| file.path.starts_with("sources/"))
    {
        let path = root.join(&file.path);
        std::fs::write(&path, &file.content).expect("write a rendered source");
        if file.path.ends_with(".c") {
            units.push(path);
        }
    }
    let model_c = std::fs::read_to_string(sources.join("model.c")).expect("read model.c");
    assert!(
        model_c.contains("rmc_cs_interval"),
        "{model} {target} carries the Co-Simulation state-event locator"
    );
    (sources, units)
}

/// Compile one translation unit with warnings as errors at `level`.
fn assert_warning_free(sources: &Path, target: &str, unit: &Path, level: &str, object: &Path) {
    let output = Command::new("cc")
        .args([
            "-std=c11",
            level,
            "-U_FORTIFY_SOURCE",
            "-D_FORTIFY_SOURCE=2",
            "-Wall",
            "-Wextra",
            "-Wmaybe-uninitialized",
            "-Werror",
            "-c",
        ])
        .arg(format!("-I{}", sources.display()))
        .arg(format!("-I{}", headers(target).display()))
        .arg(unit)
        .arg("-o")
        .arg(object)
        .output()
        .expect("start the C compiler");
    assert!(
        output.status.success(),
        "{target} {} at {level} did not compile warning-free:\n{}",
        unit.display(),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn generated_state_event_c_compiles_without_warnings_when_optimized() {
    let work = tempfile::tempdir().expect("a work directory");
    let object = work.path().join("unit.o");
    for (model, target) in [
        ("RelationSwitch", "fmi2"),
        ("RelationSwitch", "fmi3"),
        ("BouncingBall", "fmi2"),
        ("BouncingBall", "fmi3"),
    ] {
        let (sources, units) = render(work.path(), model, target);
        for (level, unit) in ["-O2", "-O3"]
            .into_iter()
            .flat_map(|level| units.iter().map(move |unit| (level, unit)))
        {
            assert_warning_free(&sources, target, unit, level, &object);
        }
    }
}
