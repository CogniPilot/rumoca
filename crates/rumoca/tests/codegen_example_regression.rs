use std::fs;
use std::path::{Path, PathBuf};

use rumoca::Compiler;
use tempfile::tempdir;

fn write_text(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent directory");
    }
    fs::write(path, content).expect("write file");
}

fn examples_template_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples/codegen")
}

fn checked_report_target_root() -> PathBuf {
    examples_template_root().join("checked_dae_report")
}

fn render_checked_report(dae: &rumoca_compile::compile::Dae, model_name: &str) -> String {
    let target = rumoca_compile::codegen::targets::TargetBundle::load(
        checked_report_target_root()
            .to_str()
            .expect("checked report target path should be utf8"),
    )
    .expect("load checked report target");
    let manifest = target
        .parse_manifest()
        .expect("parse checked report target");
    let rendered = rumoca_compile::codegen::targets::render_dae_target_files(
        &target, &manifest, dae, model_name,
    )
    .expect("render checked report target");
    assert_eq!(rendered.len(), 1, "report target has one output owner");
    rendered
        .into_iter()
        .next()
        .expect("one report output exists")
        .content
}

fn setup_mock_source_roots(root: &Path) -> (PathBuf, PathBuf, PathBuf) {
    let main_root = root.join("MainLib");
    let helper_root = root.join("HelperTypes");
    let service_root = root.join("ServiceTypes");

    write_text(
        &main_root.join("package.mo"),
        r#"
package MainLib
  model Example
    parameter Real r = HelperTypes.defaultR + ServiceTypes.bias;
    Real x(start=1);
  equation
    der(x) = -r * x;
  end Example;
end MainLib;
"#,
    );

    write_text(
        &helper_root.join("package.mo"),
        r#"
package HelperTypes
  constant Real defaultR = 10;
end HelperTypes;
"#,
    );

    write_text(
        &service_root.join("package.mo"),
        r#"
package ServiceTypes
  constant Real bias = 1;
end ServiceTypes;
"#,
    );

    (main_root, helper_root, service_root)
}

#[test]
fn all_direct_example_templates_render_in_ci() {
    let source = r#"
model ExampleTemplateSmoke
  Real x(start = 1);
  parameter Real k = 2;
equation
  der(x) = -k * x;
end ExampleTemplateSmoke;
"#;

    let result = Compiler::new()
        .model("ExampleTemplateSmoke")
        .compile_str(source, "ExampleTemplateSmoke.mo")
        .expect("compile example-template smoke model");
    let mut template_names = Vec::new();
    for entry in fs::read_dir(examples_template_root()).expect("read examples template root") {
        let path = entry.expect("read examples template entry").path();
        if path.extension().and_then(|ext| ext.to_str()) != Some("jinja") {
            continue;
        }
        let name = path
            .file_name()
            .expect("example template should have a file name")
            .to_string_lossy()
            .to_string();
        let rendered = result
            .render_template(path.to_string_lossy().as_ref())
            .unwrap_or_else(|err| panic!("render example template {name}: {err}"));
        assert!(
            !rendered.trim().is_empty(),
            "example template {name} rendered empty output"
        );
        assert!(
            !rendered.contains("{{") && !rendered.contains("{%"),
            "example template {name} leaked a Jinja placeholder"
        );
        template_names.push(name);
    }
    template_names.sort();
    assert_eq!(template_names, vec!["custom_checked_variables.jinja"]);
}

#[test]
fn checked_dae_report_uses_canonical_variables_and_systems() {
    let source = r#"
model SympyDecay
  Real x(start = 1);
  parameter Real k = 0.5;
equation
  der(x) = -k * x;
end SympyDecay;
"#;
    let result = Compiler::new()
        .model("SympyDecay")
        .compile_str(source, "SympyDecay.mo")
        .expect("compile checked report regression model");
    let report = render_checked_report(&result.dae, "SympyDecay");

    // The first literal is pinned to `dae_backend::TEMPLATE_SCHEMA_VERSION`:
    // the report template renders `dae.schema.version`, so every change to the
    // projected template shape bumps that constant and this literal with it.
    for expected in [
        "checked-dae-report 5",
        "model SympyDecay",
        "parameter k real",
        "state x real",
        "continuous_owners 1",
    ] {
        assert!(
            report.contains(expected),
            "checked report should contain {expected:?}, got:\n{report}"
        );
    }
}

#[test]
fn checked_dae_report_preserves_event_owners() {
    let source = r#"
model ReportEvent
  Real x(start = 0);
  discrete Integer selected(start = 0);
equation
  der(x) = 1;
  when x >= 0.5 then
    selected = 1;
  end when;
end ReportEvent;
"#;
    let result = Compiler::new()
        .model("ReportEvent")
        .compile_str(source, "ReportEvent.mo")
        .expect("compile checked event model");
    let report = render_checked_report(&result.dae, "ReportEvent");

    assert!(
        report.contains("b1c_owners 1"),
        "checked report must consume the atomic B.1c owner, got:\n{report}"
    );
    assert!(
        report.contains("conditions "),
        "checked report must consume the condition system, got:\n{report}"
    );
}

#[test]
fn compile_fails_when_transitive_root_source_roots_are_not_loaded() {
    let temp = tempdir().expect("tempdir");
    let (main_root, _helper_root, _service_root) = setup_mock_source_roots(temp.path());
    let source = r#"
model Wrapper
  extends MainLib.Example;
end Wrapper;
"#;

    let result = Compiler::new()
        .model("Wrapper")
        .source_root(main_root.to_string_lossy().as_ref())
        .compile_str(source, "Wrapper.mo");

    assert!(result.is_err(), "missing transitive roots must fail");
    let error = format!("{:?}", result.expect_err("error expected"));
    assert!(
        error.contains("HelperTypes") || error.contains("ServiceTypes"),
        "error should name an unresolved transitive root, got: {error}"
    );
}

#[test]
fn checked_dae_report_renders_with_all_source_roots_loaded() {
    let temp = tempdir().expect("tempdir");
    let (main_root, helper_root, service_root) = setup_mock_source_roots(temp.path());
    let source = r#"
model Wrapper
  import HelperTypes;
  import ServiceTypes;
  extends MainLib.Example;
end Wrapper;
"#;

    let result = Compiler::new()
        .model("Wrapper")
        .source_root(main_root.to_string_lossy().as_ref())
        .source_root(helper_root.to_string_lossy().as_ref())
        .source_root(service_root.to_string_lossy().as_ref())
        .compile_str(source, "Wrapper.mo")
        .expect("compile wrapper model");
    let report = render_checked_report(&result.dae, "Wrapper");

    assert!(report.contains("model Wrapper"));
    assert!(report.contains("parameter r real"));
    assert!(report.contains("state x real"));
}

#[test]
fn checked_dae_report_preserves_msl_resistor_units() {
    let source = r#"
package Modelica
  package Units
    package SI
      type Resistance = Real(final quantity = "ElectricResistance", final unit = "Ohm", final displayUnit = "Ohm");
      type Voltage = Real(final unit = "V");
      type Current = Real(final unit = "A");
    end SI;
  end Units;

  package Electrical
    package Analog
      package Basic
        model Resistor
          parameter Modelica.Units.SI.Resistance R = 1;
          Modelica.Units.SI.Voltage v;
          Modelica.Units.SI.Current i;
        equation
          v = R * i;
        end Resistor;
      end Basic;
    end Analog;
  end Electrical;
end Modelica;

model MslResistorExample
  extends Modelica.Electrical.Analog.Basic.Resistor;
equation
  i = 1;
end MslResistorExample;
"#;

    let result = Compiler::new()
        .model("MslResistorExample")
        .compile_str(source, "MslResistorExample.mo")
        .expect("compile MSL resistor wrapper");
    let report = render_checked_report(&result.dae, "MslResistorExample");

    assert!(
        report.contains("parameter R real unit=Ohm"),
        "checked report should preserve the resistor unit, got:\n{report}"
    );
}
