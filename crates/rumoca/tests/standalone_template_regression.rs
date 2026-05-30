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

fn standalone_template_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples/codegen/standalone_web/standalone_html.jinja")
}

fn standalone_javascript_template_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../examples/codegen/standalone_web/javascript.jinja")
}

fn examples_template_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples/codegen")
}

fn standalone_web_target_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples/codegen/standalone_web")
}

fn assert_current_dae_shape_renders_to_runnable_js(rendered: &str, context: &str) {
    for expected in [
        "const __rumocaP = 0.5;",
        "const __rumocaX0 = 1.0;",
        r#"const v_k = p["k"];"#,
        "const v_x = xVec[0];",
        "const der_v_x = xDotVec[0];",
        "* ((typeof v_x",
    ] {
        assert!(
            rendered.contains(expected),
            "{context} should contain {expected:?}, got:\n{rendered}"
        );
    }
    for invalid in ["v_{", "der_v_{"] {
        assert!(
            !rendered.contains(invalid),
            "{context} should not contain broken generated fragment {invalid:?}, got:\n{rendered}"
        );
    }
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
fn all_example_templates_render_in_ci() {
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
    assert_eq!(
        template_names,
        vec!["custom_casadi.jinja"],
        "all direct raw examples/codegen/*.jinja files should be covered by this CI render sweep"
    );
}

#[test]
fn standalone_web_templates_render_current_dae_refs_literals_and_ops() {
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
        .expect("compile standalone web regression model");

    let javascript = result
        .render_template(
            standalone_javascript_template_path()
                .to_str()
                .expect("standalone JavaScript template path should be utf8"),
        )
        .expect("render standalone JavaScript template");
    assert_current_dae_shape_renders_to_runnable_js(&javascript, "standalone JavaScript template");

    let html = result
        .render_template(
            standalone_template_path()
                .to_str()
                .expect("standalone HTML template path should be utf8"),
        )
        .expect("render standalone HTML template");
    assert_current_dae_shape_renders_to_runnable_js(&html, "standalone HTML template");

    let target = rumoca_compile::codegen::targets::TargetBundle::load(
        standalone_web_target_root()
            .to_str()
            .expect("standalone web target path should be utf8"),
    )
    .expect("load standalone web target");
    let manifest = target
        .parse_manifest()
        .expect("parse standalone web target");
    let rendered_files = rumoca_compile::codegen::targets::render_dae_target_files(
        &target,
        &manifest,
        &result.dae,
        "SympyDecay",
    )
    .expect("render standalone web target files");
    assert_eq!(
        rendered_files.len(),
        2,
        "standalone web target should render HTML and JavaScript outputs"
    );
    for rendered in rendered_files {
        assert!(
            rendered.content.contains(r#"name: "SympyDecay""#),
            "target output {} should contain the requested model name, got:\n{}",
            rendered.path,
            rendered.content
        );
        assert_current_dae_shape_renders_to_runnable_js(
            &rendered.content,
            &format!("standalone web target {}", rendered.path),
        );
    }
}

#[test]
fn compile_fails_when_transitive_root_source_roots_are_not_loaded() {
    let temp = tempdir().expect("tempdir");
    let (main_root, _helper_root, _service_root) = setup_mock_source_roots(temp.path());

    let wrapper_model = r#"
model Wrapper
  extends MainLib.Example;
end Wrapper;
"#;

    let result = Compiler::new()
        .model("Wrapper")
        .source_root(main_root.to_string_lossy().as_ref())
        .compile_str(wrapper_model, "Wrapper.mo");

    assert!(
        result.is_err(),
        "expected compile failure with missing roots"
    );
    let err_text = format!("{:?}", result.expect_err("error expected"));
    assert!(
        err_text.contains("HelperTypes") || err_text.contains("ServiceTypes"),
        "error should mention unresolved transitive roots, got: {err_text}"
    );
}

#[test]
fn standalone_template_renders_with_all_source_roots_loaded() {
    let temp = tempdir().expect("tempdir");
    let (main_root, helper_root, service_root) = setup_mock_source_roots(temp.path());

    let wrapper_model = r#"
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
        .compile_str(wrapper_model, "Wrapper.mo")
        .expect("compile wrapper model");

    let template_path = standalone_template_path();
    assert!(
        template_path.is_file(),
        "template missing: {}",
        template_path.display()
    );

    let rendered = result
        .render_template(template_path.to_string_lossy().as_ref())
        .expect("render standalone template");

    assert!(
        rendered.contains("function Model()"),
        "rendered template should inline generated model code"
    );
    assert!(
        rendered.contains("const simulateModel = (params, context, model) =>"),
        "rendered template should inline solver code"
    );

    assert!(
        !rendered.contains("{{ compiled_js }}") && !rendered.contains("{{ solver_js }}"),
        "rendered output must not contain placeholder tokens"
    );
    assert!(
        !rendered.contains("/*__TASKYON_GENERATED_MODEL_JS__*/")
            && !rendered.contains("/*__TASKYON_SOLVER_JS__*/"),
        "rendered output must not contain Taskyon script placeholders"
    );
}

#[test]
fn standalone_template_preserves_msl_resistor_units_in_meta() {
    // Minimal MSL-shaped model hierarchy that captures the unit propagation path:
    // SI.Resistance extends Real with unit metadata, and a resistor model uses that type.
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

      package Examples
        model Resistor
          extends Modelica.Electrical.Analog.Basic.Resistor;
        end Resistor;
      end Examples;
    end Analog;
  end Electrical;
end Modelica;

model MslResistorExample
  extends Modelica.Electrical.Analog.Examples.Resistor;
equation
  i = 1;
end MslResistorExample;
"#;

    let result = Compiler::new()
        .model("MslResistorExample")
        .compile_str(source, "MslResistorExample.mo")
        .expect("compile MSL resistor wrapper");

    let template_path = standalone_template_path();
    let rendered = result
        .render_template(template_path.to_string_lossy().as_ref())
        .expect("render standalone template");

    assert!(
        rendered.contains(r#"name: "R""#),
        "expected resistor parameter in rendered template meta, got:\n{rendered}"
    );
    assert!(
        rendered.contains(r#"unit: "Ohm""#),
        "expected propagated MSL Resistance unit in rendered template meta, got:\n{rendered}"
    );
}
