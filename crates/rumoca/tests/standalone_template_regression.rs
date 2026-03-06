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
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples/templates/standalone_html.jinja")
}

fn setup_mock_libraries(root: &Path) -> (PathBuf, PathBuf, PathBuf) {
    let main_lib = root.join("MainLib");
    let helper_lib = root.join("HelperTypes");
    let service_lib = root.join("ServiceTypes");

    write_text(
        &main_lib.join("package.mo"),
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
        &helper_lib.join("package.mo"),
        r#"
package HelperTypes
  constant Real defaultR = 10;
end HelperTypes;
"#,
    );

    write_text(
        &service_lib.join("package.mo"),
        r#"
package ServiceTypes
  constant Real bias = 1;
end ServiceTypes;
"#,
    );

    (main_lib, helper_lib, service_lib)
}

#[test]
fn compile_fails_when_transitive_root_libraries_are_not_loaded() {
    let temp = tempdir().expect("tempdir");
    let (main_lib, _helper_lib, _service_lib) = setup_mock_libraries(temp.path());

    let wrapper_model = r#"
model Wrapper
  extends MainLib.Example;
end Wrapper;
"#;

    let result = Compiler::new()
        .model("Wrapper")
        .library(main_lib.to_string_lossy().as_ref())
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
fn standalone_template_renders_with_all_library_roots_loaded() {
    let temp = tempdir().expect("tempdir");
    let (main_lib, helper_lib, service_lib) = setup_mock_libraries(temp.path());

    let wrapper_model = r#"
model Wrapper
  import HelperTypes;
  import ServiceTypes;
  extends MainLib.Example;
end Wrapper;
"#;

    let result = Compiler::new()
        .model("Wrapper")
        .library(main_lib.to_string_lossy().as_ref())
        .library(helper_lib.to_string_lossy().as_ref())
        .library(service_lib.to_string_lossy().as_ref())
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
