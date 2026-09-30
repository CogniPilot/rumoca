//! Declared causality of nested declarations (SPEC_0044 §8, MLS §4.4.2.2).
//!
//! FMI exports only top-level inputs and outputs with `input`/`output`
//! causality; a nested `input` or `output` is `local`. Each such declaration
//! carries its prefix as a namespaced per-variable annotation, whatever its
//! runtime role (algebraic, state, or discrete), while a top-level output
//! states its prefix through its causality alone and carries none.

use super::*;

const DECLARED_MODEL: &str = "NestedCausality";
const DECLARED_SOURCE: &str = r#"
model Vehicle
  input Real throttle "nested input";
  output Real p(start = 0, fixed = true) "state";
  output Real v(start = 0, fixed = true) "state";
  output Real accel "algebraic";
  output Real w[2] "algebraic array";
  output Boolean far "discrete";
equation
  accel = throttle - 0.1*v;
  der(p) = v;
  der(v) = accel;
  w = {2*p, 3*v};
  far = p > 2;
end Vehicle;

model NestedCausality
  Vehicle vehicle(throttle = 1);
  output Real speed "top-level output";
equation
  speed = 2*vehicle.v;
end NestedCausality;
"#;

const ANNOTATED: [(&str, &str); 7] = [
    ("vehicle.throttle", "input"),
    ("vehicle.p", "output"),
    ("vehicle.v", "output"),
    ("vehicle.accel", "output"),
    ("vehicle.w[1]", "output"),
    ("vehicle.w[2]", "output"),
    ("vehicle.far", "output"),
];

const UNANNOTATED: [&str; 3] = ["speed", "der(vehicle.p)", "der(vehicle.v)"];

/// The text of one published variable: from its name to the next variable.
fn published_variable<'xml>(xml: &'xml str, name: &str) -> &'xml str {
    let start = xml
        .find(&format!(" name=\"{name}\""))
        .unwrap_or_else(|| panic!("`{name}` is published:\n{xml}"));
    let end = xml[start..]
        .find("\n    <")
        .map_or(xml.len(), |offset| start + offset);
    &xml[start..end]
}

fn assert_fmi2_annotations(xml: &str) {
    for (name, declared) in ANNOTATED {
        let variable = published_variable(xml, name);
        assert!(variable.contains("causality=\"local\""), "{variable}");
        // The annotation follows the type element, the last thing the
        // variable holds.
        let annotation = format!(
            "/>\n      <Annotations>\n        <Tool name=\"rumoca\">\n          \
             <DeclaredCausality value=\"{declared}\"/>\n        </Tool>\n      \
             </Annotations>"
        );
        assert!(variable.contains(&annotation), "{variable}");
    }
    assert!(published_variable(xml, "speed").contains("causality=\"output\""));
    for name in UNANNOTATED {
        let variable = published_variable(xml, name);
        assert!(!variable.contains("Annotations"), "{variable}");
    }
}

fn assert_fmi3_annotations(xml: &str) {
    // FMI 3 publishes the array as one tensor variable named `vehicle.w`.
    let tensors = ANNOTATED
        .into_iter()
        .filter(|(name, _)| !name.ends_with("[2]"))
        .map(|(name, declared)| (name.strip_suffix("[1]").unwrap_or(name), declared));
    for (name, declared) in tensors {
        let variable = published_variable(xml, name);
        assert!(variable.contains("causality=\"local\""), "{variable}");
        let annotation = format!(
            ">\n      <Annotations>\n        \
             <Annotation type=\"rumoca.declaredCausality\">{declared}</Annotation>\n      \
             </Annotations>"
        );
        assert!(variable.contains(&annotation), "{variable}");
    }
    assert!(
        published_variable(xml, "vehicle.w")
            .contains("</Annotations>\n      <Dimension start=\"2\"/>"),
        "{xml}"
    );
    assert!(published_variable(xml, "speed").contains("causality=\"output\""));
    for name in UNANNOTATED {
        let variable = published_variable(xml, name);
        assert!(!variable.contains("Annotations"), "{variable}");
    }
}

#[test]
fn packaged_fmi2_and_fmi3_annotate_nested_declared_causality() {
    let work = tempdir().expect("declared-causality FMI work directory");
    let compiled = rumoca::Compiler::new()
        .model(DECLARED_MODEL)
        .compile_str(DECLARED_SOURCE, "NestedCausality.mo")
        .expect("compile the nested-causality model");
    let fmi2 = build_named_fmu(work.path(), &compiled, "fmi2", DECLARED_MODEL);
    let fmi3 = build_named_fmu(work.path(), &compiled, "fmi3", DECLARED_MODEL);
    let description = |fmu: &BuiltFmu| {
        fs::read_to_string(fmu.root.join("modelDescription.xml"))
            .expect("read the model description")
    };
    assert_fmi2_annotations(&description(&fmi2));
    assert_fmi3_annotations(&description(&fmi3));

    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    validate_source_package(&fmi2, &standards.0);
    validate_source_package(&fmi3, &standards.1);
}
