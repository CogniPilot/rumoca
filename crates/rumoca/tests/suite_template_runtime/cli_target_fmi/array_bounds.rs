//! Array variables with per-element bounds and nominals export schema-valid
//! metadata. FMI 3.0 declares `min`, `max`, and `nominal` once for every
//! element of an array variable, while FMI 2.0 scalarizes each element with
//! its own attributes; both packages must pass the official schemas, FMPy's
//! validator, the VDM check, and an FMPy build.

use super::*;

const MODEL: &str = "FmiArrayBounds";
const SOURCE: &str = r#"
model FmiArrayBounds
  parameter Integer color[3](min = {0, 1, 2}, max = {255, 254, 253}) = {10, 20, 30};
  output Real x[3](start = {1.0, 2.0, 3.0}, each fixed = true,
    min = {-10.0, -20.0, -30.0}, max = {10.0, 20.0, 30.0}, nominal = {1.0, 1.0, 1.0});
  output Real y[2](start = {1.0, 1.0}, each fixed = true, nominal = {1.0, 2.0});
equation
  der(x) = -0.1 * x;
  der(y) = -0.2 * y;
end FmiArrayBounds;
"#;

#[test]
fn packaged_fmi_array_bounds_are_schema_valid() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    assert_pinned_fmpy();
    let standards = standard_roots();
    let work = tempdir().expect("array bounds work directory");
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, &format!("{MODEL}.mo"))
        .unwrap_or_else(|error| panic!("compile {MODEL}: {error:?}"));
    for (target, standard) in [("fmi2", &standards.0), ("fmi3", &standards.1)] {
        let fmu = build_named_fmu(work.path(), &compiled, target, MODEL);
        let description = fs::read_to_string(fmu.root.join("modelDescription.xml"))
            .expect("read model description");
        for attribute in [" min=\"", " max=\"", " nominal=\""] {
            for value in description.split(attribute).skip(1) {
                let value = value.split('"').next().unwrap_or_default();
                assert!(
                    !value.contains(' '),
                    "{target}: {attribute} carries one value per element: {value}"
                );
            }
        }
        if target == "fmi3" {
            // The loosest element bound covers every element; a nominal the
            // elements do not share is not declared.
            assert!(
                description.contains(" min=\"0\" max=\"255\""),
                "{description}"
            );
            assert!(
                description.contains(" min=\"-30.0\" max=\"30.0\" nominal=\"1.0\""),
                "{description}"
            );
            assert!(!description.contains("nominal=\"2.0\""), "{description}");
        }
        validate_source_package(&fmu, standard);
    }
}
