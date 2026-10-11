//! Actual generated-C pulse rearming and state/time-event coexistence.

use super::*;

fn xml_attribute(xml: &str, tag: &[u8], name: &[u8], variable: Option<&str>) -> String {
    let mut reader = quick_xml::Reader::from_str(xml);
    loop {
        match reader.read_event().expect("valid rendered XML") {
            quick_xml::events::Event::Start(element) | quick_xml::events::Event::Empty(element)
                if element.name().as_ref() == tag =>
            {
                if let Some(value) = matching_attribute(&element, name, variable) {
                    return value;
                }
            }
            quick_xml::events::Event::Eof => panic!("metadata attribute absent"),
            _ => {}
        }
    }
}

fn matching_attribute(
    element: &quick_xml::events::BytesStart<'_>,
    name: &[u8],
    variable: Option<&str>,
) -> Option<String> {
    let attributes = element
        .attributes()
        .map(|attribute| attribute.expect("valid attribute"))
        .collect::<Vec<_>>();
    if variable.is_some_and(|variable| {
        !attributes.iter().any(|attribute| {
            attribute.key.as_ref() == b"name" && attribute.value.as_ref() == variable.as_bytes()
        })
    }) {
        return None;
    }
    let attribute = attributes
        .iter()
        .find(|attribute| attribute.key.as_ref() == name)
        .expect("required metadata attribute");
    Some(String::from_utf8(attribute.value.to_vec()).expect("UTF-8 metadata"))
}

#[test]
fn generated_fmi_c_rearms_pulses_without_repeating_other_events() {
    let work = tempfile::tempdir().unwrap();
    let (sources, units) = render(work.path(), "StateAndTimeEvents", "fmi2");
    let xml =
        std::fs::read_to_string(sources.parent().unwrap().join("modelDescription.xml")).unwrap();
    let guid = xml_attribute(&xml, b"fmiModelDescription", b"guid", None);
    let identifier = xml_attribute(&xml, b"CoSimulation", b"modelIdentifier", None);
    let references = ["n", "m", "ticks"].map(|name| {
        xml_attribute(&xml, b"ScalarVariable", b"valueReference", Some(name))
            .parse::<u32>()
            .unwrap()
    });
    let driver = DRIVER
        .replace("@GUID@", &guid)
        .replace("@IDENTIFIER@", &identifier)
        .replace(
            "@REFERENCES@",
            &references.map(|v| v.to_string()).join(", "),
        );
    let source = work.path().join("pulse.c");
    let executable = work.path().join("pulse");
    std::fs::write(&source, driver).unwrap();
    let output = Command::new("cc")
        .args(["-std=c11", "-O2", "-Wall", "-Wextra", "-Werror"])
        .arg(format!("-I{}", sources.display()))
        .arg(format!("-I{}", headers("fmi2").display()))
        .args(units)
        .arg(source)
        .args(["-lm", "-o"])
        .arg(&executable)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let output = Command::new(executable).output().unwrap();
    assert!(
        output.status.success(),
        "generated pulse lifecycle: {:?}\n{}\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

const DRIVER: &str = r#"
#include <stdio.h>
#include <stdlib.h>
#define FMI2_FUNCTION_PREFIX @IDENTIFIER@_
#include "fmi2Functions.h"
#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)
int main(void) {
    fmi2CallbackFunctions callbacks = { NULL, calloc, free, NULL, NULL };
    fmi2Component c = fmi2Instantiate("pulse", fmi2CoSimulation, "@GUID@", NULL, &callbacks, 0, 0);
    CHECK(c != NULL);
    CHECK(fmi2SetupExperiment(c, 1, 1e-8, 0.0, 1, 0.6) == fmi2OK);
    CHECK(fmi2EnterInitializationMode(c) == fmi2OK);
    CHECK(fmi2ExitInitializationMode(c) == fmi2OK);
    fmi2ValueReference references[3] = { @REFERENCES@ };
    fmi2Integer values[3];
    CHECK(fmi2GetInteger(c, references, 3, values) == fmi2OK);
    CHECK(values[0] == 0 && values[1] == 0 && values[2] == 0);
    for (int step = 0; step < 6; ++step) {
        double t = (double)step / 10.0;
        CHECK(fmi2DoStep(c, t, 0.1, 1) == fmi2OK);
        CHECK(fmi2GetInteger(c, references, 3, values) == fmi2OK);
        printf("%.17g,%d,%d,%d\n", t + 0.1, values[0], values[1], values[2]);
        CHECK(values[2] == step + 1);
        CHECK(values[0] == (step >= 4 ? 1 : 0));
        CHECK(values[1] == (step >= 4 ? 1 : 0));
    }
    fmi2FreeInstance(c);
    return 0;
}
"#;
