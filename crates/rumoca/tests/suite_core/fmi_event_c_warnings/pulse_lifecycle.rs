//! Actual generated-C pulse rearming and state/time-event coexistence.

use super::*;

fn xml_attribute(xml: &str, tag: &[u8], name: &[u8], variable: Option<&str>) -> String {
    let mut reader = quick_xml::Reader::from_str(xml);
    loop {
        let value = match reader.read_event().expect("valid rendered XML") {
            quick_xml::events::Event::Start(element) | quick_xml::events::Event::Empty(element)
                if element.name().as_ref() == tag =>
            {
                matching_attribute(&element, name, variable)
            }
            quick_xml::events::Event::Eof => panic!("metadata attribute absent"),
            _ => None,
        };
        if let Some(value) = value {
            return value;
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
    for target in ["fmi2", "fmi3"] {
        check_pulse_lifecycle(target);
    }
}

fn check_pulse_lifecycle(target: &str) {
    let work = tempfile::tempdir().unwrap();
    let (sources, units) = render(work.path(), "StateAndTimeEvents", target);
    let xml =
        std::fs::read_to_string(sources.parent().unwrap().join("modelDescription.xml")).unwrap();
    let (identity_attribute, variable_tag, abi) = match target {
        "fmi2" => (&b"guid"[..], &b"ScalarVariable"[..], FMI2_ABI),
        _ => (&b"instantiationToken"[..], &b"Int64"[..], FMI3_ABI),
    };
    let guid = xml_attribute(&xml, b"fmiModelDescription", identity_attribute, None);
    let identifier = xml_attribute(&xml, b"CoSimulation", b"modelIdentifier", None);
    let references = ["n", "m", "ticks"].map(|name| {
        xml_attribute(&xml, variable_tag, b"valueReference", Some(name))
            .parse::<u32>()
            .unwrap()
    });
    let driver = DRIVER
        .replace("@ABI@", abi)
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
        .arg(format!("-I{}", headers(target).display()))
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
        "generated {target} pulse lifecycle: {:?}\n{}\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    println!(
        "{target} pulse lifecycle:\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
}

const DRIVER: &str = r#"
#include <stdio.h>
#include <stdlib.h>
@ABI@
#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)
int main(void) {
    PulseInstance c = pulse_instantiate("@GUID@");
    CHECK(c != NULL);
    CHECK(pulse_initialize(c));
    PulseReference references[3] = { @REFERENCES@ };
    PulseInteger values[3];
    CHECK(pulse_read(c, references, values));
    CHECK(values[0] == 0 && values[1] == 0 && values[2] == 0);
    for (int step = 0; step < 6; ++step) {
        double t = (double)step / 10.0;
        CHECK(pulse_step(c, t));
        CHECK(pulse_read(c, references, values));
        printf("%.17g,%lld,%lld,%lld\n", t + 0.1, (long long)values[0], (long long)values[1], (long long)values[2]);
        CHECK(values[2] == step + 1);
        CHECK(values[0] == (step >= 4 ? 1 : 0));
        CHECK(values[1] == (step >= 4 ? 1 : 0));
    }
    pulse_free(c);
    return 0;
}
"#;

const FMI2_ABI: &str = r#"
#define FMI2_FUNCTION_PREFIX @IDENTIFIER@_
#include "fmi2Functions.h"
typedef fmi2Component PulseInstance;
typedef fmi2ValueReference PulseReference;
typedef fmi2Integer PulseInteger;
static PulseInstance pulse_instantiate(const char* identity) {
    fmi2CallbackFunctions callbacks = { NULL, calloc, free, NULL, NULL };
    return fmi2Instantiate("pulse", fmi2CoSimulation, identity, NULL, &callbacks, 0, 0);
}
static int pulse_initialize(PulseInstance c) {
    return fmi2SetupExperiment(c, 1, 1e-8, 0.0, 1, 0.6) == fmi2OK &&
        fmi2EnterInitializationMode(c) == fmi2OK && fmi2ExitInitializationMode(c) == fmi2OK;
}
static int pulse_read(PulseInstance c, const PulseReference* references, PulseInteger* values) {
    return fmi2GetInteger(c, references, 3, values) == fmi2OK;
}
static int pulse_step(PulseInstance c, double time) {
    return fmi2DoStep(c, time, 0.1, 1) == fmi2OK;
}
#define pulse_free fmi2FreeInstance
"#;

const FMI3_ABI: &str = r#"
#define FMI3_FUNCTION_PREFIX @IDENTIFIER@_
#include "fmi3Functions.h"
typedef fmi3Instance PulseInstance;
typedef fmi3ValueReference PulseReference;
typedef fmi3Int64 PulseInteger;
static PulseInstance pulse_instantiate(const char* identity) {
    return fmi3InstantiateCoSimulation("pulse", identity, NULL, 0, 0, 0, 0, NULL, 0, NULL, NULL, NULL);
}
static int pulse_initialize(PulseInstance c) {
    return fmi3EnterInitializationMode(c, 1, 1e-8, 0.0, 1, 0.6) == fmi3OK &&
        fmi3ExitInitializationMode(c) == fmi3OK;
}
static int pulse_read(PulseInstance c, const PulseReference* references, PulseInteger* values) {
    return fmi3GetInt64(c, references, 3, values, 3) == fmi3OK;
}
static int pulse_step(PulseInstance c, double time) {
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time;
    return fmi3DoStep(c, time, 0.1, 1, &event_needed, &terminate, &early_return, &last_time) == fmi3OK &&
        !event_needed && !terminate && !early_return && last_time == time + 0.1;
}
#define pulse_free fmi3FreeInstance
"#;
