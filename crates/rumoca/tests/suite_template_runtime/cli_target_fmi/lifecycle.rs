//! Direct C-ABI lifecycle checks against the exact official FMI headers.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::tempdir;

pub(super) fn validate(version: &str, fmu_root: &Path, standard: &Path, xml: &str) {
    let identifier = attribute(xml, "modelIdentifier");
    let driver = match version {
        "fmi2" => fmi2_driver(identifier, attribute(xml, "guid")),
        "fmi3" => fmi3_driver(identifier, attribute(xml, "instantiationToken")),
        other => panic!("unexpected FMI version {other}"),
    };
    let work = tempdir().expect("create FMI lifecycle driver directory");
    let source = work.path().join("lifecycle.c");
    let executable = work.path().join("lifecycle");
    fs::write(&source, driver).expect("write FMI lifecycle driver");
    let output = Command::new("cc")
        .args(["-std=c99", "-Wall", "-Wextra", "-Wpedantic", "-Werror"])
        .arg(format!("-I{}", standard.join("headers").display()))
        .arg(fmu_root.join("sources/model.c"))
        .arg(&source)
        .args(["-lm", "-o"])
        .arg(&executable)
        .output()
        .expect("compile FMI lifecycle driver");
    assert_command_succeeded(&output, "compile FMI lifecycle driver");
    let output = Command::new(executable)
        .output()
        .expect("execute FMI lifecycle driver");
    assert_command_succeeded(&output, "execute FMI lifecycle driver");
}

fn attribute<'a>(xml: &'a str, name: &str) -> &'a str {
    let prefix = format!("{name}=\"");
    let start = xml
        .find(&prefix)
        .unwrap_or_else(|| panic!("model description has no {name} attribute"))
        + prefix.len();
    let tail = &xml[start..];
    &tail[..tail.find('"').expect("terminate XML attribute")]
}

fn assert_command_succeeded(output: &std::process::Output, label: &str) {
    assert!(
        output.status.success(),
        "{label}: status {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn fmi2_driver(identifier: &str, guid: &str) -> String {
    FMI2_DRIVER
        .replace("{identifier}", identifier)
        .replace("{guid}", guid)
        .replace("{{", "{")
        .replace("}}", "}")
}

const FMI2_DRIVER: &str = r#"
#include <math.h>
#include <stddef.h>
#include <stdlib.h>

#define FMI2_FUNCTION_PREFIX {identifier}_
#include "fmi2Functions.h"

#define CHECK(condition) do {{ if (!(condition)) return __LINE__; }} while (0)

int main(void) {{
    fmi2CallbackFunctions callbacks = {{ NULL, calloc, free, NULL, NULL }};
    CHECK(fmi2Instantiate("bad", fmi2ModelExchange, "wrong", NULL, &callbacks, 0, 0) == NULL);

    fmi2Component me = fmi2Instantiate("me", fmi2ModelExchange, "{guid}", NULL, &callbacks, 0, 0);
    CHECK(me != NULL);
    CHECK(fmi2DoStep(me, 0.0, 0.1, fmi2True) == fmi2Error);
    CHECK(fmi2Reset(me) == fmi2OK);
    CHECK(fmi2SetupExperiment(me, fmi2False, 0.0, 0.0, fmi2True, 1.0) == fmi2OK);
    CHECK(fmi2EnterInitializationMode(me) == fmi2OK);
    CHECK(fmi2ExitInitializationMode(me) == fmi2OK);
    CHECK(fmi2EnterContinuousTimeMode(me) == fmi2OK);
    CHECK(fmi2SetTime(me, 0.1) == fmi2OK);
    fmi2Real state[2] = {{ 0.95, 0.90 }};
    CHECK(fmi2SetContinuousStates(me, state, 2) == fmi2OK);
    fmi2Real derivative[2];
    CHECK(fmi2GetDerivatives(me, derivative, 2) == fmi2OK);
    CHECK(isfinite(derivative[0]) && isfinite(derivative[1]));
    fmi2Boolean enter_event = fmi2True;
    fmi2Boolean terminate = fmi2True;
    CHECK(fmi2CompletedIntegratorStep(me, fmi2True, &enter_event, &terminate) == fmi2OK);
    CHECK(!enter_event && !terminate);
    CHECK(fmi2Terminate(me) == fmi2OK);
    fmi2FreeInstance(me);

    fmi2Component cs = fmi2Instantiate("cs", fmi2CoSimulation, "{guid}", NULL, &callbacks, 0, 0);
    CHECK(cs != NULL);
    CHECK(fmi2SetTime(cs, 0.0) == fmi2Error);
    CHECK(fmi2Reset(cs) == fmi2OK);
    CHECK(fmi2SetupExperiment(cs, fmi2False, 0.0, 0.0, fmi2True, 1.0) == fmi2OK);
    CHECK(fmi2EnterInitializationMode(cs) == fmi2OK);
    CHECK(fmi2ExitInitializationMode(cs) == fmi2OK);
    fmi2ValueReference vr = 1;
    fmi2Real before;
    CHECK(fmi2GetReal(cs, &vr, 1, &before) == fmi2OK);
    fmi2Real forbidden = 42.0;
    CHECK(fmi2SetReal(cs, &vr, 1, &forbidden) == fmi2Error);
    fmi2Real after;
    CHECK(fmi2GetReal(cs, &vr, 1, &after) == fmi2OK);
    CHECK(before == after);
    CHECK(fmi2Reset(cs) == fmi2OK);
    CHECK(fmi2SetupExperiment(cs, fmi2False, 0.0, 0.0, fmi2True, 1.0) == fmi2OK);
    CHECK(fmi2EnterInitializationMode(cs) == fmi2OK);
    CHECK(fmi2ExitInitializationMode(cs) == fmi2OK);
    CHECK(fmi2DoStep(cs, 0.0, 0.0, fmi2True) == fmi2Error);
    CHECK(fmi2Reset(cs) == fmi2OK);
    CHECK(fmi2SetupExperiment(cs, fmi2False, 0.0, 0.0, fmi2True, 1.0) == fmi2OK);
    CHECK(fmi2EnterInitializationMode(cs) == fmi2OK);
    CHECK(fmi2ExitInitializationMode(cs) == fmi2OK);
    CHECK(fmi2DoStep(cs, 0.0, 0.1, fmi2True) == fmi2OK);
    fmi2Real last_time = 0.0;
    CHECK(fmi2GetRealStatus(cs, fmi2LastSuccessfulTime, &last_time) == fmi2OK);
    CHECK(fabs(last_time - 0.1) < 1e-14);
    CHECK(fmi2Terminate(cs) == fmi2OK);
    fmi2FreeInstance(cs);
    return 0;
}}
"#;

fn fmi3_driver(identifier: &str, token: &str) -> String {
    FMI3_DRIVER
        .replace("{identifier}", identifier)
        .replace("{token}", token)
        .replace("{{", "{")
        .replace("}}", "}")
}

const FMI3_DRIVER: &str = r#"
#include <math.h>
#include <stddef.h>

#define FMI3_FUNCTION_PREFIX {identifier}_
#include "fmi3Functions.h"

#define CHECK(condition) do {{ if (!(condition)) return __LINE__; }} while (0)

int main(void) {{
    CHECK(fmi3InstantiateModelExchange("bad", "wrong", NULL, fmi3False, fmi3False, NULL, NULL) == NULL);
    CHECK(fmi3InstantiateCoSimulation("bad", "{token}", NULL, fmi3False, fmi3False,
        fmi3True, fmi3False, NULL, 0, NULL, NULL, NULL) == NULL);

    fmi3Instance me = fmi3InstantiateModelExchange("me", "{token}", NULL,
        fmi3False, fmi3False, NULL, NULL);
    CHECK(me != NULL);
    fmi3Boolean event_needed, terminate, early_return;
    fmi3Float64 last_time;
    CHECK(fmi3DoStep(me, 0.0, 0.1, fmi3True, &event_needed, &terminate,
        &early_return, &last_time) == fmi3Error);
    CHECK(fmi3Reset(me) == fmi3OK);
    CHECK(fmi3EnterInitializationMode(me, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(me) == fmi3OK);
    CHECK(fmi3EnterContinuousTimeMode(me) == fmi3OK);
    CHECK(fmi3SetTime(me, 0.1) == fmi3OK);
    fmi3Float64 state[2] = {{ 0.95, 0.90 }};
    CHECK(fmi3SetContinuousStates(me, state, 2) == fmi3OK);
    fmi3Float64 derivative[2];
    CHECK(fmi3GetContinuousStateDerivatives(me, derivative, 2) == fmi3OK);
    CHECK(isfinite(derivative[0]) && isfinite(derivative[1]));
    CHECK(fmi3CompletedIntegratorStep(me, fmi3True, &event_needed, &terminate) == fmi3OK);
    CHECK(!event_needed && !terminate);
    CHECK(fmi3Terminate(me) == fmi3OK);
    fmi3FreeInstance(me);

    fmi3Instance cs = fmi3InstantiateCoSimulation("cs", "{token}", NULL,
        fmi3False, fmi3False, fmi3False, fmi3False, NULL, 0, NULL, NULL, NULL);
    CHECK(cs != NULL);
    CHECK(fmi3SetTime(cs, 0.0) == fmi3Error);
    CHECK(fmi3Reset(cs) == fmi3OK);
    CHECK(fmi3EnterInitializationMode(cs, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(cs) == fmi3OK);
    CHECK(fmi3EnterEventMode(cs) == fmi3Error);
    CHECK(fmi3Reset(cs) == fmi3OK);
    CHECK(fmi3EnterInitializationMode(cs, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(cs) == fmi3OK);
    fmi3ValueReference vr = 1;
    fmi3Float64 before[2];
    CHECK(fmi3GetFloat64(cs, &vr, 1, before, 2) == fmi3OK);
    fmi3Float64 forbidden[2] = {{ 42.0, 42.0 }};
    CHECK(fmi3SetFloat64(cs, &vr, 1, forbidden, 2) == fmi3Error);
    fmi3Float64 after[2];
    CHECK(fmi3GetFloat64(cs, &vr, 1, after, 2) == fmi3OK);
    CHECK(before[0] == after[0] && before[1] == after[1]);
    CHECK(fmi3Reset(cs) == fmi3OK);
    CHECK(fmi3EnterInitializationMode(cs, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(cs) == fmi3OK);
    CHECK(fmi3DoStep(cs, 0.0, 0.0, fmi3True, &event_needed, &terminate,
        &early_return, &last_time) == fmi3Error);
    CHECK(fmi3Reset(cs) == fmi3OK);
    CHECK(fmi3EnterInitializationMode(cs, fmi3False, 0.0, 0.0, fmi3True, 1.0) == fmi3OK);
    CHECK(fmi3ExitInitializationMode(cs) == fmi3OK);
    CHECK(fmi3DoStep(cs, 0.0, 0.1, fmi3True, &event_needed, &terminate,
        &early_return, &last_time) == fmi3OK);
    CHECK(!event_needed && !terminate && !early_return);
    CHECK(fabs(last_time - 0.1) < 1e-14);
    CHECK(fmi3Terminate(cs) == fmi3OK);
    fmi3FreeInstance(cs);
    return 0;
}}
"#;
